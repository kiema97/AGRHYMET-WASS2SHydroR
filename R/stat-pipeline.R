#' Run the statistical pipeline for a single basin (PCR/Ridge/Lasso + meta-fusion)
#'
#' Consolidate predictions across products for PCR, Ridge, and Lasso,
#' then fuse the consolidated columns into a final prediction (last-year test).
#' Falls back to a simple average if the training window is too short
#' or meta-learning is not feasible.
#'
#' @param basin_id Basin identifier value.
#' @param data_by_product Named list of data.frames (one per product).
#' @param hybas_id Name of the basin ID column.
#' @param pred_pattern_by_product Optional named vector/list of regex patterns
#'   to select predictors per product (defaults to "^pt_").
#' @param target Name of the target column (default: `"Q"`).
#' @param date_col Name of the date column (default: `"YYYY"`).
#' @param prediction_years Optional numeric vector of length 2 giving the
#'   start and end years for a holdout prediction period.
#' @param topK Integer, number of best products per model (default: 3).
#' @param final_fuser Name of the meta-learner to use (subset of \code{SUPPORTED_FUSERS}).
#' @param grid_levels Grid density for tuning both base models and meta-learner.
#' @param quiet Logical; if \code{FALSE}, emits informative messages.
#' @param verbose_tune A logical for logging results (other than warnings and errors, which are always shown) as they are generated during training in a single R process.
#' @param target_positive Logical; if TRUE, force negative predictions to zero.
#' @param max_na_frac Numeric in \eqn{[0, 1]}: maximum allowed fraction of missing
#'   values per column before stopping (default \code{0.20} = 20\%).
#' @param impute Character, one of \code{"median"}, \code{"mean"}, or \code{"none"}.
#'   If \code{"none"}, no imputation is performed after the guard (default \code{"median"}).
#' @param require_variance Logical; if \code{TRUE}, stop when a column has zero
#'   standard deviation after imputation (default \code{TRUE}).
#' @param ... Additional parameters passed to \code{wass2s_cons_mods_stat}.
#'
#' @return A list with:
#' \itemize{
#'   \item \code{fused_by_model}: tibble with \code{YYYY}, \code{Q}, and columns \code{PCR}, \code{RIDGE}, \code{LASSO}
#'   \item \code{final_test}: tibble for the test year with \code{pred_final}
#'   \item \code{scores}: tibble with \code{HYBAS_ID}, \code{kge_final}, \code{kge_pcr}, \code{kge_ridge}, \code{kge_lasso}, \code{rmse_final}
#'   \item \code{leaderboards}: list of product leaderboards per model
#'   \item \code{cv_rs}: resampling results from meta-learner tuning (if applicable)
#' }
#' @export
wass2s_run_basin_mods_stat <- function(
    basin_id,
    data_by_product,
    hybas_id = "HYBAS_ID",
    target = "Q",
    date_col = "YYYY",
    pred_pattern_by_product = NULL,
    prediction_years = NULL,
    topK = 3,
    final_fuser = "rf",
    grid_levels = 5,
    quiet = TRUE,
    verbose_tune = TRUE,
    target_positive = TRUE,
    max_na_frac =0.3,
    impute = "median",
    require_variance = TRUE,
    ...
) {
  if (length(data_by_product) == 0) {
    stop("data_by_product cannot be empty", call. = FALSE)
  }
  if (!is.null(prediction_years) && length(prediction_years) != 2) {
    stop("prediction_years must be length 2 (start, end).", call. = FALSE)
  }
  if (!is.null(prediction_years) && prediction_years[1] > prediction_years[2]) {
    stop("Starting year must be earlier than ending year.", call. = FALSE)
  }

  final_fuser <- match.arg(final_fuser, SUPPORTED_FUSERS)
  .require_pkg(engine_pkg[final_fuser])

  # --- helper: convert year-range to YYYYMMDD range (robust) ---
  .years_to_yyyymmdd_range <- function(yrs, df_yyyy) {
    if (is.null(yrs)) return(NULL)

    # allow length-1 year
    if (length(yrs) == 1 && yrs > 0) yrs <- rep(yrs, 2)

    # if already looks like yyyymmdd, keep
    is_yyyymmdd <- function(x) is.numeric(x) && x >= 19000101 && x <= 25001231

    if (is_yyyymmdd(yrs[1]) && is_yyyymmdd(yrs[2])) {
      return(as.integer(yrs))
    }

    # treat as years -> expand to Jan 1 / Dec 31
    lo <- as.integer(yrs[1] * 10000 + 101)
    hi <- as.integer(yrs[2] * 10000 + 1231)

    # clamp to data range if provided
    if (!missing(df_yyyy) && length(df_yyyy) > 0) {
      lo <- max(lo, min(df_yyyy, na.rm = TRUE))
      hi <- min(hi, max(df_yyyy, na.rm = TRUE))
    }
    c(lo, hi)
  }

  # 1) Consolidate by model (now expects YYYYMMDD inside)
  models <- list(
    PCR = wass2s_cons_mods_stat(
      basin_id, data_by_product, hybas_id, pred_pattern_by_product, "pcr", topK,
      quiet = quiet, target_positive = target_positive,
      require_variance=require_variance, impute=impute, max_na_frac=max_na_frac, ...
    ),
    RIDGE = wass2s_cons_mods_stat(
      basin_id, data_by_product, hybas_id, pred_pattern_by_product, "ridge", topK,
      quiet = quiet, target_positive = target_positive,
      require_variance=require_variance, impute=impute, max_na_frac=max_na_frac, ...
    ),
    LASSO = wass2s_cons_mods_stat(
      basin_id, data_by_product, hybas_id, pred_pattern_by_product, "lasso", topK,
      quiet = quiet, target_positive = target_positive,
      require_variance=require_variance, impute=impute, max_na_frac=max_na_frac, ...
    )
  )

  # 2) Get (YYYY, Q) from any product and standardize YYYYMMDD
  any_df <- tryCatch({
    get_any_Q(data_by_product, basin_id, hybas_id)
  }, error = function(e) {
    if (!quiet) message("Error getting base data: ", e$message)
    tibble::tibble(YYYY = integer(), Q = numeric())
  })

  any_df <- .sanitize_numeric_columns(
    df   = any_df,
    cols = target,
    max_na_frac = max_na_frac,
    impute = impute,
    require_variance = require_variance
  )

  # Ensure date format is YYYYMMDD (critical for joins)
  if (nrow(any_df) > 0 && "YYYY" %in% names(any_df)) {
    any_df$YYYY <- .ensure_yyyymmdd(any_df$YYYY)
  }

  if (nrow(any_df) == 0L) {
    years_all <- sort(unique(unlist(lapply(models, function(m) {
      if (!is.null(m$fused)) m$fused$YYYY else NULL
    }))))
    years_all <- .ensure_yyyymmdd(years_all)
    any_df <- tibble::tibble(YYYY = years_all, Q = NA_real_)
  }

  # 3) Build table of consolidated models
  fused_list <- list()
  for (model_name in names(models)) {
    if (!is.null(models[[model_name]]$fused)) {
      tmp <- models[[model_name]]$fused %>%
        dplyr::select(YYYY, pred = pred_fused) %>%
        dplyr::rename(!!model_name := pred)

      # defensive: ensure YYYYMMDD
      tmp$YYYY <- .ensure_yyyymmdd(tmp$YYYY)
      fused_list[[model_name]] <- tmp
    }
  }
  fused_list <- purrr::compact(fused_list)

  if (length(fused_list) == 0) {
    if (!quiet) message("No valid models for basin ", basin_id)
    fused_models <- any_df
    fused_models$pred_final <- NA_real_
    return(list(
      fused_by_model = fused_models,
      final_test = dplyr::slice_tail(fused_models, n = 1),
      scores = tibble::tibble(
        HYBAS_ID = basin_id,
        kge_final = NA_real_, kge_pcr = NA_real_, kge_ridge = NA_real_, kge_lasso = NA_real_,
        rmse_final = NA_real_
      ),
      leaderboards = list(
        PCR = models$PCR$leaderboard_products,
        RIDGE = models$RIDGE$leaderboard_products,
        LASSO = models$LASSO$leaderboard_products
      ),
      cv_rs = NULL
    ))
  }

  safe_full_join_preds_stat <- function(lst) {
    lst <- purrr::compact(lst)
    if (length(lst) == 0L) return(tibble::tibble())
    Reduce(function(a, b) dplyr::full_join(a, b, by = "YYYY"), lst)
  }

  fused_models_pred <- safe_full_join_preds_stat(fused_list)

  fused_models <- dplyr::left_join(any_df, fused_models_pred, by = "YYYY") %>%
    dplyr::arrange(YYYY)

  # Convert prediction_years to YYYYMMDD bounds
  pred_bounds <- .years_to_yyyymmdd_range(prediction_years, fused_models$YYYY)

  # 4) Train/test split
  if (!is.null(pred_bounds)) {
    df_te <- dplyr::filter(fused_models, YYYY >= pred_bounds[1], YYYY <= pred_bounds[2])
    df_tr <- dplyr::filter(fused_models, !(YYYY >= pred_bounds[1] & YYYY <= pred_bounds[2]))
  } else {
    if (nrow(fused_models) < 2) stop("Need at least 2 rows of data for training and testing", call. = FALSE)
    train_idx <- 1:(nrow(fused_models) - 1L)
    df_tr <- fused_models[train_idx, , drop = FALSE]
    df_te <- fused_models[-train_idx, , drop = FALSE]
  }

  # Predictor columns
  pred_cols <- setdiff(names(df_tr), c(target, date_col))
  model_cols <- intersect(c("PCR", "RIDGE", "LASSO"), names(fused_models))

  if (length(model_cols) == 0L) {
    df_te <- dplyr::mutate(df_te, pred_final = NA_real_)
    scores <- tibble::tibble(
      HYBAS_ID   = basin_id,
      kge_final  = NA_real_,
      kge_pcr    = if ("PCR" %in% names(df_te)) kge_vec(df_te$Q, df_te$PCR) else NA_real_,
      kge_ridge  = if ("RIDGE" %in% names(df_te)) kge_vec(df_te$Q, df_te$RIDGE) else NA_real_,
      kge_lasso  = if ("LASSO" %in% names(df_te)) kge_vec(df_te$Q, df_te$LASSO) else NA_real_,
      rmse_final = yardstick::rmse_vec(df_te$Q, df_te$pred_final)
    )
    return(list(
      fused_by_model = fused_models,
      final_test     = df_te,
      scores         = scores,
      leaderboards   = list(
        PCR   = models$PCR$leaderboard_products,
        RIDGE = models$RIDGE$leaderboard_products,
        LASSO = models$LASSO$leaderboard_products
      ),
      cv_rs = NULL
    ))
  }

  # 5) Fallback too short / constant
  too_short <- nrow(df_tr) < 5L
  constant_cols <- vapply(df_tr[, model_cols, drop = FALSE], function(z) {
    sd_val <- stats::sd(z, na.rm = TRUE)
    is.na(sd_val) || sd_val < 1e-12
  }, logical(1))
  all_constant <- all(constant_cols)

  if (too_short || all_constant) {
    df_te <- df_te %>%
      dplyr::mutate(pred_final = rowMeans(dplyr::select(., dplyr::all_of(model_cols)), na.rm = TRUE)) %>%
      dplyr::mutate(pred_final = ifelse(is.nan(pred_final), NA_real_, pred_final))

    scores <- tibble::tibble(
      HYBAS_ID   = basin_id,
      kge_final  = kge_vec(df_te$Q, df_te$pred_final),
      kge_pcr    = if ("PCR" %in% names(df_te)) kge_vec(df_te$Q, df_te$PCR) else NA_real_,
      kge_ridge  = if ("RIDGE" %in% names(df_te)) kge_vec(df_te$Q, df_te$RIDGE) else NA_real_,
      kge_lasso  = if ("LASSO" %in% names(df_te)) kge_vec(df_te$Q, df_te$LASSO) else NA_real_,
      rmse_final = yardstick::rmse_vec(df_te$Q, df_te$pred_final)
    )
    return(list(
      fused_by_model = fused_models,
      final_test     = df_te,
      scores         = scores,
      leaderboards   = list(
        PCR   = models$PCR$leaderboard_products,
        RIDGE = models$RIDGE$leaderboard_products,
        LASSO = models$LASSO$leaderboard_products
      ),
      cv_rs = NULL
    ))
  }

  # 6) Meta-learner
  rec_meta <- recipes::recipe(stats::as.formula(paste(target, "~ .")), data = df_tr) |>
    recipes::update_role(!!date_col, new_role = "id") |>
    recipes::step_rm(!!date_col) |>
    recipes::step_zv(recipes::all_predictors()) |>
    recipes::step_impute_median(recipes::all_predictors())

  spec <- model_spec(final_fuser)
  grid <- model_grid(final_fuser, p = length(pred_cols), levels = grid_levels)

  wf_meta <- workflows::workflow() |>
    workflows::add_recipe(rec_meta) |>
    workflows::add_model(spec)

  rset <- tryCatch({
    make_rolling(
      df_tr,
      year_col = date_col,
      init_frac = 0.7,
      assess_frac = 0.2,
      n_splits = min(3, nrow(df_tr) - 1),
      quiet = TRUE
    )
  }, error = function(e) {
    if (!quiet) message("Error creating resamples: ", e$message)
    NULL
  })

  if (!is.null(rset)) {
    ctrl <- tune::control_grid(save_pred = TRUE, verbose = !verbose_tune, allow_par = TRUE)
    rs <- tryCatch({
      tune::tune_grid(
        wf_meta,
        resamples = rset,
        grid = grid,
        metrics = yardstick::metric_set(yardstick::rmse),
        control = ctrl
      )
    }, error = function(e) {
      if (!quiet) message("Error tuning meta-learner: ", e$message)
      NULL
    })
  } else {
    rs <- NULL
  }

  if (is.null(rs) || nrow(rs$.metrics[[1]]) == 0) {
    if (!quiet) message("Meta-learner tuning failed, falling back to simple mean.")

    fused_models$pred_final <- rowMeans(dplyr::select(fused_models, dplyr::all_of(model_cols)), na.rm = TRUE)
    fused_models$pred_final <- ifelse(is.nan(fused_models$pred_final), NA_real_, fused_models$pred_final)

    return(list(
      fused_by_model = fused_models,
      final_test     = dplyr::slice_tail(fused_models, n = 1),
      scores         = tibble::tibble(HYBAS_ID = basin_id, kge_final = NA_real_, rmse_final = NA_real_),
      leaderboards   = list(
        PCR   = models$PCR$leaderboard_products,
        RIDGE = models$RIDGE$leaderboard_products,
        LASSO = models$LASSO$leaderboard_products
      ),
      cv_rs = NULL
    ))
  }

  best <- tune::select_best(rs, metric = "rmse")
  wf_fin <- tune::finalize_workflow(wf_meta, best)
  fit_fin <- parsnip::fit(wf_fin, df_tr)

  train_prediction <- df_tr %>% dplyr::mutate(pred_final = predict(fit_fin, df_tr)$.pred)

  pred_all <- predict(fit_fin, fused_models)$.pred
  fused_models$pred_final <- if (target_positive) pmax(pred_all, 0) else pred_all

  scores <- tibble::tibble(
    HYBAS_ID   = basin_id,
    kge_final  = kge_vec(train_prediction$Q, train_prediction$pred_final),
    kge_pcr    = if ("PCR" %in% names(df_tr)) kge_vec(df_tr$Q, df_tr$PCR) else NA_real_,
    kge_ridge  = if ("RIDGE" %in% names(df_tr)) kge_vec(df_tr$Q, df_tr$RIDGE) else NA_real_,
    kge_lasso  = if ("LASSO" %in% names(df_tr)) kge_vec(df_tr$Q, df_tr$LASSO) else NA_real_,
    rmse_final = yardstick::rmse_vec(train_prediction$Q, train_prediction$pred_final)
  )

  list(
    fused_by_model = fused_models,
    final_test     = dplyr::slice_tail(fused_models, n = 1),
    scores         = scores,
    leaderboards   = list(
      PCR   = models$PCR$leaderboard_products,
      RIDGE = models$RIDGE$leaderboard_products,
      LASSO = models$LASSO$leaderboard_products
    ),
    cv_rs = tune::collect_metrics(rs)
  )
}

# wass2s_run_basin_mods_stat <- function(
#     basin_id,
#     data_by_product,
#     hybas_id = "HYBAS_ID",
#     target = "Q",
#     date_col = "YYYY",
#     pred_pattern_by_product = NULL,
#     prediction_years = NULL,
#     topK = 3,
#     final_fuser = "rf",
#     grid_levels = 5,
#     quiet = TRUE,
#     verbose_tune = TRUE,
#     target_positive = TRUE,
#     max_na_frac =0.3,
#     impute = "median",
#     require_variance = TRUE,
#     ...
# ) {
#   # Input validation
#
#   if (length(data_by_product) == 0) {
#     stop("data_by_product cannot be empty", call. = FALSE)
#   }
#
#   if (!is.null(prediction_years) && length(prediction_years) != 2) {
#     stop("prediction_years must be length 2 (start, end).", call. = FALSE)
#   }
#
#   if (!is.null(prediction_years) && prediction_years[1] > prediction_years[2]) {
#     stop("Starting year must be earlier than ending year.", call. = FALSE)
#   }
#
#   final_fuser <- match.arg(final_fuser, SUPPORTED_FUSERS)
#   .require_pkg(engine_pkg[final_fuser])
#
#
#   # 1) Consolidate by model
#   models <- list(
#     PCR = wass2s_cons_mods_stat(
#       basin_id, data_by_product, hybas_id, pred_pattern_by_product, "pcr", topK,
#       quiet = quiet,target_positive = target_positive,
#       require_variance=require_variance,impute=impute,max_na_frac=max_na_frac, ...
#     ),
#     RIDGE = wass2s_cons_mods_stat(
#       basin_id, data_by_product, hybas_id, pred_pattern_by_product, "ridge", topK,
#       quiet = quiet,target_positive = target_positive,
#       require_variance=require_variance,impute=impute,max_na_frac=max_na_frac,...
#     ),
#     LASSO = wass2s_cons_mods_stat(
#       basin_id, data_by_product, hybas_id, pred_pattern_by_product, "lasso", topK,
#       quiet = quiet,target_positive = target_positive,
#       require_variance=require_variance,impute=impute,max_na_frac=max_na_frac,...
#     )
#   )
#
#   # 2) Get (YYYY, Q) from any product (same truth for all)
#   any_df <- tryCatch({
#     get_any_Q(data_by_product, basin_id, hybas_id)
#   }, error = function(e) {
#     if (!quiet) message("Error getting base data: ", e$message)
#     tibble::tibble(YYYY = integer(), Q = numeric())
#   })
#
#   any_df <- .sanitize_numeric_columns(
#     df   = any_df,
#     cols = target,
#     max_na_frac = max_na_frac,
#     impute = impute,
#     require_variance = require_variance
#   )
#
#   if (nrow(any_df) == 0L) {
#     # If nothing found (very rare), synthesize a timeline from consolidated models
#     years_all <- sort(unique(unlist(lapply(models, function(m) {
#       if (!is.null(m$fused)) m$fused$YYYY else NULL
#     }))))
#     any_df <- tibble::tibble(YYYY = years_all, Q = NA_real_)
#   }
#
#   # 3) Build the table of consolidated models (PCR/RIDGE/LASSO)
#   # Take only YYYY + pred_fused, rename to 'pred' then to model name
#   fused_list <- list()
#   for (model_name in names(models)) {
#     if (!is.null(models[[model_name]]$fused)) {
#       fused_list[[model_name]] <- models[[model_name]]$fused %>%
#         dplyr::select(YYYY, pred = pred_fused) %>%
#         dplyr::rename(!!model_name := pred)
#     }
#   }
#
#   fused_list <- purrr::compact(fused_list)
#
#   if (length(fused_list) == 0) {
#     # No models produced valid results
#     if (!quiet) message("No valid models for basin ", basin_id)
#     fused_models <- any_df
#     fused_models$pred_final <- NA_real_
#
#     return(list(
#       fused_by_model = fused_models,
#       final_test = dplyr::slice_tail(fused_models, n = 1),
#       scores = tibble::tibble(
#         HYBAS_ID = basin_id,
#         kge_final = NA_real_,
#         kge_pcr = NA_real_,
#         kge_ridge = NA_real_,
#         kge_lasso = NA_real_,
#         rmse_final = NA_real_
#       ),
#       leaderboards = list(
#         PCR = models$PCR$leaderboard_products,
#         RIDGE = models$RIDGE$leaderboard_products,
#         LASSO = models$LASSO$leaderboard_products
#       ),
#       cv_rs = NULL
#     ))
#   }
#
#   safe_full_join_preds_stat<- function(lst) {
#     lst <- purrr::compact(lst)
#     if (length(lst) == 0L) return(tibble::tibble())
#     Reduce(function(a, b) dplyr::full_join(a, b, by = "YYYY"), lst)
#   }
#   fused_models_pred <- safe_full_join_preds_stat(fused_list)
#   # Join with Q
#   fused_models <- dplyr::left_join(any_df, fused_models_pred, by = "YYYY")
#   fused_models <- dplyr::arrange(fused_models, YYYY)
#
#   # 4) Handle prediction years or use last year as test
#   if (!is.null(prediction_years)) {
#     df_te <- dplyr::filter(
#       fused_models,
#       YYYY >= prediction_years[1], YYYY <= prediction_years[2]
#     )
#     df_tr <- dplyr::filter(
#       fused_models,
#       !(YYYY >= prediction_years[1] & YYYY <= prediction_years[2])
#     )
#   } else {
#     # Use last year as test by default
#     if (nrow(fused_models) < 2) {
#       stop("Need at least 2 years of data for training and testing", call. = FALSE)
#     }
#     train_idx <- 1:(nrow(fused_models) - 1L)
#     df_tr <- fused_models[train_idx, , drop = FALSE]
#     df_te <- fused_models[-train_idx, , drop = FALSE]
#   }
#
#   # Safety checks
#   if (!target %in% names(fused_models)) {
#     stop("Target column '", target, "' missing from training data for meta-learner.", call. = FALSE)
#   }
#   if (!date_col %in% names(fused_models)) {
#     stop("Date column '", date_col, "' missing from training data for meta-learner.", call. = FALSE)
#   }
#
#   # Get predictor columns
#   pred_cols <- setdiff(names(df_tr), c(target, date_col))
#   if (length(pred_cols) < 1L) {
#     stop("No meta-features available for meta-learning (only target and date columns present).", call. = FALSE)
#   }
#
#   # Available consolidated model columns
#   model_cols <- intersect(c("PCR", "RIDGE", "LASSO"), names(fused_models))
#
#   # If no model columns, create final NA and return
#   if (length(model_cols) == 0L) {
#     df_te <- dplyr::mutate(df_te, pred_final = NA_real_)
#     scores <- tibble::tibble(
#       HYBAS_ID   = basin_id,
#       kge_final  = NA_real_,
#       kge_pcr    = if ("PCR" %in% names(df_te)) kge_vec(df_te$Q, df_te$PCR) else NA_real_,
#       kge_ridge  = if ("RIDGE" %in% names(df_te)) kge_vec(df_te$Q, df_te$RIDGE) else NA_real_,
#       kge_lasso  = if ("LASSO" %in% names(df_te)) kge_vec(df_te$Q, df_te$LASSO) else NA_real_,
#       rmse_final = yardstick::rmse_vec(df_te$Q, df_te$pred_final)
#     )
#     return(list(
#       fused_by_model = fused_models,
#       final_test     = df_te,
#       scores         = scores,
#       leaderboards   = list(
#         PCR   = models$PCR$leaderboard_products,
#         RIDGE = models$RIDGE$leaderboard_products,
#         LASSO = models$LASSO$leaderboard_products
#       ),
#       cv_rs = NULL
#     ))
#   }
#
#   # 5) Fallback if training data too short OR constant columns → simple mean
#   too_short <- nrow(df_tr) < 5L
#   constant_cols <- vapply(df_tr[, model_cols, drop = FALSE],
#                           function(z) {
#                             sd_val <- stats::sd(z, na.rm = TRUE)
#                             is.na(sd_val) || sd_val < 1e-12
#                           }, logical(1))
#   all_constant <- all(constant_cols)
#
#   if (too_short || all_constant) {
#     df_te <- df_te %>%
#       dplyr::mutate(pred_final = rowMeans(
#         dplyr::select(., dplyr::all_of(model_cols)),
#         na.rm = TRUE
#       )) %>%
#       dplyr::mutate(pred_final = ifelse(is.nan(pred_final), NA_real_, pred_final))
#
#     scores <- tibble::tibble(
#       HYBAS_ID   = basin_id,
#       kge_final  = kge_vec(df_te$Q, df_te$pred_final),
#       kge_pcr    = if ("PCR" %in% names(df_te)) kge_vec(df_te$Q, df_te$PCR) else NA_real_,
#       kge_ridge  = if ("RIDGE" %in% names(df_te)) kge_vec(df_te$Q, df_te$RIDGE) else NA_real_,
#       kge_lasso  = if ("LASSO" %in% names(df_te)) kge_vec(df_te$Q, df_te$LASSO) else NA_real_,
#       rmse_final = yardstick::rmse_vec(df_te$Q, df_te$pred_final)
#     )
#     return(list(
#       fused_by_model = fused_models,
#       final_test     = df_te,
#       scores         = scores,
#       leaderboards   = list(
#         PCR   = models$PCR$leaderboard_products,
#         RIDGE = models$RIDGE$leaderboard_products,
#         LASSO = models$LASSO$leaderboard_products
#       ),
#       cv_rs = NULL
#     ))
#   }
#
#   # 6) Meta-learning with the specified final_fuser
#   # Create recipe for meta-learner
#   rec_meta <- recipes::recipe(stats::as.formula(paste(target, "~ .")), data = df_tr) |>
#     recipes::update_role(!!date_col, new_role = "id") |>
#     recipes::step_rm(!!date_col) |>
#     recipes::step_zv(recipes::all_predictors()) |>
#     recipes::step_impute_median(recipes::all_predictors())
#
#   spec <- model_spec(final_fuser)
#    grid <- model_grid(final_fuser, p = length(pred_cols), levels = grid_levels)
#
#   wf_meta <- workflows::workflow() |>
#     workflows::add_recipe(rec_meta) |>
#     workflows::add_model(spec)
#
#   # Create resamples for tuning
#   rset <- tryCatch({
#     make_rolling(
#       df_tr,
#       year_col = date_col,
#       init_frac = 0.7,
#       assess_frac = 0.2,
#       n_splits = min(3, nrow(df_tr) - 1),
#       quiet = TRUE
#     )
#   }, error = function(e) {
#     if (!quiet) message("Error creating resamples: ", e$message)
#     NULL
#   })
#
#   # Tune meta-learner
#   if (!is.null(rset)) {
#     ctrl <- tune::control_grid(save_pred = TRUE, verbose = !verbose_tune, allow_par = TRUE)
#
#     rs <- tryCatch({
#       tune::tune_grid(
#         wf_meta,
#         resamples = rset,
#         grid = grid,
#         metrics = yardstick::metric_set(yardstick::rmse),
#         control = ctrl
#       )
#     }, error = function(e) {
#       if (!quiet) message("Error tuning meta-learner: ", e$message)
#       NULL
#     })
#   } else {
#     rs <- NULL
#   }
#
#   # If no usable results, fallback to simple mean
#   if (is.null(rs) || nrow(rs$.metrics[[1]]) == 0) {
#     if (!quiet) message("Meta-learner tuning failed, falling back to simple mean.")
#
#     df_te <- df_te %>%
#       dplyr::mutate(pred_final = rowMeans(
#         dplyr::select(., dplyr::all_of(model_cols)),
#         na.rm = TRUE
#       )) %>%
#       dplyr::mutate(pred_final = ifelse(is.nan(pred_final), NA_real_, pred_final))
#
#     scores <- tibble::tibble(
#       HYBAS_ID   = basin_id,
#       kge_final  = kge_vec(df_te$Q, df_te$pred_final),
#       kge_pcr    = if ("PCR" %in% names(df_te)) kge_vec(df_te$Q, df_te$PCR) else NA_real_,
#       kge_ridge  = if ("RIDGE" %in% names(df_te)) kge_vec(df_te$Q, df_te$RIDGE) else NA_real_,
#       kge_lasso  = if ("LASSO" %in% names(df_te)) kge_vec(df_te$Q, df_te$LASSO) else NA_real_,
#       rmse_final = yardstick::rmse_vec(df_te$Q, df_te$pred_final)
#     )
#     return(list(
#       fused_by_model = fused_models,
#       final_test     = df_te,
#       scores         = scores,
#       leaderboards   = list(
#         PCR   = models$PCR$leaderboard_products,
#         RIDGE = models$RIDGE$leaderboard_products,
#         LASSO = models$LASSO$leaderboard_products
#       ),
#       cv_rs = NULL
#     ))
#   }
#
#   # Final training with best parameters
#   best <- tune::select_best(rs, metric = "rmse")
#   wf_fin <- tune::finalize_workflow(wf_meta, best)
#   fit_fin <- parsnip::fit(wf_fin, df_tr)
#
#   # Generate predictions
#   train_prediction <- df_tr %>%
#     dplyr::mutate(pred_final = predict(fit_fin, df_tr)$.pred)
#
#
#
#   # Apply target_positive if requested
#   if (target_positive) {
#     fused_models$pred_final <- pmax(predict(fit_fin, fused_models)$.pred,0)
#   }else{
#     fused_models$pred_final <- predict(fit_fin, fused_models)$.pred
#   }
#   # 7) Calculate scores
#   scores <- tibble::tibble(
#     HYBAS_ID   = basin_id,
#     kge_final  = kge_vec(train_prediction$Q, train_prediction$pred_final),
#     kge_pcr    = if ("PCR" %in% names(df_tr)) kge_vec(df_tr$Q, df_tr$PCR) else NA_real_,
#     kge_ridge  = if ("RIDGE" %in% names(df_tr)) kge_vec(df_tr$Q, df_tr$RIDGE) else NA_real_,
#     kge_lasso  = if ("LASSO" %in% names(df_tr)) kge_vec(df_tr$Q, df_tr$LASSO) else NA_real_,
#     rmse_final = yardstick::rmse_vec(train_prediction$Q, train_prediction$pred_final)
#   )
#
#   list(
#     fused_by_model = fused_models,
#     final_test     = dplyr::slice_tail(fused_models, n = 1),
#     scores         = scores,
#     leaderboards   = list(
#       PCR   = models$PCR$leaderboard_products,
#       RIDGE = models$RIDGE$leaderboard_products,
#       LASSO = models$LASSO$leaderboard_products
#     ),
#     cv_rs = tune::collect_metrics(rs)
#   )
# }



#' Run the statistical method across multiple basins
#'
#' Iterates over basin ids, fits PCR/Ridge/Lasso for each product,
#' fuses top-K products by KGE, then trains a meta-learner on model outputs.
#' Supports parallel execution per basin.
#'
#' @param data_by_product Named list of data frames (one per product).
#' @param hybas_id Column name for basin IDs (default: `"HYBAS_ID"`).
#' @param pred_pattern_by_product Named character vector: product -> regex to
#'   select predictor columns for that product.
#' @param topK Integer, number of top products to keep in the fusion (by KGE).
#' @param final_fuser Name of the meta-learner to use (subset of \code{SUPPORTED_FUSERS}).
#' @param basins Optional vector of basin IDs to process; default uses all found.
#' @param parallel Logical, run basins in parallel using \pkg{furrr}.
#' @param workers Integer, number of parallel workers when `parallel = TRUE`.
#' @param quiet Logical; if \code{FALSE}, emits informative messages.
#' @param ... Additional arguments passed to \code{wass2s_run_basin_mods_stat}.
#'
#' @return Named list keyed by basin id, each element the result of
#'   `wass2s_run_basin_mods_stat()`.
#' @examples
#' \dontrun{
#' res <- wass2s_run_basins_stat(data_by_product, pred_pattern_by_product = c(SST_CMCC="^pt_"))
#' }
#' @seealso [wass2s_run_basin_mods_stat()], [wass2s_cons_mods_stat()]
#' @export
wass2s_run_basins_stat <- function(data_by_product,
                                   hybas_id = "HYBAS_ID",
                                   pred_pattern_by_product = NULL,
                                   topK = 3,
                                   final_fuser = "rf",
                                   basins = NULL,
                                   parallel = FALSE,
                                   workers = 4,
                                   quiet = TRUE,
                                   ...) {

  .require_pkg(engine_pkg[final_fuser])

  if (length(data_by_product) == 0) {
    stop("data_by_product cannot be empty", call. = FALSE)
  }

  if (!hybas_id %in% names(data_by_product[[1]])) {
    stop("hybas_id column '", hybas_id, "' not found in data", call. = FALSE)
  }

  if (topK < 1) {
    stop("topK must be at least 1", call. = FALSE)
  }

  all_basins <- unique(unlist(lapply(data_by_product, function(df) unique(df[[hybas_id]]))))
  all_basins <- all_basins[!is.na(all_basins)]

  if (length(all_basins) == 0) {
    stop("No basins found in the data", call. = FALSE)
  }

  if (!is.null(basins)) {
    all_basins <- intersect(all_basins, basins)
    if (length(all_basins) == 0) {
      stop("None of the requested basins found in the data", call. = FALSE)
    }
  }

  if (!quiet) message("Processing ", length(all_basins), " basins")

  runner <- function(bid) {
    if (!quiet) message("Processing basin: ", bid)

    tryCatch({
      wass2s_run_basin_mods_stat(
        basin_id = bid,
        data_by_product = data_by_product,
        hybas_id = hybas_id,
        pred_pattern_by_product = pred_pattern_by_product,
        topK = topK,
        quiet = quiet,
        ...
      )
    }, error = function(e) {
      warning("Error processing basin ", bid, ": ", e$message)

      list(
        fused_by_model = tibble::tibble(YYYY = integer(), Q = numeric()),
        final_test = tibble::tibble(YYYY = integer(), Q = numeric(), pred_final = numeric()),
        scores = tibble::tibble(
          HYBAS_ID = bid,
          kge_final = NA_real_,
          kge_pcr = NA_real_,
          kge_ridge = NA_real_,
          kge_lasso = NA_real_,
          rmse_final = NA_real_
        ),
        leaderboards = list(
          PCR = tibble::tibble(product = character(), kge = numeric(), n_pred = integer(), sd_pred = numeric(), weight = numeric()),
          RIDGE = tibble::tibble(product = character(), kge = numeric(), n_pred = integer(), sd_pred = numeric(), weight = numeric()),
          LASSO = tibble::tibble(product = character(), kge = numeric(), n_pred = integer(), sd_pred = numeric(), weight = numeric())
        ),
        cv_rs = NULL,
        error = e$message
      )
    })
  }

  if (parallel) {
    old_plan <- future::plan(future::multisession, workers = workers)
    on.exit(future::plan(old_plan), add = TRUE)

    if (!quiet) message("Running in parallel with ", workers, " workers")

    res <- furrr::future_map(
      all_basins,
      runner,
      .progress = !quiet,
      .options = furrr::furrr_options(
        seed = TRUE,
        packages = c("dplyr", "recipes", "workflows", "parsnip", "tune", "yardstick", "purrr", "tibble")
      )
    )
  } else {
    if (!quiet) message("Running sequentially")
    res <- purrr::map(all_basins, runner)
  }

  names(res) <- as.character(all_basins)

  success_count <- sum(sapply(res, function(x) is.null(x$error)))
  if (!quiet) {
    message("Completed: ", success_count, " successful, ",
            length(all_basins) - success_count, " failed")
  }

  res
}

# wass2s_run_basins_stat <- function(data_by_product,
#                                    hybas_id = "HYBAS_ID",
#                                    pred_pattern_by_product = NULL,
#                                    topK = 3,
#                                    final_fuser = "rf",
#                                    basins = NULL,
#                                    parallel = FALSE,
#                                    workers = 4,
#                                    quiet = TRUE,
#                                    ...) {
#
#   .require_pkg(engine_pkg[final_fuser])
#
#   if (length(data_by_product) == 0) {
#     stop("data_by_product cannot be empty", call. = FALSE)
#   }
#
#   if (!hybas_id %in% names(data_by_product[[1]])) {
#     stop("hybas_id column '", hybas_id, "' not found in data", call. = FALSE)
#   }
#
#   if (topK < 1) {
#     stop("topK must be at least 1", call. = FALSE)
#   }
#
#   all_basins <- unique(unlist(lapply(data_by_product, function(df) unique(df[[hybas_id]]))))
#   all_basins <- all_basins[!is.na(all_basins)]
#
#   if (length(all_basins) == 0) {
#     stop("No basins found in the data", call. = FALSE)
#   }
#
#   if (!is.null(basins)) {
#     all_basins <- intersect(all_basins, basins)
#     if (length(all_basins) == 0) {
#       stop("None of the requested basins found in the data", call. = FALSE)
#     }
#   }
#
#   if (!quiet) message("Processing ", length(all_basins), " basins")
#
#   runner <- function(bid) {
#     if (!quiet) message("Processing basin: ", bid)
#
#     tryCatch({
#       wass2s_run_basin_mods_stat(
#         basin_id = bid,
#         data_by_product = data_by_product,
#         hybas_id = hybas_id,
#         pred_pattern_by_product = pred_pattern_by_product,
#         topK = topK,
#         quiet = quiet,
#         ...
#       )
#     }, error = function(e) {
#       warning("Error processing basin ", bid, ": ", e$message)
#
#       list(
#         fused_by_model = tibble::tibble(YYYY = integer(), Q = numeric()),
#         final_test = tibble::tibble(YYYY = integer(), Q = numeric(), pred_final = numeric()),
#         scores = tibble::tibble(
#           HYBAS_ID = bid,
#           kge_final = NA_real_,
#           kge_pcr = NA_real_,
#           kge_ridge = NA_real_,
#           kge_lasso = NA_real_,
#           rmse_final = NA_real_
#         ),
#         leaderboards = list(
#           PCR = tibble::tibble(product = character(), kge = numeric(), n_pred = integer(), sd_pred = numeric(), weight = numeric()),
#           RIDGE = tibble::tibble(product = character(), kge = numeric(), n_pred = integer(), sd_pred = numeric(), weight = numeric()),
#           LASSO = tibble::tibble(product = character(), kge = numeric(), n_pred = integer(), sd_pred = numeric(), weight = numeric())
#         ),
#         cv_rs = NULL,
#         error = e$message
#       )
#     })
#   }
#
#   if (parallel) {
#     old_plan <- future::plan(future::multisession, workers = workers)
#     on.exit(future::plan(old_plan), add = TRUE)
#
#     if (!quiet) message("Running in parallel with ", workers, " workers")
#
#     res <- furrr::future_map(
#       all_basins,
#       runner,
#       .progress = !quiet,
#       .options = furrr::furrr_options(
#         seed = TRUE,
#         packages = c("dplyr", "recipes", "workflows", "parsnip", "tune", "yardstick", "purrr", "tibble")
#       )
#     )
#   } else {
#     if (!quiet) message("Running sequentially")
#     res <- purrr::map(all_basins, runner)
#   }
#
#   names(res) <- as.character(all_basins)
#
#   success_count <- sum(sapply(res, function(x) is.null(x$error)))
#   if (!quiet) {
#     message("Completed: ", success_count, " successful, ",
#             length(all_basins) - success_count, " failed")
#   }
#
#   res
# }

# wass2s_run_basins_stat <- function(data_by_product,
#                                    hybas_id = "HYBAS_ID",
#                                    pred_pattern_by_product = NULL,
#                                    topK = 3,
#                                    final_fuser = "rf",
#                                    basins = NULL,
#                                    parallel = FALSE,
#                                    workers = 4,
#                                    quiet = TRUE,
#                                    ...) {
#   # Input validation
#   .require_pkg(engine_pkg[final_fuser])
#
#   if (length(data_by_product) == 0) {
#     stop("data_by_product cannot be empty", call. = FALSE)
#   }
#
#   if (!hybas_id %in% names(data_by_product[[1]])) {
#     stop("hybas_id column '", hybas_id, "' not found in data", call. = FALSE)
#   }
#
#   if (topK < 1) {
#     stop("topK must be at least 1", call. = FALSE)
#   }
#
#   # Get all basins present (union across all products)
#   all_basins <- unique(unlist(lapply(data_by_product, function(df) {
#     unique(df[[hybas_id]])
#   })))
#
#   all_basins <- all_basins[!is.na(all_basins)]
#
#   if (length(all_basins) == 0) {
#     stop("No basins found in the data", call. = FALSE)
#   }
#
#   if (!is.null(basins)) {
#     # Filter to requested basins
#     all_basins <- intersect(all_basins, basins)
#     if (length(all_basins) == 0) {
#       stop("None of the requested basins found in the data", call. = FALSE)
#     }
#   }
#
#   if (!quiet) {
#     message("Processing ", length(all_basins), " basins")
#   }
#
#   # Runner function with error handling
#   runner <- function(bid) {
#   message("Processing basin: ", bid)
#
#     tryCatch({
#       wass2s_run_basin_mods_stat(
#         basin_id = bid,
#         data_by_product = data_by_product,
#         hybas_id = hybas_id,
#         pred_pattern_by_product = pred_pattern_by_product,
#         topK = topK,
#         quiet = quiet,
#         ...
#       )
#     }, error = function(e) {
#       warning("Error processing basin ", bid, ": ", e$message)
#       # Return a structured error result
#       list(
#         fused_by_model = tibble::tibble(YYYY = integer(), Q = numeric()),
#         final_test = tibble::tibble(YYYY = integer(), Q = numeric(), pred_final = numeric()),
#         scores = tibble::tibble(
#           HYBAS_ID = bid,
#           kge_final = NA_real_,
#           kge_pcr = NA_real_,
#           kge_ridge = NA_real_,
#           kge_lasso = NA_real_,
#           rmse_final = NA_real_
#         ),
#         leaderboards = list(
#           PCR = tibble::tibble(product = character(), kge = numeric(), n_pred = integer(), sd_pred = numeric(), weight = numeric()),
#           RIDGE = tibble::tibble(product = character(), kge = numeric(), n_pred = integer(), sd_pred = numeric(), weight = numeric()),
#           LASSO = tibble::tibble(product = character(), kge = numeric(), n_pred = integer(), sd_pred = numeric(), weight = numeric())
#         ),
#         cv_rs = NULL,
#         error = e$message
#       )
#     })
#   }
#
#   # Process basins
#   if (parallel) {
#     # Set up parallel processing
#     old_plan <- future::plan(future::multisession, workers = workers)
#     on.exit(future::plan(old_plan), add = TRUE)
#
#     message("Running in parallel with ", workers, " workers")
#
#     res <- furrr::future_map(
#       all_basins,
#       runner,
#       .progress = !quiet,
#       .options = furrr::furrr_options(
#         seed = TRUE,
#         packages = c("dplyr", "recipes", "workflows", "parsnip", "tune", "yardstick", "purrr", "tibble")
#       )
#     )
#   } else {
#     message("Running sequentially")
#     res <- purrr::map(all_basins, runner)
#   }
#
#   # Name the results
#   names(res) <- as.character(all_basins)
#
#   # Count successes and failures
#   success_count <- sum(sapply(res, function(x) is.null(x$error)))
#   if (TRUE) {
#     message("Completed: ", success_count, " successful, ",
#             length(all_basins) - success_count, " failed")
#   }
#
#   # Return results
#   res
# }


