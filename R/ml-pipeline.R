#' Run basin-level machine learning model consolidation and final fusion
#'
#' This function consolidates predictions from multiple machine learning models
#' for a given basin and applies a final fusion strategy across the retained
#' consolidated model outputs. The final fusion can be based on a meta-learner,
#' a simple mean, a median, or a performance-based weighted mean.
#'
#' @param data_by_product Named list of data frames (one per product).
#' @param basin_id Basin identifier value.
#' @param hybas_id Name of the basin ID column.
#' @param prediction_years Optional numeric vector of length 2 giving the
#'   start and end years for a holdout prediction period. These years
#' @param target Name of the target column (default: `"Q"`).
#' @param date_col Name of the date column (default: `"YYYY"`).
#' @param pred_pattern_by_product Optional per-product regex to select predictors.
#' @param models Character vector of base models to run (subset of \code{SUPPORTED_MODELS}).
#' @param topK Integer, number of best products per base model (default: 3).
#' @param min_kge_model Minimum best KGE required to keep a base model for the basin.
#' @param final_fuser Name of the meta-learner to use (subset of \code{SUPPORTED_FUSERS}).
#' @param grid_levels Grid density for tuning both base models and meta-learner.
#' @param fusion_method Character string specifying the final fusion strategy.
#'   Supported values are:
#'   \itemize{
#'     \item \code{"meta"}: train a meta-learner on the consolidated model predictions;
#'     \item \code{"mean"}: use the simple arithmetic mean across consolidated predictions;
#'     \item \code{"median"}: use the median across consolidated predictions;
#'     \item \code{"weighted_mean"}: use a performance-based weighted mean, where
#'       weights are derived from the Kling-Gupta Efficiency (KGE) computed on the
#'       training subset.
#'   }
#' @param quiet Logical; if \code{FALSE}, emits informative messages.
#' @param verbose_tune A logical for logging results (other than warnings and errors, which are always shown) as they are generated during training in a single R process.
#' @param target_positive Logical. If \code{TRUE}, final fused predictions are
#'   constrained to be non-negative using \code{pmax(pred, 0)}. This is useful
#'   for hydrological targets such as streamflow, which are physically non-negative.
#' @param allow_par Logical. If \code{TRUE}, parallel execution is allowed during
#'   hyperparameter tuning of the meta-learner when \code{fusion_method = "meta"}.
#'   If \code{FALSE}, tuning is forced to run sequentially.
#' @param max_na_frac Numeric in \eqn{[0, 1]}: maximum allowed fraction of missing
#'   values per column before stopping (default \code{0.20} = 20\%).
#' @param impute Character, one of \code{"median"}, \code{"mean"}, or \code{"none"}.
#'   If \code{"none"}, no imputation is performed after the guard (default \code{"median"}).
#' @param require_variance Logical; if \code{TRUE}, stop when a column has zero
#'   standard deviation after imputation (default \code{TRUE}).
#' @param ... Passed to \code{WASS2SHydroR::wass2s_cons_mods_ml}.
#' @details
#' The function first consolidates predictions separately for each requested
#' machine learning model across available products. The resulting consolidated
#' predictions are then merged into a single table aligned by date.
#'
#' A final fusion step is subsequently applied using the method selected through
#' \code{fusion_method}. When \code{fusion_method = "meta"}, a second-level
#' regression model is trained on the consolidated predictions. When
#' \code{fusion_method = "weighted_mean"}, model weights are computed from
#' basin-specific training performance using the Kling-Gupta Efficiency (KGE).
#' @return A list containing:
#' \itemize{
#'   \item \code{fused_by_model}: data frame containing observed values and the
#'     consolidated predictions, along with the final fused prediction
#'     (\code{pred_final});
#'   \item \code{final_test}: the last available row of the fused output table;
#'   \item \code{scores}: combined performance summary for training and testing
#'     subsets;
#'   \item \code{scores_train}: performance metrics computed on the training subset;
#'   \item \code{scores_test}: performance metrics computed on the testing subset;
#'   \item \code{fusion_method}: the fusion strategy effectively used;
#'   \item \code{fusion_weights}: named numeric vector of weights when
#'     \code{fusion_method = "weighted_mean"}, otherwise \code{NULL};
#'   \item \code{leaderboards}: per-model leaderboards returned from the
#'     consolidation stage;
#'   \item \code{cv_rs}: tuning metrics collected from the meta-learner when
#'     \code{fusion_method = "meta"}, otherwise \code{NULL};
#'   \item \code{best_meta_params}: best hyperparameter combination selected for
#'     the meta-learner when applicable, otherwise \code{NULL}.
#' }
#' @examples
#' # wass2s_run_bas_mod_ml(basin_id = 1, data_by_product = lst, models = c("rf","xgb"))
#' @export
#'
wass2s_run_bas_mod_ml <- function(
    data_by_product,
    basin_id,
    prediction_years = NULL,
    target = "Q",
    date_col = "YYYY",
    hybas_id = "HYBAS_ID",
    pred_pattern_by_product = NULL,
    models = SUPPORTED_MODELS,
    topK = 3,
    min_kge_model = -Inf,
    grid_levels = 5,
    fusion_method = c("meta", "mean", "median", "weighted_mean"),
    final_fuser = "rf",
    quiet = TRUE,
    verbose_tune = TRUE,
    target_positive = TRUE,
    allow_par = TRUE,
    max_na_frac = 0.3,
    impute = "median",
    require_variance = TRUE,
    ...
) {
  fusion_method <- match.arg(fusion_method)

  # ----------------------------
  # Validate models & packages
  # ----------------------------
  models <- unique(tolower(models))
  bad <- setdiff(models, SUPPORTED_MODELS)

  if (length(bad)) {
    message("Ignoring unsupported base models: ", paste(bad, collapse = ", "))
    models <- intersect(models, SUPPORTED_MODELS)
  }

  final_fuser <- match.arg(final_fuser, SUPPORTED_FUSERS)
  .require_pkg(engine_pkg[c(final_fuser, models)])

  # ----------------------------
  # 1) Consolidate per base model
  # ----------------------------
  outs <- purrr::map(models, function(m) {
    tryCatch({
      cm <- wass2s_cons_mods_ml(
        basin_id = basin_id,
        data_by_product = data_by_product,
        hybas_id = hybas_id,
        target = target,
        date_col = date_col,
        pred_pattern_by_product = pred_pattern_by_product,
        model = m,
        topK = topK,
        min_kge_model = min_kge_model,
        grid_levels = grid_levels,
        quiet = quiet,
        verbose_tune = verbose_tune,
        max_na_frac = max_na_frac,
        impute = impute,
        require_variance = require_variance,
        prediction_years = prediction_years,
        ...
      )

      list(
        model = toupper(m),
        fused = cm$fused,
        leaderboard = cm$leaderboard_products,
        all_results = cm$all_results
      )
    }, error = function(e) {
      if (!quiet) message("Error processing model ", m, ": ", e$message)

      list(
        model = toupper(m),
        fused = NULL,
        leaderboard = tibble::tibble(product = character(), kge = numeric()),
        all_results = list()
      )
    })
  })

  fused_list <- purrr::set_names(
    purrr::map(outs, "fused"),
    purrr::map_chr(outs, "model")
  )
  fused_list_compact <- purrr::compact(fused_list)

  # ----------------------------
  # 2) Get observed series (YYYY/Q) and enforce YYYYMMDD
  # ----------------------------
  any_df <- tryCatch({
    get_any_Q(data_by_product, basin_id, hybas_id)
  }, error = function(e) {
    if (!quiet) message("Error getting base data: ", e$message)
    tibble::tibble(YYYY = integer(), Q = numeric())
  })

  if (nrow(any_df) > 0) {
    # Standardize names if user passed different names
    if (date_col %in% names(any_df) && !"YYYY" %in% names(any_df)) {
      any_df <- dplyr::rename(any_df, YYYY = !!rlang::sym(date_col))
    }
    if (target %in% names(any_df) && !"Q" %in% names(any_df)) {
      any_df <- dplyr::rename(any_df, Q = !!rlang::sym(target))
    }

    if (!"YYYY" %in% names(any_df) || !"Q" %in% names(any_df)) {
      any_df <- tibble::tibble(YYYY = integer(), Q = numeric())
    } else {
      any_df$YYYY <- .ensure_yyyymmdd(any_df$YYYY)
      any_df <- dplyr::arrange(any_df, YYYY)
    }
  }

  any_df <- .sanitize_numeric_columns(
    df = any_df,
    cols = "Q",
    max_na_frac = max_na_frac,
    impute = impute,
    require_variance = require_variance
  )

  # ----------------------------
  # Case A: no retained model
  # ----------------------------
  if (length(fused_list_compact) == 0L) {
    if (!quiet) {
      warning(glue::glue(
        "No ML model retained for basin {basin_id}: returning empty structures."
      ))
    }

    out_empty <- any_df
    if (!"pred_final" %in% names(out_empty)) {
      out_empty <- dplyr::mutate(out_empty, pred_final = NA_real_)
    }

    return(list(
      fused_by_model = out_empty,
      final_test = dplyr::slice_tail(out_empty, n = 1),
      scores = tibble::tibble(
        HYBAS_ID = basin_id,
        kge = NA_real_,
        rmse = NA_real_,
        split = "test"
      ),
      scores_train = tibble::tibble(
        HYBAS_ID = basin_id,
        kge = NA_real_,
        rmse = NA_real_
      ),
      scores_test = tibble::tibble(
        HYBAS_ID = basin_id,
        kge = NA_real_,
        rmse = NA_real_
      ),
      fusion_method = fusion_method,
      fusion_weights = NULL,
      leaderboards = stats::setNames(
        purrr::map(outs, "leaderboard"),
        purrr::map_chr(outs, "model")
      ),
      cv_rs = NULL,
      best_meta_params = NULL
    ))
  }

  # ----------------------------
  # 3) Join consolidated series across base models
  # ----------------------------
  fused_list_compact2 <- purrr::imap(fused_list_compact, ~ {
    if (!is.data.frame(.x)) {
      stop("Fused object for model ", .y, " is not a data.frame.", call. = FALSE)
    }
    if (!all(c("YYYY", "pred_fused") %in% names(.x))) {
      stop(
        "Fused object for model ", .y,
        " must contain columns 'YYYY' and 'pred_fused'. Found: ",
        paste(names(.x), collapse = ", "),
        call. = FALSE
      )
    }

    out <- dplyr::rename(.x, pred = pred_fused)
    out$YYYY <- .ensure_yyyymmdd(out$YYYY)
    out
  })

  fused_models_pred <- tryCatch({
    safe_full_join_preds(fused_list_compact2)
  }, error = function(e) {
    if (!quiet) message("Error joining predictions: ", e$message)
    yyyys <- unique(unlist(lapply(fused_list_compact2, function(x) x$YYYY)))
    tibble::tibble(YYYY = sort(yyyys))
  })

  if ("YYYY" %in% names(fused_models_pred)) {
    fused_models_pred$YYYY <- .ensure_yyyymmdd(fused_models_pred$YYYY)
    fused_models_pred <- dplyr::arrange(fused_models_pred, YYYY)
  }

  if (nrow(any_df) == 0L) {
    any_df <- tibble::tibble(YYYY = fused_models_pred$YYYY, Q = NA_real_)
  }

  fused_models <- dplyr::left_join(any_df, fused_models_pred, by = "YYYY") %>%
    dplyr::arrange(YYYY)

  # ----------------------------
  # Case B: no data after merge
  # ----------------------------
  if (nrow(fused_models) == 0L) {
    if (!quiet) {
      warning(glue::glue(
        "No data available for basin {basin_id} after processing."
      ))
    }

    return(list(
      fused_by_model = fused_models,
      final_test = tibble::tibble(YYYY = integer(), Q = numeric(), pred_final = numeric()),
      scores = tibble::tibble(
        HYBAS_ID = basin_id,
        kge = NA_real_,
        rmse = NA_real_,
        split = "test"
      ),
      scores_train = tibble::tibble(
        HYBAS_ID = basin_id,
        kge = NA_real_,
        rmse = NA_real_
      ),
      scores_test = tibble::tibble(
        HYBAS_ID = basin_id,
        kge = NA_real_,
        rmse = NA_real_
      ),
      fusion_method = fusion_method,
      fusion_weights = NULL,
      leaderboards = stats::setNames(
        purrr::map(outs, "leaderboard"),
        purrr::map_chr(outs, "model")
      ),
      cv_rs = NULL,
      best_meta_params = NULL
    ))
  }

  # ----------------------------
  # 4) Centralized final fusion
  # ----------------------------
  fusion_res <- .wass2s_fuse_predictions(
    fused_models = fused_models,
    basin_id = basin_id,
    target = "Q",
    date_col = "YYYY",
    prediction_years = prediction_years,
    fusion_method = fusion_method,
    final_fuser = final_fuser,
    grid_levels = grid_levels,
    quiet = quiet,
    verbose_tune = verbose_tune,
    allow_par = allow_par,
    target_positive = target_positive
  )

  if (!quiet) {
    message(glue::glue(
      "Successfully fused {length(models)} base model(s) for basin {basin_id} ",
      "using fusion method '{fusion_res$fusion_method}'."
    ))
  }

  list(
    fused_by_model = fusion_res$fused_by_model,
    final_test = fusion_res$final_test,
    scores = fusion_res$scores,
    scores_train = fusion_res$scores_train,
    scores_test = fusion_res$scores_test,
    fusion_method = fusion_res$fusion_method,
    fusion_weights = fusion_res$fusion_weights,
    leaderboards = stats::setNames(
      purrr::map(outs, "leaderboard"),
      purrr::map_chr(outs, "model")
    ),
    cv_rs = fusion_res$cv_rs,
    best_meta_params = fusion_res$best_meta_params
  )
}

# wass2s_run_bas_mod_ml_ <- function(
#     data_by_product,
#     basin_id,
#     prediction_years = NULL,
#     target = "Q",
#     date_col = "YYYY",
#     hybas_id = "HYBAS_ID",
#     pred_pattern_by_product = NULL,
#     models = SUPPORTED_MODELS,
#     topK = 3,
#     min_kge_model = -Inf,
#     grid_levels = 5,
#     final_fuser = "rf",
#     quiet = TRUE,
#     verbose_tune = TRUE,
#     max_na_frac = 0.3,
#     impute = "median",
#     require_variance = TRUE,
#     ...
# ) {
#   # ----------------------------
#   # Validate models & packages
#   # ----------------------------
#   models <- unique(tolower(models))
#   bad <- setdiff(models, SUPPORTED_MODELS)
#   if (length(bad)) {
#     message("Ignoring unsupported base models: ", paste(bad, collapse = ", "))
#     models <- intersect(models, SUPPORTED_MODELS)
#   }
#   final_fuser <- match.arg(final_fuser, SUPPORTED_FUSERS)
#   .require_pkg(engine_pkg[c(final_fuser, models)])
#
#   # ----------------------------
#   # 1) Consolidate per base model
#   # ----------------------------
#   outs <- purrr::map(models, function(m) {
#     tryCatch({
#       cm <- wass2s_cons_mods_ml(
#         basin_id = basin_id,
#         data_by_product = data_by_product,
#         hybas_id = hybas_id,
#         target = target,
#         date_col = date_col,
#         pred_pattern_by_product = pred_pattern_by_product,
#         model = m,
#         topK = topK,
#         min_kge_model = min_kge_model,
#         grid_levels = grid_levels,
#         quiet = quiet,
#         verbose_tune = verbose_tune,
#         max_na_frac = max_na_frac,
#         impute = impute,
#         require_variance = require_variance,
#         prediction_years = prediction_years,
#         ...
#       )
#       list(
#         model = toupper(m),
#         fused = cm$fused,
#         leaderboard = cm$leaderboard_products,
#         all_results = cm$all_results
#       )
#     }, error = function(e) {
#       if (!quiet) message("Error processing model ", m, ": ", e$message)
#       list(
#         model = toupper(m),
#         fused = NULL,
#         leaderboard = tibble::tibble(product = character(), kge = numeric()),
#         all_results = list()
#       )
#     })
#   })
#
#   fused_list <- purrr::set_names(purrr::map(outs, "fused"), purrr::map_chr(outs, "model"))
#   fused_list_compact <- purrr::compact(fused_list)
#
#   # ----------------------------
#   # 2) Get observed series (YYYY/Q) and enforce YYYYMMDD
#   # ----------------------------
#   any_df <- tryCatch({
#     get_any_Q(data_by_product, basin_id, hybas_id)
#   }, error = function(e) {
#     if (!quiet) message("Error getting base data: ", e$message)
#     tibble::tibble(YYYY = integer(), Q = numeric())
#   })
#
#   # Standardize names if needed and enforce YYYYMMDD
#   if (nrow(any_df) > 0) {
#     # If user passed different names, standardize here
#     if (date_col %in% names(any_df) && !"YYYY" %in% names(any_df)) {
#       any_df <- dplyr::rename(any_df, YYYY = !!rlang::sym(date_col))
#     }
#     if (target %in% names(any_df) && !"Q" %in% names(any_df)) {
#       any_df <- dplyr::rename(any_df, Q = !!rlang::sym(target))
#     }
#
#     if (!"YYYY" %in% names(any_df) || !"Q" %in% names(any_df)) {
#       # Safe fallback
#       any_df <- tibble::tibble(YYYY = integer(), Q = numeric())
#     } else {
#       any_df$YYYY <- .ensure_yyyymmdd(any_df$YYYY)
#       any_df <- dplyr::arrange(any_df, YYYY)
#     }
#   }
#
#   any_df <- .sanitize_numeric_columns(
#     df   = any_df,
#     cols = "Q",
#     max_na_frac = max_na_frac,
#     impute = impute,
#     require_variance = require_variance
#   )
#
#   # ----------------------------
#   # Case A: no retained model
#   # ----------------------------
#   if (length(fused_list_compact) == 0L) {
#     if (!quiet) warning(glue::glue(
#       "No ML model retained for basin {basin_id}: returning empty structures."
#     ))
#     return(list(
#       fused_by_model = any_df,
#       final_test = dplyr::mutate(dplyr::slice_tail(any_df, n = 1), pred_final = NA_real_),
#       scores = tibble::tibble(
#         HYBAS_ID = basin_id,
#         kge_final = NA_real_,
#         rmse_final = NA_real_
#       ),
#       leaderboards = stats::setNames(purrr::map(outs, "leaderboard"), purrr::map_chr(outs, "model")),
#       cv_rs = NULL
#     ))
#   }
#
#   # ----------------------------
#   # 3) Join consolidated series across base models
#   # ----------------------------
#   fused_list_compact2 <- purrr::imap(fused_list_compact, ~ dplyr::rename(.x, pred = pred_fused))
#
#   fused_models <- tryCatch({
#     safe_full_join_preds(fused_list_compact2)
#   }, error = function(e) {
#     if (!quiet) message("Error joining predictions: ", e$message)
#     yyyys <- unique(unlist(lapply(fused_list_compact2, function(x) x$YYYY)))
#     tibble::tibble(YYYY = sort(yyyys))
#   })
#
#   # Ensure YYYYMMDD in fused_models too
#   if ("YYYY" %in% names(fused_models)) {
#     fused_models$YYYY <- .ensure_yyyymmdd(fused_models$YYYY)
#     fused_models <- dplyr::arrange(fused_models, YYYY)
#   }
#
#   # Add observed Q (align by YYYYMMDD)
#   if (nrow(any_df) == 0) {
#     any_df <- tibble::tibble(YYYY = fused_models$YYYY, Q = NA_real_)
#   }
#   fused_models <- dplyr::left_join(any_df, fused_models, by = "YYYY")
#
#   if (nrow(fused_models) == 0) {
#     if (!quiet) warning(glue::glue("No data available for basin {basin_id} after processing"))
#     return(list(
#       fused_by_model = fused_models,
#       final_test = tibble::tibble(YYYY = integer(), Q = numeric(), pred_final = numeric()),
#       scores = tibble::tibble(
#         HYBAS_ID = basin_id,
#         kge_final = NA_real_,
#         rmse_final = NA_real_
#       ),
#       leaderboards = stats::setNames(purrr::map(outs, "leaderboard"), purrr::map_chr(outs, "model")),
#       cv_rs = NULL
#     ))
#   }
#
#   # ----------------------------
#   # 4) Meta-learner (final fusion)
#   # ----------------------------
#   bounds <- .pred_years_to_bounds(prediction_years)
#
#   if (!is.null(bounds)) {
#     df_te <- dplyr::filter(fused_models, YYYY >= bounds[1], YYYY <= bounds[2])
#     df_tr <- dplyr::filter(fused_models, !(YYYY >= bounds[1] & YYYY <= bounds[2]))
#   } else {
#     train_idx <- 1:(nrow(fused_models) - 1L)
#     df_tr <- fused_models[train_idx, ]
#     df_te <- fused_models[-train_idx, ]
#   }
#
#   # Fallback if too few training rows
#   if (nrow(df_tr) < 5L) {
#     if (!quiet) warning(glue::glue(
#       "Too few training rows ({nrow(df_tr)}) for basin {basin_id}: using simple mean across predictors."
#     ))
#
#     pred_cols <- setdiff(names(fused_models), c("YYYY", "Q"))
#     if (length(pred_cols) == 0) {
#       final <- fused_models |>
#         dplyr::mutate(pred_final = NA_real_)
#     } else {
#       final <- fused_models |>
#         dplyr::mutate(pred_final = rowMeans(
#           dplyr::select(., dplyr::all_of(pred_cols)),
#           na.rm = TRUE
#         )) |>
#         dplyr::mutate(pred_final = ifelse(is.nan(pred_final), NA_real_, pred_final))
#     }
#
#     # Score on training portion if possible
#     train_pred <- final |>
#       dplyr::filter(YYYY %in% df_tr$YYYY) |>
#       dplyr::filter(!is.na(Q), !is.na(pred_final))
#
#     kge_val <- if (nrow(train_pred) > 0) kge_vec(train_pred$Q, train_pred$pred_final) else NA_real_
#     rmse_val <- if (nrow(train_pred) > 0) yardstick::rmse_vec(train_pred$Q, train_pred$pred_final) else NA_real_
#
#     return(list(
#       fused_by_model = final,
#       final_test = dplyr::slice_tail(final, n = 1),
#       scores = tibble::tibble(
#         HYBAS_ID = basin_id,
#         kge_final = kge_val,
#         rmse_final = rmse_val
#       ),
#       leaderboards = stats::setNames(purrr::map(outs, "leaderboard"), purrr::map_chr(outs, "model")),
#       cv_rs = NULL
#     ))
#   }
#
#   pred_cols <- setdiff(names(df_tr), c("Q", "YYYY"))
#   if (length(pred_cols) < 1L) {
#     stop("wass2s_run_bas_mod_ml(): no meta-features available (only 'Q').", call. = FALSE)
#   }
#
#   rec_meta <- recipes::recipe(df_tr) |>
#     recipes::update_role(!!rlang::sym("Q"), new_role = "outcome") |>
#     recipes::update_role(dplyr::all_of(pred_cols), new_role = "predictor") |>
#     recipes::step_zv(recipes::all_predictors()) |>
#     recipes::step_impute_median(recipes::all_predictors())
#
#   spec <- model_spec(final_fuser, p = length(pred_cols))
#   grid <- model_grid(final_fuser, p = length(pred_cols), levels = grid_levels)
#
#   wf_meta <- workflows::workflow() |>
#     workflows::add_recipe(rec_meta) |>
#     workflows::add_model(spec)
#
#   # Create resamples for tuning
#   rset <- tryCatch({
#     make_rolling(df_tr, year_col = "YYYY", n_splits = 2)
#   }, error = function(e) {
#     if (!quiet) message("Error creating resamples: ", e$message)
#     NULL
#   })
#
#   # Tune meta-learner
#   rs <- NULL
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
#   }
#
#   # Handle tuning failure
#   if (is.null(rs) || length(rs$.metrics) == 0 || nrow(rs$.metrics[[1]]) == 0) {
#     if (!quiet) warning(glue::glue(
#       "Meta-learner tuning failed for basin {basin_id}: fallback to simple mean."
#     ))
#
#     final <- fused_models |>
#       dplyr::mutate(pred_final = rowMeans(
#         dplyr::select(., dplyr::all_of(pred_cols)),
#         na.rm = TRUE
#       )) |>
#       dplyr::mutate(pred_final = ifelse(is.nan(pred_final), NA_real_, pred_final))
#
#     train_pred <- final |>
#       dplyr::filter(YYYY %in% df_tr$YYYY) |>
#       dplyr::filter(!is.na(Q), !is.na(pred_final))
#
#     kge_val <- if (nrow(train_pred) > 0) kge_vec(train_pred$Q, train_pred$pred_final) else NA_real_
#     rmse_val <- if (nrow(train_pred) > 0) yardstick::rmse_vec(train_pred$Q, train_pred$pred_final) else NA_real_
#
#     return(list(
#       fused_by_model = final,
#       final_test = dplyr::slice_tail(final, n = 1),
#       scores = tibble::tibble(
#         HYBAS_ID = basin_id,
#         kge_final = kge_val,
#         rmse_final = rmse_val
#       ),
#       leaderboards = stats::setNames(purrr::map(outs, "leaderboard"), purrr::map_chr(outs, "model")),
#       cv_rs = NULL
#     ))
#   }
#
#   best    <- tune::select_best(rs, metric = "rmse")
#   wf_fin  <- tune::finalize_workflow(wf_meta, best)
#   fit_fin <- parsnip::fit(wf_fin, df_tr)
#
#   train_prediction <- df_tr |>
#     dplyr::mutate(pred_final = predict(fit_fin, df_tr)$.pred)
#
#   fused_models$pred_final <- predict(fit_fin, fused_models)$.pred
#
#   scores <- tibble::tibble(
#     HYBAS_ID   = basin_id,
#     kge_final  = kge_vec(train_prediction$Q, train_prediction$pred_final),
#     rmse_final = yardstick::rmse_vec(train_prediction$Q, train_prediction$pred_final)
#   )
#
#   message(glue::glue(
#     "Successfully trained and fused {length(models)} base model(s) for basin {basin_id} ",
#     "using meta-learner '{final_fuser}'."
#   ))
#
#   list(
#     fused_by_model = fused_models,
#     cv_rs          = tune::collect_metrics(rs),
#     scores         = scores,
#     leaderboards   = stats::setNames(purrr::map(outs, "leaderboard"),
#                                      purrr::map_chr(outs, "model"))
#   )
# }

# wass2s_run_bas_mod_ml <- function(
#     data_by_product,
#     basin_id,
#     prediction_years = NULL,
#     target = "Q",
#     date_col = "YYYY",
#     hybas_id = "HYBAS_ID",
#     pred_pattern_by_product = NULL,
#     models = SUPPORTED_MODELS,
#     topK = 3,
#     min_kge_model = -Inf,
#     grid_levels = 5,
#     final_fuser = "rf",
#     quiet = TRUE,
#     verbose_tune = TRUE,
#     max_na_frac =0.3,
#     impute = "median",
#     require_variance = TRUE,
#     ...
# ) {
#
#   models <- unique(tolower(models))
#   bad <- setdiff(models, SUPPORTED_MODELS)
#   if (length(bad)) {
#     message("Ignoring unsupported base models: ", paste(bad, collapse = ", "))
#     models <- intersect(models, SUPPORTED_MODELS)
#   }
#   final_fuser <- match.arg(final_fuser, SUPPORTED_FUSERS)
#   .require_pkg(engine_pkg[c(final_fuser,models)])
#
#
#   # 1) Consolidate per base model with error handling
#   outs <- purrr::map(models, function(m) {
#     tryCatch({
#       cm <- wass2s_cons_mods_ml(
#         basin_id = basin_id,
#         data_by_product = data_by_product,
#         hybas_id = hybas_id,
#         pred_pattern_by_product = pred_pattern_by_product,
#         model = m,
#         topK = topK,
#         min_kge_model = min_kge_model,
#         grid_levels = grid_levels,
#         quiet = quiet,
#         max_na_frac =max_na_frac,
#         impute = impute,
#         require_variance =require_variance,
#         ...
#       )
#       list(
#         model = toupper(m),
#         fused = cm$fused,
#         leaderboard = cm$leaderboard_products,
#         all_results = cm$all_results
#       )
#     }, error = function(e) {
#       if (!quiet) message("Error processing model ", m, ": ", e$message)
#       list(
#         model = toupper(m),
#         fused = NULL,
#         leaderboard = tibble::tibble(product = character(), kge = numeric()),
#         all_results = list()
#       )
#     })
#   })
#
#   # Join consolidated series (columns RF/XGB/MLP/…)
#   fused_list <- purrr::set_names(purrr::map(outs, "fused"),
#                                  purrr::map_chr(outs, "model"))
#   fused_list_compact <- purrr::compact(fused_list)
#
#   # Obtain YYYY and Q from any available product (helper)
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
#   # Case A: no retained model
#   if (length(fused_list_compact) == 0L) {
#     if (!quiet) warning(glue::glue(
#       "No ML model retained for basin {basin_id}: returning empty structures."
#     ))
#     return(list(
#       fused_by_model = any_df,
#       final_test = dplyr::mutate(dplyr::slice_tail(any_df, n = 1), pred_final = NA_real_),
#       scores = tibble::tibble(
#         HYBAS_ID = basin_id,
#         kge_final = NA_real_,
#         rmse_final = NA_real_
#       ),
#       leaderboards = stats::setNames(purrr::map(outs, "leaderboard"), purrr::map_chr(outs, "model")),
#       cv_rs = NULL
#     ))
#   }
#
#   fused_list_compact2 <- purrr::imap(fused_list_compact,
#                                      ~ dplyr::rename(.x, pred = pred_fused))
#   # Join all predictions - assuming safe_full_join_preds is defined elsewhere
#   fused_models <- tryCatch({
#     safe_full_join_preds(fused_list_compact2)
#   }, error = function(e) {
#     if (!quiet) message("Error joining predictions: ", e$message)
#     # Fallback: create empty frame with YYYY column
#     yyyys <- unique(unlist(lapply(fused_list_compact2, function(x) x$YYYY)))
#     tibble::tibble(YYYY = sort(yyyys))
#   })
#
#
#   # 3) Add observed Q for this basin
#   # if (nrow(any_df) == 0) {
#   #   any_df <- tibble::tibble(YYYY = fused_models$YYYY, Q = NA_real_)
#   #   fused_models <- dplyr::left_join(any_df, fused_models, by = "YYYY")
#   #   return(fused_models)
#   # }
#
#   # Add observed Q for this basin
#   if (nrow(any_df) == 0) {
#     any_df <- tibble::tibble(YYYY = fused_models$YYYY, Q = NA_real_)
#   }
#
#   fused_models <- dplyr::left_join(any_df, fused_models, by = "YYYY")
#
#   # Handle case where we have no data
#   if (nrow(fused_models) == 0) {
#     if (!quiet) warning(glue::glue("No data available for basin {basin_id} after processing"))
#     return(list(
#       fused_by_model = fused_models,
#       final_test = tibble::tibble(YYYY = integer(), Q = numeric(), pred_final = numeric()),
#       scores = tibble::tibble(
#         HYBAS_ID = basin_id,
#         kge_final = NA_real_,
#         rmse_final = NA_real_
#       ),
#       leaderboards = stats::setNames(purrr::map(outs, "leaderboard"), purrr::map_chr(outs, "model")),
#       cv_rs = NULL
#     ))
#   }
#
#   # 4) Meta-learner (final fusion)
#   # Split data based on prediction_years or use last year as test
#   if (!is.null(prediction_years)) {
#     if (length(prediction_years) != 2) {
#       stop("prediction_years must be length 2 (start, end).", call. = FALSE)
#     }
#     if (prediction_years[1] > prediction_years[2]) {
#       stop("Starting point must be earlier than ending point.", call. = FALSE)
#     }
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
#     train_idx <- 1:(nrow(fused_models) - 1L)
#     df_tr <- fused_models[train_idx, ]
#     df_te <- fused_models[-train_idx, ]
#   }
#
#   # Fallback if too few training rows
#   if (nrow(df_tr) < 5L) {
#     if (!quiet) warning(glue::glue(
#       "Too few training rows ({nrow(df_tr)}) for basin {basin_id}: using simple mean across predictors."
#     ))
#
#     pred_cols <- setdiff(names(fused_models), c("YYYY", "Q"))
#     if (length(pred_cols) == 0) {
#       # No predictor columns available
#       final <- fused_models |>
#         dplyr::mutate(pred_final = NA_real_)
#     } else {
#       # Calculate mean of available predictions
#       final <- fused_models |>
#         dplyr::mutate(pred_final = rowMeans(
#           dplyr::select(., dplyr::all_of(pred_cols)),
#           na.rm = TRUE
#         )) |>
#         # Replace NaN with NA (if all values were NA)
#         dplyr::mutate(pred_final = ifelse(is.nan(pred_final), NA_real_, pred_final))
#     }
#
#     # Calculate scores on training data if available
#     if (nrow(df_tr) > 0) {
#       train_pred <- final |>
#         dplyr::filter(YYYY %in% df_tr$YYYY) |>
#         dplyr::filter(!is.na(Q), !is.na(pred_final))
#
#       if (nrow(train_pred) > 0) {
#         kge_val <- kge_vec(train_pred$Q, train_pred$pred_final)
#         rmse_val <- yardstick::rmse_vec(train_pred$Q, train_pred$pred_final)
#       } else {
#         kge_val <- NA_real_
#         rmse_val <- NA_real_
#       }
#     } else {
#       kge_val <- NA_real_
#       rmse_val <- NA_real_
#     }
#
#     return(list(
#       fused_by_model = final,
#       final_test = dplyr::slice_tail(final, n = 1),
#       scores = tibble::tibble(
#         HYBAS_ID = basin_id,
#         kge_final = kge_val,
#         rmse_final = rmse_val
#       ),
#       leaderboards = stats::setNames(purrr::map(outs, "leaderboard"), purrr::map_chr(outs, "model")),
#       cv_rs = NULL
#     ))
#   }
#
#   if (!is.null(prediction_years)) {
#     if (length(prediction_years) != 2) {
#       stop("prediction_years must be length 2 (start, end).", call. = FALSE)
#     }
#     if (prediction_years[1] > prediction_years[2]) {
#       stop("Starting point must be earlier than ending point.", call. = FALSE)
#     }
#     df_te <- dplyr::filter(
#       fused_models,
#       YYYY >= prediction_years[1], YYYY <= prediction_years[2]
#     )
#     df_tr <- dplyr::filter(
#       fused_models,
#       !(YYYY >= prediction_years[1] & YYYY <= prediction_years[2])
#     )
#   }
#
#   pred_cols <- setdiff(names(df_tr), c("Q", "YYYY"))
#   if (length(pred_cols) < 1L) {
#     stop("wass2s_run_bas_mod_ml(): no meta-features available (only 'Q').", call. = FALSE)
#   }
#
#   rec_meta <- recipes::recipe(df_tr) |>
#     recipes::update_role(!!rlang::sym("Q"), new_role = "outcome") |>
#     recipes::update_role(dplyr::all_of(pred_cols), new_role = "predictor") |>
#     recipes::step_zv(recipes::all_predictors()) |>
#     recipes::step_impute_median(recipes::all_predictors())
#
#    spec <- model_spec(final_fuser)
#    grid <- model_grid(final_fuser, p = ncol(df_tr) - 2L, levels = grid_levels)
#
#   wf_meta <- workflows::workflow() |>
#     workflows::add_recipe(rec_meta) |>
#     workflows::add_model(spec)
#
#   # Create resamples for tuning
#   rset <- tryCatch({
#     make_rolling(df_tr, n_splits = 2)
#   }, error = function(e) {
#     if (!quiet) message("Error creating resamples: ", e$message)
#     NULL
#   })
#
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
#   # Handle tuning failure
#   if (is.null(rs) || nrow(rs$.metrics[[1]]) == 0) {
#     if (!quiet) warning(glue::glue(
#       "Meta-learner tuning failed for basin {basin_id}: fallback to simple mean."
#     ))
#
#     final <- fused_models |>
#       dplyr::mutate(pred_final = rowMeans(
#         dplyr::select(., dplyr::all_of(pred_cols)),
#         na.rm = TRUE
#       )) |>
#       dplyr::mutate(pred_final = ifelse(is.nan(pred_final), NA_real_, pred_final))
#
#     # Calculate scores on training data
#     train_pred <- final |>
#       dplyr::filter(YYYY %in% df_tr$YYYY) |>
#       dplyr::filter(!is.na(Q), !is.na(pred_final))
#
#     if (nrow(train_pred) > 0) {
#       kge_val <- kge_vec(train_pred$Q, train_pred$pred_final)
#       rmse_val <- yardstick::rmse_vec(train_pred$Q, train_pred$pred_final)
#     } else {
#       kge_val <- NA_real_
#       rmse_val <- NA_real_
#     }
#
#     return(list(
#       fused_by_model = final,
#       final_test = dplyr::slice_tail(final, n = 1),
#       scores = tibble::tibble(
#         HYBAS_ID = basin_id,
#         kge_final = kge_val,
#         rmse_final = rmse_val
#       ),
#       leaderboards = stats::setNames(purrr::map(outs, "leaderboard"), purrr::map_chr(outs, "model")),
#       cv_rs = NULL
#     ))
#   }
#
#   best   <- tune::select_best(rs, metric = "rmse")
#   wf_fin <- tune::finalize_workflow(wf_meta, best)
#   fit_fin <- parsnip::fit(wf_fin, df_tr)
#
#   train_prediction <- df_tr |>
#     dplyr::mutate(pred_final = predict(fit_fin, df_tr)$.pred)
#
#   fused_models$pred_final <- predict(fit_fin, fused_models)$.pred
#
#   scores <- tibble::tibble(
#     HYBAS_ID  = basin_id,
#     kge_final = kge_vec(train_prediction$Q, train_prediction$pred_final),
#     rmse_final = yardstick::rmse_vec(train_prediction$Q, train_prediction$pred_final)
#   )
#
#   message(glue::glue(
#     "Successfully trained and fused {length(models)} base model(s) for basin {basin_id} ",
#     "using meta-learner '{final_fuser}'."
#   ))
#
#   return(list(
#     fused_by_model = fused_models,
#     cv_rs          = tune::collect_metrics(rs),
#     scores         = scores,
#     leaderboards   = stats::setNames(purrr::map(outs, "leaderboard"),
#                                      purrr::map_chr(outs, "model"))
#   ))
# }


#' Run the Hydro + ML pipeline for all basins
#'
#' Iterate over all basin IDs, running \code{wass2s_run_bas_mod_ml()} for
#' each, optionally in parallel. Returns a named list indexed by basin ID.
#'
#' @param data_by_product Named list of data frames (one per product).
#' @param hybas_id Name of the basin ID column.
#' @param pred_pattern_by_product Optional per-product regex for predictor selection.
#' @param prediction_years Optional numeric vector of length 2 giving the
#'   start and end years for a holdout prediction period. These years
#' @param models Character vector of base models to include.
#' @param topK Integer, number of best products per base model.
#' @param min_kge_model Minimum best KGE required to keep a base model.
#' @param basins Optional vector of basin IDs to subset (default: all).
#' @param parallel Logical; if \code{TRUE}, uses \pkg{furrr} for parallel execution.
#' @param workers Integer number of workers when \code{parallel = TRUE}.
#' @param grid_levels Grid density for tuning.
#' @param final_fuser Name of the meta-learner for final fusion.
#' @param quiet Logical; if \code{FALSE}, emits informative messages.
#' @param ... Other parameters passed to \code{wass2s_run_bas_mod_ml}.
#'
#' @return A named list: one element per basin, each the list returned by
#'   \code{wass2s_run_bas_mod_ml()}.
#'
#' @examples
#' # res <- wass2s_run_basins_ml(data_by_product = lst, models = c("rf","xgb"), final_fuser = "glmnet")
#' @export
wass2s_run_basins_ml <- function(
    data_by_product,
    hybas_id = "HYBAS_ID",
    pred_pattern_by_product = NULL,
    prediction_years = NULL,
    models = SUPPORTED_MODELS,
    topK = 3,
    min_kge_model = -Inf,
    basins = NULL,
    parallel = FALSE,
    workers = 4,
    grid_levels = 5,
    final_fuser = "rf",
    quiet = TRUE,
    ...
) {

  .require_pkg(engine_pkg[c(final_fuser, models)])

  if (length(data_by_product) == 0) {
    stop("data_by_product cannot be empty", call. = FALSE)
  }

  if (!hybas_id %in% names(data_by_product[[1]])) {
    stop("hybas_id column not found in data", call. = FALSE)
  }

  # Get all basins if not specified
  if (is.null(basins)) {
    basins <- tryCatch({
      unique(unlist(lapply(data_by_product, function(df) unique(df[[hybas_id]]))))
    }, error = function(e) {
      stop("Error extracting basin IDs: ", e$message, call. = FALSE)
    })
    basins <- sort(basins[!is.na(basins)])
  }

  if (length(basins) == 0) {
    stop("No basins found in the data", call. = FALSE)
  }

  if (!quiet) message("Processing ", length(basins), " basins")

  runner <- function(bid) {
    if (!quiet) message("Processing basin: ", bid)

    tryCatch({
      result <- wass2s_run_bas_mod_ml(
        basin_id = bid,
        data_by_product = data_by_product,
        hybas_id = hybas_id,
        pred_pattern_by_product = pred_pattern_by_product,
        models = models,
        topK = topK,
        min_kge_model = min_kge_model,
        grid_levels = grid_levels,
        final_fuser = final_fuser,
        quiet = quiet,
        prediction_years=prediction_years,
        ...
      )
      result$basin_id <- bid
      result
    }, error = function(e) {
      if (!quiet) warning("Error processing basin ", bid, ": ", e$message)

      list(
        basin_id = bid,
        error = e$message,
        fused_by_model = tibble::tibble(),   # safer than NULL
        final_test = tibble::tibble(),       # safer than NULL
        scores = tibble::tibble(
          HYBAS_ID = bid,
          kge_final = NA_real_,
          rmse_final = NA_real_
        ),
        leaderboards = list(),
        cv_rs = NULL
      )
    })
  }

  if (parallel) {
    if (!requireNamespace("future", quietly = TRUE)) {
      stop("Package 'future' is required for parallel execution. Please install it.", call. = FALSE)
    }
    if (!requireNamespace("furrr", quietly = TRUE)) {
      stop("Package 'furrr' is required for parallel execution. Please install it.", call. = FALSE)
    }

    old_plan <- future::plan(future::multisession, workers = workers)
    on.exit(future::plan(old_plan), add = TRUE)

    if (!quiet) message("Running in parallel with ", workers, " workers")

    res <- furrr::future_map(
      basins,
      runner,
      .progress = !quiet,
      .options = furrr::furrr_options(
        seed = TRUE,
        packages = c("dplyr", "recipes", "workflows", "parsnip", "tune", "yardstick", "purrr", "tibble")
      )
    )
  } else {
    if (!quiet) message("Running sequentially")
    res <- purrr::map(basins, runner)
  }

  names(res) <- as.character(basins)

  success_count <- sum(sapply(res, function(x) is.null(x$error)))
  if (!quiet) {
    message("Completed: ", success_count, " successful, ",
            length(basins) - success_count, " failed")
  }

  res
}

# wass2s_run_basins_ml <- function(
#     data_by_product,
#     hybas_id = "HYBAS_ID",
#     pred_pattern_by_product = NULL,
#     models = SUPPORTED_MODELS,
#     topK = 3,
#     min_kge_model = -Inf,
#     basins = NULL,
#     parallel = FALSE,
#     workers = 4,
#     grid_levels = 5,
#     final_fuser = "rf",
#     quiet = TRUE,
#     ...
# ) {
#   # Input validation
#   .require_pkg(engine_pkg[c(final_fuser,models)])
#
#   if (length(data_by_product) == 0) {
#     stop("data_by_product cannot be empty", call. = FALSE)
#   }
#
#   if (!hybas_id %in% names(data_by_product[[1]])) {
#     stop("hybas_id column not found in data", call. = FALSE)
#   }
#
#   # Get all basins if not specified
#   if (is.null(basins)) {
#     basins <- tryCatch({
#       unique(unlist(lapply(data_by_product, function(df) unique(df[[hybas_id]]))))
#     }, error = function(e) {
#       stop("Error extracting basin IDs: ", e$message, call. = FALSE)
#     })
#
#     basins <- sort(basins[!is.na(basins)])
#   }
#
#   if (length(basins) == 0) {
#     stop("No basins found in the data", call. = FALSE)
#   }
#
#   if (TRUE) message("Processing ", length(basins), " basins")
#
#   # Define the runner function with error handling
#   runner <- function(bid) {
#     if (TRUE) message("Processing basin: ", bid)
#
#     tryCatch({
#       result <- wass2s_run_bas_mod_ml(
#         basin_id = bid,
#         data_by_product = data_by_product,
#         hybas_id = hybas_id,
#         pred_pattern_by_product = pred_pattern_by_product,
#         models = models,
#         topK = topK,
#         min_kge_model = min_kge_model,
#         grid_levels = grid_levels,
#         final_fuser = final_fuser,
#         quiet = quiet,
#         ...
#       )
#
#       # Add basin ID to the result for easier identification
#       result$basin_id <- bid
#       result
#     }, error = function(e) {
#       if (TRUE) warning("Error processing basin ", bid, ": ", e$message)
#
#       # Return a structured error result
#       list(
#         basin_id = bid,
#         error = e$message,
#         fused_by_model = NULL,
#         final_test = NULL,
#         scores = tibble::tibble(
#           HYBAS_ID = bid,
#           kge_final = NA_real_,
#           rmse_final = NA_real_
#         ),
#         leaderboards = list(),
#         cv_rs = NULL
#       )
#     })
#   }
#
#   # Process basins
#   if (parallel) {
#     if (!requireNamespace("future", quietly = TRUE)) {
#       stop("Package 'future' is required for parallel execution. Please install it.")
#     }
#     if (!requireNamespace("furrr", quietly = TRUE)) {
#       stop("Package 'furrr' is required for parallel execution. Please install it.")
#     }
#     # Set up parallel processing
#     old_plan <- future::plan(future::multisession, workers = workers)
#     on.exit(future::plan(old_plan), add = TRUE)
#
#     if (TRUE) message("Running in parallel with ", workers, " workers")
#
#     res <- furrr::future_map(
#       basins,
#       runner,
#       .progress = !quiet,
#       .options = furrr::furrr_options(seed = TRUE)
#     )
#   } else {
#     if (TRUE) message("Running sequentially")
#     res <- purrr::map(basins, runner)
#   }
#
#   # Name the results
#   names(res) <- as.character(basins)
#
#   # Summarize results
#   success_count <- sum(sapply(res, function(x) is.null(x$error)))
#   if (TRUE) message("Completed: ", success_count, " successful, ",
#                       length(basins) - success_count, " failed")
#
#   # Return results
#   res
# }
