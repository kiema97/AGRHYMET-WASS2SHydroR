#' Consolidate top-K products for one basin and one ML model
#'
#' For a given basin and ML model, this function:
#' \enumerate{
#'   \item fits/tunes the model on each product using \code{wass2s_tune_pred_ml()},
#'   \item ranks products by cross-validated KGE,
#'   \item keeps the top-\code{K} products,
#'   \item fuses their predictions either by a robust KGE-weighted mean (default),
#'         or by an optional meta-learner ("sub-fuser") trained on product predictions
#'         when observed \code{Q} is available.
#' }
#'
#' Fusion weights are derived from KGE (negative KGE truncated to 0), then
#' normalized to sum to 1 over the selected products. Products below
#' \code{min_kge_model} can be given weight 0, with safe fallbacks.
#'
#' @param data_by_product Named list of data frames/tibbles per product.
#' @param basin_id Basin identifier value.
#' @param hybas_id Name of the basin ID column (default: "HYBAS_ID").
#' @param target Name of the target column (default: "Q").
#' @param date_col Name of the date column (default: "YYYY").
#' @param pred_pattern_by_product Optional named list/vector of regex by product to select predictors,
#'   or a single regex applied to all products. Default is "^pt_".
#' @param model One of \code{SUPPORTED_MODELS}.
#' @param topK Integer; number of best products to fuse (default: 3).
#' @param target_positive Logical; if \code{TRUE}, force negative fused
#'   predictions to zero.
#' @param min_kge_model Minimum KGE threshold to accept non-zero fusion weight (default: -Inf).
#' @param prediction_years Optional numeric vector of length 2 (start_year, end_year) defining a holdout
#'   period excluded from training (for the sub-fuser training step).
#' @param use_sub_fuser Logical; if TRUE, tries a meta-learner fusion (requires observed Q).
#' @param sub_fuser Character; meta-learner model name passed to \code{model_spec()} (e.g. "rf").
#' @param sub_grid_levels Integer; grid levels for the sub-fuser via \code{model_grid()}.
#' @param pretrained Optional list of pre-trained workflows (indexed by \code{model} then \code{product}).
#' @param grid_levels Tuning grid granularity for the base ML model (passed to \code{wass2s_tune_pred_ml()}).
#' @param min_data_required Minimum number of rows required to train.
#' @param predictors_min Minimum number of predictors required.
#' @param quiet Logical; if FALSE, emits informative messages (default: TRUE).
#' @param verbose Logical; if TRUE, emits diagnostic messages (default: TRUE).
#' @param allow_par Logical; forwarded to tuning controls where applicable.
#' @param max_na_frac Numeric in [0,1]; maximum allowed missingness per guarded column (default: 0.3).
#' @param impute Character; one of "median", "mean", "none" (default: "median").
#' @param require_variance Logical; if TRUE, requires non-zero variance after guard (default: TRUE).
#' @param ... Passed to the underlying tuner/predictor.
#'
#' @return A list with:
#' \itemize{
#'   \item \code{fused}: tibble with columns \code{YYYY}, \code{pred_fused} (and \code{Q} if available).
#'   \item \code{leaderboard_products}: tibble of products with KGE and fusion weights.
#'   \item \code{all_results}: list of per-product results (raw outputs).
#' }
#' @export
#' @importFrom dplyr filter ungroup rename select all_of mutate
#' @importFrom purrr map map_dbl imap_dfr compact map_chr
#' @importFrom stringr str_detect
#' @importFrom tibble tibble
#' @importFrom glue glue
#' @importFrom rlang .data sym
wass2s_cons_mods_ml <- function(
    data_by_product,
    basin_id,
    hybas_id = "HYBAS_ID",
    target = "Q",
    date_col = "YYYY",
    pred_pattern_by_product = NULL,
    model = SUPPORTED_MODELS,
    topK = 3,
    target_positive = TRUE,
    min_kge_model = -Inf,
    prediction_years = NULL,

    # --- fusion options ---
    use_sub_fuser = TRUE,
    sub_fuser = "rf",
    sub_grid_levels = 10,

    # --- tuning options ---
    pretrained = NULL,
    grid_levels = 5,
    min_data_required = 10,
    predictors_min = 1,
    quiet = TRUE,
    verbose = TRUE,
    allow_par = TRUE,

    # --- data quality guards ---
    max_na_frac = 0.3,
    impute = "median",
    require_variance = TRUE,
    ...
) {
  model <- match.arg(model, SUPPORTED_MODELS)

  # ---------------------------
  # Input checks
  # ---------------------------
  prods <- names(data_by_product)
  if (length(prods) == 0) {
    stop("wass2s_cons_mods_ml(): data_by_product must be a named list.", call. = FALSE)
  }
  if (topK < 1) {
    stop("wass2s_cons_mods_ml(): topK must be at least 1.", call. = FALSE)
  }
  if (!is.infinite(min_kge_model) && min_kge_model > 1) {
    stop("wass2s_cons_mods_ml(): min_kge_model cannot be > 1.", call. = FALSE)
  }

  # Holdout bounds in YYYYMMDD (or NULL)
  hold_bounds <- .ensure_year_bounds(prediction_years)

  # ---------------------------
  # Collect all dates for this basin across products (YYYYMMDD)
  # ---------------------------
  dates_all <- sort(unique(unlist(lapply(prods, function(p) {
    dfp <- data_by_product[[p]]
    if (!is.data.frame(dfp)) return(integer())
    if (!hybas_id %in% names(dfp)) return(integer())

    dfp <- dplyr::filter(dfp, .data[[hybas_id]] == basin_id)
    if (!date_col %in% names(dfp)) return(integer())

    .ensure_yyyymmdd(dfp[[date_col]])
  }))))

  if (length(dates_all) == 0) {
    return(list(
      fused = tibble::tibble(YYYY = integer(), pred_fused = numeric()),
      leaderboard_products = tibble::tibble(),
      all_results = list()
    ))
  }

  # ---------------------------
  # 1) Fit/tune per product -> standardized results
  # ---------------------------
  results_std <- purrr::map(prods, function(p) {

    dfp <- data_by_product[[p]]
    if (!is.data.frame(dfp)) {
      .msg(quiet, verbose, "[", model, "] ", p, " : skipped (not a data.frame).")
      return(NULL)
    }
    if (!hybas_id %in% names(dfp)) {
      .msg(quiet, verbose, "[", model, "] ", p, " : skipped (missing basin id col '", hybas_id, "').")
      return(NULL)
    }

    dfp <- dfp |>
      dplyr::filter(.data[[hybas_id]] == basin_id) |>
      dplyr::ungroup()

    # Required columns
    missing_cols <- setdiff(c(target, date_col), names(dfp))
    if (length(missing_cols) > 0) {
      .msg(quiet, verbose, "[", model, "] ", p, " : skipped (missing ",
           paste(missing_cols, collapse = ", "), ").")
      return(NULL)
    }

    # Minimum sample size
    if (nrow(dfp) < min_data_required) {
      .msg(quiet, verbose, "[", model, "] ", p, " : skipped (n=", nrow(dfp),
           " < ", min_data_required, ").")
      return(NULL)
    }

    # Standardize to YYYY / Q
    dfp <- dfp |>
      dplyr::rename(
        YYYY = !!rlang::sym(date_col),
        Q    = !!rlang::sym(target)
      ) |>
      dplyr::mutate(YYYY = .ensure_yyyymmdd(.data$YYYY)) |>
      dplyr::arrange(.data$YYYY)

    # Select predictors
    pat <- .get_pattern(p, pred_pattern_by_product = pred_pattern_by_product)

    candidates <- setdiff(names(dfp), c(hybas_id, "YYYY", "Q"))
    predictors <- candidates[stringr::str_detect(candidates, pat)]

    if (length(predictors) < predictors_min) {
      .msg(quiet, verbose, "[", model, "] ", p, " : skipped (predictors ",
           length(predictors), " < ", predictors_min, ", pattern='", pat, "').")
      return(NULL)
    }

    # Optional pretrained workflow
    pre_wf <- NULL
    if (!is.null(pretrained) &&
        !is.null(pretrained[[model]]) &&
        !is.null(pretrained[[model]][[p]])) {
      pre_wf <- pretrained[[model]][[p]]
    }

    .msg(quiet, verbose, "[", model, "] ", p, " : ", length(predictors),
         " predictors (pattern='", pat, "')")

    out <- tryCatch({
      wass2s_tune_pred_ml(
        df_basin_product = dplyr::select(dfp, YYYY, Q, dplyr::all_of(predictors)),
        predictors       = predictors,
        model            = model,
        grid_levels      = grid_levels,
        pretrained_wflow = pre_wf,
        quiet            = quiet,
        allow_par        = allow_par,
        max_na_frac      = max_na_frac,
        impute           = impute,
        require_variance = require_variance,
        ...
      )
    }, error = function(e) {
      .msg(quiet, verbose, glue::glue("Error training product '{p}' for basin {basin_id}: {e$message}"))
      NULL
    })

    if (is.null(out) || is.null(out$preds) || !all(c("YYYY", "pred") %in% names(out$preds))) {
      .msg(quiet, verbose, "[", model, "] ", p, " : failed/invalid preds.")
      return(NULL)
    }

    preds <- out$preds |>
      dplyr::mutate(YYYY = .ensure_yyyymmdd(.data$YYYY)) |>
      dplyr::arrange(.data$YYYY)

    list(
      product = p,
      score   = out$kge_cv_mean,   # standardized score for the util
      preds   = preds,
      # optional extra info (kept for debugging / downstream)
      leaderboard_cfg = out$leaderboard_cfg,
      fitted_model    = out$fit
    )
  }) |>
    purrr::compact()

  if (length(results_std) == 0) {
    return(list(
      fused = tibble::tibble(YYYY = dates_all, pred_fused = NA_real_),
      leaderboard_products = tibble::tibble(),
      all_results = list()
    ))
  }

  # ---------------------------
  # 2) Delegate fusion to shared util
  # ---------------------------
  fusion <- fuse_products_predictions(
    results           = results_std,
    dates_all         = dates_all,
    topK              = topK,
    min_score         = min_kge_model,
    prediction_years  = prediction_years,   # util converts to YYYYMMDD bounds
    use_sub_fuser     = use_sub_fuser,
    sub_fuser         = sub_fuser,
    sub_grid_levels   = sub_grid_levels,
    min_data_required = min_data_required,
    target_positive   = target_positive,
    quiet             = quiet,
    verbose           = verbose,
    ...
  )

  # Keep old name "kge" expected elsewhere
  if (nrow(fusion$leaderboard_products) > 0 && "score" %in% names(fusion$leaderboard_products)) {
    fusion$leaderboard_products <- fusion$leaderboard_products |>
      dplyr::rename(kge = .data$score)
  }

  # Preserve original all_results semantics if you want:
  # here we return standardized results (recommended), but you can also
  # keep raw (results_std) as is.
  fusion$all_results <- results_std

  fusion
}

# wass2s_cons_mods_ml <- function(
#     data_by_product,
#     basin_id,
#     hybas_id = "HYBAS_ID",
#     target = "Q",
#     date_col = "YYYY",
#     pred_pattern_by_product = NULL,
#     model = SUPPORTED_MODELS,
#     topK = 3,
#     target_positive = TRUE,
#     min_kge_model = -Inf,
#     prediction_years = NULL,
#     use_sub_fuser = TRUE,
#     sub_fuser = "rf",
#     sub_grid_levels = 10,
#     pretrained = NULL,
#     grid_levels = 5,
#     min_data_required = 10,
#     predictors_min = 1,
#     quiet = TRUE,
#     verbose = TRUE,
#     allow_par = TRUE,
#     max_na_frac = 0.3,
#     impute = "median",
#     require_variance = TRUE,
#     ...
# ) {
#   model <- match.arg(model, SUPPORTED_MODELS)
#   # ---------------------------
#   # Validation
#   # ---------------------------
#   prods <- names(data_by_product)
#   if (length(prods) == 0) stop("data_by_product must be a named list.", call. = FALSE)
#   if (topK < 1) stop("topK must be at least 1.", call. = FALSE)
#   if (!is.infinite(min_kge_model) && min_kge_model > 1) {
#     stop("min_kge_model cannot be > 1.", call. = FALSE)
#   }
#
#   hold_bounds <- .ensure_year_bounds(prediction_years)
#
#   # Collect all dates available for this basin across products (YYYYMMDD)
#   dates_all <- sort(unique(unlist(lapply(prods, function(p) {
#     dfp <- data_by_product[[p]]
#     if (!is.data.frame(dfp)) return(integer())
#     if (!hybas_id %in% names(dfp)) return(integer())
#     dfp <- dplyr::filter(dfp, .data[[hybas_id]] == basin_id)
#     if (!date_col %in% names(dfp)) return(integer())
#     .ensure_yyyymmdd(dfp[[date_col]])
#   }))))
#
# if (length(dates_all) == 0) return(.empty_return(integer(), results = list()))
#
# # ---------------------------
# # 1) Fit/tune per product
# # ---------------------------
# results <- purrr::map(prods, function(p) {
#   dfp <- data_by_product[[p]]
#   if (!is.data.frame(dfp)) {
#     .msg(quiet,verbose,"[", model, "] ", p, " : skipped (not a data.frame).")
#     return(NULL)
#   }
#   if (!hybas_id %in% names(dfp)) {
#     .msg(quiet,verbose,"[", model, "] ", p, " : skipped (missing basin id col '", hybas_id, "').")
#     return(NULL)
#   }
#
#   dfp <- dfp %>%
#     dplyr::filter(.data[[hybas_id]] == basin_id) %>%
#     dplyr::ungroup()
#
#   missing_cols <- setdiff(c(target, date_col), names(dfp))
#   if (length(missing_cols) > 0) {
#     .msg(quiet,verbose,"[", model, "] ", p, " : skipped (missing ", paste(missing_cols, collapse = ", "), ").")
#     return(NULL)
#   }
#
#   if (nrow(dfp) < min_data_required) {
#     .msg(quiet,verbose,"[", model, "] ", p, " : skipped (n=", nrow(dfp), " < ", min_data_required, ").")
#     return(NULL)
#   }
#
#   # Standardize names used by downstream
#   dfp <- dfp %>%
#     dplyr::rename(
#       YYYY = !!rlang::sym(date_col),
#       Q    = !!rlang::sym(target)
#     )
#
#   dfp$YYYY <- .ensure_yyyymmdd(dfp$YYYY)
#   dfp <- dfp %>% dplyr::arrange(.data$YYYY)
#
#   pat <- .get_pattern(p,pred_pattern_by_product=pred_pattern_by_product)
#
#   candidates <- setdiff(names(dfp), c(hybas_id, "YYYY", "Q"))
#   predictors <- candidates[purrr::map_lgl(candidates, ~ stringr::str_detect(.x, pat))]
#
#   if (length(predictors) < predictors_min) {
#     .msg(quiet,verbose,"[", model, "] ", p, " : skipped (predictors ", length(predictors),
#          " < ", predictors_min, ", pattern='", pat, "').")
#     return(NULL)
#   }
#
#   # Pretrained workflow (optional)
#   pre_wf <- NULL
#   if (!is.null(pretrained) && !is.null(pretrained[[model]]) && !is.null(pretrained[[model]][[p]])) {
#     pre_wf <- pretrained[[model]][[p]]
#   }
#
#   .msg(quiet,verbose,"[", model, "] ", p, " : ", length(predictors), " predictors (pattern='", pat, "')")
#
#   out <- tryCatch({
#     wass2s_tune_pred_ml(
#       df_basin_product = dfp %>% dplyr::select(YYYY, Q, dplyr::all_of(predictors)),
#       predictors       = predictors,
#       model            = model,
#       grid_levels      = grid_levels,
#       pretrained_wflow = pre_wf,
#       quiet            = quiet,
#       allow_par        = allow_par,
#       max_na_frac      = max_na_frac,
#       impute           = impute,
#       require_variance = require_variance,
#       ...
#     )
#   }, error = function(e) {
#     .msg(quiet,verbose,glue::glue("Error training product '{p}' for basin {basin_id}: {e$message}"))
#     NULL
#   })
#
#   if (is.null(out) || is.null(out$preds) || !all(c("YYYY", "pred") %in% names(out$preds))) {
#     .msg(quiet,verbose,"[", model, "] ", p, " : failed/invalid preds.")
#     return(NULL)
#   }
#
#   # Ensure internal date format
#   out$preds$YYYY <- .ensure_yyyymmdd(out$preds$YYYY)
#
#   list(
#     product         = p,
#     kge             = out$kge_cv_mean,
#     preds           = out$preds,          # expects YYYY, pred (and maybe ID)
#     leaderboard_cfg = out$leaderboard_cfg,
#     fitted_model    = out$fit
#   )
# }) %>% purrr::compact()
#
# if (length(results) == 0) return(.empty_return(dates_all, results = list()))
#
# # ---------------------------
# # 2) Rank, keep topK, define weights
# # ---------------------------
# ord <- order(purrr::map_dbl(results, "kge"), decreasing = TRUE)
# results <- results[ord]
#
# results_top <- utils::head(results, n = min(topK, length(results)))
#
# kge_top <- purrr::map_dbl(results_top, "kge")
# kge_top[!is.finite(kge_top)] <- NA_real_
# kg <- pmax(kge_top, 0)
#
# if (!is.infinite(min_kge_model)) {
#   kg[!is.na(kge_top) & kge_top < min_kge_model] <- 0
# }
#
# # If all zero weights -> fallback to best single product
# if (all(!is.finite(kg)) || all(kg == 0, na.rm = TRUE)) {
#   results_top <- results_top[1]
#   kg <- 1
# }
#
# # Normalize weights
# w <- kg
# w[is.na(w)] <- 0
# if (sum(w) > 0) w <- w / sum(w)
#
# lb <- tibble::tibble(
#   product = purrr::map_chr(results, "product"),
#   kge     = purrr::map_dbl(results, "kge"),
#   weight  = 0
# )
# keep_names <- purrr::map_chr(results_top, "product")
# lb$weight[match(keep_names, lb$product)] <- w
#
# # ---------------------------
# # 3) Build fusion inputs
# # ---------------------------
# preds_long <- purrr::map2_dfr(results_top, seq_along(results_top), ~{
#   dplyr::transmute(
#     .x$preds,
#     YYYY = .ensure_yyyymmdd(.data$YYYY),
#     pred = .data$pred,
#     product = .x$product,
#     w = w[.y]
#   )
# })
#
# # Observed Q timeline (from any product data, if available)
# # ---- Observed Q timeline (robust): use get_any_Q() instead of prods[1] ----
# obs <- tryCatch({
#   any_df <- get_any_Q(data_by_product, basin_id, hybas_id)
#
#   # Standardize names + enforce YYYYMMDD
#   if (!all(c(date_col, target) %in% names(any_df))) {
#     tibble::tibble(YYYY = dates_all, Q = NA_real_)
#   } else {
#     any_df <- any_df %>%
#       dplyr::rename(
#         YYYY = !!rlang::sym(date_col),
#         Q    = !!rlang::sym(target)
#       ) %>%
#       dplyr::select(YYYY, Q)
#
#     any_df$YYYY <- .ensure_yyyymmdd(any_df$YYYY)
#
#     any_df <- .sanitize_numeric_columns(
#       df   = any_df,
#       cols = "Q",
#       max_na_frac = max_na_frac,
#       impute = impute,
#       require_variance = require_variance
#     )
#
#     any_df %>%
#       dplyr::distinct(.data$YYYY, .keep_all = TRUE)
#   }
# }, error = function(e) {
#   if (!quiet && isTRUE(verbose)) message("get_any_Q() failed: ", e$message)
#   tibble::tibble(YYYY = dates_all, Q = NA_real_)
# })
#
#
# # ---------------------------
# # 4) Fusion: simple weighted vs sub-fuser
# # ---------------------------
# if (!isTRUE(use_sub_fuser) || length(results_top) < 2) {
#
#   fused <- preds_long %>%
#     dplyr::group_by(.data$YYYY) %>%
#     dplyr::summarise(
#       pred_fused = .weighted_mean_na(.data$pred, .data$w),
#       .groups = "drop"
#     ) %>%
#     dplyr::left_join(obs, by = "YYYY")
#
# } else {
#
#   # If no observed Q, cannot train sub-fuser -> fallback weighted
#   if (!("Q" %in% names(obs)) || all(is.na(obs$Q))) {
#
#     fused <- preds_long %>%
#       dplyr::group_by(.data$YYYY) %>%
#       dplyr::summarise(
#         pred_fused = .weighted_mean_na(.data$pred, .data$w),
#         .groups = "drop"
#       )
#
#   } else {
#
#     # Wide table: each product is a column (predictors for sub-fuser)
#     lst_wide <- purrr::map(results_top, ~{
#       dplyr::transmute(.x$preds, YYYY = .ensure_yyyymmdd(.data$YYYY), pred = .data$pred) %>%
#         dplyr::rename(!!.x$product := .data$pred)
#     })
#     prods_wide <- Reduce(function(a, b) dplyr::full_join(a, b, by = "YYYY"), lst_wide)
#
#     dat <- dplyr::left_join(obs, prods_wide, by = "YYYY") %>%
#       dplyr::arrange(.data$YYYY)
#
#     # Train data excludes holdout years if provided
#     if (!is.null(hold_bounds)) {
#       df_tr <- dplyr::filter(dat, !(.data$YYYY >= hold_bounds[1] & .data$YYYY <= hold_bounds[2]))
#     } else {
#       df_tr <- dat
#     }
#
#     # If too few rows -> fallback weighted
#     if (nrow(df_tr) < min_data_required) {
#
#       fused <- preds_long %>%
#         dplyr::group_by(.data$YYYY) %>%
#         dplyr::summarise(
#           pred_fused = .weighted_mean_na(.data$pred, .data$w),
#           .groups = "drop"
#         )
#
#     } else {
#
#       # Meta recipe: Q ~ product preds, keep YYYY as id
#       rec_meta <- recipes::recipe(Q ~ ., data = df_tr) %>%
#         recipes::update_role(YYYY, new_role = "id") %>%
#         recipes::step_rm(YYYY) %>%
#         recipes::step_zv(recipes::all_predictors()) %>%
#         recipes::step_impute_median(recipes::all_predictors())
#
#       spec_sub <- model_spec(sub_fuser)
#       pred_cols <- setdiff(names(df_tr), c("YYYY", "Q"))
#       grid_sub <- model_grid(sub_fuser, p = length(pred_cols), levels = sub_grid_levels)
#
#       wf_sub <- workflows::workflow() %>%
#         workflows::add_recipe(rec_meta) %>%
#         workflows::add_model(spec_sub)
#
#       rset <- tryCatch({
#         make_rolling(
#           df_tr,
#           year_col = "YYYY",
#           init_frac = 0.80,
#           assess_frac = 0.20,
#           n_splits = min(3, nrow(df_tr) - 1),
#           cumulative = TRUE,
#           quiet = TRUE
#         )
#       }, error = function(e) NULL)
#
#       rs_sub <- NULL
#       if (!is.null(rset) && length(rset$splits) >= 1) {
#         ctrl_sub <- tune::control_grid(
#           save_pred = TRUE,
#           verbose = FALSE,
#           allow_par = allow_par,
#           parallel_over = "resamples"
#         )
#         rs_sub <- tryCatch({
#           suppressWarnings(
#             tune::tune_grid(
#               wf_sub,
#               resamples = rset,
#               grid = grid_sub,
#               metrics = yardstick::metric_set(yardstick::rmse),
#               control = ctrl_sub
#             )
#           )
#         }, error = function(e) NULL)
#       }
#
#       fit_sub <- NULL
#       if (is.null(rs_sub) || nrow(tune::collect_metrics(rs_sub)) == 0) {
#         fit_sub <- tryCatch(parsnip::fit(wf_sub, df_tr), error = function(e) NULL)
#       } else {
#         best_sub <- tune::select_best(rs_sub, metric = "rmse")
#         wf_sub_fin <- tune::finalize_workflow(wf_sub, best_sub)
#         fit_sub <- tryCatch(parsnip::fit(wf_sub_fin, df_tr), error = function(e) NULL)
#       }
#
#       if (is.null(fit_sub)) {
#         # fallback weighted fusion
#         fused <- preds_long %>%
#           dplyr::group_by(.data$YYYY) %>%
#           dplyr::summarise(
#             pred_fused = .weighted_mean_na(.data$pred, .data$w),
#             .groups = "drop"
#           )
#       } else {
#         fused <- dat %>%
#           dplyr::mutate(pred_fused = predict(fit_sub, new_data = dat)$.pred)
#       }
#     }
#   }
# }
#
# # ---------------------------
# # Post-processing: positivity and complete dates
# # ---------------------------
# if (isTRUE(target_positive)) {
#   fused <- fused %>% dplyr::mutate(pred_fused = pmax(.data$pred_fused, 0))
# }
#
# fused <- dplyr::full_join(
#   tibble::tibble(YYYY = dates_all),
#   fused,
#   by = "YYYY"
# ) %>%
#   dplyr::arrange(.data$YYYY)
#
# list(
#   fused = fused,
#   leaderboard_products = lb %>%
#     dplyr::arrange(dplyr::desc(.data$weight), dplyr::desc(.data$kge)),
#   all_results = results
# )
# }




# Consolidate top-K products for one basin and one ML model
#
# For a given basin and ML model, this function (i) fits/tunes the model
# on each product, (ii) ranks products by cross-validated KGE, (iii) keeps
# the top-K, and (iv) fuses their predictions via performance-weighted
# averaging (weights derived from KGE).
#
# @param data_by_product Named list of data frames/tibbles per product.
# @param basin_id Basin identifier value.
# @param hybas_id Name of the basin ID column (default: "HYBAS_ID").
# @param target Name of the target column (default: "Q").
# @param date_col Name of the date column (default: "YYYY").
# @param pred_pattern_by_product Optional named list of regex by product to select predictors.
# @param model One of `SUPPORTED_MODELS`.
# @param topK Integer; number of best products to fuse (default: 3).
# @param min_kge_model Minimum KGE threshold to accept fusion (default: -Inf).
# @param pretrained Optional list of pre-trained models (indexed by `model` then product).
# @param grid_levels Tuning grid granularity.
# @param min_data_required Minimum number of rows required to train.
# @param predictors_min Minimum number of predictors required.
# @param quiet Logical; if FALSE, emits informative messages (default: TRUE).
# @param verbose Logical, emit diagnostic messages.
# @param max_na_frac Numeric in \eqn{[0, 1]}: maximum allowed fraction of missing
#   values per column before stopping (default \code{0.20} = 20\%).
# @param impute Character, one of \code{"median"}, \code{"mean"}, or \code{"none"}.
#   If \code{"none"}, no imputation is performed after the guard (default \code{"median"}).
# @param require_variance Logical; if \code{TRUE}, stop when a column has zero
#   standard deviation after imputation (default \code{TRUE}).
# @param ... Passed to the underlying tuner/predictor.
#
# @return A list with elements:
#   \item{fused}{Fused predictions (or NULL if fusion is aborted).}
#   \item{leaderboard_products}{Tibble of products and their KGE.}
#   \item{all_results}{Raw list of per-product results.}
# @export
# @importFrom dplyr filter ungroup rename select all_of mutate
# @importFrom purrr map map_dbl imap_dfr compact map_chr
# @importFrom stringr str_detect
# @importFrom tibble tibble
# @importFrom glue glue
# @importFrom rlang .data sym
# wass2s_cons_mods_ml <- function(
#     data_by_product,
#     basin_id,
#     hybas_id = "HYBAS_ID",
#     target = "Q",
#     date_col = "YYYY",
#     pred_pattern_by_product = NULL,
#     model = SUPPORTED_MODELS,
#     topK = 3,
#     min_kge_model = -Inf,
#     pretrained = NULL,
#     grid_levels = 5,
#     min_data_required = 10,
#     predictors_min = 1,
#     quiet = TRUE,
#     verbose = TRUE,
#     max_na_frac =0.3,
#     impute = "median",
#     require_variance = TRUE,
#     ...
# ){
#   model <- match.arg(model, SUPPORTED_MODELS)
#   prods <- names(data_by_product)
#
#   # Validation supplémentaire des entrées
#   if (length(prods) == 0) {
#     stop("data_by_product must be a named list", call. = FALSE)
#   }
#
#   if (topK < 1) {
#     stop("topK must be at least 1", call. = FALSE)
#   }
#
#   results <- purrr::map(prods, function(p) {
#     dfp <- data_by_product[[p]] %>%
#       dplyr::filter(.data[[hybas_id]] == basin_id) %>%
#       dplyr::ungroup()
#
#     # Required columns present?
#     missing_cols <- setdiff(c(target, date_col), names(dfp))
#     if (length(missing_cols) > 0) {
#       stop(glue::glue(
#         "wass2s_cons_mods_ml(): missing required columns: {paste(missing_cols, collapse = ', ')}"
#       ), call. = FALSE)
#     }
#
#     # Minimum sample size
#     if (nrow(dfp) < min_data_required) {
#       if (!verbose) message(glue::glue(
#         "Skipping product '{p}' (basin {basin_id}): ",
#         "only {nrow(dfp)} rows available; at least {min_data_required} required."
#       ))
#       return(NULL)
#     }
#
#     # Standardize names: create 'YYYY' and 'Q'
#     dfp <- dfp %>%
#       dplyr::rename(
#         YYYY = !!rlang::sym(date_col),
#         Q    = !!rlang::sym(target)
#       )
#
#     # Product-specific predictor pattern (default '^pt_')
#     # pat <- if (!is.null(pred_pattern_by_product) && p %in% names(pred_pattern_by_product)) {
#     #   pred_pattern_by_product[[p]]
#     # } else {
#     #   "^pt_"
#     # }
#
#     pat <- if (!is.null(pred_pattern_by_product) && p %in% names(pred_pattern_by_product)) {
#       pred_pattern_by_product[[p]]
#     } else if(!is.null(pred_pattern_by_product) && length(pred_pattern_by_product) ==1) pred_pattern_by_product else "^pt_"
#
#
#     # Select predictors: all columns except id/time/target, then regex-filter
#     candidates <- setdiff(names(dfp), c(hybas_id, "YYYY", "Q"))
#     predictors <- candidates[purrr::map_lgl(candidates, ~ stringr::str_detect(.x, pat))]
#     if (length(predictors) < predictors_min) {
#       if (!verbose) message(glue::glue(
#         "Skipping product '{p}' (basin {basin_id}): ",
#         "only {length(predictors)} predictors matched pattern '{pat}'; ",
#         "at least {predictors_min} required."
#       ))
#       return(NULL)
#     }
#
#     # Pre-trained model (if provided)
#     pre_wf <- NULL
#     if (!is.null(pretrained) &&
#         !is.null(pretrained[[model]]) &&
#         !is.null(pretrained[[model]][[p]])) {
#       pre_wf <- pretrained[[model]][[p]]
#     }
#
#     if (verbose) message("[", model, "] ", p, " : ",
#                          length(predictors), " predictors using pattern '", pat, "'")
#
#     # Train + predict with error handling
#     out <- tryCatch({
#       wass2s_tune_pred_ml(
#         df_basin_product = dfp %>% dplyr::select(YYYY, Q, dplyr::all_of(predictors)),
#         predictors       = predictors,
#         model            = model,
#         grid_levels      = grid_levels,
#         pretrained_wflow = pre_wf,
#         quiet            = quiet,
#         max_na_frac =max_na_frac,
#         impute = impute,
#         require_variance =require_variance,
#         ...
#       )
#     }, error = function(e) {
#       if (!verbose) message(glue::glue("Error training product '{p}' for basin {basin_id}: {e$message}"))
#       return(NULL)
#     })
#
#     if (is.null(out)) return(NULL)
#
#     list(
#       product         = p,
#       kge             = out$kge_cv_mean,
#       preds           = out$preds,
#       leaderboard_cfg = out$leaderboard_cfg,
#       fitted_model    = out$fit
#     )
#
#   }) %>% purrr::compact()
#
#   # No usable result at all
#   if (length(results) == 0) {
#     if (!verbose) message(glue::glue(
#       "No valid models could be trained for basin {basin_id}: ",
#       "all products were skipped or failed to meet requirements."
#     ))
#     return(list(
#       fused = NULL,
#       leaderboard_products = tibble::tibble(
#         product = character(),
#         kge     = numeric()
#       ),
#       all_results = list()
#     ))
#   }
#
#   # Sort by KGE (descending)
#   ord <- order(purrr::map_dbl(results, "kge"), decreasing = TRUE)
#   results <- results[ord]
#
#   # Keep top-K
#   results_top <- utils::head(results, n = min(topK, length(results)))
#
#   kg <- purrr::map_dbl(results_top, "kge")
#   kg[kg < 0] <- 0
#
#
#   # Optional minimum KGE gate
#   if (!is.infinite(min_kge_model)) {
#     if (min_kge_model > 1) {
#       stop("min_kge_model cannot be greater than 1.", call. = FALSE)
#     }
#     kg[kg < min_kge_model] <- 0
#   }
#
#   if(all(kg==0)){
#     results_top <- results_top[1]
#     kg <- 1
#     topK <- 1
#   }
#
#   # If best KGE below threshold or all weights zero → no fusion
#   if (!is.finite(results[[1]]$kge) || results[[1]]$kge < min_kge_model || max(kg) <= 0) {
#     if (!verbose) message(glue::glue(
#       "Fusion aborted for basin {basin_id}: ",
#       "no product reached the minimum KGE threshold ({min_kge_model})."
#     ))
#     return(list(
#       fused = NULL,
#       leaderboard_products = tibble::tibble(
#         product = purrr::map_chr(results, "product"),
#         kge     = purrr::map_dbl(results, "kge")
#       ),
#       all_results = results
#     ))
#   }
#
#   # Weights = truncated KGE (>= 0)
#   w <- kg
#
#   # Stack weighted predictions (each out$preds must include YYYY, pred)
#   preds_long <- purrr::imap_dfr(
#     results_top,
#     ~ dplyr::mutate(.x$preds, product = .x$product, w = w[.y])
#   )
#
#   # Actual fusion (should aggregate by YYYY using weights 'w')
#   fused <- fuse_topk(preds_long)
#
#   list(
#     fused = fused,
#     leaderboard_products = tibble::tibble(
#       product = purrr::map_chr(results, "product"),
#       kge     = purrr::map_dbl(results, "kge")
#     ),
#     all_results = results
#   )
# }




# wass2s_cons_mods_ml_old <- function(
#     data_by_product,
#     basin_id,
#     hybas_id = "HYBAS_ID",
#     target = "Q",
#     date_col = "YYYY",
#     pred_pattern_by_product = NULL,
#     model = SUPPORTED_MODELS,
#     topK = 3,
#     min_kge_model = -Inf,
#     pretrained = NULL,
#     grid_levels = 5,
#     min_data_required = 10,
#     predictors_min = 3,
#     quiet = TRUE,
#     ...
# ){
#   model <- match.arg(model, SUPPORTED_MODELS)
#   prods <- names(data_by_product)
#
#   # Validation supplémentaire des entrées
#   if (length(prods) == 0) {
#     stop("data_by_product must be a named list", call. = FALSE)
#   }
#
#   if (topK < 1) {
#     stop("topK must be at least 1", call. = FALSE)
#   }
#
#   results <- purrr::map(prods, function(p) {
#     dfp <- data_by_product[[p]] %>%
#       dplyr::filter(.data[[hybas_id]] == basin_id) %>%
#       dplyr::ungroup()
#
#     # Required columns present?
#     missing_cols <- setdiff(c(target, date_col), names(dfp))
#     if (length(missing_cols) > 0) {
#       stop(glue::glue(
#         "wass2s_cons_mods_ml(): missing required columns: {paste(missing_cols, collapse = ', ')}"
#       ), call. = FALSE)
#     }
#
#     # Minimum sample size
#     if (nrow(dfp) < min_data_required) {
#       if (!quiet) message(glue::glue(
#         "Skipping product '{p}' (basin {basin_id}): ",
#         "only {nrow(dfp)} rows available; at least {min_data_required} required."
#       ))
#       return(NULL)
#     }
#
#     # Standardize names: create 'YYYY' and 'Q'
#     dfp <- dfp %>%
#       dplyr::rename(
#         YYYY = !!rlang::sym(date_col),
#         Q    = !!rlang::sym(target)
#       )
#
#     # Product-specific predictor pattern (default '^pt_')
#     pat <- if (!is.null(pred_pattern_by_product) && p %in% names(pred_pattern_by_product)) {
#       pred_pattern_by_product[[p]]
#     } else {
#       "^pt_"
#     }
#
#     # Select predictors: all columns except id/time/target, then regex-filter
#     candidates <- setdiff(names(dfp), c(hybas_id, "YYYY", "Q"))
#     predictors <- candidates[purrr::map_lgl(candidates, ~ stringr::str_detect(.x, pat))]
#     if (length(predictors) < predictors_min) {
#       if (!quiet) message(glue::glue(
#         "Skipping product '{p}' (basin {basin_id}): ",
#         "only {length(predictors)} predictors matched pattern '{pat}'; ",
#         "at least {predictors_min} required."
#       ))
#       return(NULL)
#     }
#
#     # Pre-trained model (if provided)
#     pre_wf <- NULL
#     if (!is.null(pretrained) &&
#         !is.null(pretrained[[model]]) &&
#         !is.null(pretrained[[model]][[p]])) {
#       pre_wf <- pretrained[[model]][[p]]
#     }
#
#     # Train + predict with error handling
#     out <- tryCatch({
#       wass2s_tune_pred_ml(
#         df_basin_product = dfp %>% dplyr::select(YYYY, Q, dplyr::all_of(predictors)),
#         predictors       = predictors,
#         model            = model,
#         grid_levels      = grid_levels,
#         pretrained_wflow = pre_wf,
#         quiet            = quiet,
#         ...
#       )
#     }, error = function(e) {
#       if (!quiet) message(glue::glue("Error training product '{p}' for basin {basin_id}: {e$message}"))
#       return(NULL)
#     })
#
#     if (is.null(out)) return(NULL)
#
#     list(
#       product         = p,
#       kge             = out$kge_cv_mean,
#       preds           = out$preds,
#       leaderboard_cfg = out$leaderboard_cfg,
#       fitted_model    = out$fit
#     )
#
#   }) %>% purrr::compact()
#
#   # No usable result at all
#   if (length(results) == 0) {
#     if (!quiet) message(glue::glue(
#       "No valid models could be trained for basin {basin_id}: ",
#       "all products were skipped or failed to meet requirements."
#     ))
#     return(list(
#       fused = NULL,
#       leaderboard_products = tibble::tibble(
#         product = character(),
#         kge     = numeric()
#       ),
#       all_results = list()
#     ))
#   }
#
#   # Sort by KGE (descending)
#   ord <- order(purrr::map_dbl(results, "kge"), decreasing = TRUE)
#   results <- results[ord]
#
#   # Keep top-K
#   results_top <- utils::head(results, n = min(topK, length(results)))
#
#   kg <- purrr::map_dbl(results_top, "kge")
#   kg[kg < 0] <- 0
#
#   # Optional minimum KGE gate
#   if (!is.infinite(min_kge_model)) {
#     if (min_kge_model > 1) {
#       stop("min_kge_model cannot be greater than 1.", call. = FALSE)
#     }
#     kg[kg < min_kge_model] <- 0
#   }
#
#   if(all(kg<=0)){
#     kg <- rep(1,min(topK, length(results)))
#   }
#
#   # If best KGE below threshold or all weights zero → no fusion
#   if (!is.finite(results[[1]]$kge) || results[[1]]$kge < min_kge_model || max(kg) <= 0) {
#     if (!quiet) message(glue::glue(
#       "Fusion aborted for basin {basin_id}: ",
#       "no product reached the minimum KGE threshold ({min_kge_model})."
#     ))
#     return(list(
#       fused = NULL,
#       leaderboard_products = tibble::tibble(
#         product = purrr::map_chr(results, "product"),
#         kge     = purrr::map_dbl(results, "kge")
#       ),
#       all_results = results
#     ))
#   }
#
#   # Weights = truncated KGE (>= 0)
#   w <- kg
#
#   # Stack weighted predictions (each out$preds must include YYYY, pred)
#   preds_long <- purrr::imap_dfr(
#     results_top,
#     ~ dplyr::mutate(.x$preds, product = .x$product, w = w[.y])
#   )
#
#   # Actual fusion (should aggregate by YYYY using weights 'w')
#   fused <- fuse_topk(preds_long)
#
#   list(
#     fused = fused,
#     leaderboard_products = tibble::tibble(
#       product = purrr::map_chr(results, "product"),
#       kge     = purrr::map_dbl(results, "kge")
#     ),
#     all_results = results
#   )
# }


























# wass2s_cons_mods_ml <- function(
#     data_by_product,
#     basin_id,
#     hybas_id = "HYBAS_ID",
#     target = "Q",
#     date_col = "YYYY",
#     pred_pattern_by_product = NULL,
#     model = SUPPORTED_MODELS,
#     topK = 3,
#     min_kge_model = -Inf,
#     pretrained = NULL,
#     grid_levels = 5,
#     min_data_required = 10,
#     predictors_min = 3,
#     quiet       = TRUE,
#     ...
# ){
#   model <- match.arg(model, SUPPORTED_MODELS)
#   prods <- names(data_by_product)
#
#   results <- purrr::map(prods, function(p) {
#     dfp <- data_by_product[[p]] %>%
#       dplyr::filter(.data[[hybas_id]] == basin_id) %>%
#       dplyr::ungroup()
#
#     # colonnes nécessaires présentes ?
#     missing_cols <- setdiff(c(target, date_col), names(dfp))
#     if (length(missing_cols) > 0) {
#       stop(glue::glue(
#         "wass2s_cons_mods_ml(): colonnes manquantes: {paste(missing_cols, collapse = ', ')}"
#       ), call. = FALSE)
#     }
#
#     # taille minimale
#       if (nrow(dfp) < min_data_required) {
#       message(glue::glue(
#         "Skipping product '{p}' (basin {basin_id}): ",
#         "only {nrow(dfp)} rows available, but at least {min_data_required} required."
#       ))
#       return(NULL)
#     }
#
#     # standardisation des noms : on crée "YYYY" et "Q"
#     # (new = old) : renommer la colonne 'date_col' en 'YYYY' et 'target' en 'Q'
#     dfp <- dfp %>%
#       dplyr::rename(
#         YYYY = !!date_col,
#         Q    = !!target
#       )
#
#     # choix du pattern par produit (sinon défaut "^pt_")
#     pat <- if (!is.null(pred_pattern_by_product) && p %in% names(pred_pattern_by_product)) {
#       pred_pattern_by_product[[p]]
#     } else {
#       "^pt_"
#     }
#
#     # sélection des prédicteurs : toutes colonnes sauf id/temps/cible,
#     # puis filtre regex
#     candidates <- setdiff(names(dfp), c(hybas_id, "YYYY", "Q"))
#     predictors <- candidates[purrr::map_lgl(candidates, ~ stringr::str_detect(.x, pat))]
#     if (length(predictors) < predictors_min) {
#       message(glue::glue(
#         "Skipping product '{p}' (basin {basin_id}): ",
#         "only {length(predictors)} predictors matched pattern '{pat}', ",
#         "but at least {predictors_min} required."
#       ))
#       return(NULL)
#     }
#
#     # récup. modèle pré-entraîné si présent
#     pre_wf <- NULL
#     if (!is.null(pretrained) && !is.null(pretrained[[model]]) && !is.null(pretrained[[model]][[p]])) {
#       pre_wf <- pretrained[[model]][[p]]
#     }
#
#     # entraînement + prédiction
#     out <- wass2s_tune_pred_ml(
#       df_basin_product = dfp %>% dplyr::select(YYYY, Q, dplyr::all_of(predictors)),
#       predictors       = predictors,
#       model            = model,
#       grid_levels      = grid_levels,
#       pretrained_wflow = pre_wf,
#       quiet =quiet,
#       ...
#     )
#
#     list(
#       product         = p,
#       kge             = out$kge_cv_mean,
#       preds           = out$preds,
#       leaderboard_cfg = out$leaderboard_cfg,
#       fitted_model    = out$fit
#     )
#   }) %>% purrr::compact()
#
#   # aucun resultat exploitable
#
#   # no usable result at all
#   if (length(results) == 0) {
#     message(glue::glue(
#       "No valid models could be trained for basin {basin_id}: ",
#       "all products were skipped or failed to meet requirements."
#     ))
#     return(list(
#       fused = NULL,
#       leaderboard_products = tibble::tibble(product = character(),
#                                             kge = numeric())
#     ))
#   }
#
#   # tri par KGE decroissant
#   ord <- order(purrr::map_dbl(results, "kge"), decreasing = TRUE)
#   results <- results[ord]
#
#   # topK
#   results_top <- head(results, n = min(topK, length(results)))
#
#   kg <- purrr::map_dbl(results_top, "kge")
#   kg[kg < 0] <- 0
#
#   # controle optionnel sur min_kge_model
#   if (!is.infinite(min_kge_model)) {
#     if (min_kge_model > 1) {
#       stop("min_kge_model can't be greater than 1.", call. = FALSE)
#     }
#     # remet a zero tout produit dont le KGE est en-dessous du seuil
#     kg[kg < min_kge_model] <- 0
#   }
#
#   # garde-fous : KGE trop faible pas de fusion
#   if (!is.finite(results[[1]]$kge) || results[[1]]$kge < min_kge_model || max(kg) <= 0) {
#     message(glue::glue(
#       "Fusion aborted for basin {basin_id}: ",
#       "no product reached the minimum KGE threshold ({min_kge_model})."
#     ))
#     return(list(
#       fused = NULL,
#       leaderboard_products = tibble::tibble(
#         product = purrr::map_chr(results, "product"),
#         kge     = purrr::map_dbl(results, "kge")
#       )
#     ))
#   }
#
#   # poids = KGE tronqué à [0, +inf)
#   w <- kg
#
#   # empilement des prédictions pondérées (chaque out$preds doit contenir YYYY, pred)
#   preds_long <- purrr::imap_dfr(results_top, ~ dplyr::mutate(.x$preds, product = .x$product, w = w[.y]))
#
#   # fusion proprement dite (doit agréger par YYYY avec les poids 'w')
#   fused <- fuse_topk(preds_long)
#
#   list(
#     fused = fused,
#     leaderboard_products = tibble::tibble(
#       product = purrr::map_chr(results, "product"),
#       kge     = purrr::map_dbl(results, "kge")
#     ),
#     all_results = results
#   )
# }


# wass2s_cons_mods_ml <- function(
#     data_by_product,
#     basin_id,
#     hybas_id = "HYBAS_ID",
#     target = "Q",
#     date_col = "YYYY"
#     pred_pattern_by_product = NULL,
#     model = SUPPORTED_MODELS,
#     topK = 3,
#     min_kge_model = -Inf,
#     pretrained = NULL,
#     grid_levels = 5,
#     min_data_required = 10,
#     predictors_min = 3,
#     ...
# ){
#   model <- match.arg(model, SUPPORTED_MODELS)
#   prods <- names(data_by_product)
#
#   results <- purrr::map(prods, function(p){
#     dfp <- data_by_product[[p]] |> dplyr::filter(.data[[hybas_id]] == basin_id) |> dplyr::ungroup()
#     missing_cols <- setdiff(c(target, date_col), names(dfp))
#     if (length(missing_cols) > 0) {
#       stop(glue::glue(
#         "wass2s_tune_pred_ml(): missing column(s): {paste(missing_cols, collapse = ', ')}"
#       ), call. = FALSE)
#     }
#
#     if (nrow(dfp) < min_data_required) return(NULL)
#     dfp <- dfp %>%
#       dplyr::rename(
#         !!date_col := "YYYY",
#         !!target   := "Q"
#       )
#
#     pat <- if(!is.null(pred_pattern_by_product) && p %in% names(pred_pattern_by_product)) pred_pattern_by_product[[p]] else "^pt_"
#     predictors <- names(dfp) |> setdiff(c(hybas_id,"YYYY","Q")) |> purrr::keep(~ stringr::str_detect(.x, pat))
#     if (length(predictors) < predictors_min) return(NULL)
#
#     pre_wf <- NULL
#     if (!is.null(pretrained) && !is.null(pretrained[[model]]) && !is.null(pretrained[[model]][[p]])) {
#       pre_wf <- pretrained[[model]][[p]]
#     }
#
#     out <- wass2s_tune_pred_ml(
#       df_basin_product = dfp |> select(YYYY, Q, all_of(predictors)),
#       predictors = predictors,
#       model = model,
#       grid_levels = grid_levels,
#       pretrained_wflow = pre_wf,)
#     list(product = p,
#          kge = out$kge_cv_mean,
#          preds = out$preds,
#          leaderboard_cfg = out$leaderboard_cfg,
#          fitted_model = out$fit)
#   }) |> purrr::compact()
#
#   if (length(results) == 0) return(list(fused = NULL, leaderboard_products = tibble(product=character(), kge=numeric())))
#
#   # tri + topK
#   ord <- order(purrr::map_dbl(results, "kge"), decreasing = TRUE)
#   results <- results[ord]
#
#   results_top <- head(results, n = min(topK, length(results)))
#
#   kg <- purrr::map_dbl(results_top, "kge")
#   #w <- weight_from_kge(kg)
#   kg[kg<0] <- 0
#
#   if (!is.finite(results[[1]]$kge) || results[[1]]$kge < min_kge_model || max(kg)<= 0 ) {
#     # meilleur produit trop faible → on écarte ce modèle pour ce bassin
#     return(list(fused = NULL, leaderboard_products = tibble(product = purrr::map_chr(results,"product"), kge = map_dbl(results,"kge"))))
#   }
#   w <- kg
#   preds_long <- purrr::imap_dfr(results_top, ~ dplyr::mutate(.x$preds, product = .x$product, w = w[.y]))
#
#   fused <- fuse_topk(preds_long)
#
#   list(
#     fused = fused,
#     leaderboard_products = tibble(product = purrr::map_chr(results, "product"), kge = purrr::map_dbl(results, "kge")),
#     all_results= results
#   )
# }
