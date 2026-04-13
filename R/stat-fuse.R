# ---------------------------
# Helpers
# ---------------------------
# .empty_return <- function(dates_all, lb = NULL, results = list()) {
#   if (is.null(lb)) {
#     lb <- tibble::tibble(
#       product = character(), kge = numeric(), rsq = numeric(),
#       n_pred = integer(), sd_pred = numeric(), weight = numeric()
#     )
#   } else {
#     if (!"weight" %in% names(lb)) lb$weight <- 0
#   }
#   list(
#     fused = tibble::tibble(YYYY = dates_all, pred_fused = NA_real_),
#     leaderboard_products = lb %>%
#       dplyr::arrange(dplyr::desc(.data$weight), dplyr::desc(.data$kge)),
#     all_results = results
#   )
# }
#
# .get_pattern <- function(p) {
#   if (!is.null(pred_pattern_by_product) && p %in% names(pred_pattern_by_product)) {
#     pred_pattern_by_product[[p]]
#   } else if (!is.null(pred_pattern_by_product) && length(pred_pattern_by_product) == 1) {
#     pred_pattern_by_product
#   } else {
#     "^pt_"
#   }
# }
#
# .weighted_mean_na <- function(pred, w) {
#   ok <- is.finite(pred) & is.finite(w) & !is.na(pred) & !is.na(w)
#   if (!any(ok)) return(NA_real_)
#   ww <- w[ok]
#   pp <- pred[ok]
#   if (sum(ww) <= 0) return(NA_real_)
#   sum(ww * pp, na.rm = TRUE) / sum(ww, na.rm = TRUE)
# }

#' Consolidate predictions across products for one statistical model
#'
#' For a given basin and a fixed statistical model (PCR/Ridge/Lasso), this
#' function:
#' \enumerate{
#'   \item fits/tunes one model per product using \code{wass2s_tune_pred_stat()},
#'   \item ranks products by cross-validated KGE,
#'   \item keeps the top-\code{K} products,
#'   \item fuses their predictions either by a robust KGE-weighted mean
#'         (default) or by an optional meta-learner ("sub-fuser") trained on the
#'         product predictions (when observations are available).
#' }
#'
#' Fusion weights are derived from KGE (negative KGE truncated to 0) and
#' normalized to sum to 1. Products with \code{kge < min_kge_model} receive a
#' weight of 0 (and fusion falls back gracefully if all weights become 0).
#'
#' The function is defensive: it contains multiple safeguards and fallbacks
#' (e.g., missing predictors, insufficient data, failed tuning), and will return
#' a well-formed output with \code{NA} predictions rather than failing silently.
#'
#' @param basin_id Basin identifier.
#' @param data_by_product Named list of data frames (one per product). Each data
#'   frame must contain at least \code{basin_col}, \code{YYYY} (dates), \code{Q}
#'   (target), and predictor columns (typically prefixed, e.g. \code{pt_*}).
#' @param basin_col Column name for basin IDs (default: \code{"HYBAS_ID"}).
#' @param pred_pattern_by_product Optional. Either:
#'   \itemize{
#'     \item a named character vector/list mapping \code{product -> regex} used to
#'       select predictors, or
#'     \item a single regex applied to all products.
#'   }
#'   If \code{NULL}, defaults to \code{"^pt_"}.
#' @param model One of \code{"pcr"}, \code{"ridge"}, \code{"lasso"}.
#' @param grid Optional tibble of tuning parameters passed to
#'   \code{wass2s_tune_pred_stat()}. If \code{NULL}, defaults are used.
#' @param use_sub_fuser Logical; if \code{TRUE}, attempts a meta-learner fusion on
#'   product predictions (requires observed \code{Q} in the merged data).
#'   If \code{FALSE} (or if prerequisites are not met), uses weighted fusion.
#' @param sub_fuser Character; meta-learner model name passed to
#'   \code{model_spec()} (e.g. \code{"rf"}). Only used when
#'   \code{use_sub_fuser = TRUE}.
#' @param sub_grid_levels Integer; tuning grid "levels" for the sub-fuser, passed
#'   to \code{model_grid()}.
#' @param topK Integer, number of products to keep for fusion (default: 3).
#' @param min_kge_model Numeric; minimum KGE threshold for products to receive
#'   non-zero fusion weight (default: 0.2).
#' @param min_predictors Integer; minimum number of predictors required to fit a
#'   product model (default: 1).
#' @param min_data_required Integer; minimum number of rows required to train a
#'   product model and (when enabled) the sub-fuser (default: 10).
#' @param prediction_years Optional numeric vector of length 2 giving the start
#'   and end years for a holdout prediction period. These years are excluded
#'   from training inside \code{wass2s_tune_pred_stat()} and predictions are
#'   generated on the concatenated (train + holdout) timeline after fitting.
#' @param target_positive Logical; if \code{TRUE}, force negative fused
#'   predictions to zero.
#' @param resamples Optional \code{rsample::rset} object for resampling passed to
#'   \code{wass2s_tune_pred_stat()}. If \code{NULL}, rolling-origin resampling is
#'   created via \code{make_rolling()} inside \code{wass2s_tune_pred_stat()}.
#' @param pretrained_wflow Optional \code{workflows::workflow} object passed to
#'   \code{wass2s_tune_pred_stat()}. If supplied, tuning is skipped and the
#'   workflow is used for prediction directly.
#' @param init_frac Fraction of rows used for the initial training window passed
#'   to \code{wass2s_tune_pred_stat()}.
#' @param assess_frac Fraction of rows used for the assessment window passed to
#'   \code{wass2s_tune_pred_stat()}.
#' @param n_splits Optional integer, desired number of resamples (splits) passed
#'   to \code{wass2s_tune_pred_stat()}.
#' @param cumulative Logical; passed to \code{make_rolling()} (rolling-origin
#'   resampling).
#' @param quiet Logical; if \code{FALSE}, emits informative messages (passed to
#'   underlying tuning/prediction calls).
#' @param verbose Logical; if \code{TRUE}, emits diagnostic messages for product
#'   processing and failures (default: \code{TRUE}).
#' @param allow_par Logical. If \code{TRUE}, allow parallel execution during
#'   hyperparameter tuning of the meta-learner.
#' @param seed Integer; random seed for reproducibility.
#' @param max_na_frac Numeric in \eqn{[0, 1]}; maximum allowed fraction of
#'   missing values per guarded column before stopping (default: 0.3).
#' @param impute Character; one of \code{"median"}, \code{"mean"}, or
#'   \code{"none"}. If \code{"none"}, no imputation is performed after the
#'   missingness guard (default: \code{"median"}).
#' @param require_variance Logical; if \code{TRUE}, stop when a guarded column has
#'   zero standard deviation after imputation (default: \code{TRUE}).
#' @param ... Additional arguments forwarded to \code{wass2s_tune_pred_stat()} and
#'   (when enabled) to tuning controls used internally.
#'
#' @return A list with:
#' \itemize{
#'   \item \code{fused} (tibble): at minimum \code{YYYY} and \code{pred_fused}.
#'     If observed \code{Q} is available on the fused timeline, it may also be
#'     present (depending on upstream outputs).
#'   \item \code{leaderboard_products} (tibble): per-product diagnostics including
#'     \code{product}, \code{kge}, \code{rsq}, \code{n_pred}, \code{sd_pred}, and
#'     \code{weight} (final fusion weight; sums to 1 over selected products).
#'   \item \code{all_results} (list): all individual per-product results as returned
#'     by \code{wass2s_tune_pred_stat()} (or \code{NULL}/empty entries for skipped
#'     products are omitted).
#' }
#'
#' @examples
#' \dontrun{
#' # Simple weighted fusion (recommended baseline)
#' cns <- wass2s_cons_mods_stat(
#'   basin_id = 1040021500,
#'   data_by_product = data_by_product,
#'   model = "pcr",
#'   topK = 3,
#'   use_sub_fuser = FALSE
#' )
#'
#' # Meta-learner fusion (requires Q available in the merged timeline)
#' cns2 <- wass2s_cons_mods_stat(
#'   basin_id = 1040021500,
#'   data_by_product = data_by_product,
#'   model = "ridge",
#'   topK = 5,
#'   use_sub_fuser = TRUE,
#'   sub_fuser = "rf",
#'   sub_grid_levels = 10
#' )
#' }
#' @export
wass2s_cons_mods_stat <- function(
    basin_id,
    data_by_product,
    basin_col = "HYBAS_ID",
    pred_pattern_by_product = NULL,
    model = c("pcr", "ridge", "lasso"),
    grid = NULL,

    # --- fusion options ---
    use_sub_fuser = TRUE,
    sub_fuser = "rf",
    sub_grid_levels = 10,

    # --- selection options ---
    topK = 3,
    min_kge_model = 0.2,

    # --- modeling guards ---
    min_predictors = 1,
    min_data_required = 10,
    prediction_years = NULL,
    target_positive = TRUE,

    # --- resampling / workflow ---
    resamples = NULL,
    pretrained_wflow = NULL,
    init_frac = 0.80,
    assess_frac = 0.20,
    n_splits = NULL,
    cumulative = TRUE,
    quiet = TRUE,
    verbose = TRUE,
    allow_par=TRUE,
    seed = 123,

    # --- data quality guards ---
    max_na_frac = 0.3,
    impute = "median",
    require_variance = TRUE,
    ...
) {
  set.seed(seed)
  model <- match.arg(model)

  # ---------------------------
  # Input checks
  # ---------------------------
  prods <- names(data_by_product)

  if (length(prods) == 0) {
    stop("wass2s_cons_mods_stat(): data_by_product must be a named list.", call. = FALSE)
  }
  if (topK < 1) {
    stop("wass2s_cons_mods_stat(): topK must be at least 1.", call. = FALSE)
  }
  if (!is.numeric(min_kge_model) || length(min_kge_model) != 1) {
    stop("wass2s_cons_mods_stat(): min_kge_model must be a single numeric value.", call. = FALSE)
  }

  # prediction_years: allow YYYY or YYYYMMDD, but must be numeric length 2
  if (!is.null(prediction_years)) {
    if (!is.numeric(prediction_years) || length(prediction_years) != 2 || anyNA(prediction_years)) {
      stop(
        "wass2s_cons_mods_stat(): prediction_years must be a numeric vector of length 2 (YYYY or YYYYMMDD).",
        call. = FALSE
      )
    }
  }

  # ---------------------------
  # Collect all dates for this basin across products (YYYYMMDD)
  # ---------------------------
  dates_all <- sort(unique(unlist(lapply(prods, function(p) {
    dfp <- data_by_product[[p]]
    if (!is.data.frame(dfp)) return(integer())
    if (!basin_col %in% names(dfp)) return(integer())

    dfp <- dplyr::filter(dfp, .data[[basin_col]] == basin_id)
    if (!"YYYY" %in% names(dfp)) return(integer())

    .ensure_yyyymmdd(dfp$YYYY)
  }))))

  if (length(dates_all) == 0) {
    # No data for this basin in any product
    return(list(
      fused = tibble::tibble(YYYY = integer(), pred_fused = numeric()),
      leaderboard_products = tibble::tibble(),
      all_results = list()
    ))
  }

  # ---------------------------
  # 1) Train/predict per product -> build standardized results
  # ---------------------------
  results_std <- purrr::map(prods, function(p) {

    dfp <- data_by_product[[p]]
    if (!is.data.frame(dfp)) {
      if (verbose) message("[", model, "] ", p, " : skipped (not a data.frame).")
      return(NULL)
    }
    if (!basin_col %in% names(dfp)) {
      if (verbose) message("[", model, "] ", p, " : skipped (missing basin_col='", basin_col, "').")
      return(NULL)
    }

    dfp <- dfp |>
      dplyr::filter(.data[[basin_col]] == basin_id) |>
      dplyr::ungroup()

    if (!all(c("YYYY", "Q") %in% names(dfp))) {
      if (verbose) message("[", model, "] ", p, " : skipped (missing YYYY/Q).")
      return(NULL)
    }

    # Ensure YYYYMMDD int + stable ordering
    dfp <- dfp |>
      dplyr::mutate(YYYY = .ensure_yyyymmdd(.data$YYYY)) |>
      dplyr::arrange(.data$YYYY)

    # Predictor pattern
    pat <- if (exists(".get_pattern", mode = "function")) {
      .get_pattern(p, pred_pattern_by_product = pred_pattern_by_product)
    } else {
      if (!is.null(pred_pattern_by_product) && p %in% names(pred_pattern_by_product)) {
        pred_pattern_by_product[[p]]
      } else if (!is.null(pred_pattern_by_product) && length(pred_pattern_by_product) == 1) {
        pred_pattern_by_product
      } else {
        "^pt_"
      }
    }

    predictors <- select_predictors(
      dfp,
      pattern = pat,
      exclude = c(basin_col, "YYYY", "Q")
    )

    if (verbose) {
      message("[", model, "] ", p, " : ", length(predictors),
              " predictors using pattern '", pat, "'")
    }

    if (length(predictors) < min_predictors) {
      return(list(
        product = p,
        score   = NA_real_,
        rsq     = NA_real_,
        preds   = NULL,
        n_pred  = length(predictors),
        sd_pred = NA_real_
      ))
    }

    # Sanitize target Q
    dfp <- .sanitize_numeric_columns(
      df   = dfp,
      cols = "Q",
      max_na_frac = max_na_frac,
      impute = impute,
      require_variance = require_variance
    )

    out <- tryCatch({
      wass2s_tune_pred_stat(
        df_basin_product = dplyr::select(dfp, YYYY, Q, tidyselect::all_of(predictors)),
        predictors       = predictors,
        target           = "Q",
        date_col         = "YYYY",
        model            = model,
        prediction_years = prediction_years,
        target_positive  = target_positive,
        resamples        = resamples,
        pretrained_wflow = pretrained_wflow,
        grid             = grid,
        init_frac        = init_frac,
        assess_frac      = assess_frac,
        n_splits         = n_splits,
        cumulative       = cumulative,
        quiet            = quiet,
        allow_par        = allow_par,
        verbose_tune     = TRUE,
        max_na_frac      = max_na_frac,
        impute           = impute,
        require_variance = require_variance,
        ...
      )
    }, error = function(e) {
      if (verbose) message(glue::glue("Error training product '{p}' for basin {basin_id}: {e$message}"))
      NULL
    })

    if (is.null(out) || !is.list(out) || is.null(out$preds)) {
      return(list(
        product = p,
        score   = NA_real_,
        rsq     = NA_real_,
        preds   = NULL,
        n_pred  = length(predictors),
        sd_pred = NA_real_
      ))
    }

    # preds must contain YYYY and pred
    if (!all(c("YYYY", "pred") %in% names(out$preds))) {
      if (verbose) message("[", model, "] ", p, " : skipped (preds missing YYYY/pred).")
      return(list(
        product = p,
        score   = NA_real_,
        rsq     = NA_real_,
        preds   = NULL,
        n_pred  = length(predictors),
        sd_pred = NA_real_
      ))
    }

    # Standardize preds
    preds <- out$preds |>
      dplyr::mutate(YYYY = .ensure_yyyymmdd(.data$YYYY)) |>
      dplyr::arrange(.data$YYYY)

    list(
      product = p,
      score   = out$kge_cv_mean,     # standardized key for fusion util
      rsq     = out$rsq_cv_mean,
      preds   = preds,
      n_pred  = length(predictors),
      sd_pred = stats::sd(preds$pred, na.rm = TRUE)
    )
  }) |>
    purrr::compact()

  # If no usable results, return empty fused on full timeline
  if (length(results_std) == 0) {
    return(list(
      fused = tibble::tibble(YYYY = dates_all, pred_fused = NA_real_),
      leaderboard_products = tibble::tibble(),
      all_results = list()
    ))
  }

  # ---------------------------
  # 2) Delegate product fusion to util
  # ---------------------------
  fusion <- fuse_products_predictions(
    results           = results_std,
    dates_all          = dates_all,
    topK               = topK,
    min_score          = min_kge_model,
    prediction_years   = prediction_years,
    use_sub_fuser      = use_sub_fuser,
    sub_fuser          = sub_fuser,
    sub_grid_levels    = sub_grid_levels,
    min_data_required  = min_data_required,
    target_positive    = target_positive,
    quiet              = quiet,
    verbose            = verbose,
    seed               = seed
  )

  # ---------------------------
  # 3) Keep backward-compatible structure
  # ---------------------------
  # fuse_products_predictions returns:
  # - fused: (YYYY, pred_fused, maybe Q)
  # - leaderboard_products: (product, score, weight, ...)
  # - all_results: original results list

  # If your package expects "kge" column name, map it back:
  if (nrow(fusion$leaderboard_products) > 0 && "score" %in% names(fusion$leaderboard_products)) {
    fusion$leaderboard_products <- fusion$leaderboard_products |>
      dplyr::rename(kge = .data$score)
  }

  fusion$all_results <- results_std

  fusion
}

# wass2s_cons_mods_stat <- function(
#     basin_id,
#     data_by_product,
#     basin_col = "HYBAS_ID",
#     pred_pattern_by_product = NULL,
#     model = c("pcr", "ridge", "lasso"),
#     grid = NULL,
#     use_sub_fuser = TRUE,
#     sub_fuser = "rf",
#     sub_grid_levels = 10,
#     topK = 1,
#     min_kge_model = 0.2,
#     min_predictors = 1,
#     min_data_required = 10,
#     prediction_years = NULL,
#     target_positive = TRUE,
#     resamples = NULL,
#     pretrained_wflow = NULL,
#     init_frac = 0.80,
#     assess_frac = 0.20,
#     n_splits = NULL,
#     cumulative = TRUE,
#     quiet = TRUE,
#     verbose = TRUE,
#     seed = 123,
#     max_na_frac = 0.3,
#     impute = "median",
#     require_variance = TRUE,
#     ...
# ) {
#   set.seed(seed)
#   model <- match.arg(model)
#
#   # ---------------------------
#   # Input checks
#   # ---------------------------
#   prods <- names(data_by_product)
#   if (length(prods) == 0) stop("data_by_product must be a named list.", call. = FALSE)
#   if (topK < 1) stop("topK must be at least 1.", call. = FALSE)
#
#   if (!is.null(prediction_years)) {
#     if (length(prediction_years) != 2) {
#       stop("prediction_years must be length 2 (start, end).", call. = FALSE)
#     }
#     prediction_years <- sort(prediction_years)
#   }
#
#   if (!is.numeric(min_kge_model) || length(min_kge_model) != 1) {
#     stop("min_kge_model must be a single numeric value.", call. = FALSE)
#   }
#
#   # ---------------------------
#   # Collect all dates available for this basin across products
#   # ---------------------------
#   dates_all <- sort(unique(unlist(lapply(prods, function(p) {
#     dfp <- data_by_product[[p]]
#     if (!is.data.frame(dfp)) return(integer())
#     if (!basin_col %in% names(dfp)) return(integer())
#     dfp <- dplyr::filter(dfp, .data[[basin_col]] == basin_id)
#     if (!"YYYY" %in% names(dfp)) return(integer())
#     .ensure_yyyymmdd(dfp$YYYY)
#   }))))
#
#   if (length(dates_all) == 0) {
#     # No data at all for this basin
#     return(.empty_return(integer(), results = list()))
#   }
#
#   # ---------------------------
#   # 1) Train/predict per product
#   # ---------------------------
#   results <- purrr::map(prods, function(p) {
#
#     dfp <- data_by_product[[p]]
#     if (!is.data.frame(dfp)) {
#       if (verbose) message("[", model, "] ", p, " : skipped (not a data.frame).")
#       return(NULL)
#     }
#     if (!basin_col %in% names(dfp)) {
#       if (verbose) message("[", model, "] ", p, " : skipped (missing basin_col='", basin_col, "').")
#       return(NULL)
#     }
#
#     dfp <- dfp %>%
#       dplyr::filter(.data[[basin_col]] == basin_id) %>%
#       dplyr::ungroup()
#
#     if (!all(c("YYYY", "Q") %in% names(dfp))) {
#       if (verbose) message("[", model, "] ", p, " : skipped (missing YYYY/Q).")
#       return(NULL)
#     }
#
#     # Ensure YYYYMMDD integer and sort
#     dfp <- dfp %>%
#       dplyr::mutate(YYYY = .ensure_yyyymmdd(.data$YYYY)) %>%
#       dplyr::arrange(.data$YYYY)
#
#     pat <- .get_pattern(p,pred_pattern_by_product=pred_pattern_by_product)
#
#     predictors <- select_predictors(
#       dfp,
#       pattern = pat,
#       exclude = c(basin_col, "YYYY", "Q")
#     )
#
#     if (verbose) {
#       message("[", model, "] ", p, " : ", length(predictors),
#               " predictors using pattern '", pat, "'")
#     }
#
#     if (length(predictors) < min_predictors) {
#       return(list(
#         product = p,
#         kge = NA_real_,
#         rsq = NA_real_,
#         preds = NULL,
#         n_pred = length(predictors),
#         sd_pred = NA_real_
#       ))
#     }
#
#     # Sanitize Q
#     dfp <- .sanitize_numeric_columns(
#       df = dfp,
#       cols = "Q",
#       max_na_frac = max_na_frac,
#       impute = impute,
#       require_variance = require_variance
#     )
#
#     out <- tryCatch({
#       wass2s_tune_pred_stat(
#         df_basin_product = dplyr::select(dfp, YYYY, Q, tidyselect::all_of(predictors)),
#         predictors = predictors,
#         model = model,
#         prediction_years = prediction_years,
#         target_positive = target_positive,
#         resamples = resamples,
#         pretrained_wflow = pretrained_wflow,
#         grid = grid,
#         init_frac = init_frac,
#         assess_frac = assess_frac,
#         n_splits = n_splits,
#         cumulative = cumulative,
#         quiet = quiet,
#         max_na_frac = max_na_frac,
#         impute = impute,
#         require_variance = require_variance,
#         ...
#       )
#     }, error = function(e) {
#       if (verbose) message(glue::glue("Error training product '{p}' for basin {basin_id}: {e$message}"))
#       NULL
#     })
#
#     if (is.null(out) || !is.list(out) || !"preds" %in% names(out) || is.null(out$preds)) {
#       return(list(
#         product = p,
#         kge = NA_real_,
#         rsq = NA_real_,
#         preds = NULL,
#         n_pred = length(predictors),
#         sd_pred = NA_real_
#       ))
#     }
#
#     if (!all(c("YYYY", "pred") %in% names(out$preds))) {
#       if (verbose) message("[", model, "] ", p, " : skipped (preds missing YYYY/pred).")
#       return(list(
#         product = p,
#         kge = NA_real_,
#         rsq = NA_real_,
#         preds = NULL,
#         n_pred = length(predictors),
#         sd_pred = NA_real_
#       ))
#     }
#
#     sd_pred <- stats::sd(out$preds$pred, na.rm = TRUE)
#
#     list(
#       product = p,
#       kge = out$kge_cv_mean,
#       rsq = out$rsq_cv_mean,
#       preds = out$preds,          # expects YYYY, pred (+ maybe Q)
#       n_pred = length(predictors),
#       sd_pred = sd_pred
#     )
#   }) %>% purrr::compact()
#
#   if (length(results) == 0) {
#     return(.empty_return(dates_all, results = list()))
#   }
#
#   # Keep only products with usable preds
#   results <- purrr::keep(results, ~ !is.null(.x$preds) && nrow(.x$preds) > 0)
#
#   if (length(results) == 0) {
#     return(.empty_return(dates_all, results = list()))
#   }
#
#   # Leaderboard
#   lb <- tibble::tibble(
#     product = purrr::map_chr(results, "product"),
#     kge     = purrr::map_dbl(results, "kge"),
#     rsq     = purrr::map_dbl(results, "rsq"),
#     n_pred  = purrr::map_int(results, "n_pred"),
#     sd_pred = purrr::map_dbl(results, "sd_pred"),
#     weight  = 0
#   )
#
#   # ---------------------------
#   # 2) Select topK and weights
#   # ---------------------------
#   lb_ok <- dplyr::filter(lb, is.finite(.data$kge))
#
#   if (nrow(lb_ok) > 0) {
#     lb_ok <- dplyr::arrange(lb_ok, dplyr::desc(.data$kge))
#     keep_names <- head(lb_ok$product, n = min(topK, nrow(lb_ok)))
#
#     results_top <- results[match(keep_names, purrr::map_chr(results, "product"))]
#
#     # KGE-based weights (robust)
#     kge_keep <- lb_ok$kge[match(keep_names, lb_ok$product)]
#     w <- pmax(kge_keep, 0)
#     w[kge_keep < min_kge_model] <- 0
#
#     if (all(w == 0)) {
#       lb$weight <- 0
#       return(.empty_return(dates_all, leaderboard = lb, results = results))
#     }
#     w <- w / sum(w)
#
#   } else {
#     # Fallback: keep non-constant predictions
#     non_const <- dplyr::filter(lb, is.finite(.data$sd_pred) & .data$sd_pred > 0)
#     if (nrow(non_const) == 0) {
#       lb$weight <- 0
#       return(.empty_return(dates_all, leaderboard = lb, results = results))
#     }
#     keep_names <- head(non_const$product, n = min(topK, nrow(non_const)))
#     results_top <- results[match(keep_names, purrr::map_chr(results, "product"))]
#     w <- rep(1 / length(keep_names), length(keep_names))
#   }
#
#   # Store real weights in leaderboard
#   lb$weight <- 0
#   lb$weight[match(keep_names, lb$product)] <- w
#
#   # ---------------------------
#   # 3) Fusion inputs
#   # ---------------------------
#   preds_long <- purrr::map2_dfr(results_top, seq_along(results_top), ~{
#     dplyr::transmute(
#       .x$preds,
#       YYYY = .ensure_yyyymmdd(.data$YYYY),
#       pred = .data$pred,
#       product = .x$product,
#       w = w[.y]
#     )
#   })
#
#   # Observed Q timeline (if available inside preds; else Q = NA)
#   obs <- purrr::map_dfr(results_top, ~{
#     if ("Q" %in% names(.x$preds)) {
#       dplyr::transmute(
#         .x$preds,
#         YYYY = .ensure_yyyymmdd(.data$YYYY),
#         Q = .data$Q
#       )
#     } else {
#       dplyr::transmute(
#         .x$preds,
#         YYYY = .ensure_yyyymmdd(.data$YYYY)
#       ) %>%
#         dplyr::mutate(Q = NA_real_)
#     }
#   }) %>%
#     dplyr::distinct(.data$YYYY, .keep_all = TRUE)
#
#   # ---------------------------
#   # 4) Simple fusion or sub-fuser
#   # ---------------------------
#   if (!isTRUE(use_sub_fuser) || length(results_top) < 2) {
#
#     # Weighted mean by date (robust renormalization)
#     fused <- preds_long %>%
#       dplyr::group_by(.data$YYYY) %>%
#       dplyr::summarise(
#         pred_fused = .weighted_mean_na(.data$pred, .data$w),
#         .groups = "drop"
#       ) %>%
#       dplyr::full_join(obs, by = "YYYY")
#
#   } else {
#
#     # If no observed Q, fallback to weighted fusion
#     if (!("Q" %in% names(obs)) || all(is.na(obs$Q))) {
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
#       # Wide predictors: each product is a column
#       lst_wide <- purrr::map(results_top, ~{
#         dplyr::transmute(
#           .x$preds,
#           YYYY = .ensure_yyyymmdd(.data$YYYY),
#           pred = .data$pred
#         ) %>%
#           dplyr::rename(!!.x$product := .data$pred)
#       })
#
#       prods_wide <- Reduce(function(a, b) dplyr::full_join(a, b, by = "YYYY"), lst_wide)
#
#       dat <- dplyr::left_join(obs, prods_wide, by = "YYYY") %>%
#         dplyr::arrange(.data$YYYY)
#
#       # Train set excluding holdout years
#       if (!is.null(prediction_years)) {
#         start_bound <- as.integer(paste0(prediction_years[1], "0101"))
#         end_bound   <- as.integer(paste0(prediction_years[2], "1231"))
#         df_tr <- dplyr::filter(dat, !(.data$YYYY >= start_bound & .data$YYYY <= end_bound))
#       } else {
#         df_tr <- dat
#       }
#
#       # If too few rows to train, fallback to weighted fusion
#       if (nrow(df_tr) < min_data_required) {
#
#         fused <- preds_long %>%
#           dplyr::group_by(.data$YYYY) %>%
#           dplyr::summarise(
#             pred_fused = .weighted_mean_na(.data$pred, .data$w),
#             .groups = "drop"
#           )
#
#       } else {
#
#         # Meta learner recipe: Q ~ product preds, YYYY as id
#         rec_meta <- recipes::recipe(Q ~ ., data = df_tr) %>%
#           recipes::update_role(YYYY, new_role = "id") %>%
#           recipes::step_rm(YYYY) %>%
#           recipes::step_zv(recipes::all_predictors()) %>%
#           recipes::step_impute_median(recipes::all_predictors()) %>%
#           recipes::step_normalize(recipes::all_predictors())
#
#         spec_sub <- model_spec(sub_fuser)
#         pred_cols <- setdiff(names(df_tr), c("YYYY", "Q"))
#         grid_sub <- model_grid(sub_fuser, p = length(pred_cols), levels = sub_grid_levels)
#
#         wf_sub <- workflows::workflow() %>%
#           workflows::add_recipe(rec_meta) %>%
#           workflows::add_model(spec_sub)
#
#         rset <- tryCatch({
#           make_rolling(
#             df_tr,
#             year_col = "YYYY",
#             init_frac = 0.80,
#             assess_frac = 0.20,
#             n_splits = min(3, nrow(df_tr) - 1),
#             cumulative = TRUE,
#             quiet = TRUE
#           )
#         }, error = function(e) NULL)
#
#         rs_sub <- NULL
#         if (!is.null(rset) && length(rset$splits) >= 1) {
#           ctrl_sub <- tune::control_grid(
#             save_pred = TRUE,
#             verbose = FALSE,
#             allow_par = TRUE,
#             parallel_over = "resamples"
#           )
#           rs_sub <- tryCatch({
#             suppressWarnings(
#               tune::tune_grid(
#                 wf_sub,
#                 resamples = rset,
#                 grid = grid_sub,
#                 metrics = yardstick::metric_set(yardstick::rmse),
#                 control = ctrl_sub
#               )
#             )
#           }, error = function(e) NULL)
#         }
#
#         fit_sub <- NULL
#         if (is.null(rs_sub) || nrow(tune::collect_metrics(rs_sub)) == 0) {
#           fit_sub <- tryCatch(parsnip::fit(wf_sub, df_tr), error = function(e) NULL)
#         } else {
#           best_sub <- tune::select_best(rs_sub, metric = "rmse")
#           wf_sub_fin <- tune::finalize_workflow(wf_sub, best_sub)
#           fit_sub <- tryCatch(parsnip::fit(wf_sub_fin, df_tr), error = function(e) NULL)
#         }
#
#         if (is.null(fit_sub)) {
#           fused <- preds_long %>%
#             dplyr::group_by(.data$YYYY) %>%
#             dplyr::summarise(
#               pred_fused = .weighted_mean_na(.data$pred, .data$w),
#               .groups = "drop"
#             )
#         } else {
#           fused <- dat %>%
#             dplyr::mutate(pred_fused = predict(fit_sub, new_data = dat)$.pred)
#         }
#       }
#     }
#   }
#
#   # ---------------------------
#   # Post-processing: positivity and date completion
#   # ---------------------------
#   if (isTRUE(target_positive)) {
#     fused <- fused %>% dplyr::mutate(pred_fused = pmax(.data$pred_fused, 0))
#   }
#
#   fused <- dplyr::full_join(
#     tibble::tibble(YYYY = dates_all),
#     fused,
#     by = "YYYY"
#   ) %>%
#     dplyr::arrange(.data$YYYY)
#
#   list(
#     fused = fused,
#     leaderboard_products = lb %>%
#       dplyr::arrange(dplyr::desc(.data$weight), dplyr::desc(.data$kge)),
#     all_results = results
#   )
# }



# Consolidate predictions across products for one model
#
# For a given basin and a fixed statistical model (PCR/Ridge/Lasso), tunes and
# predicts for each product, ranks products by KGE, keeps the top-K, and fuses
# them using normalized KGE-based weights (with safeguards and fallbacks).
#
# @param basin_id Basin identifier.
# @param data_by_product Named list of data frames (one per product).
# @param basin_col Column name for basin IDs (default: `"HYBAS_ID"`).
# @param pred_pattern_by_product Named character vector: product -> regex for predictors.
# @param model One of `"pcr"`, `"ridge"`, `"lasso"`.
# @param topK Integer, number of products to keep.
# @param min_kge_model Minimum KGE threshold to accept fusion (default: 0.2).
# @param prediction_years Optional numeric vector of length 2 giving the
#   start and end years for a holdout prediction period. These years
#   are excluded from training and predictions are generated after fitting.
# @param target_positive Logical; if TRUE, force negative predictions to zero.
# @param resamples Optional \code{rsample::rset} object for resampling.
#   If \code{NULL}, a rolling-origin resampling is created via
#   \code{make_rolling()}.
# @param pretrained_wflow Optional \code{workflows::workflow} object.
#   If supplied, tuning is skipped and the workflow is fitted directly.
# @param grid Optional tibble of tuning parameters. If supplied, this grid
#   is used instead of the default grid for the specified model.
# @param init_frac Fraction of rows used for the initial training window.
# @param assess_frac Fraction of rows used for the assessment window.
# @param n_splits Optional integer, desired number of resamples (splits).
# @param cumulative Logical; passed to \code{rsample::rolling_origin()}.
# @param quiet Logical; if \code{FALSE}, emits informative messages.
# @param verbose Logical, emit diagnostic messages.
# @param max_na_frac Numeric in \eqn{[0, 1]}: maximum allowed fraction of missing
#   values per column before stopping (default \code{0.20} = 20\%).
# @param impute Character, one of \code{"median"}, \code{"mean"}, or \code{"none"}.
#   If \code{"none"}, no imputation is performed after the guard (default \code{"median"}).
# @param require_variance Logical; if \code{TRUE}, stop when a column has zero
#   standard deviation after imputation (default \code{TRUE}).
# @param ... Additional arguments passed to \code{wass2s_tune_pred_stat}.
#
# @return A list with:
#   \itemize{
#     \item `fused` (tibble): columns `YYYY`, `pred_fused`.
#     \item `leaderboard_products` (tibble): product, KGE, counts, weights.
#     \item `all_results` : list of all individual product results.
#   }
# @examples
# \dontrun{
# cns <- wass2s_cons_mods_stat(1040021500, data_by_product, model = "pcr", topK = 3)
# }
# @export

# wass2s_cons_mods_stat <- function(
#     basin_id,
#     data_by_product,
#     basin_col = "HYBAS_ID",
#     pred_pattern_by_product = NULL,
#     model = c("pcr", "ridge", "lasso"),
#     topK = 3,
#     min_kge_model = 0.2,
#     prediction_years = NULL,
#     target_positive = FALSE,
#     resamples = NULL,
#     pretrained_wflow = NULL,
#     grid = NULL,
#     init_frac = 0.60,
#     assess_frac = 0.20,
#     n_splits = NULL,
#     cumulative = TRUE,
#     quiet = TRUE,
#     verbose = FALSE,
#     max_na_frac = 0.3,
#     impute = "median",
#     require_variance = TRUE,
#     ...
# ) {
#   model <- match.arg(model)
#   prods <- names(data_by_product)
#
#   # Input validation
#   if (length(prods) == 0) stop("data_by_product must be a named list", call. = FALSE)
#   if (topK < 1) stop("topK must be at least 1", call. = FALSE)
#   if (!is.null(prediction_years) && length(prediction_years) != 2)
#     stop("prediction_years must be length 2 (start, end).", call. = FALSE)
#   if (min_kge_model > 1) stop("min_kge_model cannot be greater than 1.", call. = FALSE)
#
#   # Collect all YYYYMMDD present (for an "empty" fused if needed)
#   years_all <- sort(unique(unlist(lapply(prods, function(p) {
#     dfp <- data_by_product[[p]]
#     dfp <- dplyr::filter(dfp, .data[[basin_col]] == basin_id) %>% dplyr::ungroup()
#
#     if (!"YYYY" %in% names(dfp)) return(integer(0))
#     .ensure_yyyymmdd(dfp$YYYY)
#   }))))
#
#   # Product loop
#   results <- purrr::map(prods, function(p) {
#
#     dfp <- data_by_product[[p]] %>%
#       dplyr::filter(.data[[basin_col]] == basin_id) %>%
#       dplyr::ungroup()
#
#     # Required cols (because you later select YYYY and Q)
#     missing_cols <- setdiff(c("YYYY", "Q"), names(dfp))
#     if (length(missing_cols) > 0) {
#       if (!verbose) message("[", model, "] ", p, " : skipped (missing: ", paste(missing_cols, collapse = ","), ").")
#       return(NULL)
#     }
#
#     # Standardize date format early
#     dfp$YYYY <- .ensure_yyyymmdd(dfp$YYYY)
#
#     if (nrow(dfp) < 8) {
#       if (!verbose) message("[", model, "] ", p, " : skipped (nrow < 8).")
#       return(NULL)
#     }
#
#     pat <- if (!is.null(pred_pattern_by_product) && p %in% names(pred_pattern_by_product)) {
#       pred_pattern_by_product[[p]]
#     } else if (!is.null(pred_pattern_by_product) && length(pred_pattern_by_product) == 1) {
#       pred_pattern_by_product
#     } else "^pt_"
#
#     predictors <- select_predictors(
#       dfp,
#       pattern = pat,
#       exclude = c(basin_col, "YYYY", "Q")
#     )
#
#     if (!verbose) message("[", model, "] ", p, " : ",
#                           length(predictors), " predictors using pattern '", pat, "'")
#
#     out <- tryCatch({
#       wass2s_tune_pred_stat(
#         df_basin_product = dplyr::select(dfp, YYYY, Q, tidyselect::all_of(predictors)),
#         predictors = predictors,
#         model = model,
#         prediction_years = prediction_years,
#         target_positive = target_positive,
#         resamples = resamples,
#         pretrained_wflow = pretrained_wflow,
#         grid = grid,
#         init_frac = init_frac,
#         assess_frac = assess_frac,
#         n_splits = n_splits,
#         cumulative = cumulative,
#         quiet = quiet,
#         max_na_frac = max_na_frac,
#         impute = impute,
#         require_variance = require_variance,
#         ...
#       )
#     }, error = function(e) {
#       if (!verbose) message(glue::glue("Error training product '{p}' for basin {basin_id}: {e$message}"))
#       NULL
#     })
#
#     if (is.null(out) || !is.list(out) || !"preds" %in% names(out)) {
#       if (!verbose) message("[", model, "] ", p, " : tuning/pred failed (NULL).")
#       return(NULL)
#     }
#
#     # Ensure preds YYYY is YYYYMMDD (defensive)
#     if ("YYYY" %in% names(out$preds)) out$preds$YYYY <- .ensure_yyyymmdd(out$preds$YYYY)
#
#     sd_pred <- stats::sd(out$preds$pred, na.rm = TRUE)
#
#     list(
#       product = p,
#       kge     = out$kge_cv_mean,
#       rsq     = out$rsq_cv_mean,
#       preds   = out$preds,
#       n_pred  = length(predictors),
#       sd_pred = sd_pred
#     )
#   }) %>% purrr::compact()
#
#   # No valid product
#   if (length(results) == 0) {
#     if (!verbose) message("[", model, "] No valid product for basin ", basin_id, ".")
#     fused_empty <- tibble::tibble(YYYY = years_all, pred_fused = NA_real_)
#     return(list(
#       fused = fused_empty,
#       leaderboard_products = tibble::tibble(
#         product = character(), kge = numeric(),
#         n_pred = integer(), sd_pred = numeric(), weight = numeric()
#       )
#     ))
#   }
#
#   lb <- tibble::tibble(
#     product = purrr::map_chr(results, "product"),
#     kge     = purrr::map_dbl(results, "kge"),
#     rsq     = purrr::map_dbl(results, "rsq"),
#     n_pred  = purrr::map_int(results, "n_pred"),
#     sd_pred = purrr::map_dbl(results, "sd_pred")
#   )
#
#   lb_kge_ok <- dplyr::filter(lb, is.finite(kge))
#
#   if (nrow(lb_kge_ok) > 0) {
#     lb_kge_ok <- dplyr::arrange(lb_kge_ok, dplyr::desc(kge))
#     keep_names <- head(lb_kge_ok$product, n = min(topK, nrow(lb_kge_ok)))
#     results_top <- results[match(keep_names, purrr::map_chr(results, "product"))]
#
#     # You currently override weights to 1 anyway
#     kg <- base::rep(1, min(topK, nrow(lb_kge_ok)))
#     kg[kg < 0] <- 0
#     kg[kg < min_kge_model] <- 0
#
#     if (all(kg == 0)) {
#       fused_empty <- tibble::tibble(YYYY = years_all, pred_fused = NA_real_)
#       return(list(
#         fused = fused_empty,
#         leaderboard_products = dplyr::mutate(lb, weight = 0),
#         all_results = results
#       ))
#     }
#
#     w <- kg
#   } else {
#     if (verbose) message("[", model, "] All KGEs NA  fallback: equal weights for non-constant products.")
#     non_const <- dplyr::filter(lb, is.finite(sd_pred) & sd_pred > 0)
#     if (nrow(non_const) == 0) {
#       fused_empty <- tibble::tibble(YYYY = years_all, pred_fused = NA_real_)
#       return(list(
#         fused = fused_empty,
#         leaderboard_products = dplyr::mutate(lb, weight = 0),
#         all_results = results
#       ))
#     }
#     keep_names <- head(non_const$product, n = min(topK, nrow(non_const)))
#     results_top <- results[match(keep_names, purrr::map_chr(results, "product"))]
#     w <- rep(1 / length(keep_names), length(keep_names))
#   }
#
#   preds_long <- purrr::imap_dfr(
#     results_top,
#     ~ dplyr::mutate(.x$preds, product = .x$product, w = w[.y])
#   )
#
#   fused <- fuse_topk(preds_long)
#
#   if (length(years_all) > 0) {
#     fused <- dplyr::full_join(tibble::tibble(YYYY = years_all), fused, by = "YYYY") %>%
#       dplyr::arrange(YYYY)
#   }
#
#   lb$weight <- 0
#   lb$weight[match(keep_names, lb$product)] <- w
#
#   list(
#     fused = fused,
#     leaderboard_products = lb %>% dplyr::arrange(dplyr::desc(weight), dplyr::desc(kge)),
#     all_results = results
#   )
# }

# wass2s_cons_mods_stat <- function(
#     basin_id,
#     data_by_product,
#     basin_col = "HYBAS_ID",
#     pred_pattern_by_product = NULL,
#     model = c("pcr", "ridge", "lasso"),
#     topK = 3,
#     min_kge_model = 0.2,
#     prediction_years = NULL,
#     target_positive = FALSE,
#     resamples = NULL,
#     pretrained_wflow = NULL,
#     grid = NULL,
#     init_frac = 0.60,
#     assess_frac = 0.20,
#     n_splits = NULL,
#     cumulative = TRUE,
#     quiet = TRUE,
#     verbose = FALSE,
#     max_na_frac =0.3,
#     impute = "median",
#     require_variance = TRUE,
#     ...
# ) {
#   model <- match.arg(model)
#   prods <- names(data_by_product)
#
#   # Input validation
#   if (length(prods) == 0) {
#     stop("data_by_product must be a named list", call. = FALSE)
#   }
#
#   if (topK < 1) {
#     stop("topK must be at least 1", call. = FALSE)
#   }
#
#   if (!is.null(prediction_years) && length(prediction_years) != 2) {
#     stop("prediction_years must be length 2 (start, end).", call. = FALSE)
#   }
#
#   if (min_kge_model > 1) {
#     stop("min_kge_model cannot be greater than 1.", call. = FALSE)
#   }
#
#   # Collect all years present (to create a "fused" even if no product is valid)
#   years_all <- sort(unique(unlist(lapply(prods, function(p){
#     dfp <- data_by_product[[p]]
#     dfp <- dplyr::filter(dfp, .data[[basin_col]] == basin_id)
#     dfp$YYYY
#   }))))
#
#   # Product loop
#   results <- purrr::map(prods, function(p){
#     dfp <- data_by_product[[p]] %>%
#       dplyr::filter(.data[[basin_col]] == basin_id) %>%
#       dplyr::ungroup()
#
#     if (nrow(dfp) < 8) {
#       if (!verbose) message("[", model, "] ", p, " : skipped (nrow < 8).")
#       return(NULL)
#     }
#
#     # Predictor pattern (via core::select_predictors)
#     pat <- if (!is.null(pred_pattern_by_product) && p %in% names(pred_pattern_by_product)) {
#       pred_pattern_by_product[[p]]
#     } else if(!is.null(pred_pattern_by_product) && length(pred_pattern_by_product) ==1) pred_pattern_by_product else "^pt_"
#
#     predictors <- select_predictors(
#       dfp,
#       pattern = pat,
#       exclude = c(basin_col, "YYYY", "Q")
#     )
#
#     if (!verbose) message("[", model, "] ", p, " : ",
#                          length(predictors), " predictors using pattern '", pat, "'")
#
#     # Call tuner + predictions with all additional parameters
#     out <- tryCatch({
#       # Call tuner + predictions with all additional parameters
#      wass2s_tune_pred_stat(
#         df_basin_product = dplyr::select(dfp, YYYY, Q, tidyselect::all_of(predictors)),
#         predictors = predictors,
#         model = model,
#         prediction_years = prediction_years,
#         target_positive = target_positive,
#         resamples = resamples,
#         pretrained_wflow = pretrained_wflow,
#         grid = grid,
#         init_frac = init_frac,
#         assess_frac = assess_frac,
#         n_splits = n_splits,
#         cumulative = cumulative,
#         quiet = quiet,
#         max_na_frac =max_na_frac,
#         impute = impute,
#         require_variance = require_variance,
#         ...
#       )
#     }, error = function(e) {
#       if (!verbose) message(glue::glue("Error training product '{p}' for basin {basin_id}: {e$message}"))
#       return(NULL)
#     })
#
#     # If tuner failed properly
#     if (is.null(out) || !is.list(out) || !"preds" %in% names(out)) {
#       if (!verbose) message("[", model, "] ", p, " : tuning/pred failed (NULL).")
#       return(NULL)
#     }
#
#     sd_pred <- stats::sd(out$preds$pred, na.rm = TRUE)
#
#     list(
#       product = p,
#       kge     = out$kge_cv_mean,
#       rsq     = out$rsq_cv_mean,
#       preds   = out$preds,
#       n_pred  = length(predictors),
#       sd_pred = sd_pred
#     )
#   }) %>% purrr::compact()
#
#   # No valid product  return an "empty" fused (NA)
#   if (length(results) == 0) {
#     if (!verbose) message("[", model, "] No valid product for basin ", basin_id, ".")
#     fused_empty <- tibble::tibble(YYYY = years_all, pred_fused = NA_real_)
#     return(list(
#       fused = fused_empty,
#       leaderboard_products = tibble::tibble(
#         product = character(), kge = numeric(),
#         n_pred = integer(), sd_pred = numeric(), weight = numeric()
#       )
#     ))
#   }
#
#   # Product leaderboard (may contain NA)
#   lb <- tibble::tibble(
#     product = purrr::map_chr(results, "product"),
#     kge     = purrr::map_dbl(results, "kge"),
#     rsq     = purrr::map_dbl(results, "rsq"),
#     n_pred  = purrr::map_int(results, "n_pred"),
#     sd_pred = purrr::map_dbl(results, "sd_pred")
#   )
#
#   # Isolate those with defined KGE
#   lb_kge_ok <- dplyr::filter(lb, is.finite(kge))
#
#   if (nrow(lb_kge_ok) > 0) {
#     lb_kge_ok <- dplyr::arrange(lb_kge_ok, dplyr::desc(kge))
#     keep_names <- head(lb_kge_ok$product, n = min(topK, nrow(lb_kge_ok)))
#     results_top <- results[match(keep_names, purrr::map_chr(results, "product"))]
#
#     #kg <- lb_kge_ok$kge[match(keep_names, lb_kge_ok$product)]
#     kg <- wass2s_minmax(lb_kge_ok$rsq[match(keep_names, lb_kge_ok$product)])
#
#     # Alternative
#     kg <- base::rep(1, min(topK, nrow(lb_kge_ok)))
#     # Apply KGE thresholds
#     kg[kg < 0] <- 0
#     kg[kg < min_kge_model] <- 0
#
#     # If all weights become zero after thresholding
#     if (all(kg == 0)) {
#       if (verbose) message("[", model, "] All KGE values below threshold (", min_kge_model, ") for basin ", basin_id, ".")
#       fused_empty <- tibble::tibble(YYYY = years_all, pred_fused = NA_real_)
#       return(list(
#         fused = fused_empty,
#         leaderboard_products = dplyr::mutate(lb, weight = 0),
#         all_results = results
#       ))
#     }
#
#     w <- kg
#
#   } else {
#     # Fallback: no valid KGE  keep products with non-constant predictions
#     if (verbose) message("[", model, "] All KGEs NA  fallback: equal weights for non-constant products.")
#     non_const <- dplyr::filter(lb, is.finite(sd_pred) & sd_pred > 0)
#     if (nrow(non_const) == 0) {
#       # nothing usable
#       fused_empty <- tibble::tibble(YYYY = years_all, pred_fused = NA_real_)
#       return(list(
#         fused = fused_empty,
#         leaderboard_products = dplyr::mutate(lb, weight = 0),
#         all_results = results
#       ))
#     }
#     keep_names <- head(non_const$product, n = min(topK, nrow(non_const)))
#     results_top <- results[match(keep_names, purrr::map_chr(results, "product"))]
#     w <- rep(1 / length(keep_names), length(keep_names))  # equal weights
#   }
#
#   # Fusion construction (via core::fuse_topk)
#   preds_long <- purrr::imap_dfr(results_top, ~ dplyr::mutate(.x$preds,
#                                                              product = .x$product,
#                                                              w = w[.y]))
#   fused <- fuse_topk(preds_long)
#
#   # Complete with all years if needed
#   if (length(years_all) > 0) {
#     fused <- dplyr::full_join(
#       tibble::tibble(YYYY = years_all),
#       fused, by = "YYYY"
#     ) %>% dplyr::arrange(YYYY)
#   }
#
#   # Leaderboard enriched with weights
#   lb$weight <- 0
#   lb$weight[match(keep_names, lb$product)] <- w
#
#   list(
#     fused = fused,
#     leaderboard_products = lb %>% dplyr::arrange(dplyr::desc(weight), dplyr::desc(kge)),
#     all_results = results
#   )
# }



# wass2s_cons_mods_stat <- function(
#     basin_id,
#     data_by_product,
#     basin_col = "HYBAS_ID",
#     pred_pattern_by_product = NULL,
#     model = c("pcr", "ridge", "lasso"),
#     topK = 3,
#     min_kge_model = 0.2,
#     min_predictors = 2,
#     prediction_years = NULL,
#     target_positive = FALSE,
#     resamples = NULL,
#     pretrained_wflow = NULL,
#     grid = NULL,
#     init_frac = 0.60,
#     assess_frac = 0.20,
#     n_splits = NULL,
#     cumulative = TRUE,
#     quiet = TRUE,
#     verbose = TRUE,
#     ...
# ) {
#   model <- match.arg(model)
#   prods <- names(data_by_product)
#
#   # Input validation
#   if (length(prods) == 0) {
#     stop("data_by_product must be a named list", call. = FALSE)
#   }
#
#   if (topK < 1) {
#     stop("topK must be at least 1", call. = FALSE)
#   }
#
#   if (!is.null(prediction_years) && length(prediction_years) != 2) {
#     stop("prediction_years must be length 2 (start, end).", call. = FALSE)
#   }
#
#   if (min_kge_model > 1) {
#     stop("min_kge_model cannot be greater than 1.", call. = FALSE)
#   }
#
#   # Collect all years present (to create a "fused" even if no product is valid)
#   years_all <- sort(unique(unlist(lapply(prods, function(p){
#     dfp <- data_by_product[[p]]
#     dfp <- dplyr::filter(dfp, .data[[basin_col]] == basin_id)
#     dfp$YYYY
#   }))))
#
#   # Product loop
#   results <- purrr::map(prods, function(p){
#     dfp <- data_by_product[[p]] %>%
#       dplyr::filter(.data[[basin_col]] == basin_id) %>%
#       dplyr::ungroup()
#
#     if (nrow(dfp) < 8) {
#       if (verbose) message("[", model, "] ", p, " : skipped (nrow < 8).")
#       return(NULL)
#     }
#
#     # Predictor pattern (via core::select_predictors)
#     pat <- if (!is.null(pred_pattern_by_product) && p %in% names(pred_pattern_by_product)) {
#       pred_pattern_by_product[[p]]
#     } else "^pt_"
#
#     predictors <- select_predictors(
#       dfp,
#       pattern = pat,
#       exclude = c(basin_col, "YYYY", "Q")
#     )
#
#     if (verbose) message("[", model, "] ", p, " : ",
#                          length(predictors), " predictors using pattern '", pat, "'")
#
#     if (length(predictors) < min_predictors) {
#       if (verbose) message("[", model, "] ", p, " : skipped (predictors < ", min_predictors, ").")
#       return(NULL)
#     }
#
#     # Call tuner + predictions with all additional parameters
#     out <- wass2s_tune_pred_stat(
#       df_basin_product = dplyr::select(dfp, YYYY, Q, tidyselect::all_of(predictors)),
#       predictors = predictors,
#       model = model,
#       prediction_years = prediction_years,
#       target_positive = target_positive,
#       resamples = resamples,
#       pretrained_wflow = pretrained_wflow,
#       grid = grid,
#       init_frac = init_frac,
#       assess_frac = assess_frac,
#       n_splits = n_splits,
#       cumulative = cumulative,
#       quiet = quiet,
#       ...
#     )
#
#     # If tuner failed properly
#     if (is.null(out) || !is.list(out) || !"preds" %in% names(out)) {
#       if (verbose) message("[", model, "] ", p, " : tuning/pred failed (NULL).")
#       return(NULL)
#     }
#
#     sd_pred <- stats::sd(out$preds$pred, na.rm = TRUE)
#
#     list(
#       product = p,
#       kge     = out$kge_cv_mean,
#       preds   = out$preds,
#       n_pred  = length(predictors),
#       sd_pred = sd_pred
#     )
#   }) %>% purrr::compact()
#
#   # No valid product  return an "empty" fused (NA)
#   if (length(results) == 0) {
#     if (verbose) message("[", model, "] No valid product for basin ", basin_id, ".")
#     fused_empty <- tibble::tibble(YYYY = years_all, pred_fused = NA_real_)
#     return(list(
#       fused = fused_empty,
#       leaderboard_products = tibble::tibble(
#         product = character(), kge = numeric(),
#         n_pred = integer(), sd_pred = numeric(), weight = numeric()
#       )
#     ))
#   }
#
#   # Product leaderboard (may contain NA)
#   lb <- tibble::tibble(
#     product = purrr::map_chr(results, "product"),
#     kge     = purrr::map_dbl(results, "kge"),
#     n_pred  = purrr::map_int(results, "n_pred"),
#     sd_pred = purrr::map_dbl(results, "sd_pred")
#   )
#
#   # Isolate those with defined KGE
#   lb_kge_ok <- dplyr::filter(lb, is.finite(kge))
#
#   if (nrow(lb_kge_ok) > 0) {
#     lb_kge_ok <- dplyr::arrange(lb_kge_ok, dplyr::desc(kge))
#     keep_names <- head(lb_kge_ok$product, n = min(topK, nrow(lb_kge_ok)))
#     results_top <- results[match(keep_names, purrr::map_chr(results, "product"))]
#
#     kg <- lb_kge_ok$kge[match(keep_names, lb_kge_ok$product)]
#
#     # Apply KGE thresholds
#     kg[kg < 0] <- 0
#     kg[kg < min_kge_model] <- 0
#
#     # If all weights become zero after thresholding
#     if (all(kg == 0)) {
#       if (verbose) message("[", model, "] All KGE values below threshold (", min_kge_model, ") for basin ", basin_id, ".")
#       fused_empty <- tibble::tibble(YYYY = years_all, pred_fused = NA_real_)
#       return(list(
#         fused = fused_empty,
#         leaderboard_products = dplyr::mutate(lb, weight = 0),
#         all_results = results
#       ))
#     }
#
#     w <- kg
#
#   } else {
#     # Fallback: no valid KGE  keep products with non-constant predictions
#     if (verbose) message("[", model, "] All KGEs NA  fallback: equal weights for non-constant products.")
#     non_const <- dplyr::filter(lb, is.finite(sd_pred) & sd_pred > 0)
#     if (nrow(non_const) == 0) {
#       # nothing usable
#       fused_empty <- tibble::tibble(YYYY = years_all, pred_fused = NA_real_)
#       return(list(
#         fused = fused_empty,
#         leaderboard_products = dplyr::mutate(lb, weight = 0),
#         all_results = results
#       ))
#     }
#     keep_names <- head(non_const$product, n = min(topK, nrow(non_const)))
#     results_top <- results[match(keep_names, purrr::map_chr(results, "product"))]
#     w <- rep(1 / length(keep_names), length(keep_names))  # equal weights
#   }
#
#   # Fusion construction (via core::fuse_topk)
#   preds_long <- purrr::imap_dfr(results_top, ~ dplyr::mutate(.x$preds,
#                                                              product = .x$product,
#                                                              w = w[.y]))
#   fused <- fuse_topk(preds_long)
#
#   # Complete with all years if needed
#   if (length(years_all) > 0) {
#     fused <- dplyr::full_join(
#       tibble::tibble(YYYY = years_all),
#       fused, by = "YYYY"
#     ) %>% dplyr::arrange(YYYY)
#   }
#
#   # Leaderboard enriched with weights
#   lb$weight <- 0
#   lb$weight[match(keep_names, lb$product)] <- w
#
#   list(
#     fused = fused,
#     leaderboard_products = lb %>% dplyr::arrange(dplyr::desc(weight), dplyr::desc(kge)),
#     all_results = results
#   )
# }




