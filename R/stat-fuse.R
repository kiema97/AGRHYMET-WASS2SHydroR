#' Consolidate predictions across products for one model
#'
#' For a given basin and a fixed statistical model (PCR/Ridge/Lasso), tunes and
#' predicts for each product, ranks products by KGE, keeps the top-K, and fuses
#' them using normalized KGE-based weights (with safeguards and fallbacks).
#'
#' @param basin_id Basin identifier.
#' @param data_by_product Named list of data frames (one per product).
#' @param basin_col Column name for basin IDs (default: `"HYBAS_ID"`).
#' @param pred_pattern_by_product Named character vector: product -> regex for predictors.
#' @param model One of `"pcr"`, `"ridge"`, `"lasso"`.
#' @param topK Integer, number of products to keep.
#' @param min_kge_model Minimum KGE threshold to accept fusion (default: 0.2).
#' @param min_predictors Minimum number of predictors required to keep a product.
#' @param prediction_years Optional numeric vector of length 2 giving the
#'   start and end years for a holdout prediction period. These years
#'   are excluded from training and predictions are generated after fitting.
#' @param target_positive Logical; if TRUE, force negative predictions to zero.
#' @param resamples Optional \code{rsample::rset} object for resampling.
#'   If \code{NULL}, a rolling-origin resampling is created via
#'   \code{make_rolling()}.
#' @param pretrained_wflow Optional \code{workflows::workflow} object.
#'   If supplied, tuning is skipped and the workflow is fitted directly.
#' @param grid Optional tibble of tuning parameters. If supplied, this grid
#'   is used instead of the default grid for the specified model.
#' @param init_frac Fraction of rows used for the initial training window.
#' @param assess_frac Fraction of rows used for the assessment window.
#' @param n_splits Optional integer, desired number of resamples (splits).
#' @param cumulative Logical; passed to \code{rsample::rolling_origin()}.
#' @param quiet Logical; if \code{FALSE}, emits informative messages.
#' @param verbose Logical, emit diagnostic messages.
#' @param ... Additional arguments passed to \code{wass2s_tune_pred_stat}.
#'
#' @return A list with:
#'   \itemize{
#'     \item `fused` (tibble): columns `YYYY`, `pred_fused`.
#'     \item `leaderboard_products` (tibble): product, KGE, counts, weights.
#'     \item `all_results` : list of all individual product results.
#'   }
#' @examples
#' \dontrun{
#' cns <- wass2s_cons_mods_stat(1040021500, data_by_product, model = "pcr", topK = 3)
#' }
#' @export
wass2s_cons_mods_stat <- function(
    basin_id,
    data_by_product,
    basin_col = "HYBAS_ID",
    pred_pattern_by_product = NULL,
    model = c("pcr", "ridge", "lasso"),
    topK = 3,
    min_kge_model = 0.2,
    min_predictors = 2,
    prediction_years = NULL,
    target_positive = FALSE,
    resamples = NULL,
    pretrained_wflow = NULL,
    grid = NULL,
    init_frac = 0.60,
    assess_frac = 0.20,
    n_splits = NULL,
    cumulative = TRUE,
    quiet = TRUE,
    verbose = TRUE,
    ...
) {
  model <- match.arg(model)
  prods <- names(data_by_product)

  # Input validation
  if (length(prods) == 0) {
    stop("data_by_product must be a named list", call. = FALSE)
  }

  if (topK < 1) {
    stop("topK must be at least 1", call. = FALSE)
  }

  if (!is.null(prediction_years) && length(prediction_years) != 2) {
    stop("prediction_years must be length 2 (start, end).", call. = FALSE)
  }

  if (min_kge_model > 1) {
    stop("min_kge_model cannot be greater than 1.", call. = FALSE)
  }

  # Collect all years present (to create a "fused" even if no product is valid)
  years_all <- sort(unique(unlist(lapply(prods, function(p){
    dfp <- data_by_product[[p]]
    dfp <- dplyr::filter(dfp, .data[[basin_col]] == basin_id)
    dfp$YYYY
  }))))

  # Product loop
  results <- purrr::map(prods, function(p){
    dfp <- data_by_product[[p]] %>%
      dplyr::filter(.data[[basin_col]] == basin_id) %>%
      dplyr::ungroup()

    if (nrow(dfp) < 8) {
      if (verbose) message("[", model, "] ", p, " : skipped (nrow < 8).")
      return(NULL)
    }

    # Predictor pattern (via core::select_predictors)
    pat <- if (!is.null(pred_pattern_by_product) && p %in% names(pred_pattern_by_product)) {
      pred_pattern_by_product[[p]]
    } else "^pt_"

    predictors <- select_predictors(
      dfp,
      pattern = pat,
      exclude = c(basin_col, "YYYY", "Q")
    )

    if (verbose) message("[", model, "] ", p, " : ",
                         length(predictors), " predictors using pattern '", pat, "'")

    if (length(predictors) < min_predictors) {
      if (verbose) message("[", model, "] ", p, " : skipped (predictors < ", min_predictors, ").")
      return(NULL)
    }

    # Call tuner + predictions with all additional parameters
    out <- wass2s_tune_pred_stat(
      df_basin_product = dplyr::select(dfp, YYYY, Q, tidyselect::all_of(predictors)),
      predictors = predictors,
      model = model,
      prediction_years = prediction_years,
      target_positive = target_positive,
      resamples = resamples,
      pretrained_wflow = pretrained_wflow,
      grid = grid,
      init_frac = init_frac,
      assess_frac = assess_frac,
      n_splits = n_splits,
      cumulative = cumulative,
      quiet = quiet,
      ...
    )

    # If tuner failed properly
    if (is.null(out) || !is.list(out) || !"preds" %in% names(out)) {
      if (verbose) message("[", model, "] ", p, " : tuning/pred failed (NULL).")
      return(NULL)
    }

    sd_pred <- stats::sd(out$preds$pred, na.rm = TRUE)

    list(
      product = p,
      kge     = out$kge_cv_mean,
      preds   = out$preds,
      n_pred  = length(predictors),
      sd_pred = sd_pred
    )
  }) %>% purrr::compact()

  # No valid product  return an "empty" fused (NA)
  if (length(results) == 0) {
    if (verbose) message("[", model, "] No valid product for basin ", basin_id, ".")
    fused_empty <- tibble::tibble(YYYY = years_all, pred_fused = NA_real_)
    return(list(
      fused = fused_empty,
      leaderboard_products = tibble::tibble(
        product = character(), kge = numeric(),
        n_pred = integer(), sd_pred = numeric(), weight = numeric()
      )
    ))
  }

  # Product leaderboard (may contain NA)
  lb <- tibble::tibble(
    product = purrr::map_chr(results, "product"),
    kge     = purrr::map_dbl(results, "kge"),
    n_pred  = purrr::map_int(results, "n_pred"),
    sd_pred = purrr::map_dbl(results, "sd_pred")
  )

  # Isolate those with defined KGE
  lb_kge_ok <- dplyr::filter(lb, is.finite(kge))

  if (nrow(lb_kge_ok) > 0) {
    lb_kge_ok <- dplyr::arrange(lb_kge_ok, dplyr::desc(kge))
    keep_names <- head(lb_kge_ok$product, n = min(topK, nrow(lb_kge_ok)))
    results_top <- results[match(keep_names, purrr::map_chr(results, "product"))]

    kg <- lb_kge_ok$kge[match(keep_names, lb_kge_ok$product)]

    # Apply KGE thresholds
    kg[kg < 0] <- 0
    kg[kg < min_kge_model] <- 0

    # If all weights become zero after thresholding
    if (all(kg == 0)) {
      if (verbose) message("[", model, "] All KGE values below threshold (", min_kge_model, ") for basin ", basin_id, ".")
      fused_empty <- tibble::tibble(YYYY = years_all, pred_fused = NA_real_)
      return(list(
        fused = fused_empty,
        leaderboard_products = dplyr::mutate(lb, weight = 0),
        all_results = results
      ))
    }

    w <- kg

  } else {
    # Fallback: no valid KGE  keep products with non-constant predictions
    if (verbose) message("[", model, "] All KGEs NA  fallback: equal weights for non-constant products.")
    non_const <- dplyr::filter(lb, is.finite(sd_pred) & sd_pred > 0)
    if (nrow(non_const) == 0) {
      # nothing usable
      fused_empty <- tibble::tibble(YYYY = years_all, pred_fused = NA_real_)
      return(list(
        fused = fused_empty,
        leaderboard_products = dplyr::mutate(lb, weight = 0),
        all_results = results
      ))
    }
    keep_names <- head(non_const$product, n = min(topK, nrow(non_const)))
    results_top <- results[match(keep_names, purrr::map_chr(results, "product"))]
    w <- rep(1 / length(keep_names), length(keep_names))  # equal weights
  }

  # Fusion construction (via core::fuse_topk)
  preds_long <- purrr::imap_dfr(results_top, ~ dplyr::mutate(.x$preds,
                                                             product = .x$product,
                                                             w = w[.y]))
  fused <- fuse_topk(preds_long)

  # Complete with all years if needed
  if (length(years_all) > 0) {
    fused <- dplyr::full_join(
      tibble::tibble(YYYY = years_all),
      fused, by = "YYYY"
    ) %>% dplyr::arrange(YYYY)
  }

  # Leaderboard enriched with weights
  lb$weight <- 0
  lb$weight[match(keep_names, lb$product)] <- w

  list(
    fused = fused,
    leaderboard_products = lb %>% dplyr::arrange(dplyr::desc(weight), dplyr::desc(kge)),
    all_results = results
  )
}
