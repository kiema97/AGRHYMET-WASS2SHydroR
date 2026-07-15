#' Fuse predictions from multiple products into a single forecast
#'
#' This function combines product-level predictions into a single fused prediction
#' series using one of several fusion strategies: meta-learning, simple mean,
#' median, performance-based weighted mean, or best-product selection.
#'
#' Product-level results are expected to come from upstream model training steps,
#' with each product contributing predicted values over time and an associated
#' performance score (for example cross-validated KGE).
#'
#' @param results A list of product-level results. Each element must typically
#'   contain:
#'   \itemize{
#'     \item \code{product}: product name;
#'     \item \code{score}: product performance score used for ranking and weighting;
#'     \item \code{preds}: a data frame containing at least \code{YYYY} and
#'       \code{pred}, and optionally \code{Q}.
#'   }
#'
#' @param dates_all A vector of all dates to retain in the final fused output.
#'   Dates are internally standardized to \code{YYYYMMDD} format.
#'
#' @param topK Integer. Number of best products retained before applying the
#'   fusion method.
#'
#' @param min_score Numeric. Minimum score floor used when computing weights for
#'   the \code{"weighted_mean"} fusion strategy. This prevents zero or negative
#'   weights from dominating the weighted fusion.
#'
#' @param prediction_years Optional numeric vector of length 2 specifying the
#'   prediction period boundaries. Values can be provided as \code{YYYY} or
#'   \code{YYYYMMDD}. This argument is mainly used when
#'   \code{product_fusion_method = "meta"} to define the holdout period excluded from
#'   sub-fuser training.
#'
#' @param product_fusion_method Character string specifying the fusion strategy.
#'   Supported values are:
#'   \itemize{
#'     \item \code{"meta"}: train a second-level learner on retained product predictions;
#'     \item \code{"mean"}: simple arithmetic mean across retained products;
#'     \item \code{"median"}: median across retained products;
#'     \item \code{"weighted_mean"}: weighted mean using product performance scores;
#'     \item \code{"best"}: keep only the best-ranked product.
#'   }
#'
#' @param sub_fuser Character. Meta-model used when
#'   \code{product_fusion_method = "meta"}. Must be supported by \code{model_spec()} and
#'   \code{model_grid()}.
#'
#' @param sub_grid_levels Integer. Number of levels used to generate the tuning
#'   grid for the meta-fuser.
#'
#' @param min_data_required Integer. Minimum number of training observations
#'   required to fit the meta-fuser. If not met, the function falls back to the
#'   \code{"median"} strategy.
#'
#' @param target_positive Logical. If \code{TRUE}, fused predictions are
#'   constrained to be non-negative using \code{pmax(pred_fused, 0)}.
#'
#' @param quiet Logical. If \code{TRUE}, suppress most informational messages.
#'
#' @param verbose Logical. If \code{TRUE}, allow progress and fallback messages
#'   through the internal messaging helper.
#'
#' @param seed Integer random seed for reproducibility.
#'
#' @param ... Additional arguments reserved for future extensions.
#'
#' @details
#' The function first filters the input results to retain only products with
#' usable predictions. It then builds a product leaderboard using the supplied
#' performance scores and keeps the top \code{topK} products.
#'
#' For simple fusion strategies (\code{"mean"}, \code{"median"},
#' \code{"weighted_mean"}, \code{"best"}), predictions are combined directly
#' across retained products.
#'
#' For \code{"meta"}, a second-level model is trained using retained product
#' predictions as predictors and observed values \code{Q} as the outcome. If the
#' meta-fuser cannot be trained (for example because of insufficient data,
#' missing \code{Q}, or fitting failure), the function automatically falls back
#' to \code{"median"}.
#'
#' Product weights used by \code{"weighted_mean"} are derived from the retained
#' product scores after applying a lower bound defined by \code{min_score}.
#'
#' @return A list with the following elements:
#' \itemize{
#'   \item \code{fused}: a data frame containing \code{YYYY}, \code{pred_fused},
#'     and, when available, \code{Q};
#'   \item \code{leaderboard_products}: a ranked data frame of products with
#'     their scores and assigned weights;
#'   \item \code{all_results}: the filtered input results retained internally;
#'   \item \code{product_fusion_method}: the fusion strategy effectively used. This may
#'     differ from the requested one if an automatic fallback occurred.
#' }
#'
#' @examples
#' \dontrun{
#' # Median fusion across the best 3 products
#' out <- fuse_products_predictions(
#'   results = results_std,
#'   dates_all = dates_all,
#'   topK = 3,
#'   product_fusion_method = "median"
#' )
#'
#' # Weighted mean fusion
#' out <- fuse_products_predictions(
#'   results = results_std,
#'   dates_all = dates_all,
#'   topK = 3,
#'   product_fusion_method = "weighted_mean"
#' )
#'
#' # Best-product strategy
#' out <- fuse_products_predictions(
#'   results = results_std,
#'   dates_all = dates_all,
#'   topK = 1,
#'   product_fusion_method = "best"
#' )
#'
#' # Meta-fusion
#' out <- fuse_products_predictions(
#'   results = results_std,
#'   dates_all = dates_all,
#'   topK = 3,
#'   product_fusion_method = "meta",
#'   sub_fuser = "rf"
#' )
#' }
#'
#' @keywords internal
fuse_products_predictions <- function(
    results,
    dates_all,
    topK = 3,
    min_score = 0.2,                 # e.g., min KGE
    prediction_years = NULL,         # YYYY or YYYYMMDD (len 2)
    product_fusion_method = c("median", "mean", "meta", "weighted_mean", "best"),
    sub_fuser = "rf",
    sub_grid_levels = 5,
    min_data_required = 10,
    target_positive = TRUE,
    quiet = TRUE,
    verbose = TRUE,
    use_sub_fuser = TRUE,
    seed = 123,
    ...
) {
  set.seed(seed)

  product_fusion_method <- match.arg(product_fusion_method)

  # Keep only products with usable preds
  results <- purrr::keep(results, ~ !is.null(.x$preds) && nrow(.x$preds) > 0)

  if (length(results) == 0) {
    return(list(
      fused = tibble::tibble(YYYY = dates_all, pred_fused = NA_real_),
      leaderboard_products = tibble::tibble(),
      all_results = results,
      product_fusion_method = product_fusion_method
    ))
  }

  # Build leaderboard
  lb <- tibble::tibble(
    product = purrr::map_chr(results, "product"),
    score   = purrr::map_dbl(results, "score")
  ) |>
    dplyr::mutate(score = ifelse(is.finite(.data$score), .data$score, NA_real_))

  lb_ok <- dplyr::filter(lb, is.finite(.data$score)) |>
    dplyr::arrange(dplyr::desc(.data$score))

  if (nrow(lb_ok) == 0) {
    return(list(
      fused = tibble::tibble(YYYY = dates_all, pred_fused = NA_real_),
      leaderboard_products = dplyr::mutate(lb, weight = 0),
      all_results = results,
      product_fusion_method = product_fusion_method
    ))
  }

  # Keep best topK products above threshold; if none pass, retain the best one.
  lb_keep <- lb_ok
  if (is.finite(min_score)) {
    lb_keep <- dplyr::filter(lb_ok, .data$score >= min_score)
    if (nrow(lb_keep) == 0L) {
      .msg(quiet, verbose, "No product met min_score; retaining the best available product.")
      lb_keep <- dplyr::slice(lb_ok, 1)
    }
  }

  keep_names <- head(lb_keep$product, n = min(topK, nrow(lb_keep)))
  results_top <- results[match(keep_names, purrr::map_chr(results, "product"))]

  # Weights from scores
  scores_keep <- lb_keep$score[match(keep_names, lb_keep$product)]
  w <- scores_keep
  w[!is.finite(w)] <- NA_real_
  if (is.finite(min_score)) {
    w[w < min_score] <- 0
  }
  w <- pmax(w, 0)

  if (!any(is.finite(w) & w > 0)) {
    w <- rep(0, length(keep_names))
    w[1] <- 1
  } else {
    w[!is.finite(w)] <- 0
    w <- w / sum(w)
  }

  leaderboard <- dplyr::mutate(lb, weight = 0)
  leaderboard$weight[match(keep_names, leaderboard$product)] <- w

  # Long table of predictions
  preds_long <- purrr::map2_dfr(results_top, seq_along(results_top), ~ {
    dplyr::transmute(
      .x$preds,
      YYYY    = .ensure_yyyymmdd(.data$YYYY),
      pred    = .data$pred,
      product = .x$product,
      w       = w[.y]
    )
  })

  # Observed Q if available
  obs <- purrr::map_dfr(results_top, ~ {
    if ("Q" %in% names(.x$preds)) {
      dplyr::transmute(.x$preds, YYYY = .ensure_yyyymmdd(.data$YYYY), Q = .data$Q)
    } else {
      dplyr::transmute(.x$preds, YYYY = .ensure_yyyymmdd(.data$YYYY)) |>
        dplyr::mutate(Q = NA_real_)
    }
  }) |>
    dplyr::distinct(.data$YYYY, .keep_all = TRUE)

  # Bounds (YYYYMMDD)
  bounds <- .ensure_year_bounds(prediction_years)

  # ---- simple fusion helpers ----
  simple_mean_fused <- function() {
    preds_long |>
      dplyr::group_by(.data$YYYY) |>
      dplyr::summarise(
        pred_fused = mean(.data$pred, na.rm = TRUE),
        .groups = "drop"
      ) |>
      dplyr::mutate(pred_fused = ifelse(is.nan(.data$pred_fused), NA_real_, .data$pred_fused)) |>
      dplyr::full_join(obs, by = "YYYY")
  }

  median_fused <- function() {
    preds_long |>
      dplyr::group_by(.data$YYYY) |>
      dplyr::summarise(
        pred_fused = stats::median(.data$pred, na.rm = TRUE),
        .groups = "drop"
      ) |>
      dplyr::mutate(pred_fused = ifelse(is.nan(.data$pred_fused), NA_real_, .data$pred_fused)) |>
      dplyr::full_join(obs, by = "YYYY")
  }

  weighted_mean_fused <- function() {
    preds_long |>
      dplyr::group_by(.data$YYYY) |>
      dplyr::summarise(
        pred_fused = {
          ok <- is.finite(.data$pred) & is.finite(.data$w)
          if (!any(ok)) {
            NA_real_
          } else {
            ww <- .data$w[ok]
            pp <- .data$pred[ok]
            if (sum(ww) <= 0) {
              mean(pp, na.rm = TRUE)
            } else {
              sum(pp * ww, na.rm = TRUE) / sum(ww, na.rm = TRUE)
            }
          }
        },
        .groups = "drop"
      ) |>
      dplyr::full_join(obs, by = "YYYY")
  }

  best_product_fused <- function() {
    best_prod <- keep_names[1]
    preds_long |>
      dplyr::filter(.data$product == best_prod) |>
      dplyr::transmute(YYYY = .data$YYYY, pred_fused = .data$pred) |>
      dplyr::full_join(obs, by = "YYYY")
  }

  fused <- NULL
  method_used <- product_fusion_method

  # ---------------------------
  # Fusion switch
  # ---------------------------
  if (product_fusion_method == "mean") {

    fused <- simple_mean_fused()

  } else if (product_fusion_method == "median") {

    fused <- median_fused()

  } else if (product_fusion_method == "weighted_mean") {

    fused <- weighted_mean_fused()

  } else if (product_fusion_method == "best") {

    fused <- best_product_fused()

  } else if (product_fusion_method == "meta") {

    if (length(results_top) < 2) {
      .msg(quiet, verbose, "product_fusion_method = 'meta' but fewer than 2 products retained. Falling back to 'median'.")
      fused <- median_fused()
      method_used <- "median"

    } else if (all(is.na(obs$Q))) {
      .msg(quiet, verbose, "product_fusion_method = 'meta' but observed Q is unavailable. Falling back to 'median'.")
      fused <- median_fused()
      method_used <- "median"

    } else {

      # Wide matrix of retained product predictions
      lst_wide <- purrr::map(results_top, ~ {
        dplyr::transmute(.x$preds, YYYY = .ensure_yyyymmdd(.data$YYYY), pred = .data$pred) |>
          dplyr::rename(!!.x$product := pred)
      })

      prods_wide <- Reduce(function(a, b) dplyr::full_join(a, b, by = "YYYY"), lst_wide)

      dat <- dplyr::left_join(obs, prods_wide, by = "YYYY") |>
        dplyr::arrange(.data$YYYY)

      df_tr <- dat
      if (!is.null(bounds)) {
        df_tr <- dplyr::filter(dat, !(.data$YYYY >= bounds[1] & .data$YYYY <= bounds[2]))
      }
      df_tr <- dplyr::filter(df_tr, is.finite(.data$Q))

      pred_cols <- setdiff(names(df_tr), c("YYYY", "Q"))
      pred_cols <- pred_cols[vapply(df_tr[, pred_cols, drop = FALSE], is.numeric, logical(1))]

      if (nrow(df_tr) < min_data_required || length(pred_cols) < 2) {
        .msg(quiet, verbose, "product_fusion_method = 'meta' fallback to 'median' (insufficient training data or predictors).")
        fused <- median_fused()
        method_used <- "median"

      } else {

        form <- stats::reformulate(termlabels = pred_cols, response = "Q")

        rec <- recipes::recipe(form, data = df_tr) |>
          recipes::step_zv(recipes::all_predictors()) |>
          recipes::step_impute_median(recipes::all_numeric_predictors()) |>
          recipes::step_normalize(recipes::all_numeric_predictors())

        spec <- model_spec(sub_fuser, p = length(pred_cols))
        grid_sub <- model_grid(
          sub_fuser,
          p = length(pred_cols),
          levels = sub_grid_levels,
          n_min = nrow(df_tr)
        )

        wf <- workflows::workflow() |>
          workflows::add_recipe(rec) |>
          workflows::add_model(spec)

        rset <- tryCatch({
          make_rolling(
            df_tr,
            year_col = "YYYY",
            init_frac = 0.8,
            assess_frac = 0.2,
            n_splits = min(3, nrow(df_tr) - 1L),
            cumulative = TRUE,
            quiet = TRUE
          )
        }, error = function(e) NULL)

        fit <- NULL

        if (!is.null(rset) && length(rset$splits) >= 1) {
          ctrl <- tune::control_grid(
            save_pred = TRUE,
            verbose = FALSE,
            allow_par = TRUE
          )

          tuned <- tryCatch({
            suppressWarnings(
              tune::tune_grid(
                wf,
                resamples = rset,
                grid = grid_sub,
                metrics = yardstick::metric_set(yardstick::rmse),
                control = ctrl
              )
            )
          }, error = function(e) {
            .msg(quiet, verbose, "Meta sub-fuser tuning failed: ", e$message)
            NULL
          })

          tuned_metrics <- tryCatch(tune::collect_metrics(tuned), error = function(e) NULL)

          if (!is.null(tuned) && !is.null(tuned_metrics) && nrow(tuned_metrics) > 0) {
            best <- tryCatch(tune::select_best(tuned, metric = "rmse"), error = function(e) NULL)
            if (!is.null(best)) {
              wf2 <- tune::finalize_workflow(wf, best)
              fit <- tryCatch(parsnip::fit(wf2, df_tr), error = function(e) NULL)
            }
          }
        }

        if (is.null(fit)) {
          fit <- tryCatch(parsnip::fit(wf, df_tr), error = function(e) NULL)
        }

        if (is.null(fit)) {
          .msg(quiet, verbose, "Meta sub-fuser fit failed. Falling back to 'median'.")
          fused <- median_fused()
          method_used <- "median"
        } else {
          fused <- dat |>
            dplyr::mutate(pred_fused = predict(fit, new_data = dat)$.pred)
        }
      }
    }
  }

  if (isTRUE(target_positive)) {
    fused <- dplyr::mutate(fused, pred_fused = pmax(.data$pred_fused, 0))
  }

  fused <- dplyr::full_join(tibble::tibble(YYYY = dates_all), fused, by = "YYYY") |>
    dplyr::arrange(.data$YYYY)

  list(
    fused = fused,
    leaderboard_products = dplyr::arrange(
      leaderboard,
      dplyr::desc(.data$weight),
      dplyr::desc(.data$score)
    ),
    all_results = results,
    product_fusion_method = method_used
  )
}


#' Fuse predictions across products (top-K), with optional sub-fuser
#' @keywords internal
fuse_products_predictions_ <- function(
    results,
    dates_all,
    topK = 3,
    min_score = 0.2,                 # e.g., min KGE
    prediction_years = NULL,         # YYYY or YYYYMMDD (len 2)
    use_sub_fuser = FALSE,
    sub_fuser = "rf",
    sub_grid_levels = 5,
    min_data_required = 10,
    target_positive = TRUE,
    quiet = TRUE,
    verbose = TRUE,
    seed = 123,
    ...
) {
  set.seed(seed)

  # Keep only products with usable preds
  results <- purrr::keep(results, ~ !is.null(.x$preds) && nrow(.x$preds) > 0)
  if (length(results) == 0) {
    return(list(
      fused = tibble::tibble(YYYY = dates_all, pred_fused = NA_real_),
      leaderboard_products = tibble::tibble(),
      all_results = results
    ))
  }

  # Build leaderboard (standard: product, score)
  lb <- tibble::tibble(
    product = purrr::map_chr(results, "product"),
    score   = purrr::map_dbl(results, "score")
  ) |>
    dplyr::mutate(score = ifelse(is.finite(.data$score), .data$score, NA_real_))

  # select topK by score
  lb_ok <- dplyr::filter(lb, is.finite(.data$score)) |>
    dplyr::arrange(dplyr::desc(.data$score))

  if (nrow(lb_ok) == 0) {
    return(list(
      fused = tibble::tibble(YYYY = dates_all, pred_fused = NA_real_),
      leaderboard_products = dplyr::mutate(lb, weight = 0),
      all_results = results
    ))
  }

  keep_names <- head(lb_ok$product, n = min(topK, nrow(lb_ok)))
  results_top <- results[match(keep_names, purrr::map_chr(results, "product"))]

  scores_keep <- lb_ok$score[match(keep_names, lb_ok$product)]
  w <- pmax(scores_keep, min_score)
  #w[scores_keep < min_score] <- 0

  if (all(w == 0)) {
    return(list(
      fused = tibble::tibble(YYYY = dates_all, pred_fused = NA_real_),
      leaderboard_products = dplyr::mutate(lb, weight = 0),
      all_results = results
    ))
  }
  w <- w / sum(w)

  leaderboard <- dplyr::mutate(lb, weight = 0)
  leaderboard$weight[match(keep_names, leaderboard$product)] <- w

  # Build preds_long
  preds_long <- purrr::map2_dfr(results_top, seq_along(results_top), ~{
    dplyr::transmute(
      .x$preds,
      YYYY    = .ensure_yyyymmdd(.data$YYYY),
      pred    = .data$pred,
      product = .x$product,
      w       = w[.y]
    )
  })

  # Observed Q timeline if present
  obs <- purrr::map_dfr(results_top, ~{
    if ("Q" %in% names(.x$preds)) {
      dplyr::transmute(.x$preds, YYYY = .ensure_yyyymmdd(.data$YYYY), Q = .data$Q)
    } else {
      dplyr::transmute(.x$preds, YYYY = .ensure_yyyymmdd(.data$YYYY)) |>
        dplyr::mutate(Q = NA_real_)
    }
  }) |>
    dplyr::distinct(.data$YYYY, .keep_all = TRUE)

  # Bounds (YYYYMMDD)
  bounds <- .ensure_year_bounds(prediction_years)

  # Simple fuser
  simple_fused <- function() {
    fuse_topk(preds_long) |>
      dplyr::full_join(obs, by = "YYYY")
  }

  fused <- NULL

  if (!isTRUE(use_sub_fuser) || length(results_top) < 2) {
    fused <- simple_fused()
  } else {
    # need observed Q to train sub-fuser
    if (all(is.na(obs$Q))) {
      fused <- simple_fused()
    } else {
      # wide matrix of product preds
      lst_wide <- purrr::map(results_top, ~{
        dplyr::transmute(.x$preds, YYYY = .ensure_yyyymmdd(.data$YYYY), pred = .data$pred) |>
          dplyr::rename(!!.x$product := pred)
      })
      prods_wide <- Reduce(function(a, b) dplyr::full_join(a, b, by = "YYYY"), lst_wide)

      dat <- dplyr::left_join(obs, prods_wide, by = "YYYY") |>
        dplyr::arrange(.data$YYYY)

      df_tr <- dat
      if (!is.null(bounds)) {
        df_tr <- dplyr::filter(dat, !(.data$YYYY >= bounds[1] & .data$YYYY <= bounds[2]))
      }

      if (nrow(df_tr) < min_data_required) {
        fused <- simple_fused()
      } else {
        # meta learner
        rec <- recipes::recipe(Q ~ ., data = df_tr) |>
          recipes::update_role(YYYY, new_role = "id") |>
          recipes::step_rm(YYYY) |>
          recipes::step_zv(recipes::all_predictors()) |>
          recipes::step_impute_median(recipes::all_predictors()) %>%
          recipes::step_normalize(recipes::all_predictors())

        spec <- model_spec(sub_fuser)
        pred_cols <- setdiff(names(df_tr), c("YYYY", "Q"))
        grid_sub <- model_grid(sub_fuser, p = length(pred_cols),
                               levels = sub_grid_levels)

        wf <- workflows::workflow() |>
          workflows::add_recipe(rec) |>
          workflows::add_model(spec)

        rset <- tryCatch({
          make_rolling(df_tr, year_col = "YYYY", init_frac = 0.8, assess_frac = 0.2,
                       n_splits = min(3, nrow(df_tr) - 1), cumulative = TRUE, quiet = TRUE)
        }, error = function(e) NULL)

        fit <- NULL
        if (!is.null(rset) && length(rset$splits) >= 1) {
          ctrl <- tune::control_grid(save_pred = TRUE, verbose = FALSE, allow_par = TRUE)
          tuned <- tryCatch({
            suppressWarnings(tune::tune_grid(wf, resamples = rset, grid = grid_sub,
                                             metrics = yardstick::metric_set(yardstick::rmse),
                                             control = ctrl))
          }, error = function(e) NULL)

          if (!is.null(tuned) && nrow(tune::collect_metrics(tuned)) > 0) {
            best <- tune::select_best(tuned, metric = "rmse")
            wf2  <- tune::finalize_workflow(wf, best)
            fit  <- tryCatch(parsnip::fit(wf2, df_tr), error = function(e) NULL)
          }
        }
        if (is.null(fit)) {
          fit <- tryCatch(parsnip::fit(wf, df_tr), error = function(e) NULL)
        }

        if (is.null(fit)) {
          fused <- simple_fused()
        } else {
          fused <- dat |>
            dplyr::mutate(pred_fused = predict(fit, new_data = dat)$.pred)
        }
      }
    }
  }

  if (isTRUE(target_positive)) {
    fused <- dplyr::mutate(fused, pred_fused = pmax(.data$pred_fused, 0))
  }

  fused <- dplyr::full_join(tibble::tibble(YYYY = dates_all), fused, by = "YYYY") |>
    dplyr::arrange(.data$YYYY)

  list(
    fused = fused,
    leaderboard_products = dplyr::arrange(leaderboard, dplyr::desc(.data$weight), dplyr::desc(.data$score)),
    all_results = results
  )
}


#' Weighted fusion of predictions by year
#'
#' @param preds_long Tibble with columns YYYY, pred, w (and optionally product/model).
#' @return Tibble (YYYY, pred_fused).
#' @keywords internal
fuse_topk <- function(preds_long) {
  stopifnot(all(c("YYYY", "pred", "w") %in% names(preds_long)))

  preds_long |>
    dplyr::mutate(
      YYYY = .ensure_yyyymmdd(.data$YYYY),
      pred = as.numeric(.data$pred),
      w    = as.numeric(.data$w)
    ) |>
    dplyr::group_by(.data$YYYY) |>
    dplyr::summarise(
      pred_fused = {
        ok <- is.finite(.data$pred) & is.finite(.data$w) & !is.na(.data$pred) & !is.na(.data$w)
        if (!any(ok)) {
          NA_real_
        } else {
          ww <- .data$w[ok]
          pp <- .data$pred[ok]
          sw <- sum(ww, na.rm = TRUE)
          if (!is.finite(sw) || sw <= 0) NA_real_ else sum(ww * pp, na.rm = TRUE) / sw
        }
      },
      .groups = "drop"
    )
}



# fuse_topk <- function(preds_long) {
#   stopifnot(all(c("YYYY","pred","w") %in% names(preds_long)))
#   preds_long |>
#     dplyr::group_by(YYYY) |>
#     dplyr::summarise(
#       pred_fused = mean(pred, na.rm = TRUE) ,
#       .groups = "drop"
#     )
# }
#' Full-join a list of (YYYY, pred) tibbles with renaming
#'
#' @param lst Named list of tibbles. Each must have columns YYYY, pred.
#' @return Tibble with YYYY and one column per element name.
#' @keywords internal
safe_full_join_preds <- function(lst) {
  lst <- purrr::compact(lst)
  if (length(lst) == 0L) return(tibble::tibble())

  lst2 <- purrr::imap(lst, function(.x, .nm) {
    stopifnot(all(c("YYYY", "pred") %in% names(.x)))
    .x |>
      dplyr::mutate(YYYY = .ensure_yyyymmdd(.data$YYYY)) |>
      dplyr::group_by(.data$YYYY) |>
      dplyr::summarise(pred = mean(.data$pred, na.rm = TRUE), .groups = "drop") |>
      dplyr::rename(!!.nm := pred)
  })

  Reduce(function(a, b) dplyr::full_join(a, b, by = "YYYY"), lst2)
}

# safe_full_join_preds <- function(lst) {
#   lst <- purrr::compact(lst)
#   if (length(lst) == 0L) return(tibble::tibble())
#   lst2 <- purrr::imap(lst, ~ dplyr::rename(.x, !!.y := pred))
#   Reduce(function(a, b) dplyr::full_join(a, b, by = "YYYY"), lst2)
# }


#' Extract (YYYY, Q) for a basin from the best available product
#'
#' Picks the product with the largest number of non-missing Q values for the basin.
#'
#' @param data_by_product Named list of data frames (per product).
#' @param basin_id Basin identifier value.
#' @param basin_col Name of the basin ID column.
#' @return Tibble (YYYY, Q) or empty tibble if not found.
#' @keywords internal
get_any_Q <- function(data_by_product, basin_id, basin_col = "HYBAS_ID") {

  candidates <- purrr::imap(data_by_product, function(df, p) {
    if (!is.data.frame(df)) return(NULL)
    if (!all(c(basin_col, "YYYY", "Q") %in% names(df))) return(NULL)

    tmp <- df |>
      dplyr::filter(.data[[basin_col]] == basin_id) |>
      dplyr::select(YYYY, Q) |>
      dplyr::mutate(
        YYYY = .ensure_yyyymmdd(.data$YYYY),
        Q    = suppressWarnings(as.numeric(.data$Q))
      ) |>
      dplyr::group_by(.data$YYYY) |>
      dplyr::summarise(Q = mean(.data$Q, na.rm = TRUE), .groups = "drop")

    if (nrow(tmp) == 0) return(NULL)

    n_ok <- sum(!is.na(tmp$Q))
    list(product = p, n_ok = n_ok, data = tmp)
  }) |>
    purrr::compact()

  if (length(candidates) == 0) return(tibble::tibble())

  # Choose the product with the most non-missing Q
  best_idx <- which.max(purrr::map_dbl(candidates, "n_ok"))
  candidates[[best_idx]]$data
}

# ---------------------------
# Helpers (internal)
# ---------------------------
#' Build a standardized empty return object for consolidation functions
#'
#' @param dates_all Integer vector of YYYYMMDD dates to keep in output.
#' @param leaderboard Optional tibble to use as leaderboard (will be completed).
#' @param results List of per-product results.
#' @param extra_cols Optional named list defining extra leaderboard columns and their empty types.
#' @keywords internal
.empty_return <- function(
    dates_all = integer(),
    leaderboard = NULL,
    results = list(),
    extra_cols = NULL
) {

  # --- default leaderboard schema (minimum contract) ---
  lb <- tibble::tibble(
    product = character(),
    kge     = numeric(),
    weight  = numeric()
  )

  # --- add extra columns if requested (e.g. stat) ---
  if (!is.null(extra_cols) && length(extra_cols) > 0) {
    for (nm in names(extra_cols)) {
      lb[[nm]] <- extra_cols[[nm]]
    }
  }

  # --- if a leaderboard is provided, coerce/complete it ---
  if (!is.null(leaderboard)) {
    # ensure minimum cols exist
    if (!"product" %in% names(leaderboard)) leaderboard$product <- character()
    if (!"kge" %in% names(leaderboard)) leaderboard$kge <- numeric()
    if (!"weight" %in% names(leaderboard)) leaderboard$weight <- 0

    # ensure extra cols exist
    if (!is.null(extra_cols) && length(extra_cols) > 0) {
      for (nm in names(extra_cols)) {
        if (!nm %in% names(leaderboard)) leaderboard[[nm]] <- extra_cols[[nm]]
      }
    }

    lb <- leaderboard
  }

  # --- fused (always stable) ---
  fused <- tibble::tibble(
    YYYY = dates_all,
    pred_fused = NA_real_
  )

  # if weight exists, sort deterministically (useful for printing & tests)
  if ("weight" %in% names(lb) && "kge" %in% names(lb)) {
    lb <- lb %>% dplyr::arrange(dplyr::desc(.data$weight), dplyr::desc(.data$kge))
  }

  list(
    fused = fused,
    leaderboard_products = lb,
    all_results = results
  )
}


#' Lightweight messaging helper
#' @keywords internal
.msg <- function(quiet, verbose, ...) {
  if (!isTRUE(quiet) && isTRUE(verbose)) message(...)
}

.ensure_year_bounds <- function(yrs) {

  if (is.null(yrs)) return(NULL)

  # --- validation de base ---
  if (!is.numeric(yrs) || length(yrs) != 2 || anyNA(yrs)) {
    stop(
      "prediction_years must be a numeric vector of length 2 ",
      "(YYYY or YYYYMMDD).",
      call. = FALSE
    )
  }

  yrs <- sort(as.integer(yrs))

  # helpers
  is_year <- function(x) x >= 1900 & x <= 2500
  is_yyyymmdd <- function(x) x >= 19000101 & x <= 25001231

  if (!all(is_year(yrs) | is_yyyymmdd(yrs))) {
    stop(
      "prediction_years values must be either YYYY or YYYYMMDD.",
      call. = FALSE
    )
  }

  # --- conversion vers bornes YYYYMMDD ---
  lo <- if (is_year(yrs[1])) {
    yrs[1] * 10000L + 101L
  } else {
    yrs[1]
  }

  hi <- if (is_year(yrs[2])) {
    yrs[2] * 10000L + 1231L
  } else {
    yrs[2]
  }

  if (lo > hi) {
    stop("prediction_years start must be <= end.", call. = FALSE)
  }

  c(lo, hi)
}


.get_pattern <- function(p) {
  if (!is.null(pred_pattern_by_product) && p %in% names(pred_pattern_by_product)) {
    pred_pattern_by_product[[p]]
  } else if (!is.null(pred_pattern_by_product) && length(pred_pattern_by_product) == 1) {
    pred_pattern_by_product
  } else {
    "^pt_"
  }
}
.weighted_mean_na <- function(pred, w) {
  ok <- is.finite(pred) & is.finite(w) & !is.na(pred) & !is.na(w)
  if (!any(ok)) return(NA_real_)
  ww <- w[ok]
  pp <- pred[ok]
  if (sum(ww) <= 0) return(NA_real_)
  sum(ww * pp, na.rm = TRUE) / sum(ww, na.rm = TRUE)
}
# .empty_return <- function(dates_all = integer(), results = list()) {
#   list(
#     fused = tibble::tibble(YYYY = dates_all, pred_fused = NA_real_),
#     leaderboard_products = tibble::tibble(
#       product = character(), kge = numeric(), weight = numeric()
#     ),
#     all_results = results
#   )
# }



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

#' Get predictor pattern for a product
#' @keywords internal
.get_pattern <- function(p,pred_pattern_by_product) {
  if (!is.null(pred_pattern_by_product) && p %in% names(pred_pattern_by_product)) {
    pred_pattern_by_product[[p]]
  } else if (!is.null(pred_pattern_by_product) && length(pred_pattern_by_product) == 1) {
    pred_pattern_by_product
  } else {
    "^pt_"
  }
}

# .weighted_mean_na <- function(pred, w) {
#   ok <- is.finite(pred) & is.finite(w) & !is.na(pred) & !is.na(w)
#   if (!any(ok)) return(NA_real_)
#   ww <- w[ok]
#   pp <- pred[ok]
#   if (sum(ww) <= 0) return(NA_real_)
#   sum(ww * pp, na.rm = TRUE) / sum(ww, na.rm = TRUE)
# }







#' Internal helper to compute KGE-based fusion weights
#'
#' @keywords internal
.wass2s_compute_kge_weights <- function(df, pred_cols, target = "Q") {
  if (!target %in% names(df)) {
    stop("Target column not found in data.", call. = FALSE)
  }

  if (length(pred_cols) == 0L) {
    return(stats::setNames(numeric(0), character(0)))
  }

  scores <- vapply(pred_cols, function(col) {
    wass2s_kge(df[[target]], df[[col]])
  }, numeric(1))

  scores_pos <- pmax(scores, 0)
  scores_pos[!is.finite(scores_pos)] <- 0

  if (sum(scores_pos) <= 0) {
    weights <- rep(1 / length(pred_cols), length(pred_cols))
  } else {
    weights <- scores_pos / sum(scores_pos)
  }

  stats::setNames(weights, pred_cols)
}


#' Internal helper to apply a weighted mean row-wise
#'
#' @keywords internal
.wass2s_apply_weighted_mean <- function(df, pred_cols, weights) {
  stopifnot(all(pred_cols %in% names(df)))
  stopifnot(all(pred_cols %in% names(weights)))

  x <- as.matrix(df[, pred_cols, drop = FALSE])
  w <- weights[pred_cols]

  pred <- apply(x, 1, function(row) {
    ok <- is.finite(row)
    if (!any(ok)) return(NA_real_)

    ww <- w[ok]
    if (sum(ww) <= 0) {
      ww <- rep(1 / sum(ok), sum(ok))
    } else {
      ww <- ww / sum(ww)
    }

    sum(row[ok] * ww)
  })

  as.numeric(pred)
}


#' Internal helper to apply a simple row-wise fusion
#'
#' @keywords internal
.wass2s_apply_simple_fusion <- function(df, pred_cols, method = c("mean", "median")) {
  method <- match.arg(method)

  if (length(pred_cols) == 0L) {
    return(rep(NA_real_, nrow(df)))
  }

  x <- as.matrix(df[, pred_cols, drop = FALSE])

  if (method == "mean") {
    out <- rowMeans(x, na.rm = TRUE)
    out[is.nan(out)] <- NA_real_
    return(as.numeric(out))
  }

  out <- apply(x, 1, function(row) {
    if (all(!is.finite(row))) return(NA_real_)
    stats::median(row[is.finite(row)], na.rm = TRUE)
  })

  as.numeric(out)
}


#' Internal helper to split fusion data into training and testing sets
#'
#' @keywords internal
.wass2s_split_fusion_data <- function(fused_models, prediction_years = NULL, date_col = "YYYY") {
  if (!date_col %in% names(fused_models)) {
    stop("Date column not found in `fused_models`.", call. = FALSE)
  }

  if (!is.null(prediction_years)) {
    bounds <- .pred_years_to_bounds(prediction_years)
    df_te <- dplyr::filter(fused_models, .data[[date_col]] >= bounds[1], .data[[date_col]] <= bounds[2])
    df_tr <- dplyr::filter(fused_models, !(.data[[date_col]] >= bounds[1] & .data[[date_col]] <= bounds[2]))
  } else {
    if (nrow(fused_models) < 2L) {
      stop("Need at least 2 rows to split into training/testing.", call. = FALSE)
    }
    idx <- seq_len(nrow(fused_models) - 1L)
    df_tr <- fused_models[idx, , drop = FALSE]
    df_te <- fused_models[-idx, , drop = FALSE]
  }

  list(train = df_tr, test = df_te)
}


#' Internal helper to score fusion outputs
#'
#' @keywords internal
.wass2s_score_fusion <- function(df, basin_id, target = "Q", pred_col = "pred_final") {
  df_ok <- df %>%
    dplyr::filter(is.finite(.data[[target]]), is.finite(.data[[pred_col]]))

  tibble::tibble(
    HYBAS_ID = basin_id,
    kge = if (nrow(df_ok) > 0) wass2s_kge(df_ok[[target]], df_ok[[pred_col]]) else NA_real_,
    rmse = if (nrow(df_ok) > 0) yardstick::rmse_vec(df_ok[[target]], df_ok[[pred_col]]) else NA_real_
  )
}


#' Internal helper to run meta-fusion
#'
#' @keywords internal
.wass2s_run_meta_fuser <- function(
    df_tr,
    df_all,
    basin_id,
    target = "Q",
    date_col = "YYYY",
    final_fuser = "rf",
    grid_levels = 5,
    quiet = TRUE,
    verbose_tune = TRUE,
    allow_par = TRUE,
    target_positive = FALSE
) {
  pred_cols <- setdiff(names(df_tr), c(target, date_col))
  if (length(pred_cols) < 1L) {
    stop("No meta-features available for meta-fusion.", call. = FALSE)
  }


  if (!quiet) {
    message("Meta-fuser: final_fuser = ", final_fuser)
    message("Meta-fuser: nrow(df_tr) = ", nrow(df_tr))
    message("Meta-fuser: nrow(df_all) = ", nrow(df_all))
    message("Meta-fuser: pred_cols = ", paste(pred_cols, collapse = ", "))
    message("Meta-fuser: classes = ", paste(vapply(df_tr[, pred_cols, drop = FALSE], class, character(1)), collapse = ", "))
  }
  if (!quiet) {
    #print(utils::head(df_tr))
  }


  # Keep only numeric predictors
  pred_cols <- pred_cols[vapply(df_tr[, pred_cols, drop = FALSE], is.numeric, logical(1))]

  if (length(pred_cols) < 1L) {
    stop("No numeric meta-features available for meta-fusion.", call. = FALSE)
  }

  form <- stats::reformulate(termlabels = pred_cols, response = target)

  rec_meta <- recipes::recipe(form, data = df_tr) |>
    recipes::step_zv(recipes::all_predictors()) |>
    recipes::step_impute_median(recipes::all_numeric_predictors())

  spec <- model_spec(final_fuser, p = length(pred_cols))
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
      n_splits = min(3, nrow(df_tr) - 1L),
      quiet = TRUE
    )
  }, error = function(e) {
    if (!quiet) message("Error creating resamples: ", e$message)
    NULL
  })

  rs <- NULL
  best <- NULL
  fit_fin <- NULL
  metrics_cv <- NULL

  if (!is.null(rset)) {
    ctrl <- tune::control_grid(
      save_pred = TRUE,
      verbose = verbose_tune,
      allow_par = allow_par
    )

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
  }

  has_valid_metrics <- FALSE
  if (!is.null(rs)) {
    metrics_cv <- tryCatch(tune::collect_metrics(rs), error = function(e) NULL)
    has_valid_metrics <- !is.null(metrics_cv) && nrow(metrics_cv) > 0
  }

  if (!has_valid_metrics) {
    return(list(
      success = FALSE,
      fitted = NULL,
      pred_all = NULL,
      cv_rs = NULL,
      best_params = NULL
    ))
  }

  best <- tune::select_best(rs, metric = "rmse")
  wf_fin <- tune::finalize_workflow(wf_meta, best)

  fit_fin <- tryCatch({
    parsnip::fit(wf_fin, data = df_tr)
  }, error = function(e) {
    if (!quiet) message("Error fitting finalized meta-learner: ", e$message)
    NULL
  })

  if (is.null(fit_fin)) {
    return(list(
      success = FALSE,
      fitted = NULL,
      pred_all = NULL,
      cv_rs = metrics_cv,
      best_params = best
    ))
  }

  pred_all <- predict(fit_fin, new_data = df_all)$.pred
  if (isTRUE(target_positive)) {
    pred_all <- pmax(pred_all, 0)
  }

  list(
    success = TRUE,
    fitted = fit_fin,
    pred_all = pred_all,
    cv_rs = metrics_cv,
    best_params = best
  )
}


#' Internal unified fusion engine
#'
#' @keywords internal
.wass2s_fuse_predictions <- function(
    fused_models,
    basin_id,
    target = "Q",
    date_col = "YYYY",
    prediction_years = NULL,
    fusion_method = c("meta", "mean", "median", "weighted_mean"),
    final_fuser = "rf",
    grid_levels = 5,
    quiet = TRUE,
    verbose_tune = TRUE,
    allow_par = TRUE,
    target_positive = FALSE
) {
  fusion_method <- match.arg(fusion_method)

  if (!is.data.frame(fused_models)) {
    stop("`fused_models` must be a data.frame.", call. = FALSE)
  }
  if (!all(c(date_col, target) %in% names(fused_models))) {
    stop("`fused_models` must contain date and target columns.", call. = FALSE)
  }

  fused_models <- fused_models %>%
    dplyr::arrange(.data[[date_col]])

  pred_cols <- setdiff(names(fused_models), c(target, date_col))

  if (length(pred_cols) == 0L) {
    out <- fused_models %>%
      dplyr::mutate(pred_final = NA_real_)

    return(list(
      fused_by_model = out,
      final_test = dplyr::slice_tail(out, n = 1),
      scores_train = .wass2s_score_fusion(out[0, , drop = FALSE], basin_id, target = target),
      scores_test = .wass2s_score_fusion(out, basin_id, target = target),
      scores = dplyr::bind_rows(
        .wass2s_score_fusion(out[0, , drop = FALSE], basin_id, target = target) %>% dplyr::mutate(split = "train"),
        .wass2s_score_fusion(out, basin_id, target = target) %>% dplyr::mutate(split = "test")
      ),
      fusion_method = fusion_method,
      fusion_weights = NULL,
      cv_rs = NULL,
      best_meta_params = NULL
    ))
  }

  split_obj <- .wass2s_split_fusion_data(
    fused_models = fused_models,
    prediction_years = prediction_years,
    date_col = date_col
  )

  df_tr <- split_obj$train
  df_te <- split_obj$test

  too_short <- nrow(df_tr) < 5L
  constant_cols <- vapply(df_tr[, pred_cols, drop = FALSE], function(z) {
    s <- stats::sd(z, na.rm = TRUE)
    is.na(s) || s < 1e-12
  }, logical(1))
  all_constant <- all(constant_cols)

  # Fallback to mean if meta cannot reasonably run
  if (fusion_method == "meta" && (too_short || all_constant)) {
    if (!quiet) {
      message("Meta-fusion fallback to mean: insufficient training information.")
    }
    fusion_method <- "mean"
  }

  weights <- NULL
  cv_rs <- NULL
  best_meta_params <- NULL

  if (fusion_method == "mean") {
    fused_models$pred_final <- .wass2s_apply_simple_fusion(fused_models, pred_cols, method = "mean")
  } else if (fusion_method == "median") {
    fused_models$pred_final <- .wass2s_apply_simple_fusion(fused_models, pred_cols, method = "median")
  } else if (fusion_method == "weighted_mean") {
    weights <- .wass2s_compute_kge_weights(df_tr, pred_cols, target = target)
    fused_models$pred_final <- .wass2s_apply_weighted_mean(fused_models, pred_cols, weights)
  } else if (fusion_method == "meta") {
    meta_res <- .wass2s_run_meta_fuser(
      df_tr = df_tr,
      df_all = fused_models,
      basin_id = basin_id,
      target = target,
      date_col = date_col,
      final_fuser = final_fuser,
      grid_levels = grid_levels,
      quiet = quiet,
      verbose_tune = verbose_tune,
      allow_par = allow_par,
      target_positive = target_positive
    )

    if (!isTRUE(meta_res$success)) {
      if (!quiet) {
        message("Meta-fusion failed, fallback to mean.")
      }
      fusion_method <- "mean"
      fused_models$pred_final <- .wass2s_apply_simple_fusion(fused_models, pred_cols, method = "mean")
    } else {
      fused_models$pred_final <- meta_res$pred_all
      cv_rs <- meta_res$cv_rs
      best_meta_params <- meta_res$best_params
    }
  }

  if (isTRUE(target_positive)) {
    fused_models$pred_final <- pmax(fused_models$pred_final, 0)
  }

  train_scores <- .wass2s_score_fusion(
    fused_models %>% dplyr::filter(.data[[date_col]] %in% df_tr[[date_col]]),
    basin_id = basin_id,
    target = target
  )

  test_scores <- .wass2s_score_fusion(
    fused_models %>% dplyr::filter(.data[[date_col]] %in% df_te[[date_col]]),
    basin_id = basin_id,
    target = target
  )

  scores <- dplyr::bind_rows(
    train_scores %>% dplyr::mutate(split = "train"),
    test_scores %>% dplyr::mutate(split = "test")
  )

  list(
    fused_by_model = fused_models,
    final_test = dplyr::slice_tail(fused_models, n = 1),
    scores_train = train_scores,
    scores_test = test_scores,
    scores = scores,
    fusion_method = fusion_method,
    fusion_weights = weights,
    cv_rs = cv_rs,
    best_meta_params = best_meta_params
  )
}

