#' Fuse predictions across products (top-K), with optional sub-fuser
#' @keywords internal
fuse_products_predictions <- function(
    results,
    dates_all,
    topK = 3,
    min_score = 0.2,                 # e.g., min KGE
    prediction_years = NULL,         # YYYY or YYYYMMDD (len 2)
    use_sub_fuser = FALSE,
    sub_fuser = "rf",
    sub_grid_levels = 10,
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
  #w <- pmax(scores_keep, 0)
  w[scores_keep < min_score] <- 0

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
          dplyr::rename(!!.x$product := .data$pred)
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
        grid_sub <- model_grid(sub_fuser, p = length(pred_cols), levels = sub_grid_levels)

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
#       pred_fused = mean(w * pred, na.rm = TRUE) ,
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
      dplyr::rename(!!.nm := .data$pred)
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



