#' Leaderboard by cross-validation configuration
#'
#' @param tuned A tune_grid() result or object accepted by tune::collect_predictions.
#' @param truth_col Name of the truth column in collect_predictions() (default "Q").
#' @param estimate_col Name of the prediction column in collect_predictions() (default ".pred").
#' @return Tibble with columns .config, kge_mean, rmse_mean, mae_mean and n_splits.
#' @keywords internal
compute_leaderboard_cv <- function(tuned, truth_col = "Q", estimate_col = ".pred") {
  empty <- tibble::tibble(
    .config = character(),
    kge_mean = numeric(),
    rmse_mean = numeric(),
    mae_mean = numeric(),
    n_splits = integer()
  )

  preds <- tune::collect_predictions(tuned)
  if (!all(c(truth_col, estimate_col, ".config", "id") %in% names(preds))) {
    return(empty)
  }

  preds <- dplyr::filter(
    preds,
    is.finite(.data[[truth_col]]),
    is.finite(.data[[estimate_col]])
  )
  if (nrow(preds) == 0L) return(empty)

  preds |>
    dplyr::group_by(.data$.config, .data$id) |>
    dplyr::summarise(
      kge = kge_vec(.data[[truth_col]], .data[[estimate_col]]),
      rmse = yardstick::rmse_vec(.data[[truth_col]], .data[[estimate_col]]),
      mae = yardstick::mae_vec(.data[[truth_col]], .data[[estimate_col]]),
      .groups = "drop"
    ) |>
    dplyr::group_by(.data$.config) |>
    dplyr::summarise(
      kge_mean = mean(.data$kge, na.rm = TRUE),
      rmse_mean = mean(.data$rmse, na.rm = TRUE),
      mae_mean = mean(.data$mae, na.rm = TRUE),
      n_splits = sum(is.finite(.data$kge) | is.finite(.data$rmse)),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      kge_mean = ifelse(is.nan(.data$kge_mean), NA_real_, .data$kge_mean),
      rmse_mean = ifelse(is.nan(.data$rmse_mean), NA_real_, .data$rmse_mean),
      mae_mean = ifelse(is.nan(.data$mae_mean), NA_real_, .data$mae_mean)
    ) |>
    dplyr::arrange(dplyr::desc(.data$kge_mean), .data$rmse_mean)
}

#' Select tuned parameters with an explicit scientific score contract
#'
#' @param tuned A tune_grid() result.
#' @param leaderboard Output of compute_leaderboard_cv().
#' @param selection_metric Either "rmse" or "kge".
#' @param quiet Suppress diagnostic messages.
#' @return List with params, selected_config and selected_score.
#' @keywords internal
.wass2s_select_tuned_config <- function(
    tuned,
    leaderboard,
    selection_metric = c("rmse", "kge"),
    quiet = TRUE
) {
  selection_metric <- match.arg(selection_metric)
  empty_score <- tibble::tibble(
    .config = NA_character_,
    kge_mean = NA_real_,
    rmse_mean = NA_real_,
    mae_mean = NA_real_,
    n_splits = NA_integer_
  )

  best_params <- tryCatch({
    if (identical(selection_metric, "kge") &&
        nrow(leaderboard) > 0L &&
        any(is.finite(leaderboard$kge_mean))) {
      best_cfg <- leaderboard$.config[which.max(leaderboard$kge_mean)]
      cand <- tune::show_best(tuned, metric = "rmse", n = Inf)
      best_row <- dplyr::filter(cand, .data$.config == best_cfg)
      if (nrow(best_row) == 0L) tune::select_best(tuned, metric = "rmse") else dplyr::slice(best_row, 1)
    } else {
      tune::select_best(tuned, metric = "rmse")
    }
  }, error = function(e) {
    if (!quiet) message("Error selecting tuned configuration: ", e$message)
    NULL
  })

  if (is.null(best_params)) {
    return(list(params = NULL, selected_config = NA_character_, selected_score = empty_score))
  }

  selected_config <- if (".config" %in% names(best_params)) best_params$.config[[1]] else NA_character_
  selected_score <- if (!is.na(selected_config) && nrow(leaderboard) > 0L) {
    out <- dplyr::filter(leaderboard, .data$.config == selected_config)
    if (nrow(out) == 0L) empty_score else dplyr::slice(out, 1)
  } else {
    empty_score
  }

  list(
    params = best_params,
    selected_config = selected_config,
    selected_score = selected_score
  )
}

#' Convert KGE scores into normalized weights
#'
#' @param kge Numeric vector of KGE values.
#' @return Numeric weights summing (approximately) to 1.
#' @keywords internal
weight_from_kge <- function(kge) {
  if (length(kge) == 0L) return(numeric())
  rng <- range(kge, finite = TRUE)
  if (any(!is.finite(rng)) || diff(rng) < 1e-12) {
    rep(1 / length(kge), length(kge))
  } else {
    w <- (kge - min(kge)) / (max(kge) - min(kge))
    if (sum(w) <= 0) rep(1 / length(kge), length(kge)) else w / sum(w)
  }
}

#' Ensure YYYY column is in YYYYMMDD integer format
#'
#' - If values look like YYYY (4 digits), converts to YYYY0101
#' - If values look like YYYYMMDD (8 digits), keeps as-is
#' - If Date/POSIXct, converts with format %Y%m%d
#'
#' @keywords internal
.ensure_yyyymmdd <- function(x) {

  # Date / POSIXct
  if (inherits(x, "Date") || inherits(x, "POSIXct") || inherits(x, "POSIXt")) {
    return(as.integer(format(as.Date(x), "%Y%m%d")))
  }

  # Character -> numeric if possible
  if (is.character(x)) {
    x_trim <- trimws(x)

    if (all(grepl("^\\d{4}$", x_trim, perl = TRUE) | is.na(x_trim))) {
      return(as.integer(paste0(x_trim, "0101")))
    }

    if (all(grepl("^\\d{8}$", x_trim, perl = TRUE) | is.na(x_trim))) {
      return(as.integer(x_trim))
    }

    # try to parse as Date-like string
    parsed <- suppressWarnings(as.Date(x_trim))
    if (!all(is.na(parsed))) {
      return(as.integer(format(parsed, "%Y%m%d")))
    }

    stop("YYYY/date column must be YYYY, YYYYMMDD, or coercible to Date.", call. = FALSE)
  }

  # Numeric / integer
  if (is.numeric(x)) {
    x_int <- as.integer(x)
    n_digits <- nchar(abs(x_int))

    # if all (non-NA) are 4 digits -> year
    if (all(n_digits[!is.na(n_digits)] == 4)) {
      return(as.integer(paste0(x_int, "0101")))
    }

    # if all (non-NA) are 8 digits -> yyyymmdd
    if (all(n_digits[!is.na(n_digits)] == 8)) {
      return(x_int)
    }

    stop("YYYY/date column numeric must be 4-digit (YYYY) or 8-digit (YYYYMMDD).", call. = FALSE)
  }

  stop("Unsupported YYYY/date column type.", call. = FALSE)
}
