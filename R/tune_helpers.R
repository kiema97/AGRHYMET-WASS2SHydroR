#' Leaderboard by KGE from collect_predictions()
#'
#' @param tuned A tune_grid() result or object accepted by tune::collect_predictions.
#' @param truth_col Name of the truth column in collect_predictions() (default "Q").
#' @return Tibble with columns .config and kge_mean (descending).
#' @keywords internal
compute_leaderboard_cv <- function(tuned, truth_col = "Q") {
  preds <- tune::collect_predictions(tuned)
  stopifnot(truth_col %in% names(preds))
  preds |>
    dplyr::group_by(.config, id) |>
    dplyr::summarise(kge = kge_vec(.data[[truth_col]], .pred),
                     .groups = "drop") |>
    dplyr::group_by(.config) |>
    dplyr::summarise(kge_mean = mean(kge, na.rm = TRUE), .groups = "drop") |>
    dplyr::arrange(dplyr::desc(kge_mean))
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
