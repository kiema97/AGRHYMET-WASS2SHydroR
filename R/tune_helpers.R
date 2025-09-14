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
