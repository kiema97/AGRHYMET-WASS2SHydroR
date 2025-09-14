#' Class thresholds from hydrological climatology (quartiles by default)
#'
#' Compute class thresholds from a historical discharge series. By default
#' uses quartiles: t1 = Q1 (25%), t2 = Q3 (75%), so classes are:
#' below: Q < Q1; normal: Q1 <= Q <= Q3; above: Q > Q3.
#'
#' @param q_hist numeric vector of historical discharges.
#' @param probs length-2 numeric of cumulative probs for thresholds
#'   (default c(0.25, 0.75) for Q1/Q3).
#' @param na.rm logical; remove NAs.
#' @return named numeric c(t1, t2).
#' @keywords internal
wass2s_class_thr <- function(q_hist, probs = c(0.25, 0.75), na.rm = TRUE) {
  stopifnot(length(probs) == 2)
  qs <- stats::quantile(q_hist, probs = probs, na.rm = na.rm, names = FALSE)
  stats::setNames(qs, c("t1", "t2"))
}

#' Class probabilities under Normal assumption (Q1/Q3 classes by default)
#'
#' Given Normal(mean = mu, sd = sigma) and thresholds (t1=Q1, t2=Q3),
#' returns P(below), P(normal), P(above) with:
#' below: X < t1; normal: t1 <= X <= t2; above: X > t2.
#'
#' @param mu numeric vector of predictive means.
#' @param sigma numeric vector of predictive std devs (>0).
#' @param thresholds named numeric with elements t1, t2.
#' @return tibble with columns p_below, p_normal, p_above.
#' @keywords internal
wass2s_class_probs_norm <- function(mu, sigma, thresholds) {
  stopifnot(all(c("t1","t2") %in% names(thresholds)))
  t1 <- thresholds[["t1"]]; t2 <- thresholds[["t2"]]
  sigma <- pmax(as.numeric(sigma), .Machine$double.eps)
  p_below  <- stats::pnorm(t1, mean = mu, sd = sigma)
  p_above  <- 1 - stats::pnorm(t2, mean = mu, sd = sigma)
  p_normal <- pmax(0, 1 - p_below - p_above)
  tibble::tibble(p_below = p_below, p_normal = p_normal, p_above = p_above)
}

#' Entropy of class probabilities (uncertainty indicator)
#' @keywords internal

wass2s_class_entropy <- function(p_below, p_normal, p_above) {
  p_raw <- cbind(p_below, p_normal, p_above)   # garde les scalaires en matrice n x 3

  # cas exacts : une classe = 1 et les deux autres = 0  -> entropie = 0
  is_one_hot <- rowSums(p_raw == 1, na.rm = TRUE) == 1 & rowSums(p_raw == 0, na.rm = TRUE) == 2
  out <- numeric(nrow(p_raw))
  out[is_one_hot] <- 0

  # pour le reste, on sécurise et on calcule
  if (any(!is_one_hot)) {
    p <- p_raw
    p[!is.finite(p)] <- 0
    p[p < 1e-12] <- 1e-12
    rs <- rowSums(p)
    rs[!is.finite(rs) | rs <= 0] <- 1
    p <- p / rs
    out[!is_one_hot] <- -rowSums(p[!is_one_hot, , drop = FALSE] * log(p[!is_one_hot, , drop = FALSE]))
  }
  as.numeric(out)
}



#' From forecast to class probabilities (Q1/Q3 classes by default)
#'
#' Vectorized wrapper: for a series with columns YYYY and pred (forecast mean),
#' compute class probabilities using Normal assumption and Q1/Q3 thresholds.
#'
#' @param df data frame with columns YYYY, pred.
#' @param q_hist numeric historical Q (climatology for thresholds & sigma fallback).
#' @param sigma optional numeric vector (same length as pred) of predictive sd.
#' @param rmse optional numeric; if sigma is NULL, use this constant sd.
#' @param residuals optional numeric residuals to estimate sd; ignored if sigma provided.
#' @param thresholds optional named numeric c(t1, t2); if NULL, computed from q_hist.
#' @param min_sigma_frac minimal sigma as fraction of sd(q_hist) (default 0.05).
#' @return tibble: YYYY, pred, p_below, p_normal, p_above, class_hat, entropy.
#' @export
wass2s_class_from_forecast <- function(
    df, q_hist, sigma = NULL, rmse = NULL, residuals = NULL,
    thresholds = NULL, min_sigma_frac = 0.05
){
  stopifnot(all(c("YYYY","pred") %in% names(df)))
  df <- dplyr::arrange(df, .data$YYYY)

  if (is.null(thresholds)) thresholds <- wass2s_class_thr(q_hist, probs = c(0.25, 0.75))

  if (is.null(sigma)) {
    if (!is.null(residuals)) {
      s <- stats::sd(residuals, na.rm = TRUE)
    } else if (!is.null(rmse)) {
      s <- as.numeric(rmse)
    } else {
      s <- 0.5 * stats::sd(q_hist, na.rm = TRUE)  # fallback prudent
    }
    s_min <- min_sigma_frac * stats::sd(q_hist, na.rm = TRUE)
    sigma <- pmax(s, s_min)
  }
  if (length(sigma) == 1L) sigma <- rep(sigma, nrow(df))

  P <- wass2s_class_probs_norm(mu = df$pred, sigma = sigma, thresholds = thresholds)
  labs <- c("below","normal","above")
  class_hat <- labs[max.col(as.matrix(P), ties.method = "first")]
  entropy <- wass2s_class_entropy(P$p_below, P$p_normal, P$p_above)

  dplyr::bind_cols(df, P) |>
    dplyr::mutate(class_hat = class_hat, entropy = entropy)
}
