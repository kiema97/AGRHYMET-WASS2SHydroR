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

#' Tercile thresholds from hydrological climatology
#'
#' Computes the two climatological tercile thresholds used to define
#' \code{below}, \code{normal}, and \code{above} hydrological forecast
#' categories.
#'
#' @param q_hist Numeric historical streamflow or hydrological predictand.
#' @param na.rm Logical. Remove missing values before computing quantiles.
#'
#' @return Named numeric vector \code{c(t1, t2)} where \code{t1} is the lower
#'   tercile and \code{t2} is the upper tercile.
#'
#' @examples
#' wass2s_tercile_thresholds(1:30)
#'
#' @export
wass2s_tercile_thresholds <- function(q_hist, na.rm = TRUE) {
  wass2s_class_thr(q_hist, probs = c(1 / 3, 2 / 3), na.rm = na.rm)
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

#' Class probabilities under Student's t assumption
#'
#' Computes \code{below}, \code{normal}, and \code{above} probabilities using a
#' location-scale Student distribution. This is useful for small samples, where
#' a normal predictive distribution can underestimate uncertainty.
#'
#' @param mu Numeric vector of predictive means.
#' @param sigma Numeric vector of predictive scales.
#' @param thresholds Named numeric vector with elements \code{t1} and \code{t2}.
#' @param df Degrees of freedom. Small values produce heavier tails.
#'
#' @return Tibble with columns \code{p_below}, \code{p_normal},
#'   \code{p_above}.
#'
#' @export
wass2s_class_probs_student <- function(mu, sigma, thresholds, df) {
  stopifnot(all(c("t1", "t2") %in% names(thresholds)))
  df <- as.numeric(df)[1]
  if (!is.finite(df) || df <= 0) {
    stop("df must be a positive finite number.", call. = FALSE)
  }
  t1 <- thresholds[["t1"]]
  t2 <- thresholds[["t2"]]
  sigma <- pmax(as.numeric(sigma), .Machine$double.eps)
  z1 <- (t1 - mu) / sigma
  z2 <- (t2 - mu) / sigma
  p_below <- stats::pt(z1, df = df)
  p_above <- 1 - stats::pt(z2, df = df)
  p_normal <- pmax(0, 1 - p_below - p_above)
  tibble::tibble(p_below = p_below, p_normal = p_normal, p_above = p_above)
}

#' Normalize below-normal-above probabilities
#'
#' Cleans negative and non-finite probabilities, then renormalizes each row to
#' sum to one. Invalid rows are filled with climatological tercile probabilities
#' by default.
#'
#' @param probs Data frame containing \code{p_below}, \code{p_normal},
#'   \code{p_above}.
#' @param fallback Numeric length-3 fallback probability vector.
#'
#' @return Tibble with normalized probability columns.
#'
#' @export
wass2s_normalize_probabilities <- function(probs, fallback = c(1 / 3, 1 / 3, 1 / 3)) {
  required <- c("p_below", "p_normal", "p_above")
  if (!is.data.frame(probs) || !all(required %in% names(probs))) {
    stop("probs must contain p_below, p_normal and p_above.", call. = FALSE)
  }
  fallback <- as.numeric(fallback)
  if (length(fallback) != 3L || any(!is.finite(fallback)) || any(fallback < 0) || sum(fallback) <= 0) {
    stop("fallback must be a non-negative numeric vector of length 3 with positive sum.", call. = FALSE)
  }
  fallback <- fallback / sum(fallback)

  p <- as.matrix(probs[, required, drop = FALSE])
  storage.mode(p) <- "numeric"
  p[!is.finite(p)] <- 0
  p[p < 0] <- 0
  rs <- rowSums(p)
  invalid <- !is.finite(rs) | rs <= 0
  if (any(!invalid)) p[!invalid, ] <- p[!invalid, , drop = FALSE] / rs[!invalid]
  if (any(invalid)) {
    p[invalid, ] <- matrix(fallback, nrow = sum(invalid), ncol = 3, byrow = TRUE)
  }
  tibble::as_tibble(p, .name_repair = "minimal") |>
    stats::setNames(required)
}


#' Compute entropy of class probabilities (uncertainty indicator)
#'
#' This function computes the Shannon entropy of class probabilities
#' (below, normal, above) to quantify forecast uncertainty.
#'
#' Entropy is minimal (0) when one class has probability 1 (deterministic forecast),
#' and maximal when probabilities are evenly distributed.
#'
#' @param p_below Numeric vector. Probability of the "below normal" class.
#' @param p_normal Numeric vector. Probability of the "normal" class.
#' @param p_above Numeric vector. Probability of the "above normal" class.
#' @param normalize Logical. If TRUE, entropy is normalized to [0, 1].
#'
#' @return Numeric vector of entropy values.
#'
#' @details
#' - Probabilities are automatically cleaned and normalized.
#' - Non-finite values are treated as zero.
#' - Rows with invalid probabilities (sum <= 0) return NA.
#'
#' @examples
#' wass2s_class_entropy(
#'   p_below = c(1, 0.33),
#'   p_normal = c(0, 0.33),
#'   p_above = c(0, 0.34)
#' )
#'
#' @export
wass2s_class_entropy <- function(p_below, p_normal, p_above, normalize = FALSE) {

  p <- cbind(p_below, p_normal, p_above)

  p[!is.finite(p)] <- 0
  p[p < 0] <- 0

  rs <- rowSums(p)
  invalid <- !is.finite(rs) | rs <= 0

  out <- rep(NA_real_, nrow(p))

  if (any(!invalid)) {
    p_valid <- p[!invalid, , drop = FALSE]
    p_valid <- p_valid / rowSums(p_valid)

    entropy <- -rowSums(ifelse(p_valid > 0, p_valid * log(p_valid), 0))

    if (normalize) {
      entropy <- entropy / log(3)
    }

    out[!invalid] <- entropy
  }

  out
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
#' @param distribution Predictive distribution used for class probabilities:
#'   \code{"normal"}, \code{"student"}, or \code{"auto"}. In \code{"auto"}
#'   mode, samples with fewer than 30 finite climatological observations use
#'   Student's t distribution.
#' @param df_student Degrees of freedom for the Student distribution. If
#'   \code{NULL}, uses \code{max(3, n_hist - 1)}.
#' @return tibble: YYYY, pred, p_below, p_normal, p_above, class_hat, entropy.
#' @export
wass2s_class_from_forecast <- function(
    df, q_hist, sigma = NULL, rmse = NULL, residuals = NULL,
    thresholds = NULL, min_sigma_frac = 0.05,
    distribution = c("normal", "student", "auto"),
    df_student = NULL
){
  distribution <- match.arg(distribution)
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

  n_hist <- sum(is.finite(as.numeric(q_hist)))
  if (identical(distribution, "auto")) {
    distribution <- if (n_hist > 0L && n_hist < 30L) "student" else "normal"
  }
  if (identical(distribution, "student")) {
    if (is.null(df_student)) df_student <- max(3, n_hist - 1L)
    P <- wass2s_class_probs_student(
      mu = df$pred,
      sigma = sigma,
      thresholds = thresholds,
      df = df_student
    )
  } else {
    P <- wass2s_class_probs_norm(mu = df$pred, sigma = sigma, thresholds = thresholds)
  }
  P <- wass2s_normalize_probabilities(P)
  labs <- c("below","normal","above")
  class_hat <- labs[max.col(as.matrix(P), ties.method = "first")]
  entropy <- wass2s_class_entropy(P$p_below, P$p_normal, P$p_above)

  dplyr::bind_cols(df, P) |>
    dplyr::mutate(class_hat = class_hat, entropy = entropy, distribution = distribution)
}

#' From forecast to tercile class probabilities
#'
#' Convenience wrapper around \code{wass2s_class_from_forecast()} using
#' climatological tercile thresholds. This produces categories consistent with
#' the \code{below}/\code{normal}/\code{above} convention used for seasonal
#' hydrological forecasting.
#'
#' @param df Data frame with columns \code{YYYY} and \code{pred}.
#' @param q_hist Numeric historical observations used to estimate terciles and,
#'   when needed, the fallback predictive standard deviation.
#' @param ... Passed to \code{wass2s_class_from_forecast()}.
#'
#' @return Tibble with deterministic forecast, class probabilities, winning
#'   class and entropy.
#'
#' @export
wass2s_tercile_from_forecast <- function(df, q_hist, ...) {
  wass2s_class_from_forecast(
    df = df,
    q_hist = q_hist,
    thresholds = wass2s_tercile_thresholds(q_hist),
    ...
  )
}

#' Verify tercile probability forecasts
#'
#' Computes hydrologically useful probabilistic scores for
#' \code{below}/\code{normal}/\code{above} forecasts: multicategory Brier score,
#' Ranked Probability Score (RPS), Ranked Probability Skill Score (RPSS) against
#' climatology, and classification accuracy.
#'
#' @param truth Numeric observations.
#' @param probs Data frame with \code{p_below}, \code{p_normal},
#'   \code{p_above}.
#' @param thresholds Optional named numeric \code{c(t1, t2)}. If \code{NULL},
#'   terciles are computed from \code{truth}.
#' @param climatology Numeric length-3 reference probabilities. Defaults to
#'   equal tercile probabilities.
#'
#' @return One-row tibble with probabilistic verification scores.
#'
#' @export
wass2s_probabilistic_skill <- function(truth,
                                       probs,
                                       thresholds = NULL,
                                       climatology = c(1 / 3, 1 / 3, 1 / 3)) {
  truth <- as.numeric(truth)
  if (is.null(thresholds)) thresholds <- wass2s_tercile_thresholds(truth)
  if (!all(c("t1", "t2") %in% names(thresholds))) {
    stop("thresholds must be a named numeric vector with t1 and t2.", call. = FALSE)
  }

  p <- wass2s_normalize_probabilities(probs)
  ok <- is.finite(truth)
  ok <- ok & stats::complete.cases(p)
  if (!any(ok)) {
    return(tibble::tibble(
      n = 0L,
      brier_multicategory = NA_real_,
      rps = NA_real_,
      rps_climatology = NA_real_,
      rpss = NA_real_,
      accuracy = NA_real_
    ))
  }

  truth <- truth[ok]
  p <- as.matrix(p[ok, , drop = FALSE])
  cls <- ifelse(
    truth < thresholds[["t1"]], 1L,
    ifelse(truth > thresholds[["t2"]], 3L, 2L)
  )
  obs <- matrix(0, nrow = length(cls), ncol = 3)
  obs[cbind(seq_along(cls), cls)] <- 1

  climatology <- as.numeric(climatology)
  if (length(climatology) != 3L || any(!is.finite(climatology)) || any(climatology < 0) || sum(climatology) <= 0) {
    stop("climatology must be a non-negative numeric vector of length 3 with positive sum.", call. = FALSE)
  }
  climatology <- climatology / sum(climatology)
  clim <- matrix(climatology, nrow = nrow(obs), ncol = 3, byrow = TRUE)

  brier <- rowSums((p - obs)^2)
  rps <- rowSums((t(apply(p, 1, cumsum)) - t(apply(obs, 1, cumsum)))^2) / 2
  rps_clim <- rowSums((t(apply(clim, 1, cumsum)) - t(apply(obs, 1, cumsum)))^2) / 2
  mean_rps <- mean(rps)
  mean_rps_clim <- mean(rps_clim)

  tibble::tibble(
    n = length(cls),
    brier_multicategory = mean(brier),
    rps = mean_rps,
    rps_climatology = mean_rps_clim,
    rpss = if (is.finite(mean_rps_clim) && mean_rps_clim > 0) 1 - mean_rps / mean_rps_clim else NA_real_,
    accuracy = mean(max.col(p, ties.method = "first") == cls)
  )
}
