#' Kling-Gupta Efficiency (vector form)
#'
#' @param truth Numeric vector of observations.
#' @param estimate Numeric vector of predictions.
#' @return A single numeric value (KGE), in [-Inf, 1]. Higher is better.
#' @noRd
#' @keywords internal
kge_vec <- function(truth, estimate){
  o <- as.numeric(truth); p <- as.numeric(estimate)
  ok <- is.finite(o) & is.finite(p)
  if (!any(ok)) return(NA_real_)
  o <- o[ok]; p <- p[ok]
  so <- stats::sd(o); sp <- stats::sd(p)
  if (!is.finite(so) || !is.finite(sp) || so == 0 || sp == 0) return(NA_real_)
  r     <- stats::cor(p, o)
  beta  <- mean(p) / mean(o)
  gamma <- (sp / mean(p)) / (so / mean(o))
  1 - sqrt((r - 1)^2 + (beta - 1)^2 + (gamma - 1)^2)
}

#' Nash–Sutcliffe Efficiency (vector form)
#'
#' @param truth Numeric vector of observations.
#' @param estimate Numeric vector of predictions.
#' @return A single numeric value (NSE). 1 is perfect, 0 ~= mean model, negative is worse.
#' @noRd
#' @keywords internal
nse_vec <- function(truth, estimate){
  o <- as.numeric(truth); p <- as.numeric(estimate)
  ok <- is.finite(o) & is.finite(p)
  if (!any(ok)) return(NA_real_)
  o <- o[ok]; p <- p[ok]
  denom <- sum((o - mean(o))^2)
  if (!is.finite(denom) || denom == 0) return(NA_real_)
  1 - sum((o - p)^2) / denom
}

#' Mean Absolute Error (safe)
#'
#' @param truth,estimate Numeric vectors.
#' @return MAE (non-negative) or NA if undefined.
#' @noRd
#' @keywords internal
mae_vec_safe <- function(truth, estimate){
  o <- as.numeric(truth); p <- as.numeric(estimate)
  ok <- is.finite(o) & is.finite(p)
  if (!any(ok)) return(NA_real_)
  mean(abs(o[ok] - p[ok]))
}

#' Root Mean Squared Error (safe)
#'
#' @param truth,estimate Numeric vectors.
#' @return RMSE (non-negative) or NA if undefined.
#' @noRd
#' @keywords internal
rmse_vec_safe <- function(truth, estimate){
  o <- as.numeric(truth); p <- as.numeric(estimate)
  ok <- is.finite(o) & is.finite(p)
  if (!any(ok)) return(NA_real_)
  sqrt(mean((o[ok] - p[ok])^2))
}
