# R/wass2s_kge.R

#' Kling-Gupta Efficiency (vector form)
#'
#' @description
#' Compute the Kling-Gupta Efficiency (KGE) between observations and predictions.
#' Returns a single numeric value in \eqn{(-\infty, 1]}; higher is better.
#'
#' @param truth Numeric vector of observations.
#' @param estimate Numeric vector of predictions.
#'
#' @return A single numeric value (KGE). Returns `NA_real_` if not computable
#'   (e.g., non-finite, zero variance, zero means leading to division by zero).
#'
#' @examples
#' \dontrun{
#' set.seed(1)
#' y  <- rnorm(100, 10, 2)
#' yhat <- y + rnorm(100, 0, 1)
#' wass2s_kge(y, yhat)
#' }
#' @export
wass2s_kge <- function(truth, estimate) {
  o <- as.numeric(truth)
  p <- as.numeric(estimate)

  ok <- is.finite(o) & is.finite(p)
  if (!any(ok)) return(NA_real_)
  o <- o[ok]; p <- p[ok]

  so <- stats::sd(o); sp <- stats::sd(p)
  if (!is.finite(so) || !is.finite(sp) || so == 0 || sp == 0) return(NA_real_)

  mo <- base::mean(o); mp <- base::mean(p)
  # Guards to avoid division by zero in beta/gamma
  if (!is.finite(mo) || mo == 0) return(NA_real_)
  if (!is.finite(mp) || mp == 0) return(NA_real_)

  r <- stats::cor(p, o)
  if (!is.finite(r)) return(NA_real_)

  beta  <- mp / mo
  gamma <- (sp / mp) / (so / mo)

  1 - sqrt((r - 1)^2 + (beta - 1)^2 + (gamma - 1)^2)
}


# R/wass2s_metrics.R
#' Nash–Sutcliffe Efficiency (vector)
#'
#' @description
#' Compute the Nash–Sutcliffe Efficiency (NSE) between observations and predictions.
#' Returns a single numeric value, with 1 = perfect, ~0 = mean model baseline,
#' and negative values indicating performance worse than the mean model.
#'
#' @param truth Numeric vector of observations.
#' @param estimate Numeric vector of predictions.
#'
#' @return A single numeric value (NSE). Returns `NA_real_` if not computable
#'   (e.g., non-finite, zero variance of observations).
#'
#' @examples
#' \dontrun{
#' set.seed(1)
#' y  <- rnorm(100, 10, 2)
#' yhat <- y + rnorm(100, 0, 1)
#' wass2s_nse(y, yhat)
#' }
#' @export
wass2s_nse <- function(truth, estimate) {
  o <- as.numeric(truth); p <- as.numeric(estimate)
  ok <- is.finite(o) & is.finite(p)
  if (!any(ok)) return(NA_real_)
  o <- o[ok]; p <- p[ok]

  mo <- base::mean(o)
  denom <- base::sum((o - mo)^2)
  if (!is.finite(denom) || denom == 0) return(NA_real_)
  1 - base::sum((o - p)^2) / denom
}

#' Mean Absolute Error (vector)
#'
#' @description
#' Compute the Mean Absolute Error (MAE) between observations and predictions.
#'
#' @param truth Numeric vector of observations.
#' @param estimate Numeric vector of predictions.
#'
#' @return A single numeric value (MAE). Returns `NA_real_` if not computable.
#'
#' @examples
#' \dontrun{
#' wass2s_mae(1:5, c(1,2,2,4,6))
#' }
#' @export
wass2s_mae <- function(truth, estimate) {
  o <- as.numeric(truth); p <- as.numeric(estimate)
  ok <- is.finite(o) & is.finite(p)
  if (!any(ok)) return(NA_real_)
  base::mean(base::abs(o[ok] - p[ok]))
}

#' Root Mean Squared Error (vector)
#'
#' @description
#' Compute the Root Mean Squared Error (RMSE) between observations and predictions.
#'
#' @param truth Numeric vector of observations.
#' @param estimate Numeric vector of predictions.
#'
#' @return A single numeric value (RMSE). Returns `NA_real_` if not computable.
#'
#' @examples
#' \dontrun{
#' wass2s_rmse(1:5, c(1,2,2,4,6))
#' }
#' @export
wass2s_rmse <- function(truth, estimate) {
  o <- as.numeric(truth); p <- as.numeric(estimate)
  ok <- is.finite(o) & is.finite(p)
  if (!any(ok)) return(NA_real_)
  base::sqrt(base::mean((o[ok] - p[ok])^2))
}


# R/kge_vec-deprecated.R
#' @keywords internal
#' @noRd
kge_vec <- function(truth, estimate) {
  wass2s_kge(truth, estimate)
}

# R/deprecated-metrics.R

#' @keywords internal
#' @noRd
nse_vec <- function(truth, estimate) {
  wass2s_nse(truth, estimate)
}

#' @keywords internal
#' @noRd
mae_vec_safe <- function(truth, estimate) {
  wass2s_mae(truth, estimate)
}

#' @keywords internal
#' @noRd
rmse_vec_safe <- function(truth, estimate) {
  wass2s_rmse(truth, estimate)
}


# R/wass2s_corr.R

#' Pearson Correlation Coefficient (vector form)
#'
#' @description
#' Compute the Pearson correlation coefficient (PCC) between two numeric
#' vectors, using \code{stats::cor()} internally. The PCC measures the
#' strength and direction of the linear relationship between two variables.
#'
#' @param truth Numeric vector of observations.
#' @param estimate Numeric vector of predictions or simulated values.
#' @param use Character string indicating how missing values are handled.
#'   Passed to \code{stats::cor()}. Default is \code{"pairwise.complete.obs"}.
#' @param method Correlation method to use; default is \code{"pearson"}.
#'
#' @details
#' The Pearson correlation coefficient (PCC) is defined as the ratio between the
#' covariance of two variables and the product of their standard deviations.
#' It always lies in the range \eqn{[-1, 1]}:
#' \itemize{
#'   \item \eqn{r = 1}: perfect positive linear relationship;
#'   \item \eqn{r = -1}: perfect negative linear relationship;
#'   \item \eqn{r = 0}: no linear correlation.
#' }
#'
#' If either variable has zero standard deviation (constant values),
#' \code{NA_real_} is returned.
#'
#' @return A single numeric value representing the Pearson correlation
#'   coefficient (range \eqn{[-1, 1]}). Returns \code{NA_real_} if undefined.
#'
#' @examples
#' \dontrun{
#' obs <- c(1, 2, 3, 4, 5)
#' sim <- c(1.1, 1.9, 3.2, 3.8, 5.1)
#' wass2s_corr(obs, sim)
#'
#' # With missing values
#' sim[3] <- NA
#' wass2s_corr(obs, sim)
#' }
#'
#' @export
wass2s_corr <- function(truth, estimate,
                        use = "pairwise.complete.obs",
                        method = "pearson") {
  # Convert inputs to numeric
  o <- as.numeric(truth)
  p <- as.numeric(estimate)

  # Remove NA pairs if necessary
  ok <- is.finite(o) & is.finite(p)
  if (!any(ok)) return(NA_real_)
  o <- o[ok]; p <- p[ok]

  # Check for variance
  if (stats::sd(o) == 0 || stats::sd(p) == 0) return(NA_real_)

  # Compute correlation safely
  val <- tryCatch(
    stats::cor(o, p, use = use, method = method),
    error = function(e) NA_real_
  )

  as.numeric(val)
}



#' Min–max normalization to a target range
#'
#' @description
#' Scale numeric data to a target range \eqn{[a, b]} (default \eqn{[0, 1]})
#' using the min–max transform. Works with numeric vectors, matrices, and
#' data frames (numeric columns). Handles missing values and constant
#' columns robustly.
#'
#' @param x A numeric vector, matrix, or data frame (numeric columns).
#' @param range Target range as a length-2 numeric vector \code{c(a, b)}.
#'   Default \code{c(0, 1)}.
#' @param na.rm Logical; if \code{TRUE} (default), \code{NA} are ignored when
#'   computing min/max. Original \code{NA} are kept in the output.
#' @param finite_only Logical; if \code{TRUE} (default), only finite values
#'   are used to compute min/max (excludes \code{NA}, \code{NaN}, \code{Inf}).
#' @param by_col Logical; if \code{TRUE}, scale each column independently
#'   (for matrix/data frame). If \code{FALSE}, use global min/max. Default \code{TRUE}.
#' @param constant_handling One of \code{"zero"}, \code{"mid"}, \code{"NA"}, \code{"error"}:
#'   strategy when \code{max == min} for a vector/column:
#'   \itemize{
#'     \item \code{"zero"} (default): return the lower bound \code{range[1]}.
#'     \item \code{"mid"}: return the midpoint \code{mean(range)}.
#'     \item \code{"NA"}: return \code{NA_real_}.
#'     \item \code{"error"}: stop with an informative message.
#'   }
#'
#' @return An object of the same shape as \code{x}, scaled to \code{range}.
#'
#' @examples
#' x <- c(-78, -2, 0.5, 1, -4)
#' wass2s_minmax(x)
#' wass2s_minmax(x, range = c(-1, 1))
#'
#' # Matrix (by column)
#' m <- cbind(a = x, b = x * 2)
#' wass2s_minmax(m, by_col = TRUE)
#'
#' # Data frame (numeric columns only)
#' df <- data.frame(a = x, b = x^2, c = factor(c("u","v","u","v","u")))
#' wass2s_minmax(df)  # only numeric columns are scaled; non-numeric are kept as-is
#'
#' @export
wass2s_minmax <- function(x,
                          range = c(0, 1),
                          na.rm = TRUE,
                          finite_only = TRUE,
                          by_col = TRUE,
                          constant_handling = c("zero", "mid", "NA", "error")) {
  constant_handling <- match.arg(constant_handling)

  if (!is.numeric(range) || length(range) != 2L || any(!is.finite(range))) {
    stop("`range` must be a finite numeric vector of length 2.", call. = FALSE)
  }
  a <- range[1]; b <- range[2]
  if (a == b) stop("`range[1]` and `range[2]` must differ.", call. = FALSE)

  # Helper: scale one numeric vector
  scale_vec <- function(v) {
    v_num <- v
    # Indices to compute min/max
    idx <- is.finite(v_num)
    if (!finite_only) idx <- !is.na(v_num)
    if (!any(idx)) {
      # No valid values to compute range
      if (constant_handling == "error") stop("No finite values to compute min/max.", call. = FALSE)
      return(rep(switch(constant_handling,
                        zero = a,
                        mid  = (a + b) / 2,
                        "NA"   = NA_real_), length(v_num)))
    }

    v_min <- suppressWarnings(min(v_num[idx], na.rm = na.rm))
    v_max <- suppressWarnings(max(v_num[idx], na.rm = na.rm))

    # Constant column/vector
    if (!is.finite(v_min) || !is.finite(v_max) || v_max == v_min) {
      fill <- switch(constant_handling,
                     zero = a,
                     mid  = (a + b) / 2,
                     "NA"   = NA_real_,
                     error = stop("Constant input: min == max; cannot min-max scale.", call. = FALSE))
      out <- rep(fill, length(v_num))
      # Preserve original NA positions
      out[is.na(v_num)] <- NA_real_
      return(out)
    }

    # Standard min-max to [0,1], then to [a,b]
    scaled01 <- (v_num - v_min) / (v_max - v_min)
    out <- a + (b - a) * scaled01

    # Preserve original NA
    out[is.na(v_num)] <- NA_real_
    out
  }

  # Dispatch by type
  if (is.vector(x) && is.numeric(x)) {
    return(scale_vec(x))
  }

  if (is.matrix(x)) {
    if (!is.numeric(x)) stop("Numeric matrix required.", call. = FALSE)
    if (by_col) {
      return(apply(x, 2, scale_vec))
    } else {
      # global min/max
      return(matrix(scale_vec(as.vector(x)), nrow = nrow(x), ncol = ncol(x),
                    dimnames = dimnames(x)))
    }
  }

  if (is.data.frame(x)) {
    out <- x
    num_cols <- vapply(x, is.numeric, logical(1))
    if (any(num_cols)) {
      if (by_col) {
        out[num_cols] <- lapply(out[num_cols], scale_vec)
      } else {
        # scale all numeric columns jointly using global min/max
        merged <- as.numeric(as.matrix(out[num_cols]))
        scaled <- scale_vec(merged)
        out[num_cols] <- split(scaled, rep(seq_len(sum(num_cols)), each = nrow(out)))
      }
    }
    return(out)
  }

  stop("`x` must be a numeric vector, matrix, or data frame with numeric columns.", call. = FALSE)
}
