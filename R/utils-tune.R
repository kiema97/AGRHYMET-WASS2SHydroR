# ---- helpers ----
.pred_years_to_bounds <- function(prediction_years) {
  if (is.null(prediction_years)) return(NULL)

  if (length(prediction_years) != 2) {
    stop("prediction_years must be length 2.", call. = FALSE)
  }

  py <- as.numeric(prediction_years)

  # Case A: already YYYYMMDD (>= 10^7)
  if (all(py >= 1e7)) {
    start_bound <- as.numeric(py[1])
    end_bound   <- as.numeric(py[2])
    return(list(start = start_bound, end = end_bound))
  }

  # Case B: plain years (e.g., 2001, 2005)
  start_bound <- as.numeric(paste0(py[1], "0101"))
  end_bound   <- as.numeric(paste0(py[2], "1231"))
  list(start = start_bound, end = end_bound)
}

#' Check whether predictors are valid and informative
#'
#' Internal utility to verify that a set of predictors is suitable for
#' machine learning model training. A predictor set is considered valid if:
#' \itemize{
#'   \item At least one predictor is provided;
#'   \item No predictor is entirely \code{NA};
#'   \item (Optional) Each predictor has non-zero variance.
#' }
#'
#' This helper is primarily used to guard against degenerate training splits
#' (e.g. during rolling-origin cross-validation) where all predictors may be
#' constant or missing, which would cause certain models (e.g. SVM, glmnet)
#' to fail with errors such as \emph{"No covariates found"}.
#'
#' @param df A data frame containing predictor columns.
#' @param predictors Character vector of predictor column names.
#' @param require_variance Logical; if \code{TRUE}, predictors must have
#'   strictly positive variance (default: \code{TRUE}).
#'
#' @return Logical scalar:
#' \itemize{
#'   \item \code{TRUE} if the predictors are valid and informative;
#'   \item \code{FALSE} otherwise.
#' }
#'
#' @keywords internal
#' @noRd
has_valid_predictors <- function(df, predictors, require_variance = TRUE) {

  if (!is.data.frame(df)) return(FALSE)
  if (length(predictors) == 0L) return(FALSE)

  for (v in predictors) {

    if (!v %in% names(df)) return(FALSE)

    x <- df[[v]]

    # All NA → invalid
    if (all(is.na(x))) return(FALSE)

    # Zero or undefined variance → invalid (if required)
    if (isTRUE(require_variance)) {
      s <- stats::sd(x, na.rm = TRUE)
      if (!is.finite(s) || s < 1e-12) return(FALSE)
    }
  }

  TRUE
}
