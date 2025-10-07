#' Robust rolling-origin CV with target number of splits
#'
#' Builds cumulative rolling-origin resamples with safe sizing and an optional
#' target number of splits. Internally, the function computes `initial` and
#' `assess` from fractions, then chooses `skip` so that the number of splits is
#' (approximately) equal to `n_splits`. When `n_splits` is larger than the
#' maximum achievable, it falls back to the maximum.
#'
#' @param df A data frame sorted by the time column.
#' @param year_col Name of the time column (default: `"YYYY"`).
#' @param init_frac Fraction of rows used for the initial training window
#'   (default 0.60). A hard minimum of 8 rows is enforced when possible.
#' @param assess_frac Fraction of rows used for the assessment window
#'   (default 0.20). A hard minimum of 3 rows is enforced when possible.
#' @param n_splits Optional integer, desired number of resamples (splits).
#'   If `NULL` (default), every possible split is produced (`skip = 0`).
#' @param cumulative Logical; passed to `rsample::rolling_origin()`
#'   (default `TRUE`).
#' @param quiet Logical; if `FALSE`, emits informative messages when the
#'   requested `n_splits` cannot be reached (default `TRUE`).
#'
#' @return An `rsample::rset` created by `rsample::rolling_origin()`.
#'
#' @examples
#' df <- tibble::tibble(YYYY = 1990:2010, Q = rnorm(21))
#' # All possible splits
#' rs1 <- wass2s_rolling_cv(df)
#' # About 5 splits
#' rs2 <- wass2s_rolling_cv(df, n_splits = 5)
#'
#' @export
wass2s_rolling_cv <- function(df,
                         year_col    = "YYYY",
                         init_frac   = 0.60,
                         assess_frac = 0.20,
                         n_splits    = NULL,
                         cumulative  = TRUE,
                         quiet       = TRUE){

  df <- dplyr::arrange(df, .data[[year_col]])
  n  <- nrow(df)

  # Input validation
  if (n < 2L) {
    stop("make_rolling(): not enough rows (need >= 2). Got n = ", n, call. = FALSE)
  }
  if (!is.null(n_splits)) {
    n_splits <- as.integer(n_splits)
    if (n_splits < 1L) {
      stop("make_rolling(): n_splits must be >= 1.", call. = FALSE)
    }
  }

  # Compute initial and assessment sizes
  initial_target <- max(8L, floor(n * init_frac))
  assess_target  <- max(3L, ceiling(n * assess_frac))

  # Adjust to ensure initial + assess <= n
  if (initial_target + assess_target > n) {
    initial <- max(1L, n - assess_target)
    assess  <- n - initial
  } else {
    initial <- initial_target
    assess  <- assess_target
  }

  # Maximum possible number of splits
  available <- n - initial - assess + 1L
  if (available < 1L) available <- 1L

  # Handle n_splits logic
  if (is.null(n_splits)) {
    skip <- 0L
    splits_target <- available
  } else {
    if (n_splits == 1L) {
      skip <- available - 1L
      splits_target <- 1L
    } else {
      # Compute skip to get approx n_splits
      k <- ceiling((available - 1L) / (n_splits - 1L))
      skip <- max(0L, k - 1L)
      splits_target <- floor((available - 1L) / (skip + 1L)) + 1L

      # Adjust if overshooting
      if (splits_target > n_splits) {
        skip <- skip + (splits_target - n_splits)
        splits_target <- floor((available - 1L) / (skip + 1L)) + 1L
      }
    }

    # Informative messages
    if (!quiet && splits_target != n_splits) {
      if (splits_target < n_splits) {
        message("Requested ", n_splits, " splits but only ", splits_target,
                " are achievable. Using ", splits_target, " splits.")
      } else {
        message("Requested ", n_splits, " splits but got ", splits_target,
                ". Adjusting skip to achieve exact count.")
      }
    }
  }

  # Create rolling-origin resamples
  rsample::rolling_origin(
    data       = df,
    initial    = initial,
    assess     = assess,
    cumulative = cumulative,
    skip       = skip
  )
}

#' @rdname wass2s_rolling_cv
#' @keywords internal
make_rolling <- function(...) {
  #.Deprecated("wass2s_rolling_cv")
  wass2s_rolling_cv(...)
}
