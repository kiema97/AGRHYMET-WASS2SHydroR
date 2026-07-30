#' Build a robust resampling plan
#'
#' @param n Integer sample size.
#' @param init_frac Fraction of rows used for the initial training window.
#' @param assess_frac Fraction of rows used for each assessment block.
#' @param n_splits Optional target number of splits.
#' @param strategy One of \code{"auto"}, \code{"rolling"}, or
#'   \code{"small_sample"}. In \code{"auto"} mode, samples below 20 rows use a
#'   more conservative small-sample plan with a larger training fraction and
#'   one-to-few-observation assessment blocks.
#' @param min_initial Minimum initial training size when possible.
#' @param min_assess Minimum assessment size when possible.
#' @param small_n Threshold below which \code{"auto"} switches to
#'   \code{"small_sample"}.
#'
#' @return A list with \code{initial}, \code{assess}, \code{skip},
#'   \code{available}, \code{n_splits}, and \code{strategy}.
#'
#' @export
wass2s_resampling_plan <- function(n,
                                   init_frac = 0.60,
                                   assess_frac = 0.20,
                                   n_splits = NULL,
                                   strategy = c("auto", "rolling", "small_sample"),
                                   min_initial = 8L,
                                   min_assess = 3L,
                                   small_n = 20L) {
  strategy <- match.arg(strategy)
  n <- as.integer(n)
  if (length(n) != 1L || is.na(n) || n < 2L) {
    stop("n must be a single integer >= 2.", call. = FALSE)
  }
  if (!is.null(n_splits)) {
    n_splits <- as.integer(n_splits)
    if (length(n_splits) != 1L || is.na(n_splits) || n_splits < 1L) {
      stop("n_splits must be NULL or a single integer >= 1.", call. = FALSE)
    }
  }

  effective_strategy <- strategy
  if (identical(strategy, "auto")) {
    effective_strategy <- if (n < small_n) "small_sample" else "rolling"
  }

  if (identical(effective_strategy, "small_sample")) {
    init_frac <- max(init_frac, 0.70)
    assess_frac <- min(assess_frac, 0.15)
    min_initial <- min(min_initial, max(3L, n - 2L))
    min_assess <- 1L
    if (is.null(n_splits)) n_splits <- min(5L, max(1L, n - min_initial))
  }

  initial_target <- max(as.integer(min_initial), floor(n * init_frac))
  assess_target <- max(as.integer(min_assess), ceiling(n * assess_frac))

  initial_target <- min(initial_target, n - 1L)
  assess_target <- min(assess_target, n - initial_target)
  if (assess_target < 1L) {
    assess_target <- 1L
    initial_target <- max(1L, n - 1L)
  }

  initial <- initial_target
  assess <- assess_target
  available <- n - initial - assess + 1L
  if (available < 1L) {
    initial <- max(1L, n - assess)
    available <- n - initial - assess + 1L
  }
  if (available < 1L) available <- 1L

  if (is.null(n_splits)) {
    skip <- 0L
    splits_target <- available
  } else if (n_splits == 1L) {
    skip <- max(0L, available - 1L)
    splits_target <- 1L
  } else {
    k <- ceiling((available - 1L) / (n_splits - 1L))
    skip <- max(0L, k - 1L)
    splits_target <- floor((available - 1L) / (skip + 1L)) + 1L
    if (splits_target > n_splits) {
      skip <- skip + (splits_target - n_splits)
      splits_target <- floor((available - 1L) / (skip + 1L)) + 1L
    }
  }

  list(
    initial = as.integer(initial),
    assess = as.integer(assess),
    skip = as.integer(skip),
    available = as.integer(available),
    n_splits = as.integer(splits_target),
    strategy = effective_strategy
  )
}

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
#' @param strategy One of \code{"auto"}, \code{"rolling"}, or
#'   \code{"small_sample"}. The default \code{"auto"} uses a more conservative
#'   validation plan for small samples.
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
                         strategy    = c("auto", "rolling", "small_sample"),
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

  plan <- wass2s_resampling_plan(
    n = n,
    init_frac = init_frac,
    assess_frac = assess_frac,
    n_splits = n_splits,
    strategy = strategy
  )

  if (!quiet && !is.null(n_splits) && plan$n_splits != as.integer(n_splits)) {
    message("Requested ", as.integer(n_splits), " splits but using ",
            plan$n_splits, " achievable splits.")
  }

  # Create rolling-origin resamples
  rsample::rolling_origin(
    data       = df,
    initial    = plan$initial,
    assess     = plan$assess,
    cumulative = cumulative,
    skip       = plan$skip
  )
}

#' @rdname wass2s_rolling_cv
#' @keywords internal
make_rolling <- function(...) {
  #.Deprecated("wass2s_rolling_cv")
  wass2s_rolling_cv(...)
}
