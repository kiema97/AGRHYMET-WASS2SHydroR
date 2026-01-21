# --- Toy data generator for Hydro+ML -----------------------------------------

#' Create synthetic multi-product hydrological dataset
#'
#' @param basins Integer vector of basin IDs (HYBAS_ID).
#' @param years Integer vector of years.
#' @param products Character vector of product names.
#' @param p Integer, number of predictors per product (named pt_1..pt_p).
#' @param signal_strength Numeric in [0,1], controls how predictive products are.
#' @param miss_rate Fraction of NA injected in predictors (0..1).
#' @param seed Integer seed for reproducibility.
#' @param date_format Character, either "YYYY" or "YYYYMMDD".
#' @param mmdd Character of length 4 (MMDD), used when date_format = "YYYYMMDD".
#'
#' @return Named list of data.frames (one per product) with columns:
#'   HYBAS_ID, YYYY, Q, pt_1..pt_p
#' @export
make_toy_data_by_product <- function(
    basins = c(1040021500, 215675, 330111),
    years = 1990:2023,
    products = c("SST_CMCC", "SST_ECMWF", "SST_JMA"),
    p = 6,
    signal_strength = 0.7,
    miss_rate = 0.05,
    seed = 123,
    date_format = c( "YYYYMMDD","YYYY"),
    mmdd = "1231"
){
  # -------------------- Validation --------------------
  date_format <- match.arg(date_format)

  if (!is.numeric(basins) || length(basins) < 1) {
    stop("`basins` must be a non-empty numeric/integer vector.", call. = FALSE)
  }
  if (!is.numeric(years) || length(years) < 2) {
    stop("`years` must be a numeric/integer vector with at least 2 years.", call. = FALSE)
  }
  years <- as.integer(years)

  if (!is.character(products) || length(products) < 1) {
    stop("`products` must be a non-empty character vector.", call. = FALSE)
  }
  if (!is.numeric(p) || length(p) != 1 || p < 1) {
    stop("`p` must be a single positive integer.", call. = FALSE)
  }
  p <- as.integer(p)

  if (!is.numeric(signal_strength) || signal_strength < 0 || signal_strength > 1) {
    stop("`signal_strength` must be in [0, 1].", call. = FALSE)
  }
  if (!is.numeric(miss_rate) || miss_rate < 0 || miss_rate > 1) {
    stop("`miss_rate` must be in [0, 1].", call. = FALSE)
  }

  if (date_format == "YYYYMMDD") {
    # mmdd must be 4 digits and represent a valid calendar MMDD (basic check)
    if (!is.character(mmdd) || length(mmdd) != 1 || !nzchar(mmdd)) {
      stop("`mmdd` must be a non-empty character scalar like '1231'.", call. = FALSE)
    }
    if (!grepl("^[0-9]{4}$", mmdd)) {
      stop("`mmdd` must be 4 digits (MMDD), e.g. '1231'.", call. = FALSE)
    }
    mm <- as.integer(substr(mmdd, 1, 2))
    dd <- as.integer(substr(mmdd, 3, 4))
    if (mm < 1 || mm > 12) stop("`mmdd` has invalid month.", call. = FALSE)
    if (dd < 1 || dd > 31) stop("`mmdd` has invalid day.", call. = FALSE)
  }

  set.seed(seed)

  nB <- length(basins)
  nY <- length(years)

  # Basin-specific latent coefficients for Q
  beta_basin <- matrix(stats::rnorm(nB * p, 0, 1), nrow = nB)
  rownames(beta_basin) <- as.character(basins)

  # Per-product difficulty: some products are weaker/stronger
  prod_gain <- stats::runif(length(products), 0.4, 1.2)
  names(prod_gain) <- products

  # Build the YYYY column
  make_yyyy <- function(years_vec) {
    if (date_format == "YYYY") {
      return(as.integer(years_vec))
    }
    as.integer(paste0(years_vec, mmdd))
  }

  mk_one_product <- function(prod) {

    out <- purrr::map_dfr(basins, function(bi) {

      X <- matrix(stats::rnorm(nY * p), ncol = p)
      colnames(X) <- paste0("pt_", seq_len(p))

      # Create (noisy) discharge signal Q with yearly trend + pseudo-seasonality
      trend  <- scale(seq_along(years))[, 1]
      season <- sin(2 * pi * seq_along(years) / 11)
      signal <- as.numeric(X %*% beta_basin[as.character(bi), ])

      # Product-specific scaling
      muQ <- 2000 + 200 * trend + 50 * season
      Q <- muQ +
        signal_strength * prod_gain[prod] * scale(signal)[, 1] * 300 +
        stats::rnorm(nY, sd = 300)

      df <- tibble::tibble(
        HYBAS_ID = as.numeric(bi),
        YYYY     = make_yyyy(years),
        Q        = as.numeric(Q)
      )

      df <- dplyr::bind_cols(df, as.data.frame(X))
      df
    })

    # Inject missing values in predictors only
    if (miss_rate > 0) {
      pred_cols <- grep("^pt_", names(out), value = TRUE)
      if (length(pred_cols) > 0) {
        idx <- which(stats::runif(nrow(out) * length(pred_cols)) < miss_rate)
        if (length(idx) > 0) {
          mat <- as.matrix(out[, pred_cols, drop = FALSE])
          mat[idx] <- NA
          out[, pred_cols] <- mat
        }
      }
    }

    out
  }

  setNames(purrr::map(products, mk_one_product), products)
}


# --- Toy data generator for Hydro+ML -----------------------------------------

# Create synthetic multi-product hydrological dataset
#
# @param basins integer vector of HYBAS_IDs
# @param years integer vector of years
# @param products character vector of product names
# @param p integer, number of predictors per product (named pt_1..pt_p)
# @param signal_strength numeric in [0, 1], controls how predictive products are
# @param miss_rate fraction of NA injected in predictors
# @param seed reproducibility
# @return named list of data.frames (one per product) with columns:
#   HYBAS_ID, YYYY, Q, pt_1..pt_p
# @export
# make_toy_data_by_product <- function(
#     basins = c(1040021500, 215675, 330111),
#     years = 1990:2023,
#     products = c("SST_CMCC", "SST_ECMWF", "SST_JMA"),
#     p = 6,
#     signal_strength = 0.7,
#     miss_rate = 0.05,
#     seed = 123
# ){
#   set.seed(seed)
#   nB <- length(basins); nY <- length(years)
#
#   # basin-specific latent coefficients for Q
#   beta_basin <- matrix(rnorm(nB * p, 0, 1), nrow = nB)
#   rownames(beta_basin) <- as.character(basins)
#
#   # per-product difficulty: some products are weaker/stronger
#   prod_gain <- stats::runif(length(products), 0.4, 1.2)
#   names(prod_gain) <- products
#
#   mk_one_product <- function(prod) {
#     out <- purrr::map_dfr(basins, function(bi) {
#       X <- matrix(stats::rnorm(nY * p), ncol = p)
#       colnames(X) <- paste0("pt_", seq_len(p))
#
#       # create (noisy) discharge signal Q with seasonal/yearly trend
#       trend <- scale(seq_along(years))[,1]
#       season <- sin(2*pi*seq_along(years)/11)
#       signal <- as.numeric(X %*% beta_basin[as.character(bi), ])
#
#       # product-specific scaling
#       muQ <- 2000 + 200*trend + 50*season
#       Q <- muQ + signal_strength * prod_gain[prod] * scale(signal)[,1] * 300 +
#         stats::rnorm(nY, sd = 300)
#
#       df <- tibble::tibble(
#         HYBAS_ID = bi,
#         YYYY = years,
#         Q = as.numeric(Q)
#       )
#       df <- dplyr::bind_cols(df, as.data.frame(X))
#       df
#     })
#
#     # inject missing values in predictors only
#     if (miss_rate > 0) {
#       pred_cols <- grep("^pt_", names(out), value = TRUE)
#       idx <- which(stats::runif(nrow(out) * length(pred_cols)) < miss_rate)
#       if (length(idx) > 0) {
#         mat <- as.matrix(out[, pred_cols, drop = FALSE])
#         mat[idx] <- NA
#         out[, pred_cols] <- mat
#       }
#     }
#     out
#   }
#
#   setNames(purrr::map(products, mk_one_product), products)
# }

# df_ <- make_toy_data_by_product()
