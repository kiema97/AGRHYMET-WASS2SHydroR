# --- Toy data generator for Hydro+ML -----------------------------------------

#' Create synthetic multi-product hydrological dataset
#'
#' @param basins integer vector of HYBAS_IDs
#' @param years integer vector of years
#' @param products character vector of product names
#' @param p integer, number of predictors per product (named pt_1..pt_p)
#' @param signal_strength numeric in [0, 1], controls how predictive products are
#' @param miss_rate fraction of NA injected in predictors
#' @param seed reproducibility
#' @return named list of data.frames (one per product) with columns:
#'   HYBAS_ID, YYYY, Q, pt_1..pt_p
#' @export
make_toy_data_by_product <- function(
    basins = c(1040021500, 215675, 330111),
    years = 1990:2023,
    products = c("SST_CMCC", "SST_ECMWF", "SST_JMA"),
    p = 6,
    signal_strength = 0.7,
    miss_rate = 0.05,
    seed = 123
){
  set.seed(seed)
  nB <- length(basins); nY <- length(years)

  # basin-specific latent coefficients for Q
  beta_basin <- matrix(rnorm(nB * p, 0, 1), nrow = nB)
  rownames(beta_basin) <- as.character(basins)

  # per-product difficulty: some products are weaker/stronger
  prod_gain <- stats::runif(length(products), 0.4, 1.2)
  names(prod_gain) <- products

  mk_one_product <- function(prod) {
    out <- purrr::map_dfr(basins, function(bi) {
      X <- matrix(stats::rnorm(nY * p), ncol = p)
      colnames(X) <- paste0("pt_", seq_len(p))

      # create (noisy) discharge signal Q with seasonal/yearly trend
      trend <- scale(seq_along(years))[,1]
      season <- sin(2*pi*seq_along(years)/11)
      signal <- as.numeric(X %*% beta_basin[as.character(bi), ])

      # product-specific scaling
      muQ <- 2000 + 200*trend + 50*season
      Q <- muQ + signal_strength * prod_gain[prod] * scale(signal)[,1] * 300 +
        stats::rnorm(nY, sd = 300)

      df <- tibble::tibble(
        HYBAS_ID = bi,
        YYYY = years,
        Q = as.numeric(Q)
      )
      df <- dplyr::bind_cols(df, as.data.frame(X))
      df
    })

    # inject missing values in predictors only
    if (miss_rate > 0) {
      pred_cols <- grep("^pt_", names(out), value = TRUE)
      idx <- which(stats::runif(nrow(out) * length(pred_cols)) < miss_rate)
      if (length(idx) > 0) {
        mat <- as.matrix(out[, pred_cols, drop = FALSE])
        mat[idx] <- NA
        out[, pred_cols] <- mat
      }
    }
    out
  }

  setNames(purrr::map(products, mk_one_product), products)
}

# df_ <- make_toy_data_by_product()
