#' Run the "stat" method (S3 dispatch)
#'
#' This method is automatically dispatched by
#' `run_method()` when `method` has class `method_stat`.
#' It delegates to the statistical+ML pipeline that trains PCR/Ridge/Lasso
#' per product and basin, fuses top-K products by KGE, then fits a meta-learner.
#'
#' @param method An object created by `method_id("stat")`.
#' @param data_by_product Named list of data frames, one per climate product.
#'   Each data frame must include at least: `HYBAS_ID`, `YYYY`, `Q`, and predictors.
#' @param cfg Named list parsed from YAML configuration (see `config/`).
#' @return A named list keyed by basin id, each element being the result list
#'   returned by `wass2s_run_basin_mods_stat()` (fields: `fused_by_model`,
#'   `final_test`, `scores`, `leaderboards`).
#' @seealso [wass2s_run_basins_stat()], [wass2s_run_basin_mods_stat()]
#' @keywords internal
run_method.method_stat <- function(method, data_by_product, cfg) {
  wass2s_run_pipeline_stat(data_by_product, cfg)
}


#' Wrapper function called by the core (run_method) for statistical pipeline
#'
#' This function reads useful parameters from `cfg` (YAML) and delegates to `wass2s_run_basins_stat()`.
#' It exists solely to provide a standardized interface to the core.
#'
#' @param data_by_product Named list of data frames (one per product)
#' @param cfg R list from YAML (see config/config.dev.yaml)
#' @return Same structure as `wass2s_run_basins_stat()`
#' @export
wass2s_run_pipeline_stat <- function(data_by_product, cfg) {
  # Input validation
  if (is.null(cfg)) {
    stop("Configuration (cfg) cannot be NULL", call. = FALSE)
  }

  if (length(data_by_product) == 0) {
    stop("data_by_product cannot be empty", call. = FALSE)
  }

  # Extract parameters with defaults
  patterns <- cfg$data$patterns
  topK <- cfg$train$topK %||% 3
  workers <- cfg$compute$workers %||% 8
  parallel <- cfg$compute$parallel %||% TRUE
  quiet <- cfg$compute$quiet %||% TRUE
  basins <- cfg$train$basins  # Could be NULL
  hybas_id <- cfg$data$hybas_id %||% "HYBAS_ID"

  # Validate patterns
  if (!is.null(patterns) && !is.list(patterns) && !is.character(patterns)) {
    stop("patterns must be a named character vector or list", call. = FALSE)
  }

  # Validate workers
  if (!is.numeric(workers) || workers < 1) {
    warning("Invalid workers value, using default: 8", call. = FALSE)
    workers <- 8
  }

  # Validate topK
  if (!is.numeric(topK) || topK < 1) {
    warning("Invalid topK value, using default: 3", call. = FALSE)
    topK <- 3
  }

  # Check if basins exist in data
  if (!is.null(basins)) {
    all_basins <- unique(unlist(lapply(data_by_product, function(df) {
      unique(df[[hybas_id]])
    })))

    missing_basins <- setdiff(basins, all_basins)
    if (length(missing_basins) > 0) {
      warning("Some specified basins not found in data: ",
              paste(missing_basins, collapse = ", "), call. = FALSE)
    }
  }

  # Extract additional parameters for wass2s_run_basins_stat
  additional_params <- cfg$train[setdiff(names(cfg$train),
                                         c("topK", "basins", "patterns"))]

  # Call the main function with all parameters
  result <- tryCatch({
    do.call(wass2s_run_basins_stat, c(
      list(
        data_by_product = data_by_product,
        hybas_id = hybas_id,
        pred_pattern_by_product = patterns,
        topK = topK,
        basins = basins,
        parallel = parallel,
        workers = workers,
        quiet = quiet
      ),
      additional_params
    ))
  }, error = function(e) {
    stop("Error in wass2s_run_basins_stat: ", e$message, call. = FALSE)
  })

  # Return results
  result
}

