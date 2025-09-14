#' S3 entrypoint for method "hydro"
#'
#' Called by run_method() when the 'method' object
#' has class 'method_hydro'.
#'
#' @param method object created by method_id("hydro")
#' @param data_by_product named list of data.frames (one per product)
#' @param cfg list with configuration (see Details)
#' @return list (same structure as run_one_basin_all_models_ml / wass2s_run_basins_ml)
#' @export
run_method.method_hydro <- function(method, data_by_product, cfg) {
  # Call the pipeline function
  wass2s_run_pipeline_hydro(data_by_product, cfg)
}

#' Wrapper function called by the core (run_method) for hydro (ML) pipeline
#'
#' This function reads useful parameters from `cfg` (YAML) and delegates to `wass2s_run_basins_ml()`.
#' It exists solely to provide a standardized interface to the core.
#'
#' @param data_by_product Named list of data frames (one per product)
#' @param cfg R list from YAML (see config/config.dev.yaml)
#' @return Same structure as `wass2s_run_basins_ml()`
#' @export
wass2s_run_pipeline_hydro <- function(data_by_product, cfg) {
  # Input validation
  if (is.null(cfg)) {
    stop("Configuration (cfg) cannot be NULL", call. = FALSE)
  }

  if (length(data_by_product) == 0) {
    stop("data_by_product cannot be empty", call. = FALSE)
  }

  # Extract parameters with defaults
  basin_col <- cfg$basin_col %||% "HYBAS_ID"
  patterns <- cfg$pred_pattern_by_product %||% NULL
  models <- cfg$models %||% c("rf", "xgb", "mlp")
  topK <- cfg$topK %||% 3
  min_kge <- cfg$min_kge_model %||% 0.2
  grid_lvl <- cfg$grid_levels %||% 5
  fuser <- cfg$final_fuser %||% "rf"
  basins <- cfg$basins %||% NULL
  parallel <- cfg$parallel %||% FALSE
  workers <- cfg$workers %||% 4
  quiet <- cfg$quiet %||% TRUE

  # Additional parameters for wass2s_run_basins_ml
  prediction_years <- cfg$prediction_years %||% NULL
  target_positive <- cfg$target_positive %||% FALSE
  init_frac <- cfg$init_frac %||% 0.60
  assess_frac <- cfg$assess_frac %||% 0.20
  n_splits <- cfg$n_splits %||% 3
  cumulative <- cfg$cumulative %||% TRUE

  # Validate parameters
  if (!is.numeric(topK) || topK < 1) {
    warning("Invalid topK value, using default: 3", call. = FALSE)
    topK <- 3
  }

  if (!is.numeric(workers) || workers < 1) {
    warning("Invalid workers value, using default: 4", call. = FALSE)
    workers <- 4
  }

  if (!is.numeric(grid_lvl) || grid_lvl < 1) {
    warning("Invalid grid_levels value, using default: 5", call. = FALSE)
    grid_lvl <- 5
  }

  # Check if models are supported
  unsupported_models <- setdiff(models, SUPPORTED_MODELS)
  if (length(unsupported_models) > 0) {
    warning("Ignoring unsupported models: ", paste(unsupported_models, collapse = ", "), call. = FALSE)
    models <- intersect(models, SUPPORTED_MODELS)
  }

  if (length(models) == 0) {
    stop("No valid models specified after filtering unsupported models", call. = FALSE)
  }

  # Check if final_fuser is supported
  if (!fuser %in% SUPPORTED_FUSERS) {
    warning("Unsupported final_fuser '", fuser, "', using default: 'rf'", call. = FALSE)
    fuser <- "rf"
  }

  # Check if basins exist in data
  if (!is.null(basins)) {
    all_basins <- unique(unlist(lapply(data_by_product, function(df) {
      unique(df[[basin_col]])
    })))

    missing_basins <- setdiff(basins, all_basins)
    if (length(missing_basins) > 0) {
      warning("Some specified basins not found in data: ",
              paste(missing_basins, collapse = ", "), call. = FALSE)
    }
  }

  # Call the main function
  result <- tryCatch({
    wass2s_run_basins_ml(
      data_by_product = data_by_product,
      hybas_id = basin_col,
      pred_pattern_by_product = patterns,
      models = models,
      topK = topK,
      min_kge_model = min_kge,
      basins = basins,
      parallel = parallel,
      workers = workers,
      grid_levels = grid_lvl,
      final_fuser = fuser,
      prediction_years = prediction_years,
      target_positive = target_positive,
      init_frac = init_frac,
      assess_frac = assess_frac,
      n_splits = n_splits,
      cumulative = cumulative,
      quiet = quiet
    )
  }, error = function(e) {
    stop("Error in wass2s_run_basins_ml: ", e$message, call. = FALSE)
  })

  # Return results
  result
}
