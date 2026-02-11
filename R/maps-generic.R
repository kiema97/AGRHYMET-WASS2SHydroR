# INTERNAL: detect what kind of map 'data' holds and set an S3 class.
# Rules (by column presence):
# - Probabilities: p_below, p_normal, p_above  -> class 'wass2s_probmap'
# - Final class : class_hat                     -> class 'wass2s_classmap'
# - Metric class: quality (factor/char)         -> class 'wass2s_metricclassmap'
# - Metrics cont.: any of known metric names    -> class 'wass2s_metricmap'
#   (KGE, NSE, RMSE, MAE, R2, Bias, Pbias, MAPE, KGE_cv, NSE_cv, RMSE_cv, ...)
#
# NOTE: keep it INTERNAL (no @export) to avoid polluting the public API.
as_map_data <- function(df, basin_col = "HYBAS_ID") {
  stopifnot(is.data.frame(df))
  has_probs   <- all(c("p_below","p_normal","p_above") %in% names(df))
  has_class   <- "class_hat" %in% names(df)
  has_qcateg  <- "quality" %in% names(df)
  known_metrics <- c("KGE","NSE","RMSE","MAE","R2","Bias","Pbias","MAPE",
                     "KGE_cv","NSE_cv","RMSE_cv","MAE_cv")
  metric_cols <- intersect(known_metrics, names(df))
  has_metrics <- length(metric_cols) > 0

  if (has_probs)   { class(df) <- c("wass2s_probmap", class(df)); return(df) }
  if (has_class)   { class(df) <- c("wass2s_classmap", class(df)); return(df) }
  if (has_qcateg)  { class(df) <- c("wass2s_metricclassmap", class(df)); return(df) }
  if (has_metrics) { attr(df, "metrics") <- metric_cols; class(df) <- c("wass2s_metricmap", class(df)); return(df) }

  stop("wass2s_plot_map(): type 'auto': could not infer map type from provided columns.", call. = FALSE)
}

# INTERNAL: force a user-requested type and validate required columns.
.coerce_to_type <- function(df, type, basin_col = "HYBAS_ID", metrics = NULL) {
  stopifnot(is.data.frame(df))
  req <- function(cols) {
    miss <- setdiff(cols, names(df))
    if (length(miss) > 0) stop(sprintf("wass2s_plot_map(type='%s'): missing required columns: %s",
                                       type, paste(miss, collapse = ", ")), call. = FALSE)
  }

  switch(
    type,
    "probs" = {
      req(c(basin_col, "p_below","p_normal","p_above"))
      class(df) <- c("wass2s_probmap", setdiff(class(df), c("wass2s_classmap","wass2s_metricmap","wass2s_metricclassmap")))
      df
    },
    "class" = {
      req(c(basin_col, "class_hat"))
      class(df) <- c("wass2s_classmap", setdiff(class(df), c("wass2s_probmap","wass2s_metricmap","wass2s_metricclassmap")))
      df
    },
    "metric_classes" = {
      # requires a pre-built 'quality' column (the user can build it upstream, e.g., via cut())
      req(c(basin_col, "quality"))
      class(df) <- c("wass2s_metricclassmap", setdiff(class(df), c("wass2s_probmap","wass2s_metricmap","wass2s_classmap")))
      df
    },
    "metrics" = {
      # if 'metrics' is not provided, detect standard metric columns
      known_metrics <- c("KGE","NSE","RMSE","MAE","R2","Bias","Pbias","MAPE",
                         "KGE_cv","NSE_cv","RMSE_cv","MAE_cv")
      if (is.null(metrics)) {
        metrics <- intersect(known_metrics, names(df))
      }
      if (length(metrics) == 0) {
        stop("wass2s_plot_map(type='metrics'): no metric columns detected nor provided via `metrics=`.", call. = FALSE)
      }
      req(c(basin_col, metrics))
      attr(df, "metrics") <- metrics
      class(df) <- c("wass2s_metricmap", setdiff(class(df), c("wass2s_probmap","wass2s_classmap","wass2s_metricclassmap")))
      df
    },
    {
      stop(sprintf("wass2s_plot_map(): unsupported type '%s'.", type), call. = FALSE)
    }
  )
}

#' Generic map plotter for WASS2S products (probabilities, class, metrics)
#'
#' High-level S3 dispatcher that routes to the appropriate map function based on
#' the structure of `data` **or** an explicit `type` chosen by the user.
#'
#' Supported `type` values:
#' - `"auto"`              : automatic detection based on available columns
#' - `"probs"`             : probability maps (requires `p_below,p_normal,p_above`)
#' - `"class"`             : dominant class map (requires `class_hat`)
#' - `"metrics"`           : continuous metric maps (e.g., `KGE, RMSE`) with facets
#' - `"metric_classes"`    : categorical map based on a `quality` column
#'
#' @param sf_basins sf object or path to a vector file readable by `sf::st_read()`.
#' @param data data.frame/tibble (1 row per basin) containing the expected columns depending on `type`.
#' @param basin_col join key with `sf_basins` (default `"HYBAS_ID"`).
#' @param type one of `"auto"`, `"probs"`, `"class"`, `"metrics"`, `"metric_classes"`.
#'
#' @param layers Optional list of extra ggplot2 layers. Supports both:
#'   - plain layers: `list(geom_sf(...), ...)` (all positioned by `layer_position`)
#'   - positioned layers: `list(list(layer=geom_sf(...), position="below"), ...)`
#' @param layer_position Default position for plain `layers` ("below" or "above").
#' @param sf_crop Optional sf object/path used to crop plot extent (e.g., country boundary).
#' @param basin_line_color Basin borders color (when supported by the underlying plotter).
#' @param basin_line_size Basin borders size (when supported by the underlying plotter).
#'
#' @param ... Additional arguments passed to methods and underlying plotters
#'   (e.g., `metrics=`, `limits=`, `palette=`, `facet_labels=`, `file=`, etc.).
#'
#' @return a `ggplot2::ggplot` object (invisible if saved by the method).
#' @export
wass2s_plot_map <- function(sf_basins, data,
                            basin_col = "HYBAS_ID",
                        type = c("auto","probs","class","metrics","metric_classes"),
                        layers = NULL,
                        layer_position = c("below", "above"),
                        sf_crop = NULL,
                        basin_line_color = NULL,
                        basin_line_size = NULL,
                        ...) {
  type <- match.arg(type)
  dots <- rlang::list2(...)


  # ---- expose new parameters safely (do not override user dots unless explicitly provided) ----
  if (!is.null(layers)) dots$layers <- layers
  if (!is.null(layer_position)) dots$layer_position <- layer_position
  if (!is.null(sf_crop)) dots$sf_crop <- sf_crop

  # Only pass border params if user supplied them.
  # (Avoid forcing defaults here; underlying plotters already have defaults.)
  if (!is.null(basin_line_color)) dots$basin_line_color <- basin_line_color
  if (!is.null(basin_line_size))  dots$basin_line_size  <- basin_line_size

  # Pre-type 'data' according to 'type'
  data_typed <- switch(
    type,
    "auto"           = if (!inherits(data, c("wass2s_probmap","wass2s_classmap","wass2s_metricmap","wass2s_metricclassmap"))) {
      as_map_data(data, basin_col = basin_col)
    } else data,
    "metrics"        = .coerce_to_type(data, "metrics", basin_col = basin_col, metrics = dots$metrics),
    "probs"          = .coerce_to_type(data, "probs", basin_col = basin_col),
    "class"          = .coerce_to_type(data, "class", basin_col = basin_col),
    "metric_classes" = .coerce_to_type(data, "metric_classes", basin_col = basin_col)
  )

  # S3 dispatch
  UseMethod("wass2s_plot_map", data_typed)
}

# Probabilities
#' @export
wass2s_plot_map.wass2s_probmap <- function(sf_basins, data, basin_col = "HYBAS_ID", ...) {
  dots <- rlang::list2(...)
  dots$type <- NULL
  rlang::exec(
    plot_prob_maps,
    sf_basins = sf_basins,
    probs_df  = data,
    basin_col = basin_col,
    !!!dots
  )
}

# Final dominant class
#' @export
wass2s_plot_map.wass2s_classmap <- function(sf_basins, data, basin_col = "HYBAS_ID", ...) {
  dots <- rlang::list2(...)
  dots$type <- NULL
  rlang::exec(
    plot_class_map,
    sf_basins = sf_basins,
    class_df  = data,
    basin_col = basin_col,
    !!!dots
  )
}

# Metric classes
#' @export
wass2s_plot_map.wass2s_metricclassmap <- function(sf_basins, data, basin_col = "HYBAS_ID", ...) {
  dots <- rlang::list2(...)
  dots$type <- NULL
  rlang::exec(
    plot_metric_class_map,
    sf_basins = sf_basins,
    metrics_df  = data,
    basin_col = basin_col,
    !!!dots
  )
}

# Continuous metrics
#' @export
wass2s_plot_map.wass2s_metricmap <- function(sf_basins, data, basin_col = "HYBAS_ID", ...) {
  dots <- rlang::list2(...)
  dots$type <- NULL
  metrics <- dots$metrics %||% attr(data, "metrics") %||%
    stop("wass2s_plot_map(.metricmap): no 'metrics' list and no auto-detection possible.", call. = FALSE)
  dots$metrics <- NULL
  rlang::exec(
    plot_metric_maps,
    sf_basins = sf_basins,
    metrics_df = data,
    basin_col = basin_col,
    metrics = metrics,
    !!!dots
  )
}

#' @export
wass2s_plot_map.default <- function(sf_basins, data, ...) {
  stop("wass2s_plot_map(): unsupported type/data; see ?wass2s_plot_map for expected columns.", call. = FALSE)
}
