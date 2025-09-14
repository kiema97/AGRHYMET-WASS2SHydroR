#' Faceted metric maps (continuous) for hydrological performance
#'
#' Plot basin-level performance metrics (e.g., KGE, NSE, RMSE) on maps,
#' with one facet per metric. Handles both divergent metrics (can be negative)
#' and positive-only metrics. If \pkg{viridis} is available, it is used by default.
#'
#' @param sf_basins An \code{sf} object or a path to a vector file (read via \code{sf::st_read}).
#' @param metrics_df A data.frame with one row per basin, containing \code{basin_col}
#'   and the metric columns to plot.
#' @param basin_col Name of the join key present in both inputs (default "HYBAS_ID").
#' @param metrics Character vector of metric column names to facet (e.g., c("KGE","NSE","RMSE")).
#' @param limits Optional named list of numeric length-2 vectors overriding color limits
#'   per metric, e.g. \code{list(KGE=c(-0.5,1), NSE=c(-0.5,1), RMSE=c(0,800))}.
#' @param divergent_metrics Character vector of metric names to plot with a divergent palette
#'   (e.g., c("KGE","NSE")). Others use a sequential palette.
#' @param title Optional figure title.
#' @param file Optional path to save with \code{ggplot2::ggsave()}.
#' @param width,height Size in inches when saving.
#' @return A ggplot object (returned invisibly if saved).
#' @keywords internal
plot_metric_maps <- function(
    sf_basins,
    metrics_df,
    basin_col = "HYBAS_ID",
    metrics,
    limits = NULL,
    divergent_metrics = c("KGE","NSE"),
    title = NULL,
    file = NULL, width = 10, height = 6
){
  # helper: path or sf -> sf
  as_sf <- function(x) {
    if (inherits(x, "sf")) return(x)
    if (is.character(x) && file.exists(x)) return(sf::st_read(x, quiet = TRUE))
    stop("plot_metric_maps(): 'sf_basins' must be an sf object or a valid file path.", call. = FALSE)
  }

  sf_b <- as_sf(sf_basins)

  # check columns
  miss_key <- setdiff(basin_col, names(metrics_df))
  if (length(miss_key) > 0) stop("plot_metric_maps(): missing join key in metrics_df: ", miss_key, call. = FALSE)
  miss_m  <- setdiff(metrics, names(metrics_df))
  if (length(miss_m) > 0) stop("plot_metric_maps(): missing metric columns in metrics_df: ", paste(miss_m, collapse = ", "), call. = FALSE)

  df <- dplyr::left_join(sf_b, metrics_df, by = dplyr::join_by(!!rlang::sym(basin_col)))

  long_df <- tidyr::pivot_longer(
    df,
    cols = dplyr::all_of(metrics),
    names_to = "metric",
    values_to = "value"
  )

  has_viridis <- requireNamespace("viridis", quietly = TRUE)

  # build scale per panel via ggplot2::ggplot_build? Simpler: use one of two scales with facet-specific limits via scales::free?
  # We’ll keep common scale per metric by using facet-wise rescaling via 'limits' list (optional).
  scale_fun <- function(metric_name) {
    lim <- if (!is.null(limits) && metric_name %in% names(limits)) limits[[metric_name]] else NULL
    if (metric_name %in% divergent_metrics) {
      if (has_viridis) {
        viridis::scale_fill_viridis(
          option = "plasma", direction = -1,  # vivid diverging-like feeling
          name = metric_name, limits = lim, na.value = "#f0f0f0"
        )
      } else {
        ggplot2::scale_fill_gradient2(
          low = "#4575b4", mid = "white", high = "#d73027",
          midpoint = if (is.null(lim)) 0 else mean(lim),
          limits = lim, name = metric_name, na.value = "#f0f0f0"
        )
      }
    } else {
      if (has_viridis) {
        viridis::scale_fill_viridis(
          option = "viridis",
          name = metric_name, limits = lim, na.value = "#f0f0f0"
        )
      } else {
        ggplot2::scale_fill_gradient(
          low = "white", high = "steelblue",
          limits = lim, name = metric_name, na.value = "#f0f0f0"
        )
      }
    }
  }

  # one plot per metric, then patchwork together to preserve scale per facet nicely
  # (keeps legends separate and clear)
  plots <- lapply(unique(long_df$metric), function(m) {
    d <- dplyr::filter(long_df, .data$metric == m)
    ggplot2::ggplot(d) +
      ggplot2::geom_sf(ggplot2::aes(fill = value), color = "grey70", size = 0.1) +
      scale_fun(m) +
      ggplot2::coord_sf(expand = FALSE) +
      ggplot2::labs(title = m) +
      ggplot2::theme_minimal(base_size = 12) +
      ggplot2::theme(
        panel.grid.major = ggplot2::element_line(color = "transparent"),
        axis.text = ggplot2::element_blank(),
        axis.ticks = ggplot2::element_blank(),
        panel.border = ggplot2::element_rect(color = "grey60", fill = NA)
      )
  })

  # arrange with patchwork if available; otherwise fallback to facet_wrap (shared scale)
  if (requireNamespace("patchwork", quietly = TRUE)) {
    p <- Reduce(`+`, plots) + patchwork::plot_layout(nrow = 1, guides = "collect") &
      patchwork::plot_annotation(title = title)
  } else {
    # fallback: single plot with facet_wrap (shared scale per all metrics)
    p <- ggplot2::ggplot(long_df) +
      ggplot2::geom_sf(ggplot2::aes(fill = value), color = "grey70", size = 0.1) +
      (if (has_viridis) viridis::scale_fill_viridis(name = "Value", na.value = "#f0f0f0")
       else ggplot2::scale_fill_gradient(low = "white", high = "steelblue", name = "Value", na.value = "#f0f0f0")) +
      ggplot2::facet_wrap(~ metric, nrow = 1, scales = "free") +
      ggplot2::coord_sf(expand = FALSE) +
      ggplot2::labs(title = title) +
      ggplot2::theme_minimal(base_size = 12) +
      ggplot2::theme(
        panel.grid.major = ggplot2::element_line(color = "transparent"),
        axis.text = ggplot2::element_blank(),
        axis.ticks = ggplot2::element_blank(),
        panel.border = ggplot2::element_rect(color = "grey60", fill = NA),
        strip.text = ggplot2::element_text(face = "bold")
      )
  }

  if (!is.null(file)) {
    ggplot2::ggsave(file, p, width = width, height = height, dpi = 300)
    return(invisible(p))
  }
  p
}


#' Categorical quality map from a continuous metric
#'
#' Discretize a metric (e.g., KGE) into quality classes using breakpoints,
#' then plot a categorical \code{sf} map.
#'
#' @param sf_basins sf object or path.
#' @param metrics_df data.frame with \code{basin_col} and a continuous metric column.
#' @param basin_col join key (default "HYBAS_ID").
#' @param metric name of the continuous metric column (e.g., "KGE").
#' @param breaks numeric vector of breakpoints (default c(-Inf, 0.2, 0.5, 0.75, Inf)).
#' @param labels character labels for intervals (length = length(breaks)-1).
#' @param colors named vector of colors for labels (same length as labels).
#' @param title optional title; @param file path to save; @param width,height inches.
#' @return ggplot object.
#' @keywords internal
plot_metric_class_map <- function(
    sf_basins,
    metrics_df,
    basin_col = "HYBAS_ID",
    metric = "KGE",
    breaks = c(-Inf, 0.2, 0.5, 0.75, Inf),
    labels = c("Very poor","Poor","Fair","Good"),
    colors = c("Very poor"="#b2182b","Poor"="#ef8a62","Fair"="#67a9cf","Good"="#2166ac"),
    title = NULL,
    file = NULL, width = 7, height = 6
){
  as_sf <- function(x) {
    if (inherits(x, "sf")) return(x)
    if (is.character(x) && file.exists(x)) return(sf::st_read(x, quiet = TRUE))
    stop("plot_metric_class_map(): 'sf_basins' must be an sf object or a valid path.", call. = FALSE)
  }

  sf_b <- as_sf(sf_basins)

  if (!all(c(basin_col, metric) %in% names(metrics_df))) {
    stop("plot_metric_class_map(): 'metrics_df' must contain basin_col and requested metric.", call. = FALSE)
  }

  df <- dplyr::left_join(sf_b, metrics_df, by = dplyr::join_by(!!rlang::sym(basin_col)))

  df$quality <- cut(df[[metric]], breaks = breaks, labels = labels, include.lowest = TRUE, right = TRUE)

  p <- ggplot2::ggplot(df) +
    ggplot2::geom_sf(ggplot2::aes(fill = quality), color = "grey70", size = 0.1) +
    ggplot2::scale_fill_manual(values = colors, drop = FALSE, na.value = "#f0f0f0", name = metric) +
    ggplot2::coord_sf(expand = FALSE) +
    ggplot2::labs(title = title) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      panel.grid.major = ggplot2::element_line(color = "transparent"),
      axis.text = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      panel.border = ggplot2::element_rect(color = "grey60", fill = NA),
      legend.position = "right"
    )

  if (!is.null(file)) {
    ggplot2::ggsave(file, p, width = width, height = height, dpi = 300)
    return(invisible(p))
  }
  p
}
