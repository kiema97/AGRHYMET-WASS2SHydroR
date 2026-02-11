#' Coerce to sf (path or sf object)
#'
#' @param x Character path to a vector file readable by sf (shp/gpkg/geojson/...), or an sf object.
#' @return sf object
#' @keywords internal
as_sf <- function(x) {
  if (inherits(x, "sf")) return(x)

  if (inherits(x, "sfc")) {
    return(sf::st_sf(geometry = x))
  }

  if (is.character(x) && length(x) == 1L && nzchar(x)) {
    if (file.exists(x)) {
      return(sf::st_read(x, quiet = TRUE))
    }
  }

  stop("as_sf(): provide an sf/sfc object or a valid file path.", call. = FALSE)
}


#' Plot probability maps for below/normal/above classes (faceted) with extra layers
#'
#' Join a basin geometry and a table of class probabilities, then plot
#' faceted maps (one facet per class). Expects columns `p_below`, `p_normal`,
#' `p_above`. If your table has several years, filter beforehand.
#'
#' Extra layers can be added either:
#' 1) As a list of ggplot2 layers (all added "below" by default), or
#' 2) As a list of items with `layer` and `position` (position per layer),
#'    where position is "below" or "above".
#'
#' @param sf_basins sf object or path to a vector file (shapefile, gpkg, …).
#' @param probs_df data.frame with one row per basin and columns:
#'   `basin_col`, `p_below`, `p_normal`, `p_above`.
#' @param basin_col name of the basin id column shared by both inputs.
#' @param limits numeric length-2 for the fill scale limits (default c(0,1)).
#' @param palette continuous palette for probabilities (e.g. "viridis", "magma").
#' @param facet_labels named character vector to rename facets, e.g.
#'   c(p_below="Below", p_normal="Normal", p_above="Above").
#' @param title optional title.
#' @param file optional path to save the figure via ggsave (format inferred from extension).
#' @param width,height units in inches for saving (if `file` supplied).
#'
#' @param layers optional list of layers. Two supported formats:
#'   - ggplot2 layers: `list(geom_sf(...), geom_sf(...))` (all placed by `layer_position`)
#'   - positioned layers: `list(list(layer = geom_sf(...), position = "below"),
#'                             list(layer = geom_sf(...), position = "above"))`
#' @param layer_position default position for `layers` when passed as plain ggplot layers
#'   (ignored when `layers` is passed in positioned format).
#' @param sf_crop optional sf object or path used to crop the plot extent (e.g., country boundary).
#' @param basin_line_color basin border color.
#' @param basin_line_size basin border size.
#'
#' @return ggplot object (invisibly if saved).
#' @keywords internal
plot_prob_maps <- function(
    sf_basins,
    probs_df,
    basin_col = "HYBAS_ID",
    limits = c(0, 1),
    palette = c("viridis"),
    facet_labels = c(p_below = "Below", p_normal = "Normal", p_above = "Above"),
    title = NULL,
    file = NULL, width = 10, height = 6,
    layers = NULL,
    layer_position = c("below", "above"),
    sf_crop = NULL,
    basin_line_color = "grey70",
    basin_line_size = 0.1
){
  .need("ggplot2")
  layer_position <- match.arg(layer_position)

  sf_b <- as_sf(sf_basins)

  # ---- validation ----
  required_cols <- c(basin_col, "p_below", "p_normal", "p_above")
  miss <- setdiff(required_cols, names(probs_df))
  if (length(miss) > 0) {
    stop(sprintf(
      "plot_prob_maps(): missing columns in probs_df: %s",
      paste(miss, collapse = ", ")
    ), call. = FALSE)
  }

  # ---- join geometry ----
  map_df <- dplyr::left_join(
    sf_b,
    probs_df,
    by = dplyr::join_by(!!rlang::sym(basin_col))
  )

  # ---- long format for faceting ----
  long_df <- tidyr::pivot_longer(
    data = map_df,
    cols = c("p_below", "p_normal", "p_above"),
    names_to = "class",
    values_to = "prob"
  ) |>
    dplyr::mutate(
      class = factor(
        x = class,
        levels = c("p_above", "p_normal", "p_below"),
        labels = c("p_above", "p_normal", "p_below")
      )
    )

  # ---- optional crop extent ----
  coord_limits <- NULL
  if (!is.null(sf_crop)) {
    crop_sf <- as_sf(sf_crop)
    bbox <- sf::st_bbox(crop_sf)
    coord_limits <- list(
      xlim = c(bbox[["xmin"]], bbox[["xmax"]]),
      ylim = c(bbox[["ymin"]], bbox[["ymax"]])
    )
  }

  # ---- normalize layers input ----
  # Convert `layers` to a list of {layer, position} items.
  norm_layers <- NULL

  if (!is.null(layers)) {
    if (!is.list(layers)) {
      stop("plot_prob_maps(): `layers` must be a list.", call. = FALSE)
    }

    is_positioned <- all(vapply(layers, function(x) {
      is.list(x) && all(c("layer", "position") %in% names(x))
    }, logical(1)))

    if (is_positioned) {
      for (i in seq_along(layers)) {
        pos <- layers[[i]]$position
        if (!is.character(pos) || length(pos) != 1L || !pos %in% c("below", "above")) {
          stop(sprintf(
            "plot_prob_maps(): layers[[%d]]$position must be 'below' or 'above'.",
            i
          ), call. = FALSE)
        }
      }
      norm_layers <- layers
    } else {
      norm_layers <- lapply(layers, function(ly) list(layer = ly, position = layer_position))
    }
  }

  add_layers_by_pos <- function(p, norm_layers, pos) {
    if (is.null(norm_layers)) return(p)
    for (x in norm_layers) {
      if (identical(x$position, pos)) {
        p <- p + x$layer
      }
    }
    p
  }

  # ---- fill scale ----
  has_viridis <- requireNamespace("viridis", quietly = TRUE)
  scale_fill_prob <- if (has_viridis) {
    viridis::scale_fill_viridis(
      option = palette[1],
      limits = limits,
      name = "Probability",
      na.value = "#f0f0f0"
    )
  } else {
    ggplot2::scale_fill_gradient(
      limits = limits,
      name = "Probability",
      low = "white",
      high = "steelblue",
      na.value = "#f0f0f0"
    )
  }

  # ---- build plot ----
  p <- ggplot2::ggplot()

  # Extra layers below basins
  p <- add_layers_by_pos(p, norm_layers, "below")

  # Basins (faceted)
  p <- p +
    ggplot2::geom_sf(
      data = long_df,
      ggplot2::aes(fill = prob),
      color = basin_line_color,
      size  = basin_line_size
    ) +
    scale_fill_prob +
    ggplot2::facet_wrap(
      ~ class, nrow = 1,
      labeller = ggplot2::as_labeller(facet_labels)
    )

  # Extra layers above basins
  p <- add_layers_by_pos(p, norm_layers, "above")

  # Coord
  if (!is.null(coord_limits)) {
    p <- p + ggplot2::coord_sf(
      xlim = coord_limits$xlim,
      ylim = coord_limits$ylim,
      expand = FALSE
    )
  } else {
    p <- p + ggplot2::coord_sf(expand = FALSE)
  }

  # Theme + labels
  p <- p +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      panel.background = ggplot2::element_rect(fill = "white"),
      plot.background = ggplot2::element_rect(fill = "white", color = NA),
      strip.background = ggplot2::element_rect(fill = "#f0f0f0", color = NA),
      strip.text = ggplot2::element_text(face = "bold", size = 12),
      axis.text.x = ggplot2::element_text(angle = 0, hjust = 1),
      legend.position = "bottom",
      legend.key.width = grid::unit(2, "cm"),
      legend.title = ggplot2::element_text(face = "bold"),
      plot.title = ggplot2::element_text(face = "bold", size = 16, hjust = 0.5),
      plot.subtitle = ggplot2::element_text(size = 12, hjust = 0.5),
      plot.margin = ggplot2::margin(1, 1, 1, 1)
    ) +
    ggplot2::labs(title = title)

  if (!is.null(file)) {
    ggplot2::ggsave(file, p, width = width, height = height, dpi = 300)
    return(invisible(p))
  }

  p
}


#' Coerce to sf (path or sf object)
#' @param x character path to a vector file readable by sf, or an sf object.
#' @return sf object
#' @keywords internal
as_sf_ <- function(x) {
  if (inherits(x, "sf")) return(x)
  if (is.character(x) && file.exists(x)) return(sf::st_read(x, quiet = TRUE))
  stop("as_sf(): provide an sf object or a valid file path.", call. = FALSE)
}

#' Plot probability maps for below/normal/above classes (faceted)
#'
#' Join a basin geometry and a table of class probabilities, then plot
#' faceted maps (one facet per class). Expects columns `p_below`, `p_normal`,
#' `p_above`. If your table has several years, filter beforehand.
#'
#' @param sf_basins sf object or path to a vector file (shapefile, gpkg, …).
#' @param probs_df data.frame with one row per basin and columns:
#'   `basin_col`, `p_below`, `p_normal`, `p_above`.
#' @param basin_col name of the basin id column shared by both inputs.
#' @param limits numeric length-2 for the fill scale limits (default c(0,1)).
#' @param palette continuous palette for probabilities (e.g. "viridis", "magma").
#' @param facet_labels named character vector to rename facets, e.g.
#'   c(p_below="Below", p_normal="Normal", p_above="Above").
#' @param title optional title.
#' @param file optional path to save the figure via ggsave (format inferred from extension).
#' @param width,height units in inches for saving (if `file` supplied).
#' @return ggplot object (invisibly if saved).
#' @keywords internal
plot_prob_maps_ <- function(
    sf_basins,
    probs_df,
    basin_col = "HYBAS_ID",
    limits = c(0,1),
    palette = c("viridis"),
    facet_labels = c(p_below = "Below", p_normal = "Normal", p_above = "Above"),
    title = NULL,
    file = NULL, width = 10, height = 6
){
  .need("ggplot2")
  sf_b <- as_sf(sf_basins)

  required_cols <- c(basin_col, "p_below", "p_normal", "p_above")
  miss <- setdiff(required_cols, names(probs_df))
  if (length(miss) > 0) {
    stop(sprintf("plot_prob_maps(): missing columns in probs_df: %s",
                 paste(miss, collapse = ", ")), call. = FALSE)
  }

  # Join geometry
  map_df <- dplyr::left_join(sf_b, probs_df, by = dplyr::join_by(!!rlang::sym(basin_col)))

  # Long format for faceting
  long_df <- tidyr::pivot_longer(
    data = map_df,
    cols = c("p_below", "p_normal", "p_above"),
    names_to = "class",
    values_to = "prob"
  ) |>
    mutate(class = base::factor(x =class,
                           levels = c( "p_above", "p_normal","p_below"),
                           labels =c( "p_above", "p_normal","p_below") ))

  # Palette: use viridis if available; fallback to default continuous
  has_viridis <- requireNamespace("viridis", quietly = TRUE)
  scale_fill_prob <- if (has_viridis) {
    viridis::scale_fill_viridis(
      option = palette[1],
      limits = limits,
      name = "Probability",
      na.value = "#f0f0f0"
    )
  } else {
    ggplot2::scale_fill_gradient(
      limits = limits,
      name = "Probability",
      low = "white", high = "steelblue",
      na.value = "#f0f0f0"
    )
  }

  p <- ggplot2::ggplot(long_df) +
    ggplot2::geom_sf(ggplot2::aes(fill = prob), color = "grey70", size = 0.1) +
    scale_fill_prob +
    ggplot2::facet_wrap(~ class, nrow = 1,
                        labeller = ggplot2::as_labeller(facet_labels)) +
    ggplot2::coord_sf(expand = FALSE) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      panel.background = ggplot2::element_rect(fill = "white"),
      plot.background = ggplot2::element_rect(fill = "white", color = NA),
      strip.background = ggplot2::element_rect(fill = "#f0f0f0", color = NA),
      strip.text = ggplot2::element_text(face = "bold", size = 12),
      axis.text.x = ggplot2::element_text(angle = 0, hjust = 1),
      legend.position = "bottom",
      legend.key.width = unit(2, "cm"),
      legend.title = ggplot2::element_text(face = "bold"),
      plot.title = ggplot2::element_text(face = "bold", size = 16, hjust = 0.5),
      plot.subtitle = ggplot2::element_text(size = 12, hjust = 0.5),
      plot.margin = ggplot2::margin(1, 1, 1, 1)
    )+
    ggplot2::labs(title = title)

  if (!is.null(file)) {
    ggplot2::ggsave(file, p, width = width, height = height, dpi = 300)
    return(invisible(p))
  }
  p
}
