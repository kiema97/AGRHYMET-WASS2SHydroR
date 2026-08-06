#' Plot final class map (dominant class) with per-layer positioning
#'
#' Join a basin geometry and a table with the dominant class label, then
#' plot a categorical map using the requested colors. NAs (unknown/no forecast)
#' are colored with the climatology color.
#'
#' Extra layers can be added either:
#' 1) As a list of ggplot2 layers (all added "below" by default), or
#' 2) As a list of items with `layer` and `position` (position per layer),
#'    where position is "below" or "above".
#'
#' @param sf_basins sf object or path to a vector file (shapefile, gpkg, …).
#' @param class_df data.frame with one row per basin and columns:
#'   \code{basin_col}, \code{class_hat} in \code{c("below", "normal", "above")}.
#' @param basin_col name of the basin id column shared by both inputs.
#' @param colors named colors for classes, default:
#'   list(above="#41AB5D", normal="#A1D937", below="#EC7014", climatology="#BEBEBE").
#' @param title optional title.
#' @param file optional path to save the figure via ggsave.
#' @param width,height inches for saving (if `file` supplied).
#'
#' @param layers optional list of layers. Two supported formats:
#'   - ggplot2 layers: `list(geom_sf(...), geom_sf(...))` (all placed by `layer_position`)
#'   - positioned layers: `list(list(layer = geom_sf(...), position = "below"),
#'                             list(layer = geom_sf(...), position = "above"))`
#' @param layer_position default position for `layers` when passed as plain ggplot layers
#'   (ignored when `layers` is passed in positioned format).
#' @param sf_crop optional sf object or path used to crop the basins extent (e.g., country boundary).
#' @param basin_line_color basin border color.
#' @param basin_line_size basin border size.
#'
#' @return ggplot object (invisibly if saved).
#' @keywords internal
plot_class_map <- function(
    sf_basins,
    class_df,
    basin_col = "HYBAS_ID",
    colors = list(
      above = "#41AB5D",
      normal = "#FFFFD4",
      below = "#EC7014",
      climatology = "#BEBEBE"
    ),
    title = NULL,
    file = NULL, width = 7, height = 6,
    layers = NULL,
    layer_position = c("below", "above"),
    sf_crop = NULL,
    basin_line_color = "grey70",
    basin_line_size = 0.1
){
  .need("ggplot2")
  layer_position <- match.arg(layer_position)

  sf_b <- as_sf(sf_basins)
  sf_b[[basin_col]] <- as.character(sf_b[[basin_col]])
  if (anyDuplicated(sf_b[[basin_col]]) > 0L) {
    sf_b <- sf_b |>
      dplyr::group_by(dplyr::across(dplyr::all_of(basin_col))) |>
      dplyr::summarise(.groups = "drop")
  }

  # ---- validation: class_df ----
  required_cols <- c(basin_col, "class_hat")
  miss <- setdiff(required_cols, names(class_df))
  if (length(miss) > 0) {
    stop(sprintf(
      "plot_class_map(): missing columns in class_df: %s",
      paste(miss, collapse = ", ")
    ), call. = FALSE)
  }

  class_df[[basin_col]] <- as.character(class_df[[basin_col]])
  if (anyDuplicated(class_df[[basin_col]]) > 0L) {
    class_df <- class_df |>
      dplyr::group_by(dplyr::across(dplyr::all_of(basin_col))) |>
      dplyr::summarise(
        class_hat = {
          vals <- stats::na.omit(as.character(.data$class_hat))
          if (length(vals) == 0L) NA_character_ else names(sort(table(vals), decreasing = TRUE))[1]
        },
        .groups = "drop"
      )
  }

  # ---- join ----
  map_df <- dplyr::left_join(
    sf_b,
    class_df,
    by = dplyr::join_by(!!rlang::sym(basin_col))
  )

  map_df$class_hat <- ifelse(is.na(map_df$class_hat), "climatology", map_df$class_hat)

  # control legend order
  lvl <- c("below", "normal", "above", "climatology")
  map_df$class_hat <- factor(map_df$class_hat, levels = lvl)

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
      stop("plot_class_map(): `layers` must be a list.", call. = FALSE)
    }

    # Detect whether user provided positioned format
    is_positioned <- all(vapply(layers, function(x) {
      is.list(x) && all(c("layer", "position") %in% names(x))
    }, logical(1)))

    if (is_positioned) {
      # Validate positioned entries
      for (i in seq_along(layers)) {
        pos <- layers[[i]]$position
        if (!is.character(pos) || length(pos) != 1L || !pos %in% c("below", "above")) {
          stop(sprintf(
            "plot_class_map(): layers[[%d]]$position must be 'below' or 'above'.",
            i
          ), call. = FALSE)
        }
      }
      norm_layers <- layers
    } else {
      # Treat as plain ggplot layers: apply `layer_position` to all
      norm_layers <- lapply(layers, function(ly) list(layer = ly, position = layer_position))
    }
  }

  # Helper: add layers by position
  add_layers_by_pos <- function(p, norm_layers, pos) {
    if (is.null(norm_layers)) return(p)
    for (x in norm_layers) {
      if (identical(x$position, pos)) {
        p <- p + x$layer
      }
    }
    p
  }

  # ---- build plot ----
  p <- ggplot2::ggplot()

  # Add extra layers below basins
  p <- add_layers_by_pos(p, norm_layers, "below")

  # Basins
  p <- p +
    ggplot2::geom_sf(
      data = map_df,
      ggplot2::aes(fill = class_hat),
      color = basin_line_color,
      size  = basin_line_size
    ) +
    ggplot2::scale_fill_manual(
      values = c(
        below  = colors$below,
        normal = colors$normal,
        above  = colors$above,
        climatology = colors$climatology
      ),
      drop = TRUE,
      na.value = colors$climatology,
      name = "Class"
    )

  # Add extra layers above basins
  p <- add_layers_by_pos(p, norm_layers, "above")

  # Coord
  if (!is.null(coord_limits)) {
    p <- p + ggplot2::coord_sf(
      xlim = coord_limits$xlim,
      ylim = coord_limits$ylim,
      expand = FALSE
    )
  } else {
    p <- p + ggplot2::coord_sf(expand = TRUE)
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


#' Plot final class map (dominant class)
#'
#' Join a basin geometry and a table with the dominant class label, then
#' plot a categorical map using the requested colors. NAs (unknown/no forecast)
#' are colored with the climatology color.
#'
#' @param sf_basins sf object or path to a vector file (shapefile, gpkg, …).
#' @param class_df data.frame with one row per basin and columns:
#'   \code{basin_col}, \code{class_hat} in \code{c("below", "normal", "above")}.
#' @param basin_col name of the basin id column shared by both inputs.
#' @param colors named colors for classes, default:
#'   list(above="#41AB5D", normal="#A1D937", below="#EC7014", climatology="#BEBEBE").
#' @param title optional title.
#' @param file optional path to save the figure via ggsave.
#' @param width,height inches for saving (if `file` supplied).
#' @return ggplot object (invisibly if saved).
#' @keywords internal
.plot_class_map <- function(
    sf_basins,
    class_df,
    basin_col = "HYBAS_ID",
    colors = list(
      above = "#41AB5D",
      normal = "#A1D937",
      below = "#EC7014",
      climatology = "#BEBEBE"),
    title = NULL,
    file = NULL, width = 7, height = 6
){
  .need("ggplot2")
  sf_b <- as_sf(sf_basins)

  required_cols <- c(basin_col, "class_hat")
  miss <- setdiff(required_cols, names(class_df))
  if (length(miss) > 0) {
    stop(sprintf("plot_class_map(): missing columns in class_df: %s",
                 paste(miss, collapse = ", ")), call. = FALSE)
  }

  map_df <- dplyr::left_join(sf_b, class_df, by = dplyr::join_by(!!rlang::sym(basin_col)))
  map_df$class_hat <- base::ifelse(is.na(map_df$class_hat),"climatology",map_df$class_hat)

  # make class_hat a factor to control legend order
  lvl <- c("below","normal","above","climatology")
  map_df$class_hat <- factor(map_df$class_hat, levels = lvl)

  p <- ggplot2::ggplot(map_df) +
    ggplot2::geom_sf(ggplot2::aes(fill = class_hat), color = "grey70", size = 0.1) +
    ggplot2::scale_fill_manual(
      values = c(
        below  = colors$below,
        normal = colors$normal,
        above  = colors$above,
        climatology = colors$climatology
      ),
      drop = FALSE,
      na.value = colors$climatology,
      name = "Class"
    ) +
    #ggplot2::coord_sf(expand = TRUE) +
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
