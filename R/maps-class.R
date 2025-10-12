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
plot_class_map <- function(
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

  # make class_hat a factor to control legend order
  lvl <- c("below","normal","above")
  map_df$class_hat <- factor(map_df$class_hat, levels = lvl)

  p <- ggplot2::ggplot(map_df) +
    ggplot2::geom_sf(ggplot2::aes(fill = class_hat), color = "grey70", size = 0.1) +
    ggplot2::scale_fill_manual(
      values = c(
        below  = colors$below,
        normal = colors$normal,
        above  = colors$above
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
