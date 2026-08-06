.wass2s_skill_map_data <- function(x,
                                   sf_basins,
                                   metrics = NULL,
                                   basin_col = "HYBAS_ID",
                                   report_basin_col = NULL,
                                   approach = NULL) {
  .need("sf")

  df <- .wass2s_skill_matrix_source(x, approach = approach)
  if (!is.data.frame(df) || nrow(df) == 0L) {
    stop("No rows available for skill map plotting.", call. = FALSE)
  }

  if (is.null(report_basin_col)) {
    report_basin_col <- intersect(c(basin_col, "HYBAS_ID", "id", "basin", "basin_id"), names(df))[1]
  }
  if (is.na(report_basin_col) || !nzchar(report_basin_col) || !report_basin_col %in% names(df)) {
    stop("report_basin_col must identify an existing basin identifier column in x.", call. = FALSE)
  }

  if (is.character(sf_basins) && length(sf_basins) == 1L) {
    sf_basins <- sf::st_read(sf_basins, quiet = TRUE)
  }
  if (!inherits(sf_basins, "sf")) {
    stop("sf_basins must be an sf object or a path readable by sf::st_read().", call. = FALSE)
  }
  if (!basin_col %in% names(sf_basins)) {
    stop("basin_col must identify an existing column in sf_basins.", call. = FALSE)
  }

  if (is.null(metrics)) {
    metrics <- intersect(
      c(
        "test_kge", "train_kge", "train_test_kge_gap",
        "test_rmse", "train_rmse", "test_train_rmse_ratio",
        "probabilistic_rpss", "probabilistic_accuracy",
        "overfit_flag", "used_test_for_selection", "n_warnings"
      ),
      names(df)
    )
  }
  metrics <- intersect(metrics, names(df))
  if (length(metrics) == 0L) {
    stop("No requested metrics were found in x.", call. = FALSE)
  }

  keep <- unique(c(report_basin_col, metrics))
  dat <- df[, keep, drop = FALSE]
  names(dat)[names(dat) == report_basin_col] <- ".basin_id"
  dat$.basin_id <- as.character(dat$.basin_id)
  dat <- dat[!duplicated(dat$.basin_id), , drop = FALSE]

  long <- tidyr::pivot_longer(
    dat,
    cols = dplyr::all_of(metrics),
    names_to = "metric",
    values_to = "value"
  )
  directions <- stats::setNames(.wass2s_skill_metric_direction(metrics), metrics)
  long$direction <- unname(directions[long$metric])
  long$value_numeric <- suppressWarnings(as.numeric(long$value))
  long$value_label <- .wass2s_format_skill_value(long$value)
  long$score <- NA_real_
  for (idx in split(seq_len(nrow(long)), long$metric)) {
    long$score[idx] <- .wass2s_rescale01(
      long$value_numeric[idx],
      direction = long$direction[idx][1]
    )
  }

  sf_join <- sf_basins
  sf_join[[basin_col]] <- as.character(sf_join[[basin_col]])
  out <- dplyr::left_join(sf_join, long, by = stats::setNames(".basin_id", basin_col))
  out$metric <- factor(out$metric, levels = metrics)
  out
}

#' Plot WASS2S skill maps
#'
#' Creates spatial skill diagnostics from a WASS2S forecast result,
#' \code{wass2s_run_report}, or data frame. The function joins the run summary to
#' basin geometries and draws one faceted map per selected metric.
#'
#' Map colors use the same scientific convention as
#' \code{wass2s_plot_skill_matrix()}: metrics are normalized within each metric,
#' green is better, red is worse, and metrics where lower values are better
#' (for example RMSE or overfit indicators) are automatically reversed.
#'
#' @param x A WASS2S result object, a \code{wass2s_run_report}, or a data frame.
#' @param sf_basins sf object or path to a vector file readable by
#'   \code{sf::st_read()}.
#' @param metrics Character vector of metric columns to map. If \code{NULL},
#'   common deterministic, probabilistic and diagnostic metrics are detected.
#' @param basin_col Basin identifier column in \code{sf_basins}. Default is
#'   \code{"HYBAS_ID"}.
#' @param report_basin_col Basin identifier column in \code{x}. If \code{NULL},
#'   the function tries \code{basin_col}, \code{"HYBAS_ID"}, \code{"id"},
#'   \code{"basin"}, then \code{"basin_id"}.
#' @param approach Optional approach label passed to \code{wass2s_run_report()}
#'   when \code{x} is a raw WASS2S result object.
#' @param layers Optional list of additional ggplot2 layers added after basin
#'   polygons, for example country borders or rivers.
#' @param sf_crop Optional sf object or path used to define map extent.
#' @param title Optional plot title.
#' @param file Optional path passed to \code{ggplot2::ggsave()}.
#' @param width,height Plot size in inches when saving.
#' @param ncol Number of facet columns.
#'
#' @return A \code{ggplot2} object, invisibly when saved to \code{file}.
#'
#' @examples
#' \dontrun{
#' report <- wass2s_run_report(res, approach = "ML")
#' wass2s_plot_skill_maps(report, sf_basins)
#' }
#'
#' @export
wass2s_plot_skill_maps <- function(x,
                                   sf_basins,
                                   metrics = NULL,
                                   basin_col = "HYBAS_ID",
                                   report_basin_col = NULL,
                                   approach = NULL,
                                   layers = NULL,
                                   sf_crop = NULL,
                                   title = "WASS2S Skill Maps",
                                   file = NULL,
                                   width = NULL,
                                   height = NULL,
                                   ncol = NULL) {
  .need("ggplot2")
  .need("sf")

  map_data <- .wass2s_skill_map_data(
    x = x,
    sf_basins = sf_basins,
    metrics = metrics,
    basin_col = basin_col,
    report_basin_col = report_basin_col,
    approach = approach
  )

  p <- ggplot2::ggplot(map_data) +
    ggplot2::geom_sf(ggplot2::aes(fill = .data$score), color = "grey45", linewidth = 0.15) +
    ggplot2::scale_fill_gradient2(
      low = "#b2182b",
      mid = "#f7f7f7",
      high = "#1a9850",
      midpoint = 0.5,
      limits = c(0, 1),
      na.value = "#e5e7eb",
      name = "Relative skill"
    ) +
    ggplot2::facet_wrap(~ metric, ncol = ncol) +
    ggplot2::labs(
      title = title,
      x = NULL,
      y = NULL,
      caption = "Colors are normalized within each metric: green is better, red is worse. Grey means missing skill."
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      panel.grid.major = ggplot2::element_line(color = "grey90", linewidth = 0.2),
      strip.text = ggplot2::element_text(face = "bold"),
      plot.title = ggplot2::element_text(face = "bold"),
      plot.caption = ggplot2::element_text(color = "grey35")
    )

  if (!is.null(layers)) {
    for (layer in layers) p <- p + layer
  }

  if (!is.null(sf_crop)) {
    if (is.character(sf_crop) && length(sf_crop) == 1L) {
      sf_crop <- sf::st_read(sf_crop, quiet = TRUE)
    }
    bb <- sf::st_bbox(sf_crop)
    p <- p + ggplot2::coord_sf(
      xlim = c(unname(bb["xmin"]), unname(bb["xmax"])),
      ylim = c(unname(bb["ymin"]), unname(bb["ymax"])),
      expand = FALSE
    )
  }

  if (!is.null(file)) {
    n_metrics <- length(stats::na.omit(unique(map_data$metric)))
    if (is.null(width)) width <- 9.5
    if (is.null(height)) height <- max(5.5, 2.8 * ceiling(n_metrics / max(1L, ncol %||% 2L)))
    ggplot2::ggsave(file, p, width = width, height = height, dpi = 300, bg = "white")
    return(invisible(p))
  }

  p
}