.wass2s_skill_metric_direction <- function(metrics) {
  lower_is_better <- grepl("rmse|mae|error|brier|rps$|gap|ratio|overfit|missing|entropy", metrics, ignore.case = TRUE)
  ifelse(lower_is_better, "lower", "higher")
}

.wass2s_skill_matrix_source <- function(x, approach = NULL) {
  if (inherits(x, "wass2s_run_report")) {
    return(x$summary)
  }
  if (is.data.frame(x)) {
    return(x)
  }
  if (is.list(x)) {
    return(wass2s_run_report(x, approach = approach)$summary)
  }
  stop("x must be a WASS2S result, a wass2s_run_report, or a data.frame.", call. = FALSE)
}

.wass2s_rescale01 <- function(x, direction = c("higher", "lower")) {
  direction <- match.arg(direction)
  x <- as.numeric(x)
  out <- rep(NA_real_, length(x))
  ok <- is.finite(x)
  if (!any(ok)) return(out)
  rng <- range(x[ok], na.rm = TRUE)
  if (!is.finite(diff(rng)) || diff(rng) == 0) {
    out[ok] <- 0.5
  } else {
    out[ok] <- (x[ok] - rng[1]) / diff(rng)
  }
  if (identical(direction, "lower")) out[ok] <- 1 - out[ok]
  out
}

.wass2s_format_skill_value <- function(x) {
  if (is.logical(x)) return(ifelse(is.na(x), "NA", ifelse(x, "yes", "no")))
  x_num <- suppressWarnings(as.numeric(x))
  out <- ifelse(is.finite(x_num), format(round(x_num, 3), trim = TRUE, nsmall = 0), as.character(x))
  out[is.na(out) | out == "NA"] <- ""
  out
}

.wass2s_skill_matrix_data <- function(x,
                                      metrics = NULL,
                                      row_col = NULL,
                                      group_col = NULL,
                                      approach = NULL) {
  df <- .wass2s_skill_matrix_source(x, approach = approach)
  if (!is.data.frame(df) || nrow(df) == 0L) {
    stop("No rows available for skill matrix plotting.", call. = FALSE)
  }

  if (is.null(row_col)) {
    row_col <- intersect(c("model", "method", "selected_fusion_method", "id", "approach"), names(df))[1]
  }
  if (is.na(row_col) || !nzchar(row_col) || !row_col %in% names(df)) {
    stop("row_col must identify an existing column.", call. = FALSE)
  }

  if (is.null(metrics)) {
    metrics <- intersect(
      c(
        "test_kge", "train_kge", "train_test_kge_gap",
        "test_rmse", "train_rmse", "test_train_rmse_ratio",
        "probabilistic_rpss", "probabilistic_accuracy",
        "overfit_flag", "used_test_for_selection"
      ),
      names(df)
    )
  }
  metrics <- intersect(metrics, names(df))
  if (length(metrics) == 0L) {
    stop("No requested metrics were found in x.", call. = FALSE)
  }

  if (is.null(group_col)) {
    group_col <- intersect(c("approach", "split"), names(df))[1]
  }
  if (is.na(group_col) || identical(group_col, row_col) || !group_col %in% names(df)) {
    group_col <- NULL
  }

  keep <- unique(c(row_col, group_col, metrics))
  long <- df[, keep, drop = FALSE] |>
    tidyr::pivot_longer(
      cols = dplyr::all_of(metrics),
      names_to = "metric",
      values_to = "value"
    )

  long$row_label <- as.character(long[[row_col]])
  if (!is.null(group_col)) long$group_label <- as.character(long[[group_col]])

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

  long
}

#' Plot a WASS2S skill matrix
#'
#' Creates a compact diagnostic matrix inspired by PyCPT's model-by-metric skill
#' plots. The function accepts a WASS2S forecast result, a
#' \code{wass2s_run_report}, or a data frame. Metrics are shown as columns and
#' models, methods, basins, or user-selected rows are shown as rows.
#'
#' Because metrics such as KGE, RMSE and RPSS have different units and
#' directions, tile colors use a within-metric normalized score: green is better,
#' red is worse, and the original metric value is printed inside each tile.
#'
#' @param x A WASS2S result object, a \code{wass2s_run_report}, or a data frame.
#' @param metrics Character vector of metric columns to display. If \code{NULL},
#'   common deterministic, probabilistic and diagnostic metrics are detected.
#' @param row_col Column used for matrix rows. If \code{NULL}, the function tries
#'   \code{model}, \code{method}, \code{selected_fusion_method}, \code{id}, then
#'   \code{approach}.
#' @param group_col Optional column used for faceting, for example
#'   \code{approach}. If \code{NULL}, a useful grouping column is detected when
#'   available.
#' @param approach Optional approach label passed to \code{wass2s_run_report()}
#'   when \code{x} is a raw WASS2S result object.
#' @param title Optional plot title.
#' @param file Optional path passed to \code{ggplot2::ggsave()}.
#' @param width,height Plot size in inches when saving.
#'
#' @return A \code{ggplot2} object, invisibly when saved to \code{file}.
#'
#' @examples
#' \dontrun{
#' report <- wass2s_run_report(res, approach = "ML")
#' wass2s_plot_skill_matrix(report)
#' }
#'
#' @export
wass2s_plot_skill_matrix <- function(x,
                                     metrics = NULL,
                                     row_col = NULL,
                                     group_col = NULL,
                                     approach = NULL,
                                     title = "WASS2S Skill Matrix",
                                     file = NULL,
                                     width = NULL,
                                     height = NULL) {
  .need("ggplot2")

  long <- .wass2s_skill_matrix_data(
    x = x,
    metrics = metrics,
    row_col = row_col,
    group_col = group_col,
    approach = approach
  )

  long$metric <- factor(long$metric, levels = unique(long$metric))
  long$row_label <- factor(long$row_label, levels = rev(unique(long$row_label)))

  p <- ggplot2::ggplot(long, ggplot2::aes(x = .data$metric, y = .data$row_label, fill = .data$score)) +
    ggplot2::geom_tile(color = "white", linewidth = 0.6) +
    ggplot2::geom_text(ggplot2::aes(label = .data$value_label), size = 3, na.rm = TRUE) +
    ggplot2::scale_fill_gradient2(
      low = "#b2182b",
      mid = "#f7f7f7",
      high = "#1a9850",
      midpoint = 0.5,
      limits = c(0, 1),
      na.value = "#e5e7eb",
      name = "Relative skill"
    ) +
    ggplot2::labs(
      title = title,
      x = NULL,
      y = NULL,
      caption = "Colors are normalized within each metric: green is better, red is worse."
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(angle = 35, hjust = 1),
      axis.text.y = ggplot2::element_text(face = "bold"),
      plot.title = ggplot2::element_text(face = "bold"),
      plot.caption = ggplot2::element_text(color = "grey35")
    )

  if ("group_label" %in% names(long)) {
    p <- p + ggplot2::facet_wrap(~ group_label, scales = "free_y")
  }

  if (!is.null(file)) {
    if (is.null(width)) width <- max(7, 1.2 * length(unique(long$metric)) + 3)
    if (is.null(height)) height <- max(4, 0.45 * length(unique(long$row_label)) + 2.5)
    ggplot2::ggsave(file, p, width = width, height = height, dpi = 300)
    return(invisible(p))
  }

  p
}
