#' Detect high-flow periods from daily discharge time series
#'
#' Detects one or several high-flow periods within each basin, either over the
#' full record or within each analysis year (calendar or hydrological year).
#' High-flow periods are identified using either a quantile-based threshold
#' or a z-score threshold, with optional smoothing and merging of short gaps.
#'
#' The function can return all detected periods or only a selected period rank
#' (e.g. first, second, third) within each detection unit. This is particularly
#' useful for rivers with bimodal or multimodal flow regimes.
#'
#' @param data A data frame containing at least an identifier column, a date
#'   column, and a discharge column.
#' @param id_col Character string. Name of the station or basin identifier column.
#' @param date_col Character string. Name of the date column.
#' @param flow_col Character string. Name of the discharge column.
#' @param year_type Character string. Either `"calendar"` or `"hydro"`.
#' @param hydro_year_start_month Integer. Starting month of the hydrological year
#'   when `year_type = "hydro"`. Default is `7`.
#' @param detection_scope Character string. Detection scope:
#'   `"id_year"` or `"id"`.
#' @param threshold_method Character string. Thresholding method:
#'   `"quantile"` or `"zscore"`.
#' @param q_prob Numeric. Quantile probability used when
#'   `threshold_method = "quantile"`. Default is `0.80`.
#' @param z_thr Numeric. Z-score threshold multiplier used when
#'   `threshold_method = "zscore"`. Default is `1.0`.
#' @param smooth_k Integer. Rolling mean window size. Use `1` for no smoothing.
#'   Odd values are recommended.
#' @param min_len_days Integer. Minimum duration required to retain a period.
#' @param max_gap_days Integer. Maximum gap allowed to merge two segments.
#' @param min_obs Integer. Minimum number of non-missing observations required
#'   within a detection group.
#' @param period_rank Either `"all"` or a positive integer indicating which
#'   detected period to return within each group.
#' @param period_order Character string indicating how detected periods should
#'   be ranked: `"chronological"`, `"magnitude"`, or `"duration"`.
#' @param na_rm Logical. Whether to ignore missing values when computing thresholds.
#'
#' @return A data frame with one row per detected high-flow period.
#'
#' @export
wass2s_detect_high_flow_periods <- function(
    data,
    id_col = "station_id",
    date_col = "date",
    flow_col = "Q",
    year_type = c("calendar", "hydro"),
    hydro_year_start_month = 7L,
    detection_scope = c("id_year", "id"),
    threshold_method = c("quantile", "zscore"),
    q_prob = 0.50,
    z_thr = 1.0,
    smooth_k = 1L,
    min_len_days = 5L,
    max_gap_days = 3L,
    min_obs = 10L,
    period_rank = "all",
    period_order = c("chronological", "magnitude", "duration"),
    na_rm = TRUE
) {
  year_type <- match.arg(year_type)
  detection_scope <- match.arg(detection_scope)
  threshold_method <- match.arg(threshold_method)
  period_order <- match.arg(period_order)

  if (!is.data.frame(data)) {
    stop("`data` must be a data.frame.", call. = FALSE)
  }

  cols_needed <- c(id_col, date_col, flow_col)
  missing_cols <- setdiff(cols_needed, names(data))
  if (length(missing_cols) > 0) {
    stop(
      "Missing required column(s): ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }

  if (!(identical(period_rank, "all") ||
        (is.numeric(period_rank) && length(period_rank) == 1L &&
         !is.na(period_rank) && period_rank >= 1))) {
    stop("`period_rank` must be 'all' or a single positive integer.", call. = FALSE)
  }

  period_rank_is_all <- identical(period_rank, "all")
  if (!period_rank_is_all) {
    period_rank <- as.integer(period_rank)
  }

  as_date_safe <- function(x) {
    if (inherits(x, "Date")) return(x)
    as.Date(x)
  }

  roll_mean_centered <- function(x, k) {
    k <- as.integer(k)
    if (k <= 1L) return(as.numeric(x))
    if (k %% 2L == 0L) {
      warning("`smooth_k` is even; odd values are recommended.")
    }
    as.numeric(stats::filter(x, rep(1 / k, k), sides = 2))
  }

  compute_analysis_year <- function(dates, type, hydro_start_month) {
    yy <- as.integer(format(dates, "%Y"))
    if (type == "calendar") return(yy)
    mm <- as.integer(format(dates, "%m"))
    yy + (mm >= hydro_start_month)
  }

  empty_result <- function() {
    data.frame(
      id = character(0),
      year = integer(0),
      period_id = integer(0),
      period_rank = integer(0),
      start_date = as.Date(character(0)),
      end_date = as.Date(character(0)),
      n_days = integer(0),
      peak_flow = numeric(0),
      mean_flow = numeric(0),
      threshold = numeric(0),
      detection_scope = character(0),
      stringsAsFactors = FALSE
    )
  }

  x <- data[, cols_needed, drop = FALSE]
  x[[date_col]] <- as_date_safe(x[[date_col]])

  if (anyNA(x[[date_col]])) {
    stop("Some values in `date_col` could not be converted to Date.", call. = FALSE)
  }

  x$..analysis_year <- compute_analysis_year(
    dates = x[[date_col]],
    type = year_type,
    hydro_start_month = hydro_year_start_month
  )

  ord <- order(x[[id_col]], x$..analysis_year, x[[date_col]])
  x <- x[ord, , drop = FALSE]

  x$..group_key <- if (detection_scope == "id_year") {
    paste(x[[id_col]], x$..analysis_year, sep = "___")
  } else {
    as.character(x[[id_col]])
  }

  groups <- split(seq_len(nrow(x)), x$..group_key)
  result_list <- vector("list", length(groups))

  for (i in seq_along(groups)) {
    idx <- groups[[i]]
    g <- x[idx, , drop = FALSE]

    q_raw <- g[[flow_col]]
    q_valid <- if (na_rm) q_raw[!is.na(q_raw)] else q_raw

    if (length(q_valid) < min_obs) {
      result_list[[i]] <- NULL
      next
    }

    q_smooth <- roll_mean_centered(q_raw, smooth_k)

    threshold <- switch(
      threshold_method,
      quantile = stats::quantile(
        q_valid,
        probs = q_prob,
        na.rm = na_rm,
        names = FALSE,
        type = 7
      ),
      zscore = {
        mu <- mean(q_valid, na.rm = na_rm)
        sdv <- stats::sd(q_valid, na.rm = na_rm)
        if (is.na(sdv) || sdv == 0) Inf else mu + z_thr * sdv
      }
    )

    is_high <- !is.na(q_smooth) & (q_smooth >= threshold)

    runs <- rle(is_high)
    run_ends <- cumsum(runs$lengths)
    run_starts <- run_ends - runs$lengths + 1L
    high_runs <- which(runs$values)

    if (length(high_runs) == 0L) {
      result_list[[i]] <- NULL
      next
    }

    seg_starts <- run_starts[high_runs]
    seg_ends <- run_ends[high_runs]

    if (max_gap_days > 0L && length(seg_starts) > 1L) {
      merged_starts <- seg_starts[1]
      merged_ends <- seg_ends[1]

      for (j in 2:length(seg_starts)) {
        gap_size <- seg_starts[j] - merged_ends[length(merged_ends)] - 1L
        if (!is.na(gap_size) && gap_size <= max_gap_days) {
          merged_ends[length(merged_ends)] <- seg_ends[j]
        } else {
          merged_starts <- c(merged_starts, seg_starts[j])
          merged_ends <- c(merged_ends, seg_ends[j])
        }
      }

      seg_starts <- merged_starts
      seg_ends <- merged_ends
    }

    seg_lengths <- seg_ends - seg_starts + 1L
    keep <- seg_lengths >= min_len_days

    seg_starts <- seg_starts[keep]
    seg_ends <- seg_ends[keep]
    seg_lengths <- seg_lengths[keep]

    if (length(seg_starts) == 0L) {
      result_list[[i]] <- NULL
      next
    }

    seg_peak <- vapply(
      seq_along(seg_starts),
      function(k) max(q_raw[seg_starts[k]:seg_ends[k]], na.rm = TRUE),
      numeric(1)
    )

    seg_mean <- vapply(
      seq_along(seg_starts),
      function(k) mean(q_raw[seg_starts[k]:seg_ends[k]], na.rm = TRUE),
      numeric(1)
    )

    res_i <- data.frame(
      id = as.character(g[[id_col]][1]),
      year = if (detection_scope == "id_year") g$..analysis_year[1] else NA_integer_,
      period_id = seq_along(seg_starts),
      start_date = g[[date_col]][seg_starts],
      end_date = g[[date_col]][seg_ends],
      n_days = seg_lengths,
      peak_flow = seg_peak,
      mean_flow = seg_mean,
      threshold = as.numeric(threshold),
      detection_scope = detection_scope,
      stringsAsFactors = FALSE
    )

    res_i <- switch(
      period_order,
      chronological = res_i[order(res_i$start_date), , drop = FALSE],
      magnitude     = res_i[order(-res_i$peak_flow, res_i$start_date), , drop = FALSE],
      duration      = res_i[order(-res_i$n_days, res_i$start_date), , drop = FALSE]
    )

    res_i$period_rank <- seq_len(nrow(res_i))

    if (!period_rank_is_all) {
      res_i <- res_i[res_i$period_rank == period_rank, , drop = FALSE]
      if (nrow(res_i) == 0L) {
        result_list[[i]] <- NULL
        next
      }
    }

    result_list[[i]] <- res_i
  }

  result <- do.call(rbind, result_list)

  if (is.null(result) || nrow(result) == 0L) {
    return(empty_result())
  }

  rownames(result) <- NULL
  result
}
