#' Parse a target season into start and end months
#'
#' @param target Character string such as \code{"Jun-Sep"}, \code{"JJAS"}, or
#'   \code{"May"}.
#'
#' @return Integer vector \code{c(start_month, end_month)}.
#' @keywords internal
wass2s__parse_target_months <- function(target) {
  if (!is.character(target) || length(target) != 1L || is.na(target) || !nzchar(target)) {
    stop("target must be a single non-empty character string.", call. = FALSE)
  }

  month_map <- c(
    jan = 1L, january = 1L, j = 1L,
    feb = 2L, february = 2L, f = 2L,
    mar = 3L, march = 3L, m = 3L,
    apr = 4L, april = 4L, a = 4L,
    may = 5L,
    jun = 6L, june = 6L,
    jul = 7L, july = 7L,
    aug = 8L, august = 8L,
    sep = 9L, sept = 9L, september = 9L, s = 9L,
    oct = 10L, october = 10L, o = 10L,
    nov = 11L, november = 11L, n = 11L,
    dec = 12L, december = 12L, d = 12L
  )

  month_letters <- c(
    j = 1L, f = 2L, m = 3L, a = 4L, y = 5L, u = 6L,
    l = 7L, g = 8L, s = 9L, o = 10L, n = 11L, d = 12L
  )

  x <- tolower(trimws(target))
  x <- gsub("\\s+", "", x)
  x <- gsub("_|/", "-", x)

  compact_seasons <- list(
    djf = c(12L, 2L), jfm = c(1L, 3L), fma = c(2L, 4L),
    mam = c(3L, 5L), amj = c(4L, 6L), mjj = c(5L, 7L),
    jja = c(6L, 8L), jas = c(7L, 9L), aso = c(8L, 10L),
    son = c(9L, 11L), ond = c(10L, 12L), ndj = c(11L, 1L),
    jj = c(6L, 7L), jjas = c(6L, 9L), jjason = c(6L, 11L),
    mamj = c(3L, 6L), amjj = c(4L, 7L), mjja = c(5L, 8L)
  )
  if (x %in% names(compact_seasons)) {
    return(compact_seasons[[x]])
  }

  parts <- strsplit(x, "-", fixed = TRUE)[[1]]
  if (length(parts) == 1L && nchar(parts) %in% 2:12 && !parts %in% names(month_map)) {
    letters <- strsplit(parts, "", fixed = TRUE)[[1]]
    vals <- unname(month_letters[letters])
    if (anyNA(vals)) {
      stop("Could not parse compact target season '", target, "'. Use e.g. 'Jun-Sep'.", call. = FALSE)
    }
    return(c(vals[1], vals[length(vals)]))
  }

  if (length(parts) == 1L) parts <- rep(parts, 2L)
  if (length(parts) != 2L) {
    stop("target must be a month or a start-end season, e.g. 'Jun-Sep'.", call. = FALSE)
  }

  vals <- unname(month_map[parts])
  if (anyNA(vals)) {
    stop("Could not parse target season '", target, "'. Use English month names/abbreviations.", call. = FALSE)
  }
  as.integer(vals)
}

#' Midpoint month of a target season
#'
#' Computes the climatological midpoint month of a target season. This mirrors
#' the operational convention used by PyCPT, but is exposed as a lightweight R
#' helper for hydrological workflows.
#'
#' @param target Character target season such as \code{"Dec-Feb"},
#'   \code{"Jun-Sep"}, or \code{"May"}.
#'
#' @return Numeric month index. Fractional values indicate a midpoint inside a
#'   month, for example \code{1.5} for the middle of January.
#'
#' @examples
#' wass2s_target_midpoint("Dec-Feb")
#' wass2s_target_midpoint("Jun-Sep")
#'
#' @export
wass2s_target_midpoint <- function(target) {
  mo <- wass2s__parse_target_months(target)
  start_mo <- mo[1]
  end_mo <- mo[2]
  len <- (end_mo - start_mo) %% 12L + 1L
  (start_mo - 1 + len / 2) %% 12 + 1
}

#' Year offset between forecast issue month and target season year
#'
#' The year of a target season is defined as the year of its midpoint. If a
#' forecast is issued after the target-season midpoint month, the issue belongs
#' to the previous calendar year for that target-season year.
#'
#' @param issue_month Integer month of forecast initialization.
#' @param target Character target season such as \code{"Dec-Feb"}.
#'
#' @return Integer offset to add to the target year to obtain the issue year.
#'
#' @examples
#' wass2s_issue_year_delta(11, "Dec-Feb")
#' wass2s_issue_year_delta(5, "Jun-Sep")
#'
#' @export
wass2s_issue_year_delta <- function(issue_month, target) {
  issue_month <- as.integer(issue_month)
  if (length(issue_month) != 1L || is.na(issue_month) || issue_month < 1L || issue_month > 12L) {
    stop("issue_month must be a single integer in 1:12.", call. = FALSE)
  }
  if (issue_month > wass2s_target_midpoint(target)) -1L else 0L
}

#' Build a WASS2S run manifest
#'
#' Creates a compact, serializable manifest that records the provenance of a
#' forecast run: configuration, input files, outputs, scores and warnings. The
#' manifest is intentionally generic so it can be used by STAT, ML and HYDRO
#' workflows.
#'
#' @param run_id Optional run identifier. If \code{NULL}, a timestamped id is
#'   generated.
#' @param approach Character vector of approaches involved, for example
#'   \code{c("stat", "ml")}.
#' @param issue_date Optional issue date.
#' @param target Optional target season.
#' @param config Named list of user configuration.
#' @param inputs Character vector or named list of input paths.
#' @param outputs Character vector or named list of output paths.
#' @param scores Optional data frame or list of skill scores.
#' @param warnings Character vector of non-fatal warnings.
#' @param errors Character vector of fatal or recovered errors.
#' @param extra Named list for additional metadata.
#'
#' @return A list with class \code{"wass2s_manifest"}.
#'
#' @export
wass2s_run_manifest <- function(run_id = NULL,
                                approach = character(),
                                issue_date = NULL,
                                target = NULL,
                                config = list(),
                                inputs = list(),
                                outputs = list(),
                                scores = NULL,
                                warnings = character(),
                                errors = character(),
                                extra = list()) {
  if (is.null(run_id)) {
    run_id <- paste0("wass2s_", format(Sys.time(), "%Y%m%dT%H%M%S"))
  }
  if (!is.list(config)) stop("config must be a list.", call. = FALSE)
  if (!is.list(extra)) stop("extra must be a list.", call. = FALSE)

  manifest <- list(
    run_id = as.character(run_id),
    created_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
    package = list(
      name = "WASS2SHydroR",
      version = tryCatch(
        as.character(utils::packageVersion("WASS2SHydroR")),
        error = function(e) NA_character_
      )
    ),
    approach = as.character(approach),
    issue_date = if (is.null(issue_date)) NULL else as.character(issue_date),
    target = if (is.null(target)) NULL else as.character(target),
    config = config,
    inputs = inputs,
    outputs = outputs,
    scores = scores,
    warnings = as.character(warnings),
    errors = as.character(errors),
    extra = extra
  )
  class(manifest) <- c("wass2s_manifest", "list")
  manifest
}

#' Write a WASS2S run manifest to disk
#'
#' @param manifest A manifest produced by \code{wass2s_run_manifest()}.
#' @param path Output file path. The extension controls the format:
#'   \code{.yml}/\code{.yaml} uses \pkg{yaml}; any other extension uses RDS.
#' @param overwrite Logical. If \code{FALSE}, an existing file is not replaced.
#'
#' @return Invisibly returns \code{path}.
#'
#' @export
wass2s_write_manifest <- function(manifest, path, overwrite = FALSE) {
  if (!inherits(manifest, "wass2s_manifest")) {
    stop("manifest must be produced by wass2s_run_manifest().", call. = FALSE)
  }
  if (file.exists(path) && !isTRUE(overwrite)) {
    stop("Manifest already exists: ", path, call. = FALSE)
  }
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  ext <- tolower(tools::file_ext(path))
  if (ext %in% c("yml", "yaml")) {
    yaml::write_yaml(unclass(manifest), path)
  } else {
    saveRDS(manifest, path)
  }
  invisible(path)
}

#' Compare two forecast versions
#'
#' Scores a new and an old forecast summary using objective operational
#' criteria. Higher is better. This helper supports safe update policies similar
#' to PyCPT's "replace only if the new run is better" logic.
#'
#' @param new,old Named lists or one-row data frames containing any of:
#'   \code{kge}, \code{nse}, \code{rmse}, \code{n_models},
#'   \code{n_products}, \code{n_basins}, \code{n_valid}, \code{missing_frac}.
#' @param weights Named numeric vector controlling criterion importance.
#'
#' @return A list with \code{decision}, \code{new_score}, \code{old_score} and
#'   \code{delta}.
#'
#' @export
wass2s_compare_forecast_versions <- function(new,
                                             old,
                                             weights = c(
                                               kge = 3, nse = 2, rmse = -1,
                                               n_models = 1, n_products = 1,
                                               n_basins = 2, n_valid = 1,
                                               missing_frac = -2
                                             )) {
  score_one <- function(x) {
    if (is.data.frame(x)) x <- as.list(x[1, , drop = TRUE])
    if (!is.list(x)) stop("Forecast summaries must be lists or one-row data frames.", call. = FALSE)
    vals <- stats::setNames(rep(NA_real_, length(weights)), names(weights))
    common <- intersect(names(weights), names(x))
    for (nm in common) vals[[nm]] <- suppressWarnings(as.numeric(x[[nm]])[1])
    vals[!is.finite(vals)] <- 0
    sum(vals * weights[names(vals)], na.rm = TRUE)
  }
  ns <- score_one(new)
  os <- score_one(old)
  delta <- ns - os
  list(
    decision = if (delta > 0) "new" else if (delta < 0) "old" else "tie",
    new_score = ns,
    old_score = os,
    delta = delta
  )
}
