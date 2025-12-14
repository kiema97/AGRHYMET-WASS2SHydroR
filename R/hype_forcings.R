# =============================================================================
# WASS2SHydroR - HYPE forcings / OBS preparation
# =============================================================================

#' @keywords internal
#' @noRd
wass2s__read_obs <- function(path) {
  path <- as.character(path)
  if (!fs::file_exists(path)) rlang::abort(glue::glue("OBS file not found: {path}"))

  ext <- tolower(tools::file_ext(path))

  if (identical(ext, "csv")) {
    return(read.csv(path))
  }

  if (identical(ext, "txt")) {
    # Robust default for whitespace-separated tables
    return(read.table(path, header = TRUE, sep = "\t", stringsAsFactors = FALSE))
  }

  rlang::abort(glue::glue("Unsupported OBS file extension: {ext}. Supported: .csv, .txt"))
}

#' Discover available model IDs from a forcing directory
#'
#' It searches for files named '<model>_(PRCP|TMAX|TMIN|TMEAN).txt' and returns unique <model>.
#'
#' @param forcing_dir Directory containing forcing files.
#'
#' @keywords internal
#' @noRd
wass2s_hype_get_model_ids <- function(forcing_dir) {
  forcing_dir <- as.character(forcing_dir)
  if (!fs::dir_exists(forcing_dir)) rlang::abort(glue::glue("forcing_dir not found: {forcing_dir}"))

  files <- fs::dir_ls(forcing_dir, regexp = "_(PRCP|TMAX|TMIN|TMEAN)\\.txt$", type = "file", fail = FALSE)
  if (!length(files)) return(character())

  unique(sub("_(PRCP|TMAX|TMIN|TMEAN)\\.txt$", "", basename(files)))
}

#' Write the four OBS files required by HYPE (Pobs/TMAXobs/TMINobs/Tobs)
#'
#' @param run_dir Directory where the HYPE obs files will be written.
#' @param obs_list Named list containing `prcp`, `tmax`, `tmin`, `tmean` data frames.
#'   Each must have a first column representing dates/timestamps (will be treated as DATE).
#' @param obsid_override Optional integer vector of station IDs to use in WriteObs.
#'   If NULL, IDs are derived from column names by removing a leading 'X' (e.g., X123 -> 123).
#' @param round_digits Number of digits used by `HYPEtools::WriteObs`.
#' @param ... Extra parameters passed to `HYPEtools::WriteObs`.
#'
#' @keywords internal
#' @noRd
wass2s_hype_write_obs_set <- function(run_dir,
                                      obs_list,
                                      obsid_override = NULL,
                                      round_digits = 2,
                                      ...) {

  run_dir <- as.character(run_dir)
  fs::dir_create(run_dir, recurse = TRUE)

  prcp  <- obs_list$prcp
  tmax  <- obs_list$tmax
  tmin  <- obs_list$tmin
  tmean <- obs_list$tmean

  if (is.null(prcp) || is.null(tmax) || is.null(tmin) || is.null(tmean)) {
    rlang::abort("obs_list must contain prcp, tmax, tmin, tmean.")
  }

  derive_ids <- function(df) {
    if (ncol(df) < 2) rlang::abort("OBS data frame must have at least 2 columns (DATE + stations).")
    as.integer(sub("^X", "", names(df)[-1]))
  }

  if (is.null(obsid_override)) {
    obsid_prcp  <- derive_ids(prcp)
    obsid_tmax  <- derive_ids(tmax)
    obsid_tmin  <- derive_ids(tmin)
    obsid_tmean <- derive_ids(tmean)
  } else {
    obsid_override <- as.integer(obsid_override)
    n <- ncol(prcp) - 1
    if (length(obsid_override) != n) {
      rlang::abort(glue::glue("obsid_override length ({length(obsid_override)}) does not match number of stations ({n})."))
    }
    obsid_prcp <- obsid_tmax <- obsid_tmin <- obsid_tmean <- obsid_override
  }

  # Write files (suppress warnings for some formats, as you already did)
  suppressWarnings(HYPEtools::WriteObs(x = prcp,  filename = fs::path(run_dir, "Pobs.txt"),    round = round_digits, obsid = obsid_prcp, ...))
  suppressWarnings(HYPEtools::WriteObs(x = tmax,  filename = fs::path(run_dir, "TMAXobs.txt"), round = round_digits, obsid = obsid_tmax, ...))
  suppressWarnings(HYPEtools::WriteObs(x = tmin,  filename = fs::path(run_dir, "TMINobs.txt"), round = round_digits, obsid = obsid_tmin, ...))
  suppressWarnings(HYPEtools::WriteObs(x = tmean, filename = fs::path(run_dir, "Tobs.txt"),    round = round_digits, obsid = obsid_tmean, ...))

  invisible(list(
    Pobs    = fs::path(run_dir, "Pobs.txt"),
    TMAXobs = fs::path(run_dir, "TMAXobs.txt"),
    TMINobs = fs::path(run_dir, "TMINobs.txt"),
    Tobs    = fs::path(run_dir, "Tobs.txt")
  ))
}

#' Prepare (and optionally write) the four OBS time series required by HYPE
#'
#' This function reads forcing files from `forcing_dir` using a flexible naming scheme:
#' e.g. `<prefix>PRCP.txt`, `<prefix>TMAX.txt`, `<prefix>TMIN.txt`, `<prefix>TMEAN.txt`.
#' It can optionally filter by date range and write `Pobs.txt`, `TMAXobs.txt`, `TMINobs.txt`, `Tobs.txt`
#' directly to `run_dir`.
#'
#' @param forcing_dir Directory containing forcing files.
#' @param run_dir Directory where HYPE expects obs files (and where they can be written).
#' @param obsid Optional station IDs to pass to `HYPEtools::WriteObs`. If NULL, derived from column names.
#' @param prefix Optional prefix prepended to each pattern (e.g., `"ecmwf_"`).
#' @param patterns Character vector of length 4. Default: c("PRCP","TMAX","TMIN","TMEAN").
#' @param extension File extension (default ".txt").
#' @param date_from Optional start date (inclusive) for filtering (Date or coercible).
#' @param date_to Optional end date (inclusive) for filtering (Date or coercible).
#' @param write Logical; if TRUE, writes obs files into `run_dir`.
#' @param round_digits Digits for `WriteObs`.
#' @param ... Extra args forwarded to `HYPEtools::WriteObs`.
#'
#' @keywords internal
#' @noRd
wass2s_hype_prepare_obs <- function(forcing_dir,
                                    run_dir,
                                    obsid = NULL,
                                    prefix = NULL,
                                    patterns = c("PRCP","TMAX","TMIN","TMEAN"),
                                    extension = ".txt",
                                    date_from = NULL,
                                    date_to = NULL,
                                    write = TRUE,
                                    round_digits = 2,
                                    ...) {

  forcing_dir <- as.character(forcing_dir)
  run_dir <- as.character(run_dir)

  if (!fs::dir_exists(forcing_dir)) rlang::abort(glue::glue("forcing_dir not found: {forcing_dir}"))
  if (!startsWith(extension, ".")) extension <- paste0(".", extension)
  if (length(patterns) != 4) rlang::abort("patterns must be a character vector of length 4.")

  make_name <- function(pat) {
    fname <- paste0(pat, extension)
    if (!is.null(prefix) && nzchar(prefix)) fname <- paste0(prefix, fname)
    fs::path(forcing_dir, fname)
  }

  f_prcp  <- make_name(patterns[1])
  f_tmax  <- make_name(patterns[2])
  f_tmin  <- make_name(patterns[3])
  f_tmean <- make_name(patterns[4])

  files_ok <- fs::file_exists(c(f_prcp, f_tmax, f_tmin, f_tmean))
  if (!all(files_ok)) {
    missing <- c(f_prcp, f_tmax, f_tmin, f_tmean)[!files_ok]
    rlang::abort(glue::glue("Missing forcing files in {forcing_dir}: {paste(basename(missing), collapse=', ')}"))
  }

  read_and_filter <- function(fp) {
    df <- wass2s__read_obs(fp)
    names(df)[1] <- "DATE"

    # Normalize DATE
    df$DATE <- as.Date(df$DATE)

    if (!is.null(date_from) && !is.null(date_to)) {
      df <- dplyr::filter(df, DATE >= as.Date(date_from) & DATE <= as.Date(date_to))
    }
    df
  }

  prcp  <- read_and_filter(f_prcp)
  tmax  <- read_and_filter(f_tmax)
  tmin  <- read_and_filter(f_tmin)
  tmean <- read_and_filter(f_tmean)

  data_list <- list(prcp = prcp, tmax = tmax, tmin = tmin, tmean = tmean)
  files_list <- NULL

  if (isTRUE(write)) {
    files_list <- wass2s_hype_write_obs_set(
      run_dir = run_dir,
      obs_list = data_list,
      obsid_override = obsid,
      round_digits = round_digits,
      ...
    )
  }

  invisible(list(data = data_list, files = files_list))
}

#' Update `info.txt` resultdir and ensure directory exists
#'
#' This function sets `resultdir` in `info.txt` to `"./<resultdir>/"` and creates
#' the corresponding folder inside `run_dir`.
#'
#' @param run_dir HYPE run directory (containing info.txt).
#' @param resultdir Result directory name (e.g., "ecmwf" or a model ID).
#'
#' @keywords internal
#' @noRd
wass2s_hype_update_resultdir <- function(run_dir, resultdir) {
  run_dir <- as.character(run_dir)
  resultdir <- as.character(resultdir)

  # Basic sanitation: remove slashes and backslashes
  resultdir <- stringr::str_replace_all(resultdir, "[/\\\\]+", "")
  if (!nzchar(resultdir)) rlang::abort("resultdir must be a non-empty string.")

  info_path <- fs::path(run_dir, "info.txt")
  if (!fs::file_exists(info_path)) rlang::abort(glue::glue("info.txt not found in {run_dir}"))

  info <- HYPEtools::ReadInfo(filename = info_path, mode = "exact")
  info$resultdir <- glue::glue("./{resultdir}/")

  out_dir <- fs::path(run_dir, resultdir)
  fs::dir_create(out_dir, recurse = TRUE)

  HYPEtools::WriteInfo(x = info, filename = info_path)
  invisible(out_dir)
}
