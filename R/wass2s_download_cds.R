#' Robust downloader for Copernicus/ECMWF datasets (via ecmwfr)
#'
#' Downloads climate/meteorological data from CDS/ADS/CEMS using `ecmwfr`,
#' chunked by year (or year-month), with retries, resume on existing files,
#' and clear status reporting.
#'
#' @param dataset_short_name Character. CDS/ADS/CEMS dataset name (e.g. "reanalysis-era5-single-levels",
#'   "seasonal-original-single-levels", "cams-global-reanalysis-eac4", etc.).
#' @param base_query Named list. Static part of the query (e.g. variables, product_type, pressure_level).
#'   You can leave out time-varying fields (year, month, day, leadtime_hour) — they are handled below.
#'   Names must match the dataset API fields (e.g. `variable`, `product_type`, `format`).
#' @param years Integer or character vector. Years to download.
#' @param months Integer/character vector [1..12] or NULL. If NULL, will not include `month` in query.
#' @param days Character/integer vector or NULL. E.g. c("01","02") or NULL.
#' @param times Character vector or NULL. E.g. c("00:00","06:00","12:00","18:00") for ERA5 single levels.
#' @param leadtime_hour Character/integer vector or NULL. E.g. c("24","48",...) for seasonal original.
#' @param area Numeric length-4 (N,W,S,E) or NULL.
#' @param out_dir Character. Output directory.
#' @param user Character. Your CDS/ADS user ID (e.g., "12345"). Must match the key registered with `wf_set_key()`.
#' @param service Character. One of "cds","ads","cems". Default "cds".
#' @param chunk Character. "year" or "year-month". Controls file granularity.
#' @param filename_tpl Character glue-like template. Available fields: {dataset}, {year}, {month}, {var}.
#'   Default: "{dataset}_{year}.nc". If chunk = "year-month", default becomes "{dataset}_{year}{month}.nc".
#' @param tries Integer. Number of retry attempts per chunk (default 3).
#' @param sleep_sec Numeric. Seconds to wait between retries (default 15).
#' @param timeout_sec Numeric. Timeout per request in seconds (default 3600).
#' @param force_download  Logical. If FALSE (default), skip chunks whose file already exists.
#' @param job_name optional name to use as an RStudio job and as output variable name. It has to be a syntactically valid name.
#' @param verbose Logical. Pass to `wf_request()`; also controls message printing.
#' @param ... Others parameters pass to `ecmwfr::wf_request()`
#' @return A data.frame (invisible) with columns: chunk, file, status ("ok","skip","fail"), error.
#' @export
wass2s_download_cds <- function(
    dataset_short_name,
    base_query,
    years,
    months         = NULL,
    days           = NULL,
    times          = NULL,
    leadtime_hour  = NULL,
    area           = NULL,
    out_dir =  ".",
    user =  "ecmwf",
    service        = c("cds","ads","cems"),
    chunk          = c("year","year-month"),
    job_name       = job_name,
    filename_tpl   = NULL,
    tries          = 3,
    sleep_sec      = 15,
    timeout_sec    = 3600,
    force_download       = FALSE,
    verbose        = TRUE,
    ...
) {
  stopifnot(is.character(dataset_short_name), length(dataset_short_name) == 1)
  stopifnot(is.list(base_query))
  stopifnot(length(years) >= 1)
  service <- match.arg(service)
  chunk   <- match.arg(chunk)

  # basic sanitization / formatting helpers
  .pad2 <- function(x) sprintf("%02d", as.integer(x))
  if (!is.null(months)) months <- .pad2(months)
  if (!is.null(days))   days   <- .pad2(days)

  # default file name template
  if (is.null(filename_tpl)) {
    filename_tpl <- if (chunk == "year") {
      "{dataset}_{year}.nc"
    } else {
      "{dataset}_{year}{month}.nc"
    }
  }

  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  # expand chunks
  chunk_df <- if (chunk == "year") {
    data.frame(year = as.character(years), month = NA_character_, stringsAsFactors = FALSE)
  } else {
    if (is.null(months)) stop("chunk='year-month' requires 'months' not NULL")
    expand.grid(
      year  = as.character(years),
      month = as.character(months),
      stringsAsFactors = FALSE
    )
  }

  results <- vector("list", nrow(chunk_df))
  on.exit(invisible(do.call(rbind, results)), add = TRUE)

  # main loop
  for (i in seq_len(nrow(chunk_df))) {
    yy <- chunk_df$year[i]
    mm <- chunk_df$month[i] # may be NA

    # build output filename
    fname <- gsub("\\{dataset\\}", dataset_short_name, filename_tpl)
    fname <- gsub("\\{year\\}",    yy,                 fname)
    fname <- gsub("\\{month\\}",   ifelse(is.na(mm), "", mm), fname)
    # optional variable tag if present in base_query
    var_tag <- if (!is.null(base_query$variable)) {
      # often vector -> first one for naming; adapt as you like
      if (length(base_query$variable) > 1) base_query$variable[1] else base_query$variable
    } else ""
    fname <- gsub("\\{var\\}", var_tag, fname)

    target <- file.path(out_dir, fname)

    if (!force_download  && file.exists(target)) {
      if (verbose) message(sprintf("[SKIP] %s exists", target))
      results[[i]] <- data.frame(
        chunk  = if (is.na(mm)) yy else paste0(yy, "-", mm),
        file   = target,
        status = "skip",
        error  = NA_character_,
        stringsAsFactors = FALSE
      )
      next
    }

    # compose query for this chunk
    req <- c(
      list(dataset_short_name = dataset_short_name),
      base_query
    )

    # set format + target coherently (CDS often uses 'format'; seasonal accepts 'format' too)
    if (is.null(req$format) && is.null(req$data_format)) {
      req$format <- "netcdf"
    }
    req$target <- basename(target)

    # temporal fields
    req$year  <- yy
    if (!is.null(months)) req$month <- if (is.na(mm)) months else mm
    if (!is.null(days))   req$day   <- days
    if (!is.null(times))  req$time  <- times
    if (!is.null(leadtime_hour)) req$leadtime_hour <- as.character(leadtime_hour)

    # spatial field
    if (!is.null(area)) {
      stopifnot(length(area) == 4)
      req$area <- area # (N, W, S, E)
    }

    # retries
    attempt <- 1
    last_err <- NULL
    repeat {
      if (verbose) {
        ch <- if (is.na(mm)) yy else paste0(yy, "-", mm)
        message(sprintf("[REQ] %s | attempt %d/%d -> %s",
                        ch, attempt, tries, basename(target)))
      }
      ok <- try(
        ecmwfr::wf_request(
          user     = user,
          request  = req,
          transfer = TRUE,
          path     = out_dir,
          verbose  = verbose,
          time_out  = timeout_sec,
          job_name = job_name,...
        ),
        silent = TRUE
      )
      if (!inherits(ok, "try-error") && file.exists(target)) {
        if (verbose) message(sprintf("[OK] %s", target))
        results[[i]] <- data.frame(
          chunk  = if (is.na(mm)) yy else paste0(yy, "-", mm),
          file   = target,
          status = "ok",
          error  = NA_character_,
          stringsAsFactors = FALSE
        )
        break
      }
      last_err <- if (inherits(ok, "try-error")) as.character(ok) else "Unknown error"
      if (attempt >= tries) {
        warning(sprintf("[FAIL] %s after %d tries\nLast error: %s", target, attempt, last_err))
        results[[i]] <- data.frame(
          chunk  = if (is.na(mm)) yy else paste0(yy, "-", mm),
          file   = target,
          status = "fail",
          error  = last_err,
          stringsAsFactors = FALSE
        )
        break
      }
      attempt <- attempt + 1
      if (verbose) message(sprintf("...retry in %ds", sleep_sec))
      Sys.sleep(sleep_sec)
    }
  }

  invisible(do.call(rbind, results))
}


#' Preset: ERA5 single levels (reanalysis)
#' @param vars character vector of ERA5 variables (e.g. "total_precipitation")
#' @return named list for `wass2s_download_cds(base_query = ...)`
#' @export
wass2s_preset_era5_single <- function(vars = c("total_precipitation","2m_temperature")) {
  list(
    product_type = "reanalysis",
    variable     = vars,
    format       = "netcdf"
  )
}

#' Preset: Seasonal original single levels (ECMWF system)
#' @param vars character vector of variables (e.g. "total_precipitation")
#' @param system character system id (e.g. "51")
#' @param originating_centre character data provider (e.g. "ecmwf")
#' @return named list for `wass2s_download_cds(base_query = ...)`
#' @export
wass2s_preset_seasonal_original <- function(vars = c("total_precipitation"),
                                            system = "51",
                                            originating_centre = "ecmwf") {
  list(
    originating_centre = originating_centre,
    system             = system,
    variable           = vars,
    format             = "netcdf"
  )
}
