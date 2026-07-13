#' Robust Downloader for CDS/ECMWF Data with Chunking and Optional NetCDF Merge
#'
#' Downloads climate/meteorological data from CDS/ADS/CEMS using \pkg{ecmwfr},
#' with support for multiple variables and models, year chunking, optional
#' batch-parallel submission, request pacing, resume-on-existing files, dry-run
#' inspection, status logging, and optional NetCDF consolidation.
#'
#' CDS requests over long periods can be rejected or remain queued for a long
#' time. By default, the function splits multi-year requests into one-year
#' chunks. Increase \code{chunk_years} cautiously (for example 2 or 3) after
#' testing the dataset, variables, area, and lead times.
#'
#' File naming follows by default:
#' \preformatted{model_system_VARIABLE_InitDate_Chunk_Leadtime.nc}
#' for downloaded chunks. When \code{combine = TRUE}, successfully downloaded
#' chunks are merged into one file per model/system/variable using
#' \code{combine_filename_tpl}.
#'
#' @param dataset_short_name Character. The dataset short name on CDS/ADS/CEMS,
#'   e.g., \code{"seasonal-original-single-levels"}.
#' @param base_query Named list. Static part of the query (e.g., \code{product_type},
#'   \code{pressure_level}, \code{data_format}). Time-varying fields like \code{year},
#'   \code{month}, \code{day}, \code{leadtime_hour} are set by this function.
#' @param center_variables Character vector of entries \code{"model_system.variable"},
#'   e.g., \code{"meteo_france_9.TMAX"}, \code{"ecmwf_51.TMAX"}.
#'   Only one variable per entry is supported.
#' @param years Integer/character vector. Years to request.
#' @param months Integer/character vector in \code{1..12}.
#' @param days Character/integer vector (e.g., \code{c("01","02")}).
#' @param times Character vector (e.g., \code{c("00:00","12:00")}).
#' @param leadtime_hour Character/integer vector (e.g., \code{c("24","48",...)}).
#' @param area Optional numeric length-4 \code{c(N, W, S, E)} bounding box.
#' @param out_dir Output directory. Created if missing.
#' @param user CDS/ADS/CEMS user ID used with \code{ecmwfr::wf_set_key()}.
#' @param service One of \code{"cds"}, \code{"ads"}, \code{"cems"}. Default \code{"cds"}.
#' @param filename_tpl Template for chunk filenames. Placeholders available:
#'   \code{{modelsys}}, \code{{model}}, \code{{system}}, \code{{var}},
#'   \code{{init}}, \code{{period}}, \code{{lead}}, \code{{dataset}},
#'   \code{{year}}, \code{{year_start}}, \code{{year_end}}, \code{{chunk}}.
#' @param tries Integer. Retry attempts per request in sequential mode, and per batch group for missing files in batch mode.
#' @param sleep_sec Numeric. Seconds between retry attempts in sequential mode.
#' @param timeout_sec Numeric. Timeout per request in seconds.
#' @param force_download Logical. If \code{FALSE}, existing chunk files are skipped.
#' @param parallel Logical. If \code{TRUE}, use \code{wf_request_batch()} when available.
#' @param workers Integer. Number of workers for \code{wf_request_batch()}.
#' @param job_name Optional string passed to \code{wf_request()} in sequential mode.
#' @param verbose Logical. Verbose logging.
#' @param chunk_years Integer. Number of years per CDS request. Default \code{1}.
#' @param max_requests_per_batch Integer or \code{NULL}. Maximum number of requests
#'   submitted in one \code{wf_request_batch()} call. \code{NULL} uses \code{workers}.
#' @param cooldown_sec Numeric. Seconds to wait between batch groups.
#' @param request_delay_sec Numeric. Seconds to wait between sequential requests.
#' @param dry_run Logical. If \code{TRUE}, build and return the request plan without
#'   submitting anything to CDS.
#' @param stop_on_error Logical. If \code{FALSE} (default), failed downloads are
#'   recorded with \code{status = "fail"} and the remaining requests continue.
#'   If \code{TRUE}, the function stops at the first failed request or batch group.
#' @param return_requests Logical. If \code{TRUE}, include a list-column containing
#'   the generated CDS requests in the returned data frame.
#' @param job_log Optional CSV path used to record planned/submitted/finished jobs.
#'   Set to \code{NULL} to disable logging.
#' @param combine Logical. If \code{TRUE}, combine successful chunk NetCDF files into
#'   one file per model/system/variable after download.
#' @param combine_dir Directory for combined files. Defaults to \code{out_dir}.
#' @param combine_filename_tpl Template for combined filenames. Defaults to
#'   \code{"{modelsys}_{var}_{init}_{period}_{lead}.nc"}. Use
#'   \code{"{modelsys}_{var}_{period}.nc"} for names like
#'   \code{ecmwf_51_PRCP_1993_2026.nc}.
#' @param combine_dim Character. NetCDF dimension used for concatenation. Default
#'   \code{"auto"}, which tries \code{forecast_reference_time}, \code{time},
#'   \code{valid_time}, then the unlimited dimension.
#' @param keep_chunks Logical. If \code{FALSE}, delete chunk files after a successful
#'   combine.
#' @param ... Additional arguments forwarded to \code{ecmwfr::wf_request()} in
#'   sequential mode.
#'
#' @return Invisibly, a \code{data.frame} with one row per chunk request and columns
#'   describing the chunk file, status, metadata, and optional combined output.
#'
#' @section Operational notes:
#' \itemize{
#'   \item Use small \code{chunk_years} values for large domains, many lead times, or
#'   multi-variable downloads.
#'   \item Use \code{max_requests_per_batch}, \code{cooldown_sec}, and
#'   \code{request_delay_sec} to avoid submitting too many jobs to CDS at once.
#'   \item With \code{parallel = TRUE}, CDS can report that requests have been
#'   submitted and are still being processed server-side. This is normal;
#'   \pkg{ecmwfr} downloads each file once the corresponding job is ready.
#'   \item \code{combine = TRUE} requires \pkg{ncdf4} and currently combines along
#'   a single detected NetCDF dimension, usually \code{forecast_reference_time}.
#' }
#'
#' @examples
#' \dontrun{
#' center_variables <- c("meteo_france_9.TMAX", "ecmwf_51.TMAX")
#' res <- wass2s_download_cds(
#'   dataset_short_name = "seasonal-original-single-levels",
#'   base_query = list(data_format = "netcdf"),
#'   center_variables = center_variables,
#'   years = 1993:1995,
#'   months = 4, days = "01",
#'   times = "00:00",
#'   leadtime_hour = seq(24, 240, 24),
#'   out_dir = "out",
#'   chunk_years = 1,
#'   parallel = TRUE, workers = 4,
#'   max_requests_per_batch = 4,
#'   cooldown_sec = 30,
#'   combine = TRUE,
#'   combine_filename_tpl = "{modelsys}_{var}_{period}.nc",
#'   service = "cds", verbose = TRUE
#' )
#' }
#'
#' @seealso \code{\link[ecmwfr]{wf_request}}, \code{\link[ecmwfr]{wf_request_batch}}
#' @export
wass2s_download_cds <- function(
    dataset_short_name,
    base_query,
    center_variables,
    years,
    months,
    days,
    times = "00:00",
    leadtime_hour,
    area = c(28.5, -25.5, 4.0, 26.6),
    out_dir = ".",
    user = "ecmwfr",
    service = c("cds", "ads", "cems"),
    filename_tpl = NULL,
    tries = 3,
    sleep_sec = 15,
    timeout_sec = 3600,
    force_download = FALSE,
    parallel = FALSE,
    workers = 2,
    job_name = NULL,
    verbose = TRUE,
    chunk_years = 1L,
    max_requests_per_batch = NULL,
    cooldown_sec = 0,
    request_delay_sec = 0,
    dry_run = FALSE,
    stop_on_error = FALSE,
    return_requests = FALSE,
    job_log = file.path(out_dir, "_wass2s_cds_jobs.csv"),
    combine = FALSE,
    combine_dir = out_dir,
    combine_filename_tpl = NULL,
    combine_dim = "auto",
    keep_chunks = TRUE,
    ...
) {
  .need("ecmwfr")
  wass2s__validate_download_args(
    dataset_short_name = dataset_short_name,
    base_query = base_query,
    center_variables = center_variables,
    years = years,
    months = months,
    days = days,
    times = times,
    leadtime_hour = leadtime_hour,
    area = area,
    workers = workers,
    tries = tries,
    sleep_sec = sleep_sec,
    timeout_sec = timeout_sec,
    chunk_years = chunk_years,
    max_requests_per_batch = max_requests_per_batch,
    cooldown_sec = cooldown_sec,
    request_delay_sec = request_delay_sec
  )

  service <- tolower(match.arg(service))

  if (!requireNamespace("ecmwfr", quietly = TRUE)) {
    stop("Package 'ecmwfr' is required. Please install it and configure your key with wf_set_key().", call. = FALSE)
  }

  if (isTRUE(verbose)) message("Checking key for service: ", service, " ...")
  key_ok <- wass2s__has_ecmwfr_key(user = user, service = service)
  if (!key_ok) {
    warning("No key found for service '", service, "' and user '", user,
            "'. Configure it with ecmwfr::wf_set_key(user='", user,
            "', service='", service, "').", call. = FALSE)
  }

  months <- pad2(months)
  days <- pad2(days)
  years_int <- as.integer(years)
  years_chr <- as.character(years_int)
  period <- paste0(min(years_int), "_", max(years_int))
  lh <- as.integer(leadtime_hour)
  lead_part <- paste0(min(lh), "-", max(lh))
  init_part <- paste0(month_abb[as.integer(months[1])], days[1])
  chunk_years <- as.integer(chunk_years[1])
  workers <- max(1L, as.integer(workers[1]))
  if (is.null(max_requests_per_batch)) max_requests_per_batch <- workers
  max_requests_per_batch <- max(1L, as.integer(max_requests_per_batch[1]))

  if (!dir.exists(out_dir)) {
    if (verbose) message("Creating output directory: ", out_dir)
    dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  }
  if (isTRUE(combine) && !dir.exists(combine_dir)) {
    if (verbose) message("Creating combine directory: ", combine_dir)
    dir.create(combine_dir, recursive = TRUE, showWarnings = FALSE)
  }

  if (is.null(filename_tpl)) {
    filename_tpl <- "{modelsys}_{var}_{init}_{chunk}_{lead}.nc"
  }
  if (is.null(combine_filename_tpl)) {
    combine_filename_tpl <- "{modelsys}_{var}_{init}_{period}_{lead}.nc"
  }

  parsed <- lapply(center_variables, parse_center_variable)
  models_all <- vapply(parsed, `[[`, "", "model")
  unk <- setdiff(unique(models_all), VALID_MODELS)
  if (length(unk)) warning("Unknown models (continuing anyway): ", paste(unk, collapse = ", "), call. = FALSE)

  year_chunks <- wass2s__split_years(years_int, chunk_years)

  make_chunk <- function(p, yy) {
    model <- p$model
    system <- p$system
    var_in <- p$variable
    var_api <- var_api_name(var_in)
    var_for_name <- var_short(var_api)
    year_start <- min(yy)
    year_end <- max(yy)
    chunk <- if (length(yy) == 1L) as.character(yy) else paste0(year_start, "-", year_end)
    modelsys <- paste(model, system, sep = "_")

    values <- list(
      modelsys = modelsys,
      model = model,
      system = system,
      var = var_for_name,
      init = init_part,
      period = period,
      lead = lead_part,
      dataset = dataset_short_name,
      year = chunk,
      year_start = year_start,
      year_end = year_end,
      chunk = chunk
    )

    base_name <- tpl_replace(filename_tpl, values)
    if (!grepl("\\.[A-Za-z0-9]+$", base_name)) base_name <- paste0(base_name, ".nc")
    target <- file.path(out_dir, base_name)

    skipped <- !force_download && file.exists(target)
    req <- NULL
    if (!skipped) {
      req <- c(list(dataset_short_name = dataset_short_name), base_query)
      if (is.null(req$format) && is.null(req$data_format)) req$data_format <- "netcdf"
      if (!is.null(req$format) && is.null(req$data_format)) {
        warning("base_query uses 'format'. For the current CDS API, prefer 'data_format'.", call. = FALSE)
      }
      req$target <- basename(target)
      req$originating_centre <- model
      req$system <- system
      req$variable <- var_api
      req$year <- as.character(yy)
      req$month <- months
      req$day <- days
      req$time <- times
      req$leadtime_hour <- as.character(leadtime_hour)
      if (!is.null(area)) req$area <- area
    }

    list(
      skip = skipped,
      target = target,
      model = model,
      system = system,
      variable = var_in,
      variable_api = var_api,
      variable_short = var_for_name,
      year = chunk,
      year_start = year_start,
      year_end = year_end,
      chunk = chunk,
      modelsys = modelsys,
      req = req,
      combine_values = values
    )
  }

  jobs <- list()
  for (p in parsed) {
    for (yy in year_chunks) {
      jobs[[length(jobs) + 1L]] <- make_chunk(p, yy)
    }
  }

  plan_df <- wass2s__jobs_to_df(jobs, status = if (isTRUE(dry_run)) "planned" else "pending")
  if (isTRUE(return_requests)) plan_df$request <- I(lapply(jobs, `[[`, "req"))
  wass2s__append_job_log(plan_df, job_log, stage = if (isTRUE(dry_run)) "dry_run" else "planned")
  if (isTRUE(dry_run)) return(invisible(plan_df))

  out <- list()
  for (j in jobs) {
    if (isTRUE(j$skip)) {
      out[[length(out) + 1L]] <- wass2s__job_row(j, "skip", NA_character_)
    }
  }

  submit <- Filter(function(z) !isTRUE(z$skip), jobs)
  if (length(submit) == 0L) {
    if (verbose) message("Nothing to download (all files already present).")
    res <- do.call(rbind, out)
    res <- wass2s__combine_downloads_if_needed(res, jobs, combine, combine_dir,
                                               combine_filename_tpl, combine_dim,
                                               keep_chunks, verbose)
    return(invisible(res))
  }

  batch_ok <- isTRUE(parallel) && is.function(ecmwfr::wf_request_batch)

  if (batch_ok) {
    if (verbose) {
      message("Submitting batch via wf_request_batch() with ", workers,
              " workers; max ", max_requests_per_batch, " request(s) per batch group...")
    }
    groups <- split(seq_along(submit), ceiling(seq_along(submit) / max_requests_per_batch))
    for (g in seq_along(groups)) {
      idx <- groups[[g]]
      group_jobs <- submit[idx]
      request_list <- lapply(group_jobs, `[[`, "req")
      target_list <- vapply(group_jobs, `[[`, "", "target")
      err <- NULL
      remaining_jobs <- group_jobs
      remaining_targets <- target_list
      batch_tries <- max(1L, as.integer(tries))
      attempt <- 1L
      while (length(remaining_jobs) > 0L && attempt <= batch_tries) {
        if (verbose && batch_tries > 1L) {
          message("Batch group ", g, " attempt ", attempt, "/", batch_tries,
                  " for ", length(remaining_jobs), " pending request(s)...")
        }
        ok_batch <- tryCatch({
          ecmwfr::wf_request_batch(
            request_list = lapply(remaining_jobs, `[[`, "req"),
            workers = workers,
            user = user,
            path = out_dir,
            time_out = timeout_sec,
            retry = 30,
            total_timeout = length(remaining_jobs) * timeout_sec / max(1, workers)
          )
          TRUE
        }, error = function(e) {
          err <<- e$message
          FALSE
        })

        missing <- !file.exists(remaining_targets)
        if (!any(missing)) break

        if (!ok_batch && attempt >= batch_tries && isTRUE(stop_on_error)) {
          stop("CDS batch group failed after ", batch_tries, " attempt(s): ",
               err %||% "unknown error", call. = FALSE)
        }
        if (ok_batch && attempt >= batch_tries && isTRUE(stop_on_error)) {
          stop("CDS batch group finished but some files are missing after ",
               batch_tries, " attempt(s).", call. = FALSE)
        }
        if (attempt < batch_tries) {
          if (verbose) message("Batch group ", g, " has ", sum(missing),
                               " missing file(s); retrying after ", sleep_sec, " second(s)...")
          Sys.sleep(sleep_sec)
          remaining_jobs <- remaining_jobs[missing]
          remaining_targets <- remaining_targets[missing]
        }
        attempt <- attempt + 1L
      }

      group_out <- vector("list", length(group_jobs))
      for (i in seq_along(group_jobs)) {
        ok_file <- file.exists(target_list[i])
        status <- if (ok_file) "ok" else "fail"
        error <- if (status == "ok") NA_character_ else (err %||% "File missing after batch")
        group_out[[i]] <- wass2s__job_row(group_jobs[[i]], status, error)
        out[[length(out) + 1L]] <- group_out[[i]]
      }
      wass2s__append_job_log(do.call(rbind, group_out), job_log, stage = paste0("batch_", g))
      if (g < length(groups) && cooldown_sec > 0) {
        if (verbose) message("Cooling down for ", cooldown_sec, " second(s) before next batch group...")
        Sys.sleep(cooldown_sec)
      }
    }
  } else {
    if (isTRUE(parallel) && !is.function(ecmwfr::wf_request_batch) && verbose) {
      message("wf_request_batch() not available; falling back to sequential mode.")
    }

    for (k in seq_along(submit)) {
      j <- submit[[k]]
      attempt <- 1L
      last_err <- NULL
      success <- FALSE
      while (attempt <= max(1L, as.integer(tries)) && !success) {
        if (verbose) {
          message(sprintf("[REQ] %s_%s.%s (%s) | attempt %d/%d -> %s",
                          j$model, j$system, j$variable, j$chunk,
                          attempt, max(1L, as.integer(tries)), basename(j$target)))
        }
        ok <- tryCatch({
          wf_args <- c(list(
            user = user, request = j$req, transfer = TRUE, path = out_dir,
            verbose = verbose, time_out = timeout_sec
          ), list(...))
          if (!is.null(job_name)) wf_args$job_name <- job_name
          do.call(ecmwfr::wf_request, wf_args)
          TRUE
        }, error = function(e) {
          if (verbose) message(sprintf("Error: %s", e$message))
          last_err <<- e$message
          FALSE
        })

        if (ok && file.exists(j$target)) {
          success <- TRUE
          out[[length(out) + 1L]] <- wass2s__job_row(j, "ok", NA_character_)
        } else {
          if (attempt >= tries) {
            final_err <- last_err %||% "Unknown error"
            if (isTRUE(stop_on_error)) {
              stop("CDS request failed after ", tries, " attempt(s): ", final_err, call. = FALSE)
            }
            out[[length(out) + 1L]] <- wass2s__job_row(j, "fail", final_err)
          } else {
            if (verbose) message(sprintf("...retry in %ds", sleep_sec))
            Sys.sleep(sleep_sec)
          }
          attempt <- attempt + 1L
        }
      }
      if (k < length(submit) && request_delay_sec > 0) {
        if (verbose) message("Waiting ", request_delay_sec, " second(s) before next request...")
        Sys.sleep(request_delay_sec)
      }
    }
  }

  res <- do.call(rbind, out)
  res <- res[order(res$model, res$system, res$variable, res$year_start, res$year_end), , drop = FALSE]
  rownames(res) <- NULL
  res <- wass2s__combine_downloads_if_needed(res, jobs, combine, combine_dir,
                                             combine_filename_tpl, combine_dim,
                                             keep_chunks, verbose)
  wass2s__append_job_log(res, job_log, stage = "finished")
  invisible(res)
}

wass2s__validate_download_args <- function(dataset_short_name, base_query, center_variables,
                                           years, months, days, times, leadtime_hour, area,
                                           workers, tries, sleep_sec, timeout_sec, chunk_years,
                                           max_requests_per_batch, cooldown_sec,
                                           request_delay_sec) {
  if (!is.character(dataset_short_name) || length(dataset_short_name) != 1L || is.na(dataset_short_name) || !nzchar(dataset_short_name)) {
    stop("`dataset_short_name` must be a non-empty character scalar.", call. = FALSE)
  }
  if (!is.list(base_query) || (length(base_query) > 0L && is.null(names(base_query)))) {
    stop("`base_query` must be a named list.", call. = FALSE)
  }
  if (!is.character(center_variables) || length(center_variables) < 1L || anyNA(center_variables) || any(!nzchar(center_variables))) {
    stop("`center_variables` must be a non-empty character vector.", call. = FALSE)
  }
  years_int <- suppressWarnings(as.integer(years))
  if (!length(years_int) || anyNA(years_int)) stop("`years` must contain valid integer years.", call. = FALSE)
  months_int <- suppressWarnings(as.integer(months))
  if (!length(months_int) || anyNA(months_int) || any(months_int < 1 | months_int > 12)) {
    stop("`months` must contain values between 1 and 12.", call. = FALSE)
  }
  days_int <- suppressWarnings(as.integer(days))
  if (!length(days_int) || anyNA(days_int) || any(days_int < 1 | days_int > 31)) {
    stop("`days` must contain values between 1 and 31.", call. = FALSE)
  }
  if (!is.character(times) || !length(times) || anyNA(times) || any(!grepl("^[0-2][0-9]:[0-5][0-9]$", times))) {
    stop("`times` must be character values formatted as 'HH:MM'.", call. = FALSE)
  }
  lead_int <- suppressWarnings(as.integer(leadtime_hour))
  if (!length(lead_int) || anyNA(lead_int) || any(lead_int <= 0)) {
    stop("`leadtime_hour` must contain positive integer lead times.", call. = FALSE)
  }
  if (!is.null(area) && (!is.numeric(area) || length(area) != 4L || anyNA(area))) {
    stop("`area` must be NULL or numeric length 4: c(N, W, S, E).", call. = FALSE)
  }
  for (nm in c("workers", "tries", "chunk_years")) {
    val <- get(nm)
    if (length(val) != 1L || is.na(suppressWarnings(as.numeric(val))) || as.numeric(val) < 1) {
      stop("`", nm, "` must be a positive scalar.", call. = FALSE)
    }
  }
  for (nm in c("sleep_sec", "timeout_sec", "cooldown_sec", "request_delay_sec")) {
    val <- get(nm)
    if (length(val) != 1L || is.na(suppressWarnings(as.numeric(val))) || as.numeric(val) < 0) {
      stop("`", nm, "` must be a non-negative scalar.", call. = FALSE)
    }
  }
  if (!is.null(max_requests_per_batch) && (length(max_requests_per_batch) != 1L || is.na(suppressWarnings(as.numeric(max_requests_per_batch))) || as.numeric(max_requests_per_batch) < 1)) {
    stop("`max_requests_per_batch` must be NULL or a positive scalar.", call. = FALSE)
  }
  invisible(TRUE)
}

wass2s__has_ecmwfr_key <- function(user, service) {
  tryCatch({
    do.call(ecmwfr::wf_get_key, list(user = user, service = service))
    TRUE
  }, error = function(e) {
    tryCatch({
      ecmwfr::wf_get_key(user = user)
      TRUE
    }, error = function(e2) FALSE)
  })
}

wass2s__split_years <- function(years, chunk_years) {
  years <- sort(unique(as.integer(years)))
  split(years, ceiling(seq_along(years) / as.integer(chunk_years[1])))
}

wass2s__job_row <- function(j, status, error = NA_character_) {
  data.frame(
    file = j$target,
    status = status,
    error = error,
    model = j$model,
    system = j$system,
    variable = j$variable,
    variable_api = j$variable_api,
    year = j$year,
    year_start = j$year_start,
    year_end = j$year_end,
    chunk = j$chunk,
    stringsAsFactors = FALSE
  )
}

wass2s__jobs_to_df <- function(jobs, status = "planned") {
  do.call(rbind, lapply(jobs, wass2s__job_row, status = status, error = NA_character_))
}

wass2s__append_job_log <- function(df, job_log, stage) {
  if (is.null(job_log) || !length(job_log) || !nzchar(job_log)) return(invisible(FALSE))
  log_cols <- c(
    "logged_at", "stage", "status", "file", "error", "model", "system",
    "variable", "variable_api", "year", "year_start", "year_end", "chunk",
    "combined_file", "combine_status", "combine_error"
  )
  log_df <- df
  log_df$stage <- stage
  log_df$logged_at <- format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")
  missing_cols <- setdiff(log_cols, names(log_df))
  for (nm in missing_cols) log_df[[nm]] <- NA_character_
  log_df <- log_df[, log_cols, drop = FALSE]
  dir.create(dirname(job_log), recursive = TRUE, showWarnings = FALSE)
  utils::write.table(log_df, file = job_log, sep = ",", row.names = FALSE,
                     col.names = !file.exists(job_log), append = file.exists(job_log), qmethod = "double")
  invisible(TRUE)
}

wass2s__combine_downloads_if_needed <- function(res, jobs, combine, combine_dir,
                                                combine_filename_tpl, combine_dim,
                                                keep_chunks, verbose) {
  res$combined_file <- NA_character_
  res$combine_status <- NA_character_
  res$combine_error <- NA_character_
  if (!isTRUE(combine) || !nrow(res)) return(res)
  if (!requireNamespace("ncdf4", quietly = TRUE)) {
    res$combine_status <- "fail"
    res$combine_error <- "Package 'ncdf4' is required for combine = TRUE."
    return(res)
  }

  ok <- res$status %in% c("ok", "skip") & file.exists(res$file)
  if (!any(ok)) return(res)
  keys <- unique(paste(res$model[ok], res$system[ok], res$variable[ok], sep = "\r"))
  for (key in keys) {
    idx <- which(paste(res$model, res$system, res$variable, sep = "\r") == key)
    idx_ok <- idx[res$status[idx] %in% c("ok", "skip") & file.exists(res$file[idx])]
    if (!length(idx_ok)) next
    jj <- jobs[[which(vapply(jobs, function(z) z$model == res$model[idx_ok[1]] && z$system == res$system[idx_ok[1]] && z$variable == res$variable[idx_ok[1]], logical(1)))[1]]]
    combined_name <- tpl_replace(combine_filename_tpl, jj$combine_values)
    if (!grepl("\\.[A-Za-z0-9]+$", combined_name)) combined_name <- paste0(combined_name, ".nc")
    combined_file <- file.path(combine_dir, combined_name)
    res$combined_file[idx] <- combined_file
    files <- res$file[idx_ok][order(res$year_start[idx_ok], res$year_end[idx_ok])]
    combine_err <- NULL
    combine_ok <- tryCatch({
      if (length(files) == 1L) {
        if (!identical(normalizePath(files[1], winslash = "/", mustWork = FALSE), normalizePath(combined_file, winslash = "/", mustWork = FALSE))) {
          file.copy(files[1], combined_file, overwrite = TRUE)
        }
      } else {
        wass2s__combine_netcdf(files, combined_file, concat_dim = combine_dim, overwrite = TRUE)
      }
      TRUE
    }, error = function(e) {
      combine_err <<- e$message
      FALSE
    })
    res$combine_status[idx] <- if (combine_ok) "ok" else "fail"
    res$combine_error[idx] <- if (combine_ok) NA_character_ else combine_err
    if (combine_ok && !isTRUE(keep_chunks)) {
      unlink(setdiff(files, combined_file), force = TRUE)
    }
    if (verbose) {
      if (combine_ok) message("Combined ", length(files), " chunk file(s) -> ", combined_file)
      else message("Combine failed for ", key, ": ", combine_err)
    }
  }
  res
}

wass2s__combine_netcdf <- function(files, output_file, concat_dim = "auto", overwrite = TRUE) {
  if (!requireNamespace("ncdf4", quietly = TRUE)) stop("Package 'ncdf4' is required.", call. = FALSE)
  files <- normalizePath(files, winslash = "/", mustWork = TRUE)
  if (file.exists(output_file)) {
    if (!overwrite) stop("Output file already exists: ", output_file, call. = FALSE)
    unlink(output_file)
  }

  ncs <- lapply(files, ncdf4::nc_open)
  on.exit(lapply(ncs, ncdf4::nc_close), add = TRUE)
  first <- ncs[[1]]
  dim_names <- names(first$dim)
  cd <- wass2s__detect_concat_dim(ncs, concat_dim)
  if (is.na(cd) || !nzchar(cd)) stop("Could not detect NetCDF concatenation dimension.", call. = FALSE)

  dim_defs <- vector("list", length(dim_names))
  names(dim_defs) <- dim_names
  for (dn in dim_names) {
    d0 <- first$dim[[dn]]
    vals <- d0$vals
    if (dn == cd) vals <- unlist(lapply(ncs, function(nc) nc$dim[[dn]]$vals), use.names = FALSE)
    if (is.null(vals) || !length(vals)) vals <- seq_len(if (dn == cd) sum(vapply(ncs, function(nc) nc$dim[[dn]]$len, integer(1))) else d0$len)
    dim_defs[[dn]] <- ncdf4::ncdim_def(name = dn, units = d0$units %||% "", vals = vals, unlim = isTRUE(d0$unlim), create_dimvar = TRUE)
  }

  var_defs <- list()
  for (vn in names(first$var)) {
    v0 <- first$var[[vn]]
    vdims <- lapply(v0$dim, function(d) dim_defs[[d$name]])
    var_args <- list(
      name = vn,
      units = v0$units %||% "",
      dim = vdims,
      missval = v0$missval,
      longname = v0$longname %||% vn,
      prec = wass2s__ncdf4_prec(v0$prec)
    )
    if (length(vdims) > 0L) var_args$compression <- 4
    var_defs[[vn]] <- do.call(ncdf4::ncvar_def, var_args)
  }

  dir.create(dirname(output_file), recursive = TRUE, showWarnings = FALSE)
  out_nc <- ncdf4::nc_create(output_file, vars = var_defs, force_v4 = TRUE)
  on.exit(ncdf4::nc_close(out_nc), add = TRUE)

  for (att in names(first$gatts)) {
    val <- first$gatts[[att]]
    if (!is.null(val)) try(ncdf4::ncatt_put(out_nc, 0, att, val), silent = TRUE)
  }
  ncdf4::ncatt_put(out_nc, 0, "wass2s_combined_from", paste(basename(files), collapse = ";"))
  ncdf4::ncatt_put(out_nc, 0, "wass2s_combined_on", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"))

  concat_pos <- 1L
  for (fi in seq_along(ncs)) {
    nc <- ncs[[fi]]
    clen <- nc$dim[[cd]]$len
    for (vn in names(first$var)) {
      v0 <- first$var[[vn]]
      vdn <- vapply(v0$dim, `[[`, "", "name")
      vals <- ncdf4::ncvar_get(nc, vn, collapse_degen = FALSE)
      if (cd %in% vdn) {
        dim_index <- match(cd, vdn)
        start <- rep(1L, length(vdn))
        start[dim_index] <- concat_pos
        count <- dim(vals)
        if (is.null(count)) count <- length(vals)
        ncdf4::ncvar_put(out_nc, vn, vals, start = start, count = count)
      } else if (fi == 1L) {
        ncdf4::ncvar_put(out_nc, vn, vals)
      }
    }
    concat_pos <- concat_pos + clen
  }

  invisible(output_file)
}

wass2s__detect_concat_dim <- function(ncs, concat_dim = "auto") {
  first <- ncs[[1]]
  if (!identical(concat_dim, "auto")) {
    if (!concat_dim %in% names(first$dim)) stop("`combine_dim` not found in NetCDF dimensions: ", concat_dim, call. = FALSE)
    return(concat_dim)
  }
  preferred <- c("forecast_reference_time", "time", "valid_time", "ref_time")
  hit <- preferred[preferred %in% names(first$dim)]
  if (length(hit)) return(hit[1])
  unlim <- names(first$dim)[vapply(first$dim, function(d) isTRUE(d$unlim), logical(1))]
  if (length(unlim)) return(unlim[1])
  lens <- vapply(first$dim, function(d) d$len, integer(1))
  candidates <- names(lens)[lens >= 1]
  for (dn in candidates) {
    vals <- lapply(ncs, function(nc) nc$dim[[dn]]$vals)
    if (length(unique(vapply(vals, function(x) paste(x, collapse = ","), character(1)))) > 1L) return(dn)
  }
  NA_character_
}
wass2s__ncdf4_prec <- function(prec) {
  prec <- tolower(as.character(prec %||% "float"))
  if (prec %in% c("short", "float", "double", "integer", "char", "byte")) return(prec)
  if (grepl("64|8 byte|int64|long", prec)) return("double")
  if (grepl("int", prec)) return("integer")
  if (grepl("double|real8", prec)) return("double")
  if (grepl("float|real4", prec)) return("float")
  "float"
}