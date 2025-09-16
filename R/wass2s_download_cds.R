#' Robust Parallel Downloader for CDS/ECMWF (via ecmwfr) with Year Chunking
#'
#' Downloads climate/meteorological data from CDS/ADS/CEMS using \pkg{ecmwfr},
#' with support for multiple variables and models, robust retries, optional
#' batch-parallel submission, resume-on-existing files, and clear status reporting.
#'
#' To comply with CDS request-size limits, when multiple years are provided the
#' function automatically **splits requests per year** (internal year chunking).
#' This yields one output file per year. If your \code{filename_tpl} does not
#' contain \code{{year}}, a \code{_{year}} suffix is appended before the extension
#' to avoid overwriting.
#'
#' File naming follows by default:
#' \preformatted{ModelSystem_Variable_InitDate_Period_Leadtime.nc}
#' e.g., \code{meteo_france9_TMAX_Apr01_1993_2016_24-5160.nc}
#' When year-chunking is active and \code{{year}} is not present in the template,
#' files will be named like \code{..._1993.nc}, \code{..._1994.nc}, etc.
#'
#' @param dataset_short_name Character. The dataset short name on CDS/ADS/CEMS,
#'   e.g., \code{"seasonal-original-single-levels"}.
#' @param base_query Named list. Static part of the query (e.g., \code{product_type},
#'   \code{pressure_level}, \code{format}). Time-varying fields like \code{year},
#'   \code{month}, \code{day}, \code{leadtime_hour} are set by this function.
#' @param center_variables Character vector of entries \code{"model_system.variable"},
#'   e.g., \code{"meteo_france_9.TMAX"}, \code{"ecmwf_51.TMAX"}.
#'   Only one variable per entry is supported.
#' @param years Integer/character vector. **Required.** Years to request.
#'   When multiple years are provided, requests are split one-per-year.
#' @param months Integer/character vector in \code{1..12}. **Required.**
#' @param days Character/integer vector (e.g., \code{c("01","02")}). **Required.**
#' @param times Character vector (e.g., \code{c("00:00","12:00")}). **Required.**
#' @param leadtime_hour Character/integer vector (e.g., \code{c("24","48",...)}). **Required.**
#' @param area Optional numeric length-4 \code{c(N, W, S, E)} bounding box.
#' @param out_dir Output directory. Created if missing.
#' @param user CDS/ADS/CEMS user ID used with \code{ecmwfr::wf_set_key()}.
#' @param service One of \code{"cds"}, \code{"ads"}, \code{"cems"} (case-insensitive). Default \code{"cds"}.
#' @param filename_tpl Glue-like template for filenames. Placeholders available:
#'   \code{{modelsys}}, \code{{var}}, \code{{init}}, \code{{period}}, \code{{lead}}, \code{{dataset}}, \code{{year}}.
#'   Default: \code{"{modelsys}_{var}_{init}_{period}_{lead}.nc"}.
#' @param tries Integer. Number of retry attempts per request (sequential mode). Default \code{3}.
#' @param sleep_sec Numeric. Seconds between retries (sequential mode). Default \code{15}.
#' @param timeout_sec Numeric. Timeout per request (seconds). Default \code{3600}.
#'   Passed to \code{wf_request()} or \code{wf_request_batch()} as \code{time_out}.
#' @param force_download Logical. If \code{FALSE} (default), existing files are skipped.
#' @param parallel Logical. If \code{TRUE}, use \code{wf_request_batch()} to submit requests in parallel.
#'   Falls back to sequential mode if batch is unavailable.
#' @param workers Integer. Number of parallel workers for batch submission. Default \code{2}.
#' @param job_name Optional string passed to \code{wf_request()} (sequential mode).
#' @param verbose Logical. Verbose logging. Default \code{TRUE}.
#' @param ... Additional arguments forwarded to \code{ecmwfr::wf_request()}.
#'
#' @return (Invisibly) a \code{data.frame} with columns:
#' \itemize{
#'   \item \code{file} – full path to the output file
#'   \item \code{status} – one of \code{"ok"}, \code{"skip"}, \code{"fail"}
#'   \item \code{error} – last error message (if any)
#'   \item \code{model}, \code{system}, \code{variable}, \code{year}
#' }
#'
#' @section Notes:
#' \itemize{
#'   \item Unknown models (not listed in internal \code{VALID_MODELS}) trigger a warning but do not stop execution.
#'   \item \code{retry} in \code{wf_request_batch()} is a polling interval (seconds), not a count.
#'   \item Include \code{{year}} in \code{filename_tpl} if you prefer to control where the year appears.
#' }
#'
#' @examples
#' \dontrun{
#' center_variables <- c("meteo_france_9.TMAX","ecmwf_51.TMAX")
#' res <- wass2s_download_cds(
#'   dataset_short_name = "seasonal-original-single-levels",
#'   base_query = list(format = "netcdf"),
#'   center_variables = center_variables,
#'   years = 1993:1995,
#'   months = 4, days = "01",
#'   times = "00:00",
#'   leadtime_hour = seq(24,240,24),
#'   out_dir = "out",
#'   parallel = TRUE, workers = 4,
#'   service = "cds", verbose = TRUE
#' )
#' }
#'
#' @seealso \code{\link[ecmwfr]{wf_request}}, \code{\link[ecmwfr]{wf_request_batch}}
#' @importFrom ecmwfr wf_get_key wf_request wf_request_batch
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
    user = "ecmwf",
    service = c("cds", "ads", "cems"),
    filename_tpl = NULL,
    tries = 3,
    sleep_sec = 15,
    timeout_sec = 3600,
    force_download = FALSE,
    parallel = FALSE,
    workers = 2,
    job_name = "default",
    verbose = TRUE,
    ...
) {
  # ---- validations ----
  stopifnot(is.character(dataset_short_name), length(dataset_short_name) == 1)
  stopifnot(is.list(base_query))
  stopifnot(is.character(center_variables), length(center_variables) >= 1)
  stopifnot(length(years) >= 1)
  stopifnot(!missing(months), !is.null(months))
  stopifnot(!missing(days), !is.null(days))
  stopifnot(!missing(times), !is.null(times))
  stopifnot(!missing(leadtime_hour), !is.null(leadtime_hour))

  service <- tolower(match.arg(service))

  if (!requireNamespace("ecmwfr", quietly = TRUE)) {
    stop("Package 'ecmwfr' is required. Please install it and configure your key with wf_set_key().", call. = FALSE)
  }

  # ---- key check ----
  if (isTRUE(verbose)) message("Checking key for service: ", service, " ...")
  key_ok <- tryCatch({ ecmwfr::wf_get_key(user = user); TRUE },
                     error = function(e) FALSE)
  if (!key_ok) {
    warning("No key found for service '", service, "' and user '", user,
            "'. Configure it with ecmwfr::wf_set_key(user='", user,
            "', service='", service, "').", call. = FALSE)
  }

  # ---- normalizations ----
  months <- pad2(months)
  days   <- pad2(days)

  years_chr <- as.character(years)
  period    <- paste0(min(years_chr), "_", max(years_chr))
  lh        <- as.integer(leadtime_hour)
  lead_part <- paste0(min(lh), "-", max(lh))

  init_part <- paste0(month_abb[as.integer(months[1])], days[1])

  if (!dir.exists(out_dir)) {
    if (verbose) message("Creating output directory: ", out_dir)
    dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  }

  # ---- filename template ----
  if (is.null(filename_tpl)) {
    filename_tpl <- "{modelsys}_{var}_{init}_{period}_{lead}.nc"
  }

  # ---- parse and pre-build per-year requests ----
  parsed <- lapply(center_variables, parse_center_variable)

  # warn on unknown models (do not stop)
  models_all <- vapply(parsed, `[[`, "", "model")
  unk <- setdiff(unique(models_all), VALID_MODELS)
  if (length(unk)) warning("Unknown models (continuing anyway): ", paste(unk, collapse = ", "))

  # Build (model, system, variable, year) jobs
  make_one_year <- function(p, yy) {
    model <- p$model       # lowercase
    system <- p$system
    var_in <- p$variable

    var_api <- var_api_name(var_in)     # for request
    var_for_name <- var_short(var_api)   # for filename

    modelsys <- paste0(model, system)   # preserve underscores
    base_name <- tpl_replace(filename_tpl, list(
      modelsys = modelsys,
      var      = var_for_name,
      init     = init_part,
      period   = period,
      lead     = lead_part,
      dataset  = dataset_short_name,
      year     = yy
    ))

    # If the template doesn't include {year}, append "_YYYY" before ".nc"
    if (!grepl("\\{year\\}", filename_tpl, fixed = TRUE) && !grepl("_[0-9]{4}\\.nc$", base_name)) {
      base_name <- sub("\\.nc$", paste0("_", yy, ".nc"), base_name, perl = TRUE)
    }

    target <- file.path(out_dir, base_name)

    if (!force_download && file.exists(target)) {
      return(list(skip = TRUE, target = target,
                  model = model, system = system, variable = var_in, year = yy,
                  req = NULL))
    }

    req <- c(list(dataset_short_name = dataset_short_name), base_query)
    if (is.null(req$format) && is.null(req$data_format)) req$format <- "netcdf"
    req$target              <- basename(target)
    req$originating_centre  <- model
    req$system              <- system
    req$variable            <- var_api

    req$year <- as.character(yy)         # <- per-year chunk
    req$month <- months
    req$day   <- days
    req$time  <- times
    req$leadtime_hour <- as.character(leadtime_hour)
    if (!is.null(area)) {
      stopifnot(length(area) == 4)
      req$area <- area
    }

    list(skip = FALSE, target = target,
         model = model, system = system, variable = var_in, year = yy,
         req = req)
  }

  jobs <- list()
  for (p in parsed) {
    for (yy in years_chr) {
      jobs[[length(jobs) + 1]] <- make_one_year(p, yy)
    }
  }

  # pre-fill results with SKIPs
  out <- list()
  for (j in jobs) {
    if (isTRUE(j$skip)) {
      out[[length(out) + 1]] <- data.frame(
        file = j$target, status = "skip", error = NA_character_,
        model = j$model, system = j$system, variable = j$variable, year = j$year,
        stringsAsFactors = FALSE
      )
    }
  }

  submit <- Filter(function(z) !isTRUE(z$skip), jobs)
  if (length(submit) == 0L) {
    if (verbose) message("Nothing to download (all files already present).")
    return(invisible(do.call(rbind, out)))
  }

  # ---- parallel (batch) or sequential ----
  batch_ok <- isTRUE(parallel) && is.function(ecmwfr::wf_request_batch)

  if (batch_ok) {
    if (verbose) message("Submitting batch via wf_request_batch() with ", workers, " workers...")
    request_list <- lapply(submit, `[[`, "req")
    target_list  <- vapply(submit, `[[`, "", "target")

    ecmwfr::wf_request_batch(
      request_list   = request_list,
      workers        = max(1L, as.integer(workers)),
      user           = user,
      path           = out_dir,
      time_out       = timeout_sec,
      retry          = 30,  # polling interval (seconds)
      total_timeout  = length(request_list) * timeout_sec / max(1, workers)
    )

    for (i in seq_along(submit)) {
      ok <- file.exists(target_list[i])
      out[[length(out) + 1]] <- data.frame(
        file = target_list[i],
        status = if (ok) "ok" else "fail",
        error = if (ok) NA_character_ else "File missing after batch",
        model = submit[[i]]$model,
        system = submit[[i]]$system,
        variable = submit[[i]]$variable,
        year = submit[[i]]$year,
        stringsAsFactors = FALSE
      )
    }

  } else {
    if (isTRUE(parallel) && !is.function(ecmwfr::wf_request_batch) && verbose) {
      message("wf_request_batch() not available — falling back to sequential mode.")
    }

    for (j in submit) {
      attempt <- 1L; last_err <- NULL; success <- FALSE
      while (attempt <= max(1L, as.integer(tries)) && !success) {
        if (verbose) message(sprintf("[REQ] %s_%s.%s (%s) | attempt %d/%d -> %s",
                                     j$model, j$system, j$variable, j$year,
                                     attempt, max(1L, as.integer(tries)), basename(j$target)))
        ok <- tryCatch({
          ecmwfr::wf_request(
            user = user, request = j$req, transfer = TRUE, path = out_dir,
            verbose = verbose, time_out = timeout_sec, job_name = job_name, ...
          ); TRUE
        }, error = function(e) {
          if (verbose) message(sprintf("Error: %s", e$message))
          last_err <<- e$message; FALSE
        })

        if (ok && file.exists(j$target)) {
          success <- TRUE
          out[[length(out) + 1]] <- data.frame(
            file = j$target, status = "ok", error = NA_character_,
            model = j$model, system = j$system, variable = j$variable, year = j$year,
            stringsAsFactors = FALSE
          )
        } else {
          if (attempt >= tries) {
            out[[length(out) + 1]] <- data.frame(
              file = j$target, status = "fail", error = last_err %||% "Unknown error",
              model = j$model, system = j$system, variable = j$variable, year = j$year,
              stringsAsFactors = FALSE
            )
          } else {
            if (verbose) message(sprintf("...retry in %ds", sleep_sec))
            Sys.sleep(sleep_sec)
          }
          attempt <- attempt + 1L
        }
      }
    }
  }

  invisible(do.call(rbind, out))
}
