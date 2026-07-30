#' Build expected CDS archive entries
#'
#' @keywords internal
#' @noRd
wass2s__expected_cds_entries <- function(dataset_short_name,
                                         center_variables,
                                         years,
                                         months,
                                         days,
                                         leadtime_hour,
                                         out_dir,
                                         filename_tpl = NULL) {
  if (is.null(filename_tpl)) {
    filename_tpl <- "{modelsys}_{var}_{init}_{period}_{lead}.nc"
  }

  months <- pad2(months)
  days <- pad2(days)
  years_chr <- as.character(years)
  period <- paste0(min(years_chr), "_", max(years_chr))
  lh <- as.integer(leadtime_hour)
  lead_part <- paste0(min(lh), "-", max(lh))
  init_part <- paste0(month_abb[as.integer(months[1])], days[1])

  parsed <- lapply(center_variables, parse_center_variable)
  rows <- list()

  for (p in parsed) {
    var_api <- var_api_name(p$variable)
    var_for_name <- var_short(var_api)
    modelsys <- paste0(p$model, p$system)

    for (yy in years_chr) {
      base_name <- tpl_replace(filename_tpl, list(
        modelsys = modelsys,
        var = var_for_name,
        init = init_part,
        period = period,
        lead = lead_part,
        dataset = dataset_short_name,
        year = yy
      ))

      if (!grepl("\\{year\\}", filename_tpl, fixed = TRUE) &&
          !grepl("_[0-9]{4}\\.nc$", base_name)) {
        base_name <- sub("\\.nc$", paste0("_", yy, ".nc"), base_name, perl = TRUE)
      }

      rows[[length(rows) + 1L]] <- tibble::tibble(
        center_variable = paste0(p$model, "_", p$system, ".", toupper(var_for_name)),
        model = p$model,
        system = p$system,
        variable = p$variable,
        variable_api = var_api,
        variable_short = var_for_name,
        year = as.integer(yy),
        file = normalizePath(file.path(out_dir, base_name), winslash = "/", mustWork = FALSE)
      )
    }
  }

  dplyr::bind_rows(rows)
}

#' Inspect a NetCDF file safely
#'
#' @keywords internal
#' @noRd
wass2s__inspect_netcdf <- function(path, required_dims = character(), required_vars = character()) {
  out <- list(
    can_open = NA,
    size_bytes = if (file.exists(path)) file.info(path)$size else NA_real_,
    dims = character(),
    vars = character(),
    error = NA_character_
  )

  if (!file.exists(path)) {
    out$can_open <- FALSE
    out$error <- "missing"
    return(out)
  }
  if (!requireNamespace("ncdf4", quietly = TRUE)) {
    out$can_open <- NA
    out$error <- "ncdf4 not installed"
    return(out)
  }

  nc <- tryCatch(ncdf4::nc_open(path), error = function(e) e)
  if (inherits(nc, "error")) {
    out$can_open <- FALSE
    out$error <- nc$message
    return(out)
  }
  on.exit(ncdf4::nc_close(nc), add = TRUE)

  out$can_open <- TRUE
  out$dims <- names(nc$dim)
  out$vars <- names(nc$var)

  missing_dims <- setdiff(required_dims, out$dims)
  missing_vars <- setdiff(required_vars, out$vars)
  if (length(missing_dims) || length(missing_vars)) {
    out$can_open <- FALSE
    out$error <- paste(
      c(
        if (length(missing_dims)) paste0("missing dims: ", paste(missing_dims, collapse = ", ")),
        if (length(missing_vars)) paste0("missing vars: ", paste(missing_vars, collapse = ", "))
      ),
      collapse = "; "
    )
  }

  out
}

#' Validate a CDS/ECMWF archive
#'
#' Audits a local CDS archive against the files expected from a
#' \code{wass2s_download_cds()} request. It checks missing files, suspiciously
#' small files, and optionally NetCDF readability and required dimensions or
#' variables.
#'
#' @param out_dir Archive directory.
#' @param dataset_short_name CDS dataset short name.
#' @param center_variables Character vector such as \code{"ecmwf_51.PRCP"}.
#' @param years Years expected in the archive.
#' @param months,days,leadtime_hour Request fields used to reconstruct expected
#'   filenames.
#' @param filename_tpl Filename template used by \code{wass2s_download_cds()}.
#' @param min_bytes Minimum acceptable file size. Files below this size are
#'   marked as \code{"too_small"}.
#' @param check_netcdf Logical. If \code{TRUE}, try opening files with
#'   \pkg{ncdf4}.
#' @param required_dims Optional required NetCDF dimensions.
#' @param required_vars Optional required NetCDF variables.
#'
#' @return A list with \code{audit}, \code{summary}, \code{missing},
#'   \code{corrupt}, and \code{ok} tables.
#'
#' @export
wass2s_validate_archive <- function(out_dir,
                                    dataset_short_name,
                                    center_variables,
                                    years,
                                    months,
                                    days,
                                    leadtime_hour,
                                    filename_tpl = NULL,
                                    min_bytes = 1024,
                                    check_netcdf = TRUE,
                                    required_dims = character(),
                                    required_vars = character()) {
  expected <- wass2s__expected_cds_entries(
    dataset_short_name = dataset_short_name,
    center_variables = center_variables,
    years = years,
    months = months,
    days = days,
    leadtime_hour = leadtime_hour,
    out_dir = out_dir,
    filename_tpl = filename_tpl
  )

  audit_rows <- lapply(seq_len(nrow(expected)), function(i) {
    row <- expected[i, , drop = FALSE]
    path <- row$file[[1]]
    exists <- file.exists(path)
    size <- if (exists) file.info(path)$size else NA_real_

    nc_info <- if (isTRUE(check_netcdf) && exists && is.finite(size) && size >= min_bytes) {
      wass2s__inspect_netcdf(path, required_dims = required_dims, required_vars = required_vars)
    } else {
      list(can_open = NA, dims = character(), vars = character(), error = NA_character_)
    }

    status <- dplyr::case_when(
      !exists ~ "missing",
      is.finite(size) && size < min_bytes ~ "too_small",
      isFALSE(nc_info$can_open) ~ "invalid_netcdf",
      TRUE ~ "ok"
    )

    dplyr::mutate(
      row,
      exists = exists,
      size_bytes = as.numeric(size),
      status = status,
      nc_can_open = nc_info$can_open,
      nc_dims = paste(nc_info$dims, collapse = ";"),
      nc_vars = paste(nc_info$vars, collapse = ";"),
      error = nc_info$error
    )
  })

  audit <- dplyr::bind_rows(audit_rows)
  summary <- audit |>
    dplyr::count(.data$status, name = "n") |>
    dplyr::arrange(.data$status)

  structure(
    list(
      audit = audit,
      summary = summary,
      missing = dplyr::filter(audit, .data$status == "missing"),
      corrupt = dplyr::filter(audit, .data$status %in% c("too_small", "invalid_netcdf")),
      ok = dplyr::filter(audit, .data$status == "ok")
    ),
    class = c("wass2s_archive_audit", "list")
  )
}

#' Build a repair plan for a CDS/ECMWF archive
#'
#' Converts an archive audit into a table of missing or invalid chunks that
#' should be re-downloaded. The output can be inspected before launching any new
#' CDS requests.
#'
#' @param audit Output of \code{wass2s_validate_archive()} or its \code{audit}
#'   table.
#' @param include_too_small,include_invalid_netcdf Logical switches controlling
#'   which failed statuses are included.
#'
#' @return Tibble with files, years and center variables to re-download.
#'
#' @export
wass2s_repair_archive_plan <- function(audit,
                                       include_too_small = TRUE,
                                       include_invalid_netcdf = TRUE) {
  audit_tbl <- if (inherits(audit, "wass2s_archive_audit")) audit$audit else audit
  if (!is.data.frame(audit_tbl) || !"status" %in% names(audit_tbl)) {
    stop("audit must be a wass2s_archive_audit object or an audit data frame.", call. = FALSE)
  }

  statuses <- "missing"
  if (isTRUE(include_too_small)) statuses <- c(statuses, "too_small")
  if (isTRUE(include_invalid_netcdf)) statuses <- c(statuses, "invalid_netcdf")

  audit_tbl |>
    dplyr::filter(.data$status %in% statuses) |>
    dplyr::transmute(
      center_variable = paste0(.data$model, "_", .data$system, ".", toupper(.data$variable_short)),
      model = .data$model,
      system = .data$system,
      variable = .data$variable,
      year = .data$year,
      file = .data$file,
      reason = .data$status,
      error = .data$error
    ) |>
    dplyr::arrange(.data$model, .data$system, .data$variable, .data$year)
}
