
# ---------------------------------------------------------------------------
# Local helpers
# ---------------------------------------------------------------------------

`%||%` <- function(a, b) if (is.null(a)) b else a

#' Pick the first matching name from a candidate list
#'
#' Internal helper used to resolve dimension or column names by matching
#' candidate names against a set of available names in a case-insensitive way.
#'
#' @param targets Character vector of candidate names, typically already
#' lower-cased.
#' @param available_names Character vector of names available in the current
#' object.
#'
#' @return A single matching name using the original case from
#' \code{available_names}, or \code{NULL} if no match is found.
#'
#' @keywords internal
pick_name <- function(targets, available_names) {
  if (!length(targets)) return(NULL)
  idx <- match(targets[1], tolower(available_names))
  if (is.na(idx)) return(NULL)
  available_names[idx]
}

#' Safely extract a unit string from a vector
#'
#' Internal helper that extracts unit information from vectors, including
#' objects of class \code{units} or vectors carrying a \code{"units"}
#' attribute.
#'
#' @param v A vector that may carry units metadata.
#'
#' @return A character string describing the unit, or \code{NA_character_} if
#' no unit information is available.
#'
#' @keywords internal
deparse_unit_safe <- function(v) {
  has_units <- requireNamespace("units", quietly = TRUE)
  if (has_units && inherits(v, "units")) {
    return(as.character(units::deparse_unit(v)))
  }
  u <- attr(v, "units", exact = TRUE)
  if (is.null(u)) return(NA_character_)
  as.character(u)
}

#' Convert a units-aware vector to numeric
#'
#' Internal helper that converts vectors with units to plain numeric values.
#'
#' If the \pkg{units} package is available and the input inherits from class
#' \code{units}, units are dropped safely before conversion. Otherwise, the
#' function falls back to \code{as.numeric()}.
#'
#' @param v A numeric-like vector, possibly carrying units.
#'
#' @return A numeric vector.
#'
#' @keywords internal
drop_units_num <- function(v) {
  has_units <- requireNamespace("units", quietly = TRUE)
  if (has_units && inherits(v, "units")) {
    return(as.numeric(units::drop_units(v)))
  }
  as.numeric(v)
}

#' Parse CF-style time unit strings
#'
#' Internal helper that parses CF-style time unit strings such as
#' \code{"days since 1900-01-01"} or \code{"hours since 1970-01-01 00:00:00"}
#' and returns the corresponding origin and conversion factor.
#'
#' Supported units include milliseconds, seconds, minutes, hours, and days.
#'
#' @param unit_str Character string describing time units.
#'
#' @return
#' A list with components:
#' \itemize{
#'   \item \code{mult}: numeric conversion factor to seconds;
#'   \item \code{origin}: \code{POSIXct} origin date-time.
#' }
#' Returns \code{NULL} if the input cannot be parsed.
#'
#' @keywords internal
parse_cf_time_units <- function(unit_str) {
  if (is.null(unit_str) || is.na(unit_str) || !nzchar(trimws(unit_str))) {
    return(NULL)
  }

  unit_str_orig <- trimws(unit_str)
  unit_str_lc <- tolower(unit_str_orig)

  pattern <- paste0(
    "^(milliseconds?|msecs?|ms|seconds?|secs?|sec|minutes?|mins?|min|",
    "hours?|hrs?|hr|h|days?|day)\\s+since\\s+(.+)$"
  )

  m <- regexec(pattern, unit_str_lc, perl = TRUE)
  g <- regmatches(unit_str_lc, m)[[1]]
  if (length(g) != 3) return(NULL)

  # Recover origin from original string (to preserve case/content)
  m2 <- regexec(pattern, unit_str_orig, ignore.case = TRUE, perl = TRUE)
  g2 <- regmatches(unit_str_orig, m2)[[1]]
  if (length(g2) != 3) return(NULL)

  unit_name <- tolower(g[2])
  origin_raw <- trimws(g2[3])

  origin_posix <- suppressWarnings(as.POSIXct(origin_raw, tz = "UTC"))
  if (is.na(origin_posix)) {
    # Try fallback date-only parsing
    origin_date <- suppressWarnings(as.Date(origin_raw))
    if (is.na(origin_date)) return(NULL)
    origin_posix <- as.POSIXct(origin_date, tz = "UTC")
  }

  mult <- switch(
    unit_name,
    "millisecond" = 0.001,
    "milliseconds" = 0.001,
    "msec" = 0.001,
    "msecs" = 0.001,
    "ms" = 0.001,
    "second" = 1,
    "seconds" = 1,
    "sec" = 1,
    "secs" = 1,
    "minute" = 60,
    "minutes" = 60,
    "min" = 60,
    "mins" = 60,
    "hour" = 3600,
    "hours" = 3600,
    "hr" = 3600,
    "hrs" = 3600,
    "h" = 3600,
    "day" = 86400,
    "days" = 86400,
    NULL
  )

  if (is.null(mult)) return(NULL)

  list(mult = mult, origin = origin_posix)
}


#' Parse time vector into POSIXct
#'
#' Internal helper to convert various time representations
#' (numeric with units, character, Date, POSIXct) into POSIXct.
#'
#' @param v Vector containing time information
#' @param tz Timezone
#'
#' @return POSIXct vector
#'
#' @keywords internal
parse_time_vector <- function(v, tz = "UTC") {
  has_units <- requireNamespace("units", quietly = TRUE)
  # Already POSIXt
  if (inherits(v, "POSIXt")) {
    return(as.POSIXct(v, tz = tz))
  }

  # Date
  if (inherits(v, "Date")) {
    return(as.POSIXct(v, tz = tz))
  }

  # Numeric or units-based numeric
  if (is.numeric(v) || (has_units && inherits(v, "units"))) {
    unit_str <- deparse_unit_safe(v)
    info <- parse_cf_time_units(unit_str)

    vv <- drop_units_num(v)

    if (!is.null(info)) {
      return(as.POSIXct(info$origin + vv * info$mult, tz = tz, origin = "1970-01-01"))
    }

    # Conservative fallback: assume Unix epoch seconds only if values look plausible
    # Otherwise return NA to avoid silent mistakes
    if (all(is.na(vv))) {
      return(as.POSIXct(rep(NA_real_, length(vv)), origin = "1970-01-01", tz = tz))
    }

    rng <- range(vv, na.rm = TRUE)
    plausible_epoch_sec <- is.finite(rng[1]) && is.finite(rng[2]) &&
      rng[1] > -2208988800 && rng[2] < 4102444800  # ~1900 to ~2100

    if (plausible_epoch_sec) {
      return(as.POSIXct(vv, origin = "1970-01-01", tz = tz))
    }

    return(as.POSIXct(rep(NA_real_, length(vv)), origin = "1970-01-01", tz = tz))
  }

  # Character / factor
  if (is.factor(v)) {
    v <- as.character(v)
  }

  if (is.character(v)) {
    x <- trimws(v)
    x[x %in% c("", "NA", "NaN", "NULL", "null")] <- NA_character_

    out <- rep(as.POSIXct(NA, tz = tz), length(x))

    # Try direct POSIXct
    suppressWarnings({
      p1 <- as.POSIXct(x, tz = tz)
    })
    ok1 <- !is.na(p1)
    out[ok1] <- p1[ok1]

    # Try date-only
    if (any(!ok1)) {
      suppressWarnings({
        d1 <- as.Date(x[!ok1])
      })
      ok2 <- !is.na(d1)
      out[!ok1][ok2] <- as.POSIXct(d1[ok2], tz = tz)
    }

    # Try common explicit formats
    remaining <- is.na(out) & !is.na(x)
    if (any(remaining)) {
      fmts <- c(
        "%Y-%m-%d %H:%M:%S",
        "%Y-%m-%d %H:%M",
        "%Y/%m/%d %H:%M:%S",
        "%Y/%m/%d %H:%M",
        "%Y-%m-%d",
        "%Y/%m/%d",
        "%d/%m/%Y",
        "%d-%m-%Y",
        "%m/%d/%Y",
        "%Y%m%d",
        "%Y%m%d%H%M%S"
      )

      for (fmt in fmts) {
        suppressWarnings({
          pp <- as.POSIXct(x[remaining], format = fmt, tz = tz)
        })
        ok <- !is.na(pp)
        if (any(ok)) {
          idx <- which(remaining)
          out[idx[ok]] <- pp[ok]
          remaining <- is.na(out) & !is.na(x)
        }
        if (!any(remaining)) break
      }
    }

    return(out)
  }

  # Unsupported type
  as.POSIXct(rep(NA_real_, length(v)), origin = "1970-01-01", tz = tz)
}

#' Build a unified DATE column from available time information
#'
#' Internal helper that reconstructs a \code{DATE} column from a data table
#' containing one or more time-related columns.
#'
#' The function first tries to parse a direct time column. If unsuccessful, it
#' then attempts to reconstruct dates from a forecast reference time and a
#' forecast period / lead time column.
#'
#' @param DT A \code{data.table} containing the raw data.
#' @param time_col Optional name of the direct time column.
#' @param ref_col Optional name of the forecast reference time column.
#' @param period_col Optional name of the forecast lead time / period column.
#' @param tz Time zone used to construct the output \code{DATE} vector.
#'
#' @return A \code{POSIXct} vector of length \code{nrow(DT)}.
#'
#' @keywords internal
build_date_column <- function(DT, time_col = NULL, ref_col = NULL, period_col = NULL, tz = "UTC") {
  n <- nrow(DT)

  # Case 1: direct time column
  if (!is.null(time_col) && time_col %in% names(DT)) {
    date_out <- parse_time_vector(DT[[time_col]], tz = tz)
    if (sum(!is.na(date_out)) > 0) {
      return(date_out)
    }
  }

  # Case 2: reference time + forecast period
  if (!is.null(ref_col) && !is.null(period_col) &&
      ref_col %in% names(DT) && period_col %in% names(DT)) {

    ref_time <- parse_time_vector(DT[[ref_col]], tz = tz)

    per <- DT[[period_col]]
    per_unit <- deparse_unit_safe(per)

    per_num <- drop_units_num(per)
    if (all(is.na(per_num))) {
      return(as.POSIXct(rep(NA_real_, n), origin = "1970-01-01", tz = tz))
    }

    per_unit_lc <- tolower(per_unit %||% "")

    per_secs <- if (grepl("millisecond|msec|\\bms\\b", per_unit_lc)) {
      per_num / 1000
    } else if (grepl("minute|min", per_unit_lc)) {
      per_num * 60
    } else if (grepl("hour|hr|\\bh\\b", per_unit_lc)) {
      per_num * 3600
    } else if (grepl("day", per_unit_lc)) {
      per_num * 86400
    } else if (grepl("second|sec", per_unit_lc)) {
      per_num
    } else {
      # Heuristic: if no units, assume seconds only as last resort
      per_num
    }

    out <- ref_time + per_secs
    if (sum(!is.na(out)) > 0) {
      return(out)
    }
  }

  as.POSIXct(rep(NA_real_, n), origin = "1970-01-01", tz = tz)
}



#' Filter a grid by bbox with robust fallbacks
#'
#' - Understands bbox as named numeric c(xmin, ymin, xmax, ymax) OR
#'   as unnamed numeric of length 4 in either (xmin, ymin, xmax, ymax)
#'   or (S, W, N, E) order.
#' - Auto-aligns bbox longitude convention to data convention
#'   ([-180, 180] vs [0, 360]).
#' - Adds a small tolerance proportional to grid resolution to avoid
#'   "empty selection" on coarse grids.
#' - If no cell intersects the bbox, falls back to:
#'     (a) the cell that CONTAINS the bbox center, else
#'     (b) the NEAREST grid cell to the bbox center,
#'   or errors if fallback = "error".
#'
#' @param DT A data.table with at least longitude/latitude columns.
#' @param bbox Numeric length-4; can be named (xmin,xmax,ymin,ymax) or
#'   unnamed (interpreted as (xmin,ymin,xmax,ymax) or (S,W,N,E)).
#' @param lon_col,lat_col Column names of longitude/latitude in DT.
#' @param fallback Fallback strategy if no row intersects bbox:
#'   one of c("contains_center","nearest_center","error").
#' @param auto_align_lon If TRUE, align bbox longitudes to data convention.
#' @param tol_frac Fraction of a grid step to expand bbox for tolerance.
#' @return A data.table filtered to bbox, or the selected fallback cell.
#' @keywords internal
#' @noRd
bbox_filter_or_fallback <- function(
    DT, bbox,
    lon_col = "lon", lat_col = "lat",
    fallback = c("contains_center","nearest_center","error"),
    auto_align_lon = TRUE,
    tol_frac = 0.25
) {
  stopifnot(is.numeric(bbox), length(bbox) == 4)
  fallback <- match.arg(fallback)

  if (!data.table::is.data.table(DT)) {
    DT <- data.table::as.data.table(DT)
  }
  stopifnot(lon_col %in% names(DT), lat_col %in% names(DT))

  # --- helpers (local scope)
  normalize_bbox <- function(b) {
    nms <- tolower(names(b))
    # Named bbox: expect xmin/xmax/ymin/ymax (in any order)
    if (!is.null(nms) && all(c("xmin","xmax","ymin","ymax") %in% nms)) {
      return(list(
        xmin = as.numeric(b["xmin"]),
        xmax = as.numeric(b["xmax"]),
        ymin = as.numeric(b["ymin"]),
        ymax = as.numeric(b["ymax"])
      ))
    }
    # Unnamed numeric: try (xmin, ymin, xmax, ymax) first
    if (b[1] <= b[3] && b[2] <= b[4]) {
      return(list(xmin = b[1], ymin = b[2], xmax = b[3], ymax = b[4]))
    }
    # Else treat as (S, W, N, E)
    list(xmin = b[2], ymin = b[1], xmax = b[4], ymax = b[3])
  }

  align_bbox_longitude <- function(bb, lon_vec) {
    if (!auto_align_lon) return(bb)
    rng <- range(lon_vec, na.rm = TRUE)
    # Data in [-180,180] and bbox likely in [0,360] -> convert bbox to [-180,180]
    if (rng[1] < 0 && rng[2] <= 180 && bb$xmin >= 0 && bb$xmax >= 0) {
      bb$xmin <- ifelse(bb$xmin > 180, bb$xmin - 360, bb$xmin)
      bb$xmax <- ifelse(bb$xmax > 180, bb$xmax - 360, bb$xmax)
    }
    # Data in [0,360] and bbox in [-180,180] -> convert bbox to [0,360]
    if (rng[1] >= 0 && rng[2] > 180 && (bb$xmin < 0 || bb$xmax < 0)) {
      conv <- function(x) ifelse(x < 0, x + 360, x)
      bb$xmin <- conv(bb$xmin); bb$xmax <- conv(bb$xmax)
    }
    bb
  }

  grid_edges <- function(v) {
    v <- sort(unique(as.numeric(v)))
    if (length(v) == 1L) return(c(v[1] - 0.5, v[1] + 0.5))
    dv <- diff(v)
    c(v[1] - dv[1]/2, v[-length(v)] + dv/2, v[length(v)] + dv[length(dv)]/2)
  }

  # --- 1) Normalize bbox + align longitude
  bb <- normalize_bbox(bbox)
  bb <- align_bbox_longitude(bb, DT[[lon_col]])

  # --- 2) Add tolerance based on grid spacing (helps coarse grids)
  lon_step <- diff(sort(unique(DT[[lon_col]]))); lon_step <- lon_step[is.finite(lon_step)]
  lat_step <- diff(sort(unique(DT[[lat_col]]))); lat_step <- lat_step[is.finite(lat_step)]
  if (length(c(lon_step, lat_step))) {
    tol <- suppressWarnings(tol_frac * min(c(lon_step, lat_step), na.rm = TRUE))
    if (is.finite(tol) && tol > 0) {
      bb$xmin <- bb$xmin - tol; bb$xmax <- bb$xmax + tol
      bb$ymin <- bb$ymin - tol; bb$ymax <- bb$ymax + tol
    }
  }

  # --- 3) Regular bbox filter
  hits <- DT[get(lon_col) >= bb$xmin & get(lon_col) <= bb$xmax &
               get(lat_col) >= bb$ymin & get(lat_col) <= bb$ymax]

  # --- 4) Fallbacks
  if (!nrow(hits)) {
    if (fallback == "error") {
      stop("Bbox does not intersect the grid (after normalization).")
    }

    # Center of bbox
    cx <- (bb$xmin + bb$xmax) / 2
    cy <- (bb$ymin + bb$ymax) / 2

    vlon <- sort(unique(DT[[lon_col]]))
    vlat <- sort(unique(DT[[lat_col]]))

    if (fallback == "contains_center") {
      # Choose the cell that CONTAINS (cx, cy) using reconstructed edges
      e_lon <- grid_edges(vlon)
      e_lat <- grid_edges(vlat)
      i <- findInterval(cx, e_lon, left.open = FALSE, rightmost.closed = TRUE)
      j <- findInterval(cy, e_lat, left.open = FALSE, rightmost.closed = TRUE)
      i <- max(1, min(i, length(vlon)))
      j <- max(1, min(j, length(vlat)))
      sel_lon <- vlon[i]; sel_lat <- vlat[j]
      hits <- DT[get(lon_col) == sel_lon & get(lat_col) == sel_lat]
    } else if (fallback == "nearest_center") {
      # Nearest grid center to (cx, cy)
      grid <- unique(DT[, c(lon_col, lat_col), with = FALSE])
      # Compute squared distance
      grid[, `__dist2__` := (get(lon_col) - cx)^2 + (get(lat_col) - cy)^2]
      data.table::setorder(grid, `__dist2__`)
      sel_lon <- grid[[lon_col]][1]; sel_lat <- grid[[lat_col]][1]
      hits <- DT[get(lon_col) == sel_lon & get(lat_col) == sel_lat]
      grid[, `__dist2__` := NULL]
    }
  }

  if (!nrow(hits)) {
    stop("Bbox does not intersect the grid and no suitable fallback cell could be found.")
  }
  hits[]
}

#' Robust longitude/latitude detection for a NetCDF file (ncdf4)
#'
#' - Case-insensitive name matching (x/lon/longitude/long, y/lat/latitude, etc.)
#' - Searches both dimensions and variables
#' - Falls back to CF-style attributes: standard_name + units
#' - Supports curvilinear grids (2D lon/lat)
#'
#' @param path Character, path to a NetCDF file.
#' @return A list with:
#'   - lon: numeric vector or matrix of longitudes
#'   - lat: numeric vector or matrix of latitudes
#'   - lon_name, lat_name: the detected variable/dimension names (original case)
#'   - from: "dim" or "var" (where the coords were read from)
#'   - curvilinear: TRUE if 2D lon/lat
#' @keywords internal
#' @noRd
ncdf4_guess_lonlat <- function(path) {
  stopifnot(file.exists(path))
  nc <- ncdf4::nc_open(path)
  on.exit(ncdf4::nc_close(nc), add = TRUE)

  # Pools
  dim_names <- names(nc$dim)
  var_names <- names(nc$var)
  dim_lc <- tolower(dim_names)
  var_lc <- tolower(var_names)

  # Candidate name dictionaries (ordered by preference)
  lon_candidates <- c("lon","longitude","long","x","grid_longitude","nav_lon","rlon")
  lat_candidates <- c("lat","latitude","y","grid_latitude","nav_lat","rlat")

  # Map lower-case -> original case for later retrieval
  restore_case <- function(name_lc, pool_names, pool_lc) {
    if (is.na(name_lc)) return(NA_character_)
    idx <- match(name_lc, pool_lc)
    if (is.na(idx)) NA_character_ else pool_names[idx]
  }

  # Case-insensitive pick from a pool of names
  pick_ci <- function(candidates, pool_lc) {
    cand_lc <- tolower(candidates)
    found <- cand_lc[cand_lc %in% pool_lc]
    if (length(found)) found[[1]] else NA_character_
  }

  # Attribute-based CF detection from variables
  # - standard_name = "longitude"/"latitude"
  # - units include "degrees_east|degrees_west" or "degrees_north|degrees_south"
  cf_find_by_attr <- function(target = c("lon","lat")) {
    target <- match.arg(target)
    hit <- NA_character_
    for (vn in var_names) {
      atts <- try(ncdf4::ncatt_get(nc, vn), silent = TRUE)
      if (inherits(atts, "try-error")) next
      std <- tolower(if (!is.null(atts$standard_name)) atts$standard_name else "")
      unt <- tolower(if (!is.null(atts$units))         atts$units         else "")
      if (target == "lon") {
        if (std == "longitude" || grepl("degrees_east|degrees_west", unt)) {
          hit <- vn; break
        }
      } else {
        if (std == "latitude"  || grepl("degrees_north|degrees_south", unt)) {
          hit <- vn; break
        }
      }
    }
    hit
  }

  # 1) Try to find longitude/latitude among DIMENSIONS first
  lon_dim_lc <- pick_ci(lon_candidates, dim_lc)
  lat_dim_lc <- pick_ci(lat_candidates, dim_lc)

  lon_from <- lat_from <- NA_character_
  lon_name <- lat_name <- NA_character_

  if (!is.na(lon_dim_lc)) {
    lon_name <- restore_case(lon_dim_lc, dim_names, dim_lc)
    lon_vals <- nc$dim[[lon_name]]$vals
    lon_from <- "dim"
  }

  if (!is.na(lat_dim_lc)) {
    lat_name <- restore_case(lat_dim_lc, dim_names, dim_lc)
    lat_vals <- nc$dim[[lat_name]]$vals
    lat_from <- "dim"
  }

  # 2) If not found in dims, try VARIABLES by name
  if (is.na(lon_name)) {
    lon_var_lc <- pick_ci(lon_candidates, var_lc)
    if (!is.na(lon_var_lc)) {
      lon_name <- restore_case(lon_var_lc, var_names, var_lc)
      lon_vals <- ncdf4::ncvar_get(nc, lon_name)
      lon_from <- "var"
    }
  }
  if (is.na(lat_name)) {
    lat_var_lc <- pick_ci(lat_candidates, var_lc)
    if (!is.na(lat_var_lc)) {
      lat_name <- restore_case(lat_var_lc, var_names, var_lc)
      lat_vals <- ncdf4::ncvar_get(nc, lat_name)
      lat_from <- "var"
    }
  }

  # 3) If still missing, try VARIABLES by CF attributes
  if (is.na(lon_name)) {
    cand <- cf_find_by_attr("lon")
    if (!is.na(cand)) {
      lon_name <- cand
      lon_vals <- ncdf4::ncvar_get(nc, lon_name)
      lon_from <- "var"
    }
  }
  if (is.na(lat_name)) {
    cand <- cf_find_by_attr("lat")
    if (!is.na(cand)) {
      lat_name <- cand
      lat_vals <- ncdf4::ncvar_get(nc, lat_name)
      lat_from <- "var"
    }
  }

  # 4) Validate and report
  if (is.na(lon_name) || is.na(lat_name)) {
    stop(
      "Could not detect longitude/latitude coordinates.\n",
      "- Available dims: ", paste(dim_names, collapse = ", "), "\n",
      "- Available vars: ", paste(var_names, collapse = ", "), "\n",
      "Tried names: lon{", paste(lon_candidates, collapse=","), "}, ",
      "lat{", paste(lat_candidates, collapse=","), "} and CF attrs."
    )
  }

  # 5) Basic checks: numeric, curvilinear detection
  if (!is.numeric(lon_vals)) stop("Longitude values are not numeric (name: ", lon_name, ").")
  if (!is.numeric(lat_vals)) stop("Latitude values are not numeric (name: ", lat_name, ").")

  curvilinear <- is.matrix(lon_vals) || is.matrix(lat_vals)

  # 6) Optional sanity checks: monotonicity for rectilinear grids
  if (!curvilinear) {
    lon_monotone <- isTRUE(all(diff(as.numeric(lon_vals)) >= 0)) ||
      isTRUE(all(diff(as.numeric(lon_vals)) <= 0))
    lat_monotone <- isTRUE(all(diff(as.numeric(lat_vals)) >= 0)) ||
      isTRUE(all(diff(as.numeric(lat_vals)) <= 0))
    if (!lon_monotone || !lat_monotone) {
      message("Warning: lon/lat are not strictly monotone. Grid may be irregular or wrapped.")
    }
  }

  list(
    lon = lon_vals,
    lat = lat_vals,
    lon_name = lon_name,
    lat_name = lat_name,
    from = ifelse(identical(lon_from, "var") || identical(lat_from, "var"), "var", "dim"),
    curvilinear = curvilinear
  )
}
