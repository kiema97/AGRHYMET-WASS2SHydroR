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

#' Preprocess NetCDF/stars to tidy df (stars-first, multi-ref-time + temporal agg)
#'
#' This function keeps computations in `stars`/arrays as long as possible:
#' - optional bbox crop,
#' - optional ensemble aggregation on the member dimension,
#' - drops/averages any extra dims,
#' - reconstructs a single time dim T from (time|valid_time) or (forecast_reference_time + forecast_period),
#' - temporal aggregation on the (X,Y,T) cube: "year" (default), "month", or "none",
#' - optional spatial aggregation,
#' - finally returns a tidy `data.frame` (long or wide).
#'
#' @param x path to .nc or a `stars` object.
#' @param bbox optional numeric c(N, W, S, E) in lon/lat.
#' @param spatial_reduce "none","mean","median","min","max". Default "none".
#' @param cell_layout when spatial_reduce="none": "long" (default) or "wide".
#' @param cell_prefix prefix for wide columns. Default "val".
#' @param dim_lon,dim_lat,dim_time optional overrides for lon/lat/time dims.
#' @param dim_ref_time,dim_period optional overrides for ref-time & lead/period dims.
#' @param dim_member optional override for member/ensemble dim (e.g. "number").
#' @param ensemble_reduce "mean","median","min","max","none". Default "mean".
#' @param verbose print progress. Default TRUE.
#'
#' @return data.frame with DATE and values (shape depends on args).
#' @export
#' @importFrom data.table as.data.table setDT setorder dcast
#' @importFrom data.table :=
#' @importFrom stats median
wass2s_prepare_data <- function(
    x,
    bbox = NULL,
    spatial_reduce = c("none","mean","median","min","max"),
    cell_layout   = c("long","wide"),
    cell_prefix   = "val",
    dim_lon = NULL, dim_lat = NULL, dim_time = NULL,
    dim_ref_time = NULL, dim_period = NULL, dim_member = NULL,
    ensemble_reduce = c("mean","median","min","max","none"),
    verbose = TRUE
) {
  spatial_reduce  <- match.arg(spatial_reduce)
  cell_layout     <- match.arg(cell_layout)
  ensemble_reduce <- match.arg(ensemble_reduce)

  if (!requireNamespace("stars", quietly = TRUE))
    stop("Package 'stars' is required.")
  if (!requireNamespace("ncdf4", quietly = TRUE))
    stop("Package 'ncdf4' is required.")
  if (!requireNamespace("data.table", quietly = TRUE))
    stop("Package 'data.table' is required.")
  # units est souvent installé avec stars; on l'utilise si dispo
  has_units <- requireNamespace("units", quietly = TRUE)

  # --- read stars object

  obj <- if (inherits(x, "stars")) {
    x
  } else {
    if (!file.exists(x)) stop("File not found: ", x)
    if (verbose) message("[read] ", x)
    suppressWarnings(stars::read_stars(x, proxy = FALSE, quiet = TRUE))
  }



  # detect dimension columns present in df
  cn <- names(stars::st_dimensions(obj))
  # guess lon/lat cols from df colnames, then allow overrides
  guess_lon <- intersect(c("x","lon","longitude","long"), tolower(cn))
  guess_lat <- intersect(c("y","lat","latitude"), tolower(cn))

  nc <- ncdf4_guess_lonlat(x)
  x_vals <- nc$lon
  y_vals <- nc$lat
  # map back to original case in df
  pick_name <- function(targets, cn) {
    if (!length(targets)) return(NULL)
    t <- targets[1]
    cn[match(t, tolower(cn))]
  }
  lon_col <- dim_lon %||% pick_name(guess_lon, cn)
  lat_col <- dim_lat %||% pick_name(guess_lat, cn)
  stars::st_dimensions(obj)[[lon_col]]$values <- x_vals
  stars::st_dimensions(obj)[[lat_col]]$values <- y_vals

  obj1 <- sf::st_crop(obj,bbox)


  # --- stars -> data.frame (long)
  df <- as.data.frame(obj)  # colonnes: dims + variable
  if (!nrow(df)) return(data.frame(DATE = as.POSIXct(NA))[0, ])
  cn <- names(df)

  if (is.null(lon_col) || is.null(lat_col))
    stop("Could not resolve lon/lat columns in data.frame. Found: ", paste(cn, collapse = ", "))

  # possible time columns in df
  guess_time   <- intersect(c("valid_time","time","t"), tolower(cn))
  time_col     <- dim_time %||% pick_name(guess_time, cn)
  ref_col      <- dim_ref_time %||% pick_name(intersect(c("forecast_reference_time","reftime","ref_time"), tolower(cn)), cn)
  period_col   <- dim_period   %||% pick_name(intersect(c("forecast_period","step","lead","leadtime"), tolower(cn)), cn)
  member_col   <- dim_member   %||% pick_name(intersect(c("number","member","ensemble","realization","realisation"), tolower(cn)), cn)

  # variable (last column convention from stars::as.data.frame)
  val_col <- tail(cn, 1L)
  if (val_col %in% c(lon_col, lat_col, time_col, ref_col, period_col, member_col)) {
    # extremely rare edge case: if last column is not the variable
    # choose the first column that is not a known dim
    dims_known <- c(lon_col, lat_col, time_col, ref_col, period_col, member_col)
    val_col <- setdiff(cn, dims_known)[1]
  }

  # --- helpers for units
  deparse_unit_safe <- function(v) {
    if (has_units && inherits(v, "units")) {
      return(units::deparse_unit(v))
    }
    u <- attr(v, "units", exact = TRUE)
    if (is.null(u)) return(NA_character_)
    as.character(u)
  }
  drop_units_num <- function(v) {
    if (has_units && inherits(v, "units")) as.numeric(units::drop_units(v)) else as.numeric(v)
  }
  parse_since_origin <- function(unit_str) {
    # returns list(mult = seconds per unit, origin = POSIXct) or NULL
    if (is.na(unit_str)) return(NULL)
    us <- tolower(unit_str)
    # try "xxx since YYYY-mm-dd HH:MM:SS"
    m <- regexec("^(sec|secs|second|seconds|min|mins|minute|minutes|hour|hours|h|day|days)\\s+since\\s+(.+)$", us)
    g <- regmatches(us, m)[[1]]
    if (length(g) != 3) return(NULL)
    unit <- g[2]; origin <- g[3]
    origin_posix <- suppressWarnings(as.POSIXct(origin, tz = "UTC"))
    if (is.na(origin_posix)) return(NULL)
    mult <- switch(unit,
                   sec = 1, secs = 1, second = 1, seconds = 1,
                   min = 60, mins = 60, minute = 60, minutes = 60,
                   hour = 3600, hours = 3600, h = 3600,
                   day = 86400, days = 86400,
                   1)
    list(mult = mult, origin = origin_posix)
  }

  # --- build DATE
  dfDT <- data.table::as.data.table(df)

  if (!is.null(time_col) && time_col %in% names(dfDT)) {
    # time present directly
    if (inherits(dfDT[[time_col]], "POSIXt")) {
      dfDT[, DATE := as.POSIXct(get(time_col), tz = "UTC")]
    } else {
      ustr <- deparse_unit_safe(dfDT[[time_col]])
      info <- parse_since_origin(ustr)
      if (!is.null(info)) {
        dfDT[, DATE := info$origin + drop_units_num(get(time_col)) * info$mult]
      } else {
        # fallback: treat as epoch seconds
        dfDT[, DATE := as.POSIXct(drop_units_num(get(time_col)), origin = "1970-01-01", tz = "UTC")]
      }
    }
  } else if (!is.null(ref_col) && !is.null(period_col) &&
             ref_col %in% names(dfDT) && period_col %in% names(dfDT)) {
    # ref + period → DATE
    # ref
    ref_ustr <- deparse_unit_safe(dfDT[[ref_col]])
    ref_info <- parse_since_origin(ref_ustr)
    if (is.null(ref_info)) {
      # if ref is raw epoch seconds
      ref_origin <- as.POSIXct("1970-01-01", tz = "UTC")
      ref_secs   <- drop_units_num(dfDT[[ref_col]])
      ref_abs    <- ref_origin + ref_secs
    } else {
      ref_abs <- ref_info$origin + drop_units_num(dfDT[[ref_col]]) * ref_info$mult
    }
    # period
    per_ustr <- deparse_unit_safe(dfDT[[period_col]])
    per_secs <- {
      us <- tolower(per_ustr)
      v  <- drop_units_num(dfDT[[period_col]])
      if (grepl("\\bhour|\\bh\\b", us)) v * 3600
      else if (grepl("day", us)) v * 86400
      else v  # assume seconds if unknown
    }
    dfDT[, DATE := as.POSIXct(ref_abs, tz = "UTC") + per_secs]
  } else {
    dfDT[, DATE := as.POSIXct(NA)]
    warning("No time column found and no (forecast_reference_time + forecast_period); DATE is NA.")
  }

  # --- numeric lon/lat, handle 0..360 → -180..180 when bbox negative
  dfDT[, lon := as.numeric(get(lon_col))]
  dfDT[, lat := as.numeric(get(lat_col))]
  if (!is.null(bbox)) {
    W <- bbox[2]; E <- bbox[4]
    if (max(dfDT$lon, na.rm = TRUE) > 180 && (W < 0 || E < 0)) {
      dfDT[, lon := ifelse(lon > 180, lon - 360, lon)]
    }
  }

  # --- value numeric
  if (val_col %in% names(dfDT)) {
    dfDT[, value := drop_units_num(get(val_col))]
    dfDT[, (val_col) := NULL]
  } else {
    stop("Could not find variable/value column in data.frame.")
  }

  # # --- bbox filter (via lon/lat)
  # if (!is.null(bbox)) {
  #   stopifnot(length(bbox) == 4)
  #   N <- bbox[1]; W <- bbox[2]; S <- bbox[3]; E <- bbox[4]
  #   dfDT <- dfDT[lon >= W & lon <= E & lat >= S & lat <= N]
  #   if (!nrow(dfDT)) stop("Bbox does not intersect the grid (after normalization).")
  # }

  if (!is.null(bbox)) {
    names(bbox) <- c("ymax","xmin", "ymin", "xmax")
    dfDT <- bbox_filter_or_fallback(
      DT = dfDT,
      bbox = bbox,
      lon_col = "lon",
      lat_col = "lat",
      fallback = "contains_center",   # ou "nearest_center" / "error"
      auto_align_lon = TRUE,
      tol_frac = 0.25
    )
  }


  # --- ensemble aggregation
  if (!is.null(member_col) && member_col %in% names(dfDT) && ensemble_reduce != "none") {
    fun <- switch(ensemble_reduce, mean = mean, median = stats::median, min = min, max = max)
    dfDT <- dfDT[, .(value = fun(value, na.rm = TRUE)), by = .(DATE, lon, lat)]
  } else if (!is.null(member_col) && member_col %in% names(dfDT)) {
    dfDT[, (member_col) := NULL]  # drop to avoid duplicates downstream
  }

  # --- collapse any other extra dims to avoid duplicates
  keep_basic <- c("DATE","lon","lat","value", lon_col, lat_col, time_col, ref_col, period_col, member_col)
  keep_basic <- unique(keep_basic[!is.na(keep_basic)])
  extra <- setdiff(names(dfDT), keep_basic)
  extra <- setdiff(extra, c("lon","lat","DATE","value"))
  if (length(extra)) {
    dfDT <- dfDT[, .(value = mean(value, na.rm = TRUE)), by = .(DATE, lon, lat)]
  }

  # --- spatial reduce / layout
  if (spatial_reduce != "none") {
    fun <- switch(spatial_reduce, mean = mean, median = stats::median, min = min, max = max)
    dfDT <- dfDT[!is.na(DATE)][, .(value = fun(value, na.rm = TRUE)), by = .(DATE)]
  } else {
    if (cell_layout == "wide") {
      dfDT <- dfDT[!is.na(DATE)]
      # ordre stable des cellules
      grid <- unique(dfDT[, .(lon, lat)])
      data.table::setorder(grid, -lat, lon)
      grid[, cell_id := .I]
      dfDT <- grid[dfDT, on = .(lon, lat)]
      DTw <- data.table::dcast(
        dfDT, DATE ~ cell_id, value.var = "value",
        fun.aggregate = function(z) if (length(z)) z[1] else NA_real_
      )
      newn <- names(DTw)
      if (ncol(DTw) > 1) {
        newn[-1] <- paste0(cell_prefix, "_", newn[-1])
        data.table::setnames(DTw, names(DTw), newn)
      }
      dfDT <- DTw[]
    } else {
      dfDT <- dfDT[!is.na(DATE)][, .(DATE, lon, lat, value)]
    }
  }

  if (nrow(dfDT)) data.table::setorder(dfDT, DATE)
  as.data.frame(dfDT)
}
