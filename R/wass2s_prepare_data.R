#' Prepare gridded hydro-climatic data from NetCDF or stars objects
#'
#' Prepare gridded hydro-climatic data for downstream analysis, modelling, or
#' forecasting workflows. The function reads a NetCDF file or a \code{stars}
#' object, reconstructs a robust \code{DATE} column, optionally filters the
#' spatial domain using a bounding box, handles ensemble members, and returns
#' the result in either long or wide tabular format.
#'
#' This function is designed for hydro-climatic datasets such as precipitation,
#' temperature, evapotranspiration, or forecast products distributed on regular
#' longitude/latitude grids.
#'
#' @param x A path to a NetCDF file or a \code{stars} object.
#' @param bbox Optional bounding box used to spatially subset the grid.
#'   It can be:
#'   \itemize{
#'     \item a named numeric vector with elements \code{xmin}, \code{ymin},
#'       \code{xmax}, \code{ymax};
#'     \item an unnamed numeric vector interpreted heuristically.
#'   }
#' @param spatial_reduce Character string specifying how to spatially aggregate
#'   grid cells. One of \code{"none"}, \code{"mean"}, \code{"median"},
#'   \code{"min"}, or \code{"max"}.
#' @param cell_layout Output layout when \code{spatial_reduce = "none"}.
#'   Either \code{"long"} or \code{"wide"}.
#' @param cell_prefix Prefix used for column names in wide format.
#' @param dim_lon Optional name of the longitude dimension/column.
#' @param dim_lat Optional name of the latitude dimension/column.
#' @param dim_time Optional name of the time dimension/column.
#' @param dim_ref_time Optional name of the forecast reference time
#'   dimension/column.
#' @param dim_period Optional name of the forecast lead time / period
#'   dimension/column.
#' @param dim_member Optional name of the ensemble member dimension/column.
#' @param ensemble_reduce Character string specifying how to aggregate ensemble
#'   members. One of \code{"mean"}, \code{"median"}, \code{"min"},
#'   \code{"max"}, or \code{"none"}.
#' @param keep_member Logical. If \code{TRUE} and
#'   \code{ensemble_reduce = "none"}, keep the ensemble member identifier in the
#'   output when available.
#' @param extra_dims_action How to handle unexpected extra dimensions after the
#'   main dimensions have been identified. One of:
#'   \itemize{
#'     \item \code{"warn_mean"}: collapse extra dimensions by mean and emit a warning;
#'     \item \code{"error"}: stop with an informative error;
#'     \item \code{"drop"}: drop extra dimensions before deduplication.
#'   }
#' @param tz Time zone used when constructing the \code{DATE} column.
#'   Defaults to \code{"UTC"}.
#' @param verbose Logical. If \code{TRUE}, informative messages and warnings are
#'   emitted during processing.
#'
#' @details
#' The function attempts to reconstruct the \code{DATE} column as robustly as
#' possible. It supports:
#' \itemize{
#'   \item direct date/time vectors stored as \code{Date} or \code{POSIXct};
#'   \item character date/time representations such as \code{"YYYY-MM-DD"} or
#'     \code{"YYYY-MM-DD HH:MM:SS"};
#'   \item numeric time vectors with CF-compliant units such as
#'     \code{"days since 1900-01-01"};
#'   \item forecast products using \code{forecast_reference_time} combined with
#'     \code{forecast_period}.
#' }
#'
#' Longitude values can be automatically aligned between \code{[-180, 180]} and
#' \code{[0, 360]} conventions when needed for spatial filtering.
#'
#' At present, curvilinear grids are not supported and trigger an explicit error.
#'
#' When \code{cell_layout = "wide"}, the returned object includes a
#' \code{"cell_map"} attribute describing the mapping between generated cell
#' columns and their corresponding longitude/latitude coordinates.
#'
#' @return
#' A \code{data.frame} containing at least a \code{DATE} column and one or more
#' value columns depending on the selected output mode:
#' \itemize{
#'   \item if \code{spatial_reduce != "none"}, the result contains one value per
#'     time step;
#'   \item if \code{cell_layout = "long"}, the result contains
#'     \code{DATE}, \code{lon}, \code{lat}, and \code{value};
#'   \item if \code{cell_layout = "wide"}, each grid cell becomes a separate
#'     column.
#' }
#'
#' @examples
#' \dontrun{
#' # Example 1: spatial mean over a bounding box
#' res <- wass2s_prepare_data(
#'   x = "precip.nc",
#'   bbox = c(xmin = -5, ymin = 10, xmax = 2, ymax = 15),
#'   spatial_reduce = "mean"
#' )
#'
#' # Example 2: long format without spatial aggregation
#' res <- wass2s_prepare_data(
#'   x = "temperature.nc",
#'   spatial_reduce = "none",
#'   cell_layout = "long"
#' )
#'
#' # Example 3: wide format
#' res <- wass2s_prepare_data(
#'   x = "forecast.nc",
#'   spatial_reduce = "none",
#'   cell_layout = "wide",
#'   cell_prefix = "cell"
#' )
#'
#' # Access the grid-to-column correspondence
#' attr(res, "cell_map")
#' }
#'
#' @export
#' @importFrom data.table as.data.table setDT setorder dcast
#' @importFrom data.table :=
#' @importFrom stats median
wass2s_prepare_data <- function(
    x,
    bbox = NULL,
    spatial_reduce = c("none", "mean", "median", "min", "max"),
    cell_layout = c("long", "wide"),
    cell_prefix = "val",
    dim_lon = NULL,
    dim_lat = NULL,
    dim_time = NULL,
    dim_ref_time = NULL,
    dim_period = NULL,
    dim_member = NULL,
    ensemble_reduce = c("mean", "median", "min", "max", "none"),
    keep_member = FALSE,
    extra_dims_action = c("warn_mean", "error", "drop"),
    tz = "UTC",
    verbose = TRUE
) {
  spatial_reduce <- match.arg(spatial_reduce)
  cell_layout <- match.arg(cell_layout)
  ensemble_reduce <- match.arg(ensemble_reduce)
  extra_dims_action <- match.arg(extra_dims_action)

  if (!requireNamespace("stars", quietly = TRUE)) {
    stop("Package 'stars' is required.")
  }
  if (!requireNamespace("ncdf4", quietly = TRUE)) {
    stop("Package 'ncdf4' is required.")
  }
  if (!requireNamespace("data.table", quietly = TRUE)) {
    stop("Package 'data.table' is required.")
  }

  has_units <- requireNamespace("units", quietly = TRUE)


  # ---------------------------------------------------------------------------
  # Read stars object
  # ---------------------------------------------------------------------------

  is_stars_input <- inherits(x, "stars")

  obj <- if (is_stars_input) {
    x
  } else {
    if (!file.exists(x)) stop("File not found: ", x)
    if (verbose) message("[read] ", x)
    suppressWarnings(stars::read_stars(x, proxy = FALSE, quiet = TRUE))
  }

  dims_obj <- stars::st_dimensions(obj)
  dim_names <- names(dims_obj)

  guess_lon <- intersect(c("x", "lon", "longitude", "long"), tolower(dim_names))
  guess_lat <- intersect(c("y", "lat", "latitude"), tolower(dim_names))

  lon_dim_name <- dim_lon %||% pick_name(guess_lon, dim_names)
  lat_dim_name <- dim_lat %||% pick_name(guess_lat, dim_names)

  if (is.null(lon_dim_name) || is.null(lat_dim_name)) {
    stop(
      "Could not resolve longitude/latitude dimensions in stars object. Found: ",
      paste(dim_names, collapse = ", ")
    )
  }

  # If input is a file, try to inject lon/lat coordinate values from ncdf4 helper
  if (!is_stars_input) {
    nc <- ncdf4_guess_lonlat(x)

    if (isTRUE(nc$curvilinear)) {
      stop("Curvilinear grids are not yet supported by wass2s_prepare_data().")
    }

    dims_obj <- stars::st_dimensions(obj)
    dims_obj[[lon_dim_name]]$values <- as.numeric(nc$lon)
    dims_obj[[lat_dim_name]]$values <- as.numeric(nc$lat)
    stars::st_dimensions(obj) <- dims_obj
  }

  # ---------------------------------------------------------------------------
  # stars -> data.frame
  # ---------------------------------------------------------------------------

  df <- as.data.frame(obj)
  if (!nrow(df)) {
    return(data.frame(DATE = as.POSIXct(character(), tz = tz)))
  }

  cn <- names(df)

  guess_time <- intersect(c("valid_time", "time", "t", "date", "datetime"), tolower(cn))
  guess_ref <- intersect(c("forecast_reference_time", "reftime", "ref_time", "reference_time"), tolower(cn))
  guess_period <- intersect(c("forecast_period", "step", "lead", "leadtime"), tolower(cn))
  guess_member <- intersect(c("number", "member", "ensemble", "realization", "realisation"), tolower(cn))

  time_col <- dim_time %||% pick_name(guess_time, cn)
  ref_col <- dim_ref_time %||% pick_name(guess_ref, cn)
  period_col <- dim_period %||% pick_name(guess_period, cn)
  member_col <- dim_member %||% pick_name(guess_member, cn)

  dims_known <- unique(c(lon_dim_name, lat_dim_name, time_col, ref_col, period_col, member_col))
  dims_known <- dims_known[!is.na(dims_known) & !is.null(dims_known)]

  value_candidates <- setdiff(cn, dims_known)

  if (length(value_candidates) == 0) {
    stop("Could not detect a value column after removing known dimensions.")
  }

  if (length(value_candidates) > 1) {
    # Prefer last column as stars often stores attribute/value last, but warn
    if (verbose) {
      message(
        "[warn] Multiple candidate value columns detected: ",
        paste(value_candidates, collapse = ", "),
        ". Using the last one: ", tail(value_candidates, 1)
      )
    }
  }

  val_col <- tail(value_candidates, 1L)

  # ---------------------------------------------------------------------------
  # data.table conversion
  # ---------------------------------------------------------------------------

  DT <- data.table::as.data.table(df)

  # Build DATE
  DT[, DATE := build_date_column(
    DT = DT,
    time_col = time_col,
    ref_col = ref_col,
    period_col = period_col,
    tz = tz
  )]

  if (all(is.na(DT$DATE)) && verbose) {
    warning(
      "Could not reconstruct DATE from available time information. ",
      "All DATE values are NA."
    )
  }

  # Coordinates
  DT[, lon := as.numeric(get(lon_dim_name))]
  DT[, lat := as.numeric(get(lat_dim_name))]

  # Value
  DT[, value := drop_units_num(get(val_col))]

  # Remove raw source value column only if different
  if (val_col %in% names(DT)) {
    DT[, (val_col) := NULL]
  }

  # Normalize longitudes if bbox suggests negative convention
  if (!is.null(bbox) && !all(is.na(DT$lon))) {
    bb_num <- as.numeric(bbox)
    if (length(bb_num) == 4 && max(DT$lon, na.rm = TRUE) > 180 && any(bb_num < 0, na.rm = TRUE)) {
      DT[, lon := ifelse(lon > 180, lon - 360, lon)]
    }
  }

  # Spatial filter
  if (!is.null(bbox)) {
    DT <- bbox_filter_or_fallback(
      DT = DT,
      bbox = bbox,
      lon_col = "lon",
      lat_col = "lat",
      fallback = "contains_center",
      auto_align_lon = TRUE,
      tol_frac = 0.25
    )
  }

  # Ensemble handling
  if (!is.null(member_col) && member_col %in% names(DT)) {
    if (ensemble_reduce != "none") {
      fun_ens <- switch(
        ensemble_reduce,
        mean = mean,
        median = stats::median,
        min = min,
        max = max
      )
      DT <- DT[, .(value = fun_ens(value, na.rm = TRUE)), by = .(DATE, lon, lat)]
    } else {
      if (!keep_member) {
        if (verbose) {
          message("[info] ensemble_reduce = 'none' and keep_member = FALSE: member column dropped.")
        }
        DT[, (member_col) := NULL]
      }
    }
  }

  # Extra dimensions
  base_keep <- unique(c(
    "DATE", "lon", "lat", "value",
    lon_dim_name, lat_dim_name, time_col, ref_col, period_col, member_col
  ))
  base_keep <- base_keep[!is.na(base_keep)]

  extra_cols <- setdiff(names(DT), base_keep)
  extra_cols <- setdiff(extra_cols, c("DATE", "lon", "lat", "value"))

  if (length(extra_cols) > 0) {
    if (extra_dims_action == "error") {
      stop(
        "Unexpected extra dimensions/columns found: ",
        paste(extra_cols, collapse = ", "),
        ". Please specify how to handle them."
      )
    }

    if (extra_dims_action == "warn_mean") {
      if (verbose) {
        message(
          "[warn] Extra dimensions collapsed by mean: ",
          paste(extra_cols, collapse = ", ")
        )
      }
      DT <- DT[, .(value = mean(value, na.rm = TRUE)), by = .(DATE, lon, lat)]
    }

    if (extra_dims_action == "drop") {
      if (verbose) {
        message(
          "[warn] Extra dimensions dropped before deduplication: ",
          paste(extra_cols, collapse = ", ")
        )
      }
      keep_now <- intersect(names(DT), c("DATE", "lon", "lat", "value"))
      DT <- DT[, keep_now, with = FALSE]
      DT <- DT[, .(value = mean(value, na.rm = TRUE)), by = .(DATE, lon, lat)]
    }
  }

  # Spatial reduction or layout
  if (spatial_reduce != "none") {
    fun_spatial <- switch(
      spatial_reduce,
      mean = mean,
      median = stats::median,
      min = min,
      max = max
    )
    DT <- DT[!is.na(DATE), .(value = fun_spatial(value, na.rm = TRUE)), by = .(DATE)]
  } else {
    if (cell_layout == "wide") {
      DT <- DT[!is.na(DATE)]

      grid <- unique(DT[, .(lon, lat)])
      data.table::setorder(grid, -lat, lon)
      grid[, cell_id := .I]

      DT <- grid[DT, on = .(lon, lat)]

      DTw <- data.table::dcast(
        DT,
        DATE ~ cell_id,
        value.var = "value",
        fun.aggregate = function(z) if (length(z)) z[1] else NA_real_
      )

      if (ncol(DTw) > 1) {
        old_names <- names(DTw)
        new_names <- old_names
        new_names[-1] <- paste0(cell_prefix, "_", old_names[-1])
        data.table::setnames(DTw, old = old_names, new = new_names)
      }

      out <- DTw[]

      # Attach cell map
      attr(out, "cell_map") <- grid[]
      if (nrow(out)) data.table::setorder(out, DATE)
      return(as.data.frame(out))
    } else {
      keep_cols <- c("DATE", "lon", "lat", "value")
      if (!is.null(member_col) && member_col %in% names(DT) && keep_member) {
        keep_cols <- c(keep_cols, member_col)
      }
      DT <- DT[!is.na(DATE), keep_cols, with = FALSE]
    }
  }

  if (nrow(DT)) {
    data.table::setorder(DT, DATE)
  }

  as.data.frame(DT)
}




# wass2s_prepare_data_old2 <- function(
#     x,
#     bbox = NULL,
#     spatial_reduce = c("none","mean","median","min","max"),
#     cell_layout   = c("long","wide"),
#     cell_prefix   = "val",
#     dim_lon = NULL,
#     dim_lat = NULL,
#     dim_time = NULL,
#     dim_ref_time = NULL,
#     dim_period = NULL,
#     dim_member = NULL,
#     ensemble_reduce = c("mean","median","min","max","none"),
#     verbose = TRUE
# ) {
#   spatial_reduce  <- match.arg(spatial_reduce)
#   cell_layout     <- match.arg(cell_layout)
#   ensemble_reduce <- match.arg(ensemble_reduce)
#
#   if (!requireNamespace("stars", quietly = TRUE))
#     stop("Package 'stars' is required.")
#   if (!requireNamespace("ncdf4", quietly = TRUE))
#     stop("Package 'ncdf4' is required.")
#   if (!requireNamespace("data.table", quietly = TRUE))
#     stop("Package 'data.table' is required.")
#   # units est souvent installé avec stars; on l'utilise si dispo
#   has_units <- requireNamespace("units", quietly = TRUE)
#
#   # --- read stars object
#
#   obj <- if (inherits(x, "stars")) {
#     x
#   } else {
#     if (!file.exists(x)) stop("File not found: ", x)
#     if (verbose) message("[read] ", x)
#     suppressWarnings(stars::read_stars(x, proxy = FALSE, quiet = TRUE))
#   }
#
#
#
#   # detect dimension columns present in df
#   cn <- names(stars::st_dimensions(obj))
#   # guess lon/lat cols from df colnames, then allow overrides
#   guess_lon <- intersect(c("x","lon","longitude","long"), tolower(cn))
#   guess_lat <- intersect(c("y","lat","latitude"), tolower(cn))
#
#   nc <- ncdf4_guess_lonlat(x)
#   x_vals <- nc$lon
#   y_vals <- nc$lat
#   # map back to original case in df
#   pick_name <- function(targets, cn) {
#     if (!length(targets)) return(NULL)
#     t <- targets[1]
#     cn[match(t, tolower(cn))]
#   }
#   lon_col <- dim_lon %||% pick_name(guess_lon, cn)
#   lat_col <- dim_lat %||% pick_name(guess_lat, cn)
#   stars::st_dimensions(obj)[[lon_col]]$values <- x_vals
#   stars::st_dimensions(obj)[[lat_col]]$values <- y_vals
#
#   # --- stars -> data.frame (long)
#   df <- as.data.frame(obj)  # colonnes: dims + variable
#   if (!nrow(df)) return(data.frame(DATE = as.POSIXct(NA))[0, ])
#   cn <- names(df)
#
#   if (is.null(lon_col) || is.null(lat_col))
#     stop("Could not resolve lon/lat columns in data.frame. Found: ", paste(cn, collapse = ", "))
#
#   # possible time columns in df
#   guess_time   <- intersect(c("valid_time","time","t"), tolower(cn))
#   time_col     <- dim_time %||% pick_name(guess_time, cn)
#   ref_col      <- dim_ref_time %||% pick_name(intersect(c("forecast_reference_time","reftime","ref_time"), tolower(cn)), cn)
#   period_col   <- dim_period   %||% pick_name(intersect(c("forecast_period","step","lead","leadtime"), tolower(cn)), cn)
#   member_col   <- dim_member   %||% pick_name(intersect(c("number","member","ensemble","realization","realisation"), tolower(cn)), cn)
#
#   # variable (last column convention from stars::as.data.frame)
#   val_col <- tail(cn, 1L)
#   if (val_col %in% c(lon_col, lat_col, time_col, ref_col, period_col, member_col)) {
#     # extremely rare edge case: if last column is not the variable
#     # choose the first column that is not a known dim
#     dims_known <- c(lon_col, lat_col, time_col, ref_col, period_col, member_col)
#     val_col <- setdiff(cn, dims_known)[1]
#   }
#
#   # --- helpers for units
#   deparse_unit_safe <- function(v) {
#     if (has_units && inherits(v, "units")) {
#       return(units::deparse_unit(v))
#     }
#     u <- attr(v, "units", exact = TRUE)
#     if (is.null(u)) return(NA_character_)
#     as.character(u)
#   }
#   drop_units_num <- function(v) {
#     if (has_units && inherits(v, "units")) as.numeric(units::drop_units(v)) else as.numeric(v)
#   }
#   parse_since_origin <- function(unit_str) {
#     # returns list(mult = seconds per unit, origin = POSIXct) or NULL
#     if (is.na(unit_str)) return(NULL)
#     us <- tolower(unit_str)
#     # try "xxx since YYYY-mm-dd HH:MM:SS"
#     m <- regexec("^(sec|secs|second|seconds|min|mins|minute|minutes|hour|hours|h|day|days)\\s+since\\s+(.+)$", us)
#     g <- regmatches(us, m)[[1]]
#     if (length(g) != 3) return(NULL)
#     unit <- g[2]; origin <- g[3]
#     origin_posix <- suppressWarnings(as.POSIXct(origin, tz = "UTC"))
#     if (is.na(origin_posix)) return(NULL)
#     mult <- switch(unit,
#                    sec = 1, secs = 1, second = 1, seconds = 1,
#                    min = 60, mins = 60, minute = 60, minutes = 60,
#                    hour = 3600, hours = 3600, h = 3600,
#                    day = 86400, days = 86400,
#                    1)
#     list(mult = mult, origin = origin_posix)
#   }
#
#   # --- build DATE
#   dfDT <- data.table::as.data.table(df)
#
#   if (!is.null(time_col) && time_col %in% names(dfDT)) {
#     # time present directly
#     if (inherits(dfDT[[time_col]], "POSIXt")) {
#       dfDT[, DATE := as.POSIXct(get(time_col), tz = "UTC")]
#     } else {
#       ustr <- deparse_unit_safe(dfDT[[time_col]])
#       info <- parse_since_origin(ustr)
#       if (!is.null(info)) {
#         dfDT[, DATE := info$origin + drop_units_num(get(time_col)) * info$mult]
#       } else {
#         # fallback: treat as epoch seconds
#         dfDT[, DATE := as.POSIXct(drop_units_num(get(time_col)), origin = "1970-01-01", tz = "UTC")]
#       }
#     }
#   } else if (!is.null(ref_col) && !is.null(period_col) &&
#              ref_col %in% names(dfDT) && period_col %in% names(dfDT)) {
#     # ref + period → DATE
#     # ref
#     ref_ustr <- deparse_unit_safe(dfDT[[ref_col]])
#     ref_info <- parse_since_origin(ref_ustr)
#     if (is.null(ref_info)) {
#       # if ref is raw epoch seconds
#       ref_origin <- as.POSIXct("1970-01-01", tz = "UTC")
#       ref_secs   <- drop_units_num(dfDT[[ref_col]])
#       ref_abs    <- ref_origin + ref_secs
#     } else {
#       ref_abs <- ref_info$origin + drop_units_num(dfDT[[ref_col]]) * ref_info$mult
#     }
#     # period
#     per_ustr <- deparse_unit_safe(dfDT[[period_col]])
#     per_secs <- {
#       us <- tolower(per_ustr)
#       v  <- drop_units_num(dfDT[[period_col]])
#       if (grepl("\\bhour|\\bh\\b", us)) v * 3600
#       else if (grepl("day", us)) v * 86400
#       else v  # assume seconds if unknown
#     }
#     dfDT[, DATE := as.POSIXct(ref_abs, tz = "UTC") + per_secs]
#   } else {
#     dfDT[, DATE := as.POSIXct(NA)]
#     warning("No time column found and no (forecast_reference_time + forecast_period); DATE is NA.")
#   }
#
#   # --- numeric lon/lat, handle 0..360 → -180..180 when bbox negative
#   dfDT[, lon := as.numeric(get(lon_col))]
#   dfDT[, lat := as.numeric(get(lat_col))]
#   if (!is.null(bbox)) {
#     W <- bbox[2]; E <- bbox[4]
#     if (max(dfDT$lon, na.rm = TRUE) > 180 && (W < 0 || E < 0)) {
#       dfDT[, lon := ifelse(lon > 180, lon - 360, lon)]
#     }
#   }
#
#   # --- value numeric
#   if (val_col %in% names(dfDT)) {
#     dfDT[, value := drop_units_num(get(val_col))]
#     dfDT[, (val_col) := NULL]
#   } else {
#     stop("Could not find variable/value column in data.frame.")
#   }
#
#   # # --- bbox filter (via lon/lat)
#   # if (!is.null(bbox)) {
#   #   stopifnot(length(bbox) == 4)
#   #   N <- bbox[1]; W <- bbox[2]; S <- bbox[3]; E <- bbox[4]
#   #   dfDT <- dfDT[lon >= W & lon <= E & lat >= S & lat <= N]
#   #   if (!nrow(dfDT)) stop("Bbox does not intersect the grid (after normalization).")
#   # }
#
#   if (!is.null(bbox)) {
#     names(bbox) <- c("ymax","xmin", "ymin", "xmax")
#     dfDT <- bbox_filter_or_fallback(
#       DT = dfDT,
#       bbox = bbox,
#       lon_col = "lon",
#       lat_col = "lat",
#       fallback = "contains_center",   # ou "nearest_center" / "error"
#       auto_align_lon = TRUE,
#       tol_frac = 0.25
#     )
#   }
#
#
#   # --- ensemble aggregation
#   if (!is.null(member_col) && member_col %in% names(dfDT) && ensemble_reduce != "none") {
#     fun <- switch(ensemble_reduce, mean = mean, median = stats::median, min = min, max = max)
#     dfDT <- dfDT[, .(value = fun(value, na.rm = TRUE)), by = .(DATE, lon, lat)]
#   } else if (!is.null(member_col) && member_col %in% names(dfDT)) {
#     dfDT[, (member_col) := NULL]  # drop to avoid duplicates downstream
#   }
#
#   # --- collapse any other extra dims to avoid duplicates
#   keep_basic <- c("DATE","lon","lat","value", lon_col, lat_col, time_col, ref_col, period_col, member_col)
#   keep_basic <- unique(keep_basic[!is.na(keep_basic)])
#   extra <- setdiff(names(dfDT), keep_basic)
#   extra <- setdiff(extra, c("lon","lat","DATE","value"))
#   if (length(extra)) {
#     dfDT <- dfDT[, .(value = mean(value, na.rm = TRUE)), by = .(DATE, lon, lat)]
#   }
#
#   # --- spatial reduce / layout
#   if (spatial_reduce != "none") {
#     fun <- switch(spatial_reduce, mean = mean, median = stats::median, min = min, max = max)
#     dfDT <- dfDT[!is.na(DATE)][, .(value = fun(value, na.rm = TRUE)), by = .(DATE)]
#   } else {
#     if (cell_layout == "wide") {
#       dfDT <- dfDT[!is.na(DATE)]
#       # ordre stable des cellules
#       grid <- unique(dfDT[, .(lon, lat)])
#       data.table::setorder(grid, -lat, lon)
#       grid[, cell_id := .I]
#       dfDT <- grid[dfDT, on = .(lon, lat)]
#       DTw <- data.table::dcast(
#         dfDT, DATE ~ cell_id, value.var = "value",
#         fun.aggregate = function(z) if (length(z)) z[1] else NA_real_
#       )
#       newn <- names(DTw)
#       if (ncol(DTw) > 1) {
#         newn[-1] <- paste0(cell_prefix, "_", newn[-1])
#         data.table::setnames(DTw, names(DTw), newn)
#       }
#       dfDT <- DTw[]
#     } else {
#       dfDT <- dfDT[!is.na(DATE)][, .(DATE, lon, lat, value)]
#     }
#   }
#
#   if (nrow(dfDT)) data.table::setorder(dfDT, DATE)
#   as.data.frame(dfDT)
# }


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
#' @return data.frame with DATE and values (shape depends on args).
#' @keywords internal
#' @noRd
#' @importFrom data.table as.data.table setDT setorder dcast
#' @importFrom data.table :=
#' @importFrom stats median
wass2s_prepare_data_old <- function(
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

  nc <- ncdf4::nc_open(x)
  x_vals <- ncdf4::ncvar_get(nc, "X")
  y_vals <- ncdf4::ncvar_get(nc, "Y")
  ncdf4::nc_close(nc)

  # detect dimension columns present in df
  cn <- names(stars::st_dimensions(obj))
  # guess lon/lat cols from df colnames, then allow overrides
  guess_lon <- intersect(c("x","lon","longitude","long"), tolower(cn))
  guess_lat <- intersect(c("y","lat","latitude"), tolower(cn))
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

  # --- bbox filter (via lon/lat)
  if (!is.null(bbox)) {
    stopifnot(length(bbox) == 4)
    N <- bbox[1]; W <- bbox[2]; S <- bbox[3]; E <- bbox[4]
    dfDT <- dfDT[lon >= W & lon <= E & lat >= S & lat <= N]
    if (!nrow(dfDT)) stop("Bbox does not intersect the grid (after normalization).")
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
