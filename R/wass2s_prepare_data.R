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
#' @param time_agg "year","month","none". Default "year".
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

  # --- stars -> data.frame (long)
  df <- as.data.frame(obj)  # colonnes: dims + variable
  if (!nrow(df)) return(data.frame(DATE = as.POSIXct(NA))[0, ])

  # detect dimension columns present in df
  cn <- names(df)
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
