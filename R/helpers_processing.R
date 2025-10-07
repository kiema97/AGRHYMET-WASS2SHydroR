#' @keywords internal
#' @noRd
.parse_metadata_from_filename <- function(fname) {
  # Expect e.g.: meteo_france9_TMAX_Apr01_1993_2016_24-5160_1993.nc
  # modelsys = model + system (system numeric at end of token before first '_')
  # Pattern parts:
  #   ^([a-z0-9_]+?)([0-9]+)_(.+?)_([A-Za-z]{3}[0-9]{2})_([0-9]{4})_([0-9]{4})_([0-9]+-[0-9]+)(?:_([0-9]{4}))?\\.nc$
  # If the template differs, we fall back to a more permissive parse and warn.
  low <- tolower(fname)
  re  <- "^([a-z0-9_]+?)([0-9]+)_([a-z0-9_]+)_([a-z]{3}[0-9]{2})_([0-9]{4})_([0-9]{4})_([0-9]+-[0-9]+)(?:_([0-9]{4}))?\\.nc$"
  m   <- regexec(re, low)
  g   <- regmatches(low, m)[[1]]

  if (length(g) >= 8) {
    model       <- g[2]
    system      <- g[3]
    var_short   <- toupper(g[4]) # will be like "tmax" here, but we upper it
    init_token  <- g[5]          # e.g. apr01
    per_start   <- g[6]
    per_end     <- g[7]
    lead_rng    <- g[8]
    year_file   <- if (length(g) >= 9) g[9] else NA_character_
  } else {
    # fallback: parse via positions
    warning("Filename did not match strict pattern: ", fname, " - using relaxed parsing.")
    split <- strsplit(sub("\\.nc$","", low), "_")[[1]]
    # modelsys, var, init_token, periodStart, periodEnd, lead, [year]
    model_sys <- split[1]
    var_short <- toupper(split[2] %||% "")
    init_token<- split[3] %||% "jan01"
    # extract trailing digits for system
    mm <- regexec("^([a-z0-9_]+?)([0-9]+)$", model_sys)
    mg <- regmatches(model_sys, mm)[[1]]
    model <- if (length(mg) == 3) mg[2] else model_sys
    system<- if (length(mg) == 3) mg[3] else "0"
    per_start <- split[4] %||% "1900"
    per_end   <- split[5] %||% "2100"
    lead_rng  <- split[6] %||% "0-0"
    year_file <- split[7] %||% NA_character_
  }

  # derive variable API name from short mapping (fallback to short)
  var_api <- VAR_API_FROM_SHORT[[toupper(var_short)]] %||% var_short

  # year for this file (chunk), fallback to period start if missing
  year <- if (!is.na(year_file)) year_file else per_start

  list(
    model = model,
    system = system,
    variable = tolower(var_short),
    variable_short = toupper(var_short),
    variable_api = var_api,
    init_token = init_token,
    period_start = per_start,
    period_end = per_end,
    lead_range = lead_rng,
    year = year
  )
}

#' @keywords internal
#' @noRd
.bbox_to_sf <- function(bbox, stars_obj) {
  # bbox = c(N, W, S, E) -> convert to xmin/xmax/ymin/ymax in current CRS order (lon/lat)
  stopifnot(length(bbox) == 4)
  N <- bbox[1]; W <- bbox[2]; S <- bbox[3]; E <- bbox[4]
  # stars uses sf geometries; build an sf polygon in same crs
  crs <- sf::st_crs(stars_obj)
  poly <- sf::st_polygon(list(rbind(
    c(W, S), c(E, S), c(E, N), c(W, N), c(W, S)
  )))
  sf::st_sfc(poly, crs = crs)
}

#' @keywords internal
#' @noRd
.guess_time_dim <- function(dims) {
  nm <- names(dims)
  cand <- nm[grepl("time", nm, ignore.case = TRUE)]
  cand[1] %||% NULL
}

#' @keywords internal
#' @noRd
.guess_lead_dim <- function(dims) {
  nm <- names(dims)
  cand <- nm[grepl("lead", nm, ignore.case = TRUE) | grepl("forecast_hour|fhour", nm, ignore.case = TRUE)]
  cand[1] %||% NULL
}

#' @keywords internal
#' @noRd
.init_date_from_token <- function(token, year) {
  # token like "Apr01" (case-insensitive); convert to YYYY-MM-DD
  token <- tolower(token)
  mon <- substr(token, 1, 3)
  day <- substr(token, 4, 5)
  months <- tolower(c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))
  mnum <- match(mon, substr(months,1,3))
  if (is.na(mnum)) mnum <- 1
  as.Date(sprintf("%s-%02d-%02d", year, mnum, as.integer(day)))
}

#' @keywords internal
#' @noRd
.needs_kelvin_to_c <- function(var_short) {
  # basic heuristic: temperature variables as Kelvin -> convert to Celsius
  toupper(var_short) %in% c("T2M","TMAX","TMIN","SST")
}

#' Convert a stars object to data.table (values + coords + dims)
#' @keywords internal
#' @noRd
.stars_to_dt <- function(x, time_dim = NULL, lead_dim = NULL, convert_kelvin = FALSE) {
  # stars -> long table
  # Extract array and dimension values
  dims <- stars::st_dimensions(x)
  # get variable name (first attribute)
  vname <- names(x)[1]
  arr <- as.array(x[[1]])

  # coords (assuming first two dims are x/y or lon/lat in most CF files)
  # safer: find spatial dims by checking point/regular grid
  xdim <- which(vapply(dims, function(d) isTRUE(d$point) || !is.null(d$from), logical(1)))
  # fallback: assume 2 spatial dims
  if (length(xdim) < 2) xdim <- 1:2
  lon <- as.numeric(dims[[xdim[1]]]$values %||% seq_len(dim(arr)[xdim[1]]))
  lat <- as.numeric(dims[[xdim[2]]]$values %||% seq_len(dim(arr)[xdim[2]]))

  # time values if available
  tvals <- NULL
  if (!is.null(time_dim) && time_dim %in% names(dims)) {
    tvals <- dims[[time_dim]]$values
    if (inherits(tvals, "PCICt") || inherits(tvals, "POSIXt")) {
      tvals <- as.POSIXct(tvals)
    } else if (is.numeric(tvals)) {
      # try interpret as days since origin
      origin <- attr(dims[[time_dim]], "origin") %||% "1970-01-01"
      tvals <- as.POSIXct(tvals, origin = origin, tz = "UTC")
    }
  }

  # leadtime hours if available
  lvals <- NULL
  if (!is.null(lead_dim) && lead_dim %in% names(dims)) {
    lvals <- dims[[lead_dim]]$values
    # heuristics: if units suggest hours, keep as is; else try numeric
    if (!is.numeric(lvals)) {
      lvals <- suppressWarnings(as.numeric(lvals))
    }
  }

  # reshape to long
  DT <- data.table::as.data.table(as.table(arr), keep.rownames = FALSE)
  # columns are dim1, dim2, ..., value
  setDT(DT)
  setnames(DT, names(DT)[ncol(DT)], "value")

  # attach coordinates
  # we assume order dims: [xdim1, xdim2, (time?), (lead?), ...]
  # safest: map by index
  idx1 <- names(DT)[1]
  idx2 <- names(DT)[2]
  DT[, lon := lon[get(idx1)]]
  DT[, lat := lat[get(idx2)]]
  DT[, c(idx1, idx2) := NULL]

  # attach time/lead if present by position (3rd/4th dims)
  if (!is.null(tvals)) {
    idx_t <- names(DT)[1]
    DT[, time := tvals[get(idx_t)]]
    DT[, (idx_t) := NULL]
  }
  if (!is.null(lvals)) {
    idx_l <- names(DT)[1]
    DT[, leadtime_hour := lvals[get(idx_l)]]
    DT[, (idx_l) := NULL]
  }

  # unit conversion (Kelvin -> Celsius) if requested
  if (isTRUE(convert_kelvin)) {
    # crude detection: many CF files store K; if value range > 100 likely K
    if (isTRUE(max(DT$value, na.rm = TRUE) > 150)) {
      DT[, value := value - 273.15]
    }
  }

  DT[]
}

#' @keywords internal
#' @noRd
.season_from_month <- function(m) {
  # DJF=1, MAM=2, JJA=3, SON=4
  ifelse(m %in% c(12,1,2), 1L,
         ifelse(m %in% 3:5, 2L,
                ifelse(m %in% 6:8, 3L, 4L)))
}

#' @keywords internal
#' @noRd
.season_midpoint <- function(year, season) {
  # approximate midpoints for plotting/aggregation
  # DJF -> Feb 1 of the corresponding DJF-year (use Jan for year change)
  zz <- vector("list", length(season))
  for (i in seq_along(season)) {
    s <- season[i]; y <- year[i]
    if (s == 1L) {
      # center on Jan for DJF
      zz[[i]] <- as.POSIXct(sprintf("%04d-01-15", y), tz="UTC")
    } else if (s == 2L) {
      zz[[i]] <- as.POSIXct(sprintf("%04d-04-15", y), tz="UTC")
    } else if (s == 3L) {
      zz[[i]] <- as.POSIXct(sprintf("%04d-07-15", y), tz="UTC")
    } else {
      zz[[i]] <- as.POSIXct(sprintf("%04d-10-15", y), tz="UTC")
    }
  }
  do.call(c, zz)
}
