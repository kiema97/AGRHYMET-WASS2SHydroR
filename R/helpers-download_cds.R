#' Internal Utilities for WASS2SHydroR
#'
#' A collection of internal helper functions used by \code{wass2s_download_cds()}.
#' These are not exported and may change without notice.
#'
#' @keywords internal
#' @name `wass2s_download_cds` helpers
NULL

#' Null-Coalescing Operator (internal)
#' @param x Any object.
#' @param y Fallback value if \code{x} is \code{NULL} or zero-length.
#' @return \code{x} if not \code{NULL}/empty, otherwise \code{y}.
#' @keywords internal
#' @noRd
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x

#' Zero-Pad to 2 Digits (internal)
#' @param x Integer-like vector or \code{NULL}.
#' @return Character vector with 2-digit zero-padding, or \code{NULL} if input is \code{NULL}.
#' @keywords internal
#' @noRd
pad2 <- function(x) if (is.null(x)) NULL else sprintf("%02d", as.integer(x))

#' Month Abbreviations (internal)
#' @keywords internal
#' @noRd
month_abb <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")

#' Replace {placeholders} in a Template (internal)
#' @param template Character template with \code{{placeholders}}.
#' @param values Named list of replacements.
#' @return Character string with placeholders replaced and minor cleanup of underscores.
#' @keywords internal
#' @noRd
tpl_replace <- function(template, values) {
  for (k in names(values)) {
    template <- gsub(paste0("\\{", k, "\\}"), values[[k]], template, perl = TRUE)
  }
  template <- gsub("__+", "_", template)
  template <- gsub("_+\\.", ".", template)
  template <- gsub("^_+|_+$", "", template)
  template
}

#' Parse "model_system.variable" (internal)
#'
#' Robustly parses \code{center_variables} entries of the form
#' \code{"meteo_france_9.TMAX"} where the model name may contain underscores.
#'
#' @param x Single character string \code{"model_system.variable"}.
#' @return A list with \code{model}, \code{system}, and \code{variable} (all lowercase for model/variable).
#' @keywords internal
#' @noRd
parse_center_variable <- function(x) {
  y <- tolower(x)
  m <- regexec("^([a-z0-9_]+)_([0-9]+)\\.([a-z0-9_]+)$", y)
  parts <- regmatches(y, m)[[1]]
  if (length(parts) != 4) {
    stop("Invalid 'center_variables' format: ", x,
         " - expected 'model_system.variable' (e.g., 'meteo_france_9.TMAX')", call. = FALSE)
  }
  list(model = parts[2], system = parts[3], variable = parts[4])
}

#' Map to Short Variable Code for Filenames (internal)
#' @param v Character scalar; API variable name or short code.
#' @return Uppercase short code (e.g., \code{"TMAX"}). Falls back to a sanitized abbreviation.
#' @keywords internal
#' @noRd
var_short <- function(v) {
  if (is.null(v) || length(v) == 0) return("")
  v <- tolower(v[1])
  VAR_SHORT_FROM_API[[v]] %||% {
    if (!grepl("[[:space:]]", v) && !grepl("[^a-z0-9_]", v)) {
      toupper(gsub("[^a-z0-9_]", "", v))
    } else {
      ab <- toupper(gsub("[^a-z0-9]", "", v))
      if (nchar(ab) > 12) substr(ab, 1, 12) else ab
    }
  }
}

#' Map Short Code to API Variable Name (internal)
#' @param v Character scalar; may be a short code (e.g., \code{"TMAX"}) or already an API name.
#' @return API variable name if a mapping exists; otherwise returns \code{v} unchanged.
#' @keywords internal
#' @noRd
var_api_name <- function(v) {
  if (is.null(v) || length(v) == 0) return("")
  v1 <- v[1]
  VAR_API_FROM_SHORT[[toupper(v1)]] %||% v1
}

#R/constants.R

#' Known Model Names (Lowercase)
#'
#' A vector of known model names available on CDS/ECMWF. The function
#' \code{wass2s_download_cds()} will warn if a model not in this list is used,
#' but will not stop to allow for evolving model catalogs.
#'
#' @format A character vector.
#' @keywords internal
VALID_MODELS <- c("ecmwf","meteo_france","ukmo","dwd","cmcc","ncep","eccc","bom")

#' Short API Variable Name Mappings
#'
#' Two named character vectors used to convert between short variable codes
#' (e.g., \code{"TMAX"}) and the corresponding API names expected by CDS/ECMWF
#' (e.g., \code{"maximum_2m_temperature_in_the_last_24_hours"}). These are
#' used by \code{wass2s_download_cds()} for filename generation and request
#' normalization.
#'
#' @details
#' \itemize{
#'   \item \code{VAR_API_FROM_SHORT}: short code \eqn{\rightarrow} API name.
#'   \item \code{VAR_SHORT_FROM_API}: API name \eqn{\rightarrow} short code.
#' }
#'
#' Extend or modify these mappings as needed for additional variables.
#'
#' @format Named character vectors.
#' @keywords internal
VAR_API_FROM_SHORT <- c(
  T2M  = "2m_temperature",
  TMAX = "maximum_2m_temperature_in_the_last_24_hours",
  TMIN = "minimum_2m_temperature_in_the_last_24_hours",
  PRCP = "total_precipitation",
  PET  = "potential_evaporation",
  EVAP = "evaporation",
  SST  = "sea_surface_temperature",
  MSLP = "mean_sea_level_pressure",
  U10  = "10m_u_component_of_wind",
  V10  = "10m_v_component_of_wind",
  SP   = "surface_pressure",
  TCC  = "total_cloud_cover",
  LCC  = "low_cloud_cover",
  MCC  = "medium_cloud_cover",
  HCC  = "high_cloud_cover"
)

VAR_SHORT_FROM_API <- c(
  "2m_temperature"                                   = "T2M",
  "maximum_2m_temperature_in_the_last_24_hours"      = "TMAX",
  "minimum_2m_temperature_in_the_last_24_hours"      = "TMIN",
  "total_precipitation"                              = "PRCP",
  "potential_evaporation"                            = "PET",
  "evaporation"                                      = "EVAP",
  "sea_surface_temperature"                          = "SST",
  "mean_sea_level_pressure"                          = "MSLP",
  "u_component_of_wind"                              = "U10",
  "v_component_of_wind"                              = "V10",
  "10m_u_component_of_wind"                          = "U10",
  "10m_v_component_of_wind"                          = "V10",
  "surface_pressure"                                 = "SP",
  "total_cloud_cover"                                = "TCC",
  "low_cloud_cover"                                  = "LCC",
  "medium_cloud_cover"                               = "MCC",
  "high_cloud_cover"                                 = "HCC"
)
