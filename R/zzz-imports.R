#' WASS2SHydroR: West-African & Sahel S2S Hydrology
#'
#' Tools for downloading, preparing, modeling and evaluating
#' subseasonal-to-seasonal (S2S) hydrological forecasts
#' in West Africa and the Sahel.
#' @docType package
#' @name WASS2SHydroR
#' @keywords internal
#' @import stringr
#' @import sf
#' @importFrom terra rast vect extract
#' @importFrom lubridate ymd year month as_date
#' @importFrom tidyr pivot_longer pivot_wider drop_na replace_na
#' @importFrom dplyr arrange select mutate filter group_by summarise left_join inner_join
#' @importFrom purrr map map_dfr imap imap_dfr compact set_names reduce keep map_dbl map_chr
#' @importFrom tibble tibble as_tibble
#' @importFrom ecmwfr wf_request wf_transfer wf_set_key
#' @importFrom ggplot2 element_blank element_rect element_text margin
#' @importFrom tune tune
#' @importFrom tibble tibble as_tibble
#' @importFrom stringr str_detect str_replace str_subset
#' @importFrom rlang .data .env !!
#' @importFrom grid unit
#' @importFrom ggspatial north_arrow_fancy_orienteering
#' @importFrom stats rnorm setNames predict
#' @importFrom glue glue
"_PACKAGE"
