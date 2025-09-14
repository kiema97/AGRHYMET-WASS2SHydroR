#' @keywords internal
safe_abort <- function(msg, cls = "wass2s_stat_error") {
  rlang::abort(message = msg, class = cls)
}


#' Internal helper to abort with a WASS2S-specific error class
#'
#' Wraps [rlang::abort()] to standardize error messages across the
#' WASS2S Hydro Statistical method.
#'
#' @param msg Error message.
#' @param cls Error class to attach (default "wass2s_stat_error").
#' @keywords internal
wass2s_abort <- function(msg, cls = "wass2s_stat_error") {
  rlang::abort(message = msg, class = cls)
}

