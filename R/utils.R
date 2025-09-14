#' Null-coalescing operator
#'
#' Retourne `y` si `x` est `NULL`, sinon `x`.
#'
#' @name %||%
#' @usage x %||% y
#' @param x,y objets
#' @return x si non-NULL, sinon y
#' @noRd
#' @keywords internal
`%||%` <- function(x, y) if (is.null(x)) y else x


#' Pipe operator
#' @name %>%
#' @rdname pipe
#' @noRd
#' @keywords internal
#' @importFrom magrittr %>%
magrittr::`%>%`
