#' Filter usable predictors
#'
#' Removes columns with zero variance or missing values
#'
#' @param df Data frame containing the predictors
#' @param predictors Character vector of predictor names
#' @return Character vector of usable predictors
#' @keywords internal
usable_predictors <- function(df, predictors) {
  # Check if predictors exist in the data frame
  predictors <- intersect(predictors, names(df))

  if (length(predictors) == 0) {
    return(character(0))
  }

  # Filter predictors with zero variance or too many missing values
  keepers <- vapply(predictors, function(x) {
    col_data <- df[[x]]

    # Check for sufficient non-NA values
    if (sum(!is.na(col_data)) < 3) {
      return(FALSE)
    }

    # Check for finite variance
    sx <- stats::sd(col_data, na.rm = TRUE)
    is.finite(sx) && sx > 0
  }, logical(1))

  predictors[keepers]
}


#' Select predictor columns by regex
#'
#' @param df Data frame.
#' @param pattern Regex pattern for predictors (default "^pt_").
#' @param exclude Columns to always exclude (default: c("YYYY","Q")).
#' @return Character vector of predictor names present in df.
#' @keywords internal
select_predictors <- function(df, pattern = "^pt_", exclude = c("YYYY","Q")) {
  nms <- setdiff(names(df), exclude)
  nms[stringr::str_detect(nms, pattern)]
}
