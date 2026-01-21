# ---- helpers ----
.pred_years_to_bounds <- function(prediction_years) {
  if (is.null(prediction_years)) return(NULL)

  if (length(prediction_years) != 2) {
    stop("prediction_years must be length 2.", call. = FALSE)
  }

  py <- as.numeric(prediction_years)

  # Case A: already YYYYMMDD (>= 10^7)
  if (all(py >= 1e7)) {
    start_bound <- as.numeric(py[1])
    end_bound   <- as.numeric(py[2])
    return(list(start = start_bound, end = end_bound))
  }

  # Case B: plain years (e.g., 2001, 2005)
  start_bound <- as.numeric(paste0(py[1], "0101"))
  end_bound   <- as.numeric(paste0(py[2], "1231"))
  list(start = start_bound, end = end_bound)
}
