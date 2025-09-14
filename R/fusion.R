#' Weighted fusion of predictions by year
#'
#' @param preds_long Tibble with columns YYYY, pred, w (and optionally product/model).
#' @return Tibble (YYYY, pred_fused).
#' @keywords internal
fuse_topk <- function(preds_long) {
  stopifnot(all(c("YYYY","pred","w") %in% names(preds_long)))
  preds_long |>
    dplyr::group_by(YYYY) |>
    dplyr::summarise(
      pred_fused = mean(w * pred, na.rm = TRUE) ,
      .groups = "drop"
    )
}


#' Full-join a list of (YYYY, pred) tibbles with renaming
#'
#' @param lst Named list of tibbles. Each must have columns YYYY, pred.
#' @return Tibble with YYYY and one column per element name.
#' @keywords internal
safe_full_join_preds <- function(lst) {
  lst <- purrr::compact(lst)
  if (length(lst) == 0L) return(tibble::tibble())
  lst2 <- purrr::imap(lst, ~ dplyr::rename(.x, !!.y := pred))
  Reduce(function(a, b) dplyr::full_join(a, b, by = "YYYY"), lst2)
}

#' Extract (YYYY, Q) for a basin from the first available product
#'
#' @param data_by_product Named list of data frames (per product).
#' @param basin_id Basin identifier value.
#' @param basin_col Name of the basin ID column.
#' @return Tibble (YYYY, Q) or empty tibble if not found.
#' @keywords internal
get_any_Q <- function(data_by_product, basin_id, basin_col = "HYBAS_ID") {
  for (p in names(data_by_product)) {
    df <- data_by_product[[p]]
    if (all(c(basin_col, "YYYY", "Q") %in% names(df))) {
      tmp <- dplyr::filter(df, .data[[basin_col]] == basin_id) |>
        dplyr::select(YYYY, Q)
      if (nrow(tmp) > 0) return(tmp)
    }
  }
  tibble::tibble()
}
