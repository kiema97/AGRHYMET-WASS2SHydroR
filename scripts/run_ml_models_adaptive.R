# ==============================================================================
# STEP 3: RUN ML MODELS
# ==============================================================================
# This version supports two predictor-data structures:
# 1) Basin-first structure, used for PRCP and the former SST object:
#      data_by_products[[basin]][[product]] -> data.frame(HYBAS_ID, YYYY, Q, predictors)
# 2) Compact common-predictor structure, used for the reduced SST object:
#      data_by_products$predictors[[product]] -> data.frame(YYYY, predictors)
#      data_by_products$q -> data.frame(HYBAS_ID, YYYY, Q)
# ==============================================================================

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0L || all(is.na(x))) y else x
}

is_compact_predictor_data <- function(x) {
  is.list(x) &&
    all(c("predictors", "q") %in% names(x)) &&
    is.list(x$predictors) &&
    is.data.frame(x$q)
}

get_product_predictor_columns <- function(df, hybas_id = "HYBAS_ID") {
  setdiff(names(df), c(hybas_id, "YYYY", "Q"))
}

get_basin_id_from_data_by_product <- function(data_by_product, hybas_id = "HYBAS_ID") {
  first_df <- data_by_product[[1L]]
  if (!is.data.frame(first_df) || !hybas_id %in% names(first_df)) {
    return("")
  }
  as.character(first_df[[hybas_id]][1L] %||% "")
}

build_compact_data_by_product <- function(data_by_products, basin_id, hybas_id = "HYBAS_ID") {
  q_df <- data_by_products$q[
    as.character(data_by_products$q[[hybas_id]]) == as.character(basin_id),
    ,
    drop = FALSE
  ]

  if (nrow(q_df) == 0L) {
    stop("No rows found in compact `q` table for basin: ", basin_id, call. = FALSE)
  }

  products <- names(data_by_products$predictors)

  setNames(
    lapply(products, function(product) {
      pred_df <- data_by_products$predictors[[product]]

      if (!is.data.frame(pred_df) || !"YYYY" %in% names(pred_df)) {
        stop("Compact predictors for product `", product, "` must contain `YYYY`.", call. = FALSE)
      }

      out <- merge(q_df, pred_df, by = "YYYY", all.x = TRUE, sort = FALSE)
      predictor_cols <- get_product_predictor_columns(out, hybas_id = hybas_id)
      out <- out[order(out$YYYY), c(hybas_id, "YYYY", "Q", predictor_cols), drop = FALSE]
      rownames(out) <- NULL
      out
    }),
    products
  )
}

make_basin_iterator <- function(data_by_products, hybas_id = "HYBAS_ID") {
  if (is_compact_predictor_data(data_by_products)) {
    basin_ids <- unique(as.character(data_by_products$q[[hybas_id]]))
    basin_ids <- basin_ids[!is.na(basin_ids) & nzchar(basin_ids)]

    iterator <- setNames(as.list(basin_ids), basin_ids)
    attr(iterator, "data_structure") <- "compact"
    return(iterator)
  }

  if (!is.list(data_by_products) || length(data_by_products) == 0L) {
    stop("`data_by_products` must be a non-empty list.", call. = FALSE)
  }

  attr(data_by_products, "data_structure") <- "basin_first"
  data_by_products
}

prepare_basin_data_by_product <- function(item, data_by_products, hybas_id = "HYBAS_ID") {
  if (is_compact_predictor_data(data_by_products)) {
    return(build_compact_data_by_product(data_by_products, basin_id = item, hybas_id = hybas_id))
  }

  item
}

message("▶ STEP 3: Run models — START")

tryCatch({
  basin_iterator <- make_basin_iterator(data_by_products, hybas_id = "HYBAS_ID")
  data_structure <- attr(basin_iterator, "data_structure") %||% "basin_first"

  message("Input predictor structure detected: ", data_structure)

  with_progress({
    p <- progressor(along = basin_iterator)

    res_forecast <- future_map(
      basin_iterator,
      function(.x) {
        data_by_product <- prepare_basin_data_by_product(
          item = .x,
          data_by_products = data_by_products,
          hybas_id = "HYBAS_ID"
        )

        basin_id <- get_basin_id_from_data_by_product(
          data_by_product,
          hybas_id = "HYBAS_ID"
        )

        res <- wass2s_run_basins_ml(
          data_by_product = data_by_product,
          hybas_id = "HYBAS_ID",
          pred_pattern_by_product = pred_pattern_by_product,
          models = tolower(MODELS),
          topK = 5,
          sub_fuser = "rf",
          product_fusion_method = "median",
          fusion_method = "meta",
          sub_grid_levels = 3,
          min_kge_model = -Inf,
          grid_levels = 5,
          prediction_years = fyears,
          verbose_tune = FALSE,
          quiet = FALSE,
          final_fuser = tolower(FINAL_FUSER),
          parallel = FALSE,
          workers = 4,
          auto_pca = auto_pca,
          apply_corr = apply_corr,
          apply_normalize = apply_normalize,
          apply_impute = apply_impute,
          impute_nominal = impute_nominal
        )

        p(sprintf("Done: %s", basin_id %||% ""))
        res
      },
      .options = furrr_options(seed = TRUE)
    )
  })

  plan(sequential)

  message("✔ STEP 3: Run models — COMPLETED SUCCESSFULLY")

}, error = function(e) {
  plan(sequential)
  message("❌ STEP 3: Run models — FAILED")
  stop(e)
})
