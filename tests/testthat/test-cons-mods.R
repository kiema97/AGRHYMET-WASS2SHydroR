test_that("wass2s_cons_mods_stat returns correct structure (YYYYMMDD) and unique timeline", {
  skip_if_not_installed("tune")

  data_by_product <- make_toy_data_by_product(
    basins = c(1040021500),
    years = 1990:2010,
    products = c("SST_CMCC", "SST_ECMWF"),
    p = 6,
    seed = 123
  )

  out <- wass2s_cons_mods_stat(
    basin_id = 1040021500,
    data_by_product = data_by_product,
    basin_col = "HYBAS_ID",
    model = "ridge",
    topK = 2,
    prediction_years = c(20010701, 20050701),
    use_sub_fuser = FALSE,
    quiet = TRUE,
    verbose = FALSE,
    seed = 123
  )

  expect_type(out, "list")
  expect_true(all(c("fused", "leaderboard_products") %in% names(out)))

  expect_s3_class(out$fused, "tbl_df")
  expect_true(all(c("YYYY", "pred_fused") %in% names(out$fused)))
  expect_true(is.numeric(out$fused$YYYY) || is.integer(out$fused$YYYY))
  expect_true(is.numeric(out$fused$pred_fused))

  expect_equal(anyDuplicated(out$fused$YYYY), 0L)
  expect_true(all(diff(out$fused$YYYY) >= 0))
})

test_that("wass2s_cons_mods_stat fusion should not be all NA on toy data (simple fuser)", {
  skip_if_not_installed("tune")

  data_by_product <- make_toy_data_by_product(
    basins = c(1040021500),
    years = 1990:2010,
    products = c("SST_CMCC", "SST_ECMWF", "SST_JMA"),
    p = 6,
    seed = 123
  )

  out <- wass2s_cons_mods_stat(
    basin_id = 1040021500,
    data_by_product = data_by_product,
    model = "ridge",
    topK = 2,
    prediction_years = c(20010701, 20050701),
    use_sub_fuser = FALSE,
    quiet = TRUE,
    verbose = FALSE,
    seed = 123
  )

  expect_true(nrow(out$fused) > 0)
  expect_true(sum(!is.na(out$fused$pred_fused)) > 0)
  expect_false(all(is.na(out$fused$pred_fused)))
})

test_that("wass2s_cons_mods_ml returns correct structure and non-NA fusion (simple fuser)", {
  skip_if_not_installed("tune")

  data_by_product <- make_toy_data_by_product(
    basins = c(1040021500),
    years = 1990:2010,
    products = c("SST_CMCC", "SST_ECMWF", "SST_JMA"),
    p = 6,
    seed = 123
  )

  out <- wass2s_cons_mods_ml(
    data_by_product = data_by_product,
    basin_id = 1040021500,
    model = SUPPORTED_MODELS[1],
    topK = 2,
    prediction_years = c(2001, 2005),
    use_sub_fuser = FALSE,
    quiet = TRUE,
    verbose = FALSE
  )

  expect_type(out, "list")
  expect_true(all(c("fused", "leaderboard_products") %in% names(out)))
  expect_s3_class(out$fused, "tbl_df")
  expect_true(all(c("YYYY", "pred_fused") %in% names(out$fused)))
  expect_true(sum(!is.na(out$fused$pred_fused)) > 0)
})

test_that("ML final meta fusion is guarded unless explicitly allowed", {
  skip_if_not_installed("tune")
  skip_if_not_installed("glmnet")
  skip_if_not_installed("ranger")

  data_by_product <- make_toy_data_by_product(
    basins = c(1040021500),
    years = 1990:2010,
    products = c("SST_CMCC", "SST_ECMWF"),
    p = 4,
    seed = 123
  )

  out <- wass2s_run_bas_mod_ml(
    data_by_product = data_by_product,
    basin_id = 1040021500,
    models = c("glmnet", "rf"),
    topK = 2,
    prediction_years = c(2001, 2005),
    product_fusion_method = "median",
    fusion_method = "meta",
    final_fuser = "glmnet",
    grid_levels = 2,
    quiet = TRUE
  )

  expect_equal(out$requested_fusion_method, "meta")
  expect_false(out$allow_in_sample_meta)
  expect_equal(out$fusion_method, "weighted_mean")
})
