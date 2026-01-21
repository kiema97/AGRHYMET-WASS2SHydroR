# A) Test structure + invariants (sort, unique, types)
test_that("wass2s_cons_mods_stat returns correct structure (YYYYMMDD)", {
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
    prediction_years = c(20011231, 20051231),
    use_sub_fuser = FALSE,
    quiet = TRUE,
    verbose = FALSE,
    seed = 123
  )

  expect_type(out, "list")
  expect_true(all(c("fused", "leaderboard_products") %in% names(out)))

  expect_s3_class(out$fused, "tbl_df")
  expect_true(all(c("YYYY", "pred_fused") %in% names(out$fused)))

  expect_true(is.integer(out$fused$YYYY) || is.numeric(out$fused$YYYY))
  expect_true(is.numeric(out$fused$pred_fused))

  # unique + sorted timeline
  expect_equal(anyDuplicated(out$fused$YYYY), 0L)
  expect_true(all(diff(out$fused$YYYY) >= 0))
})


# B) Test use_sub_fuser ON/OFF : même timeline (et pas d’erreur)

test_that("use_sub_fuser toggling preserves YYYY timeline", {
  data_by_product <- make_toy_data_by_product(
    basins = c(1040021500),
    years = 1990:2010,
    products = c("SST_CMCC", "SST_ECMWF", "SST_JMA"),
    p = 6,
    seed = 123
  )

  out_simple <- wass2s_cons_mods_stat(
    basin_id = 1040021500,
    data_by_product = data_by_product,
    model = "pcr",
    topK = 2,
    prediction_years = c(20011231, 20051231),
    use_sub_fuser = FALSE,
    quiet = TRUE,
    verbose = FALSE,
    seed = 123
  )

  out_meta <- wass2s_cons_mods_stat(
    basin_id = 1040021500,
    data_by_product = data_by_product,
    model = "pcr",
    topK = 2,
    prediction_years = c(20011231, 20051231),
    use_sub_fuser = TRUE,
    sub_fuser = "rf",
    sub_grid_levels = 3,  # small for tests
    quiet = TRUE,
    verbose = FALSE,
    seed = 123
  )

  expect_identical(out_simple$fused$YYYY, out_meta$fused$YYYY)
})


# C) Test topK trop grand : ne dépasse pas le nb de produits

test_that("topK larger than products does not break and weights are bounded", {
  data_by_product <- make_toy_data_by_product(
    basins = c(1040021500),
    years = 1990:2005,
    products = c("SST_CMCC", "SST_ECMWF"),
    p = 6,
    seed = 1
  )

  out <- wass2s_cons_mods_stat(
    basin_id = 1040021500,
    data_by_product = data_by_product,
    model = "lasso",
    topK = 99,
    prediction_years = c(20011231, 20051231),
    use_sub_fuser = FALSE,
    quiet = TRUE,
    verbose = FALSE
  )

  lb <- out$leaderboard_products
  expect_true("weight" %in% names(lb))
  expect_true(sum(lb$weight > 0, na.rm = TRUE) <= length(data_by_product))
})


# D) Test robustesse : un produit défectueux ne casse pas le bassin
test_that("one broken product does not crash the fusion", {
  data_by_product <- make_toy_data_by_product(
    basins = c(1040021500),
    years = 1990:2010,
    products = c("OK", "BAD"),
    p = 6,
    seed = 123
  )
  # Break BAD: remove Q
  data_by_product$BAD$Q <- NULL

  out <- wass2s_cons_mods_stat(
    basin_id = 1040021500,
    data_by_product = data_by_product,
    model = "ridge",
    topK = 2,
    prediction_years = c(20010701, 20050701),
    use_sub_fuser = FALSE,
    quiet = TRUE,
    verbose = FALSE
  )

  expect_s3_class(out$fused, "tbl_df")
  expect_true(all(c("YYYY", "pred_fused") %in% names(out$fused)))
})

# E) la fusion ne doit pas être “toute NA”
test_that("fusion should not be all NA on toy data (simple fuser)", {
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



