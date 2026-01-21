test_that("wass2s_tune_pred_ml returns expected structure and preds", {
  skip_if_not_installed("tune")
  skip_if_not_installed("workflows")
  skip_if_not_installed("recipes")
  skip_if_not_installed("yardstick")
  skip_if_not_installed("ranger")
  set.seed(123)

  data_by_product <- make_toy_data_by_product(
    basins = c(1040021500),
    years = 1990:2005,
    products = c("SST_CMCC"),
    p = 6,
    seed = 123
  )
  df <- data_by_product[[1]]
  predictors <- grep("^pt_", names(df), value = TRUE)

  out <- wass2s_tune_pred_ml(
    df_basin_product = df,
    predictors = predictors,
    model = "rf",
    grid_levels = 2,
    quiet = TRUE
  )

  expect_type(out, "list")
  expect_true(all(c("kge_cv_mean", "preds") %in% names(out)))
  expect_s3_class(out$preds, "tbl_df")
  expect_true(all(c("YYYY", "pred") %in% names(out$preds)))
  expect_true(nrow(out$preds) > 0)
})

