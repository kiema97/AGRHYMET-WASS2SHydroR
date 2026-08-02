test_that("wass2s_tune_pred_stat returns expected structure and non-empty preds", {
  skip_if_not_installed("tune")
  skip_if_not_installed("workflows")
  skip_if_not_installed("recipes")
  skip_if_not_installed("yardstick")

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

  out <- wass2s_tune_pred_stat(
    df_basin_product = df,
    predictors = predictors,
    model = "ridge",
    prediction_years = c(2000, 2001),
    quiet = TRUE,
    verbose_tune = FALSE
  )

  expect_type(out, "list")
  expect_true(all(c(
    "kge_cv_mean", "rsq_cv_mean", "rmse_cv_mean", "mae_cv_mean",
    "preds", "leaderboard_cfg", "selected_config", "selection_metric",
    "kge_cv_raw", "fit_diagnostics", "overfit_flag"
  ) %in% names(out)))
  expect_s3_class(out$preds, "tbl_df")
  expect_true(all(c("YYYY", "pred") %in% names(out$preds)))
  expect_true(nrow(out$preds) > 0)

  if (!is.na(out$selected_config) && nrow(out$leaderboard_cfg) > 0) {
    selected_row <- out$leaderboard_cfg[out$leaderboard_cfg$.config == out$selected_config, , drop = FALSE]
    expect_equal(nrow(selected_row), 1L)
    expect_equal(out$kge_cv_raw, selected_row$kge_mean[[1]], tolerance = 1e-10)
    expect_lte(out$kge_cv_mean, out$kge_cv_raw)
  }
})

test_that("wass2s_tune_pred_stat respects target_positive", {
  skip_if_not_installed("tune")
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

  out <- wass2s_tune_pred_stat(
    df_basin_product = df,
    predictors = predictors,
    model = "ridge",
    target_positive = TRUE,
    quiet = TRUE,
    verbose_tune = FALSE
  )

  expect_true(all(out$preds$pred >= 0 | is.na(out$preds$pred)))
})
