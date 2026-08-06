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
  expect_true(all(c(
    "kge_cv_mean", "rmse_cv_mean", "mae_cv_mean", "preds",
    "leaderboard_cfg", "selected_config", "selection_metric",
    "kge_cv_raw", "fit_diagnostics", "overfit_flag",
    "generalization_ok", "reject_overfit", "guard_reason"
  ) %in% names(out)))
  expect_s3_class(out$preds, "tbl_df")
  expect_true(all(c("YYYY", "pred") %in% names(out$preds)))
  expect_true(nrow(out$preds) > 0)

  if (!is.na(out$selected_config) && nrow(out$leaderboard_cfg) > 0) {
    selected_row <- out$leaderboard_cfg[out$leaderboard_cfg$.config == out$selected_config, , drop = FALSE]
    expect_equal(nrow(selected_row), 1L)
    expect_equal(out$kge_cv_raw, selected_row$kge_mean[[1]], tolerance = 1e-10)
    if (is.finite(out$kge_cv_mean)) {
      expect_lte(out$kge_cv_mean, out$kge_cv_raw)
    } else {
      expect_true(out$overfit_flag)
    }
  }
})

test_that("ML overfit diagnostics flag large fit-vs-CV gaps", {
  diag <- .wass2s_ml_fit_cv_diagnostics(
    truth = 1:10,
    estimate = 1:10,
    cv_kge = 0.1,
    cv_rmse = 100,
    max_fit_cv_kge_gap = 0.5,
    max_cv_fit_rmse_ratio = 4
  )

  expect_true(diag$overfit_flag[[1]])
  expect_false(diag$generalization_ok[[1]])
  expect_true(grepl("fit_cv_kge_gap", diag$guard_reason[[1]], fixed = TRUE))
  expect_gt(diag$fit_cv_kge_gap[[1]], 0.5)
})

test_that("ML overfit guard can reject unsafe candidates without hiding raw score", {
  skip_if_not_installed("tune")
  skip_if_not_installed("workflows")
  skip_if_not_installed("recipes")
  skip_if_not_installed("yardstick")
  skip_if_not_installed("glmnet")

  data_by_product <- make_toy_data_by_product(
    basins = c(1040021500),
    years = 1990:2005,
    products = c("SST_CMCC"),
    p = 4,
    seed = 124
  )
  df <- data_by_product[[1]]
  predictors <- grep("^pt_", names(df), value = TRUE)

  out <- wass2s_tune_pred_ml(
    df_basin_product = df,
    predictors = predictors,
    model = "glmnet",
    grid_levels = 2,
    max_fit_cv_kge_gap = -1,
    max_cv_fit_rmse_ratio = 0,
    reject_overfit = TRUE,
    quiet = TRUE
  )

  expect_true(out$overfit_flag)
  expect_false(out$generalization_ok)
  expect_true(is.na(out$kge_cv_mean))
  expect_true(is.finite(out$kge_cv_raw) || is.na(out$kge_cv_raw))
  expect_true(nchar(out$guard_reason) > 0)
})

test_that("wass2s_tune_pred_ml rejects unsafe outcome transformations", {
  skip_if_not_installed("tune")

  data_by_product <- make_toy_data_by_product(
    basins = c(1040021500),
    years = 1990:2005,
    products = c("SST_CMCC"),
    p = 6,
    seed = 123
  )
  df <- data_by_product[[1]]
  predictors <- grep("^pt_", names(df), value = TRUE)

  expect_error(
    wass2s_tune_pred_ml(
      df_basin_product = df,
      predictors = predictors,
      model = "rf",
      y_transform = "log1p",
      quiet = TRUE
    ),
    "not currently safe"
  )
})

