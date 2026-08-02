test_that(".ensure_year_bounds handles YYYY", {
  expect_equal(.ensure_year_bounds(c(2001, 2005)), c(20010101, 20051231))
})

test_that(".ensure_year_bounds handles YYYYMMDD", {
  expect_equal(.ensure_year_bounds(c(20010701, 20050701)), c(20010701, 20050701))
})

test_that(".ensure_year_bounds handles mixed formats", {
  expect_equal(.ensure_year_bounds(c(2001, 20050701)), c(20010101, 20050701))
})

test_that(".ensure_year_bounds rejects invalid input", {
  expect_error(.ensure_year_bounds(c("2001", "2005")))
  expect_error(.ensure_year_bounds(c(1800, 2005)))
  test_that(".ensure_year_bounds sorts bounds when input order is reversed", {
    expect_equal(.ensure_year_bounds(c(2005, 2001)), c(20010101, 20051231))
  })

})

test_that(".wass2s_score_fusion does not report KGE for one-point test sets", {
  df <- tibble::tibble(
    Q = 10,
    pred_final = 12
  )

  score <- .wass2s_score_fusion(df, basin_id = 1)

  expect_true(is.na(score$kge))
  expect_true(is.finite(score$rmse))
})

test_that("meta-fusion guard falls back when meta does not beat simple baselines enough", {
  set.seed(42)
  df <- tibble::tibble(
    YYYY = as.integer(paste0(2001:2020, "0101")),
    Q = seq(10, 200, length.out = 20),
    m1 = seq(11, 201, length.out = 20),
    m2 = seq(9, 199, length.out = 20),
    m3 = seq(10, 200, length.out = 20) + stats::rnorm(20, sd = 2)
  )

  res <- .wass2s_fuse_predictions(
    fused_models = df,
    basin_id = 1,
    prediction_years = c(2020, 2020),
    fusion_method = "meta",
    final_fuser = "rf",
    grid_levels = 2,
    quiet = TRUE,
    verbose_tune = FALSE,
    meta_guard = TRUE,
    meta_min_improvement = 1
  )

  expect_true(res$fusion_method %in% c("mean", "median", "weighted_mean"))
  expect_false(identical(res$fusion_method, "meta"))
})

test_that("final fusion can select the best individual model explicitly", {
  df <- tibble::tibble(
    YYYY = as.integer(paste0(2001:2010, "0101")),
    Q = 1:10,
    strong = 1:10,
    weak = rep(5, 10)
  )

  res <- .wass2s_fuse_predictions(
    fused_models = df,
    basin_id = 1,
    prediction_years = c(2009, 2010),
    fusion_method = "best",
    quiet = TRUE
  )

  expect_equal(res$fusion_method, "best")
  expect_equal(res$best_model, "strong")
  expect_equal(res$fused_by_model$pred_final, df$strong)
})

test_that("final fusion guard falls back when fusion does not beat best model", {
  df <- tibble::tibble(
    YYYY = as.integer(paste0(2001:2010, "0101")),
    Q = 1:10,
    strong = 1:10,
    weak = rep(5, 10)
  )

  res <- .wass2s_fuse_predictions(
    fused_models = df,
    basin_id = 1,
    prediction_years = c(2009, 2010),
    fusion_method = "weighted_mean",
    quiet = TRUE,
    best_model_guard = TRUE
  )

  expect_equal(res$fusion_method, "best")
  expect_equal(res$best_model, "strong")
  expect_true(all(c("pred_mean", "pred_median", "pred_weighted_mean", "pred_best") %in% names(res$fusion_candidates)))
  expect_equal(res$fusion_candidates$pred_weighted_mean, res$fusion_report$candidate_predictions$pred_weighted_mean)
})

test_that("auto final fusion compares candidates and reports decision", {
  df <- tibble::tibble(
    YYYY = as.integer(paste0(2001:2012, "0101")),
    Q = 1:12,
    strong = 1:12,
    weak = rep(6, 12)
  )

  res <- .wass2s_fuse_predictions(
    fused_models = df,
    basin_id = 1,
    prediction_years = c(2011, 2012),
    fusion_method = "auto",
    quiet = TRUE
  )

  expect_equal(res$requested_fusion_method, "auto")
  expect_equal(res$fusion_method, "best")
  expect_type(res$fusion_report, "list")
  expect_true(all(c("method", "kge", "rmse", "selected") %in% names(res$fusion_report$candidate_scores)))
  expect_true(all(c("method", "cv_kge", "cv_rmse", "selected") %in% names(res$fusion_report$candidate_cv_scores)))
  expect_true(any(res$fusion_report$candidate_scores$selected))
  expect_equal(res$fusion_report$selection_reason, "auto_best_cv_rmse_train_period")
  expect_false(res$fusion_report$diagnostics$decision$used_test_for_selection[[1]])
})

test_that("explicit fusion can be imposed by disabling best-model guard", {
  df <- tibble::tibble(
    YYYY = as.integer(paste0(2001:2010, "0101")),
    Q = 1:10,
    strong = 1:10,
    weak = rep(5, 10)
  )

  res <- .wass2s_fuse_predictions(
    fused_models = df,
    basin_id = 1,
    prediction_years = c(2009, 2010),
    fusion_method = "weighted_mean",
    quiet = TRUE,
    best_model_guard = FALSE
  )

  expect_equal(res$requested_fusion_method, "weighted_mean")
  expect_equal(res$fusion_method, "weighted_mean")
})
