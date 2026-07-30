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
