test_that("target midpoint and issue-year delta handle cross-year seasons", {
  expect_equal(wass2s_target_midpoint("Dec-Feb"), 1.5)
  expect_equal(wass2s_target_midpoint("Jun-Sep"), 8)
  expect_equal(wass2s_issue_year_delta(11, "Dec-Feb"), -1L)
  expect_equal(wass2s_issue_year_delta(5, "Jun-Sep"), 0L)
})

test_that("run manifest is serializable and preserves core metadata", {
  man <- wass2s_run_manifest(
    approach = c("stat", "ml"),
    issue_date = as.Date("2026-05-01"),
    target = "Jun-Sep",
    config = list(topK = 3),
    inputs = list(cds = "input.nc"),
    outputs = list(final = "forecast.csv"),
    scores = tibble::tibble(kge = 0.6)
  )

  expect_s3_class(man, "wass2s_manifest")
  expect_equal(man$approach, c("stat", "ml"))
  expect_equal(man$config$topK, 3)

  path <- tempfile(fileext = ".rds")
  expect_invisible(wass2s_write_manifest(man, path))
  expect_true(file.exists(path))
  expect_s3_class(readRDS(path), "wass2s_manifest")
})

test_that("forecast version comparison prefers more skilled and complete runs", {
  old <- list(kge = 0.35, rmse = 40, n_models = 3, n_basins = 10, missing_frac = 0.2)
  new <- list(kge = 0.55, rmse = 30, n_models = 4, n_basins = 10, missing_frac = 0.05)
  cmp <- wass2s_compare_forecast_versions(new, old)
  expect_equal(cmp$decision, "new")
  expect_gt(cmp$delta, 0)
})
