test_that("safe_write_result keeps existing output unless the new summary is better", {
  path <- tempfile(fileext = ".rds")

  first <- wass2s_safe_write_result(
    object = list(value = 1),
    path = path,
    summary_new = list(kge = 0.50, rmse = 10)
  )
  expect_equal(first$decision, "written")
  expect_equal(readRDS(path)$value, 1)

  kept <- wass2s_safe_write_result(
    object = list(value = 2),
    path = path,
    summary_new = list(kge = 0.30, rmse = 12)
  )
  expect_equal(kept$decision, "kept_existing")
  expect_equal(readRDS(path)$value, 1)

  replaced <- wass2s_safe_write_result(
    object = list(value = 3),
    path = path,
    summary_new = list(kge = 0.80, rmse = 8),
    backup = FALSE
  )
  expect_equal(replaced$decision, "replaced")
  expect_equal(readRDS(path)$value, 3)
})

test_that("run_issue audits archives and writes a manifest", {
  out_dir <- withr::local_tempdir()
  manifest_path <- tempfile(fileext = ".rds")

  run <- wass2s_run_issue(
    config = list(
      run_id = "test-run",
      issue_date = as.Date("2026-05-01"),
      target = "JJAS",
      approach = "ml",
      manifest_path = manifest_path,
      archive = list(
        out_dir = out_dir,
        dataset_short_name = "seasonal-original-single-levels",
        center_variables = "ecmwf_51.PRCP",
        years = 2020,
        months = 5,
        days = "01",
        leadtime_hour = 24,
        check_netcdf = FALSE,
        min_bytes = 0
      )
    ),
    dry_run = TRUE
  )

  expect_s3_class(run, "wass2s_issue_run")
  expect_true(run$ok)
  expect_equal(nrow(run$repair_plan), 1)
  expect_true(file.exists(manifest_path))
  expect_equal(readRDS(manifest_path)$run_id, "test-run")
})

test_that("run_issue executes named workflow steps with shared context", {
  run <- wass2s_run_issue(
    config = list(
      run_id = "step-run",
      issue_date = as.Date("2026-05-01"),
      target = "JJAS",
      approach = "stat"
    ),
    steps = list(
      prepare = function(context) {
        list(target = context$target, update = context$update)
      }
    ),
    update = TRUE
  )

  expect_true(run$ok)
  expect_equal(names(run$results), "prepare")
  expect_equal(run$results$prepare$target, "JJAS")
  expect_true(run$results$prepare$update)
})

test_that("run_issue can stop when archive repair is required", {
  run <- wass2s_run_issue(
    config = list(
      stop_on_archive_errors = TRUE,
      archive = list(
        out_dir = withr::local_tempdir(),
        dataset_short_name = "seasonal-original-single-levels",
        center_variables = "ecmwf_51.PRCP",
        years = 2020,
        months = 5,
        days = "01",
        leadtime_hour = 24,
        check_netcdf = FALSE,
        min_bytes = 0
      )
    ),
    steps = list(should_not_run = function(context) TRUE)
  )

  expect_false(run$ok)
  expect_equal(run$results, list())
  expect_match(run$errors, "Stopped because archive repair is required")
})
