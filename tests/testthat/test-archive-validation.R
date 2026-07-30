test_that("validate_archive identifies ok and missing yearly chunks", {
  out_dir <- withr::local_tempdir()
  existing <- file.path(out_dir, "ecmwf51_PRCP_May01_2020_2021_24-48_2020.nc")
  writeBin(as.raw(rep(1, 16)), existing)

  audit <- wass2s_validate_archive(
    out_dir = out_dir,
    dataset_short_name = "seasonal-original-single-levels",
    center_variables = "ecmwf_51.PRCP",
    years = 2020:2021,
    months = 5,
    days = "01",
    leadtime_hour = c(24, 48),
    min_bytes = 0,
    check_netcdf = FALSE
  )

  expect_s3_class(audit, "wass2s_archive_audit")
  expect_equal(nrow(audit$audit), 2)
  expect_setequal(audit$audit$status, c("ok", "missing"))

  repair <- wass2s_repair_archive_plan(audit)
  expect_equal(nrow(repair), 1)
  expect_equal(repair$year, 2021)
  expect_equal(repair$reason, "missing")
})

test_that("validate_archive flags too-small chunks for repair", {
  out_dir <- withr::local_tempdir()
  existing <- file.path(out_dir, "ukmo610_PRCP_May01_1993_1993_24-24_1993.nc")
  file.create(existing)

  audit <- wass2s_validate_archive(
    out_dir = out_dir,
    dataset_short_name = "seasonal-original-single-levels",
    center_variables = "ukmo_610.PRCP",
    years = 1993,
    months = 5,
    days = "01",
    leadtime_hour = 24,
    min_bytes = 1,
    check_netcdf = FALSE
  )

  expect_equal(audit$audit$status, "too_small")
  repair <- wass2s_repair_archive_plan(audit)
  expect_equal(nrow(repair), 1)
  expect_equal(repair$reason, "too_small")
})

test_that("repair plan validates input shape", {
  expect_error(
    wass2s_repair_archive_plan(data.frame(file = "x")),
    "audit must be"
  )
})
