test_that("wass2s_download_cds builds requests without forcing RStudio jobs", {
  skip_if_not_installed("ecmwfr")

  captured <- new.env(parent = emptyenv())
  out_dir <- withr::local_tempdir()

  testthat::local_mocked_bindings(
    wf_get_key = function(user = "ecmwfr") "dummy-key",
    wf_request = function(request, user = "ecmwfr", transfer = TRUE,
                          path = tempdir(), time_out = 3600, retry = 30,
                          verbose = TRUE, ...) {
      dots <- list(...)
      captured$request <- request
      captured$user <- user
      captured$dots <- names(dots)
      file.create(file.path(path, request$target))
      file.path(path, request$target)
    },
    .package = "ecmwfr"
  )

  res <- wass2s_download_cds(
    dataset_short_name = "seasonal-original-single-levels",
    base_query = list(format = "netcdf"),
    center_variables = "ecmwf_51.T2M",
    years = 2020,
    months = 1,
    days = "01",
    times = "00:00",
    leadtime_hour = 24,
    area = c(15, -2, 14, -1),
    out_dir = out_dir,
    user = "ecmwfr",
    tries = 1,
    timeout_sec = 10,
    verbose = FALSE
  )

  expect_equal(res$status, "ok")
  expect_equal(captured$user, "ecmwfr")
  expect_false("job_name" %in% captured$dots)
  expect_equal(captured$request$originating_centre, "ecmwf")
  expect_equal(captured$request$system, "51")
  expect_equal(captured$request$variable, "2m_temperature")
  expect_equal(captured$request$year, "2020")
  expect_true(file.exists(res$file))
})
