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
    base_query = list(data_format = "netcdf"),
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

test_that("wass2s_download_cds dry-run supports multi-year chunks and default data_format", {
  skip_if_not_installed("ecmwfr")

  testthat::local_mocked_bindings(
    wf_get_key = function(user = "ecmwfr", service = "cds") "dummy-key",
    .package = "ecmwfr"
  )

  out_dir <- withr::local_tempdir()
  plan <- wass2s_download_cds(
    dataset_short_name = "seasonal-original-single-levels",
    base_query = list(),
    center_variables = "ecmwf_51.PRCP",
    years = 1993:1997,
    months = 5,
    days = "01",
    times = "00:00",
    leadtime_hour = 24,
    area = c(15, -2, 14, -1),
    out_dir = out_dir,
    user = "ecmwfr",
    chunk_years = 2,
    dry_run = TRUE,
    return_requests = TRUE,
    job_log = NULL,
    verbose = FALSE
  )

  expect_equal(plan$status, rep("planned", 3))
  expect_equal(plan$year, c("1993-1994", "1995-1996", "1997"))
  expect_equal(plan$year_start, c(1993L, 1995L, 1997L))
  expect_equal(plan$year_end, c(1994L, 1996L, 1997L))
  expect_true(all(grepl("ecmwf_51_PRCP_May01_", basename(plan$file))))
  expect_equal(plan$request[[1]]$data_format, "netcdf")
  expect_equal(plan$request[[1]]$year, c("1993", "1994"))
})

test_that("wass2s_download_cds skips existing chunk files", {
  skip_if_not_installed("ecmwfr")

  testthat::local_mocked_bindings(
    wf_get_key = function(user = "ecmwfr", service = "cds") "dummy-key",
    .package = "ecmwfr"
  )

  out_dir <- withr::local_tempdir()
  existing <- file.path(out_dir, "ecmwf_51_T2M_Jan01_2020_24-24.nc")
  file.create(existing)

  res <- wass2s_download_cds(
    dataset_short_name = "seasonal-original-single-levels",
    base_query = list(data_format = "netcdf"),
    center_variables = "ecmwf_51.T2M",
    years = 2020,
    months = 1,
    days = "01",
    times = "00:00",
    leadtime_hour = 24,
    area = c(15, -2, 14, -1),
    out_dir = out_dir,
    user = "ecmwfr",
    job_log = NULL,
    verbose = FALSE
  )

  expect_equal(res$status, "skip")
  expect_equal(normalizePath(res$file, winslash = "/"), normalizePath(existing, winslash = "/"))
})

test_that("wass2s_download_cds combines existing NetCDF chunks", {
  skip_if_not_installed("ecmwfr")
  skip_if_not_installed("ncdf4")

  testthat::local_mocked_bindings(
    wf_get_key = function(user = "ecmwfr", service = "cds") "dummy-key",
    .package = "ecmwfr"
  )

  out_dir <- withr::local_tempdir()
  make_nc <- function(path, tref, value) {
    lon <- ncdf4::ncdim_def("longitude", "degrees_east", vals = c(0, 1))
    lat <- ncdf4::ncdim_def("latitude", "degrees_north", vals = c(10, 11))
    frt <- ncdf4::ncdim_def("forecast_reference_time", "days since 1900-01-01", vals = tref, unlim = TRUE)
    var <- ncdf4::ncvar_def("tp", "m", list(lon, lat, frt), missval = -9999, prec = "float")
    nc <- ncdf4::nc_create(path, var)
    on.exit(ncdf4::nc_close(nc), add = TRUE)
    ncdf4::ncvar_put(nc, "tp", array(value, dim = c(2, 2, 1)))
  }

  f1 <- file.path(out_dir, "ecmwf_51_PRCP_May01_1993_24-24.nc")
  f2 <- file.path(out_dir, "ecmwf_51_PRCP_May01_1994_24-24.nc")
  make_nc(f1, 34000, 1)
  make_nc(f2, 34365, 2)

  res <- wass2s_download_cds(
    dataset_short_name = "seasonal-original-single-levels",
    base_query = list(data_format = "netcdf"),
    center_variables = "ecmwf_51.PRCP",
    years = 1993:1994,
    months = 5,
    days = "01",
    times = "00:00",
    leadtime_hour = 24,
    area = c(15, -2, 14, -1),
    out_dir = out_dir,
    user = "ecmwfr",
    combine = TRUE,
    combine_filename_tpl = "{modelsys}_{var}_{period}.nc",
    job_log = NULL,
    verbose = FALSE
  )

  expect_true(all(res$status == "skip"))
  expect_true(all(res$combine_status == "ok"))
  expect_true(file.exists(res$combined_file[1]))
  nc <- ncdf4::nc_open(res$combined_file[1])
  on.exit(ncdf4::nc_close(nc), add = TRUE)
  expect_equal(nc$dim$forecast_reference_time$len, 2)
  vals <- ncdf4::ncvar_get(nc, "tp")
  expect_equal(dim(vals), c(2L, 2L, 2L))
  expect_equal(as.numeric(vals[1, 1, ]), c(1, 2))
})

test_that("wass2s_download_cds limits batch submission size", {
  skip_if_not_installed("ecmwfr")

  captured <- new.env(parent = emptyenv())
  captured$n <- 0L
  captured$sizes <- integer()
  out_dir <- withr::local_tempdir()

  testthat::local_mocked_bindings(
    wf_get_key = function(user = "ecmwfr", service = "cds") "dummy-key",
    wf_request_batch = function(request_list, workers = 1, user = "ecmwfr", path = tempdir(),
                                time_out = 3600, retry = 30, total_timeout = 3600) {
      captured$n <- captured$n + 1L
      captured$sizes <- c(captured$sizes, length(request_list))
      for (req in request_list) file.create(file.path(path, req$target))
      invisible(TRUE)
    },
    .package = "ecmwfr"
  )

  res <- wass2s_download_cds(
    dataset_short_name = "seasonal-original-single-levels",
    base_query = list(data_format = "netcdf"),
    center_variables = "ecmwf_51.T2M",
    years = 2020:2024,
    months = 1,
    days = "01",
    times = "00:00",
    leadtime_hour = 24,
    area = c(15, -2, 14, -1),
    out_dir = out_dir,
    user = "ecmwfr",
    parallel = TRUE,
    workers = 4,
    max_requests_per_batch = 2,
    job_log = NULL,
    verbose = FALSE
  )

  expect_equal(captured$n, 3L)
  expect_equal(captured$sizes, c(2L, 2L, 1L))
  expect_true(all(res$status == "ok"))
})

test_that("NetCDF combine handles NCEP-like files with scalar number variable", {
  skip_if_not_installed("ncdf4")

  out_dir <- withr::local_tempdir()
  make_ncep_like <- function(path, tref, value_offset) {
    lon <- ncdf4::ncdim_def("longitude", "degrees_east", vals = c(0, 1))
    lat <- ncdf4::ncdim_def("latitude", "degrees_north", vals = c(10, 11))
    fp <- ncdf4::ncdim_def("forecast_period", "hours", vals = c(24, 48))
    frt <- ncdf4::ncdim_def("forecast_reference_time", "seconds since 1970-01-01", vals = tref, unlim = TRUE)
    number <- ncdf4::ncvar_def("number", "1", list(), missval = NA, prec = "double")
    valid_time <- ncdf4::ncvar_def("valid_time", "seconds since 1970-01-01", list(fp, frt), missval = NA, prec = "double")
    tp <- ncdf4::ncvar_def("tp", "m", list(lon, lat, fp, frt), missval = -9999, prec = "float")
    nc <- ncdf4::nc_create(path, list(number, valid_time, tp), force_v4 = TRUE)
    on.exit(ncdf4::nc_close(nc), add = TRUE)
    ncdf4::ncvar_put(nc, "number", 0)
    ncdf4::ncvar_put(nc, "valid_time", matrix(rep(tref, each = 2) + c(24, 48) * 3600, nrow = 2))
    ncdf4::ncvar_put(nc, "tp", array(value_offset + seq_len(2 * 2 * 2 * length(tref)), dim = c(2, 2, 2, length(tref))))
  }

  f1 <- file.path(out_dir, "ncep2_PRCP_1993.nc")
  f2 <- file.path(out_dir, "ncep2_PRCP_1994.nc")
  make_ncep_like(f1, c(725846400, 726105600), 0)
  make_ncep_like(f2, c(757382400, 757641600), 100)

  output <- file.path(out_dir, "ncep2_PRCP_1993_1994.nc")
  WASS2SHydroR:::wass2s__combine_netcdf(c(f1, f2), output, concat_dim = "auto", overwrite = TRUE)

  nc <- ncdf4::nc_open(output)
  on.exit(ncdf4::nc_close(nc), add = TRUE)
  expect_equal(nc$dim$forecast_reference_time$len, 4)
  expect_true("number" %in% names(nc$var))
  expect_equal(ncdf4::ncvar_get(nc, "number"), 0)
  expect_equal(dim(ncdf4::ncvar_get(nc, "valid_time", collapse_degen = FALSE)), c(2L, 4L))
  expect_equal(dim(ncdf4::ncvar_get(nc, "tp", collapse_degen = FALSE)), c(2L, 2L, 2L, 4L))
})

test_that("wass2s_download_cds continues after a sequential request failure", {
  skip_if_not_installed("ecmwfr")

  out_dir <- withr::local_tempdir()
  calls <- new.env(parent = emptyenv())
  calls$n <- 0L

  testthat::local_mocked_bindings(
    wf_get_key = function(user = "ecmwfr", service = "cds") "dummy-key",
    wf_request = function(request, user = "ecmwfr", transfer = TRUE,
                          path = tempdir(), time_out = 3600,
                          verbose = TRUE, ...) {
      calls$n <- calls$n + 1L
      if (identical(request$year, "2020")) stop("simulated CDS failure")
      file.create(file.path(path, request$target))
      file.path(path, request$target)
    },
    .package = "ecmwfr"
  )

  res <- wass2s_download_cds(
    dataset_short_name = "seasonal-original-single-levels",
    base_query = list(data_format = "netcdf"),
    center_variables = "ecmwf_51.T2M",
    years = 2020:2021,
    months = 1,
    days = "01",
    times = "00:00",
    leadtime_hour = 24,
    area = c(15, -2, 14, -1),
    out_dir = out_dir,
    user = "ecmwfr",
    tries = 1,
    job_log = NULL,
    verbose = FALSE
  )

  expect_equal(calls$n, 2L)
  expect_equal(res$status, c("fail", "ok"))
  expect_match(res$error[1], "simulated CDS failure")
  expect_true(file.exists(res$file[2]))
})

test_that("wass2s_download_cds can stop on first sequential request failure", {
  skip_if_not_installed("ecmwfr")

  out_dir <- withr::local_tempdir()

  testthat::local_mocked_bindings(
    wf_get_key = function(user = "ecmwfr", service = "cds") "dummy-key",
    wf_request = function(request, user = "ecmwfr", transfer = TRUE,
                          path = tempdir(), time_out = 3600,
                          verbose = TRUE, ...) {
      stop("simulated CDS failure")
    },
    .package = "ecmwfr"
  )

  expect_error(
    wass2s_download_cds(
      dataset_short_name = "seasonal-original-single-levels",
      base_query = list(data_format = "netcdf"),
      center_variables = "ecmwf_51.T2M",
      years = 2020:2021,
      months = 1,
      days = "01",
      times = "00:00",
      leadtime_hour = 24,
      area = c(15, -2, 14, -1),
      out_dir = out_dir,
      user = "ecmwfr",
      tries = 1,
      stop_on_error = TRUE,
      job_log = NULL,
      verbose = FALSE
    ),
    "CDS request failed after"
  )
})