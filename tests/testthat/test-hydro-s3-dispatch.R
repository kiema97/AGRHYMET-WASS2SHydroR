test_that("hydro method is registered and dispatches", {
  m <- method_id("hydro")
  expect_s3_class(m, "method_hydro")
  expect_s3_class(m, "method")
})
