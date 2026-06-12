test_that("hydro method is registered and dispatches", {
  m <- method_id("hydro")
  expect_s3_class(m, "method_hydro")
  expect_s3_class(m, "method")
})

test_that("stat and ml methods are registered", {
  expect_true(all(c("stat", "hydro", "ml") %in% list_methods()))
  expect_s3_class(method_id("stat"), "method_stat")
  expect_s3_class(method_id("ml"), "method_ml")
})
