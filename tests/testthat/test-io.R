library(testthat)

test_that("read_cfg lit un YAML simple", {
  yml <- tempfile(fileext = ".yaml")
  writeLines("a: 1
b: [2,3]
", yml)
  cfg <- read_cfg(yml)
  expect_type(cfg, "list")
  expect_equal(cfg$a, 1)
  #expect_equal(cfg$b, list(2,3))
  expect_equal(unname(unlist(cfg$b)), c(2, 3))
})
