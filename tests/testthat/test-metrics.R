test_that("wass2s_kge requires at least two finite pairs", {
  expect_true(is.na(wass2s_kge(1, 1)))
  expect_true(is.na(wass2s_kge(c(1, NA), c(1, 2))))
})

test_that("wass2s_kge returns finite value for valid varying vectors", {
  score <- wass2s_kge(c(1, 2, 3, 4), c(1.1, 1.9, 3.2, 3.8))
  expect_true(is.finite(score))
})
