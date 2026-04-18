test_that("entropy is zero for deterministic forecasts", {
  expect_equal(wass2s_class_entropy(1, 0, 0), 0)
  expect_equal(wass2s_class_entropy(0, 1, 0), 0)
  expect_equal(wass2s_class_entropy(0, 0, 1), 0)
})

test_that("entropy is maximal for uniform probabilities", {
  expected <- log(3)

  expect_equal(
    wass2s_class_entropy(1/3, 1/3, 1/3),
    expected,
    tolerance = 1e-12
  )
})

test_that("normalized entropy is in [0, 1]", {
  expect_equal(
    wass2s_class_entropy(1, 0, 0, normalize = TRUE),
    0,
    tolerance = 1e-12
  )

  expect_equal(
    wass2s_class_entropy(1/3, 1/3, 1/3, normalize = TRUE),
    1,
    tolerance = 1e-12
  )
})

test_that("entropy is invariant to positive rescaling", {
  e1 <- wass2s_class_entropy(0.2, 0.3, 0.5)
  e2 <- wass2s_class_entropy(2, 3, 5)

  expect_equal(e1, e2, tolerance = 1e-12)
})

test_that("entropy handles vector inputs correctly", {
  res <- wass2s_class_entropy(
    p_below  = c(1, 1/3, 0.2),
    p_normal = c(0, 1/3, 0.3),
    p_above  = c(0, 1/3, 0.5)
  )

  expect_length(res, 3)
  expect_equal(res[1], 0, tolerance = 1e-12)
  expect_equal(res[2], log(3), tolerance = 1e-12)
  expect_true(is.finite(res[3]))
})

test_that("zero probabilities are handled correctly", {
  expected <- -(0.5 * log(0.5) + 0.5 * log(0.5))

  expect_equal(
    wass2s_class_entropy(0.5, 0.5, 0),
    expected,
    tolerance = 1e-12
  )

  expect_equal(
    wass2s_class_entropy(0, 0.5, 0.5),
    expected,
    tolerance = 1e-12
  )
})

test_that("invalid rows return NA when all probabilities are missing or zero", {
  expect_true(is.na(wass2s_class_entropy(NA, NA, NA)))
  expect_true(is.na(wass2s_class_entropy(0, 0, 0)))
  expect_true(is.na(wass2s_class_entropy(-1, -2, -3)))
})

test_that("negative values are clipped to zero before computation", {
  res <- wass2s_class_entropy(-1, 1, 1)
  expected <- wass2s_class_entropy(0, 1, 1)

  expect_equal(res, expected, tolerance = 1e-12)
})

test_that("non-finite values are treated as zero", {
  res1 <- wass2s_class_entropy(Inf, 1, 1)
  res2 <- wass2s_class_entropy(0, 1, 1)

  expect_equal(res1, res2, tolerance = 1e-12)

  res3 <- wass2s_class_entropy(NaN, 1, 1)
  expect_equal(res3, res2, tolerance = 1e-12)
})

test_that("output is bounded between 0 and log(3) when not normalized", {
  res <- wass2s_class_entropy(
    p_below  = c(1, 1/3, 0.7, 0.2),
    p_normal = c(0, 1/3, 0.2, 0.5),
    p_above  = c(0, 1/3, 0.1, 0.3)
  )

  expect_true(all(res >= 0, na.rm = TRUE))
  expect_true(all(res <= log(3) + 1e-12, na.rm = TRUE))
})

test_that("output is bounded between 0 and 1 when normalized", {
  res <- wass2s_class_entropy(
    p_below  = c(1, 1/3, 0.7, 0.2),
    p_normal = c(0, 1/3, 0.2, 0.5),
    p_above  = c(0, 1/3, 0.1, 0.3),
    normalize = TRUE
  )

  expect_true(all(res >= 0, na.rm = TRUE))
  expect_true(all(res <= 1 + 1e-12, na.rm = TRUE))
})
