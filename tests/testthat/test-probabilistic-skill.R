test_that("tercile thresholds use climatological thirds", {
  th <- wass2s_tercile_thresholds(1:99)
  expect_named(th, c("t1", "t2"))
  expect_equal(unname(th), as.numeric(stats::quantile(1:99, c(1 / 3, 2 / 3), names = FALSE)))
})

test_that("probability normalization repairs invalid rows", {
  p <- tibble::tibble(
    p_below = c(2, NA, -1),
    p_normal = c(2, NA, -1),
    p_above = c(0, NA, -1)
  )
  out <- wass2s_normalize_probabilities(p)
  expect_true(all(abs(rowSums(out) - 1) < 1e-12))
  expect_equal(as.numeric(out[2, ]), c(1 / 3, 1 / 3, 1 / 3), tolerance = 1e-12)
  expect_equal(as.numeric(out[3, ]), c(1 / 3, 1 / 3, 1 / 3), tolerance = 1e-12)
})

test_that("tercile forecast wrapper returns coherent probabilities", {
  df <- tibble::tibble(YYYY = 2001:2003, pred = c(2, 50, 98))
  out <- wass2s_tercile_from_forecast(df, q_hist = 1:99, rmse = 5)
  expect_true(all(c("p_below", "p_normal", "p_above", "class_hat", "entropy") %in% names(out)))
  expect_true(all(abs(out$p_below + out$p_normal + out$p_above - 1) < 1e-10))
  expect_equal(out$class_hat[c(1, 3)], c("below", "above"))
})

test_that("probabilistic skill rewards sharp correct tercile forecasts", {
  truth <- c(1, 50, 99)
  probs <- tibble::tibble(
    p_below = c(0.9, 0.05, 0.05),
    p_normal = c(0.05, 0.9, 0.05),
    p_above = c(0.05, 0.05, 0.9)
  )
  skill <- wass2s_probabilistic_skill(truth, probs, thresholds = c(t1 = 33, t2 = 66))
  expect_equal(skill$n, 3)
  expect_gt(skill$rpss, 0)
  expect_equal(skill$accuracy, 1)
})

test_that("Student probabilities are heavier-tailed than Normal probabilities", {
  thresholds <- c(t1 = -1, t2 = 1)
  p_norm <- wass2s_class_probs_norm(mu = 0, sigma = 1, thresholds = thresholds)
  p_student <- wass2s_class_probs_student(mu = 0, sigma = 1, thresholds = thresholds, df = 4)

  expect_gt(p_student$p_below, p_norm$p_below)
  expect_gt(p_student$p_above, p_norm$p_above)
  expect_lt(p_student$p_normal, p_norm$p_normal)
})

test_that("auto distribution uses Student for short climatologies", {
  df <- tibble::tibble(YYYY = 2001:2002, pred = c(5, 15))
  out <- wass2s_tercile_from_forecast(
    df,
    q_hist = 1:12,
    rmse = 2,
    distribution = "auto"
  )

  expect_true("distribution" %in% names(out))
  expect_true(all(out$distribution == "student"))
  expect_true(all(abs(out$p_below + out$p_normal + out$p_above - 1) < 1e-10))
})
