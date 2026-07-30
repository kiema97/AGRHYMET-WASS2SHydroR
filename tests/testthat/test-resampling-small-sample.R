test_that("small-sample resampling plan uses conservative training windows", {
  plan <- wass2s_resampling_plan(n = 12, strategy = "auto")

  expect_equal(plan$strategy, "small_sample")
  expect_gte(plan$initial, 8)
  expect_gte(plan$assess, 1)
  expect_lte(plan$initial + plan$assess, 12)
  expect_gte(plan$n_splits, 1)
})

test_that("rolling CV auto strategy works for short hydrological records", {
  df <- tibble::tibble(YYYY = 2001:2012, Q = seq_len(12))
  rs <- wass2s_rolling_cv(df, strategy = "auto", n_splits = 4)

  expect_s3_class(rs, "rset")
  expect_gte(length(rs$splits), 1)
  first_analysis <- rsample::analysis(rs$splits[[1]])
  expect_gte(nrow(first_analysis), 8)
})
