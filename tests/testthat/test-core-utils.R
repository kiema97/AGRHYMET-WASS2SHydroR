test_that("make_rolling works on short series", {
  df <- tibble::tibble(YYYY = 2018:2020, Q = rnorm(3))
  rs <- make_rolling(df)
  expect_s3_class(rs, "rset")
  expect_true(length(rs$splits) >= 1)
})

test_that("select_predictors respects regex and excludes", {
  df <- tibble::tibble(YYYY=2000:2002, Q=1:3, pt_1=1, px=2)
  sel <- select_predictors(df, pattern="^pt_")
  expect_equal(sel, "pt_1")
})

# test_that("weight_from_kge handles degenerate cases", {
#   w1 <- weight_from_kge(c(0.2, 0.2, 0.2))
#   expect_equal(sum(w1), 1, tolerance = 1e-8)
#   expect_true(all(w1 == 1/3))
# })

test_that("fuse_topk computes weighted average per year", {
  df <- tibble::tibble(YYYY=c(2000,2000,2001), pred=c(1,3,5), w=c(0.25,0.75,1))
  out <- fuse_topk(df)
  expect_equal(out$pred_fused[ out$YYYY==2000 ], (0.25*1 + 0.75*3)/2)
})
