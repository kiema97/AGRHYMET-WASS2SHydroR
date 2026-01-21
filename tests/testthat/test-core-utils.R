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

test_that("fuse_topk computes weighted average per date", {
  df <- tibble::tibble(
    YYYY = c(20000101, 20000101, 20010101),
    pred = c(1, 3, 5),
    w    = c(0.25, 0.75, 1)
  )
  out <- fuse_topk(df)
  expect_equal(
    out$pred_fused[out$YYYY == 20000101],
    (0.25 * 1 + 0.75 * 3) / (0.25 + 0.75)
  )
})

test_that("fuse_topk handles missing predictions by renormalizing weights", {
  df <- tibble::tibble(
    YYYY = c(20000101, 20000101, 20000101),
    pred = c(1, NA, 3),
    w    = c(0.25, 0.50, 0.75)
  )
  out <- fuse_topk(df)
  exp <- (0.25 * 1 + 0.75 * 3) / (0.25 + 0.75)
  expect_equal(out$pred_fused[out$YYYY == 20000101], exp)
})

test_that("fuse_topk returns NA if all preds are NA for a date", {
  df <- tibble::tibble(
    YYYY = c(20000101, 20000101),
    pred = c(NA, NA),
    w    = c(0.5, 0.5)
  )
  out <- fuse_topk(df)
  expect_true(is.na(out$pred_fused[out$YYYY == 20000101]))
})

test_that("fuse_topk returns NA if sum of weights is zero for a date", {
  df <- tibble::tibble(
    YYYY = c(20000101, 20000101),
    pred = c(1, 3),
    w    = c(0, 0)
  )
  out <- fuse_topk(df)
  expect_true(is.na(out$pred_fused[out$YYYY == 20000101]))
})

test_that("fuse_topk accepts year-like YYYY and converts to YYYYMMDD", {
  df <- tibble::tibble(
    YYYY = c(2000, 2000),
    pred = c(1, 3),
    w    = c(0.25, 0.75)
  )
  out <- fuse_topk(df)

  # Expect one aggregated row
  expect_equal(nrow(out), 1)

  # Expect YYYY to look like YYYYMMDD
  expect_true(out$YYYY[1] >= 19000101 && out$YYYY[1] <= 25001231)

  # Expected weighted mean
  expect_equal(out$pred_fused[1], (0.25 * 1 + 0.75 * 3) / (0.25 + 0.75))
})


test_that("safe_full_join_preds returns empty tibble on empty input", {
  out <- safe_full_join_preds(list())
  expect_s3_class(out, "tbl_df")
  expect_equal(nrow(out), 0)
})

test_that("safe_full_join_preds full-joins by YYYY and renames columns", {
  lst <- list(
    A = tibble::tibble(YYYY = c(20000101, 20010101), pred = c(1, 2)),
    B = tibble::tibble(YYYY = c(20010101, 20020101), pred = c(10, 20))
  )
  out <- safe_full_join_preds(lst)

  expect_true(all(c("YYYY", "A", "B") %in% names(out)))
  expect_equal(sort(out$YYYY), c(20000101, 20010101, 20020101))

  expect_equal(out$A[out$YYYY == 20000101], 1)
  expect_equal(out$A[out$YYYY == 20010101], 2)
  expect_true(is.na(out$A[out$YYYY == 20020101]))

  expect_true(is.na(out$B[out$YYYY == 20000101]))
  expect_equal(out$B[out$YYYY == 20010101], 10)
  expect_equal(out$B[out$YYYY == 20020101], 20)
})

test_that("safe_full_join_preds drops NULL elements via compact()", {
  lst <- list(
    A = tibble::tibble(YYYY = c(20000101), pred = c(1)),
    B = NULL
  )
  out <- safe_full_join_preds(lst)
  expect_true(all(c("YYYY", "A") %in% names(out)))
  expect_false("B" %in% names(out))
})


test_that("get_any_Q returns empty tibble if no product has required cols", {
  data_by_product <- list(
    P1 = tibble::tibble(HYBAS_ID = 1, X = 1),
    P2 = tibble::tibble(HYBAS_ID = 1, Y = 2)
  )
  out <- get_any_Q(data_by_product, basin_id = 1, basin_col = "HYBAS_ID")
  expect_equal(nrow(out), 0)
})

test_that("get_any_Q chooses product with most non-missing Q", {
  data_by_product <- list(
    P1 = tibble::tibble(HYBAS_ID = c(1,1,1), YYYY = c(20000101,20010101,20020101), Q = c(NA, 2, NA)),
    P2 = tibble::tibble(HYBAS_ID = c(1,1,1), YYYY = c(20000101,20010101,20020101), Q = c(1, 2, 3))
  )
  out <- get_any_Q(data_by_product, basin_id = 1, basin_col = "HYBAS_ID")
  expect_equal(nrow(out), 3)
  expect_equal(sum(!is.na(out$Q)), 3)  # should pick P2
})

test_that("get_any_Q returns distinct YYYY", {
  data_by_product <- list(
    P1 = tibble::tibble(HYBAS_ID = c(1,1), YYYY = c(20000101,20000101), Q = c(1,3))
  )
  out <- get_any_Q(data_by_product, basin_id = 1, basin_col = "HYBAS_ID")
  expect_equal(length(out$YYYY), length(unique(out$YYYY)))
})







# test_that("weight_from_kge handles degenerate cases", {
#   w1 <- weight_from_kge(c(0.2, 0.2, 0.2))
#   expect_equal(sum(w1), 1, tolerance = 1e-8)
#   expect_true(all(w1 == 1/3))
# })

# test_that("fuse_topk computes weighted average per year", {
#   df <- tibble::tibble(YYYY=c(2000,2000,2001), pred=c(1,3,5), w=c(0.25,0.75,1))
#   out <- fuse_topk(df)
#   expect_equal(out$pred_fused[out$YYYY==2000], (0.25*1 + 0.75*3)/(0.25+0.75))
# })
#
# test_that("fuse_topk handles missing predictions by renormalizing weights", {
#   df <- tibble::tibble(
#     YYYY = c(2000, 2000, 2000),
#     pred = c(1, NA, 3),
#     w    = c(0.25, 0.50, 0.75)
#   )
#   out <- fuse_topk(df)
#   exp <- (0.25*1 + 0.75*3) / (0.25 + 0.75)
#   expect_equal(out$pred_fused[out$YYYY==2000], exp)
# })
#
# test_that("fuse_topk returns NA if all preds are NA for a year", {
#   df <- tibble::tibble(
#     YYYY = c(2000, 2000),
#     pred = c(NA, NA),
#     w    = c(0.5, 0.5)
#   )
#   out <- fuse_topk(df)
#   expect_true(is.na(out$pred_fused[out$YYYY==2000]))
# })
#
# test_that("fuse_topk returns NA if sum of weights is zero for a year", {
#   df <- tibble::tibble(
#     YYYY = c(2000, 2000),
#     pred = c(1, 3),
#     w    = c(0, 0)
#   )
#   out <- fuse_topk(df)
#   expect_true(is.na(out$pred_fused[out$YYYY==2000]))
# })

