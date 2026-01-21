.make_results_std <- function(dates, scores, products = NULL, na_frac = 0) {
  if (is.null(products)) products <- paste0("P", seq_along(scores))
  stopifnot(length(scores) == length(products))

  purrr::map2(products, scores, ~{
    pred <- seq_along(dates) * 1.0
    if (na_frac > 0) {
      set.seed(1)
      idx <- sample(seq_along(pred), size = floor(length(pred) * na_frac))
      pred[idx] <- NA_real_
    }
    list(
      product = .x,
      score = .y,
      preds = tibble::tibble(YYYY = dates, pred = pred)
    )
  })
}

test_that("fuse_products_predictions selects topK and produces full timeline", {
  dates <- as.integer(c(20000101, 20010101, 20020101, 20030101))
  results <- .make_results_std(dates, scores = c(0.6, 0.2, 0.9))

  out <- fuse_products_predictions(
    results = results,
    dates_all = dates,
    topK = 2,
    min_score = 0.0,
    use_sub_fuser = FALSE,
    target_positive = FALSE,
    quiet = TRUE,
    verbose = FALSE
  )

  expect_type(out, "list")
  expect_true(all(c("fused", "leaderboard_products") %in% names(out)))
  expect_true(all(out$fused$YYYY == dates))
  expect_true(any(!is.na(out$fused$pred_fused)))
  expect_true(nrow(out$leaderboard_products) >= 2)
})

test_that("fuse_products_predictions respects min_score by zeroing low-score weights", {
  dates <- as.integer(c(20000101, 20010101, 20020101))
  results <- .make_results_std(dates, scores = c(0.1, 0.9))

  out <- fuse_products_predictions(
    results = results,
    dates_all = dates,
    topK = 2,
    min_score = 0.5,
    use_sub_fuser = FALSE,
    target_positive = FALSE,
    quiet = TRUE,
    verbose = FALSE
  )

  lb <- out$leaderboard_products
  # le produit score=0.1 doit avoir weight=0 si retenu dans topK ou sinon pas retenu
  if ("weight" %in% names(lb)) {
    expect_true(any(lb$weight == 0))
    expect_true(any(lb$weight > 0))
  }
})

test_that("fuse_products_predictions returns NA if all weights are zero", {
  dates <- as.integer(c(20000101, 20010101))
  results <- .make_results_std(dates, scores = c(-1, -2))

  out <- fuse_products_predictions(
    results = results,
    dates_all = dates,
    topK = 2,
    min_score = 0.5,
    use_sub_fuser = FALSE,
    quiet = TRUE,
    verbose = FALSE
  )

  expect_true(all(is.na(out$fused$pred_fused)))
})

test_that("fuse_products_predictions does not become all-NA with partial NA preds (simple fuser)", {
  dates <- as.integer(c(20000101, 20010101, 20020101, 20030101))
  results <- .make_results_std(dates, scores = c(0.8, 0.7), na_frac = 0.5)

  out <- fuse_products_predictions(
    results = results,
    dates_all = dates,
    topK = 2,
    min_score = 0,
    use_sub_fuser = FALSE,
    quiet = TRUE,
    verbose = FALSE
  )

  expect_true(sum(!is.na(out$fused$pred_fused)) > 0)
})
