test_that("skill matrix data accepts run reports", {
  report <- list(
    summary = tibble::tibble(
      id = c("basin_a", "basin_b"),
      approach = "STAT",
      selected_fusion_method = c("best", "weighted_mean"),
      test_kge = c(0.2, 0.7),
      test_rmse = c(20, 10),
      probabilistic_rpss = c(-0.1, 0.4),
      overfit_flag = c(TRUE, FALSE)
    )
  )
  class(report) <- c("wass2s_run_report", "list")

  dat <- .wass2s_skill_matrix_data(report, row_col = "id")

  expect_s3_class(dat, "tbl_df")
  expect_true(all(c("metric", "row_label", "value", "score") %in% names(dat)))
  expect_true(all(dat$score >= 0 & dat$score <= 1 | is.na(dat$score)))
  rmse <- dat[dat$metric == "test_rmse", ]
  expect_gt(rmse$score[rmse$row_label == "basin_b"], rmse$score[rmse$row_label == "basin_a"])
})

test_that("wass2s_plot_skill_matrix returns a ggplot", {
  skip_if_not_installed("ggplot2")

  df <- tibble::tibble(
    model = c("rf", "glmnet"),
    approach = c("ML", "ML"),
    test_kge = c(0.3, 0.6),
    test_rmse = c(30, 20),
    probabilistic_accuracy = c(0.4, 0.7)
  )

  p <- wass2s_plot_skill_matrix(df, row_col = "model", group_col = NULL)

  expect_s3_class(p, "ggplot")
})
