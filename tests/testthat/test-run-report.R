test_that("fusion outputs include probabilities and probabilistic skill", {
  df <- tibble::tibble(
    YYYY = as.integer(paste0(2001:2012, "0101")),
    Q = 1:12,
    strong = 1:12 + c(rep(0, 10), 1, -1),
    weak = rep(6, 12)
  )

  res <- .wass2s_fuse_predictions(
    fused_models = df,
    basin_id = 1,
    prediction_years = c(2011, 2012),
    fusion_method = "auto",
    quiet = TRUE
  )

  expect_s3_class(res$probabilities, "tbl_df")
  expect_true(all(c("p_below", "p_normal", "p_above", "class_hat", "distribution") %in% names(res$probabilities)))
  expect_true(all(abs(res$probabilities$p_below + res$probabilities$p_normal + res$probabilities$p_above - 1) < 1e-10))
  expect_s3_class(res$probabilistic_skill, "tbl_df")
  expect_true(all(c("rpss", "accuracy") %in% names(res$probabilistic_skill)))
  expect_equal(res$fusion_report$probabilistic_skill, res$probabilistic_skill)
})

test_that("wass2s_run_report summarizes a basin result", {
  obj <- list(
    scores_train = tibble::tibble(HYBAS_ID = 1, kge = 0.7, rmse = 2),
    scores_test = tibble::tibble(HYBAS_ID = 1, kge = 0.4, rmse = 3),
    fusion_method = "weighted_mean",
    requested_fusion_method = "auto",
    best_model = NA_character_,
    diagnostics = list(
      split = tibble::tibble(n_train = 8L, n_test = 2L, prediction_years = "2011 - 2012", leakage_risk = "low"),
      performance_gap = tibble::tibble(
        train_kge = 0.7, test_kge = 0.4, train_rmse = 2, test_rmse = 3,
        train_test_kge_gap = 0.3, test_train_rmse_ratio = 1.5, overfit_flag = FALSE
      ),
      decision = tibble::tibble(
        requested_fusion_method = "auto",
        selected_fusion_method = "weighted_mean",
        selection_reason = "auto_best_cv_rmse_train_period",
        used_test_for_selection = FALSE
      ),
      warnings = character()
    ),
    probabilistic_skill = tibble::tibble(
      n = 2L,
      brier_multicategory = 0.2,
      rps = 0.1,
      rps_climatology = 0.2,
      rpss = 0.5,
      accuracy = 1
    )
  )

  report <- wass2s_run_report(obj, approach = "ML")

  expect_s3_class(report, "wass2s_run_report")
  expect_s3_class(report$summary, "tbl_df")
  expect_equal(report$summary$approach, "ML")
  expect_equal(report$summary$selected_fusion_method, "weighted_mean")
  expect_false(report$summary$used_test_for_selection)
  expect_equal(report$summary$probabilistic_rpss, 0.5)
})

test_that("wass2s_run_report summarizes HYPE execution tables", {
  hype <- tibble::tibble(
    resultdir = c("ecmwf", "ukmo"),
    success = c(TRUE, FALSE),
    exit_status = c(0L, 1L),
    duration_sec = c(12, 15),
    result_dir = c("run/ecmwf", "run/ukmo"),
    n_outputs = c(1L, 0L)
  )

  report <- wass2s_run_report(hype, approach = "HYDRO")

  expect_s3_class(report, "wass2s_run_report")
  expect_equal(nrow(report$summary), 2L)
  expect_equal(report$summary$approach[[1]], "HYDRO")
  expect_true(any(report$summary$success %in% FALSE))
  expect_true(grepl("Inspect failed HYPE runs", report$recommendations))
})
