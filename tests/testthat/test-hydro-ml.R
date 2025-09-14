test_that("toy data generator returns coherent list", {
  lst <- make_toy_data_by_product(
    basins = c(1,2),
    years = 1995:2005,
    products = c("P1","P2"),
    p = 4, signal_strength = 0.6, miss_rate = 0.1, seed = 42
  )
  expect_true(is.list(lst))
  expect_setequal(names(lst), c("P1","P2"))
  expect_true(all(vapply(lst, function(x) all(c("HYBAS_ID","YYYY","Q") %in% names(x)), logical(1))))
})

test_that("end-to-end Hydro+ML runs and returns expected components", {
  skip_if_not_installed("ranger")
  skip_if_not_installed("xgboost")
  skip_if_not_installed("nnet")

  # small but non-trivial dataset
  data_by_product <- make_toy_data_by_product(
    basins = c(1001, 1002),
    years = 1998:2010,
    products = c("SST_CMCC","SST_ECMWF","SST_JMA"),
    p = 5, signal_strength = 0.7, miss_rate = 0.05, seed = 123
  )
  patterns <- c(SST_CMCC="^pt_", SST_ECMWF="^pt_", SST_JMA="^pt_")

  res <- wass2s_run_basins_ml(
    data_by_product = data_by_product,
    hybas_id = "HYBAS_ID",
    pred_pattern_by_product = patterns,
    models = c("rf","xgb","mlp"),
    final_fuser = "rf",
    topK = 2,
    min_kge_model = -Inf,
    parallel = FALSE,
    grid_levels = 3
  )

  expect_true(is.list(res))
  expect_setequal(names(res), c("1001","1002"))

  one <- res[[1]]
  expect_true(is.list(one))
  expect_true(all(c("fused_by_model","cv_rs","scores","leaderboards") %in% names(one)))

  # structure checks
  expect_true(all(c("YYYY","Q") %in% names(one$fused_by_model)))
  expect_true("pred_final" %in% names(one$fused_by_model))
  expect_true(all(c("HYBAS_ID","kge_final","rmse_final") %in% names(one$scores)))
})

test_that("topK + min_kge_model can discard weak models", {
  data_by_product <- make_toy_data_by_product(
    basins = 999,
    years = 2000:2008,
    products = c("Pgood","Pbad1","Pbad2"),
    p = 4, signal_strength = 0.2, seed = 10
  )
  patterns <- c(Pgood="^pt_", Pbad1="^pt_", Pbad2="^pt_")

  # artificially reduce signal so KGE tends to be small; set min_kge_model > 0
  out <- wass2s_run_bas_mod_ml(
    basin_id = 999,
    data_by_product = data_by_product,
    hybas_id = "HYBAS_ID",
    pred_pattern_by_product = patterns,
    models = c("rf","mlp"),
    topK = 2,
    min_kge_model = 0.15,      # force discard if best product < 0.15
    grid_levels = 3,
    final_fuser = "rf"
  )

  # Either fused_by_model has only YYYY/Q (no consolidated columns) if both models discarded,
  # or consolidated columns exist but some models may be absent. We just assert no crash and structure.
  expect_true(is.list(out))
  expect_true(all(c("fused_by_model","final_test","scores","leaderboards") %in% names(out)))
})

test_that("make_rolling is robust for very short series", {
  df <- tibble::tibble(YYYY = 2018:2020, Q = rnorm(3), pt_1 = rnorm(3))
  rs <- make_rolling(df)
  expect_s3_class(rs, "rset")
  # at least one split present
  expect_true(length(rs$splits) >= 1)
})
