.wass2s_fit_cv_diagnostics <- function(truth,
                                       estimate,
                                       cv_kge,
                                       cv_rmse,
                                       max_fit_cv_kge_gap = 0.35,
                                       max_cv_fit_rmse_ratio = 2,
                                       min_cv_kge = -0.05) {
  ok <- is.finite(truth) & is.finite(estimate)
  fit_kge <- if (sum(ok) >= 2L) wass2s_kge(truth[ok], estimate[ok]) else NA_real_
  fit_rmse <- if (sum(ok) > 0L) wass2s_rmse(truth[ok], estimate[ok]) else NA_real_

  kge_gap <- if (is.finite(fit_kge) && is.finite(cv_kge)) fit_kge - cv_kge else NA_real_
  cv_fit_rmse_ratio <- if (is.finite(cv_rmse) && is.finite(fit_rmse) && fit_rmse > 0) {
    cv_rmse / fit_rmse
  } else {
    NA_real_
  }

  reasons <- character()
  if (is.finite(kge_gap) && kge_gap > max_fit_cv_kge_gap) {
    reasons <- c(reasons, "fit_cv_kge_gap")
  }
  if (is.finite(cv_fit_rmse_ratio) && cv_fit_rmse_ratio > max_cv_fit_rmse_ratio) {
    reasons <- c(reasons, "cv_fit_rmse_ratio")
  }
  if (is.finite(min_cv_kge) && (!is.finite(cv_kge) || cv_kge < min_cv_kge)) {
    reasons <- c(reasons, "cv_kge_below_min")
  }
  overfit_flag <- length(reasons) > 0L

  tibble::tibble(
    fit_kge = fit_kge,
    fit_rmse = fit_rmse,
    cv_kge = cv_kge,
    cv_rmse = cv_rmse,
    fit_cv_kge_gap = kge_gap,
    cv_fit_rmse_ratio = cv_fit_rmse_ratio,
    overfit_flag = overfit_flag,
    generalization_ok = !overfit_flag,
    guard_reason = if (overfit_flag) paste(reasons, collapse = ";") else "accepted"
  )
}

.wass2s_fusion_generalization_diagnostics <- function(train_scores,
                                                      test_scores,
                                                      n_train,
                                                      n_test,
                                                      requested_fusion_method,
                                                      selected_fusion_method,
                                                      selection_reason,
                                                      prediction_years = NULL,
                                                      candidate_cv_scores = NULL,
                                                      max_train_test_kge_gap = 0.50,
                                                      max_test_train_rmse_ratio = 2.50) {
  train_kge <- train_scores$kge[[1]] %||% NA_real_
  test_kge <- test_scores$kge[[1]] %||% NA_real_
  train_rmse <- train_scores$rmse[[1]] %||% NA_real_
  test_rmse <- test_scores$rmse[[1]] %||% NA_real_

  kge_gap <- if (is.finite(train_kge) && is.finite(test_kge)) train_kge - test_kge else NA_real_
  rmse_ratio <- if (is.finite(train_rmse) && is.finite(test_rmse) && train_rmse > 0) {
    test_rmse / train_rmse
  } else {
    NA_real_
  }

  overfit_flag <- FALSE
  if (is.finite(kge_gap) && kge_gap > max_train_test_kge_gap) overfit_flag <- TRUE
  if (is.finite(rmse_ratio) && rmse_ratio > max_test_train_rmse_ratio) overfit_flag <- TRUE

  leakage_risk <- "low"
  if (is.null(prediction_years)) {
    leakage_risk <- "medium_no_explicit_prediction_years"
  }
  if (n_test < 1L) {
    leakage_risk <- "high_no_test_rows"
  }

  warnings <- character()
  if (is.null(prediction_years)) {
    warnings <- c(warnings, "No explicit prediction_years were provided; the last row was used as test.")
  }
  if (n_test < 1L) {
    warnings <- c(warnings, "No test rows are available for out-of-sample scoring.")
  }
  if (isTRUE(overfit_flag)) {
    warnings <- c(warnings, "Large train-test performance gap detected.")
  }
  if (is.null(candidate_cv_scores) || nrow(candidate_cv_scores) == 0L) {
    warnings <- c(warnings, "Fusion candidate selection used apparent training scores because CV scores were unavailable.")
  }

  list(
    split = tibble::tibble(
      n_train = n_train,
      n_test = n_test,
      prediction_years = paste(prediction_years %||% NA, collapse = " - "),
      leakage_risk = leakage_risk
    ),
    performance_gap = tibble::tibble(
      train_kge = train_kge,
      test_kge = test_kge,
      train_rmse = train_rmse,
      test_rmse = test_rmse,
      train_test_kge_gap = kge_gap,
      test_train_rmse_ratio = rmse_ratio,
      overfit_flag = overfit_flag
    ),
    decision = tibble::tibble(
      requested_fusion_method = requested_fusion_method,
      selected_fusion_method = selected_fusion_method,
      selection_reason = selection_reason,
      used_test_for_selection = FALSE
    ),
    warnings = warnings
  )
}

.wass2s_fusion_probabilities <- function(fused_models,
                                         df_tr,
                                         df_te,
                                         target = "Q",
                                         date_col = "YYYY",
                                         pred_col = "pred_final") {
  empty <- list(
    probabilities = tibble::tibble(),
    probabilistic_skill = tibble::tibble(
      n = 0L,
      brier_multicategory = NA_real_,
      rps = NA_real_,
      rps_climatology = NA_real_,
      rpss = NA_real_,
      accuracy = NA_real_
    )
  )

  required <- c(date_col, target, pred_col)
  if (!all(required %in% names(fused_models)) || !all(required %in% names(df_tr))) {
    return(empty)
  }

  q_hist <- df_tr[[target]]
  residuals <- df_tr[[target]] - df_tr[[pred_col]]
  if (sum(is.finite(q_hist)) < 3L || sum(is.finite(fused_models[[pred_col]])) == 0L) {
    return(empty)
  }

  probs <- tryCatch({
    in_df <- fused_models[, c(date_col, target, pred_col), drop = FALSE]
    names(in_df)[names(in_df) == date_col] <- "YYYY"
    names(in_df)[names(in_df) == pred_col] <- "pred"
    out <- wass2s_tercile_from_forecast(
      df = in_df[, c("YYYY", "pred"), drop = FALSE],
      q_hist = q_hist,
      residuals = residuals,
      distribution = "auto"
    )
    out[[target]] <- in_df[[target]]
    out
  }, error = function(e) tibble::tibble())

  if (nrow(probs) == 0L || !all(c("p_below", "p_normal", "p_above") %in% names(probs))) {
    return(empty)
  }

  skill <- tryCatch({
    test_dates <- df_te[[date_col]]
    probs_te <- probs[probs$YYYY %in% test_dates, , drop = FALSE]
    if (nrow(probs_te) == 0L || !target %in% names(probs_te)) {
      empty$probabilistic_skill
    } else {
      wass2s_probabilistic_skill(
        truth = probs_te[[target]],
        probs = probs_te[, c("p_below", "p_normal", "p_above"), drop = FALSE],
        thresholds = wass2s_tercile_thresholds(q_hist)
      )
    }
  }, error = function(e) empty$probabilistic_skill)

  list(probabilities = probs, probabilistic_skill = skill)
}
