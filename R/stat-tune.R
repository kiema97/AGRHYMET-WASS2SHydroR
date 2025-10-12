#' Tune and refit a single product/model, select configuration by KGE
#'
#' Performs rolling-origin resampling, tunes hyperparameters (PCR threshold
#' or glmnet penalty), collects predictions, computes KGE per split, and
#' selects the configuration with best mean KGE (falls back to RMSE when needed).
#'
#' @param df_basin_product Data frame for a single basin/product with columns
#'   `YYYY`, `Q`, and predictor columns.
#' @param predictors Character vector of predictor column names.
#' @param model One of `"pcr"`, `"ridge"`, `"lasso"`.
#' @param prediction_years Optional numeric vector of length 2 giving the
#'   start and end years for a holdout prediction period. These years
#'   are excluded from training and predictions are generated after fitting.
#' @param target_positive Logical; if TRUE, force negative predictions to zero.
#' @param resamples Optional \code{rsample::rset} object for resampling.
#'   If \code{NULL}, a rolling-origin resampling is created via
#'   \code{make_rolling()}.
#' @param pretrained_wflow Optional \code{workflows::workflow} object.
#'   If supplied, tuning is skipped and the workflow is fitted directly.
#' @param grid Optional tibble of tuning parameters. If supplied, this grid
#'   is used instead of the default grid for the specified model.
#' @param init_frac Fraction of rows used for the initial training window.
#' @param assess_frac Fraction of rows used for the assessment window.
#' @param n_splits Optional integer, desired number of resamples (splits).
#' @param cumulative Logical; passed to \code{rsample::rolling_origin()}.
#' @param quiet Logical; if \code{FALSE}, emits informative messages.
#' @param allow_par A logical to allow parallel processing (if a parallel backend is registered).
#' @param verbose_tune A logical for logging results (other than warnings and errors, which are always shown) as they are generated during training in a single R process.
#' @param ... Others parameters passed to \code{tune::control_grid()}
#' @return A list with:
#'   \itemize{
#'     \item `kge_cv_mean` (numeric): mean KGE across splits for the best config.
#'     \item `preds` (tibble): columns `YYYY`, `pred` from the refitted model.
#'     \item `leaderboard_cfg` (tibble): per-config mean KGE, descending.
#'   }
#'
#' @examples
#' \dontrun{
#' res <- wass2s_tune_pred_stat(df, c("pt_1","pt_2","pt_3"), "ridge")
#' }
#' @seealso [make_recipe()], [make_rolling()], [wf_pcr()], [wf_ridge()], [wf_lasso()]
#' @export
wass2s_tune_pred_stat <- function(df_basin_product, predictors,
                                  model = c("pcr", "ridge", "lasso"),
                                  prediction_years = NULL,
                                  target_positive = FALSE,
                                  resamples = NULL,
                                  pretrained_wflow = NULL,
                                  grid = NULL,
                                  init_frac = 0.60,
                                  assess_frac = 0.20,
                                  n_splits = NULL,
                                  cumulative = TRUE,
                                  quiet = TRUE,
                                  allow_par = TRUE,
                                  verbose_tune = TRUE,
                                  ...) {

  # Input validation
  model <- match.arg(model)
  required_cols <- c("YYYY", "Q")
  missing_cols <- setdiff(required_cols, names(df_basin_product))

  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "),
         call. = FALSE)
  }

  # Handle prediction years
  holdout_data <- NULL
  if (!is.null(prediction_years)) {
    if (length(prediction_years) != 2) {
      stop("prediction_years must be length 2 (start, end).", call. = FALSE)
    }
    prediction_years[2] <- min(prediction_years[2],max(df_basin_product$YYYY))
    # Extract holdout data
    holdout_mask <- df_basin_product$YYYY >= prediction_years[1] &
      df_basin_product$YYYY <= prediction_years[2]
    holdout_data <- df_basin_product[holdout_mask, ]
    df_basin_product <- df_basin_product[!holdout_mask, ]

    if (!quiet) {
      message("Using ", nrow(holdout_data), " years for holdout prediction: ",
              prediction_years[1], " to ", prediction_years[2])
    }
  }

  # Handle pretrained workflow case
  if (!is.null(pretrained_wflow)) {
    if (!quiet) message("Using pretrained workflow, skipping tuning.")

    # Fit the pretrained workflow
    fit_final <- tryCatch({
      parsnip::fit(pretrained_wflow, df_basin_product)
    }, error = function(e) {
      if (!quiet) message("Error fitting pretrained workflow: ", e$message)
      return(NULL)
    })

    if (is.null(fit_final)) {
      # Create empty predictions for all years (training + holdout)
      all_years <- unique(c(df_basin_product$YYYY, if (!is.null(holdout_data)) holdout_data$YYYY else NULL))
      preds_empty <- tibble::tibble(YYYY = all_years, pred = NA_real_)

      return(list(
        kge_cv_mean = NA_real_,
        preds = preds_empty,
        leaderboard_cfg = tibble::tibble(.config = character(), kge_mean = numeric())
      ))
    }

    # Generate predictions for all data (training + holdout)
    all_data <- if (!is.null(holdout_data)) {
      dplyr::bind_rows(df_basin_product, holdout_data)
    } else {
      df_basin_product
    }

    preds_final <- tryCatch({
      pred_values <- stats::predict(fit_final, all_data) %>%
        dplyr::pull(.pred)

      # Apply target_positive if requested
      if (target_positive) {
        pred_values <- pmax(pred_values, 0)
      }

      tibble::tibble(
        YYYY = all_data$YYYY,
        pred = pred_values
      )
    }, error = function(e) {
      if (!quiet) message("Error generating final predictions: ", e$message)
      # Create empty predictions for all years
      all_years <- unique(c(df_basin_product$YYYY, if (!is.null(holdout_data)) holdout_data$YYYY else NULL))
      tibble::tibble(YYYY = all_years, pred = NA_real_)
    })

    return(list(
      kge_cv_mean = NA_real_,
      preds = preds_final,
      leaderboard_cfg = tibble::tibble(.config = character(), kge_mean = numeric())
    ))
  }

  if (nrow(df_basin_product) < 8) {
    if (!quiet) message("Not enough rows to tune model for this basin/product.")
    # Create empty predictions for all years (training + holdout)
    all_years <- unique(c(df_basin_product$YYYY, if (!is.null(holdout_data)) holdout_data$YYYY else NULL))
    preds_empty <- tibble::tibble(YYYY = all_years, pred = NA_real_)

    return(list(
      kge_cv_mean = NA_real_,
      preds = preds_empty,
      leaderboard_cfg = tibble::tibble(.config = character(), kge_mean = numeric())
    ))
  }

  # Order and remove any grouping
  df_basin_product <- df_basin_product %>%
    dplyr::ungroup() %>%
    dplyr::arrange(YYYY)

  # Filter non-informative predictors
  predictors <- usable_predictors(df_basin_product, predictors)
  if (length(predictors) < 2L) {
    if (!quiet) message("Insufficient usable predictors after filtering.")
    # Create empty predictions for all years (training + holdout)
    all_years <- unique(c(df_basin_product$YYYY, if (!is.null(holdout_data)) holdout_data$YYYY else NULL))
    preds_empty <- tibble::tibble(YYYY = all_years, pred = NA_real_)

    return(list(
      kge_cv_mean = NA_real_,
      preds = preds_empty,
      leaderboard_cfg = tibble::tibble(.config = character(), kge_mean = numeric())
    ))
  }

  # Create recipe
  rec <- tryCatch({
    make_recipe(df_basin_product, predictors)
  }, error = function(e) {
    if (!quiet) message("Error creating recipe: ", e$message)
    return(NULL)
  })

  if (is.null(rec)) {
    # Create empty predictions for all years (training + holdout)
    all_years <- unique(c(df_basin_product$YYYY, if (!is.null(holdout_data)) holdout_data$YYYY else NULL))
    preds_empty <- tibble::tibble(YYYY = all_years, pred = NA_real_)

    return(list(
      kge_cv_mean = NA_real_,
      preds = preds_empty,
      leaderboard_cfg = tibble::tibble(.config = character(), kge_mean = numeric())
    ))
  }

  # Create or use provided resamples
  if (is.null(resamples)) {
    rs <- tryCatch({
      make_rolling(
        df_basin_product,
        year_col = "YYYY",
        init_frac = init_frac,
        assess_frac = assess_frac,
        n_splits = n_splits,
        cumulative = cumulative,
        quiet = quiet
      )
    }, error = function(e) {
      if (!quiet) message("Error creating resamples: ", e$message)
      return(NULL)
    })
  } else {
    rs <- resamples
    if (!inherits(rs, "rset")) {
      if (!quiet) message("Provided resamples is not a valid rset object.")
      return(list(
        kge_cv_mean = NA_real_,
        preds = tibble::tibble(YYYY = df_basin_product$YYYY, pred = NA_real_),
        leaderboard_cfg = tibble::tibble(.config = character(), kge_mean = numeric())
      ))
    }
  }

  if (is.null(rs) || length(rs$splits) < 2) {
    if (!quiet) message("Insufficient resamples created.")
    # Create empty predictions for all years (training + holdout)
    all_years <- unique(c(df_basin_product$YYYY, if (!is.null(holdout_data)) holdout_data$YYYY else NULL))
    preds_empty <- tibble::tibble(YYYY = all_years, pred = NA_real_)

    return(list(
      kge_cv_mean = NA_real_,
      preds = preds_empty,
      leaderboard_cfg = tibble::tibble(.config = character(), kge_mean = numeric())
    ))
  }

  # Create workflow based on model type
  model_functions <- list(
    pcr = list(wf = wf_pcr),
    ridge = list(wf = wf_ridge),
    lasso = list(wf = wf_lasso)
  )

  wf <- tryCatch({
    model_functions[[model]]$wf(rec)
  }, error = function(e) {
    if (!quiet) message("Error creating workflow: ", e$message)
    return(NULL)
  })

  if (is.null(wf)) {
    # Create empty predictions for all years (training + holdout)
    all_years <- unique(c(df_basin_product$YYYY, if (!is.null(holdout_data)) holdout_data$YYYY else NULL))
    preds_empty <- tibble::tibble(YYYY = all_years, pred = NA_real_)

    return(list(
      kge_cv_mean = NA_real_,
      preds = preds_empty,
      leaderboard_cfg = tibble::tibble(.config = character(), kge_mean = numeric())
    ))
  }

  # Use provided grid or create default grid
  if (is.null(grid)) {
    grid_functions <- list(
      pcr = grid_pcr,
      ridge = grid_glm,
      lasso = grid_glm
    )

    grid <- tryCatch({
      grid_functions[[model]]()
    }, error = function(e) {
      if (!quiet) message("Error creating grid: ", e$message)
      return(NULL)
    })
  }

  if (is.null(grid)) {
    # Create empty predictions for all years (training + holdout)
    all_years <- unique(c(df_basin_product$YYYY, if (!is.null(holdout_data)) holdout_data$YYYY else NULL))
    preds_empty <- tibble::tibble(YYYY = all_years, pred = NA_real_)

    return(list(
      kge_cv_mean = NA_real_,
      preds = preds_empty,
      leaderboard_cfg = tibble::tibble(.config = character(), kge_mean = numeric())
    ))
  }

  # Tune model
  ctrl <- tune::control_grid(save_pred = TRUE,
                             verbose = verbose_tune,
                             allow_par=allow_par,
                             parallel_over = "resamples",
                             ... )

  res <- tryCatch({
    tune::tune_grid(
      wf,
      resamples = rs,
      grid = grid,
      metrics = yardstick::metric_set(yardstick::rmse, yardstick::mae, yardstick::rsq),
      control = ctrl
    )
  }, error = function(e) {
    if (!quiet) message("Error during tuning: ", e$message)
    return(NULL)
  })

  if (is.null(res)) {
    # Create empty predictions for all years (training + holdout)
    all_years <- unique(c(df_basin_product$YYYY, if (!is.null(holdout_data)) holdout_data$YYYY else NULL))
    preds_empty <- tibble::tibble(YYYY = all_years, pred = NA_real_)

    return(list(
      kge_cv_mean = NA_real_,
      preds = preds_empty,
      leaderboard_cfg = tibble::tibble(.config = character(), kge_mean = numeric())
    ))
  }

  # Collect predictions
  preds <- tryCatch({
    tune::collect_predictions(res)
  }, error = function(e) {
    if (!quiet) message("Error collecting predictions: ", e$message)
    tibble::tibble()
  })

  if (nrow(preds) == 0L) {
    # Create empty predictions for all years (training + holdout)
    all_years <- unique(c(df_basin_product$YYYY, if (!is.null(holdout_data)) holdout_data$YYYY else NULL))
    preds_empty <- tibble::tibble(YYYY = all_years, pred = NA_real_)

    return(list(
      kge_cv_mean = NA_real_,
      preds = preds_empty,
      leaderboard_cfg = tibble::tibble(.config = character(), kge_mean = numeric())
    ))
  }

  # Calculate KGE by split and configuration
  kge_by_split <- tryCatch({
    preds %>%
      dplyr::group_by(id, .config) %>%
      dplyr::summarise(kge_split = kge_vec(Q, .pred), .groups = "drop")
  }, error = function(e) {
    if (!quiet) message("Error calculating KGE by split: ", e$message)
    tibble::tibble(id = character(), .config = character(), kge_split = numeric())
  })

  kge_by_cfg <- tryCatch({
    kge_by_split %>%
      dplyr::group_by(.config) %>%
      dplyr::summarise(kge_mean = mean(kge_split, na.rm = TRUE), .groups = "drop") %>%
      dplyr::arrange(dplyr::desc(kge_mean))
  }, error = function(e) {
    if (!quiet) message("Error calculating KGE by config: ", e$message)
    tibble::tibble(.config = character(), kge_mean = numeric())
  })

  # Select best configuration
  best_params <- tryCatch({
    if (nrow(kge_by_cfg) == 0L || all(is.na(kge_by_cfg$kge_mean))) {
      tune::select_best(res, metric = "rmse")
    } else {
      best_cfg <- kge_by_cfg$.config[1]
      cand <- tune::show_best(res, metric = "rmse", n = Inf)
      best_row <- cand %>% dplyr::filter(.config == best_cfg)
      if (nrow(best_row) == 0L) {
        tune::select_best(res, metric = "rmse")
      } else {
        best_row %>% dplyr::slice(1)
      }
    }
  }, error = function(e) {
    if (!quiet) message("Error selecting best parameters: ", e$message)
    NULL
  })

  if (is.null(best_params)) {
    # Create empty predictions for all years (training + holdout)
    all_years <- unique(c(df_basin_product$YYYY, if (!is.null(holdout_data)) holdout_data$YYYY else NULL))
    preds_empty <- tibble::tibble(YYYY = all_years, pred = NA_real_)

    return(list(
      kge_cv_mean = if (nrow(kge_by_cfg) > 0) kge_by_cfg$kge_mean[1] else NA_real_,
      preds = preds_empty,
      leaderboard_cfg = kge_by_cfg
    ))
  }

  # Final refit and predictions
  fit_final <- tryCatch({
    tune::finalize_workflow(wf, best_params) %>%
      parsnip::fit(df_basin_product)
  }, error = function(e) {
    if (!quiet) message("Error in final fitting: ", e$message)
    return(NULL)
  })

  if (is.null(fit_final)) {
    # Create empty predictions for all years (training + holdout)
    all_years <- unique(c(df_basin_product$YYYY, if (!is.null(holdout_data)) holdout_data$YYYY else NULL))
    preds_empty <- tibble::tibble(YYYY = all_years, pred = NA_real_)

    return(list(
      kge_cv_mean = if (nrow(kge_by_cfg) > 0) kge_by_cfg$kge_mean[1] else NA_real_,
      preds = preds_empty,
      leaderboard_cfg = kge_by_cfg
    ))
  }

  # Generate predictions for all data (training + holdout)
  all_data <- if (!is.null(holdout_data)) {
    dplyr::bind_rows(df_basin_product, holdout_data)
  } else {
    df_basin_product
  }

  preds_final <- tryCatch({
    pred_values <- stats::predict(fit_final, all_data) %>%
      dplyr::pull(.pred)

    # Apply target_positive if requested
    if (target_positive) {
      pred_values <- pmax(pred_values, 0)
    }

    tibble::tibble(
      YYYY = all_data$YYYY,
      pred = pred_values
    )
  }, error = function(e) {
    if (!quiet) message("Error generating final predictions: ", e$message)
    # Create empty predictions for all years
    all_years <- unique(c(df_basin_product$YYYY, if (!is.null(holdout_data)) holdout_data$YYYY else NULL))
    tibble::tibble(YYYY = all_years, pred = NA_real_)
  })

  # Return results
  list(
    kge_cv_mean = if (nrow(kge_by_cfg) > 0) kge_by_cfg$kge_mean[1] else NA_real_,
    preds = preds_final,
    leaderboard_cfg = kge_by_cfg
  )
}

