#
min_analysis_n <- function(rset) {
  min(vapply(rset$splits, function(s) nrow(rsample::analysis(s)), integer(1)))
}

#' Tune and predict for one product and one ML model
#'
#' This function performs cross-validation tuning for a single
#' (basin, product) dataset using a specified machine learning model.
#' It ranks model configurations by KGE (computed from CV predictions),
#' refits the best configuration on the full dataset, and returns fitted
#' predictions for both training and optional holdout (prediction) years.
#'
#' @param df_basin_product A data frame containing at least the columns
#'   \code{YYYY}, \code{Q}, and predictor variables.
#' @param predictors Character vector of predictor names to use.
#' @param target Name of the target column (default: `"Q"`).
#' @param date_col Name of the date column (default: `"YYYY"`).
#' @param prediction_years Optional numeric vector of length 2 giving the
#'   start and end years for a holdout prediction period. These years
#'   are excluded from training and predictions are generated after fitting.
#' @param model One of \code{SUPPORTED_MODELS}, e.g. `"rf"`, `"xgb"`, `"mlp"`.
#' @param resamples Optional \code{rsample::rset} object for resampling.
#'   If \code{NULL}, a rolling-origin resampling is created via
#'   \code{make_rolling()}.
#' @param grid_levels Number of grid levels per parameter (default: 5).
#' @param seed Random seed for reproducibility.
#' @param pretrained_wflow Optional \code{workflows::workflow} object.
#'   If supplied, tuning is skipped and the workflow is fitted directly.
#' @param init_frac Fraction of rows used for the initial training window
#'   (default 0.60). A hard minimum of 8 rows is enforced when possible.
#' @param assess_frac Fraction of rows used for the assessment window
#'   (default 0.20). A hard minimum of 3 rows is enforced when possible.
#' @param n_splits Optional integer, desired number of resamples (splits).
#'   If \code{NULL} (default), every possible split is produced (\code{skip = 0}).
#' @param cumulative Logical; passed to \code{rsample::rolling_origin()}
#'   (default \code{TRUE}).
#' @param quiet Logical; if \code{FALSE}, emits informative messages when the
#'   requested \code{n_splits} cannot be reached (default \code{TRUE}).
#' @param target_positive Logical; if TRUE, force negative predictions to zero.
#' @param allow_par A logical to allow parallel processing (if a parallel backend is registered).
#' @param verbose_tune A logical for logging results (other than warnings and errors, which are always shown) as they are generated during training in a single R process.
#' @param max_na_frac Numeric in \eqn{[0, 1]}: maximum allowed fraction of missing
#'   values per column before stopping (default \code{0.20} = 20\%).
#' @param impute Character, one of \code{"median"}, \code{"mean"}, or \code{"none"}.
#'   If \code{"none"}, no imputation is performed after the guard (default \code{"median"}).
#' @param require_variance Logical; if \code{TRUE}, stop when a column has zero
#'   standard deviation after imputation (default \code{TRUE}).
#' @param min_data_required Minimum number of rows required to train.
#' @param ... Others parameters passed to \code{tune::control_grid()}
#' @return A list with the following elements:
#' \itemize{
#'   \item \code{kge_cv_mean} Mean KGE of the best configuration across CV splits.
#'   \item \code{preds} Tibble with columns \code{YYYY, pred} containing
#'     predictions from the final fit (training + optional holdout).
#'   \item \code{fit} Final fitted workflow object.
#'   \item \code{leaderboard_cfg} Tibble of model configurations ranked by KGE.
#'    \item \code{param_grid} Tibble of parameters used for model training.
#' }
#'
#' @details
#' - The target column is always standardized internally to \code{Q}
#'   and the date column to \code{YYYY} before modeling.
#' - If \code{prediction_years} is given, those years are removed from
#'   the training data and used as an out-of-sample prediction set.
#' - Model configurations are tuned using RMSE as the optimization metric
#'   but are ranked by KGE for reporting.
#'
#' @examples
#' \dontrun{
#' # Example with toy data
#' df <- tibble::tibble(
#'   YYYY = 1990:2000,
#'   Q = rnorm(11, 1000, 200),
#'   x1 = rnorm(11), x2 = rnorm(11)
#' )
#'
#' res <- wass2s_tune_pred_ml(
#'   df_basin_product = df,
#'   predictors = c("x1", "x2"),
#'   model = "rf",
#'   grid_levels = 3
#' )
#'
#' res$kge_cv_mean
#' res$preds
#' }
#'
#' @export
wass2s_tune_pred_ml <- function(
    df_basin_product,
    predictors,
    target = "Q",
    date_col = "YYYY",
    prediction_years = NULL,
    model = SUPPORTED_MODELS,
    resamples = NULL,
    grid_levels = 5,
    seed = 123,
    pretrained_wflow = NULL,
    init_frac   = 0.80,
    assess_frac = 0.20,
    n_splits    = 3,
    cumulative  = TRUE,
    quiet       = TRUE,
    target_positive = TRUE,
    allow_par = TRUE,
    verbose_tune = TRUE,
    max_na_frac =0.3,
    impute = "median",
    require_variance = TRUE,
    min_data_required = 10,
    ...
){
  set.seed(seed)
  model <- match.arg(model, SUPPORTED_MODELS)
  spec <- model_spec(model)

  # Input validation
  if (!target %in% names(df_basin_product)) {
    stop(glue::glue("wass2s_tune_pred_ml(): column {target} not found."), call. = FALSE)
  }
  if (!date_col %in% names(df_basin_product)) {
    stop(glue::glue("wass2s_tune_pred_ml(): column {date_col} not found."), call. = FALSE)
  }

  predictors <- intersect(predictors, setdiff(names(df_basin_product), c(date_col, target)))
  if (length(predictors) < 1L) {
    stop("wass2s_tune_pred_ml(): predictors empty after intersection.", call. = FALSE)
  }

  # Standardize column names
  df_basin_product <- dplyr::rename(df_basin_product,
                                    YYYY = !!date_col,
                                    Q = !!target)

  df_basin_product <- .sanitize_numeric_columns(
    df   = df_basin_product,
    cols = "Q",
    max_na_frac = max_na_frac,
    impute = impute,
    require_variance = require_variance
  )

  # holdout_data <- NULL
  # if (!is.null(prediction_years)) {
  #   if (length(prediction_years) != 2) {
  #     stop("prediction_years must be length 2 (start, end).", call. = FALSE)
  #   }
  #   holdout_data <- dplyr::filter(
  #     df_basin_product,
  #     YYYY >= prediction_years[1], YYYY <= prediction_years[2]
  #   )
  #   df_basin_product <- dplyr::filter(
  #     df_basin_product,
  #     !(YYYY >= prediction_years[1] & YYYY <= prediction_years[2])
  #   )
  # }
  if(!is.null(prediction_years) && length(prediction_years)==1 && prediction_years>0 ){
    prediction_years <- rep(prediction_years,2)
  }
  # Handle prediction years
  holdout_data <- NULL
  if (!is.null(prediction_years)) {
    if (length(prediction_years) != 2) {
      stop("prediction_years must be length 2 (start, end).", call. = FALSE)
    }
    holdout_mask <- df_basin_product$YYYY >= prediction_years[1] &
      df_basin_product$YYYY <= prediction_years[2]
    holdout_data <- df_basin_product[holdout_mask, ]
    df_basin_product <- df_basin_product[!holdout_mask, ]
  }

  # Check minimum data requirements
  if (nrow(df_basin_product) < min_data_required) {
    stop("Insufficient training data after removing prediction years. Need at least 10 observations.",
         call. = FALSE)
  }

  # Create recipe and model spec
  rec <- make_recipe(df_basin_product, predictors, target = "Q")


  if (is.null(resamples)) resamples <- make_rolling(df_basin_product,
                                                    year_col = "YYYY",
                                                    n_splits = n_splits,
                                                    init_frac = init_frac,
                                                    assess_frac=assess_frac,
                                                    cumulative = cumulative,
                                                    quiet = TRUE)

  # Check if we have enough splits
  if (length(resamples$splits) < 1 && is.null(pretrained_wflow)) {
    warning("Insufficient splits for proper cross-validation. Consider reducing n_splits or increasing init_frac/assess_frac.",
            call. = FALSE)
  }

  n_min <- min_analysis_n(resamples)
  grid  <- model_grid(model, p = min(15,length(predictors)),
                      levels = grid_levels, n_min = n_min)

  # Handle pretrained workflow case
  if (!is.null(pretrained_wflow)) {
    fitted <- parsnip::fit(pretrained_wflow, df_basin_product)
    preds_train <- predict(fitted, df_basin_product)$.pred
    preds_holdout <- if (!is.null(holdout_data)) predict(fitted, holdout_data)$.pred else NULL

    # Combine predictions
    all_preds <- tibble::tibble(
      YYYY = c(df_basin_product$YYYY, if (!is.null(holdout_data)) holdout_data$YYYY else NULL),
      pred = c(preds_train, preds_holdout)
    )

    if (target_positive) all_preds$pred <- pmax(all_preds$pred, 0)

    return(list(kge_cv_mean = NA_real_,
                preds = all_preds,
                fit = fitted))
  }
  # In test
  holdout_data[,target] <- NA
  # Tune model
  wflow <- workflows::workflow() |>
    workflows::add_model(spec) |>
    workflows::add_recipe(rec)

  ctrl <- tune::control_grid(save_pred = TRUE,
                             verbose = !verbose_tune,
                             allow_par=allow_par)
  rs <- tryCatch({
    tune::tune_grid(
      object    = wflow,
      resamples = resamples,
      grid      = grid,
      metrics   = yardstick::metric_set(yardstick::rmse),
      control   = ctrl,
    )
  }, error = function(e) {
    stop("Model tuning failed: ", e$message, call. = FALSE)
  })

  # Compute leaderboard
  pred_cv <- compute_leaderboard_cv(rs, truth_col = "Q")
  kge_mean <- if (nrow(pred_cv) == 0) NA_real_ else pred_cv$kge_mean[[1]]

  # Refit best model
  best_config <- tryCatch({
    tune::select_best(rs, metric = "rmse")
  }, error = function(e) {
    warning("Failed to select best model, using first configuration: ", e$message)
    rs$grid$config[[1]]
  })

  best_wf <- tune::finalize_workflow(
    wflow,
    best_config
  )

  fitted <- parsnip::fit(best_wf, df_basin_product)

  # Generate predictions
  all_data <- dplyr::bind_rows(df_basin_product, holdout_data)
  preds <- tibble::tibble(
    YYYY = all_data$YYYY,
    pred = predict(fitted, all_data)$.pred
  )

  if (target_positive) preds$pred <- pmax(preds$pred, 0)

  # Return results
  list(
    kge_cv_mean    = kge_mean,
    preds          = preds,
    fit            = fitted,
    leaderboard_cfg= pred_cv,
    param_grid = grid
  )
}








# wass2s_tune_pred_ml_old <- function(
#     df_basin_product,
#     predictors,
#     target = "Q",
#     date_col = "YYYY",
#     prediction_years = NULL,
#     model = SUPPORTED_MODELS,
#     resamples = NULL,
#     grid_levels = 5,
#     seed = 123,
#     pretrained_wflow = NULL,
#     init_frac   = 0.60,
#     assess_frac = 0.20,
#     n_splits    = 3,
#     cumulative  = TRUE,
#     quiet       = TRUE,
#     target_positive = TRUE,
#     allow_par = TRUE,
#     verbose_tune = TRUE,
#     max_na_frac =0.3,
#     impute = "median",
#     require_variance = TRUE,
#     ...
# ){
#   set.seed(seed)
#   model <- match.arg(model, SUPPORTED_MODELS)
#   spec <- model_spec(model)
#
#   # Input validation
#   if (!target %in% names(df_basin_product)) {
#     stop(glue::glue("wass2s_tune_pred_ml(): column {target} not found."), call. = FALSE)
#   }
#   if (!date_col %in% names(df_basin_product)) {
#     stop(glue::glue("wass2s_tune_pred_ml(): column {date_col} not found."), call. = FALSE)
#   }
#
#   predictors <- intersect(predictors, setdiff(names(df_basin_product), c(date_col, target)))
#   if (length(predictors) < 1L) {
#     stop("wass2s_tune_pred_ml(): predictors empty after intersection.", call. = FALSE)
#   }
#
#   # Standardize column names
#   df_basin_product <- dplyr::rename(df_basin_product,
#                                     YYYY = !!date_col,
#                                     Q = !!target)
#
#   df_basin_product <- .sanitize_numeric_columns(
#     df   = df_basin_product,
#     cols = "Q",
#     max_na_frac = max_na_frac,
#     impute = impute,
#     require_variance = require_variance
#   )
#
#   # holdout_data <- NULL
#   # if (!is.null(prediction_years)) {
#   #   if (length(prediction_years) != 2) {
#   #     stop("prediction_years must be length 2 (start, end).", call. = FALSE)
#   #   }
#   #   holdout_data <- dplyr::filter(
#   #     df_basin_product,
#   #     YYYY >= prediction_years[1], YYYY <= prediction_years[2]
#   #   )
#   #   df_basin_product <- dplyr::filter(
#   #     df_basin_product,
#   #     !(YYYY >= prediction_years[1] & YYYY <= prediction_years[2])
#   #   )
#   # }
#
#   # Handle prediction years
#   holdout_data <- NULL
#   if (!is.null(prediction_years)) {
#     if (length(prediction_years) != 2) {
#       stop("prediction_years must be length 2 (start, end).", call. = FALSE)
#     }
#     holdout_mask <- df_basin_product$YYYY >= prediction_years[1] &
#       df_basin_product$YYYY <= prediction_years[2]
#     holdout_data <- df_basin_product[holdout_mask, ]
#     df_basin_product <- df_basin_product[!holdout_mask, ]
#   }
#
#   # Check minimum data requirements
#   if (nrow(df_basin_product) < 8) {
#     stop("Insufficient training data after removing prediction years. Need at least 8 observations.",
#          call. = FALSE)
#   }
#
#    # Create recipe and model spec
#   rec <- make_recipe(df_basin_product, predictors, target = "Q")
#
#
#   if (is.null(resamples)) resamples <- make_rolling(df_basin_product,
#                                                     year_col = "YYYY",
#                                                     n_splits = n_splits,
#                                                     init_frac = init_frac,
#                                                     assess_frac=assess_frac,
#                                                     cumulative = cumulative,
#                                                     quiet = quiet)
#
#   # Check if we have enough splits
#   if (length(resamples$splits) < 1 && is.null(pretrained_wflow)) {
#     warning("Insufficient splits for proper cross-validation. Consider reducing n_splits or increasing init_frac/assess_frac.",
#             call. = FALSE)
#   }
#
#   n_min <- min_analysis_n(resamples)
#   grid  <- model_grid(model, p = min(15,length(predictors)),
#                       levels = grid_levels, n_min = n_min)
#
#   # Handle pretrained workflow case
#   if (!is.null(pretrained_wflow)) {
#     fitted <- parsnip::fit(pretrained_wflow, df_basin_product)
#     preds_train <- predict(fitted, df_basin_product)$.pred
#     preds_holdout <- if (!is.null(holdout_data)) predict(fitted, holdout_data)$.pred else NULL
#
#     # Combine predictions
#     all_preds <- tibble::tibble(
#       YYYY = c(df_basin_product$YYYY, if (!is.null(holdout_data)) holdout_data$YYYY else NULL),
#       pred = c(preds_train, preds_holdout)
#     )
#
#     if (target_positive) all_preds$pred <- pmax(all_preds$pred, 0)
#
#     return(list(kge_cv_mean = NA_real_,
#                 preds = all_preds,
#                 fit = fitted))
#   }
#
#   # Tune model
#   wflow <- workflows::workflow() |> workflows::add_model(spec) |> workflows::add_recipe(rec)
#   ctrl <- tune::control_grid(save_pred = TRUE,
#                              verbose = !verbose_tune,
#                              allow_par=allow_par,
#                              ... )
#   rs <- tryCatch({
#     tune::tune_grid(
#       object    = wflow,
#       resamples = resamples,
#       grid      = grid,
#       metrics   = yardstick::metric_set(yardstick::rmse),
#       control   = ctrl,
#     )
#   }, error = function(e) {
#     stop("Model tuning failed: ", e$message, call. = FALSE)
#   })
#
#   # Compute leaderboard
#   pred_cv <- compute_leaderboard_cv(rs, truth_col = "Q")
#   kge_mean <- if (nrow(pred_cv) == 0) NA_real_ else pred_cv$kge_mean[[1]]
#
#   # Refit best model
#   best_config <- tryCatch({
#     tune::select_best(rs, metric = "rmse")
#   }, error = function(e) {
#     warning("Failed to select best model, using first configuration: ", e$message)
#     rs$grid$config[[1]]
#   })
#
#   best_wf <- tune::finalize_workflow(
#     wflow,
#     best_config
#   )
#
#   fitted <- parsnip::fit(best_wf, df_basin_product)
#
#   # Generate predictions
#   all_data <- dplyr::bind_rows(df_basin_product, holdout_data)
#   preds <- tibble::tibble(
#     YYYY = all_data$YYYY,
#     pred = predict(fitted, all_data)$.pred
#   )
#
#   if (target_positive) preds$pred <- pmax(preds$pred, 0)
#
#   # Return results
#   list(
#     kge_cv_mean    = kge_mean,
#     preds          = preds,
#     fit            = fitted,
#     leaderboard_cfg= pred_cv,
#     param_grid = grid
#   )
# }
#
#
#
#
