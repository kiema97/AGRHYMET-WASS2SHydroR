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
#' @param id_col Optional. Name of an identifier column (e.g. basin, station, or subbasin ID).
#'   If provided, this column is used together with \code{date_col} to uniquely identify
#'   each time series and to guarantee correct alignment of predictions with the input data.
#'   This is strongly recommended when \code{df_basin_product} contains stacked data from
#'   multiple basins or stations.
#'
#'   If \code{NULL} (default), the function assumes that \code{df_basin_product} contains
#'   a single time series and only uses \code{date_col} for temporal ordering.
#' @param prediction_years Optional numeric vector of length 2 giving the
#'   start and end years for a holdout prediction period. These years
#' @param y_transform Character; one of \code{"none"} (default), \code{"log1p"}
#'   (applies \code{log(Q + 1)}) or \code{"yeo"} (Yeo–Johnson) for the outcome.
#'   The outcome is left untransformed by default.
#' @param include_dummy Logical; if \code{TRUE}, expand nominal predictors via
#'   \code{step_dummy(one_hot = TRUE, keep_original_cols = FALSE)} before
#'   correlation filtering. Default: \code{FALSE}.
#' @param corr_threshold Numeric in (0, 1); absolute correlation threshold used
#'   by \code{recipes::step_corr()}. Default: \code{0.90}.
#' @param corr_method Correlation method for \code{step_corr()}, typically
#'   \code{"pearson"} (default) or \code{"spearman"}.
#' @param auto_pca Logical; if \code{TRUE}, enable automatical PCA. Default: \code{TRUE}.
#' @param auto_pca_when_gt Integer; enable auto-PCA when the number of predictors
#'   is greater than this threshold. Default: \code{15}.
#' @param pca_num_comp Integer or \code{NULL}; if provided, apply PCA with a fixed
#'   number of components (disables auto-PCA).
#' @param pca_var_threshold Numeric or \code{NULL}; if provided (e.g. \code{0.95}),
#'   apply PCA keeping enough components to reach the cumulative explained variance
#'   are excluded from training and predictions are generated after fitting.
#' @param apply_impute Logical; controls whether missing value imputation is
#' applied to predictor variables. When `TRUE` (default), numeric predictors are
#' imputed using median imputation via \code{recipes::step_impute_median()}, and
#' nominal predictors (if \code{impute_nominal = TRUE}) are imputed using
#' \code{recipes::step_impute_mode()}.
#'
#' This argument should typically be set to `FALSE` when predictors have already
#' been preprocessed upstream (e.g., EOF/PCA transformation with prior
#' imputation), in order to avoid redundant transformations and preserve
#' reproducibility of the preprocessing pipeline.
#'
#' @param apply_corr Logical; indicates whether a correlation-based filtering
#' step is applied to numeric predictors. When `TRUE` (default), highly
#' correlated predictors are removed using \code{recipes::step_corr()} with the
#' specified \code{corr_threshold} and \code{corr_method}.
#'
#' Setting this argument to `FALSE` is recommended when predictors have already
#' undergone dimensionality reduction (e.g., EOF or PCA preprocessing), as the
#' correlation structure has typically been addressed upstream.
#'
#' @param apply_normalize Logical; controls whether numeric predictors are
#' standardized using \code{recipes::step_normalize()}. When `TRUE` (default),
#' predictors are centered and scaled prior to modeling.
#'
#' This argument can be set to `FALSE` when predictors have already been
#' normalized during a prior preprocessing stage (e.g., EOF/PCA computation),
#' ensuring that the same scaling is not applied multiple times and maintaining
#' consistency across modeling workflows.
#' @param impute_nominal Logical; if \code{TRUE}, apply \code{step_impute_mode()}
#'   to nominal predictors. Default: \code{TRUE}.
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
    id_col = NULL,
    prediction_years = NULL,
    y_transform = c("none", "log1p", "yeo"),
    include_dummy  = FALSE,
    corr_threshold = 0.99,
    corr_method = "pearson",
    auto_pca = TRUE,
    auto_pca_when_gt = 15,
    pca_num_comp = NULL,
    pca_var_threshold = NULL,
    apply_impute = TRUE,
    apply_corr = TRUE,
    apply_normalize = TRUE,
    impute_nominal = TRUE,
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
    max_na_frac = 0.3,
    impute = "median",
    require_variance = TRUE,
    min_data_required = 10
){
  set.seed(seed)

  model <- match.arg(model, SUPPORTED_MODELS)


  # ---- validation ----
  if (!is.data.frame(df_basin_product)) {
    stop("wass2s_tune_pred_ml(): df_basin_product must be a data.frame.", call. = FALSE)
  }
  if (!target %in% names(df_basin_product)) {
    stop(glue::glue("wass2s_tune_pred_ml(): column '{target}' not found."), call. = FALSE)
  }
  if (!date_col %in% names(df_basin_product)) {
    stop(glue::glue("wass2s_tune_pred_ml(): column '{date_col}' not found."), call. = FALSE)
  }
  if (!is.null(id_col) && !id_col %in% names(df_basin_product)) {
    stop(glue::glue("wass2s_tune_pred_ml(): id_col '{id_col}' not found."), call. = FALSE)
  }

  # Keep only predictors that exist and are not special columns
  predictors <- intersect(
    predictors,
    setdiff(names(df_basin_product), c(date_col, target, id_col))
  )
  if (length(predictors) < 1L) {
    stop("wass2s_tune_pred_ml(): predictors empty after intersection.", call. = FALSE)
  }

  spec  <- model_spec(model,p= min(15, length(predictors)))
  # ---- validate prediction_years (YYYY or YYYYMMDD) ----
  if (!is.null(prediction_years)) {
    if (!is.numeric(prediction_years) || length(prediction_years) != 2 || anyNA(prediction_years)) {
      stop(
        "prediction_years must be a numeric vector of length 2 (format YYYY or YYYYMMDD).",
        call. = FALSE
      )
    }
  }

  # ---- sanitize target AND predictors ----

  # X : contrôle + imputation
  df_basin_product <- .sanitize_numeric_columns(
    df = df_basin_product,
    cols = predictors,
    max_na_frac = max_na_frac,
    impute = impute,
    require_variance = require_variance
  )


  # ---- standardize names (keep ID if provided) ----
  if (!is.null(id_col)) {
    df_basin_product <- dplyr::rename(df_basin_product, ID = !!rlang::sym(id_col))
  }
  df_basin_product <- dplyr::rename(df_basin_product, YYYY = !!rlang::sym(date_col), Q = !!rlang::sym(target))

  # Ensure YYYY is YYYYMMDD integer (or Date -> yyyymmdd)
  df_basin_product$YYYY <- .ensure_yyyymmdd(df_basin_product$YYYY)

  # Stable ordering to avoid positional mismatch
  if (!is.null(id_col)) {
    df_basin_product <- dplyr::arrange(df_basin_product, .data$ID, .data$YYYY)
  } else {
    df_basin_product <- dplyr::arrange(df_basin_product, .data$YYYY)
  }


  # ---- holdout slicing (by YYYYMMDD bounds) ----
  holdout_data <- NULL
  bounds <- .pred_years_to_bounds(prediction_years)|> unlist()|>sort()  # should return NULL or integer(2)
  if (!is.null(bounds)) {
    bounds[2] <- min(max(bounds), max(df_basin_product$YYYY, na.rm = TRUE))

    holdout_mask <- df_basin_product$YYYY >= min(bounds) &
      df_basin_product$YYYY <= max(bounds)

    holdout_data <- df_basin_product[holdout_mask, , drop = FALSE]
    df_basin_product <- df_basin_product[!holdout_mask, , drop = FALSE]
  }

  if (nrow(df_basin_product) < min_data_required) {
    stop("Insufficient training data after removing prediction years.", call. = FALSE)
  }

  # Guard: predictors must be usable on training set
  if (!has_valid_predictors(df_basin_product, predictors, require_variance = require_variance)) {
    stop("No valid predictors available after sanitization (all NA/constant).", call. = FALSE)
  }

  # Q : contrôle qualité uniquement
  df_basin_product <- .sanitize_numeric_columns(
    df = df_basin_product,
    cols = target,
    max_na_frac = max_na_frac,
    impute = "none",
    require_variance = TRUE
  )

  # ---- recipe ----
  # IMPORTANT: ID must NOT be a predictor
  rec <- make_recipe(
    df = df_basin_product,
    predictors = predictors,
    target = "Q",
    y_transform=y_transform,
    include_dummy=include_dummy,
    corr_threshold = corr_threshold,
    corr_method=corr_method,
    auto_pca = auto_pca,
    auto_pca_when_gt=auto_pca_when_gt,
    pca_num_comp =pca_num_comp,
    pca_var_threshold = pca_var_threshold,
    apply_corr = apply_corr,
    apply_normalize = apply_normalize,
    apply_impute = apply_impute,
    impute_nominal=impute_nominal
  )

  # ---- resamples ----
  if (is.null(resamples)) {
    resamples <- make_rolling(
      df_basin_product,
      year_col = "YYYY",
      n_splits = n_splits,
      init_frac = init_frac,
      assess_frac = assess_frac,
      cumulative = cumulative,
      quiet = TRUE
    )
  }

  # ---- filter out degenerate splits (prevents "No covariates found") ----
  .filter_valid_splits <- function(rset, predictors, require_variance) {
    if (is.null(rset) || length(rset$splits) == 0) return(rset)

    ok <- vapply(rset$splits, function(spl) {
      ana <- rsample::analysis(spl)

      # If recipe later drops everything, this is almost always caused by
      # all predictors being constant/NA within the analysis window.
      has_valid_predictors(ana, predictors, require_variance = require_variance)
    }, logical(1))

    rset$splits <- rset$splits[ok]
    rset$id     <- rset$id[ok]

    rset
  }

  resamples <- .filter_valid_splits(resamples, predictors, require_variance)

  # If no valid splits remain -> fallback to direct fit (no tuning)
  if (is.null(resamples) || length(resamples$splits) == 0) {

    fitted <- parsnip::fit(
      workflows::workflow() |>
        workflows::add_model(spec) |>
        workflows::add_recipe(rec),
      df_basin_product
    )

    all_data <- dplyr::bind_rows(df_basin_product, holdout_data)

    if (!is.null(id_col)) {
      all_data <- dplyr::arrange(all_data, .data$ID, .data$YYYY)
    } else {
      all_data <- dplyr::arrange(all_data, .data$YYYY)
    }

    pred_values <- predict(fitted, new_data = all_data)$.pred
    if (target_positive) pred_values <- pmax(pred_values, 0)

    preds <- dplyr::mutate(all_data, pred = pred_values)
    keep_cols <- c(if (!is.null(id_col)) "ID", "YYYY", "Q", "pred")

    return(list(
      kge_cv_mean     = NA_real_,
      preds           = dplyr::select(preds, dplyr::all_of(keep_cols)),
      fit             = fitted,
      leaderboard_cfg = tibble::tibble(),
      param_grid      = tibble::tibble()
    ))
  }

  # ---- grid ----
  n_min <- min_analysis_n(resamples)
  grid  <- model_grid(
    model,
    p = min(15, length(predictors)),
    levels = grid_levels,
    n_min = n_min
  )

  # ---- pretrained path ----
  if (!is.null(pretrained_wflow)) {
    fitted <- parsnip::fit(pretrained_wflow, df_basin_product)

    all_data <- dplyr::bind_rows(df_basin_product, holdout_data)
    pred_values <- predict(fitted, new_data = all_data)$.pred
    if (target_positive) pred_values <- pmax(pred_values, 0)

    preds <- dplyr::mutate(all_data, pred = pred_values)
    keep_cols <- c(if (!is.null(id_col)) "ID", "YYYY","Q", "pred")

    return(list(
      kge_cv_mean = NA_real_,
      preds = dplyr::select(preds, dplyr::all_of(keep_cols)),
      fit = fitted
    ))
  }

  # ---- tune path ----
  if (!is.null(holdout_data) && nrow(holdout_data) > 0) holdout_data$Q <- NA_real_

  wflow <- workflows::workflow() |>
    workflows::add_model(spec) |>
    workflows::add_recipe(rec)

  ctrl <- tune::control_grid(
    save_pred = TRUE,
    verbose   = !verbose_tune,
    allow_par = allow_par
  )

  rs <- tune::tune_grid(
    object    = wflow,
    resamples = resamples,
    grid      = grid,
    metrics   = yardstick::metric_set(yardstick::rmse),
    control   = ctrl
  )

  pred_cv  <- compute_leaderboard_cv(rs, truth_col = "Q")
  kge_mean <- if (nrow(pred_cv) == 0) NA_real_ else pred_cv$kge_mean[[1]]

  best_config <- tune::select_best(rs, metric = "rmse")
  best_wf <- tune::finalize_workflow(wflow, best_config)
  fitted  <- parsnip::fit(best_wf, df_basin_product)

  all_data <- dplyr::bind_rows(df_basin_product, holdout_data)
  if (!is.null(id_col)) {
    all_data <- dplyr::arrange(all_data, .data$ID, .data$YYYY)
  } else {
    all_data <- dplyr::arrange(all_data, .data$YYYY)
  }

  pred_values <- predict(fitted, new_data = all_data)$.pred
  if (target_positive) pred_values <- pmax(pred_values, 0)

  preds <- dplyr::mutate(all_data, pred = pred_values)
  keep_cols <- c(if (!is.null(id_col)) "ID", "YYYY","Q", "pred")

  list(
    kge_cv_mean     = kge_mean,
    preds           = dplyr::select(preds, dplyr::all_of(keep_cols)),
    fit             = fitted,
    leaderboard_cfg = pred_cv,
    param_grid      = grid
  )
}

# wass2s_tune_pred_ml <- function(
#     df_basin_product,
#     predictors,
#     target = "Q",
#     date_col = "YYYY",
#     id_col = NULL,
#     prediction_years = NULL,
#     model = SUPPORTED_MODELS,
#     resamples = NULL,
#     grid_levels = 5,
#     seed = 123,
#     pretrained_wflow = NULL,
#     init_frac   = 0.80,
#     assess_frac = 0.20,
#     n_splits    = 3,
#     cumulative  = TRUE,
#     quiet       = TRUE,
#     target_positive = TRUE,
#     allow_par = TRUE,
#     verbose_tune = TRUE,
#     max_na_frac = 0.3,
#     impute = "median",
#     require_variance = TRUE,
#     min_data_required = 10,
#     ...
# ){
#   set.seed(seed)
#   model <- match.arg(model, SUPPORTED_MODELS)
#   spec  <- model_spec(model)
#
#   # ---- validation ----
#   if (!target %in% names(df_basin_product)) {
#     stop(glue::glue("wass2s_tune_pred_ml(): column {target} not found."), call. = FALSE)
#   }
#   if (!date_col %in% names(df_basin_product)) {
#     stop(glue::glue("wass2s_tune_pred_ml(): column {date_col} not found."), call. = FALSE)
#   }
#   if (!is.null(id_col) && !id_col %in% names(df_basin_product)) {
#     stop(glue::glue("wass2s_tune_pred_ml(): id_col {id_col} not found."), call. = FALSE)
#   }
#
#   predictors <- intersect(predictors, setdiff(names(df_basin_product), c(date_col, target, id_col)))
#   if (length(predictors) < 1L) {
#     stop("wass2s_tune_pred_ml(): predictors empty after intersection.", call. = FALSE)
#   }
#   if (!is.null(prediction_years)) {
#
#     if (!is.numeric(prediction_years) ||
#         length(prediction_years) != 2 ||
#         anyNA(prediction_years)) {
#
#       stop(
#         "prediction_years must be a numeric vector of length 2 ",
#         "(format YYYY or YYYYMMDD).",
#         call. = FALSE
#       )
#     }
#   }
#
#
#   # ---- standardize names (keep ID if provided) ----
#   if (!is.null(id_col)) {
#     df_basin_product <- dplyr::rename(df_basin_product, ID = !!id_col)
#   }
#   df_basin_product <- dplyr::rename(df_basin_product, YYYY = !!date_col, Q = !!target)
#
#   # Ensure YYYY is YYYYMMDD integer (or Date -> yyyymmdd)
#   df_basin_product$YYYY <- .ensure_yyyymmdd(df_basin_product$YYYY)
#
#   # Stable ordering to avoid positional mismatch
#   if (!is.null(id_col)) {
#     df_basin_product <- dplyr::arrange(df_basin_product, ID, YYYY)
#   } else {
#     df_basin_product <- dplyr::arrange(df_basin_product, YYYY)
#   }
#
#   # Sanitize target column
#   df_basin_product <- .sanitize_numeric_columns(
#     df   = df_basin_product,
#     cols = "Q",
#     max_na_frac = max_na_frac,
#     impute = impute,
#     require_variance = require_variance
#   )
#
#   # ---- holdout slicing (by YYYYMMDD bounds) ----
#   holdout_data <- NULL
#   bounds <- .pred_years_to_bounds(prediction_years)|> unlist()
#   if (!is.null(bounds)) {
#     bounds[2] <- min(bounds[2], max(df_basin_product$YYYY, na.rm = TRUE))
#
#     holdout_mask <- df_basin_product$YYYY >= bounds[1] &
#       df_basin_product$YYYY <= bounds[2]
#     holdout_data <- df_basin_product[holdout_mask, , drop = FALSE]
#     df_basin_product <- df_basin_product[!holdout_mask, , drop = FALSE]
#   }
#
#   if (nrow(df_basin_product) < min_data_required) {
#     stop("Insufficient training data after removing prediction years.", call. = FALSE)
#   }
#
#   # Recipe (IMPORTANT: do NOT require ID as predictor)
#   rec <- make_recipe(df_basin_product, predictors, target = "Q")
#
#   # Resamples
#   if (is.null(resamples)) {
#     resamples <- make_rolling(
#       df_basin_product,
#       year_col = "YYYY",
#       n_splits = n_splits,
#       init_frac = init_frac,
#       assess_frac = assess_frac,
#       cumulative = cumulative,
#       quiet = TRUE
#     )
#   }
#
#   n_min <- min_analysis_n(resamples)
#   grid  <- model_grid(
#     model,
#     p = min(15, length(predictors)),
#     levels = grid_levels,
#     n_min = n_min
#   )
#
#   # ---- pretrained path ----
#   if (!is.null(pretrained_wflow)) {
#     fitted <- parsnip::fit(pretrained_wflow, df_basin_product)
#
#     all_data <- dplyr::bind_rows(df_basin_product, holdout_data)
#
#     pred_values <- predict(fitted, new_data = all_data)$.pred
#     if (target_positive) pred_values <- pmax(pred_values, 0)
#
#     preds <- dplyr::mutate(all_data, pred = pred_values)
#
#     # Return only stable key + pred (plus optionally Q if you want)
#     keep_cols <- c(if (!is.null(id_col)) "ID", "YYYY","Q", "pred")
#     return(list(
#       kge_cv_mean = NA_real_,
#       preds = dplyr::select(preds, dplyr::all_of(keep_cols)),
#       fit = fitted
#     ))
#   }
#
#   # ---- tune path ----
#   if (!is.null(holdout_data) && nrow(holdout_data) > 0) holdout_data$Q <- NA_real_
#
#   wflow <- workflows::workflow() |>
#     workflows::add_model(spec) |>
#     workflows::add_recipe(rec)
#
#   ctrl <- tune::control_grid(
#     save_pred = TRUE,
#     verbose = !verbose_tune,
#     allow_par = allow_par,
#     ...
#   )
#
#   rs <- tune::tune_grid(
#     object    = wflow,
#     resamples = resamples,
#     grid      = grid,
#     metrics   = yardstick::metric_set(yardstick::rmse),
#     control   = ctrl
#   )
#
#   pred_cv  <- compute_leaderboard_cv(rs, truth_col = "Q")
#   kge_mean <- if (nrow(pred_cv) == 0) NA_real_ else pred_cv$kge_mean[[1]]
#
#   best_config <- tune::select_best(rs, metric = "rmse")
#   best_wf <- tune::finalize_workflow(wflow, best_config)
#   fitted  <- parsnip::fit(best_wf, df_basin_product)
#   all_data <- dplyr::bind_rows(df_basin_product, holdout_data)
#   if (!is.null(id_col)) {
#     all_data <- dplyr::arrange(all_data, ID, YYYY)
#   } else {
#     all_data <- dplyr::arrange(all_data, YYYY)
#   }
#
#   pred_values <- predict(fitted, new_data = all_data)$.pred
#   if (target_positive) pred_values <- pmax(pred_values, 0)
#
#   preds <- dplyr::mutate(all_data, pred = pred_values)
#
#   keep_cols <- c(if (!is.null(id_col)) "ID", "YYYY","Q", "pred")
#   list(
#     kge_cv_mean     = kge_mean,
#     preds           = dplyr::select(preds, dplyr::all_of(keep_cols)),
#     fit             = fitted,
#     leaderboard_cfg = pred_cv,
#     param_grid      = grid
#   )
# }

# wass2s_tune_pred_ml <- function(
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
#     init_frac   = 0.80,
#     assess_frac = 0.20,
#     n_splits    = 3,
#     cumulative  = TRUE,
#     quiet       = TRUE,
#     target_positive = TRUE,
#     allow_par = TRUE,
#     verbose_tune = TRUE,
#     max_na_frac = 0.3,
#     impute = "median",
#     require_variance = TRUE,
#     min_data_required = 10,
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
#   df_basin_product <- dplyr::rename(
#     df_basin_product,
#     YYYY = !!date_col,
#     Q    = !!target
#   )
#
#   # Force YYYY to YYYYMMDD and sort
#   df_basin_product$YYYY <- .ensure_yyyymmdd(df_basin_product$YYYY)
#   df_basin_product <- dplyr::arrange(df_basin_product, YYYY)
#
#   # Sanitize target column
#   df_basin_product <- .sanitize_numeric_columns(
#     df   = df_basin_product,
#     cols = "Q",
#     max_na_frac = max_na_frac,
#     impute = impute,
#     require_variance = require_variance
#   )
#
#   # Handle prediction years
#   holdout_data <- NULL
#   if (!is.null(prediction_years)) {
#
#     if (length(prediction_years) == 1 && prediction_years > 0) {
#       prediction_years <- rep(prediction_years, 2)
#     }
#
#     if (length(prediction_years) != 2) {
#       stop("prediction_years must be length 2 (start, end).", call. = FALSE)
#     }
#
#     # Convert years -> YYYYMMDD bounds
#     start_bound <- as.integer(paste0(prediction_years[1], "0101"))
#     end_bound   <- as.integer(paste0(prediction_years[2], "1231"))
#
#     holdout_mask <- df_basin_product$YYYY >= start_bound &
#       df_basin_product$YYYY <= end_bound
#
#     holdout_data <- df_basin_product[holdout_mask, , drop = FALSE]
#     df_basin_product <- df_basin_product[!holdout_mask, , drop = FALSE]
#   }
#
#   # Check minimum data requirements
#   if (nrow(df_basin_product) < min_data_required) {
#     stop(
#       "Insufficient training data after removing prediction years. Need at least 10 observations.",
#       call. = FALSE
#     )
#   }
#
#   # Create recipe and model spec
#   rec <- make_recipe(df_basin_product, predictors, target = "Q")
#
#   if (is.null(resamples)) {
#     resamples <- make_rolling(
#       df_basin_product,
#       year_col = "YYYY",      # now YYYYMMDD
#       n_splits = n_splits,
#       init_frac = init_frac,
#       assess_frac = assess_frac,
#       cumulative = cumulative,
#       quiet = TRUE
#     )
#   }
#
#   # Check if we have enough splits
#   if (length(resamples$splits) < 1 && is.null(pretrained_wflow)) {
#     warning(
#       "Insufficient splits for proper cross-validation. Consider reducing n_splits or increasing init_frac/assess_frac.",
#       call. = FALSE
#     )
#   }
#
#   n_min <- min_analysis_n(resamples)
#   grid  <- model_grid(
#     model,
#     p = min(15, length(predictors)),
#     levels = grid_levels,
#     n_min = n_min
#   )
#
#   # Handle pretrained workflow case
#   if (!is.null(pretrained_wflow)) {
#     fitted <- parsnip::fit(pretrained_wflow, df_basin_product)
#     preds_train <- predict(fitted, df_basin_product)$.pred
#     preds_holdout <- if (!is.null(holdout_data) && nrow(holdout_data) > 0) {
#       predict(fitted, holdout_data)$.pred
#     } else {
#       NULL
#     }
#
#     all_preds <- tibble::tibble(
#       YYYY = c(df_basin_product$YYYY,
#                if (!is.null(holdout_data) && nrow(holdout_data) > 0) holdout_data$YYYY else NULL),
#       pred = c(preds_train, preds_holdout)
#     )
#
#     if (target_positive) all_preds$pred <- pmax(all_preds$pred, 0)
#
#     return(list(
#       kge_cv_mean = NA_real_,
#       preds = all_preds,
#       fit = fitted
#     ))
#   }
#
#   # Ensure Q exists in holdout and is NA (if holdout exists)
#   if (!is.null(holdout_data) && nrow(holdout_data) > 0) {
#     holdout_data$Q <- NA_real_
#   }
#
#   # Tune model
#   wflow <- workflows::workflow() |>
#     workflows::add_model(spec) |>
#     workflows::add_recipe(rec)
#
#   ctrl <- tune::control_grid(
#     save_pred = TRUE,
#     verbose = !verbose_tune,
#     allow_par = allow_par,
#     ...
#   )
#
#   rs <- tryCatch({
#     tune::tune_grid(
#       object    = wflow,
#       resamples = resamples,
#       grid      = grid,
#       metrics   = yardstick::metric_set(yardstick::rmse),
#       control   = ctrl
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
#   best_wf <- tune::finalize_workflow(wflow, best_config)
#   fitted  <- parsnip::fit(best_wf, df_basin_product)
#
#   # Generate predictions (train + holdout if any)
#   all_data <- dplyr::bind_rows(df_basin_product, holdout_data)
#   pred_vec <- predict(fitted, all_data)$.pred
#   preds <- dplyr::mutate(all_data, pred = pred_vec)
#
#
#   # preds <- tibble::tibble(
#   #   YYYY = all_data$YYYY,
#   #   pred = predict(fitted, all_data)$.pred
#   # )
#
#   if (target_positive) preds$pred <- pmax(preds$pred, 0)
#
#   list(
#     kge_cv_mean     = kge_mean,
#     preds           = preds,
#     fit             = fitted,
#     leaderboard_cfg = pred_cv,
#     param_grid      = grid
#   )
# }






# wass2s_tune_pred_ml_v2 <- function(
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
#     init_frac   = 0.80,
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
#     min_data_required = 10,
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
#   if(!is.null(prediction_years) && length(prediction_years)==1 && prediction_years>0 ){
#     prediction_years <- rep(prediction_years,2)
#   }
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
#   if (nrow(df_basin_product) < min_data_required) {
#     stop("Insufficient training data after removing prediction years. Need at least 10 observations.",
#          call. = FALSE)
#   }
#
#   # Create recipe and model spec
#   rec <- make_recipe(df_basin_product, predictors, target = "Q")
#
#
#   if (is.null(resamples)) resamples <- make_rolling(df_basin_product,
#                                                     year_col = "YYYY",
#                                                     n_splits = n_splits,
#                                                     init_frac = init_frac,
#                                                     assess_frac=assess_frac,
#                                                     cumulative = cumulative,
#                                                     quiet = TRUE)
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
#   # In test
#   holdout_data[[target]] <- NA_real_
#   # Tune model
#   wflow <- workflows::workflow() |>
#     workflows::add_model(spec) |>
#     workflows::add_recipe(rec)
#
#   ctrl <- tune::control_grid(save_pred = TRUE,
#                              verbose = !verbose_tune,
#                              allow_par=allow_par)
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
