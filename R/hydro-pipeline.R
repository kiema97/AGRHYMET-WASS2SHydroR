#' Train all selected ML models for one basin and perform final fusion
#'
#' For a given basin, consolidate each chosen base model across products (top-K
#' weighted fusion), then train a meta-learner on the consolidated columns to
#' produce the final prediction for the last year.
#'
#' @param data_by_product Named list of data frames (one per product).
#' @param basin_id Basin identifier value.
#' @param hybas_id Name of the basin ID column.
#' @param prediction_years Optional numeric vector of length 2 giving the
#'   start and end years for a holdout prediction period. These years
#' @param target Name of the target column (default: `"Q"`).
#' @param date_col Name of the date column (default: `"YYYY"`).
#' @param pred_pattern_by_product Optional per-product regex to select predictors.
#' @param models Character vector of base models to run (subset of \code{SUPPORTED_MODELS}).
#' @param topK Integer, number of best products per base model (default: 3).
#' @param min_kge_model Minimum best KGE required to keep a base model for the basin.
#' @param final_fuser Name of the meta-learner to use (subset of \code{SUPPORTED_FUSERS}).
#' @param grid_levels Grid density for tuning both base models and meta-learner.
#' @param quiet Logical; if \code{FALSE}, emits informative messages.
#' @param verbose_tune A logical for logging results (other than warnings and errors, which are always shown) as they are generated during training in a single R process.
#' @param max_na_frac Numeric in \eqn{[0, 1]}: maximum allowed fraction of missing
#'   values per column before stopping (default \code{0.20} = 20\%).
#' @param impute Character, one of \code{"median"}, \code{"mean"}, or \code{"none"}.
#'   If \code{"none"}, no imputation is performed after the guard (default \code{"median"}).
#' @param require_variance Logical; if \code{TRUE}, stop when a column has zero
#'   standard deviation after imputation (default \code{TRUE}).
#' @param ... Passed to \code{WASS2SHydroR::wass2s_cons_mods_ml}.
#' @return A list with:
#' \itemize{
#'   \item \code{fused_by_model}: tibble with \code{YYYY}, \code{Q}, and consolidated columns per model,
#'   \item \code{final_test}: tibble for the test year with \code{pred_final},
#'   \item \code{scores}: tibble with \code{HYBAS_ID}, \code{kge_final}, \code{rmse_final},
#'   \item \code{leaderboards}: list of product leaderboards per model.
#'   \item \code{cv_rs}: resampling results (or NULL if not applicable)
#' }
#' @examples
#' # wass2s_run_bas_mod_ml(basin_id = 1, data_by_product = lst, models = c("rf","xgb"))
#' @export
#'
wass2s_run_bas_mod_ml <- function(
    data_by_product,
    basin_id,
    prediction_years = NULL,
    target = "Q",
    date_col = "YYYY",
    hybas_id = "HYBAS_ID",
    pred_pattern_by_product = NULL,
    models = SUPPORTED_MODELS,
    topK = 3,
    min_kge_model = -Inf,
    grid_levels = 5,
    final_fuser = "rf",
    quiet = TRUE,
    verbose_tune = TRUE,
    max_na_frac =0.3,
    impute = "median",
    require_variance = TRUE,
    ...
) {

  models <- unique(tolower(models))
  bad <- setdiff(models, SUPPORTED_MODELS)
  if (length(bad)) {
    message("Ignoring unsupported base models: ", paste(bad, collapse = ", "))
    models <- intersect(models, SUPPORTED_MODELS)
  }
  final_fuser <- match.arg(final_fuser, SUPPORTED_FUSERS)
  .require_pkg(engine_pkg[c(final_fuser,models)])


  # 1) Consolidate per base model with error handling
  outs <- purrr::map(models, function(m) {
    tryCatch({
      cm <- wass2s_cons_mods_ml(
        basin_id = basin_id,
        data_by_product = data_by_product,
        hybas_id = hybas_id,
        pred_pattern_by_product = pred_pattern_by_product,
        model = m,
        topK = topK,
        min_kge_model = min_kge_model,
        grid_levels = grid_levels,
        quiet = quiet,
        max_na_frac =max_na_frac,
        impute = impute,
        require_variance =require_variance,
        ...
      )
      list(
        model = toupper(m),
        fused = cm$fused,
        leaderboard = cm$leaderboard_products,
        all_results = cm$all_results
      )
    }, error = function(e) {
      if (!quiet) message("Error processing model ", m, ": ", e$message)
      list(
        model = toupper(m),
        fused = NULL,
        leaderboard = tibble::tibble(product = character(), kge = numeric()),
        all_results = list()
      )
    })
  })

  # Join consolidated series (columns RF/XGB/MLP/…)
  fused_list <- purrr::set_names(purrr::map(outs, "fused"),
                                 purrr::map_chr(outs, "model"))
  fused_list_compact <- purrr::compact(fused_list)

  # Obtain YYYY and Q from any available product (helper)
  any_df <- tryCatch({
    get_any_Q(data_by_product, basin_id, hybas_id)
  }, error = function(e) {
    if (!quiet) message("Error getting base data: ", e$message)
    tibble::tibble(YYYY = integer(), Q = numeric())
  })

  any_df <- .sanitize_numeric_columns(
    df   = any_df,
    cols = target,
    max_na_frac = max_na_frac,
    impute = impute,
    require_variance = require_variance
  )

  # Case A: no retained model
  if (length(fused_list_compact) == 0L) {
    if (!quiet) warning(glue::glue(
      "No ML model retained for basin {basin_id}: returning empty structures."
    ))
    return(list(
      fused_by_model = any_df,
      final_test = dplyr::mutate(dplyr::slice_tail(any_df, n = 1), pred_final = NA_real_),
      scores = tibble::tibble(
        HYBAS_ID = basin_id,
        kge_final = NA_real_,
        rmse_final = NA_real_
      ),
      leaderboards = stats::setNames(purrr::map(outs, "leaderboard"), purrr::map_chr(outs, "model")),
      cv_rs = NULL
    ))
  }

  fused_list_compact2 <- purrr::imap(fused_list_compact,
                                     ~ dplyr::rename(.x, pred = pred_fused))
  # Join all predictions - assuming safe_full_join_preds is defined elsewhere
  fused_models <- tryCatch({
    safe_full_join_preds(fused_list_compact2)
  }, error = function(e) {
    if (!quiet) message("Error joining predictions: ", e$message)
    # Fallback: create empty frame with YYYY column
    yyyys <- unique(unlist(lapply(fused_list_compact2, function(x) x$YYYY)))
    tibble::tibble(YYYY = sort(yyyys))
  })


  # 3) Add observed Q for this basin
  # if (nrow(any_df) == 0) {
  #   any_df <- tibble::tibble(YYYY = fused_models$YYYY, Q = NA_real_)
  #   fused_models <- dplyr::left_join(any_df, fused_models, by = "YYYY")
  #   return(fused_models)
  # }

  # Add observed Q for this basin
  if (nrow(any_df) == 0) {
    any_df <- tibble::tibble(YYYY = fused_models$YYYY, Q = NA_real_)
  }

  fused_models <- dplyr::left_join(any_df, fused_models, by = "YYYY")

  # Handle case where we have no data
  if (nrow(fused_models) == 0) {
    if (!quiet) warning(glue::glue("No data available for basin {basin_id} after processing"))
    return(list(
      fused_by_model = fused_models,
      final_test = tibble::tibble(YYYY = integer(), Q = numeric(), pred_final = numeric()),
      scores = tibble::tibble(
        HYBAS_ID = basin_id,
        kge_final = NA_real_,
        rmse_final = NA_real_
      ),
      leaderboards = stats::setNames(purrr::map(outs, "leaderboard"), purrr::map_chr(outs, "model")),
      cv_rs = NULL
    ))
  }

  # 4) Meta-learner (final fusion)
  # Split data based on prediction_years or use last year as test
  if (!is.null(prediction_years)) {
    if (length(prediction_years) != 2) {
      stop("prediction_years must be length 2 (start, end).", call. = FALSE)
    }
    if (prediction_years[1] > prediction_years[2]) {
      stop("Starting point must be earlier than ending point.", call. = FALSE)
    }
    df_te <- dplyr::filter(
      fused_models,
      YYYY >= prediction_years[1], YYYY <= prediction_years[2]
    )
    df_tr <- dplyr::filter(
      fused_models,
      !(YYYY >= prediction_years[1] & YYYY <= prediction_years[2])
    )
  } else {
    # Use last year as test by default
    train_idx <- 1:(nrow(fused_models) - 1L)
    df_tr <- fused_models[train_idx, ]
    df_te <- fused_models[-train_idx, ]
  }

  # Fallback if too few training rows
  if (nrow(df_tr) < 5L) {
    if (!quiet) warning(glue::glue(
      "Too few training rows ({nrow(df_tr)}) for basin {basin_id}: using simple mean across predictors."
    ))

    pred_cols <- setdiff(names(fused_models), c("YYYY", "Q"))
    if (length(pred_cols) == 0) {
      # No predictor columns available
      final <- fused_models |>
        dplyr::mutate(pred_final = NA_real_)
    } else {
      # Calculate mean of available predictions
      final <- fused_models |>
        dplyr::mutate(pred_final = rowMeans(
          dplyr::select(., dplyr::all_of(pred_cols)),
          na.rm = TRUE
        )) |>
        # Replace NaN with NA (if all values were NA)
        dplyr::mutate(pred_final = ifelse(is.nan(pred_final), NA_real_, pred_final))
    }

    # Calculate scores on training data if available
    if (nrow(df_tr) > 0) {
      train_pred <- final |>
        dplyr::filter(YYYY %in% df_tr$YYYY) |>
        dplyr::filter(!is.na(Q), !is.na(pred_final))

      if (nrow(train_pred) > 0) {
        kge_val <- kge_vec(train_pred$Q, train_pred$pred_final)
        rmse_val <- yardstick::rmse_vec(train_pred$Q, train_pred$pred_final)
      } else {
        kge_val <- NA_real_
        rmse_val <- NA_real_
      }
    } else {
      kge_val <- NA_real_
      rmse_val <- NA_real_
    }

    return(list(
      fused_by_model = final,
      final_test = dplyr::slice_tail(final, n = 1),
      scores = tibble::tibble(
        HYBAS_ID = basin_id,
        kge_final = kge_val,
        rmse_final = rmse_val
      ),
      leaderboards = stats::setNames(purrr::map(outs, "leaderboard"), purrr::map_chr(outs, "model")),
      cv_rs = NULL
    ))
  }

  if (!is.null(prediction_years)) {
    if (length(prediction_years) != 2) {
      stop("prediction_years must be length 2 (start, end).", call. = FALSE)
    }
    if (prediction_years[1] > prediction_years[2]) {
      stop("Starting point must be earlier than ending point.", call. = FALSE)
    }
    df_te <- dplyr::filter(
      fused_models,
      YYYY >= prediction_years[1], YYYY <= prediction_years[2]
    )
    df_tr <- dplyr::filter(
      fused_models,
      !(YYYY >= prediction_years[1] & YYYY <= prediction_years[2])
    )
  }

  pred_cols <- setdiff(names(df_tr), c("Q", "YYYY"))
  if (length(pred_cols) < 1L) {
    stop("wass2s_run_bas_mod_ml(): no meta-features available (only 'Q').", call. = FALSE)
  }

  rec_meta <- recipes::recipe(df_tr) |>
    recipes::update_role(!!rlang::sym("Q"), new_role = "outcome") |>
    recipes::update_role(dplyr::all_of(pred_cols), new_role = "predictor") |>
    recipes::step_zv(recipes::all_predictors()) |>
    recipes::step_impute_median(recipes::all_predictors())

   spec <- model_spec(final_fuser)
   grid <- model_grid(final_fuser, p = ncol(df_tr) - 2L, levels = grid_levels)

  wf_meta <- workflows::workflow() |>
    workflows::add_recipe(rec_meta) |>
    workflows::add_model(spec)

  # Create resamples for tuning
  rset <- tryCatch({
    make_rolling(df_tr, n_splits = 2)
  }, error = function(e) {
    if (!quiet) message("Error creating resamples: ", e$message)
    NULL
  })


  # Tune meta-learner
  if (!is.null(rset)) {
    ctrl <- tune::control_grid(save_pred = TRUE, verbose = !verbose_tune, allow_par = TRUE)

    rs <- tryCatch({
      tune::tune_grid(
        wf_meta,
        resamples = rset,
        grid = grid,
        metrics = yardstick::metric_set(yardstick::rmse),
        control = ctrl
      )
    }, error = function(e) {
      if (!quiet) message("Error tuning meta-learner: ", e$message)
      NULL
    })
  } else {
    rs <- NULL
  }

  # Handle tuning failure
  if (is.null(rs) || nrow(rs$.metrics[[1]]) == 0) {
    if (!quiet) warning(glue::glue(
      "Meta-learner tuning failed for basin {basin_id}: fallback to simple mean."
    ))

    final <- fused_models |>
      dplyr::mutate(pred_final = rowMeans(
        dplyr::select(., dplyr::all_of(pred_cols)),
        na.rm = TRUE
      )) |>
      dplyr::mutate(pred_final = ifelse(is.nan(pred_final), NA_real_, pred_final))

    # Calculate scores on training data
    train_pred <- final |>
      dplyr::filter(YYYY %in% df_tr$YYYY) |>
      dplyr::filter(!is.na(Q), !is.na(pred_final))

    if (nrow(train_pred) > 0) {
      kge_val <- kge_vec(train_pred$Q, train_pred$pred_final)
      rmse_val <- yardstick::rmse_vec(train_pred$Q, train_pred$pred_final)
    } else {
      kge_val <- NA_real_
      rmse_val <- NA_real_
    }

    return(list(
      fused_by_model = final,
      final_test = dplyr::slice_tail(final, n = 1),
      scores = tibble::tibble(
        HYBAS_ID = basin_id,
        kge_final = kge_val,
        rmse_final = rmse_val
      ),
      leaderboards = stats::setNames(purrr::map(outs, "leaderboard"), purrr::map_chr(outs, "model")),
      cv_rs = NULL
    ))
  }

  best   <- tune::select_best(rs, metric = "rmse")
  wf_fin <- tune::finalize_workflow(wf_meta, best)
  fit_fin <- parsnip::fit(wf_fin, df_tr)

  train_prediction <- df_tr |>
    dplyr::mutate(pred_final = predict(fit_fin, df_tr)$.pred)

  fused_models$pred_final <- predict(fit_fin, fused_models)$.pred

  scores <- tibble::tibble(
    HYBAS_ID  = basin_id,
    kge_final = kge_vec(train_prediction$Q, train_prediction$pred_final),
    rmse_final = yardstick::rmse_vec(train_prediction$Q, train_prediction$pred_final)
  )

  message(glue::glue(
    "Successfully trained and fused {length(models)} base model(s) for basin {basin_id} ",
    "using meta-learner '{final_fuser}'."
  ))

  return(list(
    fused_by_model = fused_models,
    cv_rs          = tune::collect_metrics(rs),
    scores         = scores,
    leaderboards   = stats::setNames(purrr::map(outs, "leaderboard"),
                                     purrr::map_chr(outs, "model"))
  ))
}


#' Run the Hydro + ML pipeline for all basins
#'
#' Iterate over all basin IDs, running \code{wass2s_run_bas_mod_ml()} for
#' each, optionally in parallel. Returns a named list indexed by basin ID.
#'
#' @param data_by_product Named list of data frames (one per product).
#' @param hybas_id Name of the basin ID column.
#' @param pred_pattern_by_product Optional per-product regex for predictor selection.
#' @param models Character vector of base models to include.
#' @param topK Integer, number of best products per base model.
#' @param min_kge_model Minimum best KGE required to keep a base model.
#' @param basins Optional vector of basin IDs to subset (default: all).
#' @param parallel Logical; if \code{TRUE}, uses \pkg{furrr} for parallel execution.
#' @param workers Integer number of workers when \code{parallel = TRUE}.
#' @param grid_levels Grid density for tuning.
#' @param final_fuser Name of the meta-learner for final fusion.
#' @param quiet Logical; if \code{FALSE}, emits informative messages.
#' @param ... Other parameters passed to \code{wass2s_run_bas_mod_ml}.
#'
#' @return A named list: one element per basin, each the list returned by
#'   \code{wass2s_run_bas_mod_ml()}.
#'
#' @examples
#' # res <- wass2s_run_basins_ml(data_by_product = lst, models = c("rf","xgb"), final_fuser = "glmnet")
#' @export
wass2s_run_basins_ml <- function(
    data_by_product,
    hybas_id = "HYBAS_ID",
    pred_pattern_by_product = NULL,
    models = SUPPORTED_MODELS,
    topK = 3,
    min_kge_model = -Inf,
    basins = NULL,
    parallel = FALSE,
    workers = 4,
    grid_levels = 5,
    final_fuser = "rf",
    quiet = TRUE,
    ...
) {
  # Input validation
  .require_pkg(engine_pkg[c(final_fuser,models)])

  if (length(data_by_product) == 0) {
    stop("data_by_product cannot be empty", call. = FALSE)
  }

  if (!hybas_id %in% names(data_by_product[[1]])) {
    stop("hybas_id column not found in data", call. = FALSE)
  }

  # Get all basins if not specified
  if (is.null(basins)) {
    basins <- tryCatch({
      unique(unlist(lapply(data_by_product, function(df) unique(df[[hybas_id]]))))
    }, error = function(e) {
      stop("Error extracting basin IDs: ", e$message, call. = FALSE)
    })

    basins <- sort(basins[!is.na(basins)])
  }

  if (length(basins) == 0) {
    stop("No basins found in the data", call. = FALSE)
  }

  if (TRUE) message("Processing ", length(basins), " basins")

  # Define the runner function with error handling
  runner <- function(bid) {
    if (TRUE) message("Processing basin: ", bid)

    tryCatch({
      result <- wass2s_run_bas_mod_ml(
        basin_id = bid,
        data_by_product = data_by_product,
        hybas_id = hybas_id,
        pred_pattern_by_product = pred_pattern_by_product,
        models = models,
        topK = topK,
        min_kge_model = min_kge_model,
        grid_levels = grid_levels,
        final_fuser = final_fuser,
        quiet = quiet,
        ...
      )

      # Add basin ID to the result for easier identification
      result$basin_id <- bid
      result
    }, error = function(e) {
      if (TRUE) warning("Error processing basin ", bid, ": ", e$message)

      # Return a structured error result
      list(
        basin_id = bid,
        error = e$message,
        fused_by_model = NULL,
        final_test = NULL,
        scores = tibble::tibble(
          HYBAS_ID = bid,
          kge_final = NA_real_,
          rmse_final = NA_real_
        ),
        leaderboards = list(),
        cv_rs = NULL
      )
    })
  }

  # Process basins
  if (parallel) {
    if (!requireNamespace("future", quietly = TRUE)) {
      stop("Package 'future' is required for parallel execution. Please install it.")
    }
    if (!requireNamespace("furrr", quietly = TRUE)) {
      stop("Package 'furrr' is required for parallel execution. Please install it.")
    }
    # Set up parallel processing
    old_plan <- future::plan(future::multisession, workers = workers)
    on.exit(future::plan(old_plan), add = TRUE)

    if (TRUE) message("Running in parallel with ", workers, " workers")

    res <- furrr::future_map(
      basins,
      runner,
      .progress = !quiet,
      .options = furrr::furrr_options(seed = TRUE)
    )
  } else {
    if (TRUE) message("Running sequentially")
    res <- purrr::map(basins, runner)
  }

  # Name the results
  names(res) <- as.character(basins)

  # Summarize results
  success_count <- sum(sapply(res, function(x) is.null(x$error)))
  if (TRUE) message("Completed: ", success_count, " successful, ",
                      length(basins) - success_count, " failed")

  # Return results
  res
}
