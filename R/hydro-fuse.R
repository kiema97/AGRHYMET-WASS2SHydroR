#' Consolidate top-K products for one basin and one ML model
#'
#' For a given basin and ML model, this function (i) fits/tunes the model
#' on each product, (ii) ranks products by cross-validated KGE, (iii) keeps
#' the top-K, and (iv) fuses their predictions via performance-weighted
#' averaging (weights derived from KGE).
#'
#' @param data_by_product Named list of data frames/tibbles per product.
#' @param basin_id Basin identifier value.
#' @param hybas_id Name of the basin ID column (default: "HYBAS_ID").
#' @param target Name of the target column (default: "Q").
#' @param date_col Name of the date column (default: "YYYY").
#' @param pred_pattern_by_product Optional named list of regex by product to select predictors.
#' @param model One of `SUPPORTED_MODELS`.
#' @param topK Integer; number of best products to fuse (default: 3).
#' @param min_kge_model Minimum KGE threshold to accept fusion (default: -Inf).
#' @param pretrained Optional list of pre-trained models (indexed by `model` then product).
#' @param grid_levels Tuning grid granularity.
#' @param min_data_required Minimum number of rows required to train.
#' @param predictors_min Minimum number of predictors required.
#' @param quiet Logical; if FALSE, emits informative messages (default: TRUE).
#' @param verbose Logical, emit diagnostic messages.
#' @param max_na_frac Numeric in \eqn{[0, 1]}: maximum allowed fraction of missing
#'   values per column before stopping (default \code{0.20} = 20\%).
#' @param impute Character, one of \code{"median"}, \code{"mean"}, or \code{"none"}.
#'   If \code{"none"}, no imputation is performed after the guard (default \code{"median"}).
#' @param require_variance Logical; if \code{TRUE}, stop when a column has zero
#'   standard deviation after imputation (default \code{TRUE}).
#' @param ... Passed to the underlying tuner/predictor.
#'
#' @return A list with elements:
#'   \item{fused}{Fused predictions (or NULL if fusion is aborted).}
#'   \item{leaderboard_products}{Tibble of products and their KGE.}
#'   \item{all_results}{Raw list of per-product results.}
#' @export
#' @importFrom dplyr filter ungroup rename select all_of mutate
#' @importFrom purrr map map_dbl imap_dfr compact map_chr
#' @importFrom stringr str_detect
#' @importFrom tibble tibble
#' @importFrom glue glue
#' @importFrom rlang .data sym
wass2s_cons_mods_ml <- function(
    data_by_product,
    basin_id,
    hybas_id = "HYBAS_ID",
    target = "Q",
    date_col = "YYYY",
    pred_pattern_by_product = NULL,
    model = SUPPORTED_MODELS,
    topK = 3,
    min_kge_model = -Inf,
    pretrained = NULL,
    grid_levels = 5,
    min_data_required = 10,
    predictors_min = 3,
    quiet = TRUE,
    verbose = TRUE,
    max_na_frac =0.3,
    impute = "median",
    require_variance = TRUE,
    ...
){
  model <- match.arg(model, SUPPORTED_MODELS)
  prods <- names(data_by_product)

  # Validation supplémentaire des entrées
  if (length(prods) == 0) {
    stop("data_by_product must be a named list", call. = FALSE)
  }

  if (topK < 1) {
    stop("topK must be at least 1", call. = FALSE)
  }

  results <- purrr::map(prods, function(p) {
    dfp <- data_by_product[[p]] %>%
      dplyr::filter(.data[[hybas_id]] == basin_id) %>%
      dplyr::ungroup()

    # Required columns present?
    missing_cols <- setdiff(c(target, date_col), names(dfp))
    if (length(missing_cols) > 0) {
      stop(glue::glue(
        "wass2s_cons_mods_ml(): missing required columns: {paste(missing_cols, collapse = ', ')}"
      ), call. = FALSE)
    }

    # Minimum sample size
    if (nrow(dfp) < min_data_required) {
      if (!verbose) message(glue::glue(
        "Skipping product '{p}' (basin {basin_id}): ",
        "only {nrow(dfp)} rows available; at least {min_data_required} required."
      ))
      return(NULL)
    }

    # Standardize names: create 'YYYY' and 'Q'
    dfp <- dfp %>%
      dplyr::rename(
        YYYY = !!rlang::sym(date_col),
        Q    = !!rlang::sym(target)
      )

    # Product-specific predictor pattern (default '^pt_')
    # pat <- if (!is.null(pred_pattern_by_product) && p %in% names(pred_pattern_by_product)) {
    #   pred_pattern_by_product[[p]]
    # } else {
    #   "^pt_"
    # }

    pat <- if (!is.null(pred_pattern_by_product) && p %in% names(pred_pattern_by_product)) {
      pred_pattern_by_product[[p]]
    } else if(!is.null(pred_pattern_by_product) && length(pred_pattern_by_product) ==1) pred_pattern_by_product else "^pt_"


    # Select predictors: all columns except id/time/target, then regex-filter
    candidates <- setdiff(names(dfp), c(hybas_id, "YYYY", "Q"))
    predictors <- candidates[purrr::map_lgl(candidates, ~ stringr::str_detect(.x, pat))]
    if (length(predictors) < predictors_min) {
      if (!verbose) message(glue::glue(
        "Skipping product '{p}' (basin {basin_id}): ",
        "only {length(predictors)} predictors matched pattern '{pat}'; ",
        "at least {predictors_min} required."
      ))
      return(NULL)
    }

    # Pre-trained model (if provided)
    pre_wf <- NULL
    if (!is.null(pretrained) &&
        !is.null(pretrained[[model]]) &&
        !is.null(pretrained[[model]][[p]])) {
      pre_wf <- pretrained[[model]][[p]]
    }

    if (verbose) message("[", model, "] ", p, " : ",
                         length(predictors), " predictors using pattern '", pat, "'")

    # Train + predict with error handling
    out <- tryCatch({
      wass2s_tune_pred_ml(
        df_basin_product = dfp %>% dplyr::select(YYYY, Q, dplyr::all_of(predictors)),
        predictors       = predictors,
        model            = model,
        grid_levels      = grid_levels,
        pretrained_wflow = pre_wf,
        quiet            = quiet,
        max_na_frac =max_na_frac,
        impute = impute,
        require_variance =require_variance,
        ...
      )
    }, error = function(e) {
      if (!verbose) message(glue::glue("Error training product '{p}' for basin {basin_id}: {e$message}"))
      return(NULL)
    })

    if (is.null(out)) return(NULL)

    list(
      product         = p,
      kge             = out$kge_cv_mean,
      preds           = out$preds,
      leaderboard_cfg = out$leaderboard_cfg,
      fitted_model    = out$fit
    )

  }) %>% purrr::compact()

  # No usable result at all
  if (length(results) == 0) {
    if (!verbose) message(glue::glue(
      "No valid models could be trained for basin {basin_id}: ",
      "all products were skipped or failed to meet requirements."
    ))
    return(list(
      fused = NULL,
      leaderboard_products = tibble::tibble(
        product = character(),
        kge     = numeric()
      ),
      all_results = list()
    ))
  }

  # Sort by KGE (descending)
  ord <- order(purrr::map_dbl(results, "kge"), decreasing = TRUE)
  results <- results[ord]

  # Keep top-K
  results_top <- utils::head(results, n = min(topK, length(results)))

  kg <- purrr::map_dbl(results_top, "kge")
  kg[kg < 0] <- 0


  # Optional minimum KGE gate
  if (!is.infinite(min_kge_model)) {
    if (min_kge_model > 1) {
      stop("min_kge_model cannot be greater than 1.", call. = FALSE)
    }
    kg[kg < min_kge_model] <- 0
  }

  if(all(kg==0)){
    results_top <- results_top[1]
    kg <- 1
    topK <- 1
  }

  # If best KGE below threshold or all weights zero → no fusion
  if (!is.finite(results[[1]]$kge) || results[[1]]$kge < min_kge_model || max(kg) <= 0) {
    if (!verbose) message(glue::glue(
      "Fusion aborted for basin {basin_id}: ",
      "no product reached the minimum KGE threshold ({min_kge_model})."
    ))
    return(list(
      fused = NULL,
      leaderboard_products = tibble::tibble(
        product = purrr::map_chr(results, "product"),
        kge     = purrr::map_dbl(results, "kge")
      ),
      all_results = results
    ))
  }

  # Weights = truncated KGE (>= 0)
  w <- kg

  # Stack weighted predictions (each out$preds must include YYYY, pred)
  preds_long <- purrr::imap_dfr(
    results_top,
    ~ dplyr::mutate(.x$preds, product = .x$product, w = w[.y])
  )

  # Actual fusion (should aggregate by YYYY using weights 'w')
  fused <- fuse_topk(preds_long)

  list(
    fused = fused,
    leaderboard_products = tibble::tibble(
      product = purrr::map_chr(results, "product"),
      kge     = purrr::map_dbl(results, "kge")
    ),
    all_results = results
  )
}




# wass2s_cons_mods_ml_old <- function(
#     data_by_product,
#     basin_id,
#     hybas_id = "HYBAS_ID",
#     target = "Q",
#     date_col = "YYYY",
#     pred_pattern_by_product = NULL,
#     model = SUPPORTED_MODELS,
#     topK = 3,
#     min_kge_model = -Inf,
#     pretrained = NULL,
#     grid_levels = 5,
#     min_data_required = 10,
#     predictors_min = 3,
#     quiet = TRUE,
#     ...
# ){
#   model <- match.arg(model, SUPPORTED_MODELS)
#   prods <- names(data_by_product)
#
#   # Validation supplémentaire des entrées
#   if (length(prods) == 0) {
#     stop("data_by_product must be a named list", call. = FALSE)
#   }
#
#   if (topK < 1) {
#     stop("topK must be at least 1", call. = FALSE)
#   }
#
#   results <- purrr::map(prods, function(p) {
#     dfp <- data_by_product[[p]] %>%
#       dplyr::filter(.data[[hybas_id]] == basin_id) %>%
#       dplyr::ungroup()
#
#     # Required columns present?
#     missing_cols <- setdiff(c(target, date_col), names(dfp))
#     if (length(missing_cols) > 0) {
#       stop(glue::glue(
#         "wass2s_cons_mods_ml(): missing required columns: {paste(missing_cols, collapse = ', ')}"
#       ), call. = FALSE)
#     }
#
#     # Minimum sample size
#     if (nrow(dfp) < min_data_required) {
#       if (!quiet) message(glue::glue(
#         "Skipping product '{p}' (basin {basin_id}): ",
#         "only {nrow(dfp)} rows available; at least {min_data_required} required."
#       ))
#       return(NULL)
#     }
#
#     # Standardize names: create 'YYYY' and 'Q'
#     dfp <- dfp %>%
#       dplyr::rename(
#         YYYY = !!rlang::sym(date_col),
#         Q    = !!rlang::sym(target)
#       )
#
#     # Product-specific predictor pattern (default '^pt_')
#     pat <- if (!is.null(pred_pattern_by_product) && p %in% names(pred_pattern_by_product)) {
#       pred_pattern_by_product[[p]]
#     } else {
#       "^pt_"
#     }
#
#     # Select predictors: all columns except id/time/target, then regex-filter
#     candidates <- setdiff(names(dfp), c(hybas_id, "YYYY", "Q"))
#     predictors <- candidates[purrr::map_lgl(candidates, ~ stringr::str_detect(.x, pat))]
#     if (length(predictors) < predictors_min) {
#       if (!quiet) message(glue::glue(
#         "Skipping product '{p}' (basin {basin_id}): ",
#         "only {length(predictors)} predictors matched pattern '{pat}'; ",
#         "at least {predictors_min} required."
#       ))
#       return(NULL)
#     }
#
#     # Pre-trained model (if provided)
#     pre_wf <- NULL
#     if (!is.null(pretrained) &&
#         !is.null(pretrained[[model]]) &&
#         !is.null(pretrained[[model]][[p]])) {
#       pre_wf <- pretrained[[model]][[p]]
#     }
#
#     # Train + predict with error handling
#     out <- tryCatch({
#       wass2s_tune_pred_ml(
#         df_basin_product = dfp %>% dplyr::select(YYYY, Q, dplyr::all_of(predictors)),
#         predictors       = predictors,
#         model            = model,
#         grid_levels      = grid_levels,
#         pretrained_wflow = pre_wf,
#         quiet            = quiet,
#         ...
#       )
#     }, error = function(e) {
#       if (!quiet) message(glue::glue("Error training product '{p}' for basin {basin_id}: {e$message}"))
#       return(NULL)
#     })
#
#     if (is.null(out)) return(NULL)
#
#     list(
#       product         = p,
#       kge             = out$kge_cv_mean,
#       preds           = out$preds,
#       leaderboard_cfg = out$leaderboard_cfg,
#       fitted_model    = out$fit
#     )
#
#   }) %>% purrr::compact()
#
#   # No usable result at all
#   if (length(results) == 0) {
#     if (!quiet) message(glue::glue(
#       "No valid models could be trained for basin {basin_id}: ",
#       "all products were skipped or failed to meet requirements."
#     ))
#     return(list(
#       fused = NULL,
#       leaderboard_products = tibble::tibble(
#         product = character(),
#         kge     = numeric()
#       ),
#       all_results = list()
#     ))
#   }
#
#   # Sort by KGE (descending)
#   ord <- order(purrr::map_dbl(results, "kge"), decreasing = TRUE)
#   results <- results[ord]
#
#   # Keep top-K
#   results_top <- utils::head(results, n = min(topK, length(results)))
#
#   kg <- purrr::map_dbl(results_top, "kge")
#   kg[kg < 0] <- 0
#
#   # Optional minimum KGE gate
#   if (!is.infinite(min_kge_model)) {
#     if (min_kge_model > 1) {
#       stop("min_kge_model cannot be greater than 1.", call. = FALSE)
#     }
#     kg[kg < min_kge_model] <- 0
#   }
#
#   if(all(kg<=0)){
#     kg <- rep(1,min(topK, length(results)))
#   }
#
#   # If best KGE below threshold or all weights zero → no fusion
#   if (!is.finite(results[[1]]$kge) || results[[1]]$kge < min_kge_model || max(kg) <= 0) {
#     if (!quiet) message(glue::glue(
#       "Fusion aborted for basin {basin_id}: ",
#       "no product reached the minimum KGE threshold ({min_kge_model})."
#     ))
#     return(list(
#       fused = NULL,
#       leaderboard_products = tibble::tibble(
#         product = purrr::map_chr(results, "product"),
#         kge     = purrr::map_dbl(results, "kge")
#       ),
#       all_results = results
#     ))
#   }
#
#   # Weights = truncated KGE (>= 0)
#   w <- kg
#
#   # Stack weighted predictions (each out$preds must include YYYY, pred)
#   preds_long <- purrr::imap_dfr(
#     results_top,
#     ~ dplyr::mutate(.x$preds, product = .x$product, w = w[.y])
#   )
#
#   # Actual fusion (should aggregate by YYYY using weights 'w')
#   fused <- fuse_topk(preds_long)
#
#   list(
#     fused = fused,
#     leaderboard_products = tibble::tibble(
#       product = purrr::map_chr(results, "product"),
#       kge     = purrr::map_dbl(results, "kge")
#     ),
#     all_results = results
#   )
# }


























# wass2s_cons_mods_ml <- function(
#     data_by_product,
#     basin_id,
#     hybas_id = "HYBAS_ID",
#     target = "Q",
#     date_col = "YYYY",
#     pred_pattern_by_product = NULL,
#     model = SUPPORTED_MODELS,
#     topK = 3,
#     min_kge_model = -Inf,
#     pretrained = NULL,
#     grid_levels = 5,
#     min_data_required = 10,
#     predictors_min = 3,
#     quiet       = TRUE,
#     ...
# ){
#   model <- match.arg(model, SUPPORTED_MODELS)
#   prods <- names(data_by_product)
#
#   results <- purrr::map(prods, function(p) {
#     dfp <- data_by_product[[p]] %>%
#       dplyr::filter(.data[[hybas_id]] == basin_id) %>%
#       dplyr::ungroup()
#
#     # colonnes nécessaires présentes ?
#     missing_cols <- setdiff(c(target, date_col), names(dfp))
#     if (length(missing_cols) > 0) {
#       stop(glue::glue(
#         "wass2s_cons_mods_ml(): colonnes manquantes: {paste(missing_cols, collapse = ', ')}"
#       ), call. = FALSE)
#     }
#
#     # taille minimale
#       if (nrow(dfp) < min_data_required) {
#       message(glue::glue(
#         "Skipping product '{p}' (basin {basin_id}): ",
#         "only {nrow(dfp)} rows available, but at least {min_data_required} required."
#       ))
#       return(NULL)
#     }
#
#     # standardisation des noms : on crée "YYYY" et "Q"
#     # (new = old) : renommer la colonne 'date_col' en 'YYYY' et 'target' en 'Q'
#     dfp <- dfp %>%
#       dplyr::rename(
#         YYYY = !!date_col,
#         Q    = !!target
#       )
#
#     # choix du pattern par produit (sinon défaut "^pt_")
#     pat <- if (!is.null(pred_pattern_by_product) && p %in% names(pred_pattern_by_product)) {
#       pred_pattern_by_product[[p]]
#     } else {
#       "^pt_"
#     }
#
#     # sélection des prédicteurs : toutes colonnes sauf id/temps/cible,
#     # puis filtre regex
#     candidates <- setdiff(names(dfp), c(hybas_id, "YYYY", "Q"))
#     predictors <- candidates[purrr::map_lgl(candidates, ~ stringr::str_detect(.x, pat))]
#     if (length(predictors) < predictors_min) {
#       message(glue::glue(
#         "Skipping product '{p}' (basin {basin_id}): ",
#         "only {length(predictors)} predictors matched pattern '{pat}', ",
#         "but at least {predictors_min} required."
#       ))
#       return(NULL)
#     }
#
#     # récup. modèle pré-entraîné si présent
#     pre_wf <- NULL
#     if (!is.null(pretrained) && !is.null(pretrained[[model]]) && !is.null(pretrained[[model]][[p]])) {
#       pre_wf <- pretrained[[model]][[p]]
#     }
#
#     # entraînement + prédiction
#     out <- wass2s_tune_pred_ml(
#       df_basin_product = dfp %>% dplyr::select(YYYY, Q, dplyr::all_of(predictors)),
#       predictors       = predictors,
#       model            = model,
#       grid_levels      = grid_levels,
#       pretrained_wflow = pre_wf,
#       quiet =quiet,
#       ...
#     )
#
#     list(
#       product         = p,
#       kge             = out$kge_cv_mean,
#       preds           = out$preds,
#       leaderboard_cfg = out$leaderboard_cfg,
#       fitted_model    = out$fit
#     )
#   }) %>% purrr::compact()
#
#   # aucun resultat exploitable
#
#   # no usable result at all
#   if (length(results) == 0) {
#     message(glue::glue(
#       "No valid models could be trained for basin {basin_id}: ",
#       "all products were skipped or failed to meet requirements."
#     ))
#     return(list(
#       fused = NULL,
#       leaderboard_products = tibble::tibble(product = character(),
#                                             kge = numeric())
#     ))
#   }
#
#   # tri par KGE decroissant
#   ord <- order(purrr::map_dbl(results, "kge"), decreasing = TRUE)
#   results <- results[ord]
#
#   # topK
#   results_top <- head(results, n = min(topK, length(results)))
#
#   kg <- purrr::map_dbl(results_top, "kge")
#   kg[kg < 0] <- 0
#
#   # controle optionnel sur min_kge_model
#   if (!is.infinite(min_kge_model)) {
#     if (min_kge_model > 1) {
#       stop("min_kge_model can't be greater than 1.", call. = FALSE)
#     }
#     # remet a zero tout produit dont le KGE est en-dessous du seuil
#     kg[kg < min_kge_model] <- 0
#   }
#
#   # garde-fous : KGE trop faible pas de fusion
#   if (!is.finite(results[[1]]$kge) || results[[1]]$kge < min_kge_model || max(kg) <= 0) {
#     message(glue::glue(
#       "Fusion aborted for basin {basin_id}: ",
#       "no product reached the minimum KGE threshold ({min_kge_model})."
#     ))
#     return(list(
#       fused = NULL,
#       leaderboard_products = tibble::tibble(
#         product = purrr::map_chr(results, "product"),
#         kge     = purrr::map_dbl(results, "kge")
#       )
#     ))
#   }
#
#   # poids = KGE tronqué à [0, +inf)
#   w <- kg
#
#   # empilement des prédictions pondérées (chaque out$preds doit contenir YYYY, pred)
#   preds_long <- purrr::imap_dfr(results_top, ~ dplyr::mutate(.x$preds, product = .x$product, w = w[.y]))
#
#   # fusion proprement dite (doit agréger par YYYY avec les poids 'w')
#   fused <- fuse_topk(preds_long)
#
#   list(
#     fused = fused,
#     leaderboard_products = tibble::tibble(
#       product = purrr::map_chr(results, "product"),
#       kge     = purrr::map_dbl(results, "kge")
#     ),
#     all_results = results
#   )
# }


# wass2s_cons_mods_ml <- function(
#     data_by_product,
#     basin_id,
#     hybas_id = "HYBAS_ID",
#     target = "Q",
#     date_col = "YYYY"
#     pred_pattern_by_product = NULL,
#     model = SUPPORTED_MODELS,
#     topK = 3,
#     min_kge_model = -Inf,
#     pretrained = NULL,
#     grid_levels = 5,
#     min_data_required = 10,
#     predictors_min = 3,
#     ...
# ){
#   model <- match.arg(model, SUPPORTED_MODELS)
#   prods <- names(data_by_product)
#
#   results <- purrr::map(prods, function(p){
#     dfp <- data_by_product[[p]] |> dplyr::filter(.data[[hybas_id]] == basin_id) |> dplyr::ungroup()
#     missing_cols <- setdiff(c(target, date_col), names(dfp))
#     if (length(missing_cols) > 0) {
#       stop(glue::glue(
#         "wass2s_tune_pred_ml(): missing column(s): {paste(missing_cols, collapse = ', ')}"
#       ), call. = FALSE)
#     }
#
#     if (nrow(dfp) < min_data_required) return(NULL)
#     dfp <- dfp %>%
#       dplyr::rename(
#         !!date_col := "YYYY",
#         !!target   := "Q"
#       )
#
#     pat <- if(!is.null(pred_pattern_by_product) && p %in% names(pred_pattern_by_product)) pred_pattern_by_product[[p]] else "^pt_"
#     predictors <- names(dfp) |> setdiff(c(hybas_id,"YYYY","Q")) |> purrr::keep(~ stringr::str_detect(.x, pat))
#     if (length(predictors) < predictors_min) return(NULL)
#
#     pre_wf <- NULL
#     if (!is.null(pretrained) && !is.null(pretrained[[model]]) && !is.null(pretrained[[model]][[p]])) {
#       pre_wf <- pretrained[[model]][[p]]
#     }
#
#     out <- wass2s_tune_pred_ml(
#       df_basin_product = dfp |> select(YYYY, Q, all_of(predictors)),
#       predictors = predictors,
#       model = model,
#       grid_levels = grid_levels,
#       pretrained_wflow = pre_wf,)
#     list(product = p,
#          kge = out$kge_cv_mean,
#          preds = out$preds,
#          leaderboard_cfg = out$leaderboard_cfg,
#          fitted_model = out$fit)
#   }) |> purrr::compact()
#
#   if (length(results) == 0) return(list(fused = NULL, leaderboard_products = tibble(product=character(), kge=numeric())))
#
#   # tri + topK
#   ord <- order(purrr::map_dbl(results, "kge"), decreasing = TRUE)
#   results <- results[ord]
#
#   results_top <- head(results, n = min(topK, length(results)))
#
#   kg <- purrr::map_dbl(results_top, "kge")
#   #w <- weight_from_kge(kg)
#   kg[kg<0] <- 0
#
#   if (!is.finite(results[[1]]$kge) || results[[1]]$kge < min_kge_model || max(kg)<= 0 ) {
#     # meilleur produit trop faible → on écarte ce modèle pour ce bassin
#     return(list(fused = NULL, leaderboard_products = tibble(product = purrr::map_chr(results,"product"), kge = map_dbl(results,"kge"))))
#   }
#   w <- kg
#   preds_long <- purrr::imap_dfr(results_top, ~ dplyr::mutate(.x$preds, product = .x$product, w = w[.y]))
#
#   fused <- fuse_topk(preds_long)
#
#   list(
#     fused = fused,
#     leaderboard_products = tibble(product = purrr::map_chr(results, "product"), kge = purrr::map_dbl(results, "kge")),
#     all_results= results
#   )
# }
