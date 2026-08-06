# ---- Models supported -------------------------------------------------------
#' Supported meta-learners for final fusion (identifiers)
#'
#' Character vector of model keys allowed in \code{meta_spec()} and \code{meta_grid()}.
#'
#' @format Character vector.
#' @keywords internal

SUPPORTED_FUSERS <- c("rf","xgb","glmnet","kknn","svmlinear","mars","cubist","mlp")


#' Supported ML models
#'
#' Character vector of model keys allowed in \code{meta_spec()} and \code{meta_grid()}.
#'
#' @format Character vector.
#' @keywords internal

SUPPORTED_MODELS <- c("rf","xgb","mlp","kknn","svmlinear","mars","cubist","glmnet")

#' Mapping of ML engines to required R packages
#' @format A named character vector:
#' - **names**: internal engine identifiers
#' - **values**: required R package names
#'
#' @keywords internal
#' @noRd

engine_pkg <- c(
  rf        = "ranger",
  xgb       = "xgboost",
  glmnet    = "glmnet",
  kknn      = "kknn",
  svmlinear = "kernlab",
  mars      = "earth",
  cubist    = "Cubist",
  mlp       = "nnet"
)



#' Check that required packages are installed
#'
#' This internal utility checks whether one or more required packages
#' are installed. If not, it throws a clear error message.
#'
#' @param pkg A character vector of package names.
#'
#' @keywords internal
#' @noRd
.require_pkg <- function(pkg) {
  stopifnot(is.character(pkg), length(pkg) >= 1L)

  missing_pkgs <- pkg[!vapply(pkg, requireNamespace, logical(1), quietly = TRUE)]

  if (length(missing_pkgs) > 0) {
    msg <- paste0(
      "The following required package(s) are missing: ",
      paste(missing_pkgs, collapse = ", "),
      "\nPlease install them with:\n  install.packages(c(\"",
      paste(missing_pkgs, collapse = "\", \""),
      "\"))"
    )
    stop(msg, call. = FALSE)
  }

  invisible(TRUE)
}

.wass2s_model_p <- function(p) {
  if (is.null(p) || length(p) != 1L || !is.finite(p) || p < 1) {
    return(1L)
  }
  as.integer(max(1L, floor(p)))
}

.wass2s_model_n <- function(n_min) {
  if (is.null(n_min) || length(n_min) != 1L || !is.finite(n_min) || n_min < 2) {
    return(50L)
  }
  as.integer(max(2L, floor(n_min)))
}

.wass2s_int_range <- function(lower, upper, min_value = 1L) {
  lower <- as.integer(max(min_value, floor(lower)))
  upper <- as.integer(max(min_value, floor(upper)))
  if (upper < lower) upper <- lower
  c(lower, upper)
}

.wass2s_grid_regular_limited <- function(..., levels = 5, grid_max = 80) {
  levels <- as.integer(max(1L, floor(levels)))
  grid <- dials::grid_regular(..., levels = levels)
  if (!is.finite(grid_max) || nrow(grid) <= grid_max) {
    return(grid)
  }
  idx <- unique(as.integer(round(seq(1, nrow(grid), length.out = grid_max))))
  grid[idx, , drop = FALSE]
}

# parsnip specs (unifiées)
#' Model specification factory (parsnip)
#'
#' Return a \pkg{parsnip} model specification for a given lightweight,
#' CPU-friendly algorithm.
#'
#' @param name One of \code{SUPPORTED_MODELS}, e.g. \code{"rf"}, \code{"xgb"},
#'   \code{"mlp"}, \code{"kknn"}, \code{"svmlinear"}, \code{"mars"}, \code{"cubist"}.
#' @return A \pkg{parsnip} model specification (mode = regression).
#' @seealso \code{\link{model_grid}}
#' @keywords internal
#' @noRd
model_spec <- function(name,p=NULL) {
  name <- match.arg(name, SUPPORTED_MODELS)
  p <- .wass2s_model_p(p)
  pkg <- engine_pkg[[name]]
  .require_pkg(pkg)

  switch(name,
         rf = parsnip::rand_forest(
           mtry = tune(),
           min_n = tune(),
           trees = 1000
         ) |>
           parsnip::set_engine("ranger", importance = "impurity") |>
           parsnip::set_mode("regression"),

         # xgb = parsnip::boost_tree(
         #   trees = 1000,
         #   learn_rate    = tune(),
         #   tree_depth    = tune(),
         #   loss_reduction= tune(),
         #   mtry          = tune(),
         #   min_n         = tune()
         # ) |>
         #   parsnip::set_engine("xgboost") |>
         #   parsnip::set_mode("regression"),

         xgb = parsnip::boost_tree(
           trees = 500,
           learn_rate = tune(),
           tree_depth = tune(),
           mtry = min(p, max(1L, floor(sqrt(p)))),
           min_n = tune(),
           loss_reduction = tune(),
           sample_size = 0.7,
           stop_iter = 50
         ) |>
           parsnip::set_engine("xgboost") |>
           parsnip::set_mode("regression"),


         glmnet = parsnip::linear_reg(
           penalty = tune(),
           mixture = tune()) |>
           parsnip::set_engine("glmnet", standardize = TRUE) |>
           parsnip::set_mode("regression"),


         mlp = parsnip::mlp(
           hidden_units = tune(),
           penalty = tune(),
           epochs = tune()
         ) |>
           parsnip::set_engine("nnet", trace = FALSE, MaxNWts = 5000) |>
           parsnip::set_mode("regression"),

         kknn = parsnip::nearest_neighbor(
           neighbors = tune(),
           weight_func = tune()
         )|>
           parsnip::set_engine("kknn") |>
           parsnip::set_mode("regression"),

         svmlinear = parsnip::svm_linear(cost = tune()) |>
           parsnip::set_engine("kernlab") |>
           parsnip::set_mode("regression"),

         mars = parsnip::mars(
           num_terms = tune(),
           prod_degree = tune(),
           prune_method = "backward") |>
           parsnip::set_engine("earth") |>
           parsnip::set_mode("regression"),

         cubist = parsnip::cubist_rules(committees = tune(),
                                        neighbors = tune()) |>
           parsnip::set_engine("Cubist") |>
           parsnip::set_mode("regression")
  )
}

# Grilles d’hyperparamètres (dials)

#' Hyperparameter grid factory (dials)
#'
#' Construct a regular grid of hyperparameters for the selected model.
#'
#' @param name One of \code{SUPPORTED_MODELS}.
#' @param p Integer, number of predictors (used to bound \code{mtry} where relevant).
#' @param levels Integer number of levels per parameter (default: 5).
#' @return A tibble of hyperparameter combinations (dials grid).
#' @seealso \code{\link{model_spec}}
#' @keywords internal
#' @noRd
model_grid <- function(name, p, levels = 5, n_min = Inf, grid_max = 80) {
  name <- match.arg(name, SUPPORTED_MODELS)
  p <- .wass2s_model_p(p)
  n_min <- .wass2s_model_n(n_min)
  pkg <- engine_pkg[[name]]
  .require_pkg(pkg)
  cap <- function(x, m) max(1L, min(as.integer(x), as.integer(m)))

  switch(name,
         kknn = .wass2s_grid_regular_limited(
           dials::neighbors(range = .wass2s_int_range(3L, cap(25L, n_min - 1L))),
           dials::weight_func(values = c("rectangular", "triangular", "epanechnikov")),
           levels = levels,
           grid_max = grid_max
         ),

         mars = .wass2s_grid_regular_limited(
           dials::num_terms(range = .wass2s_int_range(2L, cap(20L, n_min - 1L))),
           dials::prod_degree(range = .wass2s_int_range(1L, 2L)),
           levels = levels,
           grid_max = grid_max
         ),

         rf = .wass2s_grid_regular_limited(
           dials::mtry(range = .wass2s_int_range(1L, p)),
           dials::min_n(range = .wass2s_int_range(
             min(5L, max(2L, floor(n_min / 3))),
             min(40L, max(2L, floor(n_min / 2)))
           )),
           levels = levels,
           grid_max = grid_max
         ),

         # xgb = dials::grid_regular(
         #   dials::learn_rate(range = c(-3, -1)),
         #   dials::tree_depth(range = c(2L, cap(6L, max(2L, floor(log2(n_min)))))),
         #   dials::loss_reduction(),
         #   dials::mtry(range = c(1L, max(1L, p))),
         #   dials::min_n(range = c(2L, cap(40L, max(2L, floor(n_min/2))))),
         #   levels = levels
         # ),

         xgb = .wass2s_grid_regular_limited(
           dials::learn_rate(range = c(-4, -2)),
           dials::tree_depth(range = .wass2s_int_range(1L, cap(3L, max(1L, floor(log2(n_min)))))),
           dials::min_n(range = .wass2s_int_range(
             min(5L, max(2L, floor(n_min / 3))),
             min(40L, max(2L, floor(n_min / 2)))
           )),
           dials::loss_reduction(range = c(-3, 1)),
           levels = levels,
           grid_max = grid_max
         ),

         glmnet = .wass2s_grid_regular_limited(
           dials::penalty(range = c(-6, 1)),
           dials::mixture(range = c(0, 1) ),
           levels = levels,
           grid_max = grid_max),

         mlp = .wass2s_grid_regular_limited(
           dials::hidden_units(range = .wass2s_int_range(1L, cap(4L, max(1L, floor(n_min / 5))))),
           dials::penalty(range = c(-4, -1)),
           dials::epochs(range = .wass2s_int_range(25L, 75L)),
           levels = levels,
           grid_max = grid_max
         ),

         svmlinear = .wass2s_grid_regular_limited(
           dials::cost(range = c(-6, 4)),
           levels = levels,
           grid_max = grid_max
         ),

         cubist = .wass2s_grid_regular_limited(
           rules::committees(range = .wass2s_int_range(1L, cap(25L, max(1L, floor(n_min / 3))))),
           dials::neighbors(range = .wass2s_int_range(0L, cap(9L, max(0L, n_min - 1L)), min_value = 0L)),
           levels = levels,
           grid_max = grid_max
         )
  )
}




#' Build a modeling recipe with cleaning, correlation filtering, normalization, and optional PCA
#'
#' Construct a robust \pkg{recipes} pipeline for a **numeric outcome** (annual mean
#' discharge). The recipe assigns roles, removes zero/near-zero variance predictors,
#' imputes missing values (median for numeric, optional mode for nominal),
#' optionally removes linear combinations, filters highly correlated predictors,
#' normalizes numeric predictors, and can apply PCA either explicitly or
#' **automatically when the number of predictors exceeds a threshold**.
#'
#' @section Behavior:
#' If neither \code{pca_num_comp} nor \code{pca_var_threshold} is set, the function
#' will automatically enable PCA when \code{length(predictors) > auto_pca_when_gt},
#' keeping enough components to reach \code{auto_pca_var_threshold} cumulative variance.
#' If either \code{pca_num_comp} or \code{pca_var_threshold} is provided, this explicit
#' setting takes precedence and auto-PCA is disabled.
#'
#' @param df A data frame or tibble containing \code{target} and \code{predictors}.
#' @param predictors Character vector of predictor column names. Nonexistent names
#'   are dropped silently; an error is thrown if none remain.
#' @param target Character scalar; outcome column name (annual mean discharge).
#'   Must be numeric. Default: \code{"Q"}.
#' @param corr_threshold Numeric in (0, 1); absolute correlation threshold used
#'   by \code{recipes::step_corr()}. Default: \code{0.90}.
#' @param corr_method Correlation method for \code{step_corr()}, typically
#'   \code{"pearson"} (default) or \code{"spearman"}.
#' @param impute_nominal Logical; if \code{TRUE}, apply \code{step_impute_mode()}
#'   to nominal predictors. Default: \code{TRUE}.
#' @param include_dummy Logical; if \code{TRUE}, expand nominal predictors via
#'   \code{step_dummy(one_hot = TRUE, keep_original_cols = FALSE)} before
#'   correlation filtering. Default: \code{FALSE}.
#' @param y_transform Character; one of \code{"none"} (default), \code{"log1p"}
#'   (applies \code{log(Q + 1)}) or \code{"yeo"} (Yeo–Johnson) for the outcome.
#'   The outcome is left untransformed by default.
#' @param pca_num_comp Integer or \code{NULL}; if provided, apply PCA with a fixed
#'   number of components (disables auto-PCA).
#' @param pca_var_threshold Numeric or \code{NULL}; if provided (e.g. \code{0.95}),
#'   apply PCA keeping enough components to reach the cumulative explained variance
#'   threshold (disables auto-PCA). Do not set together with \code{pca_num_comp}.
#' @param remove_linear_comb Logical; if \code{TRUE} (default), remove linear
#'   combinations using \code{step_lincomb()}.
#' @param auto_pca Logical; if \code{TRUE}, enable automatical PCA. Default: \code{TRUE}.
#' @param auto_pca_when_gt Integer; enable auto-PCA when the number of predictors
#'   is greater than this threshold. Default: \code{15}.
#' @param auto_pca_var_threshold Numeric in (0, 1); cumulative variance target used
#'   when auto-PCA is triggered. Default: \code{0.95}.
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
#' @param verbose Logical; if \code{TRUE}, emit informational messages.
#'   Default: \code{FALSE}.
#'
#' @details
#' Typical step order: \emph{imputation} \eqn{\rightarrow} (optional) \emph{dummy encoding}
#' \eqn{\rightarrow} \emph{correlation filtering} \eqn{\rightarrow} \emph{normalization}
#' \eqn{\rightarrow} (optional) \emph{PCA}. Correlation is computed after imputation.
#' Normalization does not affect pairwise correlation but is required before PCA.
#'
#' @return An unprepped \code{recipes::recipe} object suitable for use inside
#'   \pkg{workflows}/\pkg{tune}.
#'
#' @examples
#' \dontrun{
#' # Minimal example
#' rec <- make_recipe(
#'   df = data,
#'   predictors = c("x1","x2","x3","region"),
#'   target = "Q",
#'   corr_threshold = 0.9,
#'   include_dummy = TRUE
#' )
#' rec_prep <- recipes::prep(rec)
#'
#' # Force PCA to 10 components (disables auto-PCA)
#' rec_pca_fixed <- make_recipe(
#'   df = data,
#'   predictors = setdiff(names(data), "Q"),
#'   pca_num_comp = 10
#' )
#'
#' # Keep 95% cumulative variance via PCA (disables auto-PCA)
#' rec_pca_var <- make_recipe(
#'   df = data,
#'   predictors = setdiff(names(data), "Q"),
#'   pca_var_threshold = 0.95
#' )
#' }
#'
#' @seealso \code{\link[recipes]{recipe}}, \code{\link[recipes]{step_corr}},
#'   \code{\link[recipes]{step_pca}}, \code{\link[workflows]{workflow}}
#' @family modeling utilities
#' @keywords internal
make_recipe <- function(
    df,
    predictors,
    target = "Q",
    corr_threshold = 0.95,
    corr_method = c("pearson", "spearman", "kendall"),
    impute_nominal = TRUE,
    include_dummy = FALSE,
    y_transform = c("none", "log1p", "yeo"),
    pca_num_comp = NULL,
    pca_var_threshold = NULL,
    remove_linear_comb = FALSE,
    auto_pca = TRUE,
    auto_pca_when_gt = 15,
    auto_pca_var_threshold = 0.80,
    apply_impute = TRUE,
    apply_corr = TRUE,
    apply_normalize = TRUE,
    verbose = FALSE
) {
  y_transform <- match.arg(y_transform)
  corr_method <- match.arg(corr_method)

  # ---------------------------------------------------------------------------
  # Validation
  # ---------------------------------------------------------------------------

  if (!is.data.frame(df)) {
    stop("`df` must be a data.frame or tibble.", call. = FALSE)
  }

  if (!is.character(target) || length(target) != 1L || !nzchar(target)) {
    stop("`target` must be a non-empty character scalar.", call. = FALSE)
  }

  if (!target %in% names(df)) {
    stop(sprintf("Target '%s' not found in `df`.", target), call. = FALSE)
  }

  if (!is.numeric(df[[target]])) {
    stop(sprintf("Target '%s' must be numeric.", target), call. = FALSE)
  }

  if (!is.character(predictors) || length(predictors) < 1L) {
    stop("`predictors` must be a non-empty character vector.", call. = FALSE)
  }

  predictors <- unique(intersect(predictors, setdiff(names(df), target)))

  if (length(predictors) == 0L) {
    stop("No predictors found after intersection with `df` columns.", call. = FALSE)
  }

  if (!is.numeric(corr_threshold) || length(corr_threshold) != 1L ||
      is.na(corr_threshold) || corr_threshold <= 0 || corr_threshold >= 1) {
    stop("`corr_threshold` must be a single numeric value in (0, 1), e.g. 0.95.", call. = FALSE)
  }

  if (!is.null(pca_num_comp) && !is.null(pca_var_threshold)) {
    stop("Provide either `pca_num_comp` or `pca_var_threshold`, not both.", call. = FALSE)
  }

  if (!is.null(pca_num_comp)) {
    if (!is.numeric(pca_num_comp) || length(pca_num_comp) != 1L ||
        is.na(pca_num_comp) || pca_num_comp < 1) {
      stop("`pca_num_comp` must be a single positive integer.", call. = FALSE)
    }
    pca_num_comp <- as.integer(pca_num_comp)
  }

  if (!is.null(pca_var_threshold)) {
    if (!is.numeric(pca_var_threshold) || length(pca_var_threshold) != 1L ||
        is.na(pca_var_threshold) || pca_var_threshold <= 0 || pca_var_threshold >= 1) {
      stop("`pca_var_threshold` must be a single numeric value in (0, 1), e.g. 0.90.", call. = FALSE)
    }
  }

  if (!is.numeric(auto_pca_when_gt) || length(auto_pca_when_gt) != 1L ||
      is.na(auto_pca_when_gt) || auto_pca_when_gt < 1) {
    stop("`auto_pca_when_gt` must be a single positive integer.", call. = FALSE)
  }

  auto_pca_when_gt <- as.integer(auto_pca_when_gt)

  if (!is.numeric(auto_pca_var_threshold) || length(auto_pca_var_threshold) != 1L ||
      is.na(auto_pca_var_threshold) || auto_pca_var_threshold <= 0 || auto_pca_var_threshold >= 1) {
    stop("`auto_pca_var_threshold` must be a single numeric value in (0, 1), e.g. 0.80.", call. = FALSE)
  }

  if (!isTRUE(apply_impute) && isTRUE(impute_nominal)) {
    stop("`impute_nominal = TRUE` requires `apply_impute = TRUE`.", call. = FALSE)
  }

  if (y_transform == "log1p" && any(df[[target]] < -1, na.rm = TRUE)) {
    stop("`y_transform = 'log1p'` requires all target values to be >= -1.", call. = FALSE)
  }

  # Informative warnings for NA patterns when imputation is disabled
  if (!isTRUE(apply_impute)) {
    na_cols <- names(which(colSums(is.na(df[predictors])) > 0))
    if (length(na_cols) > 0 && isTRUE(verbose)) {
      message(
        "Imputation is disabled. Missing values detected in predictors: ",
        paste(na_cols, collapse = ", ")
      )
    }
  }

  if (isTRUE(verbose)) {
    message("Number of requested predictors: ", length(predictors))
    na_cols <- names(which(colSums(is.na(df[predictors])) > 0))
    if (length(na_cols) > 0) {
      message("Predictors containing NA values: ", paste(na_cols, collapse = ", "))
    }
  }

  # ---------------------------------------------------------------------------
  # Recipe skeleton
  # ---------------------------------------------------------------------------

  form <- stats::reformulate(termlabels = predictors, response = target)

  rec <- recipes::recipe(form, data = df) |>
    recipes::step_zv(recipes::all_predictors(), id = "zv") |>
    recipes::step_nzv(recipes::all_predictors(), id = "nzv")

  # ---------------------------------------------------------------------------
  # Optional imputation
  # ---------------------------------------------------------------------------

  if (isTRUE(apply_impute)) {
    rec <- rec |>
      recipes::step_impute_median(recipes::all_numeric_predictors(), id = "imp_num")

    if (isTRUE(impute_nominal)) {
      rec <- rec |>
        recipes::step_impute_mode(recipes::all_nominal_predictors(), id = "imp_nom")
    }
  }

  # ---------------------------------------------------------------------------
  # Optional linear-combination filtering
  # ---------------------------------------------------------------------------

  if (isTRUE(remove_linear_comb)) {
    rec <- rec |>
      recipes::step_lincomb(recipes::all_numeric_predictors(), id = "lincomb")
  }

  # ---------------------------------------------------------------------------
  # Optional dummy encoding
  # ---------------------------------------------------------------------------

  if (isTRUE(include_dummy)) {
    rec <- rec |>
      recipes::step_dummy(
        recipes::all_nominal_predictors(),
        one_hot = TRUE,
        id = "dummy"
      )
  }

  # ---------------------------------------------------------------------------
  # Optional correlation filter
  # ---------------------------------------------------------------------------

  if (isTRUE(apply_corr)) {
    rec <- rec |>
      recipes::step_corr(
        recipes::all_numeric_predictors(),
        threshold = corr_threshold,
        method = corr_method,
        id = "corr"
      )
  }

  # ---------------------------------------------------------------------------
  # Optional normalization
  # ---------------------------------------------------------------------------

  if (isTRUE(apply_normalize)) {
    rec <- rec |>
      recipes::step_normalize(recipes::all_numeric_predictors(), id = "norm")
  }

  # ---------------------------------------------------------------------------
  # Optional outcome transformation
  # ---------------------------------------------------------------------------

  if (y_transform == "log1p") {
    rec <- rec |>
      recipes::step_log(recipes::all_outcomes(), offset = 1, id = "y_log1p")
  } else if (y_transform == "yeo") {
    rec <- rec |>
      recipes::step_YeoJohnson(recipes::all_outcomes(), id = "y_yeo")
  }

  # ---------------------------------------------------------------------------
  # PCA logic
  # ---------------------------------------------------------------------------

  do_auto_pca <- isTRUE(auto_pca) &&
    is.null(pca_num_comp) &&
    is.null(pca_var_threshold) &&
    length(predictors) > auto_pca_when_gt

  if (do_auto_pca) {
    if (isTRUE(verbose)) {
      message(
        "Auto PCA enabled because the number of predictors (",
        length(predictors),
        ") exceeds `auto_pca_when_gt` (", auto_pca_when_gt,
        "). Retaining enough PCs to explain ",
        round(auto_pca_var_threshold * 100, 1),
        "% of the variance."
      )
    }

    rec <- rec |>
      recipes::step_pca(
        recipes::all_numeric_predictors(),
        threshold = auto_pca_var_threshold,
        id = "pca"
      )
  } else {
    if (!is.null(pca_num_comp)) {
      if (isTRUE(verbose)) {
        message("PCA enabled with fixed number of components: ", pca_num_comp)
      }

      rec <- rec |>
        recipes::step_pca(
          recipes::all_numeric_predictors(),
          num_comp = pca_num_comp,
          id = "pca"
        )
    } else if (!is.null(pca_var_threshold)) {
      if (isTRUE(verbose)) {
        message(
          "PCA enabled with explained variance threshold: ",
          round(pca_var_threshold * 100, 1), "%"
        )
      }

      rec <- rec |>
        recipes::step_pca(
          recipes::all_numeric_predictors(),
          threshold = pca_var_threshold,
          id = "pca"
        )
    } else {
      if (isTRUE(verbose)) {
        message("PCA disabled.")
      }
    }
  }

  rec
}
make_recipe__ <- function(
    df,
    predictors,
    target = "Q",
    corr_threshold = 0.99,
    corr_method = "pearson",
    impute_nominal = TRUE,
    include_dummy  = FALSE,
    y_transform = c("none", "log1p", "yeo"),
    pca_num_comp = NULL,
    pca_var_threshold = NULL,
    remove_linear_comb = FALSE,
    auto_pca = TRUE,
    auto_pca_when_gt = 15,
    auto_pca_var_threshold = 0.80,
    # --- NEW switches (for EOF-preprocessed inputs) ---
    apply_impute = TRUE,
    apply_corr = TRUE,
    apply_normalize = TRUE,
    verbose = FALSE
) {
  y_transform <- match.arg(y_transform)

  # ---- validation ----
  if (!is.data.frame(df)) stop("`df` must be a data.frame or tibble.", call. = FALSE)
  if (!is.character(target) || length(target) != 1L || !nzchar(target))
    stop("`target` must be a non-empty character scalar.", call. = FALSE)
  if (!target %in% names(df))
    stop(sprintf("make_recipe(): target '%s' not found in `df`.", target), call. = FALSE)

  if (y_transform == "log1p" && any(df[[target]] < -1, na.rm = TRUE)) {
    stop("`log1p` transformation requires target values >= -1.", call. = FALSE)
  }
  if (!is.numeric(df[[target]]))
    stop(sprintf("make_recipe(): target '%s' must be numeric.", target), call. = FALSE)
  if (!is.character(predictors) || length(predictors) < 1L)
    stop("`predictors` must be a non-empty character vector.", call. = FALSE)

  predictors <- intersect(predictors, setdiff(names(df), target))
  if (length(predictors) == 0L)
    stop("make_recipe(): no predictors found after intersection.", call. = FALSE)

  if (!is.numeric(corr_threshold) || corr_threshold < 0 || corr_threshold > 1)
    stop("`corr_threshold` must be in (0,1), e.g. 0.80.", call. = FALSE)
  if (!is.null(pca_num_comp) && !is.null(pca_var_threshold))
    stop("Provide either `pca_num_comp` OR `pca_var_threshold`, not both.", call. = FALSE)
  if (!is.null(pca_num_comp) && (!is.numeric(pca_num_comp) || length(pca_num_comp) != 1L || pca_num_comp < 1))
    stop("`pca_num_comp` must be a single positive integer.", call. = FALSE)
  if (!is.null(pca_var_threshold) && (!is.numeric(pca_var_threshold) || pca_var_threshold <= 0 || pca_var_threshold >= 1))
    stop("`pca_var_threshold` must be in (0,1), e.g. 0.95.", call. = FALSE)

  if (!isTRUE(apply_impute) && isTRUE(impute_nominal)) {
    stop("`impute_nominal = TRUE` requires `apply_impute = TRUE`.", call. = FALSE)
  }


  if (isTRUE(verbose)) {
    na_cols <- names(which(colSums(is.na(df[predictors])) > 0))
    if (length(na_cols)) message("NA columns among predictors: ", paste(na_cols, collapse = ", "))
  }

  # ---- recipe skeleton (FORMULA-BASED) ----
  # This prevents accidental inclusion of non-predictor columns like YYYY.
  rec <- recipes::recipe(
    stats::as.formula(paste(target, "~", paste(predictors, collapse = " + "))),
    data = df
  ) |>
    recipes::step_zv(recipes::all_predictors(), id = "zv") |>
    recipes::step_nzv(recipes::all_predictors(), id = "nzv")

  # ---- imputation (optional) ----
  if (isTRUE(apply_impute)) {
    rec <- rec |> recipes::step_impute_median(recipes::all_numeric_predictors(), id = "imp_num")
    if (isTRUE(impute_nominal)) {
      rec <- rec |> recipes::step_impute_mode(recipes::all_nominal_predictors(), id = "imp_nom")
    }
  }

  if (isTRUE(remove_linear_comb)) {
    rec <- rec |> recipes::step_lincomb(recipes::all_numeric_predictors(), id = "lincomb")
  }

  if (isTRUE(include_dummy)) {
    rec <- rec |> recipes::step_dummy(
      recipes::all_nominal_predictors(),
      one_hot = TRUE, keep_original_cols = FALSE, id = "dummy"
    )
  }

  # ---- corr filter + normalization (optional) ----
  if (isTRUE(apply_corr)) {
    rec <- rec |>
      recipes::step_corr(
        recipes::all_numeric_predictors(),
        threshold = corr_threshold, method = corr_method, id = "corr"
      )
  }

  if (isTRUE(apply_normalize)) {
    rec <- rec |> recipes::step_normalize(recipes::all_numeric_predictors(), id = "norm")
  }

  # outcome transform (optional)
  if (y_transform == "log1p") {
    rec <- rec |> recipes::step_log(recipes::all_outcomes(), offset = 1, id = "y_log1p")
  } else if (y_transform == "yeo") {
    rec <- rec |> recipes::step_YeoJohnson(recipes::all_outcomes(), id = "y_yeo")
  }

  # ---- PCA logic ----
  do_auto_pca <- is.null(pca_num_comp) && is.null(pca_var_threshold) &&
    length(predictors) > auto_pca_when_gt && auto_pca

  if (do_auto_pca) {
    if (isTRUE(verbose)) message("Auto PCA enabled (predictors > ", auto_pca_when_gt, ").")
    rec <- rec |> recipes::step_pca(
      recipes::all_numeric_predictors(),
      num_comp = as.integer(auto_pca_when_gt), id = "pca"
    )
  } else {
    if (!is.null(pca_num_comp)) {
      rec <- rec |> recipes::step_pca(
        recipes::all_numeric_predictors(),
        num_comp = as.integer(pca_num_comp), id = "pca"
      )
    } else if (!is.null(pca_var_threshold)) {
      rec <- rec |> recipes::step_pca(
        recipes::all_numeric_predictors(),
        threshold = pca_var_threshold, id = "pca"
      )
    }
  }

  rec
}

# make_recipe <- function(
#     df,
#     predictors,
#     target = "Q",
#     corr_threshold = 0.99,
#     corr_method = "pearson",
#     impute_nominal = TRUE,
#     include_dummy  = FALSE,
#     y_transform = c("none", "log1p", "yeo"),
#     pca_num_comp = NULL,
#     pca_var_threshold = NULL,
#     remove_linear_comb = FALSE,
#     auto_pca = TRUE,
#     auto_pca_when_gt = 15,
#     auto_pca_var_threshold = 0.80,
#     verbose = FALSE
# ) {
#   y_transform <- match.arg(y_transform)
#
#   # ---- validation ----
#   if (!is.data.frame(df)) stop("`df` must be a data.frame or tibble.", call. = FALSE)
#   if (!is.character(target) || length(target) != 1L || !nzchar(target))
#     stop("`target` must be a non-empty character scalar.", call. = FALSE)
#   if (!target %in% names(df))
#     stop(sprintf("make_recipe(): target '%s' not found in `df`.", target), call. = FALSE)
#   if (!is.numeric(df[[target]]))
#     stop(sprintf("make_recipe(): target '%s' must be numeric.", target), call. = FALSE)
#   if (!is.character(predictors) || length(predictors) < 1L)
#     stop("`predictors` must be a non-empty character vector.", call. = FALSE)
#
#   predictors <- intersect(predictors, setdiff(names(df), target))
#   if (length(predictors) == 0L)
#     stop("make_recipe(): no predictors found after intersection.", call. = FALSE)
#
#   if (!is.numeric(corr_threshold) || corr_threshold <= 0 || corr_threshold >= 1)
#     stop("`corr_threshold` must be in (0,1), e.g. 0.80.", call. = FALSE)
#   if (!is.null(pca_num_comp) && !is.null(pca_var_threshold))
#     stop("Provide either `pca_num_comp` OR `pca_var_threshold`, not both.", call. = FALSE)
#   if (!is.null(pca_num_comp) && (!is.numeric(pca_num_comp) || length(pca_num_comp) != 1L || pca_num_comp < 1))
#     stop("`pca_num_comp` must be a single positive integer.", call. = FALSE)
#   if (!is.null(pca_var_threshold) && (!is.numeric(pca_var_threshold) || pca_var_threshold <= 0 || pca_var_threshold >= 1))
#     stop("`pca_var_threshold` must be in (0,1), e.g. 0.95.", call. = FALSE)
#
#   if (isTRUE(verbose)) {
#     na_cols <- names(which(colSums(is.na(df[predictors])) > 0))
#     if (length(na_cols)) message("Imputation will be applied; NA columns: ", paste(na_cols, collapse = ", "))
#   }
#
#   # ---- recipe skeleton (FORMULA-BASED) ----
#   # This prevents accidental inclusion of non-predictor columns like YYYY.
#   rec <- recipes::recipe(
#     stats::as.formula(paste(target, "~", paste(predictors, collapse = " + "))),
#     data = df
#   ) |>
#     recipes::step_zv(recipes::all_predictors(), id = "zv") |>
#     recipes::step_nzv(recipes::all_predictors(), id = "nzv") |>
#     recipes::step_impute_median(recipes::all_numeric_predictors(), id = "imp_num")
#
#   if (isTRUE(impute_nominal)) {
#     rec <- rec |> recipes::step_impute_mode(recipes::all_nominal_predictors(), id = "imp_nom")
#   }
#   if (isTRUE(remove_linear_comb)) {
#     rec <- rec |> recipes::step_lincomb(recipes::all_numeric_predictors(), id = "lincomb")
#   }
#   if (isTRUE(include_dummy)) {
#     rec <- rec |> recipes::step_dummy(
#       recipes::all_nominal_predictors(),
#       one_hot = TRUE, keep_original_cols = FALSE, id = "dummy"
#     )
#   }
#
#   rec <- rec |>
#     recipes::step_corr(
#       recipes::all_numeric_predictors(),
#       threshold = corr_threshold, method = corr_method, id = "corr"
#     ) |>
#     recipes::step_normalize(recipes::all_numeric_predictors(), id = "norm")
#
#   # outcome transform (optional)
#   if (y_transform == "log1p") {
#     rec <- rec |> recipes::step_log(recipes::all_outcomes(), offset = 1, id = "y_log1p")
#   } else if (y_transform == "yeo") {
#     rec <- rec |> recipes::step_YeoJohnson(recipes::all_outcomes(), id = "y_yeo")
#   }
#
#   # ---- PCA logic ----
#   do_auto_pca <- is.null(pca_num_comp) && is.null(pca_var_threshold) &&
#     length(predictors) > auto_pca_when_gt && auto_pca
#
#   if (do_auto_pca) {
#     if (isTRUE(verbose)) message("Auto PCA enabled (predictors > ", auto_pca_when_gt, ").")
#     rec <- rec |> recipes::step_pca(
#       recipes::all_numeric_predictors(),
#       num_comp = as.integer(auto_pca_when_gt), id = "pca"
#     )
#   } else {
#     if (!is.null(pca_num_comp)) {
#       rec <- rec |> recipes::step_pca(
#         recipes::all_numeric_predictors(),
#         num_comp = as.integer(pca_num_comp), id = "pca"
#       )
#     } else if (!is.null(pca_var_threshold)) {
#       rec <- rec |> recipes::step_pca(
#         recipes::all_numeric_predictors(),
#         threshold = pca_var_threshold, id = "pca"
#       )
#     }
#   }
#
#   rec
# }

# make_recipe <- function(
#     df,
#     predictors,
#     target = "Q",
#     corr_threshold = 0.99,
#     corr_method = "pearson",
#     impute_nominal = TRUE,
#     include_dummy  = FALSE,
#     y_transform = c("none", "log1p", "yeo"),
#     pca_num_comp = NULL,
#     pca_var_threshold = NULL,
#     remove_linear_comb = FALSE,
#     auto_pca = TRUE,
#     auto_pca_when_gt = 15,
#     auto_pca_var_threshold = 0.80,
#     verbose = FALSE
# ) {
#   y_transform <- match.arg(y_transform)
#
#   # ---- validation ----
#   if (!is.data.frame(df)) stop("`df` must be a data.frame or tibble.", call. = FALSE)
#   if (!is.character(target) || length(target) != 1L || !nzchar(target))
#     stop("`target` must be a non-empty character scalar.", call. = FALSE)
#   if (!target %in% names(df))
#     stop(sprintf("make_recipe(): target '%s' not found in `df`.", target), call. = FALSE)
#   if (!is.numeric(df[[target]]))
#     stop(sprintf("make_recipe(): target '%s' must be numeric.", target), call. = FALSE)
#   if (!is.character(predictors) || length(predictors) < 1L)
#     stop("`predictors` must be a non-empty character vector.", call. = FALSE)
#
#   predictors <- intersect(predictors, setdiff(names(df), target))
#   if (length(predictors) == 0L)
#     stop("make_recipe(): no predictors found after intersection.", call. = FALSE)
#
#   if (!is.numeric(corr_threshold) || corr_threshold <= 0 || corr_threshold >= 1)
#     stop("`corr_threshold` must be in (0,1), e.g. 0.80.", call. = FALSE)
#   if (!is.null(pca_num_comp) && !is.null(pca_var_threshold))
#     stop("Provide either `pca_num_comp` OR `pca_var_threshold`, not both.", call. = FALSE)
#   if (!is.null(pca_num_comp) && (!is.numeric(pca_num_comp) || length(pca_num_comp) != 1L || pca_num_comp < 1))
#     stop("`pca_num_comp` must be a single positive integer.", call. = FALSE)
#   if (!is.null(pca_var_threshold) && (!is.numeric(pca_var_threshold) || pca_var_threshold <= 0 || pca_var_threshold >= 1))
#     stop("`pca_var_threshold` must be in (0,1), e.g. 0.95.", call. = FALSE)
#
#   if (isTRUE(verbose)) {
#     na_cols <- names(which(colSums(is.na(df[predictors])) > 0))
#     if (length(na_cols)) message("Imputation will be applied; NA columns: ", paste(na_cols, collapse = ", "))
#   }
#
#   # ---- recipe skeleton ----
#   rec <- recipes::recipe(df) |>
#     recipes::update_role(!!rlang::sym(target), new_role = "outcome") |>
#     recipes::update_role(dplyr::all_of(predictors), new_role = "predictor") |>
#     recipes::step_zv(recipes::all_predictors(), id = "zv") |>
#     recipes::step_nzv(recipes::all_predictors(), id = "nzv") |>
#     recipes::step_impute_median(recipes::all_numeric_predictors(), id = "imp_num")
#
#   if (isTRUE(impute_nominal)) {
#     rec <- rec |> recipes::step_impute_mode(recipes::all_nominal_predictors(), id = "imp_nom")
#   }
#   if (isTRUE(remove_linear_comb)) {
#     rec <- rec |> recipes::step_lincomb(recipes::all_numeric_predictors(), id = "lincomb")
#   }
#   if (isTRUE(include_dummy)) {
#     rec <- rec |> recipes::step_dummy(recipes::all_nominal_predictors(),
#                                       one_hot = TRUE, keep_original_cols = FALSE, id = "dummy")
#   }
#
#   rec <- rec |>
#     recipes::step_corr(recipes::all_numeric_predictors(),
#                        threshold = corr_threshold, method = corr_method, id = "corr") |>
#     recipes::step_normalize(recipes::all_numeric_predictors(), id = "norm")
#
#   # outcome transform (optional)
#   if (y_transform == "log1p") {
#     rec <- rec |> recipes::step_log(recipes::all_outcomes(), offset = 1, id = "y_log1p")
#   } else if (y_transform == "yeo") {
#     rec <- rec |> recipes::step_YeoJohnson(recipes::all_outcomes(), id = "y_yeo")
#   }
#
#   # ---- PCA logic ----
#   # If user explicitly set pca_ respect that.
#   # Else, auto-enable PCA when number of predictors > auto_pca_when_gt.
#   do_auto_pca <- is.null(pca_num_comp) && is.null(pca_var_threshold) &&
#     length(predictors) > auto_pca_when_gt && auto_pca
#
#   if (do_auto_pca) {
#     if (isTRUE(verbose)) message("Auto PCA enabled (predictors > ", auto_pca_when_gt, ").")
#     rec <- rec |> recipes::step_pca(recipes::all_numeric_predictors(),
#                                     num_comp = as.integer(auto_pca_when_gt), id = "pca")
#   } else {
#     if (!is.null(pca_num_comp)) {
#       rec <- rec |> recipes::step_pca(recipes::all_numeric_predictors(),
#                                       num_comp = as.integer(pca_num_comp), id = "pca")
#     } else if (!is.null(pca_var_threshold)) {
#       rec <- rec |> recipes::step_pca(recipes::all_numeric_predictors(),
#                                       threshold = pca_var_threshold, id = "pca")
#     }
#   }
#
#   rec
# }



#' Build a preprocessing recipe (role-based, no formula)
#'
#' @param df Data frame with `YYYY`, `Q`, and predictors.
#' @param predictors Character vector of predictor column names.
#' @param target Target column name (default `"Q"`).
#' @return A \code{recipes::recipe()}.
#' @keywords internal
#' @noRd
make_recipe_ <- function(df, predictors, target = "Q") {
  # garde-fous
  if (!target %in% names(df)) {
    stop(sprintf("make_recipe(): target '%s' not found in data.", target), call. = FALSE)
  }
  predictors <- intersect(predictors, setdiff(names(df), target))
  if (length(predictors) == 0L) {
    stop("make_recipe(): no predictors found after intersection.", call. = FALSE)
  }

  recipes::recipe(df) |>
    recipes::update_role(!!rlang::sym(target), new_role = "outcome") |>
    recipes::update_role(dplyr::all_of(predictors), new_role = "predictor") |>
    recipes::step_zv(recipes::all_predictors()) |>
    recipes::step_impute_median(recipes::all_predictors()) |>
    recipes::step_normalize(recipes::all_predictors())
}



