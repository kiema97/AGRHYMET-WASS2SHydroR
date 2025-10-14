# ---- Models supported -------------------------------------------------------
#' Supported meta-learners for final fusion (identifiers)
#'
#' Character vector of model keys allowed in \code{meta_spec()} and \code{meta_grid()}.
#'
#' @format Character vector.
#' @keywords internal

SUPPORTED_FUSERS <- c("rf","xgb","glmnet","kknn","svmLinear","mars","cubist")


#' Supported ML models
#'
#' Character vector of model keys allowed in \code{meta_spec()} and \code{meta_grid()}.
#'
#' @format Character vector.
#' @keywords internal

SUPPORTED_MODELS <- c("rf","xgb","mlp","kknn","svmLinear","mars","cubist")

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
  svmLinear = "kernlab",
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

# parsnip specs (unifiées)
#' Model specification factory (parsnip)
#'
#' Return a \pkg{parsnip} model specification for a given lightweight,
#' CPU-friendly algorithm.
#'
#' @param name One of \code{SUPPORTED_MODELS}, e.g. \code{"rf"}, \code{"xgb"},
#'   \code{"lgbm"}, \code{"mlp"}, \code{"kknn"}, \code{"svmLinear"}, \code{"mars"}, \code{"cubist"}.
#' @return A \pkg{parsnip} model specification (mode = regression).
#' @seealso \code{\link{model_grid}}
#' @keywords internal
#' @noRd
model_spec <- function(name, strict = TRUE) {
  name <- match.arg(name, SUPPORTED_MODELS)
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

         xgb = parsnip::boost_tree(
           trees = 1000,
           learn_rate    = tune(),
           tree_depth    = tune(),
           loss_reduction= tune(),
           mtry          = tune(),
           min_n         = tune()
         ) |>
           parsnip::set_engine("xgboost") |>
           parsnip::set_mode("regression"),

         # gbm = parsnip::boost_tree(
         #   trees = 1000,
         #   learn_rate    = tune(),
         #   tree_depth    = tune(),
         #   loss_reduction= tune(),
         #   mtry          = tune(),
         #   min_n         = tune()
         # ) |>
         #   set_engine("gbm") |>
         #   set_mode("regression"),

         # lgbm = parsnip::boost_tree(
         #   trees = 1000,
         #   learn_rate    = tune(),
         #   tree_depth    = tune(),
         #   loss_reduction= tune(),
         #   mtry          = tune(),
         #   min_n         = tune()
         # ) |>
         #   parsnip::set_engine("lightgbm") |>
         #   parsnip::set_mode("regression"),

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
           parsnip::set_engine("nnet") |>
           parsnip::set_mode("regression"),

         kknn = parsnip::nearest_neighbor(
           neighbors = tune(),
           weight_func = tune()
         )|>
           parsnip::set_engine("kknn") |>
           parsnip::set_mode("regression"),

         svmLinear = parsnip::svm_linear(cost = tune()) |>
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
model_grid <- function(name, p, levels = 5, n_min = Inf) {
  name <- match.arg(name, SUPPORTED_MODELS)
  pkg <- engine_pkg[[name]]
  .require_pkg(pkg)
  # helper borne
  cap <- function(x, m) max(1L, min(x, m))

  switch(name,
         kknn = dials::grid_regular(
           dials::neighbors(range = c(3L, cap(25L, n_min - 1L))),
           dials::weight_func(),
           levels = levels
         ),

         mars = dials::grid_regular(
           dials::num_terms(range = c(4L, cap(40L, n_min - 1L))),
           dials::prod_degree(range = c(1L, 2L)),
           levels = levels
         ),

         rf = dials::grid_regular(
           dials::mtry(range = c(1L, max(1L, p))),
           dials::min_n(range = c(2L, cap(40L, max(2L, floor(n_min/2))))),
           levels = levels
         ),

         xgb = dials::grid_regular(
           dials::learn_rate(range = c(-3, -1)),
           dials::tree_depth(range = c(2L, cap(6L, max(2L, floor(log2(n_min)))))),
           dials::loss_reduction(),
           dials::mtry(range = c(1L, max(1L, p))),
           dials::min_n(range = c(2L, cap(40L, max(2L, floor(n_min/2))))),
           levels = levels
         ),

         lgbm = dials::grid_regular(
           dials::learn_rate(range = c(-3, -1)),
           dials::tree_depth(range = c(2L, cap(6L, max(2L, floor(log2(n_min)))))),
           dials::loss_reduction(),
           dials::mtry(range = c(1L, max(1L, p))),
           dials::min_n(range = c(2L, cap(40L, max(2L, floor(n_min/2))))),
           levels = levels
         ),

         glmnet = dials::grid_regular(
           dials::penalty(range = c(-6, 1)),
           dials::mixture(range = c(0, 1) ),
           levels = levels),

         mlp = dials::grid_regular(
           dials::hidden_units(range = c(2L, cap(20L, max(2L, n_min - 2L)))),
           dials::penalty(range = c(-6, -2)),
           dials::epochs(range = c(50L, 200L)),
           levels = levels
         ),

         svmLinear = dials::grid_regular(
           dials::cost(range = c(-6, 4)),
           levels = levels
         ),

         cubist = dials::grid_regular(
           rules::committees(range = c(1L, cap(50L, max(1L, floor(n_min/2))))),
           dials::neighbors(range = c(0L, cap(9L, max(0L, n_min - 1L)))),
           levels = levels
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
#' @param auto_pca_when_gt Integer; enable auto-PCA when the number of predictors
#'   is greater than this threshold. Default: \code{15}.
#' @param auto_pca_var_threshold Numeric in (0, 1); cumulative variance target used
#'   when auto-PCA is triggered. Default: \code{0.95}.
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
    corr_threshold = 0.70,
    corr_method = "pearson",
    impute_nominal = TRUE,
    include_dummy  = FALSE,
    y_transform = c("none", "log1p", "yeo"),
    pca_num_comp = NULL,
    pca_var_threshold = NULL,
    remove_linear_comb = TRUE,
    auto_pca_when_gt = 15,
    auto_pca_var_threshold = 0.80,
    verbose = FALSE
) {
  y_transform <- match.arg(y_transform)

  # ---- validation ----
  if (!is.data.frame(df)) stop("`df` must be a data.frame or tibble.", call. = FALSE)
  if (!is.character(target) || length(target) != 1L || !nzchar(target))
    stop("`target` must be a non-empty character scalar.", call. = FALSE)
  if (!target %in% names(df))
    stop(sprintf("make_recipe(): target '%s' not found in `df`.", target), call. = FALSE)
  if (!is.numeric(df[[target]]))
    stop(sprintf("make_recipe(): target '%s' must be numeric.", target), call. = FALSE)
  if (!is.character(predictors) || length(predictors) < 1L)
    stop("`predictors` must be a non-empty character vector.", call. = FALSE)

  predictors <- intersect(predictors, setdiff(names(df), target))
  if (length(predictors) == 0L)
    stop("make_recipe(): no predictors found after intersection.", call. = FALSE)

  if (!is.numeric(corr_threshold) || corr_threshold <= 0 || corr_threshold >= 1)
    stop("`corr_threshold` must be in (0,1), e.g. 0.90.", call. = FALSE)
  if (!is.null(pca_num_comp) && !is.null(pca_var_threshold))
    stop("Provide either `pca_num_comp` OR `pca_var_threshold`, not both.", call. = FALSE)
  if (!is.null(pca_num_comp) && (!is.numeric(pca_num_comp) || length(pca_num_comp) != 1L || pca_num_comp < 1))
    stop("`pca_num_comp` must be a single positive integer.", call. = FALSE)
  if (!is.null(pca_var_threshold) && (!is.numeric(pca_var_threshold) || pca_var_threshold <= 0 || pca_var_threshold >= 1))
    stop("`pca_var_threshold` must be in (0,1), e.g. 0.95.", call. = FALSE)

  if (isTRUE(verbose)) {
    na_cols <- names(which(colSums(is.na(df[predictors])) > 0))
    if (length(na_cols)) message("Imputation will be applied; NA columns: ", paste(na_cols, collapse = ", "))
  }

  # ---- recipe skeleton ----
  rec <- recipes::recipe(df) |>
    recipes::update_role(!!rlang::sym(target), new_role = "outcome") |>
    recipes::update_role(dplyr::all_of(predictors), new_role = "predictor") |>
    recipes::step_zv(recipes::all_predictors(), id = "zv") |>
    recipes::step_nzv(recipes::all_predictors(), id = "nzv") |>
    recipes::step_impute_median(recipes::all_numeric_predictors(), id = "imp_num")

  if (isTRUE(impute_nominal)) {
    rec <- rec |> recipes::step_impute_mode(recipes::all_nominal_predictors(), id = "imp_nom")
  }
  if (isTRUE(remove_linear_comb)) {
    rec <- rec |> recipes::step_lincomb(recipes::all_numeric_predictors(), id = "lincomb")
  }
  if (isTRUE(include_dummy)) {
    rec <- rec |> recipes::step_dummy(recipes::all_nominal_predictors(),
                                      one_hot = TRUE, keep_original_cols = FALSE, id = "dummy")
  }

  rec <- rec |>
    recipes::step_corr(recipes::all_numeric_predictors(),
                       threshold = corr_threshold, method = corr_method, id = "corr") |>
    recipes::step_normalize(recipes::all_numeric_predictors(), id = "norm")

  # outcome transform (optional)
  if (y_transform == "log1p") {
    rec <- rec |> recipes::step_log(recipes::all_outcomes(), offset = 1, id = "y_log1p")
  } else if (y_transform == "yeo") {
    rec <- rec |> recipes::step_YeoJohnson(recipes::all_outcomes(), id = "y_yeo")
  }

  # ---- PCA logic ----
  # If user explicitly set pca_ respect that.
  # Else, auto-enable PCA when number of predictors > auto_pca_when_gt.
  do_auto_pca <- is.null(pca_num_comp) && is.null(pca_var_threshold) &&
    length(predictors) > auto_pca_when_gt

  if (do_auto_pca) {
    if (isTRUE(verbose)) message("Auto PCA enabled (predictors > ", auto_pca_when_gt, ").")
    rec <- rec |> recipes::step_pca(recipes::all_numeric_predictors(),
                                    num_comp = as.integer(auto_pca_when_gt), id = "pca")
  } else {
    if (!is.null(pca_num_comp)) {
      rec <- rec |> recipes::step_pca(recipes::all_numeric_predictors(),
                                      num_comp = as.integer(pca_num_comp), id = "pca")
    } else if (!is.null(pca_var_threshold)) {
      rec <- rec |> recipes::step_pca(recipes::all_numeric_predictors(),
                                      threshold = pca_var_threshold, id = "pca")
    }
  }

  rec
}



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



