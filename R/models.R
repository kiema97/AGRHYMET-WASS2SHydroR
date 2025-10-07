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
  # map modèle -> package du moteur
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

  pkg <- engine_pkg[[name]]
  ok <- engine_is_available(pkg, strict = strict)

  # if (!engine_is_available(pkg, strict = strict)) {
  #   install.packages(pkg)
  #   return(NULL)
  # }
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


#' Meta-learner specification factory (parsnip)
#'
#' Return a \pkg{parsnip} specification for the final fusion model
#' (stacking/blending of consolidated ML outputs).
#'
#' @param name One of \code{SUPPORTED_FUSERS}, e.g. \code{"rf"}, \code{"xgb"},
#'   \code{"glmnet"}, \code{"kknn"}, \code{"svmLinear"}, \code{"gbm"}, \code{"mars"}, \code{"cubist"}.
#' @return A \pkg{parsnip} model specification (mode = regression).
#' @seealso \code{\link{meta_grid}}
#' @keywords internal
#' @noRd
meta_spec <- function(name) {
  name <- match.arg(name, SUPPORTED_FUSERS)

  switch(name,
         rf = parsnip::rand_forest(mtry = tune(), min_n = tune(), trees = 500) |>
           parsnip::set_engine("ranger") |> parsnip::set_mode("regression"),

         xgb = parsnip::boost_tree(
           trees = 800, learn_rate = tune(), tree_depth = tune(),
           loss_reduction = tune(), mtry = tune(), min_n = tune()
         ) |>
           parsnip::set_engine("xgboost") |> parsnip::set_mode("regression"),

         glmnet = parsnip::linear_reg(penalty = tune(), mixture = tune()) |>
           parsnip::set_engine("glmnet", standardize = TRUE) |> parsnip::set_mode("regression"),

         kknn = parsnip::nearest_neighbor(neighbors = tune(), weight_func = tune()) |>
           parsnip::set_engine("kknn") |> parsnip::set_mode("regression"),

         svmLinear = parsnip::svm_linear(cost = tune()) |>
           parsnip::set_engine("kernlab") |> parsnip::set_mode("regression"),

         # gbm = parsnip::boost_tree(
         #   trees = 800, learn_rate = tune(), tree_depth = tune(),
         #   loss_reduction = tune(), mtry = tune(), min_n = tune()
         # ) |>
         #   set_engine("gbm") |> set_mode("regression"),

         # lgbm = bonsai::boost_tree(
         #   trees = 1000,
         #   learn_rate    = tune(),
         #   tree_depth    = tune(),
         #   loss_reduction= tune(),
         #   mtry          = tune(),
         #   min_n         = tune()
         # ) |>
         #   parsnip::set_engine("lightgbm") |>
         #   parsnip::set_mode("regression"),

         mars = parsnip::mars(num_terms = tune(), prod_degree = tune(), prune_method = "backward") |>
           parsnip::set_engine("earth") |> parsnip::set_mode("regression"),

         cubist = parsnip::cubist_rules(committees = tune(), neighbors = tune()) |>
           parsnip::set_engine("Cubist") |> parsnip::set_mode("regression")
  )
}


#' Hyperparameter grid for meta-learner (dials)
#'
#' Build a tuning grid for the chosen fusion model.
#'
#' @param name One of \code{SUPPORTED_FUSERS}.
#' @param p Integer, number of meta-features (number of consolidated model columns).
#' @param levels Integer number of levels per parameter (default: 5).
#' @return A tibble of hyperparameter combinations.
#' @seealso \code{\link{meta_spec}}
#' @keywords internal
#' @noRd
meta_grid <- function(name, p, levels = 5) {
  name <- match.arg(name, SUPPORTED_FUSERS)

  switch(name,
         rf = dials::grid_regular(dials::mtry(range = c(1L, max(1L, p))), dials::min_n(), levels = levels),
         xgb = dials::grid_regular(
           dials::learn_rate(range = c(-3, -1)),
           dials::tree_depth(range = c(2L, 6L)),
           dials::loss_reduction(),
           dials::mtry(range = c(1L, max(1L, p))),
           dials::min_n(),
           levels = levels
         ),

         glmnet = dials::grid_regular(
           dials::penalty(range = c(-6, 1)),
           dials::mixture(range = c(0, 1)),
           levels = levels),

         kknn = dials::grid_regular(
           dials::neighbors(range = c(3L, 25L)),
           dials::weight_func(),
           levels = levels),

         svmLinear = dials::grid_regular(
           dials::cost(range = c(-6, 4)),
           levels = levels),
         # lgbm = dials::grid_regular(
         #   dials::learn_rate(range = c(-3, -1)),
         #   dials::tree_depth(range = c(2L, 6L)),
         #   dials::loss_reduction(),
         #   dials::mtry(range = c(1L, max(1L, p))),
         #   dials::min_n(),
         #   levels = levels
         # ),
         mars = dials::grid_regular(
           dials::num_terms(range = c(4L, 40L)),
           dials::prod_degree(range = c(1L, 2L)),
           levels = levels),

         cubist = dials::grid_regular(
           rules::committees(range = c(1L, 50L)),
           dials::neighbors(range = c(0L, 9L)),
           levels = levels)
  )
}



#' Build a preprocessing recipe (role-based, no formula)
#'
#' @param df Data frame with `YYYY`, `Q`, and predictors.
#' @param predictors Character vector of predictor column names.
#' @param target Target column name (default `"Q"`).
#' @return A \code{recipes::recipe()}.
#' @keywords internal
#' @noRd
make_recipe <- function(df, predictors, target = "Q") {
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



