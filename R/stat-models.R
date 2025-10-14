#' Build a PCR workflow (with PCA threshold tuning)
#'
#' @param rec A `recipes` recipe.
#' @return A `workflows` workflow (`lm` model with PCA preprocessor).
#' @keywords internal
wf_pcr <- function(rec){
  workflows::workflow() |>
    workflows::add_recipe(rec |>
                            recipes::step_pca(recipes::all_predictors(), threshold = tune::tune())
    ) |>
    workflows::add_model(parsnip::linear_reg() |> parsnip::set_engine("lm"))
}

#' Build a Ridge workflow (glmnet)
#'
#' @param rec A `recipes` recipe.
#' @return A `workflows` workflow for ridge regression.
#' @keywords internal
wf_ridge <- function(rec){
  workflows::workflow() |>
    workflows::add_recipe(rec) |>
    workflows::add_model(parsnip::linear_reg(penalty = tune::tune(), mixture = 0) |>
                           parsnip::set_engine("glmnet"))
}

#' Build a Lasso workflow (glmnet)
#'
#' @param rec A `recipes` recipe.
#' @return A `workflows` workflow for lasso regression.
#' @keywords internal
wf_lasso <- function(rec){
  workflows::workflow() |>
    workflows::add_recipe(rec) |>
    workflows::add_model(parsnip::linear_reg(penalty = tune::tune(), mixture = 1) |>
                           parsnip::set_engine("glmnet"))
}

#' Grid for PCA threshold (variance explained)
#'
#' @return A `dials` grid for `threshold` in [0.70, 0.995].
#' @keywords internal
grid_pcr <- function(){
  dials::grid_regular(dials::threshold(range = c(0.70, 0.995)),
                      levels = 8)
}

#' Grid for glmnet penalties
#'
#' @return A `dials` grid for `penalty` (~ 1e-6 .. 10).
#' @keywords internal
grid_glm <- function(){
  dials::grid_regular(dials::penalty(range = c(-6, 1)),
                      levels = 30)  # ~1e-6..10
}
