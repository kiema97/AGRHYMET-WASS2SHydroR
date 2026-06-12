# Tune and predict for one product and one ML model

This function performs cross-validation tuning for a single (basin,
product) dataset using a specified machine learning model. It ranks
model configurations by KGE (computed from CV predictions), refits the
best configuration on the full dataset, and returns fitted predictions
for both training and optional holdout (prediction) years.

## Usage

``` r
wass2s_tune_pred_ml(
  df_basin_product,
  predictors,
  target = "Q",
  date_col = "YYYY",
  id_col = NULL,
  prediction_years = NULL,
  y_transform = c("none", "log1p", "yeo"),
  include_dummy = FALSE,
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
  init_frac = 0.8,
  assess_frac = 0.2,
  n_splits = 3,
  cumulative = TRUE,
  quiet = TRUE,
  target_positive = TRUE,
  allow_par = TRUE,
  verbose_tune = TRUE,
  selection_metric = c("rmse", "kge"),
  max_na_frac = 0.3,
  impute = "median",
  require_variance = TRUE,
  min_data_required = 10
)
```

## Arguments

- df_basin_product:

  A data frame containing at least the columns `YYYY`, `Q`, and
  predictor variables.

- predictors:

  Character vector of predictor names to use.

- target:

  Name of the target column (default: \`"Q"\`).

- date_col:

  Name of the date column (default: \`"YYYY"\`).

- id_col:

  Optional. Name of an identifier column (e.g. basin, station, or
  subbasin ID). If provided, this column is used together with
  `date_col` to uniquely identify each time series and to guarantee
  correct alignment of predictions with the input data. This is strongly
  recommended when `df_basin_product` contains stacked data from
  multiple basins or stations.

  If `NULL` (default), the function assumes that `df_basin_product`
  contains a single time series and only uses `date_col` for temporal
  ordering.

- prediction_years:

  Optional numeric vector of length 2 giving the start and end years for
  a holdout prediction period. These years

- y_transform:

  Character; one of `"none"` (default), `"log1p"` (applies `log(Q + 1)`)
  or `"yeo"` (Yeo–Johnson) for the outcome. The outcome is left
  untransformed by default.

- include_dummy:

  Logical; if `TRUE`, expand nominal predictors via
  `step_dummy(one_hot = TRUE, keep_original_cols = FALSE)` before
  correlation filtering. Default: `FALSE`.

- corr_threshold:

  Numeric in (0, 1); absolute correlation threshold used by
  [`recipes::step_corr()`](https://recipes.tidymodels.org/reference/step_corr.html).
  Default: `0.90`.

- corr_method:

  Correlation method for `step_corr()`, typically `"pearson"` (default)
  or `"spearman"`.

- auto_pca:

  Logical; if `TRUE`, enable automatical PCA. Default: `TRUE`.

- auto_pca_when_gt:

  Integer; enable auto-PCA when the number of predictors is greater than
  this threshold. Default: `15`.

- pca_num_comp:

  Integer or `NULL`; if provided, apply PCA with a fixed number of
  components (disables auto-PCA).

- pca_var_threshold:

  Numeric or `NULL`; if provided (e.g. `0.95`), apply PCA keeping enough
  components to reach the cumulative explained variance are excluded
  from training and predictions are generated after fitting.

- apply_impute:

  Logical; controls whether missing value imputation is applied to
  predictor variables. When \`TRUE\` (default), numeric predictors are
  imputed using median imputation via
  [`recipes::step_impute_median()`](https://recipes.tidymodels.org/reference/step_impute_median.html),
  and nominal predictors (if `impute_nominal = TRUE`) are imputed using
  [`recipes::step_impute_mode()`](https://recipes.tidymodels.org/reference/step_impute_mode.html).

  This argument should typically be set to \`FALSE\` when predictors
  have already been preprocessed upstream (e.g., EOF/PCA transformation
  with prior imputation), in order to avoid redundant transformations
  and preserve reproducibility of the preprocessing pipeline.

- apply_corr:

  Logical; indicates whether a correlation-based filtering step is
  applied to numeric predictors. When \`TRUE\` (default), highly
  correlated predictors are removed using
  [`recipes::step_corr()`](https://recipes.tidymodels.org/reference/step_corr.html)
  with the specified `corr_threshold` and `corr_method`.

  Setting this argument to \`FALSE\` is recommended when predictors have
  already undergone dimensionality reduction (e.g., EOF or PCA
  preprocessing), as the correlation structure has typically been
  addressed upstream.

- apply_normalize:

  Logical; controls whether numeric predictors are standardized using
  [`recipes::step_normalize()`](https://recipes.tidymodels.org/reference/step_normalize.html).
  When \`TRUE\` (default), predictors are centered and scaled prior to
  modeling.

  This argument can be set to \`FALSE\` when predictors have already
  been normalized during a prior preprocessing stage (e.g., EOF/PCA
  computation), ensuring that the same scaling is not applied multiple
  times and maintaining consistency across modeling workflows.

- impute_nominal:

  Logical; if `TRUE`, apply `step_impute_mode()` to nominal predictors.
  Default: `TRUE`.

- model:

  One of `SUPPORTED_MODELS`, e.g. \`"rf"\`, \`"xgb"\`, \`"mlp"\`.

- resamples:

  Optional `rsample::rset` object for resampling. If `NULL`, a
  rolling-origin resampling is created via
  [`make_rolling()`](https://kiema97.github.io/AGRHYMET-WASS2SHydroR/reference/wass2s_rolling_cv.md).

- grid_levels:

  Number of grid levels per parameter (default: 5).

- seed:

  Random seed for reproducibility.

- pretrained_wflow:

  Optional
  [`workflows::workflow`](https://workflows.tidymodels.org/reference/workflow.html)
  object. If supplied, tuning is skipped and the workflow is fitted
  directly.

- init_frac:

  Fraction of rows used for the initial training window (default 0.60).
  A hard minimum of 8 rows is enforced when possible.

- assess_frac:

  Fraction of rows used for the assessment window (default 0.20). A hard
  minimum of 3 rows is enforced when possible.

- n_splits:

  Optional integer, desired number of resamples (splits). If `NULL`
  (default), every possible split is produced (`skip = 0`).

- cumulative:

  Logical; passed to
  [`rsample::rolling_origin()`](https://rsample.tidymodels.org/reference/rolling_origin.html)
  (default `TRUE`).

- quiet:

  Logical; if `FALSE`, emits informative messages when the requested
  `n_splits` cannot be reached (default `TRUE`).

- target_positive:

  Logical; if TRUE, force negative predictions to zero.

- allow_par:

  A logical to allow parallel processing (if a parallel backend is
  registered).

- verbose_tune:

  A logical for logging results (other than warnings and errors, which
  are always shown) as they are generated during training in a single R
  process.

- selection_metric:

  Character; `"rmse"` keeps the RMSE-selected configuration while
  reporting the KGE of that same configuration. `"kge"` selects the
  configuration with the best cross-validated KGE and uses RMSE only to
  retrieve the corresponding tuned parameters.

- max_na_frac:

  Numeric in \\\[0, 1\]\\: maximum allowed fraction of missing values
  per column before stopping (default `0.20` = 20%).

- impute:

  Character, one of `"median"`, `"mean"`, or `"none"`. If `"none"`, no
  imputation is performed after the guard (default `"median"`).

- require_variance:

  Logical; if `TRUE`, stop when a column has zero standard deviation
  after imputation (default `TRUE`).

- min_data_required:

  Minimum number of rows required to train.

## Value

A list with the following elements:

- `kge_cv_mean` Mean KGE of the best configuration across CV splits.

- `preds` Tibble with columns `YYYY, pred` containing predictions from
  the final fit (training + optional holdout).

- `fit` Final fitted workflow object.

- `leaderboard_cfg` Tibble of model configurations ranked by KGE.

- `param_grid` Tibble of parameters used for model training.

## Details

\- The target column is always standardized internally to `Q` and the
date column to `YYYY` before modeling. - If `prediction_years` is given,
those years are removed from the training data and used as an
out-of-sample prediction set. - By default, model configurations are
selected using RMSE because direct KGE optimization may be unstable for
some ML learners. The reported KGE is always the KGE of the selected
configuration.

## Examples

``` r
if (FALSE) { # \dontrun{
# Example with toy data
df <- tibble::tibble(
  YYYY = 1990:2000,
  Q = rnorm(11, 1000, 200),
  x1 = rnorm(11), x2 = rnorm(11)
)

res <- wass2s_tune_pred_ml(
  df_basin_product = df,
  predictors = c("x1", "x2"),
  model = "rf",
  grid_levels = 3
)

res$kge_cv_mean
res$preds
} # }
```
