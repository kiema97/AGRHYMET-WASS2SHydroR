# Tune and refit a single product/model, select configuration by KGE

Performs rolling-origin resampling, tunes hyperparameters (PCR threshold
or glmnet penalty), collects predictions, computes KGE per split, and
selects the configuration with best mean KGE (falls back to RMSE when
needed).

## Usage

``` r
wass2s_tune_pred_stat(
  df_basin_product,
  predictors,
  target = "Q",
  date_col = "YYYY",
  id_col = NULL,
  model = c("pcr", "ridge", "lasso"),
  prediction_years = NULL,
  auto_pca = TRUE,
  pca_num_comp = NULL,
  pca_var_threshold = NULL,
  apply_impute = TRUE,
  apply_corr = TRUE,
  apply_normalize = TRUE,
  impute_nominal = TRUE,
  target_positive = TRUE,
  resamples = NULL,
  pretrained_wflow = NULL,
  grid = NULL,
  min_predictors = 1,
  min_data_required = 10,
  init_frac = 0.6,
  assess_frac = 0.2,
  n_splits = NULL,
  cumulative = TRUE,
  quiet = TRUE,
  allow_par = TRUE,
  verbose_tune = TRUE,
  selection_metric = c("kge", "rmse"),
  max_na_frac = 0.3,
  impute = "median",
  require_variance = TRUE
)
```

## Arguments

- df_basin_product:

  Data frame for a single basin/product with columns \`YYYY\`, \`Q\`,
  and predictor columns.

- predictors:

  Character vector of predictor column names.

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

- model:

  One of \`"pcr"\`, \`"ridge"\`, \`"lasso"\`.

- prediction_years:

  Optional numeric vector of length 2 giving the start and end years for
  a holdout prediction period. These years are excluded from training
  and predictions are generated after fitting.

- auto_pca:

  Logical; if `TRUE`, enable automatical PCA. Default: `TRUE`.

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

- target_positive:

  Logical; if TRUE, force negative predictions to zero.

- resamples:

  Optional `rsample::rset` object for resampling. If `NULL`, a
  rolling-origin resampling is created via
  [`make_rolling()`](https://kiema97.github.io/AGRHYMET-WASS2SHydroR/reference/wass2s_rolling_cv.md).

- pretrained_wflow:

  Optional
  [`workflows::workflow`](https://workflows.tidymodels.org/reference/workflow.html)
  object. If supplied, tuning is skipped and the workflow is fitted
  directly.

- grid:

  Optional tibble of tuning parameters. If supplied, this grid is used
  instead of the default grid for the specified model.

- min_predictors:

  Minimum number of predictors required to keep a product.

- min_data_required:

  Minimum number of rows required to train.

- init_frac:

  Fraction of rows used for the initial training window.

- assess_frac:

  Fraction of rows used for the assessment window.

- n_splits:

  Optional integer, desired number of resamples (splits).

- cumulative:

  Logical; passed to
  [`rsample::rolling_origin()`](https://rsample.tidymodels.org/reference/rolling_origin.html).

- quiet:

  Logical; if `FALSE`, emits informative messages.

- allow_par:

  A logical to allow parallel processing (if a parallel backend is
  registered).

- verbose_tune:

  A logical for logging results (other than warnings and errors, which
  are always shown) as they are generated during training in a single R
  process.

- selection_metric:

  Character; `"kge"` keeps the historical behavior and selects the
  configuration with the best cross-validated KGE. `"rmse"` selects by
  RMSE while reporting KGE/RMSE/MAE for that same configuration.

- max_na_frac:

  Numeric in \\\[0, 1\]\\: maximum allowed fraction of missing values
  per column before stopping (default `0.20` = 20%).

- impute:

  Character, one of `"median"`, `"mean"`, or `"none"`. If `"none"`, no
  imputation is performed after the guard (default `"median"`).

- require_variance:

  Logical; if `TRUE`, stop when a column has zero standard deviation
  after imputation (default `TRUE`).

## Value

A list with:

- \`kge_cv_mean\` (numeric): mean KGE across splits for the best config.

- \`preds\` (tibble): columns \`YYYY\`, \`pred\` from the refitted
  model.

- \`leaderboard_cfg\` (tibble): per-config mean KGE, descending.

## See also

\[make_recipe()\], \[make_rolling()\], \[wf_pcr()\], \[wf_ridge()\],
\[wf_lasso()\]

## Examples

``` r
if (FALSE) { # \dontrun{
res <- wass2s_tune_pred_stat(df, c("pt_1","pt_2","pt_3"), "ridge")
} # }
```
