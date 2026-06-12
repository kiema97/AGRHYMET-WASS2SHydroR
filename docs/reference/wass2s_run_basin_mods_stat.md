# Run basin-level statistical model consolidation and fusion

This function consolidates statistical models (PCR, RIDGE, LASSO) for a
given basin and applies a final fusion strategy to combine their
predictions into a single forecast.

## Usage

``` r
wass2s_run_basin_mods_stat(
  basin_id,
  data_by_product,
  hybas_id = "HYBAS_ID",
  target = "Q",
  date_col = "YYYY",
  pred_pattern_by_product = NULL,
  prediction_years = NULL,
  topK = 3,
  product_fusion_method = "median",
  fusion_method = c("meta", "mean", "median", "weighted_mean"),
  final_fuser = "rf",
  grid_levels = 5,
  sub_fuser = NULL,
  sub_grid_levels = NULL,
  quiet = TRUE,
  verbose_tune = TRUE,
  target_positive = TRUE,
  allow_par = TRUE,
  min_kge_model = 0.2,
  max_na_frac = 0.3,
  impute = "median",
  require_variance = TRUE,
  ...
)
```

## Arguments

- basin_id:

  Integer or character identifier of the basin to process.

- data_by_product:

  A nested list containing predictor data grouped by product. This
  structure is typically produced upstream in the WASS2SHydroR workflow.

- hybas_id:

  Character. Name of the basin identifier column. Default is
  `"HYBAS_ID"`.

- target:

  Character. Name of the target variable (e.g., streamflow). Default is
  `"Q"`.

- date_col:

  Character. Name of the temporal column. Default is `"YYYY"`.

- pred_pattern_by_product:

  Optional named list defining predictor selection patterns for each
  product.

- prediction_years:

  Optional numeric vector of length 2 specifying the prediction period
  (start, end). Can be provided as years or YYYYMMDD format.

- topK:

  Integer. Number of best predictors/products retained per model.

- product_fusion_method:

  Character string specifying the fusion strategy. Supported values are:

  - `"meta"`: train a second-level learner on retained product
    predictions;

  - `"mean"`: simple arithmetic mean across retained products;

  - `"median"`: median across retained products;

  - `"weighted_mean"`: weighted mean using product performance scores;

  - `"best"`: keep only the best-ranked product.

- fusion_method:

  Character string specifying the final fusion strategy. Supported
  values are:

  - `"meta"`: train a meta-learner on the consolidated model
    predictions;

  - `"mean"`: use the simple arithmetic mean across consolidated
    predictions;

  - `"median"`: use the median across consolidated predictions;

  - `"weighted_mean"`: use a performance-based weighted mean, where
    weights are derived from the Kling-Gupta Efficiency (KGE) computed
    on the training subset.

- final_fuser:

  Character. Meta-learner used when `fusion_method = "meta"`. Must be
  one of `SUPPORTED_FUSERS`.

- grid_levels:

  Integer. Number of levels used to generate hyperparameter grids for
  tuning the meta-learner.

- sub_fuser:

  Optional meta-learner used to consolidate top-K products within each
  statistical model. Defaults to `final_fuser`.

- sub_grid_levels:

  Optional grid size for the within-model product fuser. Defaults to
  `grid_levels`.

- quiet:

  Logical. If `TRUE`, suppress informational messages.

- verbose_tune:

  Logical. If `TRUE`, print tuning progress.

- target_positive:

  Logical. If `TRUE`, constrain final predictions to be non-negative
  using `pmax(pred, 0)`.

- allow_par:

  Logical. If `TRUE`, allow parallel execution during hyperparameter
  tuning of the meta-learner.

- min_kge_model:

  Minimum KGE threshold used when selecting products/models for
  consolidation.

- max_na_frac:

  Numeric. Maximum fraction of missing values allowed per variable.

- impute:

  Character. Imputation method for missing values. Default is
  `"median"`.

- require_variance:

  Logical. If `TRUE`, remove predictors with no variance.

- ...:

  Additional arguments passed to
  [`wass2s_cons_mods_stat()`](https://kiema97.github.io/AGRHYMET-WASS2SHydroR/reference/wass2s_cons_mods_stat.md).

## Value

A list containing:

- `fused_by_model`: Data frame containing observed values and model
  predictions, including the final fused prediction (`pred_final`);

- `final_test`: The last available row of the fused output;

- `scores`: Performance metrics (KGE, RMSE) for training and testing
  subsets;

- `scores_train`: Performance metrics on the training subset;

- `scores_test`: Performance metrics on the testing subset;

- `fusion_method`: Fusion strategy effectively used;

- `fusion_weights`: Named numeric vector of weights when using
  `"weighted_mean"`, otherwise `NULL`;

- `leaderboards`: Per-model ranking of selected predictors/products;

- `cv_rs`: Cross-validation metrics from meta-learner tuning (only when
  `fusion_method = "meta"`);

- `best_meta_params`: Best hyperparameters selected for the
  meta-learner, if applicable.

## Details

The function supports multiple fusion strategies including simple
averaging, median aggregation, performance-based weighted averaging, and
meta-learning.

The function proceeds in four main steps:

1.  Build statistical models (PCR, RIDGE, LASSO) using available
    predictors;

2.  Extract and align observed data (`Q`) across time;

3.  Merge model predictions into a unified table indexed by date;

4.  Apply a final fusion method to produce a single prediction series.

When `fusion_method = "weighted_mean"`, model weights are computed from
their training performance using the Kling-Gupta Efficiency (KGE).

When `fusion_method = "meta"`, a second-level regression model is
trained using the consolidated model outputs as predictors.

## Examples

``` r
if (FALSE) { # \dontrun{
res <- wass2s_run_basin_mods_stat(
  basin_id = 1050915990,
  data_by_product = data_by_product,
  fusion_method = "median"
)

res <- wass2s_run_basin_mods_stat(
  basin_id = 1050915990,
  data_by_product = data_by_product,
  fusion_method = "weighted_mean"
)

res <- wass2s_run_basin_mods_stat(
  basin_id = 1050915990,
  data_by_product = data_by_product,
  fusion_method = "meta",
  final_fuser = "rf"
)
} # }
```
