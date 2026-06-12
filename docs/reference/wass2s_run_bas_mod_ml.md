# Run basin-level machine learning model consolidation and final fusion

This function consolidates predictions from multiple machine learning
models for a given basin and applies a final fusion strategy across the
retained consolidated model outputs. The final fusion can be based on a
meta-learner, a simple mean, a median, or a performance-based weighted
mean.

## Usage

``` r
wass2s_run_bas_mod_ml(
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
  product_fusion_method = "median",
  fusion_method = c("meta", "mean", "median", "weighted_mean"),
  final_fuser = "rf",
  quiet = TRUE,
  verbose_tune = TRUE,
  target_positive = TRUE,
  allow_par = TRUE,
  selection_metric = c("rmse", "kge"),
  max_na_frac = 0.3,
  impute = "median",
  require_variance = TRUE,
  ...
)
```

## Arguments

- data_by_product:

  Named list of data frames (one per product).

- basin_id:

  Basin identifier value.

- prediction_years:

  Optional numeric vector of length 2 giving the start and end years for
  a holdout prediction period. These years

- target:

  Name of the target column (default: \`"Q"\`).

- date_col:

  Name of the date column (default: \`"YYYY"\`).

- hybas_id:

  Name of the basin ID column.

- pred_pattern_by_product:

  Optional per-product regex to select predictors.

- models:

  Character vector of base models to run (subset of `SUPPORTED_MODELS`).

- topK:

  Integer, number of best products per base model (default: 3).

- min_kge_model:

  Minimum best KGE required to keep a base model for the basin.

- grid_levels:

  Grid density for tuning both base models and meta-learner.

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

  Name of the meta-learner to use (subset of `SUPPORTED_FUSERS`).

- quiet:

  Logical; if `FALSE`, emits informative messages.

- verbose_tune:

  A logical for logging results (other than warnings and errors, which
  are always shown) as they are generated during training in a single R
  process.

- target_positive:

  Logical. If `TRUE`, final fused predictions are constrained to be
  non-negative using `pmax(pred, 0)`. This is useful for hydrological
  targets such as streamflow, which are physically non-negative.

- allow_par:

  Logical. If `TRUE`, parallel execution is allowed during
  hyperparameter tuning of the meta-learner when
  `fusion_method = "meta"`. If `FALSE`, tuning is forced to run
  sequentially.

- max_na_frac:

  Numeric in \\\[0, 1\]\\: maximum allowed fraction of missing values
  per column before stopping (default `0.20` = 20%).

- impute:

  Character, one of `"median"`, `"mean"`, or `"none"`. If `"none"`, no
  imputation is performed after the guard (default `"median"`).

- require_variance:

  Logical; if `TRUE`, stop when a column has zero standard deviation
  after imputation (default `TRUE`).

- ...:

  Passed to
  [`WASS2SHydroR::wass2s_cons_mods_ml`](https://kiema97.github.io/WASS2SHydroR/reference/wass2s_cons_mods_ml.md).

## Value

A list containing:

- `fused_by_model`: data frame containing observed values and the
  consolidated predictions, along with the final fused prediction
  (`pred_final`);

- `final_test`: the last available row of the fused output table;

- `scores`: combined performance summary for training and testing
  subsets;

- `scores_train`: performance metrics computed on the training subset;

- `scores_test`: performance metrics computed on the testing subset;

- `fusion_method`: the fusion strategy effectively used;

- `fusion_weights`: named numeric vector of weights when
  `fusion_method = "weighted_mean"`, otherwise `NULL`;

- `leaderboards`: per-model leaderboards returned from the consolidation
  stage;

- `cv_rs`: tuning metrics collected from the meta-learner when
  `fusion_method = "meta"`, otherwise `NULL`;

- `best_meta_params`: best hyperparameter combination selected for the
  meta-learner when applicable, otherwise `NULL`.

## Details

The function first consolidates predictions separately for each
requested machine learning model across available products. The
resulting consolidated predictions are then merged into a single table
aligned by date.

A final fusion step is subsequently applied using the method selected
through `fusion_method`. When `fusion_method = "meta"`, a second-level
regression model is trained on the consolidated predictions. When
`fusion_method = "weighted_mean"`, model weights are computed from
basin-specific training performance using the Kling-Gupta Efficiency
(KGE).

## Examples

``` r
# wass2s_run_bas_mod_ml(basin_id = 1, data_by_product = lst, models = c("rf","xgb"))
```
