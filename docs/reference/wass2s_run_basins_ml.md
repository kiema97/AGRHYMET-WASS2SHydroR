# Run the Hydro + ML pipeline for all basins

Iterate over all basin IDs, running
[`wass2s_run_bas_mod_ml()`](https://kiema97.github.io/WASS2SHydroR/reference/wass2s_run_bas_mod_ml.md)
for each, optionally in parallel. Returns a named list indexed by basin
ID.

## Usage

``` r
wass2s_run_basins_ml(
  data_by_product,
  hybas_id = "HYBAS_ID",
  pred_pattern_by_product = NULL,
  prediction_years = NULL,
  models = SUPPORTED_MODELS,
  topK = 3,
  min_kge_model = -Inf,
  basins = NULL,
  parallel = FALSE,
  workers = 4,
  grid_levels = 5,
  product_fusion_method = "median",
  fusion_method = c("meta", "mean", "median", "weighted_mean"),
  final_fuser = "rf",
  quiet = TRUE,
  target_positive = TRUE,
  allow_par = TRUE,
  selection_metric = c("rmse", "kge"),
  ...
)
```

## Arguments

- data_by_product:

  Named list of data frames (one per product).

- hybas_id:

  Name of the basin ID column.

- pred_pattern_by_product:

  Optional per-product regex for predictor selection.

- prediction_years:

  Optional numeric vector of length 2 giving the start and end years for
  a holdout prediction period. These years

- models:

  Character vector of base models to include.

- topK:

  Integer, number of best products per base model.

- min_kge_model:

  Minimum best KGE required to keep a base model.

- basins:

  Optional vector of basin IDs to subset (default: all).

- parallel:

  Logical; if `TRUE`, uses furrr for parallel execution.

- workers:

  Integer number of workers when `parallel = TRUE`.

- grid_levels:

  Grid density for tuning.

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

  Name of the meta-learner for final fusion.

- quiet:

  Logical; if `FALSE`, emits informative messages.

- ...:

  Other parameters passed to `wass2s_run_bas_mod_ml`.

## Value

A named list: one element per basin, each the list returned by
[`wass2s_run_bas_mod_ml()`](https://kiema97.github.io/WASS2SHydroR/reference/wass2s_run_bas_mod_ml.md).

## Examples

``` r
# res <- wass2s_run_basins_ml(data_by_product = lst, models = c("rf","xgb"), final_fuser = "glmnet")
```
