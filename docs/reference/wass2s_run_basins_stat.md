# Run the statistical method across multiple basins

Iterates over basin ids, fits PCR/Ridge/Lasso for each product, fuses
top-K products by KGE, then trains a meta-learner on model outputs.
Supports parallel execution per basin.

## Usage

``` r
wass2s_run_basins_stat(
  data_by_product,
  hybas_id = "HYBAS_ID",
  pred_pattern_by_product = NULL,
  topK = 3,
  final_fuser = "rf",
  grid_levels = 5,
  product_fusion_method = "median",
  fusion_method = c("meta", "mean", "median", "weighted_mean"),
  basins = NULL,
  parallel = FALSE,
  workers = 4,
  quiet = TRUE,
  verbose_tune = TRUE,
  target_positive = TRUE,
  allow_par = TRUE,
  max_na_frac = 0.3,
  impute = "median",
  require_variance = TRUE,
  ...
)
```

## Arguments

- data_by_product:

  Named list of data frames (one per product).

- hybas_id:

  Column name for basin IDs (default: \`"HYBAS_ID"\`).

- pred_pattern_by_product:

  Named character vector: product -\> regex to select predictor columns
  for that product.

- topK:

  Integer, number of top products to keep in the fusion (by KGE).

- final_fuser:

  Name of the meta-learner to use (subset of `SUPPORTED_FUSERS`).

- grid_levels:

  Integer. Number of levels used to generate hyperparameter grids for
  tuning the meta-learner.

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

- basins:

  Optional vector of basin IDs to process; default uses all found.

- parallel:

  Logical, run basins in parallel using furrr.

- workers:

  Integer, number of parallel workers when \`parallel = TRUE\`.

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

- max_na_frac:

  Numeric. Maximum fraction of missing values allowed per variable.

- impute:

  Character. Imputation method for missing values. Default is
  `"median"`.

- require_variance:

  Logical. If `TRUE`, remove predictors with no variance.

- ...:

  Additional arguments passed to `wass2s_run_basin_mods_stat`.

## Value

Named list keyed by basin id, each element the result of
\`wass2s_run_basin_mods_stat()\`.

## See also

\[wass2s_run_basin_mods_stat()\], \[wass2s_cons_mods_stat()\]

## Examples

``` r
if (FALSE) { # \dontrun{
res <- wass2s_run_basins_stat(data_by_product, pred_pattern_by_product = c(SST_CMCC="^pt_"))
} # }
```
