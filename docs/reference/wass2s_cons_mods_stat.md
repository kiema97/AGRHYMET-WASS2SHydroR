# Consolidate predictions across products for one statistical model

For a given basin and a fixed statistical model (PCR/Ridge/Lasso), this
function:

1.  fits/tunes one model per product using
    [`wass2s_tune_pred_stat()`](https://kiema97.github.io/WASS2SHydroR/reference/wass2s_tune_pred_stat.md),

2.  ranks products by cross-validated KGE,

3.  keeps the top-`K` products,

4.  fuses their predictions either by a robust KGE-weighted mean
    (default) or by an optional meta-learner ("sub-fuser") trained on
    the product predictions (when observations are available).

## Usage

``` r
wass2s_cons_mods_stat(
  basin_id,
  data_by_product,
  basin_col = "HYBAS_ID",
  target = "Q",
  date_col = "YYYY",
  pred_pattern_by_product = NULL,
  model = c("pcr", "ridge", "lasso"),
  grid = NULL,
  product_fusion_method = "median",
  sub_fuser = "rf",
  sub_grid_levels = 10,
  use_sub_fuser = TRUE,
  topK = 3,
  min_kge_model = 0.2,
  min_predictors = 1,
  min_data_required = 10,
  prediction_years = NULL,
  target_positive = TRUE,
  resamples = NULL,
  pretrained_wflow = NULL,
  init_frac = 0.8,
  assess_frac = 0.2,
  n_splits = NULL,
  cumulative = TRUE,
  quiet = TRUE,
  verbose = TRUE,
  allow_par = TRUE,
  verbose_tune = TRUE,
  selection_metric = c("kge", "rmse"),
  seed = 123,
  max_na_frac = 0.3,
  impute = "median",
  require_variance = TRUE,
  ...
)
```

## Arguments

- basin_id:

  Basin identifier.

- data_by_product:

  Named list of data frames (one per product). Each data frame must
  contain at least `basin_col`, `YYYY` (dates), `Q` (target), and
  predictor columns (typically prefixed, e.g. `pt_*`).

- basin_col:

  Column name for basin IDs (default: `"HYBAS_ID"`).

- target:

  Name of the target column (default: `"Q"`).

- date_col:

  Name of the date column (default: `"YYYY"`).

- pred_pattern_by_product:

  Optional. Either:

  - a named character vector/list mapping `product -> regex` used to
    select predictors, or

  - a single regex applied to all products.

  If `NULL`, defaults to `"^pt_"`.

- model:

  One of `"pcr"`, `"ridge"`, `"lasso"`.

- grid:

  Optional tibble of tuning parameters passed to
  [`wass2s_tune_pred_stat()`](https://kiema97.github.io/WASS2SHydroR/reference/wass2s_tune_pred_stat.md).
  If `NULL`, defaults are used.

- product_fusion_method:

  Character string specifying the fusion strategy. Supported values are:

  - `"meta"`: train a second-level learner on retained product
    predictions;

  - `"mean"`: simple arithmetic mean across retained products;

  - `"median"`: median across retained products;

  - `"weighted_mean"`: weighted mean using product performance scores;

  - `"best"`: keep only the best-ranked product.

- sub_fuser:

  Character; meta-learner model name passed to `model_spec()` (e.g.
  `"rf"`).

- sub_grid_levels:

  Integer; tuning grid "levels" for the sub-fuser, passed to
  `model_grid()`.

- use_sub_fuser:

  Logical; backward-compatible switch for enabling meta-fusion behavior
  in downstream fusion helpers.

- topK:

  Integer, number of products to keep for fusion (default: 3).

- min_kge_model:

  Numeric; minimum KGE threshold for products to receive non-zero fusion
  weight (default: 0.2).

- min_predictors:

  Integer; minimum number of predictors required to fit a product model
  (default: 1).

- min_data_required:

  Integer; minimum number of rows required to train a product model and
  (when enabled) the sub-fuser (default: 10).

- prediction_years:

  Optional numeric vector of length 2 giving the start and end years for
  a holdout prediction period. These years are excluded from training
  inside
  [`wass2s_tune_pred_stat()`](https://kiema97.github.io/WASS2SHydroR/reference/wass2s_tune_pred_stat.md)
  and predictions are generated on the concatenated (train + holdout)
  timeline after fitting.

- target_positive:

  Logical; if `TRUE`, force negative fused predictions to zero.

- resamples:

  Optional `rsample::rset` object for resampling passed to
  [`wass2s_tune_pred_stat()`](https://kiema97.github.io/WASS2SHydroR/reference/wass2s_tune_pred_stat.md).
  If `NULL`, rolling-origin resampling is created via
  [`make_rolling()`](https://kiema97.github.io/WASS2SHydroR/reference/wass2s_rolling_cv.md)
  inside
  [`wass2s_tune_pred_stat()`](https://kiema97.github.io/WASS2SHydroR/reference/wass2s_tune_pred_stat.md).

- pretrained_wflow:

  Optional
  [`workflows::workflow`](https://workflows.tidymodels.org/reference/workflow.html)
  object passed to
  [`wass2s_tune_pred_stat()`](https://kiema97.github.io/WASS2SHydroR/reference/wass2s_tune_pred_stat.md).
  If supplied, tuning is skipped and the workflow is used for prediction
  directly.

- init_frac:

  Fraction of rows used for the initial training window passed to
  [`wass2s_tune_pred_stat()`](https://kiema97.github.io/WASS2SHydroR/reference/wass2s_tune_pred_stat.md).

- assess_frac:

  Fraction of rows used for the assessment window passed to
  [`wass2s_tune_pred_stat()`](https://kiema97.github.io/WASS2SHydroR/reference/wass2s_tune_pred_stat.md).

- n_splits:

  Optional integer, desired number of resamples (splits) passed to
  [`wass2s_tune_pred_stat()`](https://kiema97.github.io/WASS2SHydroR/reference/wass2s_tune_pred_stat.md).

- cumulative:

  Logical; passed to
  [`make_rolling()`](https://kiema97.github.io/WASS2SHydroR/reference/wass2s_rolling_cv.md)
  (rolling-origin resampling).

- quiet:

  Logical; if `FALSE`, emits informative messages (passed to underlying
  tuning/prediction calls).

- verbose:

  Logical; if `TRUE`, emits diagnostic messages for product processing
  and failures (default: `TRUE`).

- allow_par:

  Logical. If `TRUE`, allow parallel execution during hyperparameter
  tuning of the meta-learner.

- verbose_tune:

  Logical; forwarded to
  [`wass2s_tune_pred_stat()`](https://kiema97.github.io/WASS2SHydroR/reference/wass2s_tune_pred_stat.md)
  to control tuning verbosity.

- selection_metric:

  Character; forwarded to
  [`wass2s_tune_pred_stat()`](https://kiema97.github.io/WASS2SHydroR/reference/wass2s_tune_pred_stat.md).
  The default `"kge"` keeps the historical statistical-model selection
  behavior.

- seed:

  Integer; random seed for reproducibility.

- max_na_frac:

  Numeric in \\\[0, 1\]\\; maximum allowed fraction of missing values
  per guarded column before stopping (default: 0.3).

- impute:

  Character; one of `"median"`, `"mean"`, or `"none"`. If `"none"`, no
  imputation is performed after the missingness guard (default:
  `"median"`).

- require_variance:

  Logical; if `TRUE`, stop when a guarded column has zero standard
  deviation after imputation (default: `TRUE`).

- ...:

  Additional arguments forwarded to
  [`wass2s_tune_pred_stat()`](https://kiema97.github.io/WASS2SHydroR/reference/wass2s_tune_pred_stat.md)
  and (when enabled) to tuning controls used internally.

## Value

A list with:

- `fused` (tibble): at minimum `YYYY` and `pred_fused`. If observed `Q`
  is available on the fused timeline, it may also be present (depending
  on upstream outputs).

- `leaderboard_products` (tibble): per-product diagnostics including
  `product`, `kge`, `rsq`, `n_pred`, `sd_pred`, and `weight` (final
  fusion weight; sums to 1 over selected products).

- `all_results` (list): all individual per-product results as returned
  by
  [`wass2s_tune_pred_stat()`](https://kiema97.github.io/WASS2SHydroR/reference/wass2s_tune_pred_stat.md)
  (or `NULL`/empty entries for skipped products are omitted).

## Details

Fusion weights are derived from KGE (negative KGE truncated to 0) and
normalized to sum to 1. Products with `kge < min_kge_model` receive a
weight of 0 (and fusion falls back gracefully if all weights become 0).

The function is defensive: it contains multiple safeguards and fallbacks
(e.g., missing predictors, insufficient data, failed tuning), and will
return a well-formed output with `NA` predictions rather than failing
silently.

## Examples

``` r
if (FALSE) { # \dontrun{
# Simple weighted fusion (recommended baseline)
cns <- wass2s_cons_mods_stat(
  basin_id = 1040021500,
  data_by_product = data_by_product,
  model = "pcr",
  topK = 3,
  use_sub_fuser = FALSE
)

# Meta-learner fusion (requires Q available in the merged timeline)
cns2 <- wass2s_cons_mods_stat(
  basin_id = 1040021500,
  data_by_product = data_by_product,
  model = "ridge",
  topK = 5,
  use_sub_fuser = TRUE,
  sub_fuser = "rf",
  sub_grid_levels = 10
)
} # }
```
