# Consolidate top-K products for one basin and one ML model

For a given basin and ML model, this function:

1.  fits/tunes the model on each product using
    [`wass2s_tune_pred_ml()`](https://kiema97.github.io/AGRHYMET-WASS2SHydroR/reference/wass2s_tune_pred_ml.md),

2.  ranks products by cross-validated KGE,

3.  keeps the top-`K` products,

4.  fuses their predictions either by a robust KGE-weighted mean
    (default), or by an optional meta-learner ("sub-fuser") trained on
    product predictions when observed `Q` is available.

## Usage

``` r
wass2s_cons_mods_ml(
  data_by_product,
  basin_id,
  hybas_id = "HYBAS_ID",
  target = "Q",
  date_col = "YYYY",
  pred_pattern_by_product = NULL,
  model = SUPPORTED_MODELS,
  topK = 3,
  target_positive = TRUE,
  min_kge_model = -Inf,
  prediction_years = NULL,
  product_fusion_method = "median",
  sub_fuser = "rf",
  sub_grid_levels = 10,
  use_sub_fuser = TRUE,
  pretrained = NULL,
  grid_levels = 5,
  min_data_required = 10,
  predictors_min = 1,
  quiet = TRUE,
  verbose = TRUE,
  allow_par = TRUE,
  selection_metric = c("rmse", "kge"),
  max_na_frac = 0.3,
  impute = "median",
  require_variance = TRUE,
  seed = 123,
  ...
)
```

## Arguments

- data_by_product:

  Named list of data frames/tibbles per product.

- basin_id:

  Basin identifier value.

- hybas_id:

  Name of the basin ID column (default: "HYBAS_ID").

- target:

  Name of the target column (default: "Q").

- date_col:

  Name of the date column (default: "YYYY").

- pred_pattern_by_product:

  Optional named list/vector of regex by product to select predictors,
  or a single regex applied to all products. Default is "^pt\_".

- model:

  One of `SUPPORTED_MODELS`.

- topK:

  Integer; number of best products to fuse (default: 3).

- target_positive:

  Logical; if `TRUE`, force negative fused predictions to zero.

- min_kge_model:

  Minimum KGE threshold to accept non-zero fusion weight (default:
  -Inf).

- prediction_years:

  Optional numeric vector of length 2 (start_year, end_year) defining a
  holdout period excluded from training (for the sub-fuser training
  step).

- product_fusion_method:

  Character string specifying the fusion strategy. Supported values are:

  - `"meta"`: train a second-level learner on retained product
    predictions;

  - `"mean"`: simple arithmetic mean across retained products;

  - `"median"`: median across retained products;

  - `"weighted_mean"`: weighted mean using product performance scores;

  - `"best"`: keep only the best-ranked product.

- sub_fuser:

  Character. Meta-model used when `fusion_method = "meta"`. Must be
  supported by `model_spec()` and `model_grid()`.

- sub_grid_levels:

  Integer; grid levels for the sub-fuser via `model_grid()`.

- use_sub_fuser:

  Logical; backward-compatible switch for enabling meta-fusion behavior
  in downstream fusion helpers.

- pretrained:

  Optional list of pre-trained workflows (indexed by `model` then
  `product`).

- grid_levels:

  Tuning grid granularity for the base ML model (passed to
  [`wass2s_tune_pred_ml()`](https://kiema97.github.io/AGRHYMET-WASS2SHydroR/reference/wass2s_tune_pred_ml.md)).

- min_data_required:

  Minimum number of rows required to train.

- predictors_min:

  Minimum number of predictors required.

- quiet:

  Logical; if FALSE, emits informative messages (default: TRUE).

- verbose:

  Logical; if TRUE, emits diagnostic messages (default: TRUE).

- allow_par:

  Logical; forwarded to tuning controls where applicable.

- selection_metric:

  Character; forwarded to
  [`wass2s_tune_pred_ml()`](https://kiema97.github.io/AGRHYMET-WASS2SHydroR/reference/wass2s_tune_pred_ml.md).
  The default `"rmse"` keeps RMSE tuning but reports product KGE for the
  same selected configuration.

- max_na_frac:

  Numeric in \[0,1\]; maximum allowed missingness per guarded column
  (default: 0.3).

- impute:

  Character; one of "median", "mean", "none" (default: "median").

- require_variance:

  Logical; if TRUE, requires non-zero variance after guard (default:
  TRUE).

- seed:

  Integer; random seed for reproducibility.

- ...:

  Passed to the underlying tuner/predictor.

## Value

A list with:

- `fused`: tibble with columns `YYYY`, `pred_fused` (and `Q` if
  available).

- `leaderboard_products`: tibble of products with KGE and fusion
  weights.

- `all_results`: list of per-product results (raw outputs).

## Details

Fusion weights are derived from KGE (negative KGE truncated to 0), then
normalized to sum to 1 over the selected products. Products below
`min_kge_model` can be given weight 0, with safe fallbacks.
