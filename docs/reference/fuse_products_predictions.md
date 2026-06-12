# Fuse predictions from multiple products into a single forecast

This function combines product-level predictions into a single fused
prediction series using one of several fusion strategies: meta-learning,
simple mean, median, performance-based weighted mean, or best-product
selection.

## Usage

``` r
fuse_products_predictions(
  results,
  dates_all,
  topK = 3,
  min_score = 0.2,
  prediction_years = NULL,
  product_fusion_method = c("median", "mean", "meta", "weighted_mean", "best"),
  sub_fuser = "rf",
  sub_grid_levels = 5,
  min_data_required = 10,
  target_positive = TRUE,
  quiet = TRUE,
  verbose = TRUE,
  use_sub_fuser = TRUE,
  seed = 123,
  ...
)
```

## Arguments

- results:

  A list of product-level results. Each element must typically contain:

  - `product`: product name;

  - `score`: product performance score used for ranking and weighting;

  - `preds`: a data frame containing at least `YYYY` and `pred`, and
    optionally `Q`.

- dates_all:

  A vector of all dates to retain in the final fused output. Dates are
  internally standardized to `YYYYMMDD` format.

- topK:

  Integer. Number of best products retained before applying the fusion
  method.

- min_score:

  Numeric. Minimum score floor used when computing weights for the
  `"weighted_mean"` fusion strategy. This prevents zero or negative
  weights from dominating the weighted fusion.

- prediction_years:

  Optional numeric vector of length 2 specifying the prediction period
  boundaries. Values can be provided as `YYYY` or `YYYYMMDD`. This
  argument is mainly used when `product_fusion_method = "meta"` to
  define the holdout period excluded from sub-fuser training.

- product_fusion_method:

  Character string specifying the fusion strategy. Supported values are:

  - `"meta"`: train a second-level learner on retained product
    predictions;

  - `"mean"`: simple arithmetic mean across retained products;

  - `"median"`: median across retained products;

  - `"weighted_mean"`: weighted mean using product performance scores;

  - `"best"`: keep only the best-ranked product.

- sub_fuser:

  Character. Meta-model used when `product_fusion_method = "meta"`. Must
  be supported by `model_spec()` and `model_grid()`.

- sub_grid_levels:

  Integer. Number of levels used to generate the tuning grid for the
  meta-fuser.

- min_data_required:

  Integer. Minimum number of training observations required to fit the
  meta-fuser. If not met, the function falls back to the `"median"`
  strategy.

- target_positive:

  Logical. If `TRUE`, fused predictions are constrained to be
  non-negative using `pmax(pred_fused, 0)`.

- quiet:

  Logical. If `TRUE`, suppress most informational messages.

- verbose:

  Logical. If `TRUE`, allow progress and fallback messages through the
  internal messaging helper.

- seed:

  Integer random seed for reproducibility.

- ...:

  Additional arguments reserved for future extensions.

## Value

A list with the following elements:

- `fused`: a data frame containing `YYYY`, `pred_fused`, and, when
  available, `Q`;

- `leaderboard_products`: a ranked data frame of products with their
  scores and assigned weights;

- `all_results`: the filtered input results retained internally;

- `product_fusion_method`: the fusion strategy effectively used. This
  may differ from the requested one if an automatic fallback occurred.

## Details

Product-level results are expected to come from upstream model training
steps, with each product contributing predicted values over time and an
associated performance score (for example cross-validated KGE).

The function first filters the input results to retain only products
with usable predictions. It then builds a product leaderboard using the
supplied performance scores and keeps the top `topK` products.

For simple fusion strategies (`"mean"`, `"median"`, `"weighted_mean"`,
`"best"`), predictions are combined directly across retained products.

For `"meta"`, a second-level model is trained using retained product
predictions as predictors and observed values `Q` as the outcome. If the
meta-fuser cannot be trained (for example because of insufficient data,
missing `Q`, or fitting failure), the function automatically falls back
to `"median"`.

Product weights used by `"weighted_mean"` are derived from the retained
product scores after applying a lower bound defined by `min_score`.

## Examples

``` r
if (FALSE) { # \dontrun{
# Median fusion across the best 3 products
out <- fuse_products_predictions(
  results = results_std,
  dates_all = dates_all,
  topK = 3,
  product_fusion_method = "median"
)

# Weighted mean fusion
out <- fuse_products_predictions(
  results = results_std,
  dates_all = dates_all,
  topK = 3,
  product_fusion_method = "weighted_mean"
)

# Best-product strategy
out <- fuse_products_predictions(
  results = results_std,
  dates_all = dates_all,
  topK = 1,
  product_fusion_method = "best"
)

# Meta-fusion
out <- fuse_products_predictions(
  results = results_std,
  dates_all = dates_all,
  topK = 3,
  product_fusion_method = "meta",
  sub_fuser = "rf"
)
} # }
```
