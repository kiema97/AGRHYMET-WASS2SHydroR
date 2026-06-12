# Fuse predictions across products (top-K), with optional sub-fuser

Fuse predictions across products (top-K), with optional sub-fuser

## Usage

``` r
fuse_products_predictions_(
  results,
  dates_all,
  topK = 3,
  min_score = 0.2,
  prediction_years = NULL,
  use_sub_fuser = FALSE,
  sub_fuser = "rf",
  sub_grid_levels = 5,
  min_data_required = 10,
  target_positive = TRUE,
  quiet = TRUE,
  verbose = TRUE,
  seed = 123,
  ...
)
```
