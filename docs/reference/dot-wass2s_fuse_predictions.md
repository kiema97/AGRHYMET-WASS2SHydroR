# Internal unified fusion engine

Internal unified fusion engine

## Usage

``` r
.wass2s_fuse_predictions(
  fused_models,
  basin_id,
  target = "Q",
  date_col = "YYYY",
  prediction_years = NULL,
  fusion_method = c("meta", "mean", "median", "weighted_mean"),
  final_fuser = "rf",
  grid_levels = 5,
  quiet = TRUE,
  verbose_tune = TRUE,
  allow_par = TRUE,
  target_positive = FALSE
)
```
