# Internal helper to run meta-fusion

Internal helper to run meta-fusion

## Usage

``` r
.wass2s_run_meta_fuser(
  df_tr,
  df_all,
  basin_id,
  target = "Q",
  date_col = "YYYY",
  final_fuser = "rf",
  grid_levels = 5,
  quiet = TRUE,
  verbose_tune = TRUE,
  allow_par = TRUE,
  target_positive = FALSE
)
```
