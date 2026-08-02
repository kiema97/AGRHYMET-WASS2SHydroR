# WASS2SHydroR

WASS2SHydroR is an R package for seasonal and sub-seasonal hydrological
forecasting in West Africa and the Sahel. It provides tools to download climate
data, prepare gridded NetCDF inputs, train statistical and machine learning
models, combine multi-product predictions, evaluate hydrological skill, and run
HYPE scenarios.

The package documentation website can be built with:

```r
pkgdown::build_site()
```

The generated site is written to `docs/`.

## Quick Start

```r
library(WASS2SHydroR)

data_by_product <- make_toy_data_by_product()
basin_id <- unique(data_by_product[[1]]$HYBAS_ID)[1]

res <- wass2s_run_basin_mods_stat(
  basin_id = basin_id,
  data_by_product = data_by_product,
  fusion_method = "auto",
  best_model_guard = TRUE,
  quiet = FALSE
)

res$scores
res$probabilities

report <- wass2s_run_report(res, approach = "STAT")
report$summary
```

See the website articles for the complete CDS, data preparation, statistical,
machine learning, and HYPE workflows.

## Recommended Operational Outputs

For operational runs, use `fusion_method = "auto"` with
`best_model_guard = TRUE`. The package compares candidate fusion methods and
individual models using validation information from the training period, keeps
all candidate outputs for audit, and leaves the test period for evaluation.

STAT and ML basin-level workflows return a common set of outputs:

- `scores`, `scores_train`, `scores_test`
- `diagnostics`
- `fusion_report`
- `probabilities`
- `probabilistic_skill`

Use `wass2s_run_report()` to create a compact summary for user-facing scripts
and reports.

## Implemented ML Models

The ML model registry currently supports:

```r
c("rf", "xgb", "mlp", "kknn", "svmlinear", "mars", "cubist", "glmnet")
```

The final/product meta-fuser registry currently supports:

```r
c("rf", "xgb", "glmnet", "kknn", "svmlinear", "mars", "cubist", "mlp")
```
