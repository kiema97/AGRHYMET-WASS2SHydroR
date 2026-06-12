# WASS2SHydroR

WASS2SHydroR is an R package for seasonal and sub-seasonal hydrological
forecasting in West Africa and the Sahel. It provides tools to download
climate data, prepare gridded NetCDF inputs, train statistical and
machine learning models, combine multi-product predictions, evaluate
hydrological skill, and run HYPE scenarios.

The package documentation website can be built with:

``` r
pkgdown::build_site()
```

The generated site is written to `docs/`.

## Installation

Install the development version from the `staging` branch on GitHub:

``` r
install.packages("remotes")

remotes::install_github(
  "kiema97/AGRHYMET-WASS2SHydroR",
  ref = "staging",
  build_vignettes = FALSE,
  upgrade = "never",
  dependencies = c("Depends", "Imports", "LinkingTo")
)
```

This installs the package and its core dependencies without rebuilding
the vignettes or upgrading packages already installed in your R library.

For a fuller installation including optional packages used by CDS
access, NetCDF processing, mapping, parallel execution, and additional
ML engines, use:

``` r
remotes::install_github(
  "kiema97/AGRHYMET-WASS2SHydroR",
  ref = "staging",
  build_vignettes = FALSE,
  upgrade = "never",
  dependencies = TRUE
)
```

For local package development from a clone, run
[`renv::restore()`](https://rstudio.github.io/renv/reference/restore.html)
from the package root, then use `pkgload::load_all(".")` while editing
the source.

## Quick Start

``` r
library(WASS2SHydroR)

data_by_product <- make_toy_data_by_product()
basin_id <- unique(data_by_product[[1]]$HYBAS_ID)[1]

res <- wass2s_run_basin_mods_stat(
  basin_id = basin_id,
  data_by_product = data_by_product,
  fusion_method = "median",
  quiet = FALSE
)

res$scores
```

See the website articles for the complete CDS, data preparation,
statistical, machine learning, and HYPE workflows.

## Implemented ML Models

The ML model registry currently supports:

``` r
c("rf", "xgb", "mlp", "kknn", "svmlinear", "mars", "cubist", "glmnet")
```

The final/product meta-fuser registry currently supports:

``` r
c("rf", "xgb", "glmnet", "kknn", "svmlinear", "mars", "cubist", "mlp")
```
