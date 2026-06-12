# Get Started with WASS2SHydroR

## Purpose

WASS2SHydroR is an R toolkit for seasonal and sub-seasonal hydrological
forecasting in West Africa and the Sahel. It supports the main steps of
an operational forecasting workflow:

- download climate forecasts from CDS/ECMWF;
- prepare gridded NetCDF or `stars` objects;
- assemble product-by-product basin data;
- fit statistical models such as PCR, ridge, and lasso;
- fit machine learning models such as random forest, XGBoost, and MLP;
- rank climate products using cross-validation skill;
- combine forecasts with median, mean, KGE-weighted mean, or
  meta-learning;
- evaluate forecasts with KGE, RMSE, MAE, NSE, and correlation;
- run and monitor HYPE scenarios.

## Overall Process

![](data:image/svg+xml;base64,PHN2ZyB2aWV3Ym94PSIwIDAgMTA0MCAzNjAiIHJvbGU9ImltZyIgYXJpYS1sYWJlbD0iV0FTUzJTSHlkcm9SIGVuZC10by1lbmQgZm9yZWNhc3RpbmcgcHJvY2VzcyB3aXRoIHN0YXRpc3RpY2FsLCBtYWNoaW5lIGxlYXJuaW5nLCBoeWRyb2xvZ2ljYWwgYW5kIG11bHRpLWFwcHJvYWNoIGNvbnNvbGlkYXRpb24iIHN0eWxlPSJ3aWR0aDoxMDAlOyBtYXgtd2lkdGg6MTA0MHB4OyBoZWlnaHQ6YXV0bzsiPjxkZWZzPjxtYXJrZXIgaWQ9ImFycm93LW1haW4iIG1hcmtlcndpZHRoPSIxMCIgbWFya2VyaGVpZ2h0PSIxMCIgcmVmeD0iOCIgcmVmeT0iMyIgb3JpZW50PSJhdXRvIiBtYXJrZXJ1bml0cz0ic3Ryb2tlV2lkdGgiPjxwYXRoIGQ9Ik0wLDAgTDAsNiBMOSwzIHoiIGZpbGw9IiMxYjZmOGYiIC8+PC9tYXJrZXI+PC9kZWZzPjxyZWN0IHg9IjIwIiB5PSIxMzUiIHdpZHRoPSIxMzUiIGhlaWdodD0iNzUiIHJ4PSI4IiBmaWxsPSIjZThmM2Y3IiBzdHJva2U9IiMxYjZmOGYiIHN0cm9rZS13aWR0aD0iMiIgLz48dGV4dCB4PSI4NyIgeT0iMTY0IiB0ZXh0LWFuY2hvcj0ibWlkZGxlIiBmb250LXNpemU9IjE0IiBmb250LXdlaWdodD0iNzAwIj5Eb3dubG9hZDwvdGV4dD48dGV4dCB4PSI4NyIgeT0iMTg1IiB0ZXh0LWFuY2hvcj0ibWlkZGxlIiBmb250LXNpemU9IjEyIj5DRFMgLyBjbGltYXRlPC90ZXh0Pjx0ZXh0IHg9Ijg3IiB5PSIyMDIiIHRleHQtYW5jaG9yPSJtaWRkbGUiIGZvbnQtc2l6ZT0iMTIiPnByb2R1Y3RzPC90ZXh0PjxyZWN0IHg9IjE4NSIgeT0iMTM1IiB3aWR0aD0iMTM1IiBoZWlnaHQ9Ijc1IiByeD0iOCIgZmlsbD0iI2VlZjdlZCIgc3Ryb2tlPSIjNGI4YjNiIiBzdHJva2Utd2lkdGg9IjIiIC8+PHRleHQgeD0iMjUyIiB5PSIxNjQiIHRleHQtYW5jaG9yPSJtaWRkbGUiIGZvbnQtc2l6ZT0iMTQiIGZvbnQtd2VpZ2h0PSI3MDAiPlByb2Nlc3Npbmc8L3RleHQ+PHRleHQgeD0iMjUyIiB5PSIxODUiIHRleHQtYW5jaG9yPSJtaWRkbGUiIGZvbnQtc2l6ZT0iMTIiPmZvcm1hdHRpbmcgYW5kPC90ZXh0Pjx0ZXh0IHg9IjI1MiIgeT0iMjAyIiB0ZXh0LWFuY2hvcj0ibWlkZGxlIiBmb250LXNpemU9IjEyIj5iYXNpbiB0YWJsZXM8L3RleHQ+PHJlY3QgeD0iMzgwIiB5PSIyNSIgd2lkdGg9IjE4MCIgaGVpZ2h0PSI3OCIgcng9IjgiIGZpbGw9IiNmZmY0ZGYiIHN0cm9rZT0iI2I2NmQwMCIgc3Ryb2tlLXdpZHRoPSIyIiAvPjx0ZXh0IHg9IjQ3MCIgeT0iNTMiIHRleHQtYW5jaG9yPSJtaWRkbGUiIGZvbnQtc2l6ZT0iMTQiIGZvbnQtd2VpZ2h0PSI3MDAiPlN0YXRpc3RpY2FsIGFwcHJvYWNoPC90ZXh0Pjx0ZXh0IHg9IjQ3MCIgeT0iNzQiIHRleHQtYW5jaG9yPSJtaWRkbGUiIGZvbnQtc2l6ZT0iMTIiPnBlciBtb2RlbDogcHJvZHVjdHMgLSZndDs8L3RleHQ+PHRleHQgeD0iNDcwIiB5PSI5MSIgdGV4dC1hbmNob3I9Im1pZGRsZSIgZm9udC1zaXplPSIxMiI+dG9wSyBjb25zb2xpZGF0aW9uPC90ZXh0PjxyZWN0IHg9IjM4MCIgeT0iMTM2IiB3aWR0aD0iMTgwIiBoZWlnaHQ9Ijc4IiByeD0iOCIgZmlsbD0iI2YwZWNmYiIgc3Ryb2tlPSIjNjY1MGE0IiBzdHJva2Utd2lkdGg9IjIiIC8+PHRleHQgeD0iNDcwIiB5PSIxNjQiIHRleHQtYW5jaG9yPSJtaWRkbGUiIGZvbnQtc2l6ZT0iMTQiIGZvbnQtd2VpZ2h0PSI3MDAiPk1MIGFwcHJvYWNoPC90ZXh0Pjx0ZXh0IHg9IjQ3MCIgeT0iMTg1IiB0ZXh0LWFuY2hvcj0ibWlkZGxlIiBmb250LXNpemU9IjEyIj5jaG9zZW4gTUwgbW9kZWxzIC0mZ3Q7PC90ZXh0Pjx0ZXh0IHg9IjQ3MCIgeT0iMjAyIiB0ZXh0LWFuY2hvcj0ibWlkZGxlIiBmb250LXNpemU9IjEyIj50b3BLIGNvbnNvbGlkYXRpb248L3RleHQ+PHJlY3QgeD0iMzgwIiB5PSIyNDciIHdpZHRoPSIxODAiIGhlaWdodD0iNzgiIHJ4PSI4IiBmaWxsPSIjZWRmMWY4IiBzdHJva2U9IiMzODVmOWYiIHN0cm9rZS13aWR0aD0iMiIgLz48dGV4dCB4PSI0NzAiIHk9IjI3NSIgdGV4dC1hbmNob3I9Im1pZGRsZSIgZm9udC1zaXplPSIxNCIgZm9udC13ZWlnaHQ9IjcwMCI+SHlkcm9sb2dpY2FsIGFwcHJvYWNoPC90ZXh0Pjx0ZXh0IHg9IjQ3MCIgeT0iMjk2IiB0ZXh0LWFuY2hvcj0ibWlkZGxlIiBmb250LXNpemU9IjEyIj5IWVBFIGZvcmNlZCBieTwvdGV4dD48dGV4dCB4PSI0NzAiIHk9IjMxMyIgdGV4dC1hbmNob3I9Im1pZGRsZSIgZm9udC1zaXplPSIxMiI+Y2xpbWF0ZSBmb3JjaW5nczwvdGV4dD48cmVjdCB4PSI2MzUiIHk9IjI1IiB3aWR0aD0iMTYwIiBoZWlnaHQ9Ijc4IiByeD0iOCIgZmlsbD0iI2ZmZjhlYSIgc3Ryb2tlPSIjYjY2ZDAwIiBzdHJva2Utd2lkdGg9IjIiIC8+PHRleHQgeD0iNzE1IiB5PSI1MyIgdGV4dC1hbmNob3I9Im1pZGRsZSIgZm9udC1zaXplPSIxNCIgZm9udC13ZWlnaHQ9IjcwMCI+U3RhdCBzZWFzb25hbDwvdGV4dD48dGV4dCB4PSI3MTUiIHk9Ijc0IiB0ZXh0LWFuY2hvcj0ibWlkZGxlIiBmb250LXNpemU9IjEyIj5mb3JlY2FzdDwvdGV4dD48cmVjdCB4PSI2MzUiIHk9IjEzNiIgd2lkdGg9IjE2MCIgaGVpZ2h0PSI3OCIgcng9IjgiIGZpbGw9IiNmNWYxZmYiIHN0cm9rZT0iIzY2NTBhNCIgc3Ryb2tlLXdpZHRoPSIyIiAvPjx0ZXh0IHg9IjcxNSIgeT0iMTY0IiB0ZXh0LWFuY2hvcj0ibWlkZGxlIiBmb250LXNpemU9IjE0IiBmb250LXdlaWdodD0iNzAwIj5NTCBzZWFzb25hbDwvdGV4dD48dGV4dCB4PSI3MTUiIHk9IjE4NSIgdGV4dC1hbmNob3I9Im1pZGRsZSIgZm9udC1zaXplPSIxMiI+Zm9yZWNhc3Q8L3RleHQ+PHJlY3QgeD0iNjM1IiB5PSIyNDciIHdpZHRoPSIxNjAiIGhlaWdodD0iNzgiIHJ4PSI4IiBmaWxsPSIjZjBmNGZiIiBzdHJva2U9IiMzODVmOWYiIHN0cm9rZS13aWR0aD0iMiIgLz48dGV4dCB4PSI3MTUiIHk9IjI3NSIgdGV4dC1hbmNob3I9Im1pZGRsZSIgZm9udC1zaXplPSIxNCIgZm9udC13ZWlnaHQ9IjcwMCI+SHlkcm8gc2Vhc29uYWw8L3RleHQ+PHRleHQgeD0iNzE1IiB5PSIyOTYiIHRleHQtYW5jaG9yPSJtaWRkbGUiIGZvbnQtc2l6ZT0iMTIiPmZvcmVjYXN0PC90ZXh0PjxyZWN0IHg9Ijg2MCIgeT0iMTM1IiB3aWR0aD0iMTU1IiBoZWlnaHQ9Ijg1IiByeD0iOCIgZmlsbD0iI2Y4ZWVlZSIgc3Ryb2tlPSIjYTMzZDNkIiBzdHJva2Utd2lkdGg9IjIiIC8+PHRleHQgeD0iOTM3IiB5PSIxNjQiIHRleHQtYW5jaG9yPSJtaWRkbGUiIGZvbnQtc2l6ZT0iMTQiIGZvbnQtd2VpZ2h0PSI3MDAiPk9wdGlvbmFsPC90ZXh0Pjx0ZXh0IHg9IjkzNyIgeT0iMTg1IiB0ZXh0LWFuY2hvcj0ibWlkZGxlIiBmb250LXNpemU9IjEyIj5tdWx0aS1hcHByb2FjaDwvdGV4dD48dGV4dCB4PSI5MzciIHk9IjIwMiIgdGV4dC1hbmNob3I9Im1pZGRsZSIgZm9udC1zaXplPSIxMiI+Y29uc29saWRhdGlvbjwvdGV4dD48bGluZSB4MT0iMTU1IiB5MT0iMTczIiB4Mj0iMTc3IiB5Mj0iMTczIiBzdHJva2U9IiMxYjZmOGYiIHN0cm9rZS13aWR0aD0iMyIgbWFya2VyLWVuZD0idXJsKCNhcnJvdy1tYWluKSI+PC9saW5lPjxsaW5lIHgxPSIzMjAiIHkxPSIxNzMiIHgyPSIzNzIiIHkyPSI2NCIgc3Ryb2tlPSIjMWI2ZjhmIiBzdHJva2Utd2lkdGg9IjMiIG1hcmtlci1lbmQ9InVybCgjYXJyb3ctbWFpbikiPjwvbGluZT48bGluZSB4MT0iMzIwIiB5MT0iMTczIiB4Mj0iMzcyIiB5Mj0iMTc1IiBzdHJva2U9IiMxYjZmOGYiIHN0cm9rZS13aWR0aD0iMyIgbWFya2VyLWVuZD0idXJsKCNhcnJvdy1tYWluKSI+PC9saW5lPjxsaW5lIHgxPSIzMjAiIHkxPSIxNzMiIHgyPSIzNzIiIHkyPSIyODYiIHN0cm9rZT0iIzFiNmY4ZiIgc3Ryb2tlLXdpZHRoPSIzIiBtYXJrZXItZW5kPSJ1cmwoI2Fycm93LW1haW4pIj48L2xpbmU+PGxpbmUgeDE9IjU2MCIgeTE9IjY0IiB4Mj0iNjI3IiB5Mj0iNjQiIHN0cm9rZT0iIzFiNmY4ZiIgc3Ryb2tlLXdpZHRoPSIzIiBtYXJrZXItZW5kPSJ1cmwoI2Fycm93LW1haW4pIj48L2xpbmU+PGxpbmUgeDE9IjU2MCIgeTE9IjE3NSIgeDI9IjYyNyIgeTI9IjE3NSIgc3Ryb2tlPSIjMWI2ZjhmIiBzdHJva2Utd2lkdGg9IjMiIG1hcmtlci1lbmQ9InVybCgjYXJyb3ctbWFpbikiPjwvbGluZT48bGluZSB4MT0iNTYwIiB5MT0iMjg2IiB4Mj0iNjI3IiB5Mj0iMjg2IiBzdHJva2U9IiMxYjZmOGYiIHN0cm9rZS13aWR0aD0iMyIgbWFya2VyLWVuZD0idXJsKCNhcnJvdy1tYWluKSI+PC9saW5lPjxsaW5lIHgxPSI3OTUiIHkxPSI2NCIgeDI9Ijg1MiIgeTI9IjE3NiIgc3Ryb2tlPSIjMWI2ZjhmIiBzdHJva2Utd2lkdGg9IjMiIG1hcmtlci1lbmQ9InVybCgjYXJyb3ctbWFpbikiPjwvbGluZT48bGluZSB4MT0iNzk1IiB5MT0iMTc1IiB4Mj0iODUyIiB5Mj0iMTc2IiBzdHJva2U9IiMxYjZmOGYiIHN0cm9rZS13aWR0aD0iMyIgbWFya2VyLWVuZD0idXJsKCNhcnJvdy1tYWluKSI+PC9saW5lPjxsaW5lIHgxPSI3OTUiIHkxPSIyODYiIHgyPSI4NTIiIHkyPSIxNzYiIHN0cm9rZT0iIzFiNmY4ZiIgc3Ryb2tlLXdpZHRoPSIzIiBtYXJrZXItZW5kPSJ1cmwoI2Fycm93LW1haW4pIj48L2xpbmU+PC9zdmc+)

In WASS2SHydroR, each approach first produces its own seasonal forecast.
A final multi-approach forecast can then be obtained by consolidating
the forecasts from the statistical, machine learning, and hydrological
modelling approaches.

## Installation

For regular use, install WASS2SHydroR directly from the `staging` branch
on GitHub:

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

This recommended installation keeps the setup lightweight and
reproducible: `build_vignettes = FALSE` avoids rebuilding local
vignettes during installation, `upgrade = "never"` avoids changing
packages that are already installed, and
`dependencies = c("Depends", "Imports", "LinkingTo")` installs only the
core runtime dependencies.

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

Then load the package:

``` r
library(WASS2SHydroR)
```

For local development from a cloned repository, restore the project
environment from the package root:

``` r
renv::restore()
```

During development, load the package without reinstalling it:

``` r
pkgload::load_all(".")
```

Optional features require optional packages:

- `ecmwfr` for CDS/ECMWF data access;
- `stars`, `ncdf4`, and `units` for NetCDF preparation;
- `ranger`, `xgboost`, `nnet`, `glmnet`, `earth`, `Cubist`, `kernlab`,
  or `kknn` depending on the selected modelling engines;
- `future` and `furrr` for parallel execution.

## Expected Data Structure

Most modelling functions expect a named list of data frames, with one
data frame per climate product:

``` r
data_by_product <- list(
  ECMWF = ecmwf_df,
  MeteoFrance = mf_df,
  CMCC = cmcc_df
)
```

Each product table should contain at least:

- a basin identifier column, by default `HYBAS_ID`;
- a date or year column, by default `YYYY`;
- an observed hydrological target, by default `Q`;
- predictor columns, often selected with a regular expression such as
  `^pt_`.

Example:

``` r
head(data_by_product[[1]])
# HYBAS_ID   YYYY       Q     pt_1    pt_2    pt_3
# 1050915990 19930101   120.4 0.23    1.42    0.91
```

## Minimal Reproducible Example

The package includes a toy dataset generator:

``` r
data_by_product <- make_toy_data_by_product()
basin_id <- unique(data_by_product[[1]]$HYBAS_ID)[1]
```

Run a statistical workflow:

``` r
stat_res <- wass2s_run_basin_mods_stat(
  basin_id = basin_id,
  data_by_product = data_by_product,
  topK = 2,
  fusion_method = "median",
  quiet = FALSE
)

stat_res$scores
```

Run a machine learning workflow:

``` r
ml_res <- wass2s_run_bas_mod_ml(
  basin_id = basin_id,
  data_by_product = data_by_product,
  models = c("rf"),
  topK = 2,
  fusion_method = "median",
  selection_metric = "rmse",
  quiet = FALSE
)

ml_res$scores
```

## Main User-Facing Functions

| Area | Function | Purpose |
|----|----|----|
| Data | [`wass2s_download_cds()`](https://kiema97.github.io/AGRHYMET-WASS2SHydroR/reference/wass2s_download_cds.md) | Download climate forecasts from CDS/ECMWF |
| Data | [`wass2s_prepare_data()`](https://kiema97.github.io/AGRHYMET-WASS2SHydroR/reference/wass2s_prepare_data.md) | Convert NetCDF or `stars` data to tabular form |
| Statistics | [`wass2s_tune_pred_stat()`](https://kiema97.github.io/AGRHYMET-WASS2SHydroR/reference/wass2s_tune_pred_stat.md) | Tune and predict one statistical product model |
| Statistics | [`wass2s_cons_mods_stat()`](https://kiema97.github.io/AGRHYMET-WASS2SHydroR/reference/wass2s_cons_mods_stat.md) | Combine products for one statistical model |
| Statistics | [`wass2s_run_basin_mods_stat()`](https://kiema97.github.io/AGRHYMET-WASS2SHydroR/reference/wass2s_run_basin_mods_stat.md) | Full statistical workflow for one basin |
| Statistics | [`wass2s_run_basins_stat()`](https://kiema97.github.io/AGRHYMET-WASS2SHydroR/reference/wass2s_run_basins_stat.md) | Statistical workflow for several basins |
| ML | [`wass2s_tune_pred_ml()`](https://kiema97.github.io/AGRHYMET-WASS2SHydroR/reference/wass2s_tune_pred_ml.md) | Tune and predict one ML product model |
| ML | [`wass2s_cons_mods_ml()`](https://kiema97.github.io/AGRHYMET-WASS2SHydroR/reference/wass2s_cons_mods_ml.md) | Combine products for one ML model |
| ML | [`wass2s_run_bas_mod_ml()`](https://kiema97.github.io/AGRHYMET-WASS2SHydroR/reference/wass2s_run_bas_mod_ml.md) | Full ML workflow for one basin |
| ML | [`wass2s_run_basins_ml()`](https://kiema97.github.io/AGRHYMET-WASS2SHydroR/reference/wass2s_run_basins_ml.md) | ML workflow for several basins |
| HYPE | [`wass2s_hype_run()`](https://kiema97.github.io/AGRHYMET-WASS2SHydroR/reference/wass2s_hype_run.md) | Run HYPE in standard mode |
| HYPE | [`wass2s_hype_run_watchlog()`](https://kiema97.github.io/AGRHYMET-WASS2SHydroR/reference/wass2s_hype_run_watchlog.md) | Run HYPE and monitor logs |
| Metrics | [`wass2s_kge()`](https://kiema97.github.io/AGRHYMET-WASS2SHydroR/reference/wass2s_kge.md) | Kling-Gupta Efficiency |
| Interface | [`run_wass2s()`](https://kiema97.github.io/AGRHYMET-WASS2SHydroR/reference/run_wass2s.md) | Generic method dispatcher |

## Implemented Model Registry

The model registry is defined in `R/models.R`.

### Base ML models

`SUPPORTED_MODELS` currently contains:

| Key | Model family | parsnip engine | Required package |
|----|----|----|----|
| `rf` | Random forest | `ranger` | `ranger` |
| `xgb` | Gradient boosted trees | `xgboost` | `xgboost` |
| `mlp` | Multilayer perceptron | `nnet` | `nnet` |
| `kknn` | k-nearest neighbors | `kknn` | `kknn` |
| `svmlinear` | Linear support vector machine | `kernlab` | `kernlab` |
| `mars` | Multivariate adaptive regression splines | `earth` | `earth` |
| `cubist` | Cubist rule-based regression | `Cubist` | `Cubist` |
| `glmnet` | Elastic-net linear regression | `glmnet` | `glmnet` |

### Meta-fusers

`SUPPORTED_FUSERS` currently contains:

``` r
c("rf", "xgb", "glmnet", "kknn", "svmlinear", "mars", "cubist", "mlp")
```

The same modelling factory, `model_spec()`, is used for ML base learners
and meta-fusers.

## Generic Interface

Available methods can be listed with:

``` r
list_methods()
```

The generic interface is:

``` r
run_wass2s("stat", data_by_product, cfg)
run_wass2s("hydro", data_by_product, cfg)
run_wass2s("ml", data_by_product, cfg)
```

`"ml"` is an alias for the hydro/ML pipeline.
