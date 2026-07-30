# Machine Learning Forecasting Workflow

## Overview

The machine learning workflow follows the same two-level consolidation
logic as the statistical workflow, but uses machine learning models
instead of PCR, ridge, and lasso. It is more flexible because the user
can choose the base models used to generate intermediate forecasts. The
selected intermediate forecasts are then consolidated into one final
ML-approach seasonal forecast.

The default ML selection metric is RMSE because it is often numerically
more stable during hyperparameter search. KGE is still computed and
reported for the selected configuration.

## Implemented Models and Fusers

The ML model registry is defined by `SUPPORTED_MODELS`:

``` r
c("rf", "xgb", "mlp", "kknn", "svmlinear", "mars", "cubist", "glmnet")
```

The meta-fuser registry is defined by `SUPPORTED_FUSERS`:

``` r
c("rf", "xgb", "glmnet", "kknn", "svmlinear", "mars", "cubist", "mlp")
```

| Key         | Model family                             | Engine    | Package   |
|-------------|------------------------------------------|-----------|-----------|
| `rf`        | Random forest                            | `ranger`  | `ranger`  |
| `xgb`       | Gradient boosted trees                   | `xgboost` | `xgboost` |
| `mlp`       | Multilayer perceptron                    | `nnet`    | `nnet`    |
| `kknn`      | k-nearest neighbors                      | `kknn`    | `kknn`    |
| `svmlinear` | Linear support vector machine            | `kernlab` | `kernlab` |
| `mars`      | Multivariate adaptive regression splines | `earth`   | `earth`   |
| `cubist`    | Cubist regression rules                  | `Cubist`  | `Cubist`  |
| `glmnet`    | Elastic-net linear regression            | `glmnet`  | `glmnet`  |

## ML Process Schema

![](data:image/svg+xml;base64,PHN2ZyB2aWV3Ym94PSIwIDAgMTA4MCAzMzAiIHJvbGU9ImltZyIgYXJpYS1sYWJlbD0iTWFjaGluZSBsZWFybmluZyB3b3JrZmxvdyB3aXRoIHNlbGVjdGVkIGJhc2UgbW9kZWxzIGFuZCB0d28tbGV2ZWwgY29uc29saWRhdGlvbiIgc3R5bGU9IndpZHRoOjEwMCU7IG1heC13aWR0aDoxMDgwcHg7IGhlaWdodDphdXRvOyI+PGRlZnM+PG1hcmtlciBpZD0iYXJyb3ctbWwiIG1hcmtlcndpZHRoPSIxMCIgbWFya2VyaGVpZ2h0PSIxMCIgcmVmeD0iOCIgcmVmeT0iMyIgb3JpZW50PSJhdXRvIiBtYXJrZXJ1bml0cz0ic3Ryb2tlV2lkdGgiPjxwYXRoIGQ9Ik0wLDAgTDAsNiBMOSwzIHoiIGZpbGw9IiMxYjZmOGYiIC8+PC9tYXJrZXI+PC9kZWZzPjxyZWN0IHg9IjIwIiB5PSIxMjUiIHdpZHRoPSIxMzAiIGhlaWdodD0iODAiIHJ4PSI4IiBmaWxsPSIjZThmM2Y3IiBzdHJva2U9IiMxYjZmOGYiIHN0cm9rZS13aWR0aD0iMiIgLz48dGV4dCB4PSI4NSIgeT0iMTU0IiB0ZXh0LWFuY2hvcj0ibWlkZGxlIiBmb250LXNpemU9IjE0IiBmb250LXdlaWdodD0iNzAwIj5QcmVwYXJlZCBkYXRhPC90ZXh0Pjx0ZXh0IHg9Ijg1IiB5PSIxNzYiIHRleHQtYW5jaG9yPSJtaWRkbGUiIGZvbnQtc2l6ZT0iMTIiPm11bHRpcGxlIGNsaW1hdGU8L3RleHQ+PHRleHQgeD0iODUiIHk9IjE5MyIgdGV4dC1hbmNob3I9Im1pZGRsZSIgZm9udC1zaXplPSIxMiI+cHJvZHVjdHM8L3RleHQ+PHJlY3QgeD0iMjEwIiB5PSIyNSIgd2lkdGg9IjE4MCIgaGVpZ2h0PSI3OCIgcng9IjgiIGZpbGw9IiNmMGVjZmIiIHN0cm9rZT0iIzY2NTBhNCIgc3Ryb2tlLXdpZHRoPSIyIiAvPjx0ZXh0IHg9IjMwMCIgeT0iNTMiIHRleHQtYW5jaG9yPSJtaWRkbGUiIGZvbnQtc2l6ZT0iMTQiIGZvbnQtd2VpZ2h0PSI3MDAiPlNlbGVjdGVkIE1MIG1vZGVsIDE8L3RleHQ+PHRleHQgeD0iMzAwIiB5PSI3NCIgdGV4dC1hbmNob3I9Im1pZGRsZSIgZm9udC1zaXplPSIxMiI+cGVyIHByb2R1Y3QgZm9yZWNhc3RzPC90ZXh0Pjx0ZXh0IHg9IjMwMCIgeT0iOTEiIHRleHQtYW5jaG9yPSJtaWRkbGUiIGZvbnQtc2l6ZT0iMTIiPmUuZy4gcmY8L3RleHQ+PHJlY3QgeD0iMjEwIiB5PSIxMjYiIHdpZHRoPSIxODAiIGhlaWdodD0iNzgiIHJ4PSI4IiBmaWxsPSIjZjBlY2ZiIiBzdHJva2U9IiM2NjUwYTQiIHN0cm9rZS13aWR0aD0iMiIgLz48dGV4dCB4PSIzMDAiIHk9IjE1NCIgdGV4dC1hbmNob3I9Im1pZGRsZSIgZm9udC1zaXplPSIxNCIgZm9udC13ZWlnaHQ9IjcwMCI+U2VsZWN0ZWQgTUwgbW9kZWwgMjwvdGV4dD48dGV4dCB4PSIzMDAiIHk9IjE3NSIgdGV4dC1hbmNob3I9Im1pZGRsZSIgZm9udC1zaXplPSIxMiI+cGVyIHByb2R1Y3QgZm9yZWNhc3RzPC90ZXh0Pjx0ZXh0IHg9IjMwMCIgeT0iMTkyIiB0ZXh0LWFuY2hvcj0ibWlkZGxlIiBmb250LXNpemU9IjEyIj5lLmcuIHhnYjwvdGV4dD48cmVjdCB4PSIyMTAiIHk9IjIyNyIgd2lkdGg9IjE4MCIgaGVpZ2h0PSI3OCIgcng9IjgiIGZpbGw9IiNmMGVjZmIiIHN0cm9rZT0iIzY2NTBhNCIgc3Ryb2tlLXdpZHRoPSIyIiAvPjx0ZXh0IHg9IjMwMCIgeT0iMjU1IiB0ZXh0LWFuY2hvcj0ibWlkZGxlIiBmb250LXNpemU9IjE0IiBmb250LXdlaWdodD0iNzAwIj5TZWxlY3RlZCBNTCBtb2RlbCBuPC90ZXh0Pjx0ZXh0IHg9IjMwMCIgeT0iMjc2IiB0ZXh0LWFuY2hvcj0ibWlkZGxlIiBmb250LXNpemU9IjEyIj5wZXIgcHJvZHVjdCBmb3JlY2FzdHM8L3RleHQ+PHRleHQgeD0iMzAwIiB5PSIyOTMiIHRleHQtYW5jaG9yPSJtaWRkbGUiIGZvbnQtc2l6ZT0iMTIiPnVzZXItZGVmaW5lZCBzZXQ8L3RleHQ+PHJlY3QgeD0iNDYwIiB5PSIyNSIgd2lkdGg9IjE2MCIgaGVpZ2h0PSI3OCIgcng9IjgiIGZpbGw9IiNlZWY3ZWQiIHN0cm9rZT0iIzRiOGIzYiIgc3Ryb2tlLXdpZHRoPSIyIiAvPjx0ZXh0IHg9IjU0MCIgeT0iNTMiIHRleHQtYW5jaG9yPSJtaWRkbGUiIGZvbnQtc2l6ZT0iMTQiIGZvbnQtd2VpZ2h0PSI3MDAiPlRvcEsgKyBmdXNpb248L3RleHQ+PHRleHQgeD0iNTQwIiB5PSI3NCIgdGV4dC1hbmNob3I9Im1pZGRsZSIgZm9udC1zaXplPSIxMiI+Zm9yZWNhc3QgZm9yIE1MPC90ZXh0Pjx0ZXh0IHg9IjU0MCIgeT0iOTEiIHRleHQtYW5jaG9yPSJtaWRkbGUiIGZvbnQtc2l6ZT0iMTIiPm1vZGVsIDE8L3RleHQ+PHJlY3QgeD0iNDYwIiB5PSIxMjYiIHdpZHRoPSIxNjAiIGhlaWdodD0iNzgiIHJ4PSI4IiBmaWxsPSIjZWVmN2VkIiBzdHJva2U9IiM0YjhiM2IiIHN0cm9rZS13aWR0aD0iMiIgLz48dGV4dCB4PSI1NDAiIHk9IjE1NCIgdGV4dC1hbmNob3I9Im1pZGRsZSIgZm9udC1zaXplPSIxNCIgZm9udC13ZWlnaHQ9IjcwMCI+VG9wSyArIGZ1c2lvbjwvdGV4dD48dGV4dCB4PSI1NDAiIHk9IjE3NSIgdGV4dC1hbmNob3I9Im1pZGRsZSIgZm9udC1zaXplPSIxMiI+Zm9yZWNhc3QgZm9yIE1MPC90ZXh0Pjx0ZXh0IHg9IjU0MCIgeT0iMTkyIiB0ZXh0LWFuY2hvcj0ibWlkZGxlIiBmb250LXNpemU9IjEyIj5tb2RlbCAyPC90ZXh0PjxyZWN0IHg9IjQ2MCIgeT0iMjI3IiB3aWR0aD0iMTYwIiBoZWlnaHQ9Ijc4IiByeD0iOCIgZmlsbD0iI2VlZjdlZCIgc3Ryb2tlPSIjNGI4YjNiIiBzdHJva2Utd2lkdGg9IjIiIC8+PHRleHQgeD0iNTQwIiB5PSIyNTUiIHRleHQtYW5jaG9yPSJtaWRkbGUiIGZvbnQtc2l6ZT0iMTQiIGZvbnQtd2VpZ2h0PSI3MDAiPlRvcEsgKyBmdXNpb248L3RleHQ+PHRleHQgeD0iNTQwIiB5PSIyNzYiIHRleHQtYW5jaG9yPSJtaWRkbGUiIGZvbnQtc2l6ZT0iMTIiPmZvcmVjYXN0IGZvciBNTDwvdGV4dD48dGV4dCB4PSI1NDAiIHk9IjI5MyIgdGV4dC1hbmNob3I9Im1pZGRsZSIgZm9udC1zaXplPSIxMiI+bW9kZWwgbjwvdGV4dD48cmVjdCB4PSI3MDAiIHk9IjEyNSIgd2lkdGg9IjE2MCIgaGVpZ2h0PSI4MCIgcng9IjgiIGZpbGw9IiNmOGVlZWUiIHN0cm9rZT0iI2EzM2QzZCIgc3Ryb2tlLXdpZHRoPSIyIiAvPjx0ZXh0IHg9Ijc4MCIgeT0iMTU0IiB0ZXh0LWFuY2hvcj0ibWlkZGxlIiBmb250LXNpemU9IjE0IiBmb250LXdlaWdodD0iNzAwIj5Nb2RlbC1sZXZlbDwvdGV4dD48dGV4dCB4PSI3ODAiIHk9IjE3NiIgdGV4dC1hbmNob3I9Im1pZGRsZSIgZm9udC1zaXplPSIxMiI+Y29uc29saWRhdGlvbiBvZjwvdGV4dD48dGV4dCB4PSI3ODAiIHk9IjE5MyIgdGV4dC1hbmNob3I9Im1pZGRsZSIgZm9udC1zaXplPSIxMiI+TUwgZm9yZWNhc3RzPC90ZXh0PjxyZWN0IHg9IjkyNSIgeT0iMTI1IiB3aWR0aD0iMTM1IiBoZWlnaHQ9IjgwIiByeD0iOCIgZmlsbD0iI2VkZjFmOCIgc3Ryb2tlPSIjMzg1ZjlmIiBzdHJva2Utd2lkdGg9IjIiIC8+PHRleHQgeD0iOTkyIiB5PSIxNTQiIHRleHQtYW5jaG9yPSJtaWRkbGUiIGZvbnQtc2l6ZT0iMTQiIGZvbnQtd2VpZ2h0PSI3MDAiPk1MIGFwcHJvYWNoPC90ZXh0Pjx0ZXh0IHg9Ijk5MiIgeT0iMTc2IiB0ZXh0LWFuY2hvcj0ibWlkZGxlIiBmb250LXNpemU9IjEyIj5zZWFzb25hbDwvdGV4dD48dGV4dCB4PSI5OTIiIHk9IjE5MyIgdGV4dC1hbmNob3I9Im1pZGRsZSIgZm9udC1zaXplPSIxMiI+Zm9yZWNhc3Q8L3RleHQ+PGxpbmUgeDE9IjE1MCIgeTE9IjE2NSIgeDI9IjIwMiIgeTI9IjY0IiBzdHJva2U9IiMxYjZmOGYiIHN0cm9rZS13aWR0aD0iMyIgbWFya2VyLWVuZD0idXJsKCNhcnJvdy1tbCkiPjwvbGluZT48bGluZSB4MT0iMTUwIiB5MT0iMTY1IiB4Mj0iMjAyIiB5Mj0iMTY1IiBzdHJva2U9IiMxYjZmOGYiIHN0cm9rZS13aWR0aD0iMyIgbWFya2VyLWVuZD0idXJsKCNhcnJvdy1tbCkiPjwvbGluZT48bGluZSB4MT0iMTUwIiB5MT0iMTY1IiB4Mj0iMjAyIiB5Mj0iMjY2IiBzdHJva2U9IiMxYjZmOGYiIHN0cm9rZS13aWR0aD0iMyIgbWFya2VyLWVuZD0idXJsKCNhcnJvdy1tbCkiPjwvbGluZT48bGluZSB4MT0iMzkwIiB5MT0iNjQiIHgyPSI0NTIiIHkyPSI2NCIgc3Ryb2tlPSIjMWI2ZjhmIiBzdHJva2Utd2lkdGg9IjMiIG1hcmtlci1lbmQ9InVybCgjYXJyb3ctbWwpIj48L2xpbmU+PGxpbmUgeDE9IjM5MCIgeTE9IjE2NSIgeDI9IjQ1MiIgeTI9IjE2NSIgc3Ryb2tlPSIjMWI2ZjhmIiBzdHJva2Utd2lkdGg9IjMiIG1hcmtlci1lbmQ9InVybCgjYXJyb3ctbWwpIj48L2xpbmU+PGxpbmUgeDE9IjM5MCIgeTE9IjI2NiIgeDI9IjQ1MiIgeTI9IjI2NiIgc3Ryb2tlPSIjMWI2ZjhmIiBzdHJva2Utd2lkdGg9IjMiIG1hcmtlci1lbmQ9InVybCgjYXJyb3ctbWwpIj48L2xpbmU+PGxpbmUgeDE9IjYyMCIgeTE9IjY0IiB4Mj0iNjkyIiB5Mj0iMTY1IiBzdHJva2U9IiMxYjZmOGYiIHN0cm9rZS13aWR0aD0iMyIgbWFya2VyLWVuZD0idXJsKCNhcnJvdy1tbCkiPjwvbGluZT48bGluZSB4MT0iNjIwIiB5MT0iMTY1IiB4Mj0iNjkyIiB5Mj0iMTY1IiBzdHJva2U9IiMxYjZmOGYiIHN0cm9rZS13aWR0aD0iMyIgbWFya2VyLWVuZD0idXJsKCNhcnJvdy1tbCkiPjwvbGluZT48bGluZSB4MT0iNjIwIiB5MT0iMjY2IiB4Mj0iNjkyIiB5Mj0iMTY1IiBzdHJva2U9IiMxYjZmOGYiIHN0cm9rZS13aWR0aD0iMyIgbWFya2VyLWVuZD0idXJsKCNhcnJvdy1tbCkiPjwvbGluZT48bGluZSB4MT0iODYwIiB5MT0iMTY1IiB4Mj0iOTE3IiB5Mj0iMTY1IiBzdHJva2U9IiMxYjZmOGYiIHN0cm9rZS13aWR0aD0iMyIgbWFya2VyLWVuZD0idXJsKCNhcnJvdy1tbCkiPjwvbGluZT48L3N2Zz4=)

## Tune One Product

``` r
data_by_product <- make_toy_data_by_product()
df <- data_by_product[[1]]
basin_id <- unique(df$HYBAS_ID)[1]

df_basin <- subset(df, HYBAS_ID == basin_id)
predictors <- grep("^pt_", names(df_basin), value = TRUE)

res_ml <- wass2s_tune_pred_ml(
  df_basin_product = df_basin,
  predictors = predictors,
  model = "rf",
  target = "Q",
  date_col = "YYYY",
  selection_metric = "rmse",
  target_positive = TRUE,
  quiet = FALSE
)
```

If direct KGE optimization is stable for the dataset:

``` r
selection_metric = "kge"
```

## Consolidate Products for One ML Model

``` r
res_cons_ml <- wass2s_cons_mods_ml(
  basin_id = basin_id,
  data_by_product = data_by_product,
  model = "rf",
  topK = 3,
  product_fusion_method = "median",
  selection_metric = "rmse",
  quiet = FALSE
)

res_cons_ml$leaderboard_products
res_cons_ml$fused
```

## Full ML Workflow for One Basin

``` r
res_basin_ml <- wass2s_run_bas_mod_ml(
  basin_id = basin_id,
  data_by_product = data_by_product,
  models = c("rf", "xgb", "mlp"),
  topK = 3,
  product_fusion_method = "median",
  fusion_method = "weighted_mean",
  final_fuser = "rf",
  selection_metric = "rmse",
  target_positive = TRUE,
  quiet = FALSE
)

res_basin_ml$scores
res_basin_ml$fused_by_model
```

## Several Basins

``` r
res_all_ml <- wass2s_run_basins_ml(
  data_by_product = data_by_product,
  models = c("rf", "xgb"),
  topK = 3,
  fusion_method = "median",
  selection_metric = "rmse",
  parallel = FALSE,
  quiet = FALSE
)
```

## Preprocessing Controls

[`wass2s_tune_pred_ml()`](https://kiema97.github.io/WASS2SHydroR/reference/wass2s_tune_pred_ml.md)
provides several recipe controls:

- `apply_impute`: impute missing predictors;
- `apply_corr`: remove highly correlated predictors;
- `corr_threshold`: correlation threshold;
- `apply_normalize`: center and scale predictors;
- `auto_pca`: apply PCA when there are many predictors;
- `auto_pca_when_gt`: predictor count threshold for PCA;
- `pca_num_comp`: fixed number of PCA components;
- `pca_var_threshold`: cumulative variance threshold.

Disable preprocessing steps when predictors have already been
preprocessed upstream, for example by EOF or PCA.

## Recommended Starting Point

For operational testing:

``` r
res <- wass2s_run_bas_mod_ml(
  basin_id = basin_id,
  data_by_product = data_by_product,
  models = c("rf"),
  topK = 2,
  product_fusion_method = "median",
  fusion_method = "median",
  selection_metric = "rmse",
  quiet = FALSE
)
```

Then increase the number of models, products, and basins gradually.
