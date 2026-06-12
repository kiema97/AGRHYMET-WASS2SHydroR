# Run the "stat" method (S3 dispatch)

This method is automatically dispatched by \`run_method()\` when
\`method\` has class \`method_stat\`. It delegates to the statistical+ML
pipeline that trains PCR/Ridge/Lasso per product and basin, fuses top-K
products by KGE, then fits a meta-learner.

## Usage

``` r
# S3 method for class 'method_stat'
run_method(method, data_by_product, cfg)
```

## Arguments

- method:

  An object created by \`method_id("stat")\`.

- data_by_product:

  Named list of data frames, one per climate product. Each data frame
  must include at least: \`HYBAS_ID\`, \`YYYY\`, \`Q\`, and predictors.

- cfg:

  Named list parsed from YAML configuration (see \`config/\`).

## Value

A named list keyed by basin id, each element being the result list
returned by \`wass2s_run_basin_mods_stat()\` (fields:
\`fused_by_model\`, \`final_test\`, \`scores\`, \`leaderboards\`).

## See also

\[wass2s_run_basins_stat()\], \[wass2s_run_basin_mods_stat()\]
