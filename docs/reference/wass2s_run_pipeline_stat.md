# Wrapper function called by the core (run_method) for statistical pipeline

This function reads useful parameters from \`cfg\` (YAML) and delegates
to \`wass2s_run_basins_stat()\`. It exists solely to provide a
standardized interface to the core.

## Usage

``` r
wass2s_run_pipeline_stat(data_by_product, cfg)
```

## Arguments

- data_by_product:

  Named list of data frames (one per product)

- cfg:

  R list from YAML (see config/config.dev.yaml)

## Value

Same structure as \`wass2s_run_basins_stat()\`
