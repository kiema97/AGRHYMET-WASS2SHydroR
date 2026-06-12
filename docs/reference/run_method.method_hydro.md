# S3 entrypoint for method "hydro"

Called by run_method() when the 'method' object has class
'method_hydro'.

## Usage

``` r
# S3 method for class 'method_hydro'
run_method(method, data_by_product, cfg)
```

## Arguments

- method:

  object created by method_id("hydro")

- data_by_product:

  named list of data.frames (one per product)

- cfg:

  list with configuration (see Details)

## Value

list (same structure as run_one_basin_all_models_ml /
wass2s_run_basins_ml)
