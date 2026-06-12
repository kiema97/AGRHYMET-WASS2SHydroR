# S3 entrypoint for method "ml"

`method_id("ml")` is an alias of the hydro/ML pipeline.

## Usage

``` r
# S3 method for class 'method_ml'
run_method(method, data_by_product, cfg)
```

## Arguments

- method:

  object created by method_id("ml")

- data_by_product:

  named list of data.frames (one per product)

- cfg:

  list with configuration

## Value

Same structure as
[`run_method.method_hydro()`](https://kiema97.github.io/WASS2SHydroR/reference/run_method.method_hydro.md).
