# High-level runner for WASS2S

High-level runner for WASS2S

## Usage

``` r
run_wass2s(method, data_by_product, cfg)
```

## Arguments

- method:

  One of "stat", "hydro" (later "ai")

- data_by_product:

  Named list of data.frames

- cfg:

  List or path to YAML configuration file

## Value

Results from the selected method

## Examples

``` r
if (FALSE) { # \dontrun{
# Example usage
result <- run_wass2s("hydro", data_by_product, "config.yaml")
} # }
```
