# Mean Absolute Error (vector)

Compute the Mean Absolute Error (MAE) between observations and
predictions.

## Usage

``` r
wass2s_mae(truth, estimate)
```

## Arguments

- truth:

  Numeric vector of observations.

- estimate:

  Numeric vector of predictions.

## Value

A single numeric value (MAE). Returns \`NA_real\_\` if not computable.

## Examples

``` r
if (FALSE) { # \dontrun{
wass2s_mae(1:5, c(1,2,2,4,6))
} # }
```
