# Root Mean Squared Error (vector)

Compute the Root Mean Squared Error (RMSE) between observations and
predictions.

## Usage

``` r
wass2s_rmse(truth, estimate)
```

## Arguments

- truth:

  Numeric vector of observations.

- estimate:

  Numeric vector of predictions.

## Value

A single numeric value (RMSE). Returns \`NA_real\_\` if not computable.

## Examples

``` r
if (FALSE) { # \dontrun{
wass2s_rmse(1:5, c(1,2,2,4,6))
} # }
```
