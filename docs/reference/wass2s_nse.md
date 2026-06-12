# Nash–Sutcliffe Efficiency (vector)

Compute the Nash–Sutcliffe Efficiency (NSE) between observations and
predictions. Returns a single numeric value, with 1 = perfect, ~0 = mean
model baseline, and negative values indicating performance worse than
the mean model.

## Usage

``` r
wass2s_nse(truth, estimate)
```

## Arguments

- truth:

  Numeric vector of observations.

- estimate:

  Numeric vector of predictions.

## Value

A single numeric value (NSE). Returns \`NA_real\_\` if not computable
(e.g., non-finite, zero variance of observations).

## Examples

``` r
if (FALSE) { # \dontrun{
set.seed(1)
y  <- rnorm(100, 10, 2)
yhat <- y + rnorm(100, 0, 1)
wass2s_nse(y, yhat)
} # }
```
