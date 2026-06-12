# Kling-Gupta Efficiency (vector form)

Compute the Kling-Gupta Efficiency (KGE) between observations and
predictions. Returns a single numeric value in \\(-\infty, 1\]\\; higher
is better.

## Usage

``` r
wass2s_kge(truth, estimate)
```

## Arguments

- truth:

  Numeric vector of observations.

- estimate:

  Numeric vector of predictions.

## Value

A single numeric value (KGE). Returns \`NA_real\_\` if not computable
(e.g., non-finite, zero variance, zero means leading to division by
zero).

## Examples

``` r
if (FALSE) { # \dontrun{
set.seed(1)
y  <- rnorm(100, 10, 2)
yhat <- y + rnorm(100, 0, 1)
wass2s_kge(y, yhat)
} # }
```
