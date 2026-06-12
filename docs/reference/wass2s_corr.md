# Pearson Correlation Coefficient (vector form)

Compute the Pearson correlation coefficient (PCC) between two numeric
vectors, using [`stats::cor()`](https://rdrr.io/r/stats/cor.html)
internally. The PCC measures the strength and direction of the linear
relationship between two variables.

## Usage

``` r
wass2s_corr(truth, estimate, use = "pairwise.complete.obs", method = "pearson")
```

## Arguments

- truth:

  Numeric vector of observations.

- estimate:

  Numeric vector of predictions or simulated values.

- use:

  Character string indicating how missing values are handled. Passed to
  [`stats::cor()`](https://rdrr.io/r/stats/cor.html). Default is
  `"pairwise.complete.obs"`.

- method:

  Correlation method to use; default is `"pearson"`.

## Value

A single numeric value representing the Pearson correlation coefficient
(range \\\[-1, 1\]\\). Returns `NA_real_` if undefined.

## Details

The Pearson correlation coefficient (PCC) is defined as the ratio
between the covariance of two variables and the product of their
standard deviations. It always lies in the range \\\[-1, 1\]\\:

- \\r = 1\\: perfect positive linear relationship;

- \\r = -1\\: perfect negative linear relationship;

- \\r = 0\\: no linear correlation.

If either variable has zero standard deviation (constant values),
`NA_real_` is returned.

## Examples

``` r
if (FALSE) { # \dontrun{
obs <- c(1, 2, 3, 4, 5)
sim <- c(1.1, 1.9, 3.2, 3.8, 5.1)
wass2s_corr(obs, sim)

# With missing values
sim[3] <- NA
wass2s_corr(obs, sim)
} # }
```
