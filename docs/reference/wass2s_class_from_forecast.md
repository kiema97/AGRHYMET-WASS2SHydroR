# From forecast to class probabilities (Q1/Q3 classes by default)

Vectorized wrapper: for a series with columns YYYY and pred (forecast
mean), compute class probabilities using Normal assumption and Q1/Q3
thresholds.

## Usage

``` r
wass2s_class_from_forecast(
  df,
  q_hist,
  sigma = NULL,
  rmse = NULL,
  residuals = NULL,
  thresholds = NULL,
  min_sigma_frac = 0.05
)
```

## Arguments

- df:

  data frame with columns YYYY, pred.

- q_hist:

  numeric historical Q (climatology for thresholds & sigma fallback).

- sigma:

  optional numeric vector (same length as pred) of predictive sd.

- rmse:

  optional numeric; if sigma is NULL, use this constant sd.

- residuals:

  optional numeric residuals to estimate sd; ignored if sigma provided.

- thresholds:

  optional named numeric c(t1, t2); if NULL, computed from q_hist.

- min_sigma_frac:

  minimal sigma as fraction of sd(q_hist) (default 0.05).

## Value

tibble: YYYY, pred, p_below, p_normal, p_above, class_hat, entropy.
