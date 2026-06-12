# Class thresholds from hydrological climatology (quartiles by default)

Compute class thresholds from a historical discharge series. By default
uses quartiles: t1 = Q1 (25 below: Q \< Q1; normal: Q1 \<= Q \<= Q3;
above: Q \> Q3.

## Usage

``` r
wass2s_class_thr(q_hist, probs = c(0.25, 0.75), na.rm = TRUE)
```

## Arguments

- q_hist:

  numeric vector of historical discharges.

- probs:

  length-2 numeric of cumulative probs for thresholds (default c(0.25,
  0.75) for Q1/Q3).

- na.rm:

  logical; remove NAs.

## Value

named numeric c(t1, t2).
