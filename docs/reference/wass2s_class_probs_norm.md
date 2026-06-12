# Class probabilities under Normal assumption (Q1/Q3 classes by default)

Given Normal(mean = mu, sd = sigma) and thresholds (t1=Q1, t2=Q3),
returns P(below), P(normal), P(above) with: below: X \< t1; normal: t1
\<= X \<= t2; above: X \> t2.

## Usage

``` r
wass2s_class_probs_norm(mu, sigma, thresholds)
```

## Arguments

- mu:

  numeric vector of predictive means.

- sigma:

  numeric vector of predictive std devs (\>0).

- thresholds:

  named numeric with elements t1, t2.

## Value

tibble with columns p_below, p_normal, p_above.
