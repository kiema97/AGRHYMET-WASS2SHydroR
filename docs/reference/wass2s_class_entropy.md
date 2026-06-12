# Compute entropy of class probabilities (uncertainty indicator)

This function computes the Shannon entropy of class probabilities
(below, normal, above) to quantify forecast uncertainty.

## Usage

``` r
wass2s_class_entropy(p_below, p_normal, p_above, normalize = FALSE)
```

## Arguments

- p_below:

  Numeric vector. Probability of the "below normal" class.

- p_normal:

  Numeric vector. Probability of the "normal" class.

- p_above:

  Numeric vector. Probability of the "above normal" class.

- normalize:

  Logical. If TRUE, entropy is normalized to \[0, 1\].

## Value

Numeric vector of entropy values.

## Details

Entropy is minimal (0) when one class has probability 1 (deterministic
forecast), and maximal when probabilities are evenly distributed.

\- Probabilities are automatically cleaned and normalized. - Non-finite
values are treated as zero. - Rows with invalid probabilities (sum \<=
0) return NA.

## Examples

``` r
wass2s_class_entropy(
  p_below = c(1, 0.33),
  p_normal = c(0, 0.33),
  p_above = c(0, 0.34)
)
#> [1] 0.000000 1.098513
```
