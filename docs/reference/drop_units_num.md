# Convert a units-aware vector to numeric

Internal helper that converts vectors with units to plain numeric
values.

## Usage

``` r
drop_units_num(v)
```

## Arguments

- v:

  A numeric-like vector, possibly carrying units.

## Value

A numeric vector.

## Details

If the units package is available and the input inherits from class
`units`, units are dropped safely before conversion. Otherwise, the
function falls back to
[`as.numeric()`](https://rdrr.io/r/base/numeric.html).
