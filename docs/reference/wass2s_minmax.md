# Min–max normalization to a target range

Scale numeric data to a target range \\\[a, b\]\\ (default \\\[0, 1\]\\)
using the min–max transform. Works with numeric vectors, matrices, and
data frames (numeric columns). Handles missing values and constant
columns robustly.

## Usage

``` r
wass2s_minmax(
  x,
  range = c(0, 1),
  na.rm = TRUE,
  finite_only = TRUE,
  by_col = TRUE,
  constant_handling = c("zero", "mid", "NA", "error")
)
```

## Arguments

- x:

  A numeric vector, matrix, or data frame (numeric columns).

- range:

  Target range as a length-2 numeric vector `c(a, b)`. Default
  `c(0, 1)`.

- na.rm:

  Logical; if `TRUE` (default), `NA` are ignored when computing min/max.
  Original `NA` are kept in the output.

- finite_only:

  Logical; if `TRUE` (default), only finite values are used to compute
  min/max (excludes `NA`, `NaN`, `Inf`).

- by_col:

  Logical; if `TRUE`, scale each column independently (for matrix/data
  frame). If `FALSE`, use global min/max. Default `TRUE`.

- constant_handling:

  One of `"zero"`, `"mid"`, `"NA"`, `"error"`: strategy when
  `max == min` for a vector/column:

  - `"zero"` (default): return the lower bound `range[1]`.

  - `"mid"`: return the midpoint `mean(range)`.

  - `"NA"`: return `NA_real_`.

  - `"error"`: stop with an informative message.

## Value

An object of the same shape as `x`, scaled to `range`.

## Examples

``` r
x <- c(-78, -2, 0.5, 1, -4)
wass2s_minmax(x)
#> [1] 0.0000000 0.9620253 0.9936709 1.0000000 0.9367089
wass2s_minmax(x, range = c(-1, 1))
#> [1] -1.0000000  0.9240506  0.9873418  1.0000000  0.8734177

# Matrix (by column)
m <- cbind(a = x, b = x * 2)
wass2s_minmax(m, by_col = TRUE)
#>              a         b
#> [1,] 0.0000000 0.0000000
#> [2,] 0.9620253 0.9620253
#> [3,] 0.9936709 0.9936709
#> [4,] 1.0000000 1.0000000
#> [5,] 0.9367089 0.9367089

# Data frame (numeric columns only)
df <- data.frame(a = x, b = x^2, c = factor(c("u","v","u","v","u")))
wass2s_minmax(df)  # only numeric columns are scaled; non-numeric are kept as-is
#>           a            b c
#> 1 0.0000000 1.0000000000 u
#> 2 0.9620253 0.0006163961 v
#> 3 0.9936709 0.0000000000 u
#> 4 1.0000000 0.0001232792 v
#> 5 0.9367089 0.0025888638 u
```
