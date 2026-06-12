# Full-join a list of (YYYY, pred) tibbles with renaming

Full-join a list of (YYYY, pred) tibbles with renaming

## Usage

``` r
safe_full_join_preds(lst)
```

## Arguments

- lst:

  Named list of tibbles. Each must have columns YYYY, pred.

## Value

Tibble with YYYY and one column per element name.
