# Extract (YYYY, Q) for a basin from the best available product

Picks the product with the largest number of non-missing Q values for
the basin.

## Usage

``` r
get_any_Q(data_by_product, basin_id, basin_col = "HYBAS_ID")
```

## Arguments

- data_by_product:

  Named list of data frames (per product).

- basin_id:

  Basin identifier value.

- basin_col:

  Name of the basin ID column.

## Value

Tibble (YYYY, Q) or empty tibble if not found.
