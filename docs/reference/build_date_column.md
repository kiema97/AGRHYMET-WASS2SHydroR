# Build a unified DATE column from available time information

Internal helper that reconstructs a `DATE` column from a data table
containing one or more time-related columns.

## Usage

``` r
build_date_column(
  DT,
  time_col = NULL,
  ref_col = NULL,
  period_col = NULL,
  tz = "UTC"
)
```

## Arguments

- DT:

  A `data.table` containing the raw data.

- time_col:

  Optional name of the direct time column.

- ref_col:

  Optional name of the forecast reference time column.

- period_col:

  Optional name of the forecast lead time / period column.

- tz:

  Time zone used to construct the output `DATE` vector.

## Value

A `POSIXct` vector of length `nrow(DT)`.

## Details

The function first tries to parse a direct time column. If unsuccessful,
it then attempts to reconstruct dates from a forecast reference time and
a forecast period / lead time column.
