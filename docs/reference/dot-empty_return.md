# Build a standardized empty return object for consolidation functions

Build a standardized empty return object for consolidation functions

## Usage

``` r
.empty_return(
  dates_all = integer(),
  leaderboard = NULL,
  results = list(),
  extra_cols = NULL
)
```

## Arguments

- dates_all:

  Integer vector of YYYYMMDD dates to keep in output.

- leaderboard:

  Optional tibble to use as leaderboard (will be completed).

- results:

  List of per-product results.

- extra_cols:

  Optional named list defining extra leaderboard columns and their empty
  types.
