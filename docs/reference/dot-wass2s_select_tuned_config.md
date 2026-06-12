# Select tuned parameters with an explicit scientific score contract

Select tuned parameters with an explicit scientific score contract

## Usage

``` r
.wass2s_select_tuned_config(
  tuned,
  leaderboard,
  selection_metric = c("rmse", "kge"),
  quiet = TRUE
)
```

## Arguments

- tuned:

  A tune_grid() result.

- leaderboard:

  Output of compute_leaderboard_cv().

- selection_metric:

  Either "rmse" or "kge".

- quiet:

  Suppress diagnostic messages.

## Value

List with params, selected_config and selected_score.
