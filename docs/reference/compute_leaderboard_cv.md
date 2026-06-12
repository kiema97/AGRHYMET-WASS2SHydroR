# Leaderboard by cross-validation configuration

Leaderboard by cross-validation configuration

## Usage

``` r
compute_leaderboard_cv(tuned, truth_col = "Q", estimate_col = ".pred")
```

## Arguments

- tuned:

  A tune_grid() result or object accepted by tune::collect_predictions.

- truth_col:

  Name of the truth column in collect_predictions() (default "Q").

- estimate_col:

  Name of the prediction column in collect_predictions() (default
  ".pred").

## Value

Tibble with columns .config, kge_mean, rmse_mean, mae_mean and n_splits.
