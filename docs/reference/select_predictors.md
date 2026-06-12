# Select predictor columns by regex

Select predictor columns by regex

## Usage

``` r
select_predictors(df, pattern = "^pt_", exclude = c("YYYY", "Q"))
```

## Arguments

- df:

  Data frame.

- pattern:

  Regex pattern for predictors (default "^pt\_").

- exclude:

  Columns to always exclude (default: c("YYYY","Q")).

## Value

Character vector of predictor names present in df.
