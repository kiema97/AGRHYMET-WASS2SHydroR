# Detect high-flow periods from daily discharge time series

Detects one or several high-flow periods within each basin, either over
the full record or within each analysis year (calendar or hydrological
year). High-flow periods are identified using either a quantile-based
threshold or a z-score threshold, with optional smoothing and merging of
short gaps.

## Usage

``` r
wass2s_detect_high_flow_periods(
  data,
  id_col = "station_id",
  date_col = "date",
  flow_col = "Q",
  year_type = c("calendar", "hydro"),
  hydro_year_start_month = 7L,
  detection_scope = c("id_year", "id"),
  threshold_method = c("quantile", "zscore"),
  q_prob = 0.8,
  z_thr = 1,
  smooth_k = 1L,
  min_len_days = 5L,
  max_gap_days = 3L,
  min_obs = 10L,
  period_rank = "all",
  period_order = c("chronological", "magnitude", "duration"),
  na_rm = TRUE
)
```

## Arguments

- data:

  A data frame containing at least an identifier column, a date column,
  and a discharge column.

- id_col:

  Character string. Name of the station or basin identifier column.

- date_col:

  Character string. Name of the date column.

- flow_col:

  Character string. Name of the discharge column.

- year_type:

  Character string. Either \`"calendar"\` or \`"hydro"\`.

- hydro_year_start_month:

  Integer. Starting month of the hydrological year when \`year_type =
  "hydro"\`. Default is \`7\`.

- detection_scope:

  Character string. Detection scope: \`"id_year"\` or \`"id"\`.

- threshold_method:

  Character string. Thresholding method: \`"quantile"\` or \`"zscore"\`.

- q_prob:

  Numeric. Quantile probability used when \`threshold_method =
  "quantile"\`. Default is \`0.80\`.

- z_thr:

  Numeric. Z-score threshold multiplier used when \`threshold_method =
  "zscore"\`. Default is \`1.0\`.

- smooth_k:

  Integer. Rolling mean window size. Use \`1\` for no smoothing. Odd
  values are recommended.

- min_len_days:

  Integer. Minimum duration required to retain a period.

- max_gap_days:

  Integer. Maximum gap allowed to merge two segments.

- min_obs:

  Integer. Minimum number of non-missing observations required within a
  detection group.

- period_rank:

  Either \`"all"\` or a positive integer indicating which detected
  period to return within each group.

- period_order:

  Character string indicating how detected periods should be ranked:
  \`"chronological"\`, \`"magnitude"\`, or \`"duration"\`.

- na_rm:

  Logical. Whether to ignore missing values when computing thresholds.

## Value

A data frame with one row per detected high-flow period.

## Details

The function can return all detected periods or only a selected period
rank (e.g. first, second, third) within each detection unit. This is
particularly useful for rivers with bimodal or multimodal flow regimes.
