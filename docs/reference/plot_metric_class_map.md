# Categorical quality map from a continuous metric

Discretize a metric (e.g., KGE) into quality classes using breakpoints,
then plot a categorical `sf` map.

## Usage

``` r
plot_metric_class_map(
  sf_basins,
  metrics_df,
  basin_col = "HYBAS_ID",
  metric = "KGE",
  breaks = c(-Inf, 0.2, 0.5, 0.75, Inf),
  labels = c("Very poor", "Poor", "Fair", "Good"),
  colors = c(`Very poor` = "#b2182b", Poor = "#ef8a62", Fair = "#67a9cf", Good =
    "#2166ac"),
  title = NULL,
  file = NULL,
  width = 7,
  height = 6
)
```

## Arguments

- sf_basins:

  sf object or path.

- metrics_df:

  data.frame with `basin_col` and a continuous metric column.

- basin_col:

  join key (default "HYBAS_ID").

- metric:

  name of the continuous metric column (e.g., "KGE").

- breaks:

  numeric vector of breakpoints (default c(-Inf, 0.2, 0.5, 0.75, Inf)).

- labels:

  character labels for intervals (length = length(breaks)-1).

- colors:

  named vector of colors for labels (same length as labels).

- title:

  optional title; @param file path to save; @param width,height inches.

## Value

ggplot object.
