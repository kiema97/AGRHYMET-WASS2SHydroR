# Faceted metric maps (continuous) for hydrological performance

Plot basin-level performance metrics (e.g., KGE, NSE, RMSE) on maps,
with one facet per metric. Handles both divergent metrics (can be
negative) and positive-only metrics. If viridis is available, it is used
by default.

## Usage

``` r
plot_metric_maps(
  sf_basins,
  metrics_df,
  basin_col = "HYBAS_ID",
  metrics,
  limits = NULL,
  divergent_metrics = c("KGE", "NSE"),
  title = NULL,
  file = NULL,
  width = 10,
  height = 6
)
```

## Arguments

- sf_basins:

  An `sf` object or a path to a vector file (read via
  [`sf::st_read`](https://r-spatial.github.io/sf/reference/st_read.html)).

- metrics_df:

  A data.frame with one row per basin, containing `basin_col` and the
  metric columns to plot.

- basin_col:

  Name of the join key present in both inputs (default "HYBAS_ID").

- metrics:

  Character vector of metric column names to facet (e.g.,
  c("KGE","NSE","RMSE")).

- limits:

  Optional named list of numeric length-2 vectors overriding color
  limits per metric, e.g.
  `list(KGE=c(-0.5,1), NSE=c(-0.5,1), RMSE=c(0,800))`.

- divergent_metrics:

  Character vector of metric names to plot with a divergent palette
  (e.g., c("KGE","NSE")). Others use a sequential palette.

- title:

  Optional figure title.

- file:

  Optional path to save with
  [`ggplot2::ggsave()`](https://ggplot2.tidyverse.org/reference/ggsave.html).

- width, height:

  Size in inches when saving.

## Value

A ggplot object (returned invisibly if saved).
