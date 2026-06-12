# Plot probability maps for below/normal/above classes (faceted)

Join a basin geometry and a table of class probabilities, then plot
faceted maps (one facet per class). Expects columns \`p_below\`,
\`p_normal\`, \`p_above\`. If your table has several years, filter
beforehand.

## Usage

``` r
plot_prob_maps_(
  sf_basins,
  probs_df,
  basin_col = "HYBAS_ID",
  limits = c(0, 1),
  palette = c("viridis"),
  facet_labels = c(p_below = "Below", p_normal = "Normal", p_above = "Above"),
  title = NULL,
  file = NULL,
  width = 10,
  height = 6
)
```

## Arguments

- sf_basins:

  sf object or path to a vector file (shapefile, gpkg, …).

- probs_df:

  data.frame with one row per basin and columns: \`basin_col\`,
  \`p_below\`, \`p_normal\`, \`p_above\`.

- basin_col:

  name of the basin id column shared by both inputs.

- limits:

  numeric length-2 for the fill scale limits (default c(0,1)).

- palette:

  continuous palette for probabilities (e.g. "viridis", "magma").

- facet_labels:

  named character vector to rename facets, e.g. c(p_below="Below",
  p_normal="Normal", p_above="Above").

- title:

  optional title.

- file:

  optional path to save the figure via ggsave (format inferred from
  extension).

- width, height:

  units in inches for saving (if \`file\` supplied).

## Value

ggplot object (invisibly if saved).
