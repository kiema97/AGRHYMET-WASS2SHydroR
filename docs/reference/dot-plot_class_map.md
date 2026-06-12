# Plot final class map (dominant class)

Join a basin geometry and a table with the dominant class label, then
plot a categorical map using the requested colors. NAs (unknown/no
forecast) are colored with the climatology color.

## Usage

``` r
.plot_class_map(
  sf_basins,
  class_df,
  basin_col = "HYBAS_ID",
  colors = list(above = "#41AB5D", normal = "#A1D937", below = "#EC7014", climatology =
    "#BEBEBE"),
  title = NULL,
  file = NULL,
  width = 7,
  height = 6
)
```

## Arguments

- sf_basins:

  sf object or path to a vector file (shapefile, gpkg, …).

- class_df:

  data.frame with one row per basin and columns: `basin_col`,
  `class_hat` in `c("below", "normal", "above")`.

- basin_col:

  name of the basin id column shared by both inputs.

- colors:

  named colors for classes, default: list(above="#41AB5D",
  normal="#A1D937", below="#EC7014", climatology="#BEBEBE").

- title:

  optional title.

- file:

  optional path to save the figure via ggsave.

- width, height:

  inches for saving (if \`file\` supplied).

## Value

ggplot object (invisibly if saved).
