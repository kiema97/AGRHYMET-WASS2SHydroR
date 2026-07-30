# Plot final class map (dominant class) with per-layer positioning

Join a basin geometry and a table with the dominant class label, then
plot a categorical map using the requested colors. NAs (unknown/no
forecast) are colored with the climatology color.

## Usage

``` r
plot_class_map(
  sf_basins,
  class_df,
  basin_col = "HYBAS_ID",
  colors = list(above = "#41AB5D", normal = "#A1D937", below = "#EC7014", climatology =
    "#BEBEBE"),
  title = NULL,
  file = NULL,
  width = 7,
  height = 6,
  layers = NULL,
  layer_position = c("below", "above"),
  sf_crop = NULL,
  basin_line_color = "grey70",
  basin_line_size = 0.1
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

- layers:

  optional list of layers. Two supported formats: - ggplot2 layers:
  \`list(geom_sf(...), geom_sf(...))\` (all placed by
  \`layer_position\`) - positioned layers: \`list(list(layer =
  geom_sf(...), position = "below"), list(layer = geom_sf(...), position
  = "above"))\`

- layer_position:

  default position for \`layers\` when passed as plain ggplot layers
  (ignored when \`layers\` is passed in positioned format).

- sf_crop:

  optional sf object or path used to crop the basins extent (e.g.,
  country boundary).

- basin_line_color:

  basin border color.

- basin_line_size:

  basin border size.

## Value

ggplot object (invisibly if saved).

## Details

Extra layers can be added either: 1) As a list of ggplot2 layers (all
added "below" by default), or 2) As a list of items with \`layer\` and
\`position\` (position per layer), where position is "below" or "above".
