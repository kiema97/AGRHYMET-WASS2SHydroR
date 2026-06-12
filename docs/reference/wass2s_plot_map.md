# Generic map plotter for WASS2S products (probabilities, class, metrics)

High-level S3 dispatcher that routes to the appropriate map function
based on the structure of \`data\` \*\*or\*\* an explicit \`type\`
chosen by the user.

## Usage

``` r
wass2s_plot_map(
  sf_basins,
  data,
  basin_col = "HYBAS_ID",
  type = c("auto", "probs", "class", "metrics", "metric_classes"),
  layers = NULL,
  layer_position = c("below", "above"),
  sf_crop = NULL,
  basin_line_color = NULL,
  basin_line_size = NULL,
  ...
)
```

## Arguments

- sf_basins:

  sf object or path to a vector file readable by \`sf::st_read()\`.

- data:

  data.frame/tibble (1 row per basin) containing the expected columns
  depending on \`type\`.

- basin_col:

  join key with \`sf_basins\` (default \`"HYBAS_ID"\`).

- type:

  one of \`"auto"\`, \`"probs"\`, \`"class"\`, \`"metrics"\`,
  \`"metric_classes"\`.

- layers:

  Optional list of extra ggplot2 layers. Supports both: - plain layers:
  \`list(geom_sf(...), ...)\` (all positioned by \`layer_position\`) -
  positioned layers: \`list(list(layer=geom_sf(...), position="below"),
  ...)\`

- layer_position:

  Default position for plain \`layers\` ("below" or "above").

- sf_crop:

  Optional sf object/path used to crop plot extent (e.g., country
  boundary).

- basin_line_color:

  Basin borders color (when supported by the underlying plotter).

- basin_line_size:

  Basin borders size (when supported by the underlying plotter).

- ...:

  Additional arguments passed to methods and underlying plotters (e.g.,
  \`metrics=\`, \`limits=\`, \`palette=\`, \`facet_labels=\`, \`file=\`,
  etc.).

## Value

a \`ggplot2::ggplot\` object (invisible if saved by the method).

## Details

Supported \`type\` values: - \`"auto"\` : automatic detection based on
available columns - \`"probs"\` : probability maps (requires
\`p_below,p_normal,p_above\`) - \`"class"\` : dominant class map
(requires \`class_hat\`) - \`"metrics"\` : continuous metric maps (e.g.,
\`KGE, RMSE\`) with facets - \`"metric_classes"\` : categorical map
based on a \`quality\` column
