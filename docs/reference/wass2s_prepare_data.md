# Prepare gridded hydro-climatic data from NetCDF or stars objects

Prepare gridded hydro-climatic data for downstream analysis, modelling,
or forecasting workflows. The function reads a NetCDF file or a `stars`
object, reconstructs a robust `DATE` column, optionally filters the
spatial domain using a bounding box, handles ensemble members, and
returns the result in either long or wide tabular format.

## Usage

``` r
wass2s_prepare_data(
  x,
  bbox = NULL,
  spatial_reduce = c("none", "mean", "median", "min", "max"),
  cell_layout = c("long", "wide"),
  cell_prefix = "val",
  dim_lon = NULL,
  dim_lat = NULL,
  dim_time = NULL,
  dim_ref_time = NULL,
  dim_period = NULL,
  dim_member = NULL,
  ensemble_reduce = c("mean", "median", "min", "max", "none"),
  keep_member = FALSE,
  extra_dims_action = c("warn_mean", "error", "drop"),
  tz = "UTC",
  verbose = TRUE
)
```

## Arguments

- x:

  A path to a NetCDF file or a `stars` object.

- bbox:

  Optional bounding box used to spatially subset the grid. It can be:

  - a named numeric vector with elements `xmin`, `ymin`, `xmax`, `ymax`;

  - an unnamed numeric vector interpreted heuristically.

- spatial_reduce:

  Character string specifying how to spatially aggregate grid cells. One
  of `"none"`, `"mean"`, `"median"`, `"min"`, or `"max"`.

- cell_layout:

  Output layout when `spatial_reduce = "none"`. Either `"long"` or
  `"wide"`.

- cell_prefix:

  Prefix used for column names in wide format.

- dim_lon:

  Optional name of the longitude dimension/column.

- dim_lat:

  Optional name of the latitude dimension/column.

- dim_time:

  Optional name of the time dimension/column.

- dim_ref_time:

  Optional name of the forecast reference time dimension/column.

- dim_period:

  Optional name of the forecast lead time / period dimension/column.

- dim_member:

  Optional name of the ensemble member dimension/column.

- ensemble_reduce:

  Character string specifying how to aggregate ensemble members. One of
  `"mean"`, `"median"`, `"min"`, `"max"`, or `"none"`.

- keep_member:

  Logical. If `TRUE` and `ensemble_reduce = "none"`, keep the ensemble
  member identifier in the output when available.

- extra_dims_action:

  How to handle unexpected extra dimensions after the main dimensions
  have been identified. One of:

  - `"warn_mean"`: collapse extra dimensions by mean and emit a warning;

  - `"error"`: stop with an informative error;

  - `"drop"`: drop extra dimensions before deduplication.

- tz:

  Time zone used when constructing the `DATE` column. Defaults to
  `"UTC"`.

- verbose:

  Logical. If `TRUE`, informative messages and warnings are emitted
  during processing.

## Value

A `data.frame` containing at least a `DATE` column and one or more value
columns depending on the selected output mode:

- if `spatial_reduce != "none"`, the result contains one value per time
  step;

- if `cell_layout = "long"`, the result contains `DATE`, `lon`, `lat`,
  and `value`;

- if `cell_layout = "wide"`, each grid cell becomes a separate column.

## Details

This function is designed for hydro-climatic datasets such as
precipitation, temperature, evapotranspiration, or forecast products
distributed on regular longitude/latitude grids.

The function attempts to reconstruct the `DATE` column as robustly as
possible. It supports:

- direct date/time vectors stored as `Date` or `POSIXct`;

- character date/time representations such as `"YYYY-MM-DD"` or
  `"YYYY-MM-DD HH:MM:SS"`;

- numeric time vectors with CF-compliant units such as
  `"days since 1900-01-01"`;

- forecast products using `forecast_reference_time` combined with
  `forecast_period`.

Longitude values can be automatically aligned between `[-180, 180]` and
`[0, 360]` conventions when needed for spatial filtering.

At present, curvilinear grids are not supported and trigger an explicit
error.

When `cell_layout = "wide"`, the returned object includes a `"cell_map"`
attribute describing the mapping between generated cell columns and
their corresponding longitude/latitude coordinates.

## Examples

``` r
if (FALSE) { # \dontrun{
# Example 1: spatial mean over a bounding box
res <- wass2s_prepare_data(
  x = "precip.nc",
  bbox = c(xmin = -5, ymin = 10, xmax = 2, ymax = 15),
  spatial_reduce = "mean"
)

# Example 2: long format without spatial aggregation
res <- wass2s_prepare_data(
  x = "temperature.nc",
  spatial_reduce = "none",
  cell_layout = "long"
)

# Example 3: wide format
res <- wass2s_prepare_data(
  x = "forecast.nc",
  spatial_reduce = "none",
  cell_layout = "wide",
  cell_prefix = "cell"
)

# Access the grid-to-column correspondence
attr(res, "cell_map")
} # }
```
