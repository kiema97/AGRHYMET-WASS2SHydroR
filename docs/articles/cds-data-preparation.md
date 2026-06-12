# CDS Data Acquisition and Preparation

## CDS Configuration

[`wass2s_download_cds()`](https://kiema97.github.io/AGRHYMET-WASS2SHydroR/reference/wass2s_download_cds.md)
uses the `ecmwfr` package. The CDS key must be configured before
downloading data:

``` r
ecmwfr::wf_set_key(user = "ecmwfr")
```

The package default is `user = "ecmwfr"`, which matches the default user
name used by `ecmwfr`.

You can check the local key with:

``` r
ecmwfr::wf_get_key(user = "ecmwfr")
```

## Data Acquisition Schema

![](data:image/svg+xml;base64,PHN2ZyB2aWV3Ym94PSIwIDAgOTgwIDE5MCIgcm9sZT0iaW1nIiBhcmlhLWxhYmVsPSJDRFMgZG93bmxvYWQgYW5kIHByZXBhcmF0aW9uIHByb2Nlc3MiIHN0eWxlPSJ3aWR0aDoxMDAlOyBtYXgtd2lkdGg6OTgwcHg7IGhlaWdodDphdXRvOyI+PGRlZnM+PG1hcmtlciBpZD0iYXJyb3ctY2RzIiBtYXJrZXJ3aWR0aD0iMTAiIG1hcmtlcmhlaWdodD0iMTAiIHJlZng9IjgiIHJlZnk9IjMiIG9yaWVudD0iYXV0byIgbWFya2VydW5pdHM9InN0cm9rZVdpZHRoIj48cGF0aCBkPSJNMCwwIEwwLDYgTDksMyB6IiBmaWxsPSIjMWI2ZjhmIiAvPjwvbWFya2VyPjwvZGVmcz48cmVjdCB4PSIyMCIgeT0iNTAiIHdpZHRoPSIxNDUiIGhlaWdodD0iNzUiIHJ4PSI4IiBmaWxsPSIjZThmM2Y3IiBzdHJva2U9IiMxYjZmOGYiIHN0cm9rZS13aWR0aD0iMiIgLz48dGV4dCB4PSI5MiIgeT0iODAiIHRleHQtYW5jaG9yPSJtaWRkbGUiIGZvbnQtc2l6ZT0iMTUiIGZvbnQtd2VpZ2h0PSI3MDAiPlJlcXVlc3Qgc2V0dXA8L3RleHQ+PHRleHQgeD0iOTIiIHk9IjEwMiIgdGV4dC1hbmNob3I9Im1pZGRsZSIgZm9udC1zaXplPSIxMyI+ZGF0YXNldCwgeWVhcnMsPC90ZXh0Pjx0ZXh0IHg9IjkyIiB5PSIxMjAiIHRleHQtYW5jaG9yPSJtaWRkbGUiIGZvbnQtc2l6ZT0iMTMiPm1vZGVscywgdmFyaWFibGVzPC90ZXh0PjxyZWN0IHg9IjIxNSIgeT0iNTAiIHdpZHRoPSIxNDUiIGhlaWdodD0iNzUiIHJ4PSI4IiBmaWxsPSIjZWVmN2VkIiBzdHJva2U9IiM0YjhiM2IiIHN0cm9rZS13aWR0aD0iMiIgLz48dGV4dCB4PSIyODciIHk9IjgwIiB0ZXh0LWFuY2hvcj0ibWlkZGxlIiBmb250LXNpemU9IjE1IiBmb250LXdlaWdodD0iNzAwIj5DRFMgZG93bmxvYWQ8L3RleHQ+PHRleHQgeD0iMjg3IiB5PSIxMDIiIHRleHQtYW5jaG9yPSJtaWRkbGUiIGZvbnQtc2l6ZT0iMTMiPmVjbXdmciByZXF1ZXN0PC90ZXh0Pjx0ZXh0IHg9IjI4NyIgeT0iMTIwIiB0ZXh0LWFuY2hvcj0ibWlkZGxlIiBmb250LXNpemU9IjEzIj55ZWFyIGNodW5rczwvdGV4dD48cmVjdCB4PSI0MTAiIHk9IjUwIiB3aWR0aD0iMTQ1IiBoZWlnaHQ9Ijc1IiByeD0iOCIgZmlsbD0iI2ZmZjRkZiIgc3Ryb2tlPSIjYjY2ZDAwIiBzdHJva2Utd2lkdGg9IjIiIC8+PHRleHQgeD0iNDgyIiB5PSI4MCIgdGV4dC1hbmNob3I9Im1pZGRsZSIgZm9udC1zaXplPSIxNSIgZm9udC13ZWlnaHQ9IjcwMCI+TmV0Q0RGIGZpbGVzPC90ZXh0Pjx0ZXh0IHg9IjQ4MiIgeT0iMTAyIiB0ZXh0LWFuY2hvcj0ibWlkZGxlIiBmb250LXNpemU9IjEzIj5vbmUgZmlsZSBwZXI8L3RleHQ+PHRleHQgeD0iNDgyIiB5PSIxMjAiIHRleHQtYW5jaG9yPSJtaWRkbGUiIGZvbnQtc2l6ZT0iMTMiPmpvYi95ZWFyPC90ZXh0PjxyZWN0IHg9IjYwNSIgeT0iNTAiIHdpZHRoPSIxNDUiIGhlaWdodD0iNzUiIHJ4PSI4IiBmaWxsPSIjZjBlY2ZiIiBzdHJva2U9IiM2NjUwYTQiIHN0cm9rZS13aWR0aD0iMiIgLz48dGV4dCB4PSI2NzciIHk9IjgwIiB0ZXh0LWFuY2hvcj0ibWlkZGxlIiBmb250LXNpemU9IjE1IiBmb250LXdlaWdodD0iNzAwIj5QcmVwYXJhdGlvbjwvdGV4dD48dGV4dCB4PSI2NzciIHk9IjEwMiIgdGV4dC1hbmNob3I9Im1pZGRsZSIgZm9udC1zaXplPSIxMyI+REFURSwgbG9uLCBsYXQsPC90ZXh0Pjx0ZXh0IHg9IjY3NyIgeT0iMTIwIiB0ZXh0LWFuY2hvcj0ibWlkZGxlIiBmb250LXNpemU9IjEzIj52YWx1ZSBjb2x1bW5zPC90ZXh0PjxyZWN0IHg9IjgwMCIgeT0iNTAiIHdpZHRoPSIxNDUiIGhlaWdodD0iNzUiIHJ4PSI4IiBmaWxsPSIjZjhlZWVlIiBzdHJva2U9IiNhMzNkM2QiIHN0cm9rZS13aWR0aD0iMiIgLz48dGV4dCB4PSI4NzIiIHk9IjgwIiB0ZXh0LWFuY2hvcj0ibWlkZGxlIiBmb250LXNpemU9IjE1IiBmb250LXdlaWdodD0iNzAwIj5Nb2RlbCBpbnB1dHM8L3RleHQ+PHRleHQgeD0iODcyIiB5PSIxMDIiIHRleHQtYW5jaG9yPSJtaWRkbGUiIGZvbnQtc2l6ZT0iMTMiPmJhc2luLXByb2R1Y3Q8L3RleHQ+PHRleHQgeD0iODcyIiB5PSIxMjAiIHRleHQtYW5jaG9yPSJtaWRkbGUiIGZvbnQtc2l6ZT0iMTMiPnRhYmxlczwvdGV4dD48bGluZSB4MT0iMTY1IiB5MT0iODgiIHgyPSIyMDciIHkyPSI4OCIgc3Ryb2tlPSIjMWI2ZjhmIiBzdHJva2Utd2lkdGg9IjMiIG1hcmtlci1lbmQ9InVybCgjYXJyb3ctY2RzKSI+PC9saW5lPjxsaW5lIHgxPSIzNjAiIHkxPSI4OCIgeDI9IjQwMiIgeTI9Ijg4IiBzdHJva2U9IiMxYjZmOGYiIHN0cm9rZS13aWR0aD0iMyIgbWFya2VyLWVuZD0idXJsKCNhcnJvdy1jZHMpIj48L2xpbmU+PGxpbmUgeDE9IjU1NSIgeTE9Ijg4IiB4Mj0iNTk3IiB5Mj0iODgiIHN0cm9rZT0iIzFiNmY4ZiIgc3Ryb2tlLXdpZHRoPSIzIiBtYXJrZXItZW5kPSJ1cmwoI2Fycm93LWNkcykiPjwvbGluZT48bGluZSB4MT0iNzUwIiB5MT0iODgiIHgyPSI3OTIiIHkyPSI4OCIgc3Ryb2tlPSIjMWI2ZjhmIiBzdHJva2Utd2lkdGg9IjMiIG1hcmtlci1lbmQ9InVybCgjYXJyb3ctY2RzKSI+PC9saW5lPjwvc3ZnPg==)

## Download a Minimal Seasonal Forecast

The following request downloads one ECMWF seasonal forecast field for
one year, one initialization day, one lead time, and a small spatial
domain:

``` r
res_cds <- wass2s_download_cds(
  dataset_short_name = "seasonal-original-single-levels",
  base_query = list(format = "netcdf"),
  center_variables = "ecmwf_51.T2M",
  years = 2020,
  months = 1,
  days = "01",
  times = "00:00",
  leadtime_hour = 24,
  area = c(15, -2, 14, -1),
  out_dir = "data/cds",
  user = "ecmwfr",
  tries = 1,
  timeout_sec = 300,
  verbose = TRUE
)

res_cds
```

The returned table reports one row per request:

- `file`: downloaded NetCDF path;
- `status`: `ok`, `skip`, or `fail`;
- `error`: error message when the request fails;
- `model`, `system`, `variable`, `year`: request metadata.

## `center_variables` Format

Each entry uses:

``` text
model_system.variable
```

Examples:

``` r
c("ecmwf_51.T2M", "meteo_france_9.TMAX", "cmcc_35.PRCP")
```

Short variable names are mapped to CDS API names:

| Short name | CDS variable                                  |
|------------|-----------------------------------------------|
| `T2M`      | `2m_temperature`                              |
| `TMAX`     | `maximum_2m_temperature_in_the_last_24_hours` |
| `TMIN`     | `minimum_2m_temperature_in_the_last_24_hours` |
| `PRCP`     | `total_precipitation`                         |
| `SST`      | `sea_surface_temperature`                     |

## Multi-Year Requests

When several years are provided, requests are split year by year. This
reduces request size and avoids overwriting output files:

``` r
res_cds <- wass2s_download_cds(
  dataset_short_name = "seasonal-original-single-levels",
  base_query = list(format = "netcdf"),
  center_variables = c("ecmwf_51.T2M", "meteo_france_9.T2M"),
  years = 1993:1995,
  months = 4,
  days = "01",
  times = "00:00",
  leadtime_hour = seq(24, 240, 24),
  out_dir = "data/cds"
)
```

If the filename template does not include `{year}`, the year is appended
before the `.nc` extension.

## Prepare NetCDF Data

[`wass2s_prepare_data()`](https://kiema97.github.io/AGRHYMET-WASS2SHydroR/reference/wass2s_prepare_data.md)
reads NetCDF files or `stars` objects and returns a regular data frame
with a reconstructed `DATE` column.

Spatial mean:

``` r
df_mean <- wass2s_prepare_data(
  x = res_cds$file[res_cds$status == "ok"][1],
  spatial_reduce = "mean",
  verbose = TRUE
)

head(df_mean)
```

Long cell-by-cell output:

``` r
df_long <- wass2s_prepare_data(
  x = res_cds$file[res_cds$status == "ok"][1],
  spatial_reduce = "none",
  cell_layout = "long",
  verbose = FALSE
)

head(df_long)
```

Wide cell-by-cell output:

``` r
df_wide <- wass2s_prepare_data(
  x = res_cds$file[res_cds$status == "ok"][1],
  spatial_reduce = "none",
  cell_layout = "wide",
  cell_prefix = "cell"
)

attr(df_wide, "cell_map")
```

## Spatial Subsetting

Use `bbox` to crop before aggregation:

``` r
df_bbox <- wass2s_prepare_data(
  x = "data/cds/file.nc",
  bbox = c(xmin = -5, ymin = 10, xmax = 2, ymax = 15),
  spatial_reduce = "mean"
)
```

Longitude conventions `[-180, 180]` and `[0, 360]` are handled
automatically when possible.

## Notes

- Curvilinear grids are not currently supported.
- If dimensions are not detected automatically, provide `dim_lon`,
  `dim_lat`, `dim_time`, `dim_ref_time`, or `dim_period`.
- Leave `job_name = NULL` outside RStudio. Supplying `job_name` triggers
  RStudio Jobs in `ecmwfr`.
