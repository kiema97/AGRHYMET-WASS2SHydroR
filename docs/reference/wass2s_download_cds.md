# Robust Parallel Downloader for CDS/ECMWF (via ecmwfr) with Year Chunking

Downloads climate/meteorological data from CDS/ADS/CEMS using ecmwfr,
with support for multiple variables and models, robust retries, optional
batch-parallel submission, resume-on-existing files, and clear status
reporting.

## Usage

``` r
wass2s_download_cds(
  dataset_short_name,
  base_query,
  center_variables,
  years,
  months,
  days,
  times = "00:00",
  leadtime_hour,
  area = c(28.5, -25.5, 4, 26.6),
  out_dir = ".",
  user = "ecmwfr",
  service = c("cds", "ads", "cems"),
  filename_tpl = NULL,
  tries = 3,
  sleep_sec = 15,
  timeout_sec = 3600,
  force_download = FALSE,
  parallel = FALSE,
  workers = 2,
  job_name = NULL,
  verbose = TRUE,
  ...
)
```

## Arguments

- dataset_short_name:

  Character. The dataset short name on CDS/ADS/CEMS, e.g.,
  `"seasonal-original-single-levels"`.

- base_query:

  Named list. Static part of the query (e.g., `product_type`,
  `pressure_level`, `format`). Time-varying fields like `year`, `month`,
  `day`, `leadtime_hour` are set by this function.

- center_variables:

  Character vector of entries `"model_system.variable"`, e.g.,
  `"meteo_france_9.TMAX"`, `"ecmwf_51.TMAX"`. Only one variable per
  entry is supported.

- years:

  Integer/character vector. \*\*Required.\*\* Years to request. When
  multiple years are provided, requests are split one-per-year.

- months:

  Integer/character vector in `1..12`. \*\*Required.\*\*

- days:

  Character/integer vector (e.g., `c("01","02")`). \*\*Required.\*\*

- times:

  Character vector (e.g., `c("00:00","12:00")`). \*\*Required.\*\*

- leadtime_hour:

  Character/integer vector (e.g., `c("24","48",...)`). \*\*Required.\*\*

- area:

  Optional numeric length-4 `c(N, W, S, E)` bounding box.

- out_dir:

  Output directory. Created if missing.

- user:

  CDS/ADS/CEMS user ID used with
  [`ecmwfr::wf_set_key()`](https://rdrr.io/pkg/ecmwfr/man/wf_set_key.html).

- service:

  One of `"cds"`, `"ads"`, `"cems"` (case-insensitive). Default `"cds"`.

- filename_tpl:

  Glue-like template for filenames. Placeholders available:
  `{modelsys}`, `{var}`, `{init}`, `{period}`, `{lead}`, `{dataset}`,
  `{year}`. Default: `"{modelsys}_{var}_{init}_{period}_{lead}.nc"`.

- tries:

  Integer. Number of retry attempts per request (sequential mode).
  Default `3`.

- sleep_sec:

  Numeric. Seconds between retries (sequential mode). Default `15`.

- timeout_sec:

  Numeric. Timeout per request (seconds). Default `3600`. Passed to
  `wf_request()` or `wf_request_batch()` as `time_out`.

- force_download:

  Logical. If `FALSE` (default), existing files are skipped.

- parallel:

  Logical. If `TRUE`, use `wf_request_batch()` to submit requests in
  parallel. Falls back to sequential mode if batch is unavailable.

- workers:

  Integer. Number of parallel workers for batch submission. Default `2`.

- job_name:

  Optional string passed to `wf_request()` (sequential mode). Leave
  `NULL` outside RStudio; non-NULL values trigger RStudio Jobs in
  ecmwfr.

- verbose:

  Logical. Verbose logging. Default `TRUE`.

- ...:

  Additional arguments forwarded to
  [`ecmwfr::wf_request()`](https://rdrr.io/pkg/ecmwfr/man/wf_request.html).

## Value

(Invisibly) a `data.frame` with columns:

- `file` : full path to the output file

- `status` : one of `"ok"`, `"skip"`, `"fail"`

- `error` : last error message (if any)

- `model`, `system`, `variable`, `year`

## Details

To comply with CDS request-size limits, when multiple years are provided
the function automatically \*\*splits requests per year\*\* (internal
year chunking). This yields one output file per year. If your
`filename_tpl` does not contain `{year}`, a `_{year}` suffix is appended
before the extension to avoid overwriting.

File naming follows by default:

    ModelSystem_Variable_InitDate_Period_Leadtime.nc

e.g., `meteo_france9_TMAX_Apr01_1993_2016_24-5160.nc` When year-chunking
is active and `{year}` is not present in the template, files will be
named like `..._1993.nc`, `..._1994.nc`, etc.

## Notes

- Unknown models (not listed in internal `VALID_MODELS`) trigger a
  warning but do not stop execution.

- `retry` in `wf_request_batch()` is a polling interval (seconds), not a
  count.

- Include `{year}` in `filename_tpl` if you prefer to control where the
  year appears.

## See also

[`wf_request`](https://rdrr.io/pkg/ecmwfr/man/wf_request.html),
[`wf_request_batch`](https://rdrr.io/pkg/ecmwfr/man/wf_request.html)

## Examples

``` r
if (FALSE) { # \dontrun{
center_variables <- c("meteo_france_9.TMAX","ecmwf_51.TMAX")
res <- wass2s_download_cds(
  dataset_short_name = "seasonal-original-single-levels",
  base_query = list(format = "netcdf"),
  center_variables = center_variables,
  years = 1993:1995,
  months = 4, days = "01",
  times = "00:00",
  leadtime_hour = seq(24,240,24),
  out_dir = "out",
  parallel = TRUE, workers = 4,
  service = "cds", verbose = TRUE
)
} # }
```
