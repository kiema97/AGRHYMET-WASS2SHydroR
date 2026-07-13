# Robust Downloader for CDS/ECMWF Data with Chunking and Optional NetCDF Merge

Downloads climate/meteorological data from CDS/ADS/CEMS using ecmwfr,
with support for multiple variables and models, year chunking, optional
batch-parallel submission, request pacing, resume-on-existing files,
dry-run inspection, status logging, and optional NetCDF consolidation.

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
  chunk_years = 1L,
  max_requests_per_batch = NULL,
  cooldown_sec = 0,
  request_delay_sec = 0,
  dry_run = FALSE,
  stop_on_error = FALSE,
  return_requests = FALSE,
  job_log = file.path(out_dir, "_wass2s_cds_jobs.csv"),
  combine = FALSE,
  combine_dir = out_dir,
  combine_filename_tpl = NULL,
  combine_dim = "auto",
  keep_chunks = TRUE,
  ...
)
```

## Arguments

- dataset_short_name:

  Character. The dataset short name on CDS/ADS/CEMS, e.g.,
  `"seasonal-original-single-levels"`.

- base_query:

  Named list. Static part of the query (e.g., `product_type`,
  `pressure_level`, `data_format`). Time-varying fields like `year`,
  `month`, `day`, `leadtime_hour` are set by this function.

- center_variables:

  Character vector of entries `"model_system.variable"`, e.g.,
  `"meteo_france_9.TMAX"`, `"ecmwf_51.TMAX"`. Only one variable per
  entry is supported.

- years:

  Integer/character vector. Years to request.

- months:

  Integer/character vector in `1..12`.

- days:

  Character/integer vector (e.g., `c("01","02")`).

- times:

  Character vector (e.g., `c("00:00","12:00")`).

- leadtime_hour:

  Character/integer vector (e.g., `c("24","48",...)`).

- area:

  Optional numeric length-4 `c(N, W, S, E)` bounding box.

- out_dir:

  Output directory. Created if missing.

- user:

  CDS/ADS/CEMS user ID used with
  [`ecmwfr::wf_set_key()`](https://rdrr.io/pkg/ecmwfr/man/wf_set_key.html).

- service:

  One of `"cds"`, `"ads"`, `"cems"`. Default `"cds"`.

- filename_tpl:

  Template for chunk filenames. Placeholders available: `{modelsys}`,
  `{model}`, `{system}`, `{var}`, `{init}`, `{period}`, `{lead}`,
  `{dataset}`, `{year}`, `{year_start}`, `{year_end}`, `{chunk}`.

- tries:

  Integer. Retry attempts per request in sequential mode.

- sleep_sec:

  Numeric. Seconds between retry attempts in sequential mode.

- timeout_sec:

  Numeric. Timeout per request in seconds.

- force_download:

  Logical. If `FALSE`, existing chunk files are skipped.

- parallel:

  Logical. If `TRUE`, use `wf_request_batch()` when available.

- workers:

  Integer. Number of workers for `wf_request_batch()`.

- job_name:

  Optional string passed to `wf_request()` in sequential mode.

- verbose:

  Logical. Verbose logging.

- chunk_years:

  Integer. Number of years per CDS request. Default `1`.

- max_requests_per_batch:

  Integer or `NULL`. Maximum number of requests submitted in one
  `wf_request_batch()` call. `NULL` uses `workers`.

- cooldown_sec:

  Numeric. Seconds to wait between batch groups.

- request_delay_sec:

  Numeric. Seconds to wait between sequential requests.

- dry_run:

  Logical. If `TRUE`, build and return the request plan without
  submitting anything to CDS.

- stop_on_error:

  Logical. If `FALSE` (default), failed downloads are recorded with
  `status = "fail"` and the remaining requests continue. If `TRUE`, the
  function stops at the first failed request or batch group.

- return_requests:

  Logical. If `TRUE`, include a list-column containing the generated CDS
  requests in the returned data frame.

- job_log:

  Optional CSV path used to record planned/submitted/finished jobs. Set
  to `NULL` to disable logging.

- combine:

  Logical. If `TRUE`, combine successful chunk NetCDF files into one
  file per model/system/variable after download.

- combine_dir:

  Directory for combined files. Defaults to `out_dir`.

- combine_filename_tpl:

  Template for combined filenames. Defaults to
  `"{modelsys}_{var}_{init}_{period}_{lead}.nc"`. Use
  `"{modelsys}_{var}_{period}.nc"` for names like
  `ecmwf_51_PRCP_1993_2026.nc`.

- combine_dim:

  Character. NetCDF dimension used for concatenation. Default `"auto"`,
  which tries `forecast_reference_time`, `time`, `valid_time`, then the
  unlimited dimension.

- keep_chunks:

  Logical. If `FALSE`, delete chunk files after a successful combine.

- ...:

  Additional arguments forwarded to
  [`ecmwfr::wf_request()`](https://rdrr.io/pkg/ecmwfr/man/wf_request.html)
  in sequential mode.

## Value

Invisibly, a `data.frame` with one row per chunk request and columns
describing the chunk file, status, metadata, and optional combined
output.

## Details

CDS requests over long periods can be rejected or remain queued for a
long time. By default, the function splits multi-year requests into
one-year chunks. Increase `chunk_years` cautiously (for example 2 or 3)
after testing the dataset, variables, area, and lead times.

File naming follows by default:

    model_system_VARIABLE_InitDate_Chunk_Leadtime.nc

for downloaded chunks. When `combine = TRUE`, successfully downloaded
chunks are merged into one file per model/system/variable using
`combine_filename_tpl`.

## Operational notes

- Use small `chunk_years` values for large domains, many lead times, or
  multi-variable downloads.

- Use `max_requests_per_batch`, `cooldown_sec`, and `request_delay_sec`
  to avoid submitting too many jobs to CDS at once.

- With `parallel = TRUE`, CDS can report that requests have been
  submitted and are still being processed server-side. This is normal;
  ecmwfr downloads each file once the corresponding job is ready.

- `combine = TRUE` requires ncdf4 and currently combines along a single
  detected NetCDF dimension, usually `forecast_reference_time`.

## See also

[`wf_request`](https://rdrr.io/pkg/ecmwfr/man/wf_request.html),
[`wf_request_batch`](https://rdrr.io/pkg/ecmwfr/man/wf_request.html)

## Examples

``` r
if (FALSE) { # \dontrun{
center_variables <- c("meteo_france_9.TMAX", "ecmwf_51.TMAX")
res <- wass2s_download_cds(
  dataset_short_name = "seasonal-original-single-levels",
  base_query = list(data_format = "netcdf"),
  center_variables = center_variables,
  years = 1993:1995,
  months = 4, days = "01",
  times = "00:00",
  leadtime_hour = seq(24, 240, 24),
  out_dir = "out",
  chunk_years = 1,
  parallel = TRUE, workers = 4,
  max_requests_per_batch = 4,
  cooldown_sec = 30,
  combine = TRUE,
  combine_filename_tpl = "{modelsys}_{var}_{period}.nc",
  service = "cds", verbose = TRUE
)
} # }
```
