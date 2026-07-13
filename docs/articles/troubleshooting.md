# Troubleshooting and Best Practices

## CDS Key Not Found

Check the configured key:

``` r
ecmwfr::wf_get_key(user = "ecmwfr")
```

If no key is found:

``` r
ecmwfr::wf_set_key(user = "ecmwfr")
```

Use the same `user` value in
[`wass2s_download_cds()`](https://kiema97.github.io/AGRHYMET-WASS2SHydroR/reference/wass2s_download_cds.md).

## CDS Download Fails Outside RStudio

Leave `job_name = NULL`. In `ecmwfr`, a non-NULL `job_name` triggers
RStudio Jobs. This is useful in RStudio but not in command-line R
sessions.

``` r
wass2s_download_cds(
  dataset_short_name = "seasonal-original-single-levels",
  base_query = list(data_format = "netcdf"),
  center_variables = "ecmwf_51.T2M",
  years = 2020,
  months = 1,
  days = "01",
  times = "00:00",
  leadtime_hour = 24,
  job_name = NULL
)
```

## Large CDS Downloads and Server-Side Queues

For long periods such as 1993–2026, avoid sending one very large request
to CDS. Use year chunks and request pacing:

``` r
res_cds <- wass2s_download_cds(
  dataset_short_name = "seasonal-original-single-levels",
  base_query = list(data_format = "netcdf"),
  center_variables = "ecmwf_51.PRCP",
  years = 1993:2026,
  months = 5,
  days = "01",
  times = "00:00",
  leadtime_hour = seq(24, 4416, 24),
  out_dir = "data/cds",
  chunk_years = 1,
  parallel = TRUE,
  workers = 4,
  max_requests_per_batch = 4,
  cooldown_sec = 30,
  job_log = "data/cds/_wass2s_cds_jobs.csv",
  combine = TRUE,
  combine_filename_tpl = "{modelsys}_{var}_{period}.nc"
)
```

Useful safeguards:

- run first with `dry_run = TRUE` and `return_requests = TRUE`;
- keep `chunk_years = 1` for very large domains or long lead-time
  vectors;
- use `max_requests_per_batch` and `cooldown_sec` to avoid submitting
  too many jobs in a short time window;
- keep `job_log` enabled to retain a local audit trail;
- keep the default `stop_on_error = FALSE` so one failed CDS request is
  recorded as `status = "fail"` without stopping the remaining
  downloads;
- keep `keep_chunks = TRUE` until the combined NetCDF file has been
  validated.

Messages saying that a CDS request has been submitted and is still being
processed are normal. Leave the R session open when possible; otherwise
use the `wf_transfer()` command printed by `ecmwfr` to retrieve
completed jobs later. \## NetCDF Dimensions Are Not Detected

Provide dimension names explicitly:

``` r
wass2s_prepare_data(
  x = "file.nc",
  dim_lon = "longitude",
  dim_lat = "latitude",
  dim_time = "time"
)
```

Curvilinear grids are not currently supported.

## No Predictors Are Selected

Check the predictor names:

``` r
grep("^pt_", names(data_by_product[[1]]), value = TRUE)
```

Then provide the appropriate regular expression:

``` r
pred_pattern_by_product = "^precip_|^temp_"
```

## Scores Are `NA`

This can happen when:

- there are too few observations;
- the target is constant;
- predictions are constant;
- the validation period is too short;
- too many values are missing.

During debugging, use:

``` r
max_na_frac = 0.5
require_variance = FALSE
quiet = FALSE
```

Then restore stricter controls once the data issue is understood.

## KGE Versus RMSE Selection

KGE is hydrologically meaningful because it combines correlation, bias,
and variability. However, direct KGE optimization can be unstable on
small samples or nearly constant series. For this reason:

- statistical workflows default to KGE selection;
- ML workflows default to RMSE selection while reporting the KGE of the
  selected configuration.

You can override the behavior with:

``` r
selection_metric = "kge"
selection_metric = "rmse"
```

## Recommended Operational Practice

- Start with one basin and one model.
- Use `quiet = FALSE` during setup.
- Validate CDS files with
  [`wass2s_prepare_data()`](https://kiema97.github.io/AGRHYMET-WASS2SHydroR/reference/wass2s_prepare_data.md)
  before modelling.
- Start with `fusion_method = "median"`.
- Use `weighted_mean` only when product skill scores are reliable.
- Use `meta` only when enough training observations are available.
- Record `topK`, `min_kge_model`, `selection_metric`, `fusion_method`,
  and `prediction_years` for each experiment.
