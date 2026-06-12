# Run one model workflow: prepare OBS -\> update info.txt resultdir -\> run HYPE

Typical usage: in a forcing directory containing
\`\<model\>\_PRCP.txt\`, \`\<model\>\_TMAX.txt\`, ... you call this
function with \`resultdir = model\` and \`prefix = paste0(model,
"\_")\`.

## Usage

``` r
wass2s_hype_run_one_model(
  run_dir,
  forcing_dir,
  resultdir,
  exe_path = NULL,
  timeout_sec = 300,
  args = character(),
  env = character(),
  expected_files = character(),
  clean_regex = c("hyss_.*\\.log$"),
  log_prefix = NULL,
  log_regex = "hyss_.*\\.log$",
  check_interval = 10,
  stable_duration = 30,
  obsid = NULL,
  prefix = NULL,
  patterns = c("PRCP", "TMAX", "TMIN", "TMEAN"),
  extension = ".txt",
  date_from = NULL,
  date_to = NULL,
  write = TRUE,
  round_digits = 2,
  ...
)
```

## Arguments

- run_dir:

  HYPE run directory (contains info.txt and static inputs).

- forcing_dir:

  Directory containing forcing files for the selected model.

- resultdir:

  Result directory name for this run (often the model ID).

- exe_path:

  Optional explicit path to HYPE executable. If NULL, searches for a
  single \`.exe\` in \`run_dir\`.

- timeout_sec:

  Timeout for the HYPE run.

- args:

  Command-line arguments passed to the executable (if supported).

- env:

  Named character vector of environment variables during the run.

- expected_files:

  Output files expected (relative to \`run_dir\`). If empty, defaults to
  \`\<resultdir\>/timeCOUT.txt\`.

- clean_regex:

  REGEX patterns to delete before run (applied under \`run_dir\`).

- log_prefix:

  Prefix for stdout/stderr logs.

- log_regex:

  REGEX for completion log detection (watchlog mode only).

- check_interval:

  Seconds between watch checks (watchlog mode only).

- stable_duration:

  Seconds of unchanged log size (watchlog mode only).

- obsid:

  Optional station IDs to pass to \`HYPEtools::WriteObs\`. If NULL,
  derived from column names.

- prefix:

  Optional prefix prepended to each pattern (e.g., \`"ecmwf\_"\`).

- patterns:

  Character vector of length 4. Default:
  c("PRCP","TMAX","TMIN","TMEAN").

- extension:

  File extension (default ".txt").

- date_from:

  Optional start date (inclusive) for filtering (Date or coercible).

- date_to:

  Optional end date (inclusive) for filtering (Date or coercible).

- write:

  Logical; if TRUE, writes obs files into \`run_dir\`.

- round_digits:

  Digits for \`WriteObs\`.

- ...:

  Extra args forwarded to \`HYPEtools::WriteObs\`.

## Value

A one-row tibble with run metadata.
