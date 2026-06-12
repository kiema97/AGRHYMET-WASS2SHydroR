# Run HYPE in "watch-log" mode (Windows GUI executables)

Some HYPE builds may open a final Yes/No dialog and keep the process
alive. This function starts HYPE as a background process, waits for a
log file matching \`log_regex\` to appear and become stable (file size
unchanged for \`stable_duration\`), then attempts to close the window
(Windows) and falls back to kill if needed.

## Usage

``` r
wass2s_hype_run_watchlog(
  run_dir,
  exe_path = NULL,
  args = character(),
  env = character(),
  timeout_sec = 150,
  expected_files = character(),
  clean_regex = c("hyss_.*\\.log$"),
  log_prefix = NULL,
  log_regex = "hyss_.*\\.log$",
  check_interval = 10,
  stable_duration = 30
)
```

## Arguments

- run_dir:

  Directory containing HYPE inputs.

- exe_path:

  Optional explicit path to the HYPE executable. If NULL, searches for
  exactly one \`.exe\` inside \`run_dir\`.

- args:

  Character vector of command-line arguments passed to HYPE (if
  supported).

- env:

  Named character vector of environment variables to set during the run.

- timeout_sec:

  Maximum time to wait in seconds (0 = no timeout).

- expected_files:

  Expected output files (relative to \`run_dir\`) to validate.

- clean_regex:

  REGEX patterns used to delete files under \`run_dir\` before running.

- log_prefix:

  Prefix for stdout/stderr logs (defaults to timestamp tag).

- log_regex:

  REGEX to detect the "completion" log file in \`run_dir\` (default:
  \`"^hyss\_.\*\\.log\$"\`).

- check_interval:

  Seconds between log checks.

- stable_duration:

  Seconds of unchanged log size to consider the run finished.

## Value

Same structure as \`wass2s_hype_run()\`, plus \`watched_log\` (character
or NA).
