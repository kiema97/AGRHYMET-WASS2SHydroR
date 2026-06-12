# Run HYPE in a given directory (non-GUI mode)

This function runs the HYPE executable from \`run_dir\` and captures
stdout/stderr into log files. Optionally, it deletes old outputs before
running and validates that expected outputs exist after the run.

## Usage

``` r
wass2s_hype_run(
  run_dir,
  exe_path = NULL,
  args = character(),
  env = character(),
  timeout_sec = 300,
  expected_files = character(),
  clean_regex = character(),
  log_prefix = NULL
)
```

## Arguments

- run_dir:

  Directory containing HYPE inputs (info.txt, GeoData.txt, etc.).

- exe_path:

  Optional explicit path to the HYPE executable. If NULL, the function
  searches for exactly one \`.exe\` directly inside \`run_dir\`.

- args:

  Character vector of command-line arguments passed to HYPE (if
  supported).

- env:

  Named character vector of environment variables to set during the run.

- timeout_sec:

  Maximum time to wait in seconds (0 = no timeout).

- expected_files:

  Character vector of output file paths (relative to \`run_dir\`) that
  must exist after a successful run.

- clean_regex:

  Character vector of REGEX patterns used to delete files under
  \`run_dir\` before running (e.g., \`c("^hyss\_.\*\\.log\$",
  "results/.\*\\.txt\$")\`).

- log_prefix:

  Prefix for stdout/stderr log files. Defaults to a timestamp tag.

## Value

A list with:

- success (logical)

- exit_status (integer)

- duration_sec (double)

- run_dir (character)

- stdout_log (character)

- stderr_log (character)

- outputs (tibble)
