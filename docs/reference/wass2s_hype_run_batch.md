# Run multiple HYPE scenarios (one directory each)

Run multiple HYPE scenarios (one directory each)

## Usage

``` r
wass2s_hype_run_batch(run_dirs, mode = c("watchlog", "run"), ...)
```

## Arguments

- run_dirs:

  Character vector of scenario directories.

- mode:

  Execution mode: \`"watchlog"\` (default) or \`"run"\`.

- ...:

  Passed to \`wass2s_hype_run_watchlog()\` or \`wass2s_hype_run()\`.

## Value

A tibble with one row per scenario.
