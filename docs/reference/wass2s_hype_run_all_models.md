# Run the full workflow for all models discovered in a forcing directory

This function discovers model IDs with \`wass2s_hype_get_model_ids()\`
and runs each model by: - writing obs files into \`run_dir\` from
\`\<model\>\_PRCP.txt\` etc. - updating \`info.txt\` resultdir to
\`./\<model\>/\` - executing HYPE (watchlog mode by default)

## Usage

``` r
wass2s_hype_run_all_models(
  run_dir,
  forcing_dir,
  exclude_models = character(),
  exe_path = NULL,
  timeout_sec = 300,
  ...
)
```

## Arguments

- run_dir:

  HYPE run directory.

- forcing_dir:

  Directory containing \`\<model\>\_(PRCP\|TMAX\|TMIN\|TMEAN).txt\`.

- exclude_models:

  Character vector of model IDs to skip.

- exe_path:

  Optional explicit path to HYPE executable.

- timeout_sec:

  Timeout for each run.

- ...:

  Extra args forwarded to \`wass2s_hype_run_one_model()\` and
  \`wass2s_hype_prepare_obs()\` (e.g., obsid, date_from/date_to,
  round_digits, log_regex, etc.).

## Value

Tibble with one row per model.
