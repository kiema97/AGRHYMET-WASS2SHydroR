# Internal helper to abort with a WASS2S-specific error class

Wraps \[rlang::abort()\] to standardize error messages across the WASS2S
Hydro Statistical method.

## Usage

``` r
wass2s_abort(msg, cls = "wass2s_stat_error")
```

## Arguments

- msg:

  Error message.

- cls:

  Error class to attach (default "wass2s_stat_error").
