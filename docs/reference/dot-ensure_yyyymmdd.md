# Ensure YYYY column is in YYYYMMDD integer format

\- If values look like YYYY (4 digits), converts to YYYY0101 - If values
look like YYYYMMDD (8 digits), keeps as-is - If Date/POSIXct, converts
with format

## Usage

``` r
.ensure_yyyymmdd(x)
```
