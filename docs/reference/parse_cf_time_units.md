# Parse CF-style time unit strings

Internal helper that parses CF-style time unit strings such as
`"days since 1900-01-01"` or `"hours since 1970-01-01 00:00:00"` and
returns the corresponding origin and conversion factor.

## Usage

``` r
parse_cf_time_units(unit_str)
```

## Arguments

- unit_str:

  Character string describing time units.

## Value

A list with components:

- `mult`: numeric conversion factor to seconds;

- `origin`: `POSIXct` origin date-time.

Returns `NULL` if the input cannot be parsed.

## Details

Supported units include milliseconds, seconds, minutes, hours, and days.
