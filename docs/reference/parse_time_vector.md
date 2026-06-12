# Parse time vector into POSIXct

Internal helper to convert various time representations (numeric with
units, character, Date, POSIXct) into POSIXct.

## Usage

``` r
parse_time_vector(v, tz = "UTC")
```

## Arguments

- v:

  Vector containing time information

- tz:

  Timezone

## Value

POSIXct vector
