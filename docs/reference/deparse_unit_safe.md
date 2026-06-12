# Safely extract a unit string from a vector

Internal helper that extracts unit information from vectors, including
objects of class `units` or vectors carrying a `"units"` attribute.

## Usage

``` r
deparse_unit_safe(v)
```

## Arguments

- v:

  A vector that may carry units metadata.

## Value

A character string describing the unit, or `NA_character_` if no unit
information is available.
