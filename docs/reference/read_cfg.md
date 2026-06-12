# Read YAML configuration

Read YAML configuration

## Usage

``` r
read_cfg(path)
```

## Arguments

- path:

  Path to a YAML file.

## Value

A named list with configuration values.

## Examples

``` r
if (FALSE) { # \dontrun{
cfg <- read_cfg("config/config.dev.yaml")
cfg$data$products
} # }
```
