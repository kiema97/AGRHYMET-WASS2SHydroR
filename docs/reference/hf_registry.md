# Registry of available forecasting methods

Simple plugin registry so the core can discover and run methods (e.g.
"stat", "hydro", "ml") without hard dependencies.

## Usage

``` r
hf_registry
```

## Format

An environment containing registered method constructors.
