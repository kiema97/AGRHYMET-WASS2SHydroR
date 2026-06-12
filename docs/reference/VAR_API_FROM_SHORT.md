# Short API Variable Name Mappings

Two named character vectors used to convert between short variable codes
(e.g., `"TMAX"`) and the corresponding API names expected by CDS/ECMWF
(e.g., `"maximum_2m_temperature_in_the_last_24_hours"`). These are used
by
[`wass2s_download_cds()`](https://kiema97.github.io/AGRHYMET-WASS2SHydroR/reference/wass2s_download_cds.md)
for filename generation and request normalization.

## Usage

``` r
VAR_API_FROM_SHORT
```

## Format

Named character vectors.

## Details

- `VAR_API_FROM_SHORT`: short code \\\rightarrow\\ API name.

- `VAR_SHORT_FROM_API`: API name \\\rightarrow\\ short code.

Extend or modify these mappings as needed for additional variables.
