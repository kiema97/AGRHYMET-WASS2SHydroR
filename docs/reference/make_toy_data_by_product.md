# Create synthetic multi-product hydrological dataset

Create synthetic multi-product hydrological dataset

## Usage

``` r
make_toy_data_by_product(
  basins = c(1040021500, 215675, 330111),
  years = 1990:2023,
  products = c("SST_CMCC", "SST_ECMWF", "SST_JMA"),
  p = 6,
  signal_strength = 0.7,
  miss_rate = 0.05,
  seed = 123,
  date_format = c("YYYYMMDD", "YYYY"),
  mmdd = "1231"
)
```

## Arguments

- basins:

  Integer vector of basin IDs (HYBAS_ID).

- years:

  Integer vector of years.

- products:

  Character vector of product names.

- p:

  Integer, number of predictors per product (named pt_1..pt_p).

- signal_strength:

  Numeric in \[0,1\], controls how predictive products are.

- miss_rate:

  Fraction of NA injected in predictors (0..1).

- seed:

  Integer seed for reproducibility.

- date_format:

  Character, either "YYYY" or "YYYYMMDD".

- mmdd:

  Character of length 4 (MMDD), used when date_format = "YYYYMMDD".

## Value

Named list of data.frames (one per product) with columns: HYBAS_ID,
YYYY, Q, pt_1..pt_p
