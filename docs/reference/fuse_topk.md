# Weighted fusion of predictions by year

Weighted fusion of predictions by year

## Usage

``` r
fuse_topk(preds_long)
```

## Arguments

- preds_long:

  Tibble with columns YYYY, pred, w (and optionally product/model).

## Value

Tibble (YYYY, pred_fused).
