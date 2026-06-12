# Build a modeling recipe with cleaning, correlation filtering, normalization, and optional PCA

Construct a robust recipes pipeline for a \*\*numeric outcome\*\*
(annual mean discharge). The recipe assigns roles, removes
zero/near-zero variance predictors, imputes missing values (median for
numeric, optional mode for nominal), optionally removes linear
combinations, filters highly correlated predictors, normalizes numeric
predictors, and can apply PCA either explicitly or \*\*automatically
when the number of predictors exceeds a threshold\*\*.

## Usage

``` r
make_recipe(
  df,
  predictors,
  target = "Q",
  corr_threshold = 0.95,
  corr_method = c("pearson", "spearman", "kendall"),
  impute_nominal = TRUE,
  include_dummy = FALSE,
  y_transform = c("none", "log1p", "yeo"),
  pca_num_comp = NULL,
  pca_var_threshold = NULL,
  remove_linear_comb = FALSE,
  auto_pca = TRUE,
  auto_pca_when_gt = 15,
  auto_pca_var_threshold = 0.8,
  apply_impute = TRUE,
  apply_corr = TRUE,
  apply_normalize = TRUE,
  verbose = FALSE
)
```

## Arguments

- df:

  A data frame or tibble containing `target` and `predictors`.

- predictors:

  Character vector of predictor column names. Nonexistent names are
  dropped silently; an error is thrown if none remain.

- target:

  Character scalar; outcome column name (annual mean discharge). Must be
  numeric. Default: `"Q"`.

- corr_threshold:

  Numeric in (0, 1); absolute correlation threshold used by
  [`recipes::step_corr()`](https://recipes.tidymodels.org/reference/step_corr.html).
  Default: `0.90`.

- corr_method:

  Correlation method for `step_corr()`, typically `"pearson"` (default)
  or `"spearman"`.

- impute_nominal:

  Logical; if `TRUE`, apply `step_impute_mode()` to nominal predictors.
  Default: `TRUE`.

- include_dummy:

  Logical; if `TRUE`, expand nominal predictors via
  `step_dummy(one_hot = TRUE, keep_original_cols = FALSE)` before
  correlation filtering. Default: `FALSE`.

- y_transform:

  Character; one of `"none"` (default), `"log1p"` (applies `log(Q + 1)`)
  or `"yeo"` (Yeo–Johnson) for the outcome. The outcome is left
  untransformed by default.

- pca_num_comp:

  Integer or `NULL`; if provided, apply PCA with a fixed number of
  components (disables auto-PCA).

- pca_var_threshold:

  Numeric or `NULL`; if provided (e.g. `0.95`), apply PCA keeping enough
  components to reach the cumulative explained variance threshold
  (disables auto-PCA). Do not set together with `pca_num_comp`.

- remove_linear_comb:

  Logical; if `TRUE` (default), remove linear combinations using
  `step_lincomb()`.

- auto_pca:

  Logical; if `TRUE`, enable automatical PCA. Default: `TRUE`.

- auto_pca_when_gt:

  Integer; enable auto-PCA when the number of predictors is greater than
  this threshold. Default: `15`.

- auto_pca_var_threshold:

  Numeric in (0, 1); cumulative variance target used when auto-PCA is
  triggered. Default: `0.95`.

- apply_impute:

  Logical; controls whether missing value imputation is applied to
  predictor variables. When \`TRUE\` (default), numeric predictors are
  imputed using median imputation via
  [`recipes::step_impute_median()`](https://recipes.tidymodels.org/reference/step_impute_median.html),
  and nominal predictors (if `impute_nominal = TRUE`) are imputed using
  [`recipes::step_impute_mode()`](https://recipes.tidymodels.org/reference/step_impute_mode.html).

  This argument should typically be set to \`FALSE\` when predictors
  have already been preprocessed upstream (e.g., EOF/PCA transformation
  with prior imputation), in order to avoid redundant transformations
  and preserve reproducibility of the preprocessing pipeline.

- apply_corr:

  Logical; indicates whether a correlation-based filtering step is
  applied to numeric predictors. When \`TRUE\` (default), highly
  correlated predictors are removed using
  [`recipes::step_corr()`](https://recipes.tidymodels.org/reference/step_corr.html)
  with the specified `corr_threshold` and `corr_method`.

  Setting this argument to \`FALSE\` is recommended when predictors have
  already undergone dimensionality reduction (e.g., EOF or PCA
  preprocessing), as the correlation structure has typically been
  addressed upstream.

- apply_normalize:

  Logical; controls whether numeric predictors are standardized using
  [`recipes::step_normalize()`](https://recipes.tidymodels.org/reference/step_normalize.html).
  When \`TRUE\` (default), predictors are centered and scaled prior to
  modeling.

  This argument can be set to \`FALSE\` when predictors have already
  been normalized during a prior preprocessing stage (e.g., EOF/PCA
  computation), ensuring that the same scaling is not applied multiple
  times and maintaining consistency across modeling workflows.

- verbose:

  Logical; if `TRUE`, emit informational messages. Default: `FALSE`.

## Value

An unprepped
[`recipes::recipe`](https://recipes.tidymodels.org/reference/recipe.html)
object suitable for use inside workflows/tune.

## Details

Typical step order: *imputation* \\\rightarrow\\ (optional) *dummy
encoding* \\\rightarrow\\ *correlation filtering* \\\rightarrow\\
*normalization* \\\rightarrow\\ (optional) *PCA*. Correlation is
computed after imputation. Normalization does not affect pairwise
correlation but is required before PCA.

## Behavior

If neither `pca_num_comp` nor `pca_var_threshold` is set, the function
will automatically enable PCA when
`length(predictors) > auto_pca_when_gt`, keeping enough components to
reach `auto_pca_var_threshold` cumulative variance. If either
`pca_num_comp` or `pca_var_threshold` is provided, this explicit setting
takes precedence and auto-PCA is disabled.

## See also

[`recipe`](https://recipes.tidymodels.org/reference/recipe.html),
[`step_corr`](https://recipes.tidymodels.org/reference/step_corr.html),
[`step_pca`](https://recipes.tidymodels.org/reference/step_pca.html),
[`workflow`](https://workflows.tidymodels.org/reference/workflow.html)

## Examples

``` r
if (FALSE) { # \dontrun{
# Minimal example
rec <- make_recipe(
  df = data,
  predictors = c("x1","x2","x3","region"),
  target = "Q",
  corr_threshold = 0.9,
  include_dummy = TRUE
)
rec_prep <- recipes::prep(rec)

# Force PCA to 10 components (disables auto-PCA)
rec_pca_fixed <- make_recipe(
  df = data,
  predictors = setdiff(names(data), "Q"),
  pca_num_comp = 10
)

# Keep 95% cumulative variance via PCA (disables auto-PCA)
rec_pca_var <- make_recipe(
  df = data,
  predictors = setdiff(names(data), "Q"),
  pca_var_threshold = 0.95
)
} # }
```
