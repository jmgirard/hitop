# Rank and list the top-\\n\\ scales per row

For each row of selected scale columns, return a comma-separated string
of the `top` highest (or lowest) scale names, optionally removing a
common `prefix` from the column names and optionally appending the
result to the original data as a tibble.

## Usage

``` r
rank_scales(
  data,
  scales,
  prefix = NULL,
  top = 5,
  dir = "high",
  reverse = NULL,
  srange = NULL,
  name = "top_scales",
  append = TRUE
)
```

## Arguments

- data:

  A data frame containing the scale columns.

- scales:

  A character or integer vector identifying the columns in `data` to
  rank (names or positions). These columns should be numeric.

- prefix:

  A length-1 string giving a leading string to strip from each selected
  column name before concatenation, or `NULL` to keep names as is. The
  match is literal, not a regular expression: a column name that does
  not begin with exactly `prefix` is left unchanged, and characters that
  are regex metacharacters (`.`, `(`, `+`) stand for themselves.

- top:

  Integer (length 1). The number of columns to include per row after
  ranking. Must be between 1 and `length(scales)`.

- dir:

  Direction of ranking: `"high"` for largest values first or `"low"` for
  smallest values first.

- reverse:

  A subset of `scales` (names or positions) identifying scales that are
  keyed in the opposite direction to the rest (higher = healthier, e.g.
  a well-being scale). Before ranking, each such column is reflected via
  `sum(srange) - value` so that all selected scales share a common
  "higher = more elevated" metric and `dir` applies uniformly. Set to
  `NULL` (the default) when every scale is keyed the same way.

- srange:

  A numeric vector of length 2 giving the minimum and maximum of the
  scale scores, used to reflect the `reverse` columns. Required when
  `reverse` is not `NULL`; ignored otherwise. (default = `NULL`)

- name:

  A length-1 string giving the name of the output column holding the
  ranked-scale strings. (default = `"top_scales"`)

- append:

  Logical. If `TRUE`, bind the result as a new column to `data`. If
  `FALSE`, return only the result column.

## Value

A [tibble](https://tibble.tidyverse.org/reference/tibble.html). If
`append = TRUE`, it is `data` with an added character column named by
`name`. If `append = FALSE`, it is a one-column tibble (named by `name`)
whose values are the comma-separated ranked scale names, one per row of
`data`.

## Details

Ranking is performed row-wise using
[`order()`](https://rdrr.io/r/base/order.html) on the selected columns.
Ties are resolved by the original column order (the default behavior of
`order`), which is alphabetical if using the package's scoring
functions. Missing values are placed last by
[`order()`](https://rdrr.io/r/base/order.html) and will be included only
if there are fewer than `top` non-missing values in a row.

If `prefix` is not `NULL`, the function removes that exact leading
string from each selected column name before concatenation. Names that
do not begin with it are carried through whole.

The output column is named by `name`. When `append = TRUE` it is added
after the existing columns of `data` (whose order is preserved); when
`append = FALSE` the result is a one-column tibble.

**Errors.** A `scales` argument that names no columns is an error,
reported ahead of the other selection arguments, so the cause named is
the empty selection and not a consequence of it. The condition is
classed `hitop_empty_selection`, so a caller can catch this refusal by
name.

With `append = TRUE`, a column of `data` whose name this call would also
produce is an error rather than an overwrite or a duplicated column: the
message names every colliding column. Re-run with `append = FALSE` to
return only the new columns, or drop the colliding columns from `data`
first. The condition is classed `hitop_append_collision`, so a caller
can catch this refusal by name.

## Examples

``` r
# List each respondent's 3 highest-scoring HiTOP-BR scales
scored <- score_hitopbr(sim_hitopbr, items = 1:45, append = FALSE)
rank_scales(scored, scales = names(scored), prefix = "hbr_", top = 3,
            append = FALSE)
#> # A tibble: 100 × 1
#>    top_scales                                 
#>    <chr>                                      
#>  1 detachment,somatoform,antagonism           
#>  2 thoughtDisorder,disinhibition,internalizing
#>  3 detachment,somatoform,antagonism           
#>  4 antagonism,somatoform,detachment           
#>  5 externalizing,thoughtDisorder,antagonism   
#>  6 disinhibition,thoughtDisorder,internalizing
#>  7 disinhibition,thoughtDisorder,externalizing
#>  8 antagonism,pFactor,externalizing           
#>  9 disinhibition,detachment,internalizing     
#> 10 somatoform,antagonism,thoughtDisorder      
#> # ℹ 90 more rows
```
