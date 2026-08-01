# Estimate HiTOP-SR scale reliability

Compute per-scale internal-consistency reliability — Cronbach's alpha
and McDonald's omega — for the HiTOP Self-Report (405 items).
Reliability is estimated on the reverse-keyed item responses for each of
the scales that
[`score_hitopsr()`](https://jmgirard.github.io/hitop/reference/score_hitopsr.md)
outputs.

## Usage

``` r
reliability_hitopsr(
  data,
  items,
  srange = c(1, 4),
  alpha = TRUE,
  omega = TRUE,
  subset = NULL
)
```

## Arguments

- data:

  A data frame containing the HiTOP-SR items (numerically coded): all
  405 of them, or, when `subset` is supplied, that short form's items.

- items:

  A vector of column names (as strings) or numbers (as integers)
  corresponding to the HiTOP-SR items held in `data` — all 405, or, when
  `subset` is supplied, that short form's items. Items must be supplied
  in instrument order; duplicated entries are an error.

- srange:

  An optional numeric vector specifying the minimum and maximum values
  of the HiTOP-SR items, used for reverse-coding. (default = `c(1, 4)`)

- alpha:

  Optional logical; if `TRUE`, include a column of Cronbach's alpha per
  scale. (default = `TRUE`)

- omega:

  Optional logical; if `TRUE`, include a column of McDonald's omega
  (total) per scale, estimated via a one-factor CFA (requires the lavaan
  package). (default = `TRUE`)

- subset:

  An optional `hitop_subset` object, as returned by
  [`hitop_subset()`](https://jmgirard.github.io/hitop/reference/hitop_subset.md),
  describing a short form of the instrument. When supplied, `data` and
  `items` hold only that subset's item columns — in ascending instrument
  order, as the `generate_*_hitopsr()` forms lay them out — and one row
  is returned per subset scale. When `NULL`, all 405 items are expected
  and all 76 scales are estimated. (default = `NULL`)

## Value

A [tibble](https://tibble.tidyverse.org/reference/tibble.html) with one
row per scale and columns `scale`, `nItems`, and (when requested)
`alpha` and `omega`.

## Details

Alpha is computed by
[`calc_alpha()`](https://jmgirard.github.io/hitop/reference/calc_alpha.md)
(covariance-based, pairwise deletion) and omega by
[`calc_omega()`](https://jmgirard.github.io/hitop/reference/calc_omega.md)
(one-factor lavaan CFA, FIML). A scale whose estimate cannot be computed
(e.g. too few items or, for omega, a non-converging CFA or an
uninstalled lavaan) is returned as `NA` rather than aborting the call.

## Examples

``` r
# Per-scale alpha for the HiTOP-SR
reliability_hitopsr(sim_hitopsr, items = 1:405, omega = FALSE)
#> # A tibble: 76 × 3
#>    scale                nItems    alpha
#>    <chr>                 <int>    <dbl>
#>  1 Agoraphobia               5 -0.108  
#>  2 Antisocial Behavior       8 -0.136  
#>  3 Appetite Loss             3  0.00603
#>  4 Binge Eating              3  0.0509 
#>  5 Bodily Distress           6  0.0879 
#>  6 Body Dissatisfaction      4 -0.0891 
#>  7 Body Focus                5 -0.0282 
#>  8 Callousness               6 -0.347  
#>  9 Checking                  5 -0.247  
#> 10 Cleaning                  6  0.174  
#> # ℹ 66 more rows

# Per-scale alpha for data collected with a two-scale short form. Select the
# item columns by name: `s$items` holds original HiTOP-SR numbers, which are
# column positions only in a data frame that is exactly the 405 items in order.
s <- hitop_subset("hitopsr", scales = c("Agoraphobia", "Appetite Loss"))
short <- sim_hitopsr[paste0("hsr_", s$items)]
reliability_hitopsr(short, items = names(short), subset = s, omega = FALSE)
#> # A tibble: 2 × 3
#>   scale         nItems    alpha
#>   <chr>          <int>    <dbl>
#> 1 Agoraphobia        5 -0.108  
#> 2 Appetite Loss      3  0.00603
```
