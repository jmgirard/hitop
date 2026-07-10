# Estimate HiTOP-SR scale reliability

Compute per-scale internal-consistency reliability — Cronbach's alpha
and McDonald's omega — for the HiTOP Self-Report (405 items).
Reliability is estimated on the reverse-keyed item responses for each of
the scales that
[`score_hitopsr()`](https://jmgirard.github.io/hitop/reference/score_hitopsr.md)
outputs.

## Usage

``` r
reliability_hitopsr(data, items, srange = c(1, 4), alpha = TRUE, omega = TRUE)
```

## Arguments

- data:

  A data frame containing all HiTOP-SR items (numerically coded).

- items:

  A vector of column names (as strings) or numbers (as integers)
  corresponding to the 405 HiTOP-SR items in order. Items must be
  supplied in instrument order; duplicated entries are an error.

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
```
