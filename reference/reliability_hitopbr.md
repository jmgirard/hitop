# Estimate HiTOP-BR scale reliability

Compute per-scale internal-consistency reliability — Cronbach's alpha
and McDonald's omega — for the HiTOP Brief-Report (45 items).
Reliability is estimated on the item responses for each of the scales
that
[`score_hitopbr()`](https://jmgirard.github.io/hitop/reference/score_hitopbr.md)
outputs (the HiTOP-BR has no reverse-keyed items).

## Usage

``` r
reliability_hitopbr(data, items, srange = c(1, 4), alpha = TRUE, omega = TRUE)
```

## Arguments

- data:

  A data frame containing all HiTOP-BR items (numerically coded).

- items:

  A vector of column names (as strings) or numbers (as integers)
  corresponding to the 45 HiTOP-BR items in order. Items must be
  supplied in instrument order; duplicated entries are an error.

- srange:

  An optional numeric vector specifying the minimum and maximum values
  of the HiTOP-BR items, used for reverse-coding. (default = `c(1, 4)`)

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
The overlapping `externalizing` and `pFactor` scales are included
alongside the six base spectra.

## Examples

``` r
# Per-scale alpha for the HiTOP-BR
reliability_hitopbr(sim_hitopbr, items = 1:45, omega = FALSE)
#> # A tibble: 8 × 3
#>   scale            nItems    alpha
#>   <chr>             <int>    <dbl>
#> 1 Antagonism            9 -0.160  
#> 2 Detachment            6 -0.0706 
#> 3 Disinhibition         9  0.145  
#> 4 Internalizing         7  0.0344 
#> 5 Somatoform            8  0.0527 
#> 6 Thought Disorder      6  0.00754
#> 7 Externalizing        10  0.145  
#> 8 P Factor             12 -0.111  
```
