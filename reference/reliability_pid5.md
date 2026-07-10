# Estimate PID-5 scale reliability

Compute per-scale internal-consistency reliability — Cronbach's alpha
and McDonald's omega — for the Personality Inventory for DSM-5: full
version (PID-5, 220 items), short form (PID-5-SF, 100 items), or brief
form (PID-5-BF, 25 items). Reliability is estimated on the reverse-keyed
item responses, at the facet level for FULL/SF and the domain level for
BF (the same scales
[`score_pid5()`](https://jmgirard.github.io/hitop/reference/score_pid5.md)
outputs, before FULL/SF domain aggregation).

## Usage

``` r
reliability_pid5(
  data,
  items,
  version = c("FULL", "SF", "BF"),
  srange = c(0, 3),
  alpha = TRUE,
  omega = TRUE
)
```

## Arguments

- data:

  A data frame containing (at least) all the PID items (numerically
  scored and in order).

- items:

  A vector of column names (as strings) or numbers (as integers)
  corresponding to the PID items in order. Items must be supplied in
  instrument order; duplicated entries are an error.

- version:

  A string indicating the version of the PID to score: "FULL", "SF", or
  "BF". Will be automatically capitalized. (default = `"FULL"`)

- srange:

  An optional numeric vector specifying the minimum and maximum values
  of the items, used for reverse-coding. (default = `c(0, 3)`)

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
# Facet-level reliability for the full PID-5 (alpha only)
reliability_pid5(sim_pid5, items = 1:220, version = "FULL", omega = FALSE)
#> # A tibble: 25 × 3
#>    scale                  nItems   alpha
#>    <chr>                   <int>   <dbl>
#>  1 Anhedonia                   8 -0.211 
#>  2 Suspiciousness              7 -0.211 
#>  3 Risk Taking                14 -0.0128
#>  4 Impulsivity                 6  0.141 
#>  5 Eccentricity               13  0.0554
#>  6 Distractibility             9 -0.154 
#>  7 Restricted Affectivity      7 -0.144 
#>  8 Submissiveness              4 -0.194 
#>  9 Withdrawal                 10 -0.0290
#> 10 Callousness                14  0.0147
#> # ℹ 15 more rows
```
