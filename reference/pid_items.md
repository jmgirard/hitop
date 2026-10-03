# Personality Inventory for DSM-5 Item Data

Information about the items in different versions of the PID-5.

## Usage

``` r
pid_items
```

## Format

A [tibble](https://tibble.tidyverse.org/reference/tibble.html) with 220
rows and 16 columns:

- FULL, SF, BF:

  Item number on the full PID-5, PID-5 faceted short form, and PID-5
  brief form (integer)

- BFPM:

  Item number on the PID5BF+M, the 36-item modified brief form
  (integer). Its keying is in `pid_scales$BFPM` and
  [pid_bfpm_domains](https://jmgirard.github.io/hitop/reference/pid_bfpm_domains.md)

- Reverse:

  Whether the item needs to be reverse scored

- INC,INCS:

  Item number on the response inconsistency scale full and short forms
  (integer)

- ORS,ORSS:

  Item number on the overreporting scale full and short forms (integer)

- PRD,PRDS:

  Item number on the positive impression management response distortion
  scale full and short forms (integer)

- SDTD,SDTDS:

  Item number on the social desirability-total denial scale full and
  short forms (integer)

- Facet:

  Name of the PID-5 facet. The PID5BF+M regroups six of the Rigid
  Perfectionism items into its three anankastia facets, whose item
  membership is held only in `pid_scales$BFPM`

- Domain:

  Name of the domain

- Text:

  Item text, copyright APA

## Examples

``` r
pid_items
#> # A tibble: 220 × 16
#>     FULL    SF    BF  BFPM Reverse   INC  INCS   ORS  ORSS   PRD  PRDS  SDTD
#>    <int> <int> <int> <int> <lgl>   <int> <int> <int> <int> <int> <int> <int>
#>  1     1    NA    NA    NA FALSE      NA    NA    NA    NA    NA    NA    NA
#>  2     2     1    NA    NA FALSE      NA    NA     1     1     1     1     1
#>  3     3    NA     1    NA FALSE      NA    NA    NA    NA    NA    NA    NA
#>  4     4     2     2     9 FALSE      NA    NA    NA    NA    NA    NA     2
#>  5     5    NA    NA    NA FALSE      NA    NA    NA    NA    NA    NA    NA
#>  6     6    NA    NA    15 FALSE      NA    NA    NA    NA    NA    NA    NA
#>  7     7    NA    NA    NA TRUE       NA    NA    NA    NA    NA    NA    NA
#>  8     8    NA    NA    NA FALSE      NA    NA     2    NA    NA    NA    NA
#>  9     9     3    NA    NA FALSE      NA    NA    NA    NA    NA    NA    NA
#> 10    10    NA    NA    NA FALSE      NA    NA    NA    NA    NA    NA    NA
#> # ℹ 210 more rows
#> # ℹ 4 more variables: SDTDS <int>, Facet <chr>, Domain <chr>, Text <chr>
```
