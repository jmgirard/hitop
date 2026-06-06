# Personality Inventory for DSM-5 Item Data

Information about the items in different versions of the PID-5.

## Usage

``` r
pid_items
```

## Format

A [tibble](https://tibble.tidyverse.org/reference/tibble.html) with 220
rows and 12 columns:

- FULL, SF, BF:

  Item number on the full PID-5, PID-5 faceted short form, and PID-5
  brief form

- Reverse:

  Whether the item needs to be reverse scored

- INC,INCS:

  Item number on the response inconsistency scale full and short forms

- ORS,ORSS:

  Item number on the overreporting scale full and short forms

- PRD,PRDS:

  Item number on the positive impression management response distortion
  scale full and short forms

- SDTD,SDTDS:

  Item number on the social desirability-total denial scale full and
  short forms

- Facet:

  Name of the facet

- Domain:

  Name of the domain

- Text:

  Item text, copyright APA

## Examples

``` r
pid_items
#> # A tibble: 220 × 15
#>     FULL    SF    BF Reverse   INC  INCS   ORS  ORSS   PRD  PRDS  SDTD SDTDS
#>    <dbl> <dbl> <dbl> <lgl>   <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#>  1     1    NA    NA FALSE      NA    NA    NA    NA    NA    NA    NA    NA
#>  2     2     1    NA FALSE      NA    NA     1     1     1     1     1     1
#>  3     3    NA     1 FALSE      NA    NA    NA    NA    NA    NA    NA    NA
#>  4     4     2     2 FALSE      NA    NA    NA    NA    NA    NA     2     2
#>  5     5    NA    NA FALSE      NA    NA    NA    NA    NA    NA    NA    NA
#>  6     6    NA    NA FALSE      NA    NA    NA    NA    NA    NA    NA    NA
#>  7     7    NA    NA TRUE       NA    NA    NA    NA    NA    NA    NA    NA
#>  8     8    NA    NA FALSE      NA    NA     2    NA    NA    NA    NA    NA
#>  9     9     3    NA FALSE      NA    NA    NA    NA    NA    NA    NA    NA
#> 10    10    NA    NA FALSE      NA    NA    NA    NA    NA    NA    NA    NA
#> # ℹ 210 more rows
#> # ℹ 3 more variables: Facet <chr>, Domain <chr>, Text <chr>
```
