# Simulated HiTOP-BR Data

Simulated responses to items on the HiTOP-BR (with 45 items). Note that
this is a naive simulation where response options 1 to 4 are all equally
likely and generated independently per item. Thus, responses are not
clustered within scales, and these data can be used (eventually) to test
validity tools intended to detect inconsistent/random responding.

## Usage

``` r
sim_hitopbr
```

## Format

A [tibble](https://tibble.tidyverse.org/reference/tibble.html) with 100
rows and 45 columns.

- hbr_01 to hbr_45:

  Responses on each item

## Examples

``` r
sim_hitopbr
#> # A tibble: 100 × 45
#>    hbr_01 hbr_02 hbr_03 hbr_04 hbr_05 hbr_06 hbr_07 hbr_08 hbr_09 hbr_10 hbr_11
#>     <int>  <int>  <int>  <int>  <int>  <int>  <int>  <int>  <int>  <int>  <int>
#>  1      3      3      3      2      2      3      2      1      2      3      4
#>  2      1      2      2      4      1      1      3      3      2      3      4
#>  3      3      4      3      2      1      1      2      1      3      4      1
#>  4      1      3      3      2      3      3      2      1      2      2      2
#>  5      4      4      4      3      1      2      3      2      1      1      4
#>  6      1      3      3      3      3      3      4      1      4      3      2
#>  7      2      2      4      3      4      3      1      2      4      1      4
#>  8      3      3      1      2      3      4      1      4      1      1      1
#>  9      2      4      1      1      2      2      2      3      1      3      1
#> 10      4      2      4      2      4      4      1      4      3      4      3
#> # ℹ 90 more rows
#> # ℹ 34 more variables: hbr_12 <int>, hbr_13 <int>, hbr_14 <int>, hbr_15 <int>,
#> #   hbr_16 <int>, hbr_17 <int>, hbr_18 <int>, hbr_19 <int>, hbr_20 <int>,
#> #   hbr_21 <int>, hbr_22 <int>, hbr_23 <int>, hbr_24 <int>, hbr_25 <int>,
#> #   hbr_26 <int>, hbr_27 <int>, hbr_28 <int>, hbr_29 <int>, hbr_30 <int>,
#> #   hbr_31 <int>, hbr_32 <int>, hbr_33 <int>, hbr_34 <int>, hbr_35 <int>,
#> #   hbr_36 <int>, hbr_37 <int>, hbr_38 <int>, hbr_39 <int>, hbr_40 <int>, …
```
