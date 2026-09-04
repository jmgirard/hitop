# Real HiTOP-BR Data

Real responses to items on the HiTOP-BR from University of Kansas
students.

## Usage

``` r
ku_hitopbr
```

## Format

A [tibble](https://tibble.tidyverse.org/reference/tibble.html) with 411
rows and 47 columns.

- participant:

  An anonymized id for each participant

- biosex:

  A factor indicating each participant's biological sex

- hbr_01 to hbr_45:

  Responses on each item

## Examples

``` r
ku_hitopbr
#> # A tibble: 411 × 47
#>    participant biosex hbr_01 hbr_02 hbr_03 hbr_04 hbr_05 hbr_06 hbr_07 hbr_08
#>    <chr>       <fct>   <int>  <int>  <int>  <int>  <int>  <int>  <int>  <int>
#>  1 P001        male        1      1      1      1      2      1      1      1
#>  2 P002        male        1      1      1      1      2      2      2      1
#>  3 P003        male        1      2      1      2      3      4      3      3
#>  4 P004        male        1      1      1      1      2      1      1      1
#>  5 P005        male        1      4      1      1      3      1      1      1
#>  6 P006        female      1      1      1      1      1      1      1      1
#>  7 P007        female      1      1      1      1      1      1      1      1
#>  8 P008        male        2      1      1      1      3      1      3      2
#>  9 P009        female      1      1      1      1      3      1      1      1
#> 10 P010        female      1      1      1      1      2      1      1      1
#> # ℹ 401 more rows
#> # ℹ 37 more variables: hbr_09 <int>, hbr_10 <int>, hbr_11 <int>, hbr_12 <int>,
#> #   hbr_13 <int>, hbr_14 <int>, hbr_15 <int>, hbr_16 <int>, hbr_17 <int>,
#> #   hbr_18 <int>, hbr_19 <int>, hbr_20 <int>, hbr_21 <int>, hbr_22 <int>,
#> #   hbr_23 <int>, hbr_24 <int>, hbr_25 <int>, hbr_26 <int>, hbr_27 <int>,
#> #   hbr_28 <int>, hbr_29 <int>, hbr_30 <int>, hbr_31 <int>, hbr_32 <int>,
#> #   hbr_33 <int>, hbr_34 <int>, hbr_35 <int>, hbr_36 <int>, hbr_37 <int>, …
```
