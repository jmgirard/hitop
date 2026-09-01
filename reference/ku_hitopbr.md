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
#>    <chr>       <fct>   <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>
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
#> # ℹ 37 more variables: hbr_09 <dbl>, hbr_10 <dbl>, hbr_11 <dbl>, hbr_12 <dbl>,
#> #   hbr_13 <dbl>, hbr_14 <dbl>, hbr_15 <dbl>, hbr_16 <dbl>, hbr_17 <dbl>,
#> #   hbr_18 <dbl>, hbr_19 <dbl>, hbr_20 <dbl>, hbr_21 <dbl>, hbr_22 <dbl>,
#> #   hbr_23 <dbl>, hbr_24 <dbl>, hbr_25 <dbl>, hbr_26 <dbl>, hbr_27 <dbl>,
#> #   hbr_28 <dbl>, hbr_29 <dbl>, hbr_30 <dbl>, hbr_31 <dbl>, hbr_32 <dbl>,
#> #   hbr_33 <dbl>, hbr_34 <dbl>, hbr_35 <dbl>, hbr_36 <dbl>, hbr_37 <dbl>, …
```
