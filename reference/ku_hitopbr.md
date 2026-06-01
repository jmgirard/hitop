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

- hbr01 to hbr45:

  Responses on each item

## Examples

``` r
ku_hitopbr
#> # A tibble: 411 × 47
#>    participant biosex hbr01 hbr02 hbr03 hbr04 hbr05 hbr06 hbr07 hbr08 hbr09
#>    <chr>       <fct>  <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#>  1 P001        male       1     1     1     1     2     1     1     1     1
#>  2 P002        male       1     1     1     1     2     2     2     1     2
#>  3 P003        male       1     2     1     2     3     4     3     3     3
#>  4 P004        male       1     1     1     1     2     1     1     1     1
#>  5 P005        male       1     4     1     1     3     1     1     1     2
#>  6 P006        female     1     1     1     1     1     1     1     1     1
#>  7 P007        female     1     1     1     1     1     1     1     1     1
#>  8 P008        male       2     1     1     1     3     1     3     2     2
#>  9 P009        female     1     1     1     1     3     1     1     1     1
#> 10 P010        female     1     1     1     1     2     1     1     1     1
#> # ℹ 401 more rows
#> # ℹ 36 more variables: hbr10 <dbl>, hbr11 <dbl>, hbr12 <dbl>, hbr13 <dbl>,
#> #   hbr14 <dbl>, hbr15 <dbl>, hbr16 <dbl>, hbr17 <dbl>, hbr18 <dbl>,
#> #   hbr19 <dbl>, hbr20 <dbl>, hbr21 <dbl>, hbr22 <dbl>, hbr23 <dbl>,
#> #   hbr24 <dbl>, hbr25 <dbl>, hbr26 <dbl>, hbr27 <dbl>, hbr28 <dbl>,
#> #   hbr29 <dbl>, hbr30 <dbl>, hbr31 <dbl>, hbr32 <dbl>, hbr33 <dbl>,
#> #   hbr34 <dbl>, hbr35 <dbl>, hbr36 <dbl>, hbr37 <dbl>, hbr38 <dbl>, …
```
