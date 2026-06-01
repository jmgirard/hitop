# Real HiTOP-SR Data

Real responses to items on the HiTOP-SR from University of Kansas
students.

## Usage

``` r
ku_hitopsr
```

## Format

A [tibble](https://tibble.tidyverse.org/reference/tibble.html) with 411
rows and 407 columns.

- participant:

  An anonymized id for each participant

- biosex:

  A factor indicating each participant's biological sex

- hsr001 to hsr405:

  Responses on each item

## Examples

``` r
ku_hitopsr
#> # A tibble: 411 × 407
#>    participant biosex hsr001 hsr002 hsr003 hsr004 hsr005 hsr006 hsr007 hsr008
#>    <chr>       <fct>   <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>
#>  1 P001        male        1      1      1      1      1      2      1      2
#>  2 P002        male        2      1      1      1      2      1      1      1
#>  3 P003        male        1      2      2      3      2      1      1      1
#>  4 P004        male        1      1      2      1      1      1      1      1
#>  5 P005        male        1      2      1      1      3      1      1      1
#>  6 P006        female      1      1      1      1      1      1      1      1
#>  7 P007        female      1      1      1      1      1      1      1      1
#>  8 P008        male        1      1      1      1      1      1      1      1
#>  9 P009        female      3      2      3      1      1      1      1      1
#> 10 P010        female      1      1      1      1      1      1      1      1
#> # ℹ 401 more rows
#> # ℹ 397 more variables: hsr009 <dbl>, hsr010 <dbl>, hsr011 <dbl>, hsr012 <dbl>,
#> #   hsr013 <dbl>, hsr014 <dbl>, hsr015 <dbl>, hsr016 <dbl>, hsr017 <dbl>,
#> #   hsr018 <dbl>, hsr019 <dbl>, hsr020 <dbl>, hsr021 <dbl>, hsr022 <dbl>,
#> #   hsr023 <dbl>, hsr024 <dbl>, hsr025 <dbl>, hsr026 <dbl>, hsr027 <dbl>,
#> #   hsr028 <dbl>, hsr029 <dbl>, hsr030 <dbl>, hsr031 <dbl>, hsr032 <dbl>,
#> #   hsr033 <dbl>, hsr034 <dbl>, hsr035 <dbl>, hsr036 <dbl>, hsr037 <dbl>, …
```
