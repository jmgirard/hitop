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

- hsr_001 to hsr_405:

  Responses on each item

## Examples

``` r
ku_hitopsr
#> # A tibble: 411 × 407
#>    participant biosex hsr_001 hsr_002 hsr_003 hsr_004 hsr_005 hsr_006 hsr_007
#>    <chr>       <fct>    <dbl>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl>
#>  1 P001        male         1       1       1       1       1       2       1
#>  2 P002        male         2       1       1       1       2       1       1
#>  3 P003        male         1       2       2       3       2       1       1
#>  4 P004        male         1       1       2       1       1       1       1
#>  5 P005        male         1       2       1       1       3       1       1
#>  6 P006        female       1       1       1       1       1       1       1
#>  7 P007        female       1       1       1       1       1       1       1
#>  8 P008        male         1       1       1       1       1       1       1
#>  9 P009        female       3       2       3       1       1       1       1
#> 10 P010        female       1       1       1       1       1       1       1
#> # ℹ 401 more rows
#> # ℹ 398 more variables: hsr_008 <dbl>, hsr_009 <dbl>, hsr_010 <dbl>,
#> #   hsr_011 <dbl>, hsr_012 <dbl>, hsr_013 <dbl>, hsr_014 <dbl>, hsr_015 <dbl>,
#> #   hsr_016 <dbl>, hsr_017 <dbl>, hsr_018 <dbl>, hsr_019 <dbl>, hsr_020 <dbl>,
#> #   hsr_021 <dbl>, hsr_022 <dbl>, hsr_023 <dbl>, hsr_024 <dbl>, hsr_025 <dbl>,
#> #   hsr_026 <dbl>, hsr_027 <dbl>, hsr_028 <dbl>, hsr_029 <dbl>, hsr_030 <dbl>,
#> #   hsr_031 <dbl>, hsr_032 <dbl>, hsr_033 <dbl>, hsr_034 <dbl>, …
```
