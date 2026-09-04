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
#>    <chr>       <fct>    <int>   <int>   <int>   <int>   <int>   <int>   <int>
#>  1 P001        male         1       1       1       1       1       1       2
#>  2 P002        male         3       4       1       1       2       2       2
#>  3 P003        male         3       3       1       1       2       1       2
#>  4 P004        male         2       3       1       1       1       1       1
#>  5 P005        male         1       2       1       1       3       1       2
#>  6 P006        female       2       1       1       1       2       2       2
#>  7 P007        female       1       1       1       1       1       1       1
#>  8 P008        male         2       4       1       1       1       1       2
#>  9 P009        female       4       1       1       1       2       1       3
#> 10 P010        female       1       3       1       1       1       1       1
#> # ℹ 401 more rows
#> # ℹ 398 more variables: hsr_008 <int>, hsr_009 <int>, hsr_010 <int>,
#> #   hsr_011 <int>, hsr_012 <int>, hsr_013 <int>, hsr_014 <int>, hsr_015 <int>,
#> #   hsr_016 <int>, hsr_017 <int>, hsr_018 <int>, hsr_019 <int>, hsr_020 <int>,
#> #   hsr_021 <int>, hsr_022 <int>, hsr_023 <int>, hsr_024 <int>, hsr_025 <int>,
#> #   hsr_026 <int>, hsr_027 <int>, hsr_028 <int>, hsr_029 <int>, hsr_030 <int>,
#> #   hsr_031 <int>, hsr_032 <int>, hsr_033 <int>, hsr_034 <int>, …
```
