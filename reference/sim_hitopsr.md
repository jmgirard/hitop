# Simulated HiTOP-SR Data

Simulated responses to items on the full HiTOP-SR (with 405 items). Note
that this is a naive simulation where response options 1 to 4 are all
equally likely and generated independently per item. Thus, responses are
not clustered within scales, and these data can be used (eventually) to
test validity tools intended to detect inconsistent/random responding.

## Usage

``` r
sim_hitopsr
```

## Format

A [tibble](https://tibble.tidyverse.org/reference/tibble.html) with 100
rows and 405 columns.

- hsr_001 to hsr_405:

  Responses on each item

## Examples

``` r
sim_hitopsr
#> # A tibble: 100 × 405
#>    hsr_001 hsr_002 hsr_003 hsr_004 hsr_005 hsr_006 hsr_007 hsr_008 hsr_009
#>      <int>   <int>   <int>   <int>   <int>   <int>   <int>   <int>   <int>
#>  1       2       2       2       3       2       1       3       1       1
#>  2       3       4       4       1       1       3       4       1       1
#>  3       3       2       2       3       2       3       3       3       1
#>  4       3       3       4       2       2       3       4       4       1
#>  5       1       3       4       1       2       1       4       3       2
#>  6       1       1       2       3       1       2       2       4       1
#>  7       2       1       1       3       2       1       2       3       4
#>  8       1       1       3       3       1       1       3       2       3
#>  9       3       4       4       3       4       3       3       2       3
#> 10       2       4       1       1       2       2       3       2       2
#> # ℹ 90 more rows
#> # ℹ 396 more variables: hsr_010 <int>, hsr_011 <int>, hsr_012 <int>,
#> #   hsr_013 <int>, hsr_014 <int>, hsr_015 <int>, hsr_016 <int>, hsr_017 <int>,
#> #   hsr_018 <int>, hsr_019 <int>, hsr_020 <int>, hsr_021 <int>, hsr_022 <int>,
#> #   hsr_023 <int>, hsr_024 <int>, hsr_025 <int>, hsr_026 <int>, hsr_027 <int>,
#> #   hsr_028 <int>, hsr_029 <int>, hsr_030 <int>, hsr_031 <int>, hsr_032 <int>,
#> #   hsr_033 <int>, hsr_034 <int>, hsr_035 <int>, hsr_036 <int>, …
```
