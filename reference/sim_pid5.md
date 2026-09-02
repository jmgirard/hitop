# Simulated PID-5 Data

Simulated responses to items on the full PID-5 (with 220 items).

## Usage

``` r
sim_pid5
```

## Format

A [tibble](https://tibble.tidyverse.org/reference/tibble.html) with 100
rows and 220 columns.

- pid5_001 to pid5_220:

  Responses on each item

## Examples

``` r
sim_pid5
#> # A tibble: 100 × 220
#>    pid5_001 pid5_002 pid5_003 pid5_004 pid5_005 pid5_006 pid5_007 pid5_008
#>       <int>    <int>    <int>    <int>    <int>    <int>    <int>    <int>
#>  1        0        3        2        1        1        3        1        3
#>  2        3        3        0        3        0        3        2        2
#>  3        3        2        3        2        3        3        0        3
#>  4        1        3        0        2        1        0        2        0
#>  5        0        1        3        2        3        1        0        1
#>  6        2        1        1        3        3        2        2        0
#>  7        1        1        3        3        1        3        1        0
#>  8        2        0        3        0        3        2        0        1
#>  9        1        1        3        0        1        1        2        3
#> 10        0        3        2        3        3        0        1        2
#> # ℹ 90 more rows
#> # ℹ 212 more variables: pid5_009 <int>, pid5_010 <int>, pid5_011 <int>,
#> #   pid5_012 <int>, pid5_013 <int>, pid5_014 <int>, pid5_015 <int>,
#> #   pid5_016 <int>, pid5_017 <int>, pid5_018 <int>, pid5_019 <int>,
#> #   pid5_020 <int>, pid5_021 <int>, pid5_022 <int>, pid5_023 <int>,
#> #   pid5_024 <int>, pid5_025 <int>, pid5_026 <int>, pid5_027 <int>,
#> #   pid5_028 <int>, pid5_029 <int>, pid5_030 <int>, pid5_031 <int>, …
```
