# Simulated PID-5-BF Data

Simulated responses to items on the PID-5-BF (with 25 items).

## Usage

``` r
sim_pid5bf
```

## Format

A [tibble](https://tibble.tidyverse.org/reference/tibble.html) with 100
rows and 25 columns.

- pid5bf_01 to pid5bf_25:

  Responses on each item

## Examples

``` r
sim_pid5bf
#> # A tibble: 100 × 25
#>    pid5bf_01 pid5bf_02 pid5bf_03 pid5bf_04 pid5bf_05 pid5bf_06 pid5bf_07
#>        <int>     <int>     <int>     <int>     <int>     <int>     <int>
#>  1         1         3         2         3         3         0         1
#>  2         2         3         3         3         3         0         2
#>  3         3         3         3         0         0         3         1
#>  4         3         3         3         3         1         2         1
#>  5         2         1         3         1         3         2         2
#>  6         2         2         1         1         1         3         3
#>  7         2         1         2         3         0         0         0
#>  8         2         3         2         2         0         0         1
#>  9         2         3         1         1         2         0         0
#> 10         1         0         1         3         2         2         2
#> # ℹ 90 more rows
#> # ℹ 18 more variables: pid5bf_08 <int>, pid5bf_09 <int>, pid5bf_10 <int>,
#> #   pid5bf_11 <int>, pid5bf_12 <int>, pid5bf_13 <int>, pid5bf_14 <int>,
#> #   pid5bf_15 <int>, pid5bf_16 <int>, pid5bf_17 <int>, pid5bf_18 <int>,
#> #   pid5bf_19 <int>, pid5bf_20 <int>, pid5bf_21 <int>, pid5bf_22 <int>,
#> #   pid5bf_23 <int>, pid5bf_24 <int>, pid5bf_25 <int>
```
