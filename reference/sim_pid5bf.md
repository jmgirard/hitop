# Simulated PID-5-BF Data

Simulated responses to items on the PID-5-BF (with 25 items).

## Usage

``` r
sim_pid5bf
```

## Format

A [tibble](https://tibble.tidyverse.org/reference/tibble.html) with 100
rows and 25 columns.

- pid_1 to pid_25:

  Responses on each item

## Examples

``` r
sim_pid5bf
#> # A tibble: 100 × 25
#>    pid_1 pid_2 pid_3 pid_4 pid_5 pid_6 pid_7 pid_8 pid_9 pid_10 pid_11 pid_12
#>    <int> <int> <int> <int> <int> <int> <int> <int> <int>  <int>  <int>  <int>
#>  1     1     3     2     3     3     0     1     1     1      3      2      1
#>  2     2     3     3     3     3     0     2     2     0      3      0      0
#>  3     3     3     3     0     0     3     1     3     3      1      0      1
#>  4     3     3     3     3     1     2     1     0     0      2      0      0
#>  5     2     1     3     1     3     2     2     3     3      3      2      1
#>  6     2     2     1     1     1     3     3     2     1      3      0      3
#>  7     2     1     2     3     0     0     0     3     1      0      1      0
#>  8     2     3     2     2     0     0     1     3     2      1      3      1
#>  9     2     3     1     1     2     0     0     0     1      1      2      3
#> 10     1     0     1     3     2     2     2     2     0      0      1      0
#> # ℹ 90 more rows
#> # ℹ 13 more variables: pid_13 <int>, pid_14 <int>, pid_15 <int>, pid_16 <int>,
#> #   pid_17 <int>, pid_18 <int>, pid_19 <int>, pid_20 <int>, pid_21 <int>,
#> #   pid_22 <int>, pid_23 <int>, pid_24 <int>, pid_25 <int>
```
