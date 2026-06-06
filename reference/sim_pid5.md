# Simulated PID-5 Data

Simulated responses to items on the full PID-5 (with 220 items).

## Usage

``` r
sim_pid5
```

## Format

A [tibble](https://tibble.tidyverse.org/reference/tibble.html) with 100
rows and 220 columns.

- pid_1 to pid_220:

  Responses on each item

## Examples

``` r
sim_pid5
#> # A tibble: 100 × 220
#>    pid_1 pid_2 pid_3 pid_4 pid_5 pid_6 pid_7 pid_8 pid_9 pid_10 pid_11 pid_12
#>    <int> <int> <int> <int> <int> <int> <int> <int> <int>  <int>  <int>  <int>
#>  1     0     3     2     1     1     3     1     3     3      0      0      3
#>  2     3     3     0     3     0     3     2     2     1      2      0      0
#>  3     3     2     3     2     3     3     0     3     3      2      3      3
#>  4     1     3     0     2     1     0     2     0     3      2      3      2
#>  5     0     1     3     2     3     1     0     1     2      2      2      2
#>  6     2     1     1     3     3     2     2     0     1      3      1      3
#>  7     1     1     3     3     1     3     1     0     1      1      0      2
#>  8     2     0     3     0     3     2     0     1     3      1      2      0
#>  9     1     1     3     0     1     1     2     3     1      1      3      1
#> 10     0     3     2     3     3     0     1     2     1      3      0      2
#> # ℹ 90 more rows
#> # ℹ 208 more variables: pid_13 <int>, pid_14 <int>, pid_15 <int>, pid_16 <int>,
#> #   pid_17 <int>, pid_18 <int>, pid_19 <int>, pid_20 <int>, pid_21 <int>,
#> #   pid_22 <int>, pid_23 <int>, pid_24 <int>, pid_25 <int>, pid_26 <int>,
#> #   pid_27 <int>, pid_28 <int>, pid_29 <int>, pid_30 <int>, pid_31 <int>,
#> #   pid_32 <int>, pid_33 <int>, pid_34 <int>, pid_35 <int>, pid_36 <int>,
#> #   pid_37 <int>, pid_38 <int>, pid_39 <int>, pid_40 <int>, pid_41 <int>, …
```
