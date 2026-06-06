# Simulated PID-5-SF Data

Simulated responses to items on the PID-5-SF (with 100 items).

## Usage

``` r
sim_pid5sf
```

## Format

A [tibble](https://tibble.tidyverse.org/reference/tibble.html) with 100
rows and 100 columns.

- pid_1 to pid_100:

  Responses on each item

## Examples

``` r
sim_pid5sf
#> # A tibble: 100 × 100
#>    pid_1 pid_2 pid_3 pid_4 pid_5 pid_6 pid_7 pid_8 pid_9 pid_10 pid_11 pid_12
#>    <int> <int> <int> <int> <int> <int> <int> <int> <int>  <int>  <int>  <int>
#>  1     3     3     1     2     1     1     2     1     3      1      3      2
#>  2     3     1     1     0     2     1     3     1     0      2      3      1
#>  3     0     3     1     2     0     3     3     0     0      0      2      3
#>  4     3     0     2     3     2     1     0     1     0      3      0      3
#>  5     2     1     3     0     1     0     0     1     0      0      1      2
#>  6     1     3     3     3     0     3     2     0     0      1      1      3
#>  7     0     0     0     1     0     0     0     0     0      0      1      3
#>  8     2     0     3     0     0     0     3     3     0      3      3      3
#>  9     2     3     3     1     3     1     1     0     3      0      0      3
#> 10     0     2     3     3     0     1     3     2     2      2      2      1
#> # ℹ 90 more rows
#> # ℹ 88 more variables: pid_13 <int>, pid_14 <int>, pid_15 <int>, pid_16 <int>,
#> #   pid_17 <int>, pid_18 <int>, pid_19 <int>, pid_20 <int>, pid_21 <int>,
#> #   pid_22 <int>, pid_23 <int>, pid_24 <int>, pid_25 <int>, pid_26 <int>,
#> #   pid_27 <int>, pid_28 <int>, pid_29 <int>, pid_30 <int>, pid_31 <int>,
#> #   pid_32 <int>, pid_33 <int>, pid_34 <int>, pid_35 <int>, pid_36 <int>,
#> #   pid_37 <int>, pid_38 <int>, pid_39 <int>, pid_40 <int>, pid_41 <int>, …
```
