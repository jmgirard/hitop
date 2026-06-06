# Real PID-5-SF Data

Real responses to items on the PID-5-SF (with 100 items) from University
of Kansas students.

## Usage

``` r
ku_pid5sf
```

## Format

A [tibble](https://tibble.tidyverse.org/reference/tibble.html) with 386
rows and 101 columns.

- response_id:

  An anonymized id for each participant

- pid_1 to pid_100:

  Responses on each item

## Examples

``` r
ku_pid5sf
#> # A tibble: 386 × 101
#>    response_id      pid_1 pid_2 pid_3 pid_4 pid_5 pid_6 pid_7 pid_8 pid_9 pid_10
#>    <chr>            <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>  <dbl>
#>  1 R_2BsNloyAVAk3U…     0     0     2     0     0     0     0     0     0      0
#>  2 R_WrIeYf92JAqCD…     0     0     0     2     0     0     0     1     1      0
#>  3 R_3OlvdFDleHTo9…     1     1     2     2     2     2     2     2     2      1
#>  4 R_3JI4ceKdZSeoo…     0     1     0     0     1     1     0     1     3      3
#>  5 R_2CCf8JCtPLrwQ…     1     2     2     2     3     2     0     3     1      2
#>  6 R_VJQP3waDjzIAS…     0     0     1     1     0     1     0     2     0      0
#>  7 R_6WqPyblq7lfVG…     1     1     1     1     1     1     0     0     1      2
#>  8 R_3p59ilU5u9hxa…     0     1     1     1     0     0     0     0     0      2
#>  9 R_1GEzNhlFaoXuA…     0     0     2     2     0     0     0     0     2      0
#> 10 R_Dl45sKLgxFLJ6…     0     1     2     3     0     0     0     1     1      0
#> # ℹ 376 more rows
#> # ℹ 90 more variables: pid_11 <dbl>, pid_12 <dbl>, pid_13 <dbl>, pid_14 <dbl>,
#> #   pid_15 <dbl>, pid_16 <dbl>, pid_17 <dbl>, pid_18 <dbl>, pid_19 <dbl>,
#> #   pid_20 <dbl>, pid_21 <dbl>, pid_22 <dbl>, pid_23 <dbl>, pid_24 <dbl>,
#> #   pid_25 <dbl>, pid_26 <dbl>, pid_27 <dbl>, pid_28 <dbl>, pid_29 <dbl>,
#> #   pid_30 <dbl>, pid_31 <dbl>, pid_32 <dbl>, pid_33 <dbl>, pid_34 <dbl>,
#> #   pid_35 <dbl>, pid_36 <dbl>, pid_37 <dbl>, pid_38 <dbl>, pid_39 <dbl>, …
```
