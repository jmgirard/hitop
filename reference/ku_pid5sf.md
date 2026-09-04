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

- pid5sf_001 to pid5sf_100:

  Responses on each item

## Examples

``` r
ku_pid5sf
#> # A tibble: 386 × 101
#>    response_id pid5sf_001 pid5sf_002 pid5sf_003 pid5sf_004 pid5sf_005 pid5sf_006
#>    <chr>            <int>      <int>      <int>      <int>      <int>      <int>
#>  1 R_2BsNloyA…          0          0          2          0          0          0
#>  2 R_WrIeYf92…          0          0          0          2          0          0
#>  3 R_3OlvdFDl…          1          1          2          2          2          2
#>  4 R_3JI4ceKd…          0          1          0          0          1          1
#>  5 R_2CCf8JCt…          1          2          2          2          3          2
#>  6 R_VJQP3waD…          0          0          1          1          0          1
#>  7 R_6WqPyblq…          1          1          1          1          1          1
#>  8 R_3p59ilU5…          0          1          1          1          0          0
#>  9 R_1GEzNhlF…          0          0          2          2          0          0
#> 10 R_Dl45sKLg…          0          1          2          3          0          0
#> # ℹ 376 more rows
#> # ℹ 94 more variables: pid5sf_007 <int>, pid5sf_008 <int>, pid5sf_009 <int>,
#> #   pid5sf_010 <int>, pid5sf_011 <int>, pid5sf_012 <int>, pid5sf_013 <int>,
#> #   pid5sf_014 <int>, pid5sf_015 <int>, pid5sf_016 <int>, pid5sf_017 <int>,
#> #   pid5sf_018 <int>, pid5sf_019 <int>, pid5sf_020 <int>, pid5sf_021 <int>,
#> #   pid5sf_022 <int>, pid5sf_023 <int>, pid5sf_024 <int>, pid5sf_025 <int>,
#> #   pid5sf_026 <int>, pid5sf_027 <int>, pid5sf_028 <int>, pid5sf_029 <int>, …
```
