# Simulated HiTOP-BR Data

Simulated responses to items on the HiTOP-BR (with 45 items). Note that
this is a naive simulation where response options 1 to 4 are all equally
likely and generated independently per item. Thus, responses are not
clustered within scales, and these data can be used (eventually) to test
validity tools intended to detect inconsistent/random responding.

## Usage

``` r
sim_hitopbr
```

## Format

A [tibble](https://tibble.tidyverse.org/reference/tibble.html) with 100
rows and 45 columns.

- hitopbr_1 to hitopbr_45:

  Responses on each item

## Examples

``` r
sim_hitopbr
#> # A tibble: 100 × 45
#>    hitopbr_1 hitopbr_2 hitopbr_3 hitopbr_4 hitopbr_5 hitopbr_6 hitopbr_7
#>        <int>     <int>     <int>     <int>     <int>     <int>     <int>
#>  1         3         3         3         2         2         3         2
#>  2         1         2         2         4         1         1         3
#>  3         3         4         3         2         1         1         2
#>  4         1         3         3         2         3         3         2
#>  5         4         4         4         3         1         2         3
#>  6         1         3         3         3         3         3         4
#>  7         2         2         4         3         4         3         1
#>  8         3         3         1         2         3         4         1
#>  9         2         4         1         1         2         2         2
#> 10         4         2         4         2         4         4         1
#> # ℹ 90 more rows
#> # ℹ 38 more variables: hitopbr_8 <int>, hitopbr_9 <int>, hitopbr_10 <int>,
#> #   hitopbr_11 <int>, hitopbr_12 <int>, hitopbr_13 <int>, hitopbr_14 <int>,
#> #   hitopbr_15 <int>, hitopbr_16 <int>, hitopbr_17 <int>, hitopbr_18 <int>,
#> #   hitopbr_19 <int>, hitopbr_20 <int>, hitopbr_21 <int>, hitopbr_22 <int>,
#> #   hitopbr_23 <int>, hitopbr_24 <int>, hitopbr_25 <int>, hitopbr_26 <int>,
#> #   hitopbr_27 <int>, hitopbr_28 <int>, hitopbr_29 <int>, hitopbr_30 <int>, …
```
