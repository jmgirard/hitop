# Scoring the PID-5-BF

``` r
library(hitop)
```

WORK IN PROGRESS

### Score simulated PID-5-BF data

Finally, the PID-5-BF is a brief version with 25 items that yields the
domain scores only. The only validity scale that is calculable with this
subset of items is the percentage of missing items (PNA).

``` r
data("sim_pid5bf")

score_pid5(sim_pid5bf, items = 1:25, version = "BF", append = FALSE)
#> # A tibble: 100 × 5
#>    pid_disinhibition pid_detachment pid_psychoticism pid_negativeAffectivity
#>                <dbl>          <dbl>            <dbl>                   <dbl>
#>  1               1.8            1.6              2                       1.8
#>  2               2.2            2.2              2.2                     1.4
#>  3               2.4            1.2              1.8                     1.6
#>  4               2.4            2.2              0.8                     0.8
#>  5               2.2            1.2              1.4                     2.8
#>  6               1.8            0.6              2.2                     1.2
#>  7               1              2                1.6                     1.4
#>  8               1.4            1.8              1.2                     1.8
#>  9               1.6            0.8              2.2                     0.8
#> 10               1.2            1.8              1.4                     0.6
#> # ℹ 90 more rows
#> # ℹ 1 more variable: pid_antagonism <dbl>

validity_pid5(sim_pid5bf, items = 1:25, version = "BF", append = FALSE)
#> # A tibble: 100 × 1
#>    pid_PNA
#>      <dbl>
#>  1       0
#>  2       0
#>  3       0
#>  4       0
#>  5       0
#>  6       0
#>  7       0
#>  8       0
#>  9       0
#> 10       0
#> # ℹ 90 more rows
```
