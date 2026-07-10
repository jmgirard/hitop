# Scoring the PID-5-BF

``` r

library(hitop)
```

The PID-5-BF (Brief Form) is a 25-item version of the PID-5 that yields
the 5 personality-trait domain scores only (it does not produce the 25
facet scores). We can demonstrate the package’s functionality using some
simulated data.

## Score simulated PID-5-BF data

The `sim_pid5bf` dataset is built into the package and contains 100 rows
(each a simulated participant) across 25 columns named `pid_1` to
`pid_25`. To compute the 5 domain scores, we use
[`score_pid5()`](https://jmgirard.github.io/hitop/reference/score_pid5.md)
with `version = "BF"`. As with the other forms, we can specify the items
by column number (`items = 1:25`) and set `append = FALSE` to see just
the scores. The only validity scale calculable from this subset of items
is the percentage of missing items (PNA), which
[`validity_pid5()`](https://jmgirard.github.io/hitop/reference/validity_pid5.md)
returns.

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

## Scale Reliability

As we compute scale scores, we can also estimate their inter-item
reliability using Cronbach’s α (alpha) or McDonald’s ω (omega total). α
is fast and widely used, but it assumes tau-equivalence (all items load
equally on a single factor); violations can make α under- or
over-estimate reliability. ω is based on a congeneric single-factor
model, allowing items to have different loadings and error variances; it
typically provides a more accurate reliability estimate for
unit-weighted sums. Both assume the scale is essentially unidimensional;
α and ω coincide when tau-equivalence holds.

We estimate reliability with the
[`reliability_pid5()`](https://jmgirard.github.io/hitop/reference/reliability_pid5.md)
function, which returns a tibble with one row per scale and columns for
the number of items and the requested coefficients. By default it
computes both `alpha` and `omega`; for the latter, we will need the
**lavaan** package installed (set `omega = FALSE` to skip it). Note
that, because this is naively simulated data, we would expect the
reliability in this example to be poor.

``` r

reliability_pid5(
  data = sim_pid5bf,
  items = 1:25,
  version = "BF"
)
#> # A tibble: 5 × 4
#>   scale                nItems   alpha    omega
#>   <chr>                 <int>   <dbl>    <dbl>
#> 1 Disinhibition             5 -0.260  0.00111 
#> 2 Detachment                5  0.238  0.365   
#> 3 Psychoticism              5  0.0658 0.0863  
#> 4 Negative Affectivity      5 -0.0852 0.000422
#> 5 Antagonism                5 -0.0967 0.105
```
