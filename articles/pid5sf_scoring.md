# Scoring the PID-5-SF

``` r

library(hitop)
```

### Score simulated PID-5-SF data

The PID-5-SF is a shorter version of the PID-5 with 100 items that still
yields all domain and facet scores. The validity scales are still
calculable but may have fewer items and their psychometric properties
have not, to my knowledge, been examined with the FSF.

``` r

data("sim_pid5sf")

score_pid5(sim_pid5sf, items = 1:100, version = "SF", append = FALSE)
#> # A tibble: 100 × 30
#>    pid_suspiciousness pid_impulsivity pid_submissiveness pid_callousness
#>                 <dbl>           <dbl>              <dbl>           <dbl>
#>  1               1.5             1.5                1               2.25
#>  2               2               1.25               1               2   
#>  3               0.5             1.5                1.25            1.5 
#>  4               2               1                  2               1.25
#>  5               2.75            0.75               1               1.25
#>  6               0.75            1.5                2.75            1.5 
#>  7               0.75            0                  1.75            1   
#>  8               0.5             0.75               1               2.25
#>  9               2.25            1.75               2               1.5 
#> 10               1               1.25               1.75            1.5 
#> # ℹ 90 more rows
#> # ℹ 26 more variables: pid_anhedonia <dbl>, pid_eccentricity <dbl>,
#> #   pid_hostility <dbl>, pid_riskTaking <dbl>, pid_grandiosity <dbl>,
#> #   pid_perceptualDysregulation <dbl>, pid_separationInsecurity <dbl>,
#> #   pid_deceitfulness <dbl>, pid_perseveration <dbl>,
#> #   pid_attentionSeeking <dbl>, pid_anxiousness <dbl>, pid_depressivity <dbl>,
#> #   pid_withdrawal <dbl>, pid_restrictedAffectivity <dbl>, …

validity_pid5(sim_pid5sf, items = 1:100, version = "SF", append = FALSE)
#> ! A total of 96 observations (96.0%) met criteria for inconsistent responding on the INCS (0 missing).
#> ℹ Consider removing them with `dplyr::filter(df, pid_INCS < 8)`
#> ! Cut scores for the ORS-S, PRD-S, and SDTD-S have not been developed.
#> # A tibble: 100 × 5
#>    pid_PNA pid_INCS pid_ORSS pid_PRDS pid_SDTDS
#>      <dbl>    <dbl>    <dbl>    <dbl>     <dbl>
#>  1       0       11        2       26        19
#>  2       0       14        3       17        10
#>  3       0       13        1       14        11
#>  4       0       16        3       26        16
#>  5       0        9        3       11         8
#>  6       0       15        2       21        13
#>  7       0       15        4       16        11
#>  8       0       17        1       28        11
#>  9       0       15        1       18        16
#> 10       0       15        1       16        10
#> # ℹ 90 more rows
```

### Score real PID-5-SF data

We can repeat this process with real data that was collected at
University of Kansas (KU). There should be fewer (but still some)
validity problems since this is real data. We can also retain un-scored
“ID” variables in the dataset.

``` r

data("ku_pid5sf")

score_pid5(
  ku_pid5sf,
  items = paste0("pid_", 1:100),
  version = "SF",
  append = FALSE
)
#> # A tibble: 386 × 30
#>    pid_suspiciousness pid_impulsivity pid_submissiveness pid_callousness
#>                 <dbl>           <dbl>              <dbl>           <dbl>
#>  1               0               0                  0.5             0   
#>  2               0.5             0.25               1.5             0.5 
#>  3               1.75            1.75               2               1.75
#>  4               0.25            1                  0               0.25
#>  5               1.5             2.5                2               0.5 
#>  6               0.75            0.75               0.75            0   
#>  7               1.5             0.75               0.75            0.25
#>  8               0               0.25               1.25            0   
#>  9               0               0                  2.25            0   
#> 10               0.5             0.5                2.5             0.75
#> # ℹ 376 more rows
#> # ℹ 26 more variables: pid_anhedonia <dbl>, pid_eccentricity <dbl>,
#> #   pid_hostility <dbl>, pid_riskTaking <dbl>, pid_grandiosity <dbl>,
#> #   pid_perceptualDysregulation <dbl>, pid_separationInsecurity <dbl>,
#> #   pid_deceitfulness <dbl>, pid_perseveration <dbl>,
#> #   pid_attentionSeeking <dbl>, pid_anxiousness <dbl>, pid_depressivity <dbl>,
#> #   pid_withdrawal <dbl>, pid_restrictedAffectivity <dbl>, …

validity_pid5(
  ku_pid5sf,
  items = paste0("pid_", 1:100),
  version = "SF",
  append = FALSE
)
#> ! A total of 51 observations (13.2%) met criteria for inconsistent responding on the INCS (5 missing).
#> ℹ Consider removing them with `dplyr::filter(df, pid_INCS < 8)`
#> ! Cut scores for the ORS-S, PRD-S, and SDTD-S have not been developed.
#> # A tibble: 386 × 5
#>    pid_PNA pid_INCS pid_ORSS pid_PRDS pid_SDTDS
#>      <dbl>    <dbl>    <dbl>    <dbl>     <dbl>
#>  1       0        0        0        0         0
#>  2       0        2        0        7         7
#>  3       0        9        0       22        14
#>  4       0        4        1       10        14
#>  5       0        3        1       13         9
#>  6       0        6        0        3         3
#>  7       0        5        0       10         8
#>  8       0        4        0        2         5
#>  9       0        5        0        5         5
#> 10       0        7        0       14         7
#> # ℹ 376 more rows
```

### Scale Reliability

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
**lavaan** package installed (set `omega = FALSE` to skip it).

``` r

reliability_pid5(
  data = ku_pid5sf,
  items = sprintf("pid_%d", 1:100),
  version = "SF"
)
#> # A tibble: 25 × 4
#>    scale                    nItems alpha omega
#>    <chr>                     <int> <dbl> <dbl>
#>  1 Suspiciousness                4 0.663 0.665
#>  2 Impulsivity                   4 0.804 0.808
#>  3 Submissiveness                4 0.818 0.818
#>  4 Callousness                   4 0.816 0.816
#>  5 Anhedonia                     4 0.864 0.865
#>  6 Eccentricity                  4 0.832 0.833
#>  7 Hostility                     4 0.748 0.762
#>  8 Risk Taking                   4 0.782 0.785
#>  9 Grandiosity                   4 0.763 0.772
#> 10 Perceptual Dysregulation      4 0.690 0.689
#> # ℹ 15 more rows
```
