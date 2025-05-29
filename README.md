
<!-- README.md is generated from README.Rmd. Please edit that file -->

# hitop

<!-- badges: start -->

<!-- badges: end -->

The goal of hitop is to provide functions helpful for researchers
working on the Hierarchical Taxonomy of Psychopathology (HiTOP).

## Installation

You can install the development version of hitop from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("jmgirard/hitop")
```

## Examples

### Score real HiTOP-PRO data

The full HiTOP-PRO has 405 items and yields 76 scale scores. We can
demonstrate the ability of the `score_hitop_pro()` function to calcualte
these scores using real data that was collected at the University of
Kansas (KU).

``` r
library(hitop)
data("ku_hitoppro")

score_hitop_pro(ku_hitoppro, id = c("participant", "biosex"), tibble = TRUE)
#> # A tibble: 143 × 78
#>    participant    biosex agoraphobia antisocialBehavior appetiteLoss bingeEating
#>    <chr>          <fct>        <dbl>              <dbl>        <dbl>       <dbl>
#>  1 R_3KOxNF2JCWC… female         1                 1            1           1.33
#>  2 R_1RLNDHA6qwM… female         1.4               1            1.33        1   
#>  3 R_61bkFQweO6u… female         1                 1            1.67        1.67
#>  4 R_3kO0nXySSg3… female         1.2               1            2.33        3   
#>  5 R_5yGEvYGy4Yq… female         3.2               1            2.67        1   
#>  6 R_3Pv6gPT8dxY… female         1.2               1            2           1.33
#>  7 R_3KBAdhGCuga… female         1.6               1.25         2.67        1.67
#>  8 R_5fkRaVh1ZiN… male           1                 1.12         1           2.33
#>  9 R_5hbvtJ91lzo… male           3                 1            1.33        2   
#> 10 R_5dWEI8k79ah… female         1                 1            1           1   
#> # ℹ 133 more rows
#> # ℹ 72 more variables: bodilyDistress <dbl>, bodyDissatisfaction <dbl>,
#> #   bodyFocus <dbl>, callousness <dbl>, checking <dbl>, cleaning <dbl>,
#> #   cognitiveProblems <dbl>, conversionSymptoms <dbl>, counting <dbl>,
#> #   dietaryRestraint <dbl>, difficultiesReachingOrgasm <dbl>,
#> #   diseaseConviction <dbl>, dishonesty <dbl>, disorganization <dbl>,
#> #   dissociation <dbl>, distressDysphoria <dbl>, domineering <dbl>, …
```

### Score simulated PID-5 data

The full PID-5 has 220 items and yields 5 domain scores and 25 facet
scores. We can demonstrate the ability of the `score_pid5()` function to
calculate these scores using simulated (fake) data.

``` r
data("sim_pid5")

score_pid5(sim_pid5, tibble = TRUE)
#> # A tibble: 100 × 30
#>    f_anhedo f_anxiou f_attent f_callou f_deceit f_depres f_distra f_eccent
#>       <dbl>    <dbl>    <dbl>    <dbl>    <dbl>    <dbl>    <dbl>    <dbl>
#>  1     1.25     2.75     1.5      1        0.5      1        0.75     1.25
#>  2     1.25     1.75     1.75     2        0.75     0.75     0.5      1.25
#>  3     1        1        1.75     1.5      1.25     1.5      1.25     1.25
#>  4     0.5      1.25     1.25     2        1.75     0.25     1        1   
#>  5     1.5      2.25     1.75     1.25     2        1        1        2   
#>  6     1.5      1.75     2        2        1.5      1        1.25     1.25
#>  7     1.75     0.5      2        2        2.5      1.25     1.75     2   
#>  8     1.25     1.5      2        1        0.75     1.25     0.5      2   
#>  9     2.75     1.5      1.25     1.25     1        2.25     1        1.25
#> 10     1.25     2        1.25     1.5      2        0.75     0.5      2   
#> # ℹ 90 more rows
#> # ℹ 22 more variables: f_emotio <dbl>, f_grandi <dbl>, f_hostil <dbl>,
#> #   f_impuls <dbl>, f_intima <dbl>, f_irresp <dbl>, f_manipu <dbl>,
#> #   f_percep <dbl>, f_persev <dbl>, f_restri <dbl>, f_rigidp <dbl>,
#> #   f_riskta <dbl>, f_separa <dbl>, f_submis <dbl>, f_suspis <dbl>,
#> #   f_unusua <dbl>, f_withdr <dbl>, d_negati <dbl>, d_detach <dbl>,
#> #   d_antago <dbl>, d_disinh <dbl>, d_psycho <dbl>
```

There are also several validity scales that have been developed for the
full PID-5, including measures of overreporting, inconsistent
responding, and positive impression management. We can demonstrate the
ability of the `validity_pid5()` function to calculate these scores and
flag issues using the same simulated data. Note that, because the data
is fake, we would expect there to be lots of validity issues.

``` r
validity_pid5(sim_pid5, tibble = TRUE)
#> ! A total of 99 observations (99.0%) met criteria for inconsistent responding (0 missing).
#> ℹ Consider removing them with `dplyr::filter(df, v_inc < 17)`
#> ! A total of 53 observations (53.0%) met criteria for overreporting (0 missing).
#> ℹ Consider removing them with `dplyr::filter(df, v_ors < 3)`
#> # A tibble: 100 × 5
#>    v_pna v_inc v_ors v_prd v_sdtd
#>    <dbl> <dbl> <dbl> <dbl>  <dbl>
#>  1     0    29     1    34     25
#>  2     0    29     0    41     26
#>  3     0    27     2    28     22
#>  4     0    25     3    29     16
#>  5     0    29     4    37     27
#>  6     0    24     2    29     18
#>  7     0    23     2    31     25
#>  8     0    23     3    30     29
#>  9     0    22     5    40     27
#> 10     0    21     5    35     28
#> # ℹ 90 more rows
```

### Score simulated PID-5-FSF data

The PID-5-FSF is a shorter version with 100 items that still yields all
domain and facet scores. The validity scales are still calculable but
may have fewer items and their psychometric properties have not, to my
knowledge, been examined with the FSF.

``` r
data("sim_pid5fsf")

score_pid5fsf(sim_pid5fsf, tibble = TRUE)
#> # A tibble: 100 × 30
#>    f_anhedo f_anxiou f_attent f_callou f_deceit f_depres f_distra f_eccent
#>       <dbl>    <dbl>    <dbl>    <dbl>    <dbl>    <dbl>    <dbl>    <dbl>
#>  1     1.25     2.25     1.25     2.5      0.75     1.75     2        1   
#>  2     0.5      2.5      1.75     1.25     2        0.75     1.75     1   
#>  3     2.25     1.5      2.25     2.75     1.5      2        2        1.5 
#>  4     1        2.25     1.5      1.25     1.75     2.25     2        1   
#>  5     0.75     1.75     0.25     1.5      1        1        2.25     1.5 
#>  6     2.25     1.5      1.25     1.25     2.25     1.25     2        2.25
#>  7     1.25     1.25     0.75     1.25     1.25     1.5      0.5      1.75
#>  8     2.25     2.5      1.25     1        0.75     2.5      2        0.75
#>  9     0.75     1.75     2        1        1.25     2.5      2.25     1.25
#> 10     1        1.5      2.25     1.75     1.5      2.25     1.5      1.25
#> # ℹ 90 more rows
#> # ℹ 22 more variables: f_emotio <dbl>, f_grandi <dbl>, f_hostil <dbl>,
#> #   f_impuls <dbl>, f_intima <dbl>, f_irresp <dbl>, f_manipu <dbl>,
#> #   f_percep <dbl>, f_persev <dbl>, f_restri <dbl>, f_rigidp <dbl>,
#> #   f_riskta <dbl>, f_separa <dbl>, f_submis <dbl>, f_suspis <dbl>,
#> #   f_unusua <dbl>, f_withdr <dbl>, d_negati <dbl>, d_detach <dbl>,
#> #   d_antago <dbl>, d_disinh <dbl>, d_psycho <dbl>

validity_pid5fsf(sim_pid5fsf, tibble = TRUE)
#> ! A total of 96 observations (96.0%) met criteria for inconsistent responding (0 missing).
#> ℹ Consider removing them with `dplyr::filter(df, v_incs < 8)`
#> # A tibble: 100 × 5
#>    v_pna v_incs v_orss v_prds v_sdtds
#>    <dbl>  <dbl>  <dbl>  <dbl>   <dbl>
#>  1     0     12      1     16      10
#>  2     0     15      2     15      16
#>  3     0     21      2     18      13
#>  4     0     12      3     19       8
#>  5     0     11      4     18      11
#>  6     0     17      0     15      13
#>  7     0     10      1     13      13
#>  8     0     19      3     24      14
#>  9     0     17      2     18      11
#> 10     0     12      3     17      11
#> # ℹ 90 more rows
```

### Score real PID-5-FSF data

We can repeat this process with real data that was collected at
University of Kansas (KU). There should be fewer (but still some)
validity problems since this is real data. We can also retain un-scored
“ID” variables in the dataset.

``` r
data("ku_pid5fsf")

score_pid5fsf(ku_pid5fsf, id = "response_id", tibble = TRUE)
#> # A tibble: 386 × 31
#>    response_id    f_anhedo f_anxiou f_attent f_callou f_deceit f_depres f_distra
#>    <chr>             <dbl>    <dbl>    <dbl>    <dbl>    <dbl>    <dbl>    <dbl>
#>  1 R_2BsNloyAVAk…     0        0        0        0        0        0        0   
#>  2 R_WrIeYf92JAq…     1        2        0.75     0.5      0.25     1        1.5 
#>  3 R_3OlvdFDleHT…     1.25     1.75     1.5      1.75     1.5      1.5      1   
#>  4 R_3JI4ceKdZSe…     2.75     0        2.5      0.25     0.25     1.25     0.5 
#>  5 R_2CCf8JCtPLr…     1        1.75     0.75     0.5      0.75     0.25     3   
#>  6 R_VJQP3waDjzI…     0        0.75     0.75     0        0.5      0        0.25
#>  7 R_6WqPyblq7lf…     1        2        0.5      0.25     1.5      0.5      1   
#>  8 R_3p59ilU5u9h…     0        1.25     1.5      0        0.25     0.25     0   
#>  9 R_1GEzNhlFaoX…     0.75     1        1        0        0        0.25     2   
#> 10 R_Dl45sKLgxFL…     0.75     2.75     1.5      0.75     2        2.25     1.25
#> # ℹ 376 more rows
#> # ℹ 23 more variables: f_eccent <dbl>, f_emotio <dbl>, f_grandi <dbl>,
#> #   f_hostil <dbl>, f_impuls <dbl>, f_intima <dbl>, f_irresp <dbl>,
#> #   f_manipu <dbl>, f_percep <dbl>, f_persev <dbl>, f_restri <dbl>,
#> #   f_rigidp <dbl>, f_riskta <dbl>, f_separa <dbl>, f_submis <dbl>,
#> #   f_suspis <dbl>, f_unusua <dbl>, f_withdr <dbl>, d_negati <dbl>,
#> #   d_detach <dbl>, d_antago <dbl>, d_disinh <dbl>, d_psycho <dbl>

validity_pid5fsf(ku_pid5fsf, id = "response_id", tibble = TRUE)
#> ! A total of 51 observations (13.2%) met criteria for inconsistent responding (5 missing).
#> ℹ Consider removing them with `dplyr::filter(df, v_incs < 8)`
#> # A tibble: 386 × 6
#>    response_id       v_pna v_incs v_orss v_prds v_sdtds
#>    <chr>             <dbl>  <dbl>  <dbl>  <dbl>   <dbl>
#>  1 R_2BsNloyAVAk3UnQ     0      0      0      0       0
#>  2 R_WrIeYf92JAqCDE5     0      2      0      7       7
#>  3 R_3OlvdFDleHTo9IE     0      9      0     22      14
#>  4 R_3JI4ceKdZSeoo6m     0      4      1     10      14
#>  5 R_2CCf8JCtPLrwQfB     0      3      1     13       9
#>  6 R_VJQP3waDjzIAStz     0      6      0      3       3
#>  7 R_6WqPyblq7lfVGzn     0      5      0     10       8
#>  8 R_3p59ilU5u9hxa7v     0      4      0      2       5
#>  9 R_1GEzNhlFaoXuAF6     0      5      0      5       5
#> 10 R_Dl45sKLgxFLJ67f     0      7      0     14       7
#> # ℹ 376 more rows
```

### Score simulated PID-5-BF data

Finally, the PID-5-BF is a brief version with 25 items that yields the
domain scores only. The validity scales are not calculable with this
subset of items.

``` r
data("sim_pid5bf")
score_pid5bf(sim_pid5bf, tibble = TRUE)
#> # A tibble: 100 × 5
#>    d_negati d_detatc d_antago d_disinh d_psycho
#>       <dbl>    <dbl>    <dbl>    <dbl>    <dbl>
#>  1      1.4      0.8      0.8      1.4      1.4
#>  2      1.8      1.8      2.4      1        2  
#>  3      2.2      1.6      1.4      1        0.4
#>  4      2.2      2.2      1.4      1.2      1.2
#>  5      2.4      2.2      0.8      1.6      2.2
#>  6      1.2      1.8      1.2      2.2      1.6
#>  7      1.4      1.6      1.2      1.8      2  
#>  8      2.2      1.8      2.2      1.6      1.4
#>  9      1.4      1.4      2.2      1.4      1.6
#> 10      1.4      1.8      2        2.6      0.4
#> # ℹ 90 more rows
```
