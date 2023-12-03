
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

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(hitop)

## Score simulated PID-5 data
data("sim_pid5")
score_pid5(sim_pid5, tibble = TRUE)
#> # A tibble: 100 × 30
#>    d_negati d_detatc d_antago d_disinh d_psycho f_anhedo f_anxiou f_attent
#>       <dbl>    <dbl>    <dbl>    <dbl>    <dbl>    <dbl>    <dbl>    <dbl>
#>  1     1.39     1.54     2.03     1.67     1.37     1.62     1.89    1.62 
#>  2     1.50     1.39     1.6      1.54     1.61     1.75     1.22    1.38 
#>  3     1.51     1.56     1.54     1.28     1.69     1.75     1.67    2    
#>  4     1.36     1.45     1.51     1.43     1.44     1.12     1.22    1.12 
#>  5     1.76     1.33     1.63     1.12     1.73     1.12     2       0.875
#>  6     1.59     1.46     1.36     1.51     1.64     1.62     1.33    0.625
#>  7     1.63     2.08     1.42     1.53     1.52     1.5      1.89    2    
#>  8     1.48     1.43     1.72     1.83     1.42     1.5      1       2.38 
#>  9     1.59     1.71     1.5      2.07     1.24     1.38     1.78    1.88 
#> 10     2.03     1.46     1.7      1.99     1.68     1.5      1.67    1.75 
#> # ℹ 90 more rows
#> # ℹ 22 more variables: f_callou <dbl>, f_deceit <dbl>, f_depres <dbl>,
#> #   f_distra <dbl>, f_eccent <dbl>, f_emotio <dbl>, f_grandi <dbl>,
#> #   f_hostil <dbl>, f_impuls <dbl>, f_intima <dbl>, f_irresp <dbl>,
#> #   f_manipu <dbl>, f_percep <dbl>, f_persev <dbl>, f_restri <dbl>,
#> #   f_rigidp <dbl>, f_riskta <dbl>, f_separa <dbl>, f_submis <dbl>,
#> #   f_suspis <dbl>, f_unusua <dbl>, f_withdr <dbl>
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

## Score simulated PID-5-FSF data
data("sim_pid5fsf")
score_pid5fsf(sim_pid5fsf, tibble = TRUE)
#> # A tibble: 100 × 30
#>    d_negati d_detatc d_antago d_disinh d_psycho f_anhedo f_anxiou f_attent
#>       <dbl>    <dbl>    <dbl>    <dbl>    <dbl>    <dbl>    <dbl>    <dbl>
#>  1     2.17     1.33     1       1.83      1.08     1.25     2.25     1.25
#>  2     2        1.17     1.25    2.08      0.75     0.5      2.5      1.75
#>  3     1.58     2        1.17    1.33      1.33     2.25     1.5      2.25
#>  4     1.42     1.25     1.42    1.42      1.5      1        2.25     1.5 
#>  5     1.67     1.33     1.33    2.25      1.5      0.75     1.75     0.25
#>  6     1.5      1.83     1.83    1.75      1.67     2.25     1.5      1.25
#>  7     1.33     1.5      1.92    0.917     1.25     1.25     1.25     0.75
#>  8     1.92     1.75     1.42    1.92      1.08     2.25     2.5      1.25
#>  9     1.42     1        1.67    1.83      1.25     0.75     1.75     2   
#> 10     1.17     1.25     1       1.67      2        1        1.5      2.25
#> # ℹ 90 more rows
#> # ℹ 22 more variables: f_callou <dbl>, f_deceit <dbl>, f_depres <dbl>,
#> #   f_distra <dbl>, f_eccent <dbl>, f_emotio <dbl>, f_grandi <dbl>,
#> #   f_hostil <dbl>, f_impuls <dbl>, f_intima <dbl>, f_irresp <dbl>,
#> #   f_manipu <dbl>, f_percep <dbl>, f_persev <dbl>, f_restri <dbl>,
#> #   f_rigidp <dbl>, f_riskta <dbl>, f_separa <dbl>, f_submis <dbl>,
#> #   f_suspis <dbl>, f_unusua <dbl>, f_withdr <dbl>
validity_pid5fsf(sim_pid5fsf, tibble = TRUE)
#> ! A total of 97 observations (97.0%) met criteria for inconsistent responding (0 missing).
#> ℹ Consider removing them with `dplyr::filter(df, v_incs < 8)`
#> # A tibble: 100 × 5
#>    v_pna v_incs v_orss v_prds v_sdtds
#>    <dbl>  <dbl>  <dbl>  <dbl>   <dbl>
#>  1     0     13      1     16      10
#>  2     0     16      2     15      16
#>  3     0     23      2     18      13
#>  4     0     13      3     19       8
#>  5     0     11      4     18      11
#>  6     0     18      0     15      13
#>  7     0     11      1     13      13
#>  8     0     20      3     24      14
#>  9     0     17      2     18      11
#> 10     0     14      3     17      11
#> # ℹ 90 more rows

## Score real PID-5-FSF data
data("ku_pid5fsf")
score_pid5fsf(ku_pid5fsf, id = "response_id", tibble = TRUE)
#> # A tibble: 386 × 31
#>    response_id    d_negati d_detatc d_antago d_disinh d_psycho f_anhedo f_anxiou
#>    <chr>             <dbl>    <dbl>    <dbl>    <dbl>    <dbl>    <dbl>    <dbl>
#>  1 R_2BsNloyAVAk…    0        0        0.167    0        0         0        0   
#>  2 R_WrIeYf92JAq…    1.58     1.17     0.25     0.583    0.25      1        2   
#>  3 R_3OlvdFDleHT…    1.75     1.67     1.58     1.5      1.5       1.25     1.75
#>  4 R_3JI4ceKdZSe…    1        2        1.08     0.833    2.25      2.75     0   
#>  5 R_2CCf8JCtPLr…    0.917    0.917    1        1.92     1.75      1        1.75
#>  6 R_VJQP3waDjzI…    0.833    0.167    0.25     0.333    0.5       0        0.75
#>  7 R_6WqPyblq7lf…    1.17     1.17     0.75     0.667    1.25      1        2   
#>  8 R_3p59ilU5u9h…    0.917    0.25     0.167    0.167    0.667     0        1.25
#>  9 R_1GEzNhlFaoX…    0.75     0.5      0        0.75     0.333     0.75     1   
#> 10 R_Dl45sKLgxFL…    2.17     1.42     1        0.667    0.25      0.75     2.75
#> # ℹ 376 more rows
#> # ℹ 23 more variables: f_attent <dbl>, f_callou <dbl>, f_deceit <dbl>,
#> #   f_depres <dbl>, f_distra <dbl>, f_eccent <dbl>, f_emotio <dbl>,
#> #   f_grandi <dbl>, f_hostil <dbl>, f_impuls <dbl>, f_intima <dbl>,
#> #   f_irresp <dbl>, f_manipu <dbl>, f_percep <dbl>, f_persev <dbl>,
#> #   f_restri <dbl>, f_rigidp <dbl>, f_riskta <dbl>, f_separa <dbl>,
#> #   f_submis <dbl>, f_suspis <dbl>, f_unusua <dbl>, f_withdr <dbl>
validity_pid5fsf(ku_pid5fsf, id = "response_id", tibble = TRUE)
#> ! A total of 69 observations (17.9%) met criteria for inconsistent responding (5 missing).
#> ℹ Consider removing them with `dplyr::filter(df, v_incs < 8)`
#> # A tibble: 386 × 6
#>    response_id       v_pna v_incs v_orss v_prds v_sdtds
#>    <chr>             <dbl>  <dbl>  <dbl>  <dbl>   <dbl>
#>  1 R_2BsNloyAVAk3UnQ     0      0      0      0       0
#>  2 R_WrIeYf92JAqCDE5     0      3      0      7       7
#>  3 R_3OlvdFDleHTo9IE     0      9      0     22      14
#>  4 R_3JI4ceKdZSeoo6m     0      4      1     10      14
#>  5 R_2CCf8JCtPLrwQfB     0      4      1     13       9
#>  6 R_VJQP3waDjzIAStz     0      7      0      3       3
#>  7 R_6WqPyblq7lfVGzn     0      5      0     10       8
#>  8 R_3p59ilU5u9hxa7v     0      5      0      2       5
#>  9 R_1GEzNhlFaoXuAF6     0      5      0      5       5
#> 10 R_Dl45sKLgxFLJ67f     0      7      0     14       7
#> # ℹ 376 more rows
```
