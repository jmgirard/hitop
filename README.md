
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
#> # A tibble: 100 × 2
#>    v_ris v_ors
#>    <dbl> <dbl>
#>  1 0.483   0.1
#>  2 0.483   0  
#>  3 0.45    0.2
#>  4 0.417   0.3
#>  5 0.483   0.4
#>  6 0.4     0.2
#>  7 0.383   0.2
#>  8 0.383   0.3
#>  9 0.367   0.5
#> 10 0.35    0.5
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
#> # A tibble: 100 × 2
#>    v_ris v_ors
#>    <dbl> <dbl>
#>  1 0.394 0.125
#>  2 0.485 0.25 
#>  3 0.697 0.25 
#>  4 0.394 0.375
#>  5 0.333 0.5  
#>  6 0.545 0    
#>  7 0.333 0.125
#>  8 0.606 0.375
#>  9 0.515 0.25 
#> 10 0.424 0.375
#> # ℹ 90 more rows
```
