
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
data("sim_pid5")

score_pid5(sim_pid5, tibble = TRUE)
#> # A tibble: 100 × 30
#>    d_negati d_detatc d_antago d_disinh d_psycho f_anhedo f_anxiou f_attent
#>       <dbl>    <dbl>    <dbl>    <dbl>    <dbl>    <dbl>    <dbl>    <dbl>
#>  1     1.87     2.24     1.78     1.63     2.11     1.88     1.89     2   
#>  2     1.77     1.82     1.91     1.30     2.16     1.62     2.44     1.62
#>  3     1.26     1.36     2.76     2.25     1.74     1.5      1.22     2.38
#>  4     1.78     2.15     1.56     2.59     2.32     1.75     2.33     2   
#>  5     1.87     2.30     2.43     1.97     2.35     2.12     2.33     1.62
#>  6     1.82     2.21     1.82     2.38     2.26     1.75     1.89     1.75
#>  7     1.82     1.89     2.06     2.06     2.13     2        1.89     1.75
#>  8     2.08     1.79     1.77     1.45     1.73     1.75     2.11     1.88
#>  9     1.87     2.32     2.14     2.19     2.07     1.12     1.89     1.62
#> 10     1.73     2.02     2.26     2.34     1.82     1.5      1.33     2.25
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
#>  1    27     3
#>  2    31     1
#>  3    27     3
#>  4    20     0
#>  5    28     4
#>  6    27     1
#>  7    38     4
#>  8    26     2
#>  9    31     3
#> 10    28     3
#> # ℹ 90 more rows
```
