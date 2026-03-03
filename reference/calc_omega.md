# Compute McDonald's Omega (total) via CFA

Computes McDonald's \\\omega\_\mathrm{total}\\ for a unidimensional
scale using a one-factor confirmatory factor analysis (CFA) fit with
**lavaan** using MLR estimation. Missing data can be handled via full
information maximum likelihood (FIML).

## Usage

``` r
calc_omega(df)
```

## Arguments

- df:

  A data frame or matrix with one row per observation and one column per
  item.

## Value

A numeric scalar giving McDonald;s \\\omega\_\mathrm{total}\\ (typically
in \[0, 1\]; small estimation anomalies can yield values slightly
outside this range).

## Details

The model is `f =~ item1 + item2 + ... + itemK` with `std.lv = TRUE`
(i.e., \\\mathrm{Var}(f)=1\\).

\\\omega = \frac{(\sum_i \lambda_i)^2} {(\sum_i \lambda_i)^2 + \sum_i
\theta_i}\\, where \\\lambda_i\\ are *unstandardized* loadings and
\\\theta_i\\ are *unstandardized* residual variances of the indicators.

The function validates inputs and fails with informative messages if the
CFA does not converge or if implied residual variances are invalid.
Mixed-sign loadings trigger a warning (often indicates reverse-keying is
needed).

Note that the latent variable will be estimated as `f`, so please avoid
naming any of your items that.

## References

- McDonald, R. P. (1999). *Test Theory: A Unified Treatment*. Lawrence
  Erlbaum.

- Green, S. B., & Yang, Y. (2009). Reliability of summed item scores
  using structural equation modeling: An alternative to coefficient
  alpha. *Psychometrika, 74*(1), 155–167.

- Zinbarg, R. E., Revelle, W., Yovel, I., & Li, W. (2005). Cronbach's α,
  Revelle's β, and McDonald's ωH. *Psychometrika, 70*(1), 123–133.
