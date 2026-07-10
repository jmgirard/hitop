# Score the HiTOP-BR Instrument

Create a data frame with scores on all the HiTOP-BR scales.

## Usage

``` r
score_hitopbr(
  data,
  items,
  srange = c(1, 4),
  prefix = "hbr_",
  na.rm = TRUE,
  calc_se = FALSE,
  alpha = FALSE,
  omega = FALSE,
  append = TRUE,
  tibble = TRUE
)
```

## Arguments

- data:

  A data frame containing all HiTOP-BR items (numerically coded).

- items:

  A vector of column names (as strings) or numbers (as integers)
  corresponding to the 45 HiTOP-BR items in order. Items must be
  supplied in instrument order; a misordered mapping silently scores the
  wrong items, so a warning is issued when the names share a common
  prefix and trailing number but those numbers are not ascending.
  Duplicated entries are an error.

- srange:

  An optional numeric vector specifying the minimum and maximum values
  of the HiTOP-BR items, used for reverse-coding. (default = `c(1, 4)`)

- prefix:

  An optional string to add before each scale column name. If no prefix
  is desired, set to an empty string `""`. (default = `"hbr_"`)

- na.rm:

  An optional logical indicating whether missing values should be
  ignored when calculating scale scores. (default = `TRUE`)

- calc_se:

  An optional logical indicating whether to calculate the standard error
  of each scale score. (default = `FALSE`)

- alpha:

  Optional logical; if `TRUE`, compute and print Cronbach’s alpha for
  each scale. (default = `FALSE`)

- omega:

  Optional logical; if `TRUE`, compute and print McDonald’s omega for
  each scale using Pearson correlations (i.e., non-ordinal). (default =
  `FALSE`)

- append:

  An optional logical indicating whether the new columns should be added
  to the end of the `data` input. (default = `TRUE`)

- tibble:

  An optional logical indicating whether the output should be converted
  to a
  [`tibble::tibble()`](https://tibble.tidyverse.org/reference/tibble.html).
  (default = `TRUE`)

## Value

A data frame containing all scale scores and standard errors (if
requested) and all original `data` columns (if requested). Reliability
estimates, when requested, are printed as a side effect.

## Details

If either `alpha` or `omega` are `TRUE`, the function prints a per-scale
reliability summary. Only reliability columns that contain at least one
non-`NA` value are shown (the `scale` column is always shown).

## Examples

``` r
# Score all HiTOP-BR scales from the simulated data
score_hitopbr(sim_hitopbr, items = 1:45, append = FALSE)
#> # A tibble: 100 × 8
#>    hbr_antagonism hbr_detachment hbr_disinhibition hbr_internalizing
#>             <dbl>          <dbl>             <dbl>             <dbl>
#>  1           2.56           2.5               1.67              2.29
#>  2           2.11           1.67              2.67              2.57
#>  3           2.44           3                 2.22              2.14
#>  4           2.67           2.33              2.22              2.29
#>  5           2.78           2.17              2.78              2   
#>  6           2              2.5               3.22              2.86
#>  7           2.44           2.17              3.11              2.29
#>  8           2.78           2.17              2.33              2.43
#>  9           1.89           2.67              2.67              2.43
#> 10           2.89           2                 2.78              2.71
#> # ℹ 90 more rows
#> # ℹ 4 more variables: hbr_somatoform <dbl>, hbr_thoughtDisorder <dbl>,
#> #   hbr_externalizing <dbl>, hbr_pFactor <dbl>
```
