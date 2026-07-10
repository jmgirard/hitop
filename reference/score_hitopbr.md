# Score the HiTOP-BR Instrument

Create a data frame with scores on all the HiTOP-BR scales.

## Usage

``` r
score_hitopbr(
  data,
  items,
  srange = c(1, 4),
  prefix = "hbr_",
  missing = c("available", "complete"),
  calc_se = FALSE,
  append = TRUE
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

- missing:

  A string selecting how missing item responses are handled when
  computing scale scores. `"available"` (the default) averages whatever
  items are present (`rowMeans(na.rm = TRUE)`); `"complete"` returns
  `NA` for any scale with a missing item (`rowMeans(na.rm = FALSE)`).
  (default = `"available"`)

- calc_se:

  An optional logical indicating whether to calculate the standard error
  of each scale score. (default = `FALSE`)

- append:

  An optional logical indicating whether the new columns should be added
  to the end of the `data` input. (default = `TRUE`)

## Value

A [tibble](https://tibble.tidyverse.org/reference/tibble.html)
containing all scale scores and standard errors (if requested) and all
original `data` columns (if requested).

## Details

For per-scale reliability estimates (Cronbach's alpha, McDonald's
omega), use
[`reliability_hitopbr()`](https://jmgirard.github.io/hitop/reference/reliability_hitopbr.md).

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
