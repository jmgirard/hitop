# Score the HiTOP-SR Instrument

Create a data frame with scores on all the HiTOP-SR scales.

## Usage

``` r
score_hitopsr(
  data,
  items,
  srange = c(1, 4),
  prefix = "hsr_",
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

  A data frame containing all HiTOP-SR items (numerically coded).

- items:

  A vector of column names (as strings) or numbers (as integers)
  corresponding to the 405 HiTOP-SR items in order.

- srange:

  An optional numeric vector specifying the minimum and maximum values
  of the HiTOP-SR items, used for reverse-coding. (default = `c(1, 4)`)

- prefix:

  An optional string to add before each scale column name. If no prefix
  is desired, set to an empty string `""`. (default = `"hsr_"`)

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
