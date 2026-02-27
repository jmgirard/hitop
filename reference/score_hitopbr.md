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
  append = TRUE,
  tibble = TRUE
)
```

## Arguments

- data:

  A data frame containing all HiTOP-BR items (numerically coded).

- items:

  A vector of column names (as strings) or numbers (as integers)
  corresponding to the 45 HiTOP-BR items in order.

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
requested) and all original `data` columns (if requested)
