# Score the HiTOP-SR Instrument

Create a data frame with scores on all the HiTOP-SR scales.

## Usage

``` r
score_hitopsr(
  data,
  items,
  srange = c(1, 4),
  prefix = "hsr_",
  missing = c("available", "complete"),
  calc_se = FALSE,
  append = TRUE,
  subset = NULL
)
```

## Arguments

- data:

  A data frame containing the HiTOP-SR items (numerically coded): all
  405 of them, or, when `subset` is supplied, that short form's items.

- items:

  A vector of column names (as strings) or numbers (as integers)
  corresponding to the HiTOP-SR items held in `data` — all 405, or, when
  `subset` is supplied, that short form's items. Items must be supplied
  in instrument order; a misordered mapping silently scores the wrong
  items, so a warning is issued when the names share a common prefix and
  trailing number but those numbers are not ascending. Duplicated
  entries are an error.

- srange:

  An optional numeric vector specifying the minimum and maximum values
  of the HiTOP-SR items, used for reverse-coding. (default = `c(1, 4)`)

- prefix:

  An optional string to add before each scale column name. If no prefix
  is desired, set to an empty string `""`. (default = `"hsr_"`)

- missing:

  A string selecting how missing item responses are handled when
  computing scale scores. `"available"` (the default) averages whatever
  items are present (`rowMeans(na.rm = TRUE)`); `"complete"` returns
  `NA` for any scale with a missing item (`rowMeans(na.rm = FALSE)`).
  (default = `"available"`)

- calc_se:

  An optional logical indicating whether to calculate a standard error
  for each scale score: the SD of the items the respondent actually
  answered divided by the square root of how many of those items they
  answered. Each one summarizes how much a respondent's answers varied
  within a scale. It is not a standard error of measurement — no
  reliability estimate enters it — so it does not give a confidence
  interval for a respondent's true score; for measurement precision see
  [`reliability_hitopsr()`](https://jmgirard.github.io/hitop/reference/reliability_hitopsr.md).
  (default = `FALSE`)

- append:

  An optional logical indicating whether the new columns should be added
  to the end of the `data` input. (default = `TRUE`)

- subset:

  An optional `hitop_subset` object, as returned by
  [`hitop_subset()`](https://jmgirard.github.io/hitop/reference/hitop_subset.md),
  describing a short form of the instrument. When supplied, `data` and
  `items` hold only that subset's item columns — in ascending instrument
  order, as the `generate_*_hitopsr()` forms lay them out — and only
  that subset's scales are scored. When `NULL`, all 405 items are
  expected and all 76 scales are scored. (default = `NULL`)

## Value

A [tibble](https://tibble.tidyverse.org/reference/tibble.html)
containing all scale scores and standard errors (if requested) and all
original `data` columns (if requested).

## Details

For per-scale reliability estimates (Cronbach's alpha, McDonald's
omega), use
[`reliability_hitopsr()`](https://jmgirard.github.io/hitop/reference/reliability_hitopsr.md).

## Examples

``` r
# Score all HiTOP-SR scales from the simulated data
score_hitopsr(sim_hitopsr, items = 1:405, append = FALSE)
#> # A tibble: 100 × 76
#>    hsr_agoraphobia hsr_antisocialBehavior hsr_appetiteLoss hsr_bingeEating
#>              <dbl>                  <dbl>            <dbl>           <dbl>
#>  1             2.8                   2.75             2.67            2.67
#>  2             2.6                   2.75             3               2.33
#>  3             2.4                   2.75             2.67            2.33
#>  4             2.4                   2.38             2               2.67
#>  5             2.6                   2.5              2               2.67
#>  6             2.4                   3.12             2.67            2.33
#>  7             2.6                   2.38             2.33            1.33
#>  8             3                     2.38             2.67            1.67
#>  9             2.4                   2.38             1.67            2.33
#> 10             2.4                   2                2.33            1.67
#> # ℹ 90 more rows
#> # ℹ 72 more variables: hsr_bodilyDistress <dbl>, hsr_bodyDissatisfaction <dbl>,
#> #   hsr_bodyFocus <dbl>, hsr_callousness <dbl>, hsr_checking <dbl>,
#> #   hsr_cleaning <dbl>, hsr_cognitiveProblems <dbl>,
#> #   hsr_conversionSymptoms <dbl>, hsr_counting <dbl>,
#> #   hsr_dietaryRestraint <dbl>, hsr_difficultiesReachingOrgasm <dbl>,
#> #   hsr_diseaseConviction <dbl>, hsr_dishonesty <dbl>, …

# Score data collected with a two-scale short form. Select the item columns
# by name: `s$items` holds original HiTOP-SR numbers, which are column
# positions only in a data frame that is exactly the 405 items in order.
s <- hitop_subset("hitopsr", scales = c("Agoraphobia", "Appetite Loss"))
short <- sim_hitopsr[paste0("hsr_", s$items)]
score_hitopsr(short, items = names(short), subset = s, append = FALSE)
#> # A tibble: 100 × 2
#>    hsr_agoraphobia hsr_appetiteLoss
#>              <dbl>            <dbl>
#>  1             2.8             2.67
#>  2             2.6             3   
#>  3             2.4             2.67
#>  4             2.4             2   
#>  5             2.6             2   
#>  6             2.4             2.67
#>  7             2.6             2.33
#>  8             3               2.67
#>  9             2.4             1.67
#> 10             2.4             2.33
#> # ℹ 90 more rows
```
