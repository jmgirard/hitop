# Score the Personality Inventory for DSM-5

Calculate scale scores on the Personality Inventory for DSM-5: full
version (PID-5, 220 items), short form version (PID-5-SF, 100 items), or
brief form version (PID-5-BF, 25 items) from item-level data.

## Usage

``` r
score_pid5(
  data,
  items,
  version = c("FULL", "SF", "BF"),
  srange = c(0, 3),
  prefix = "pid_",
  na.rm = TRUE,
  apa_scoring = TRUE,
  calc_se = FALSE,
  alpha = FALSE,
  omega = FALSE,
  append = TRUE,
  tibble = TRUE
)
```

## Arguments

- data:

  A data frame containing (at least) all the PID items (numerically
  scored and in order).

- items:

  A vector of column names (as strings) or numbers (as integers)
  corresponding to the PID items in order.

- version:

  A string indicating the version of the PID to score: "FULL", "SF", or
  "BF". Will be automatically capitalized. (default = `"FULL"`)

- srange:

  An optional numeric vector specifying the minimum and maximum values
  of the items, used for reverse-coding. (default = `c(0, 3)`)

- prefix:

  An optional string to add before each scale column name. If no prefix
  is desired, set to an empty string `""`. (default = `"pid_"`)

- na.rm:

  An optional logical indicating whether missing values should be
  ignored when calculating scale scores. Ignored when
  `apa_scoring = TRUE` (the APA missing-data rule governs instead); a
  warning is issued if `na.rm = FALSE` is set explicitly alongside
  `apa_scoring = TRUE`. (default = `TRUE`)

- apa_scoring:

  An optional logical selecting the missing-data algorithm. If `TRUE`
  (the default), scale scores follow the published APA scoring key: a
  facet or domain-item scale with more than 25% of its items unanswered
  is set to `NA`, and otherwise the raw score is prorated to the full
  item count and rounded to the nearest whole number before averaging; a
  FULL/SF domain is `NA` if any one of its three contributing facets is
  `NA`. If `FALSE`, scores use the traditional `rowMeans(na.rm = na.rm)`
  behavior, which averages whatever items are present. With no missing
  items the two agree. (default = `TRUE`)

- calc_se:

  An optional logical indicating whether to calculate the standard error
  of each scale score. Standard errors are `NA` wherever their scale
  score is `NA`. (default = `FALSE`)

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
  to a [tibble](https://tibble.tidyverse.org/reference/tibble.html).
  (default = `TRUE`)

## Value

A [tibble](https://tibble.tidyverse.org/reference/tibble.html)
containing all scale scores and standard errors (if requested) and all
original `data` columns (if requested)

## Details

For the FULL and SF versions, the output includes the 25 facet scores
followed by the 5 personality-trait domain scores. Following the APA
scoring key (Step 3), each domain score is the mean of the average
scores of its 3 primary facets (the map is stored in `pid_domains`). The
BF version scores its 5 domains directly from its items. By default
(`apa_scoring = TRUE`) all versions apply the APA missing-data and
proration rule; set `apa_scoring = FALSE` for the traditional
`rowMeans(na.rm = na.rm)` behavior. If either `alpha` or `omega` are
`TRUE`, the function prints a per-scale reliability summary (facets only
for FULL/SF). Only reliability columns that contain at least one
non-`NA` value are shown (the `scale` column is always shown).

## References

Krueger, R. F., Derringer, J., Markon, K. E., Watson, D., & Skodol, A.
E. (2012). Initial construction of a maladaptive personality trait model
and inventory for DSM-5. *Psychological Medicine, 42*, 1879-1890.
[doi:10.1017/s0033291711002674](https://doi.org/10.1017/s0033291711002674)

Anderson, J. L., Sellbom, M., & Salekin, R. T. (2016). Utility of the
Personality Inventory for DSM-5-Brief Form (PID-5-BF) in the measurement
of maladaptive personality and psychopathology. *Assessment, 25*(5),
596–607.
[doi:10.1177/1073191116676889](https://doi.org/10.1177/1073191116676889)

Maples, J. L., Carter, N. T., Few, L. R., Crego, C., Gore, W. L.,
Samuel, D. B., Williamson, R. L., Lynam, D. R., Widiger, T. A., Markon,
K. E., Krueger, R. F., & Miller, J. D. (2015). Testing whether the DSM-5
personality disorder trait model can be measured with a reduced set of
items: An item response theory investigation of the personality
inventory for DSM-5. *Psychological Assessment, 27*(4), 1195–1210.
[doi:10.1037/pas0000120](https://doi.org/10.1037/pas0000120)
