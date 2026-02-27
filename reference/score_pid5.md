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
  calc_se = FALSE,
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
  ignored when calculating scale scores. (default = `TRUE`)

- calc_se:

  An optional logical indicating whether to calculate the standard error
  of each scale score. (default = `FALSE`)

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
