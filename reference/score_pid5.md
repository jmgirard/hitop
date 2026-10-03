# Score the Personality Inventory for DSM-5

Calculate scale scores on the Personality Inventory for DSM-5: full
version (PID-5, 220 items), short form version (PID-5-SF, 100 items),
brief form version (PID-5-BF, 25 items), or modified brief form
(PID5BF+M, 36 items; Bach et al., 2020) from item-level data.

## Usage

``` r
score_pid5(
  data,
  items,
  version = c("FULL", "SF", "BF", "BFPM"),
  srange = c(0, 3),
  prefix = "pid_",
  missing = c("apa", "available", "complete"),
  calc_se = FALSE,
  append = TRUE
)
```

## Arguments

- data:

  A data frame containing (at least) all the PID items (numerically
  scored and in order).

- items:

  A vector of column names (as strings) or numbers (as integers)
  corresponding to the PID items in order. Items must be supplied in
  instrument order; a misordered mapping silently scores the wrong
  items, so a warning is issued when the names share a common prefix and
  trailing number but those numbers are not ascending. Duplicated
  entries are an error. Each column must be numeric or logical, or
  character holding only numbers (blank cells and `NA` values count as
  missing, but the text `"NA"` is refused). A haven labelled column is
  read as its plain values. Any other column, such as the choice text of
  an online export or a factor, is an error of class
  `hitop_nonnumeric_items`. So is a 64-bit integer (`integer64`) column,
  and, in a UTF-8 session, a column holding text that is not valid UTF-8
  (text marked Latin-1 is read as its text); convert such text with
  [`iconv()`](https://rdrr.io/r/base/iconv.html). So is an SPSS column
  (`haven_labelled_spss`) that holds a value it declares missing; turn
  those codes into `NA` with
  [`haven::zap_missing()`](https://haven.tidyverse.org/reference/zap_missing.html)
  first. A declared value that is blank is not refused, since blank
  cells count as missing. The SPSS class of haven before 2.0
  (`labelled_spss`) is refused the same way, but
  [`haven::zap_missing()`](https://haven.tidyverse.org/reference/zap_missing.html)
  leaves it unchanged, so set the values its `na_values` or `na_range`
  attribute declares to `NA` first.

- version:

  A string indicating the version of the PID to score: "FULL", "SF",
  "BF", or "BFPM" (the 36-item PID5BF+M). Will be automatically
  capitalized. (default = `"FULL"`)

- srange:

  An optional numeric vector specifying the minimum and maximum values
  of the items, used for reverse-coding. (default = `c(0, 3)`)

- prefix:

  An optional string to add before each scale column name. If no prefix
  is desired, set to an empty string `""`. (default = `"pid_"`)

- missing:

  A string selecting how missing item responses are handled when
  computing scale scores. `"apa"` (the default) follows the published
  APA scoring key: a facet or domain-item scale with more than 25% of
  its items unanswered is set to `NA`, and otherwise the raw score is
  prorated to the full item count and rounded to the nearest whole
  number before averaging (a FULL, SF or BFPM domain is `NA` if any one
  of its three contributing facets is `NA`). `"available"` averages
  whatever items are present (`rowMeans(na.rm = TRUE)`). `"complete"`
  returns `NA` for any scale with a missing item
  (`rowMeans(na.rm = FALSE)`). With no missing items the three agree.
  (default = `"apa"`)

- calc_se:

  **Deprecated.** This argument, and the `_se` columns it adds, will be
  removed in a future release; a call with `calc_se = TRUE` warns; the
  warning is classed `hitop_deprecated_calc_se`, so a caller can silence
  it by name. This package has no interval function for the PID-5, so
  there is no replacement for it on this instrument; for measurement
  precision see
  [`reliability_pid5()`](https://jmgirard.github.io/hitop/reference/reliability_pid5.md).
  What it does while it lasts: an optional logical indicating whether to
  calculate a standard error for each scale score. For the facets, and
  for the brief form's domains and total, this is the SD of the items
  the respondent actually answered divided by the square root of how
  many of those items they answered. The FULL, SF and BFPM domain scores
  are means of three facet scores rather than of items, so their
  standard errors are taken one level up: the SD of the three
  contributing facet scores divided by the square root of 3. Standard
  errors are `NA` wherever their scale score is `NA`. A BFPM facet
  scored from one answered item under `missing = "available"` has a
  score but an `NA` standard error, because the SD of one value is
  undefined. Each one summarizes how much a respondent's answers varied
  within a scale. It is not a standard error of measurement — no
  reliability estimate enters it — so it does not give a confidence
  interval for a respondent's true score; for measurement precision see
  [`reliability_pid5()`](https://jmgirard.github.io/hitop/reference/reliability_pid5.md).
  (default = `FALSE`)

- append:

  An optional logical indicating whether the new columns should be added
  to the end of the `data` input. (default = `TRUE`)

## Value

A [tibble](https://tibble.tidyverse.org/reference/tibble.html)
containing all scale scores and standard errors (if requested) and all
original `data` columns (if requested)

## Details

For the FULL and SF versions, the output includes the 25 facet scores
followed by the 5 personality-trait domain scores. Following the APA
scoring key (Step 3), each domain score is the mean of the average
scores of its 3 primary facets (the map is stored in `pid_domains`). The
BF version scores its 5 domains directly from its items, and adds a
`total` score. By default (`missing = "apa"`) all versions apply the APA
missing-data and proration rule; use `missing = "available"` or
`missing = "complete"` for the traditional
[`rowMeans()`](https://rdrr.io/r/base/colSums.html) behaviors. For
per-scale reliability estimates (Cronbach's alpha, McDonald's omega),
use
[`reliability_pid5()`](https://jmgirard.github.io/hitop/reference/reliability_pid5.md).

### The PID5BF+M

`version = "BFPM"` scores the PID5BF+M of Bach et al. (2020), a 36-item
form with 6 domains: the 5 PID-5 trait domains and Anankastia. Every
item is a PID-5 item, and none is reverse-keyed. The output is 18 facets
of 2 items each, then 6 domains, in the order of the form's key:
Negative affectivity, Detachment, Antagonism, Disinhibition, Anankastia
and Psychoticism. Each domain score is the mean of its 3 facet scores
(the map is stored in
[pid_bfpm_domains](https://jmgirard.github.io/hitop/reference/pid_bfpm_domains.md)).
The 15 facets the form shares with the PID-5 keep their PID-5 column
names. The three Anankastia facets are `perfectionism`, `rigidity` and
`orderliness`. All six of their items are PID-5 Rigid Perfectionism
items, but these facets are not parts of `rigidPerfectionism`, which
this version does not score.

Scores are item means on the 0 to 3 scale, as for the other versions.
The form's published key sums the 2 items of a facet and averages the
facet sums for a domain. On complete data, the key's facet sum is
`2 * pid_<facet>` and its domain score is `2 * pid_<domain>`.

No missing-data rule is published for this form. Under the default
`missing = "apa"`, the 25% rule applied to a 2-item facet means that any
missing item makes the facet `NA`, and an `NA` facet makes its domain
`NA`. So with whole-number responses `"apa"` gives the same output as
`"complete"` here. (The APA rule rounds each scale's sum, so responses
with decimals can differ.) Under `missing = "available"`, a facet can be
scored from one item and a domain from one or two of its facets.

### The PID-5-BF total score

`version = "BF"` returns a `total` column after its 5 domains. Markon et
al. (2024, p. 23) define it as the item-level mean over **all 25
items**, not the mean of the 5 domain means: the total "can be computed
by averaging the overall score by the total number of items in the
measure (i.e., 25)". With five equal-sized domains the two definitions
coincide on complete data and differ only when items are missing, where
the published rule above governs.

The total is scored like any other scale, so `missing` applies to it at
the 25-item level. Under `missing = "apa"` that means it is `NA` when
more than a quarter of the 25 items are unanswered (7 or more) and
prorated otherwise, independently of the domains. Because a 5-item
domain is dropped at 2 unanswered items while the total tolerates 6, **a
total can be reported alongside one or more `NA` domains** (at most 3 of
the 5; blanking all five requires 10 unanswered items, which blanks the
total as well). This is the published rule applied as written, not an
oversight.

The FULL, SF and BFPM versions have no total score: the PID-5 book
defines one only for the brief form, and the PID5BF+M sources define
none.

**Errors.** With `append = TRUE`, a column of `data` whose name this
call would also produce is an error rather than an overwrite or a
duplicated column: the message names every colliding column. Re-run with
`append = FALSE` to return only the new columns, or drop the colliding
columns from `data` first. The condition is classed
`hitop_append_collision`, so a caller can catch this refusal by name.

An item column that cannot be read as numbers (see `items`) is an error
of class `hitop_nonnumeric_items`, raised before the collision check.

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

Markon, K. E., Fossati, A., Somma, A., & Krueger, R. F. (2024).
*Understanding the Personality Inventory for DSM-5 (PID-5).* American
Psychiatric Association Publishing. The source for the PID-5-BF total
score's definition (p. 23) and for the normative tables in `pid_norms`.

Maples, J. L., Carter, N. T., Few, L. R., Crego, C., Gore, W. L.,
Samuel, D. B., Williamson, R. L., Lynam, D. R., Widiger, T. A., Markon,
K. E., Krueger, R. F., & Miller, J. D. (2015). Testing whether the DSM-5
personality disorder trait model can be measured with a reduced set of
items: An item response theory investigation of the personality
inventory for DSM-5. *Psychological Assessment, 27*(4), 1195–1210.
[doi:10.1037/pas0000120](https://doi.org/10.1037/pas0000120)

Bach, B., Kerber, A., Aluja, A., Bastiaens, T., Keeley, J. W., Claes,
L., Fossati, A., Gutierrez, F., Oliveira, S. E. S., Pires, R., Riegel,
K. D., Rolland, J.-P., Roskam, I., Sellbom, M., Somma, A., Spanemberg,
L., Strus, W., Thimm, J. C., Wright, A. G. C., & Zimmermann, J. (2020).
International assessment of DSM-5 and ICD-11 personality disorder
traits: Toward a common nosology in DSM-5.1. *Psychopathology, 53*(3-4),
179-188. [doi:10.1159/000507589](https://doi.org/10.1159/000507589) The
source of the PID5BF+M.

## Examples

``` r
# Score the full PID-5 (25 facets + 5 domains) from the simulated data
score_pid5(sim_pid5, items = 1:220, version = "FULL", append = FALSE)
#> # A tibble: 100 × 30
#>    pid_anhedonia pid_suspiciousness pid_riskTaking pid_impulsivity
#>            <dbl>              <dbl>          <dbl>           <dbl>
#>  1          1.25               1.71           1.36           2.33 
#>  2          1.38               1.57           1.43           2    
#>  3          1.88               1              1.29           1.83 
#>  4          1.25               2.43           1.21           1.5  
#>  5          1.12               1.57           1.64           2.5  
#>  6          2.12               1              1.79           1.83 
#>  7          1.38               1.14           1.86           1.17 
#>  8          1.5                1.71           1.86           0.667
#>  9          1.12               1.14           1.86           1.67 
#> 10          1.38               1.86           2.07           2    
#> # ℹ 90 more rows
#> # ℹ 26 more variables: pid_eccentricity <dbl>, pid_distractibility <dbl>,
#> #   pid_restrictedAffectivity <dbl>, pid_submissiveness <dbl>,
#> #   pid_withdrawal <dbl>, pid_callousness <dbl>,
#> #   pid_separationInsecurity <dbl>, pid_attentionSeeking <dbl>,
#> #   pid_emotionalLability <dbl>, pid_depressivity <dbl>, pid_hostility <dbl>,
#> #   pid_irresponsibility <dbl>, pid_rigidPerfectionism <dbl>, …

# Short form, using the item column names instead of positions
score_pid5(sim_pid5sf, items = sprintf("pid5sf_%03d", 1:100), version = "SF",
           append = FALSE)
#> # A tibble: 100 × 30
#>    pid_suspiciousness pid_impulsivity pid_submissiveness pid_callousness
#>                 <dbl>           <dbl>              <dbl>           <dbl>
#>  1               1.5             1.5                1               2.25
#>  2               2               1.25               1               2   
#>  3               0.5             1.5                1.25            1.5 
#>  4               2               1                  2               1.25
#>  5               2.75            0.75               1               1.25
#>  6               0.75            1.5                2.75            1.5 
#>  7               0.75            0                  1.75            1   
#>  8               0.5             0.75               1               2.25
#>  9               2.25            1.75               2               1.5 
#> 10               1               1.25               1.75            1.5 
#> # ℹ 90 more rows
#> # ℹ 26 more variables: pid_anhedonia <dbl>, pid_eccentricity <dbl>,
#> #   pid_hostility <dbl>, pid_riskTaking <dbl>, pid_grandiosity <dbl>,
#> #   pid_perceptualDysregulation <dbl>, pid_separationInsecurity <dbl>,
#> #   pid_deceitfulness <dbl>, pid_perseveration <dbl>,
#> #   pid_attentionSeeking <dbl>, pid_anxiousness <dbl>, pid_depressivity <dbl>,
#> #   pid_withdrawal <dbl>, pid_restrictedAffectivity <dbl>, …

# Brief form (5 domains + the total) with standard errors. `calc_se` is
# deprecated, so this call warns; the PID-5 has no interval function to
# replace it with.
score_pid5(sim_pid5bf, items = 1:25, version = "BF", calc_se = TRUE,
           append = FALSE)
#> Warning: The `calc_se` argument is deprecated.
#> ℹ It, and the `_se` columns it adds, will be removed in a future release.
#> ℹ This package has no interval function for the PID-5; for measurement
#>   precision see `reliability_pid5()`.
#> # A tibble: 100 × 12
#>    pid_disinhibition pid_detachment pid_psychoticism pid_negativeAffectivity
#>                <dbl>          <dbl>            <dbl>                   <dbl>
#>  1               1.8            1.6              2                       1.8
#>  2               2.2            2.2              2.2                     1.4
#>  3               2.4            1.2              1.8                     1.6
#>  4               2.4            2.2              0.8                     0.8
#>  5               2.2            1.2              1.4                     2.8
#>  6               1.8            0.6              2.2                     1.2
#>  7               1              2                1.6                     1.4
#>  8               1.4            1.8              1.2                     1.8
#>  9               1.6            0.8              2.2                     0.8
#> 10               1.2            1.8              1.4                     0.6
#> # ℹ 90 more rows
#> # ℹ 8 more variables: pid_antagonism <dbl>, pid_total <dbl>,
#> #   pid_disinhibition_se <dbl>, pid_detachment_se <dbl>,
#> #   pid_psychoticism_se <dbl>, pid_negativeAffectivity_se <dbl>,
#> #   pid_antagonism_se <dbl>, pid_total_se <dbl>

# PID5BF+M (18 facets + 6 domains). No BF+M dataset ships, but every BF+M
# item is a PID-5 item, so take its 36 items from the full-form data in
# BF+M order.
bfpm_rows <- pid_items[!is.na(pid_items$BFPM), ]
bfpm_rows <- bfpm_rows[order(bfpm_rows$BFPM), ]
sim_bfpm <- sim_pid5[sprintf("pid5_%03d", bfpm_rows$FULL)]
score_pid5(sim_bfpm, items = 1:36, version = "BFPM", append = FALSE)
#> # A tibble: 100 × 24
#>    pid_emotionalLability pid_anxiousness pid_separationInsecurity pid_withdrawal
#>                    <dbl>           <dbl>                    <dbl>          <dbl>
#>  1                   1.5             1.5                      1.5            2  
#>  2                   2               0.5                      0              2.5
#>  3                   1.5             0                        2              1  
#>  4                   1               1                        0.5            2  
#>  5                   1               0.5                      1.5            0.5
#>  6                   1.5             1.5                      2              0.5
#>  7                   2               1.5                      1              1  
#>  8                   2.5             1.5                      1              1  
#>  9                   2               1.5                      3              1.5
#> 10                   1.5             1                        2              2  
#> # ℹ 90 more rows
#> # ℹ 20 more variables: pid_anhedonia <dbl>, pid_intimacyAvoidance <dbl>,
#> #   pid_manipulativeness <dbl>, pid_deceitfulness <dbl>, pid_grandiosity <dbl>,
#> #   pid_irresponsibility <dbl>, pid_impulsivity <dbl>,
#> #   pid_distractibility <dbl>, pid_perfectionism <dbl>, pid_rigidity <dbl>,
#> #   pid_orderliness <dbl>, pid_unusualBeliefsExperiences <dbl>,
#> #   pid_eccentricity <dbl>, pid_perceptualDysregulation <dbl>, …
```
