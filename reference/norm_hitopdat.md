# Convert HiTOP-DAT Scale Scores to T Scores

Converts the scale scores from
[`score_hitopdat()`](https://jmgirard.github.io/hitop/reference/score_hitopdat.md)
to T scores against the norms the battery's authors named, and flags
high IDAS-II Suicidality and CAT-PD Self-Harm T scores. The norms are in
[hitopdat_norms](https://jmgirard.github.io/hitop/reference/hitopdat_norms.md).

## Usage

``` r
norm_hitopdat(data, prefix = "dat_", alert_t = 65, append = TRUE)
```

## Arguments

- data:

  A data frame holding the scale score columns that
  [`score_hitopdat()`](https://jmgirard.github.io/hitop/reference/score_hitopdat.md)
  returns, named `prefix` plus each scale's `camelCase` name in
  [hitopdat_norms](https://jmgirard.github.io/hitop/reference/hitopdat_norms.md).
  Other columns are ignored. Each score column must be numeric or
  logical; any other column, such as text or a bit64 `integer64` column,
  is an error of class `hitop_nonnumeric_items`.

- prefix:

  The prefix of the score column names, and of the columns this function
  adds. Use the `prefix` given to
  [`score_hitopdat()`](https://jmgirard.github.io/hitop/reference/score_hitopdat.md).
  Set it to an empty string `""` when the columns have no prefix.
  (default = `"dat_"`)

- alert_t:

  The T score at or above which an alert column is `TRUE`: one finite
  number. (default = `65`)

- append:

  An optional logical indicating whether the new columns should be added
  to the end of the `data` input. (default = `TRUE`)

## Value

A [tibble](https://tibble.tidyverse.org/reference/tibble.html) with one
`_t` column per scale in
[hitopdat_norms](https://jmgirard.github.io/hitop/reference/hitopdat_norms.md)
order (`prefix` plus the scale's `camelCase` name plus `"_t"`), then the
logical columns `prefix` plus `"suicidality_alert"` and `prefix` plus
`"selfHarm_alert"`, after all original `data` columns (if requested).

## Details

**T scores.** Each scale's T score is `50 + 10 * (score - M) / SD`, with
the scale's `M` and `SD` from
[hitopdat_norms](https://jmgirard.github.io/hitop/reference/hitopdat_norms.md),
rounded to a whole number with halves rounded away from zero. Both of
the authors' versions of the battery round T scores to whole numbers. A
score of `NA` gives a T score of `NA`. The norms and their sources are:

- WHODAS: Andrews et al. (2009), Table 1, the total row and column.

- The 18 IDAS-II scales: Watson et al. (2012), Table 2, the "Adults"
  column.

- CAPE positive symptoms: Galletly et al. (2016), Table 1, the men's and
  women's means weighted by their n, with the SD printed for both,
  divided by the 20 items.

- The 33 CAT-PD facets: the "Community" means and SDs of Len Simms's
  CAT-PD and IDAS scoring workbook. These norms are unpublished. The
  battery's authors use them, and this function follows the authors.

The AUDIT, DUDIT and PHQ-15 get no T score. The AUDIT and DUDIT norms
are split by sex, and how to report them waits on the authors' answer.
The authors keep the PHQ-15 on its cutoff scores rather than a T score.
The IDAS-II General Depression scale is not scored (see
[`score_hitopdat()`](https://jmgirard.github.io/hitop/reference/score_hitopdat.md)).

**Alerts.** The battery alerts on two scales: IDAS-II Suicidality and
CAT-PD Self-Harm. Each alert column is `TRUE` when its scale's rounded T
score is at least `alert_t`, `FALSE` when it is lower, and `NA` when the
T score is `NA`. The default of 65 is the alert point Katherine Jonas,
the battery manual's first author, gave in her email to the package
maintainer of 2026-10-15. She added that clinics may set another, for
example to suit their state's laws.

**Errors.** A score column missing from `data`, or a score column name
held by more than one column of `data`, is an error that names it.

With `append = TRUE`, a column of `data` whose name this call would also
produce is an error rather than an overwrite or a duplicated column: the
message names every colliding column. Re-run with `append = FALSE` to
return only the new columns, or drop the colliding columns from `data`
first. The condition is classed `hitop_append_collision`, so a caller
can catch this refusal by name.

## References

Andrews, G., Kemp, A., Sunderland, M., Von Korff, M., & Ustun, T. B.
(2009). Normative data for the 12 item WHO Disability Assessment
Schedule 2.0. *PLoS ONE, 4*(12), e8343.
[doi:10.1371/journal.pone.0008343](https://doi.org/10.1371/journal.pone.0008343)

Galletly, C., Clark, L., McFarlane, A., Searle, A., Sawyer, M., Sim, M.,
Baghurst, P., & van Hooff, M. (2016). Childhood lead exposure, childhood
trauma, substance use and subclinical psychotic experiences: A
longitudinal cohort study. *Psychiatry Research, 239*, 54-61.
[doi:10.1016/j.psychres.2016.02.066](https://doi.org/10.1016/j.psychres.2016.02.066)

Simms, L. J. (n.d.). *CAT-PD and IDAS scoring* \[Unpublished Excel
workbook\]. Received by the package maintainer from K. G. Jonas,
2026-10-15.

Watson, D., O'Hara, M. W., Naragon-Gainey, K., Koffel, E., Chmielewski,
M., Kotov, R., Stasik, S. M., & Ruggero, C. J. (2012). Development and
validation of new anxiety and bipolar symptom scales for an expanded
version of the IDAS (the IDAS-II). *Assessment, 19*(4), 399-420.
[doi:10.1177/1073191112449857](https://doi.org/10.1177/1073191112449857)

## See also

[`score_hitopdat()`](https://jmgirard.github.io/hitop/reference/score_hitopdat.md),
[hitopdat_norms](https://jmgirard.github.io/hitop/reference/hitopdat_norms.md)

## Examples

``` r
# Five simulated respondents, each answer drawn from its item's answer set
set.seed(1)
answers <- lapply(hitopdat_items$Choice_Set, function(set) {
  values <- hitopdat_choices$Value[hitopdat_choices$Choice_Set == set]
  values[sample.int(length(values), 5, replace = TRUE)]
})
names(answers) <- sprintf("hdat_%03d", hitopdat_items$Item)
scores <- score_hitopdat(as.data.frame(answers), items = names(answers),
                         append = FALSE)
t_scores <- norm_hitopdat(scores, append = FALSE)
t_scores[, c("dat_suicidality_t", "dat_suicidality_alert",
             "dat_selfHarm_t", "dat_selfHarm_alert")]
#> # A tibble: 5 × 4
#>   dat_suicidality_t dat_suicidality_alert dat_selfHarm_t dat_selfHarm_alert
#>               <dbl> <lgl>                          <dbl> <lgl>             
#> 1                83 TRUE                              81 TRUE              
#> 2               130 TRUE                              70 TRUE              
#> 3               104 TRUE                              79 TRUE              
#> 4               100 TRUE                              72 TRUE              
#> 5               109 TRUE                              81 TRUE              

# A clinic that alerts at T 70
norm_hitopdat(scores, alert_t = 70, append = FALSE)$dat_selfHarm_alert
#> [1] TRUE TRUE TRUE TRUE TRUE
```
