# Personality Inventory for DSM-5 Normative Tables

Published normative score distributions for the PID-5, PID-5-SF, and
PID-5-BF, in long form: the raw score and percentile at each T score for
the five domain scales (and the brief form's total score), and the
percentile at each raw score for the validity scales, which are tabled
without T scores.

## Usage

``` r
pid_norms
```

## Format

A [tibble](https://tibble.tidyverse.org/reference/tibble.html) with 1056
rows and 5 columns:

- version:

  The PID-5 version the row norms: `"FULL"`, `"SF"`, or `"BF"`

- scale:

  Name of the scale, as the score-output column stem used by
  [`score_pid5()`](https://jmgirard.github.io/hitop/reference/score_pid5.md)
  and
  [`validity_pid5()`](https://jmgirard.github.io/hitop/reference/validity_pid5.md)
  (i.e., without their `prefix`), so a lookup joins to scored output
  with no crosswalk. Every scale normed here is produced by one of those
  two functions, the brief form's `"total"` included (see
  [`score_pid5()`](https://jmgirard.github.io/hitop/reference/score_pid5.md))

- tscore:

  The T score, or `NA` for the validity scales, whose tables print none

- raw:

  The raw scale score, on the metric
  [`score_pid5()`](https://jmgirard.github.io/hitop/reference/score_pid5.md)
  and
  [`validity_pid5()`](https://jmgirard.github.io/hitop/reference/validity_pid5.md)
  return: for the FULL and SF domains, the mean of the three primary
  facet scores (themselves item means, and the facets differ in length,
  so this is not a mean over the domain's items); for the BF domains and
  total, a mean item response; for the validity scales, an item sum

- percentile:

  The percentile of the normative distribution at that score, as a
  proportion between 0 and 1

## Source

Markon, K. E., Fossati, A., Somma, A., & Krueger, R. F. (2024).
*Understanding the Personality Inventory for DSM-5 (PID-5).* American
Psychiatric Association Publishing. Appendix, Tables A-1 to A-5, A-7,
and A-9 (pp. 113-219).

## Details

The `INC` and `INCS` scales are called the Variable Response
Inconsistency (VRIN) scale by Markon et al. (2024), so a reader coming
from the book will find those tables here under the package's own names.

Norms come from a sample of 1,082 individuals from a U.S. Census-matched
panel. The validity-scale distributions use all 1,082; the FULL and SF
domain distributions use the 995 respondents who scored below 17 on the
inconsistency scale, left no more than a quarter of responses missing,
and did not endorse both infrequency items. The source states no
separate sample size for the brief form tables. All T scores and
percentiles were computed with sampling weights reflecting U.S. Census
data.

The published facet-level and informant-form tables are not included.

## Examples

``` r
pid_norms
#> # A tibble: 1,056 × 5
#>    version scale               tscore   raw percentile
#>    <chr>   <chr>                <int> <dbl>      <dbl>
#>  1 FULL    negativeAffectivity     35  0          0   
#>  2 FULL    negativeAffectivity     36  0          0   
#>  3 FULL    negativeAffectivity     37  0.05       0.02
#>  4 FULL    negativeAffectivity     38  0.11       0.04
#>  5 FULL    negativeAffectivity     39  0.17       0.1 
#>  6 FULL    negativeAffectivity     40  0.23       0.16
#>  7 FULL    negativeAffectivity     41  0.28       0.21
#>  8 FULL    negativeAffectivity     42  0.34       0.25
#>  9 FULL    negativeAffectivity     43  0.4        0.29
#> 10 FULL    negativeAffectivity     44  0.46       0.35
#> # ℹ 1,046 more rows
```
