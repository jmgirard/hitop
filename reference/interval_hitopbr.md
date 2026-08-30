# Confidence intervals for HiTOP-BR scale scores

Converts scored HiTOP-BR columns into a regression-based true-score
estimate and a confidence interval around it, using the
development-sample mean, standard deviation and reliability shipped as
[hitopbr_devstats](https://jmgirard.github.io/hitop/reference/hitopbr_devstats.md).
This function converts already-scored columns and never rescores: the 8
scale columns
[`score_hitopbr()`](https://jmgirard.github.io/hitop/reference/score_hitopbr.md)
produces are the ones it is built for, and
[hitopbr_devstats](https://jmgirard.github.io/hitop/reference/hitopbr_devstats.md)
carries a row for each of them.

## Usage

``` r
interval_hitopbr(
  data,
  scores,
  srange = c(1, 4),
  prefix = "hbr_",
  level = 0.95,
  append = TRUE
)
```

## Arguments

- data:

  A data frame containing scored HiTOP-BR columns.

- scores:

  The score columns to convert, as column names or column positions
  (mirroring the `items` argument of
  [`score_hitopbr()`](https://jmgirard.github.io/hitop/reference/score_hitopbr.md)).
  Each column must be numeric (or logical) and each may be named only
  once.

- srange:

  The response range the items were scored on, as `c(low, high)`. The
  reference statistics are printed on the official four-option `c(1, 4)`
  coding; any other coding is a different metric. See Details.

- prefix:

  The prefix
  [`score_hitopbr()`](https://jmgirard.github.io/hitop/reference/score_hitopbr.md)
  applied to its output columns, used to match a score column back to
  its scale. Matched literally, not as a regular expression: a column
  name that does not begin with exactly this string keeps its whole name
  and is reported as uncovered. Pass `""` when the columns are named for
  the scales themselves, with no prefix to strip.

- level:

  The confidence level, as a proportion between 0 and 1. Defaults to
  `0.95`.

- append:

  Whether to return the input `data` with the interval columns appended
  (`TRUE`, the default) or the interval columns alone.

## Value

A [tibble](https://tibble.tidyverse.org/reference/tibble.html) with an
`_est`, `_lo` and `_hi` column for every requested score column,
alongside all original `data` columns if requested. An `NA` score
returns `NA` in all three.

## Details

**What is computed.** For a score \\x\\ on a scale whose reference group
has mean \\M\\, standard deviation \\SD\\ and reliability \\r\\, the
estimate and bounds are Schmukle's (2026) Equations (10) to (12), p.
821:

\$\$est = M + \sqrt{r}\\(x - M)\$\$ \$\$SEM = SD \sqrt{1 - r}\$\$
\$\$lo,\\ hi = est \pm z\\SEM\$\$

where \\z\\ is the two-sided standard normal quantile for `level`
(1.959964 at the default 0.95). The estimate is the observed score
pulled toward the reference mean, because with imperfect measurement a
true score tends to lie nearer the mean than the observed score does;
the \\\sqrt{r}\\ factor is Schmukle's scale correction, which returns
the estimate to the metric the observed score is on so the two can be
read against each other.

**The reference group is a development sample.**
[hitopbr_devstats](https://jmgirard.github.io/hitop/reference/hitopbr_devstats.md)
carries the statistics printed for the HiTOP-SR introduction paper's
Development Sample 2: N = 780 Prolific Academic participants stratified
by sex and age to approximate a community-representative United States
population. That is a development sample and not a community norm – no
census weighting was applied and no raw-score to T-score table is
published – so an interval from this function says where a score sits
relative to the sample the instrument was developed on, and not what
percentile it occupies in any population.

**Two limitations worth stating.** Equation (12)'s interval is symmetric
about the estimate and the same width for every respondent on a given
scale, which is what classical test theory implies and what the source
computes. On a floor-heavy scale that width can put a bound outside the
response range, and the bounds are **not** clamped, because clamping
would report something other than the equation this function cites.
Every HiTOP-BR scale is floor-heavy enough for this to bite: on all
eight, a score of 1 – the response floor – returns a lower bound
below 1. And the coverage Schmukle demonstrates is *marginal*: over a
population of respondents drawn from the reference distribution, about
`level` of the intervals contain the true score. It is established under
a linear measurement model with approximately normal item responses, so
it is not a guarantee for any one respondent, nor a demonstration on
scales as skewed as these.

**The scales overlap.** Externalizing and p-Factor are drawn from the
same items as the six spectrum scales rather than added to them, so a
respondent contributes the same answers to several of these intervals.
Read them as eight views of one response set, not as eight independent
measurements.

**Reporting and silence.** Both things this function reports are warning
conditions, so a single
[`suppressWarnings()`](https://rdrr.io/r/base/warning.html) call
silences it and either report can still be caught and tested for on its
own.

- A score column with no
  [hitopbr_devstats](https://jmgirard.github.io/hitop/reference/hitopbr_devstats.md)
  row returns `NA` in all three columns, with a warning of class
  `hitop_interval_uncovered` naming the columns. A column is matched by
  stripping `prefix` and looking the rest up in
  `hitopbr_devstats$camelCase`, so this fires for a mistyped prefix as
  well as for a column that is not a HiTOP-BR scale score.

- A call whose `srange` is not the `c(1, 4)` coding the reference mean
  and standard deviation are printed on returns `NA` in every interval
  column, with a warning of class `hitop_interval_coding`. Nothing is
  reconciled: a shift or a stretch of the response range moves a scale
  score, and no mapping from another coding onto these statistics is
  published.

**Partly scored scales are not detected.** A score computed from fewer
than a scale's full items – from data with items missing under
`missing = "available"` – is not on the same footing as the reference
statistics, which come from complete scales. This function receives
scores and not items, so it cannot tell such a column from a fully
scored one and does not try: it converts what it is given. Treat an
interval on a partly scored scale as not comparable to the reference
group.

**Errors.** `scores` is checked before anything is converted. Naming the
same score column twice is an error rather than a duplicated set of
output columns, and a factor or character score column is an error
rather than a silent coercion – a factor's integer codes are not its
scores, and a character column would coerce to `NA`. Logical columns are
accepted.

A `scores` argument that names no columns is an error, reported ahead of
the other selection arguments, so the cause named is the empty selection
and not a consequence of it. The condition is classed
`hitop_empty_selection`, so a caller can catch this refusal by name.

With `append = TRUE`, a column of `data` whose name this call would also
produce is an error rather than an overwrite or a duplicated column: the
message names every colliding column. Re-run with `append = FALSE` to
return only the new columns, or drop the colliding columns from `data`
first. The condition is classed `hitop_append_collision`, so a caller
can catch this refusal by name.

## References

Schmukle, S. C. (2026). Unbiased confidence intervals for psychological
testing: The regression-based true score approach with scale correction.
*Assessment, 33*(5), 817-825. Equations (10) to (12), p. 821, are what
this function computes; Box 1, p. 823, works two examples.

## Examples

``` r
# Score the HiTOP-BR, then put an interval around two scales
scored <- score_hitopbr(sim_hitopbr, items = 1:45)
interval_hitopbr(
  scored,
  scores = c("hbr_detachment", "hbr_pFactor"),
  append = FALSE
)
#> # A tibble: 100 × 6
#>    hbr_detachment_est hbr_detachment_lo hbr_detachment_hi hbr_pFactor_est
#>                 <dbl>             <dbl>             <dbl>           <dbl>
#>  1               2.75             2.11               3.40            2.36
#>  2               1.64             0.993              2.28            1.98
#>  3               2.75             2.11               3.40            2.29
#>  4               2.57             1.92               3.21            1.90
#>  5               2.19             1.55               2.84            2.52
#>  6               2.38             1.74               3.03            2.59
#>  7               2.38             1.74               3.03            2.44
#>  8               2.38             1.74               3.03            2.67
#>  9               2.57             1.92               3.21            2.21
#> 10               2.19             1.55               2.84            2.67
#> # ℹ 90 more rows
#> # ℹ 2 more variables: hbr_pFactor_lo <dbl>, hbr_pFactor_hi <dbl>

# A 90% interval is narrower
interval_hitopbr(
  scored,
  scores = "hbr_detachment",
  level = 0.90,
  append = FALSE
)
#> # A tibble: 100 × 3
#>    hbr_detachment_est hbr_detachment_lo hbr_detachment_hi
#>                 <dbl>             <dbl>             <dbl>
#>  1               2.75              2.21              3.29
#>  2               1.64              1.10              2.18
#>  3               2.75              2.21              3.29
#>  4               2.57              2.02              3.11
#>  5               2.19              1.65              2.74
#>  6               2.38              1.84              2.92
#>  7               2.38              1.84              2.92
#>  8               2.38              1.84              2.92
#>  9               2.57              2.02              3.11
#> 10               2.19              1.65              2.74
#> # ℹ 90 more rows
```
