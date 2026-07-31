# Convert PID-5 scale scores to normative T scores and percentiles

Looks up scored PID-5 columns in the published normative tables shipped
as [pid_norms](https://jmgirard.github.io/hitop/reference/pid_norms.md)
and returns a T score and a percentile for each. Scores are produced by
[`score_pid5()`](https://jmgirard.github.io/hitop/reference/score_pid5.md)
and
[`validity_pid5()`](https://jmgirard.github.io/hitop/reference/validity_pid5.md);
this function converts them and never rescores.

## Usage

``` r
norm_pid5(
  data,
  scores,
  version = c("FULL", "SF", "BF"),
  srange = c(0, 3),
  prefix = "pid_",
  append = TRUE
)
```

## Arguments

- data:

  A data frame containing scored PID-5 columns.

- scores:

  The score columns to convert, as column names or column positions
  (mirroring the `items` argument of
  [`score_pid5()`](https://jmgirard.github.io/hitop/reference/score_pid5.md)).
  Each column must be numeric (or logical) and each may be named only
  once.

- version:

  Which PID-5 version the scores came from: `"FULL"` (220 items), `"SF"`
  (100 items), or `"BF"` (25 items). The normative tables differ by
  version.

- srange:

  The response range the items were coded on, as `c(low, high)`. Any
  four-option coding is accepted and reconciled to the official
  `c(0, 3)` range before lookup; a coding with a different number of
  options is not convertible. See Details.

- prefix:

  The prefix
  [`score_pid5()`](https://jmgirard.github.io/hitop/reference/score_pid5.md)
  applied to its output columns, used to match a score column back to
  its scale. Matched literally, not as a regular expression: a column
  name that does not begin with exactly this string keeps its whole name
  and is reported as uncovered.

- append:

  Whether to return the input `data` with the conversion columns
  appended (`TRUE`, the default) or the conversion columns alone.

## Value

A [tibble](https://tibble.tidyverse.org/reference/tibble.html) with a
`_t` column for every converted scale whose normative rows carry a T
score (the five domains, plus the brief form's total) and a `_ptl`
column for every converted scale, alongside all original `data` columns
if requested. The four validity scales (`INC`, `INCS`, `ORS`, `PRD`) are
distributed as percentiles only and get no `_t` column.

## Details

For each named score column the tables are searched for the printed row
whose raw score is **nearest** the observed value, and that one row's
printed T score and printed percentile are returned. Nothing is
interpolated: every returned number is a cell of Markon et al. (2024).

Markon et al. print the tables but give no instruction for reading them
— no rounding, interpolation, or tie rule appears anywhere in the book —
so the rules below are this package's, chosen and defended rather than
quoted. They were settled by an independent review of the tables
recorded in the package's development history as report RR02 (in the
project repository, not the installed package).

- **Between printed rows.** The nearer row wins. Printed raws step by
  0.01-0.07 while attainable scores fall on much coarser grids (a 5-item
  brief-form domain mean can only be a multiple of 0.2), so most lookups
  land between rows.

- **Ties.** Where two or more rows are equally near – a raw printed in
  several rows, or a value exactly midway between two rows – the row
  whose T score is nearest 50 is returned. The four validity scales
  carry no T score, so a tie there returns the row whose percentile is
  nearest 0.50.

- **Scores of 0.** Each domain table prints raw 0.00 across a run of low
  T scores, because the linear T the book tabulated predicts a negative
  raw there and 0.00 is printed instead. The tie rule returns the run's
  highest T, the one row of the run that renders an attainable score.
  Its printed percentile is positive on some scales and 0.00 on others;
  that asymmetry is a property of the published tables, not of this
  function.

- **Scores outside the table.** A score above the highest printed row
  returns that row's values, rather than an extrapolation. A score below
  the lowest returns whatever an observation *at* the lowest printed raw
  returns – which, on the scales whose tables print a run of 0.00, is
  that run's highest-T row and not the table's first row, so the two
  agree instead of jumping. A warning reports how many observations were
  capped at each end. This is reachable in ordinary data: `PRD` is a
  22-item sum reaching 66 while its table stops at 55.

- **Unattainable printed rows.** Five domain tables print rows above the
  3.00 ceiling a 0-3 item mean can reach, so the top of those T ranges
  cannot be attained. A maximum score returns T = 84 (brief-form
  negative affectivity), 87 (brief-form detachment), 93 (brief-form
  disinhibition), 87 (full-form negative affectivity), or 85 (short-form
  negative affectivity) – each at percentile 1.00. Nothing is wrong with
  such data and nothing is reported.

- **Comparison tolerance.** All comparisons use an absolute tolerance of
  1e-8, so that scores on grids with no exact binary representation (a
  short-form domain mean is a twelfth) match the printed 2-decimal raws
  as intended.

Columns the tables do not cover for the requested `version` – the 25
facets, for instance – return `NA` in both conversion columns with a
warning naming them. An `NA` score returns `NA`.

**Reporting and silence.** Everything this function reports – the
capping count above, the uncovered-column warning, and the two
response-coding reports below – is a warning condition, so a single
[`suppressWarnings()`](https://rdrr.io/r/base/warning.html) call
silences the function and any one report can still be caught and tested
for individually.

**Errors.** `scores` is checked before anything is converted. Naming the
same score column twice is an error rather than a duplicated pair of
output columns, and a factor or character score column is an error
rather than a silent coercion – a factor's integer codes are not its
scores, and a character column would coerce to `NA`. Logical columns are
accepted, since a 0/1 indicator converts as it reads.

**Response coding.** The normative tables are built on the official
four-option 0-3 coding. Data collected on a four-option coding that
merely starts elsewhere – 1-4, say – carries the same information, so
each score is reconciled to the official range before lookup and the
conversion proceeds. A coding with a different *number* of options is a
different metric: no mapping onto a four-option norm table is defined,
so every conversion column is returned as `NA` with a warning, and the
items must be recoded and rescored.

How much a shift moves a score depends on how the scale is computed, so
the reconciliation is applied per scale rather than per item. No
published source states these rules – Markon et al. give the tables for
the official coding only – so, like the reading rules above, they are
this package's, derived from each scale's own definition:

- **Item means** (the five domains, and the brief form's total) are
  reconciled by subtracting `srange[[1]]`. Shifting every item by a
  constant shifts their mean by the same constant.

- **`PRD`** is a plain sum over its 22 items, so the same shift moves it
  by `srange[[1]]` times the number of items, which is what is
  subtracted. The item count is read from
  [pid_items](https://jmgirard.github.io/hitop/reference/pid_items.md)
  rather than assumed.

- **`INC` and `INC-S`** are sums of *absolute differences within item
  pairs*. A constant added to both members of a pair cancels in the
  difference, so these are unchanged by a shift and nothing is
  subtracted.

- **`ORS`** is a count of items answered at the top of the response
  range –
  [`validity_pid5()`](https://jmgirard.github.io/hitop/reference/validity_pid5.md)
  computes it by comparing each item to `srange[[2]]` rather than to a
  fixed value (`R/validity_pid5.R:153` in the package sources). A shift
  moves the top of the range along with the answers, so the same items
  are counted and the score is unchanged.

A shifted coding raises one warning per call naming which of the
requested scales were adjusted and which were left alone; where every
requested scale turns out to be coding-invariant, it says so rather than
claiming an adjustment. The warning covers the scales the tables
actually carry, so a request the tables cover nowhere raises the
coverage warning above instead and nothing about coding. The official
coding is silent.

One consequence is worth stating plainly, because it can put two
differently grounded numbers side by side in the same session:
[`validity_pid5()`](https://jmgirard.github.io/hitop/reference/validity_pid5.md)'s
published cut scores are **not** reconciled to a shifted coding. `PRD`
and `SD-TD` are compared against fixed thresholds that assume 0-3 items,
and
[`validity_pid5()`](https://jmgirard.github.io/hitop/reference/validity_pid5.md)
warns rather than adapting them. So a respondent scored on a 1-4 coding
can receive a reconciled percentile from this function and, from
[`validity_pid5()`](https://jmgirard.github.io/hitop/reference/validity_pid5.md),
a validity flag still read against the 0-3 thresholds. Adapting those
cut scores is a separate, deliberately deferred question.

## References

Markon, K. E., Fossati, A., Somma, A., & Krueger, R. F. (2024).
*Understanding the Personality Inventory for DSM-5 (PID-5).* American
Psychiatric Association Publishing. The normative tables in
[pid_norms](https://jmgirard.github.io/hitop/reference/pid_norms.md),
Appendix "Normative Score Distributions" (pp. 113-219), are the source
for every value this function returns.

## Examples

``` r
# Score the brief form, then convert its domains and total
scored <- score_pid5(sim_pid5bf, items = 1:25, version = "BF")
norm_pid5(scored, scores = paste0("pid_", c("detachment", "total")),
          version = "BF", append = FALSE)
#> # A tibble: 100 × 4
#>    pid_detachment_t pid_detachment_ptl pid_total_t pid_total_ptl
#>               <int>              <dbl>       <int>         <dbl>
#>  1               65               0.92          76          0.98
#>  2               74               0.98          77          0.98
#>  3               58               0.77          78          0.99
#>  4               74               0.98          71          0.95
#>  5               58               0.77          80          0.99
#>  6               49               0.59          69          0.94
#>  7               71               0.96          69          0.94
#>  8               68               0.96          72          0.95
#>  9               52               0.59          72          0.95
#> 10               68               0.96          66          0.93
#> # ℹ 90 more rows
```
