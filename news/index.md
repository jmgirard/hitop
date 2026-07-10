# Changelog

## hitop 0.2.0

This release makes several **breaking** API changes to stabilize the
interface before a CRAN submission.

- **New
  [`reliability_pid5()`](https://jmgirard.github.io/hitop/reference/reliability_pid5.md),
  [`reliability_hitopsr()`](https://jmgirard.github.io/hitop/reference/reliability_hitopsr.md),
  and
  [`reliability_hitopbr()`](https://jmgirard.github.io/hitop/reference/reliability_hitopbr.md)
  functions** return a per-scale tibble (`scale`, `nItems`, `alpha`,
  `omega`). These replace the `alpha` and `omega` arguments of
  [`score_pid5()`](https://jmgirard.github.io/hitop/reference/score_pid5.md),
  [`score_hitopsr()`](https://jmgirard.github.io/hitop/reference/score_hitopsr.md),
  and
  [`score_hitopbr()`](https://jmgirard.github.io/hitop/reference/score_hitopbr.md),
  which only *printed* a reliability table as a side effect and have
  been **removed**
- **[`score_pid5()`](https://jmgirard.github.io/hitop/reference/score_pid5.md),
  [`score_hitopsr()`](https://jmgirard.github.io/hitop/reference/score_hitopsr.md),
  and
  [`score_hitopbr()`](https://jmgirard.github.io/hitop/reference/score_hitopbr.md)
  now take a single `missing` argument** in place of the previous
  `na.rm` (and, for
  [`score_pid5()`](https://jmgirard.github.io/hitop/reference/score_pid5.md),
  `apa_scoring`) arguments. For
  [`score_pid5()`](https://jmgirard.github.io/hitop/reference/score_pid5.md),
  `missing = "apa"` (the default) applies the APA missing-data/proration
  rule, `"available"` averages the present items (the old
  `apa_scoring = FALSE, na.rm = TRUE`), and `"complete"` returns `NA`
  for any scale with a missing item (the old `na.rm = FALSE`).
  [`score_hitopsr()`](https://jmgirard.github.io/hitop/reference/score_hitopsr.md)/[`score_hitopbr()`](https://jmgirard.github.io/hitop/reference/score_hitopbr.md)
  offer `"available"` (default) and `"complete"`. Default behavior is
  unchanged
- **[`rank_scales()`](https://jmgirard.github.io/hitop/reference/rank_scales.md)
  gains a `name` argument** (default `"top_scales"`) naming its output
  column, which was previously hard-coded as `"out"`. It also gains
  `reverse` and `srange` arguments: scales named in `reverse` are
  reflected via `sum(srange) - value` before ranking, so a
  reverse-directioned scale (e.g. a well-being scale, where higher =
  healthier) ranks on the same “higher = more elevated” metric as the
  other scales
- **The `tibble` argument has been removed** from
  [`score_pid5()`](https://jmgirard.github.io/hitop/reference/score_pid5.md),
  [`score_hitopsr()`](https://jmgirard.github.io/hitop/reference/score_hitopsr.md),
  [`score_hitopbr()`](https://jmgirard.github.io/hitop/reference/score_hitopbr.md),
  [`validity_pid5()`](https://jmgirard.github.io/hitop/reference/validity_pid5.md),
  and
  [`rank_scales()`](https://jmgirard.github.io/hitop/reference/rank_scales.md);
  these functions now always return a tibble
- **Documentation accuracy and polish** across the scoring tutorials and
  pkgdown instrument pages: corrected stale column/dataset names in the
  HiTOP-SR tutorial (leftovers from an earlier “HiTOP-PRO” draft),
  updated the HiTOP-BR scale count (8, not 7) and the PID-5
  appended-column count (now includes the 5 domains), finished the
  previously “work in progress” PID-5-BF tutorial, added the missing
  Scale Reliability sections to the HiTOP-BR and PID-5-BF tutorials,
  fixed a mis-targeted REDCap “Import Instructions” link on the PID-5
  download page, and reconciled the instrument download pages so each
  describes only the resources it actually links

## hitop 0.1.0

- Add initial HiTOP-HSUM functions
- Add data export functions
- Build out phase 1 website
- [`score_pid5()`](https://jmgirard.github.io/hitop/reference/score_pid5.md)
  now returns the 5 personality-trait domain scores for the FULL and SF
  versions (APA scoring key Step 3), appended after the 25 facet scores
- Add the `pid_domains` dataset (the domain to primary-facet map used
  for FULL/SF domain scoring)
- [`score_pid5()`](https://jmgirard.github.io/hitop/reference/score_pid5.md)
  gains an `apa_scoring` argument (default `TRUE`) that applies the
  published APA missing-data and proration rule: a facet (or BF domain)
  with more than 25% of its items unanswered is set to `NA`; otherwise
  the raw score is prorated to the full item count and rounded before
  averaging, and a FULL/SF domain is `NA` if any contributing facet is
  `NA`. **This changes the default scored output under missing data**
  (previously `rowMeans(na.rm = TRUE)` averaged whatever items were
  present). Pass `apa_scoring = FALSE` to restore the previous behavior.
  Under `apa_scoring = TRUE`, `na.rm` is ignored (with a warning if set
  to `FALSE`), and any standard error is `NA` wherever its scale score
  is `NA`
- Fix
  [`validity_pid5()`](https://jmgirard.github.io/hitop/reference/validity_pid5.md)
  erroring on single-row input for the FULL and SF forms
- Fix `score_pid5(calc_se = TRUE)` erroring on single-row input
- Add tests for the `generate_docx_*`, `generate_qualtrics_*`, and
  `generate_redcap_*` export families, verifying each generated file
  against the source instrument datasets (including the HiTOP-HSUM
  REDCap branching logic)
- [`score_pid5()`](https://jmgirard.github.io/hitop/reference/score_pid5.md),
  [`score_hitopsr()`](https://jmgirard.github.io/hitop/reference/score_hitopsr.md),
  [`score_hitopbr()`](https://jmgirard.github.io/hitop/reference/score_hitopbr.md),
  and
  [`validity_pid5()`](https://jmgirard.github.io/hitop/reference/validity_pid5.md)
  now guard against two ways a bad `items` mapping silently produces
  wrong scores: they error on duplicated `items` entries and warn when
  `items` column names share a common prefix and trailing number but
  those numbers are not in ascending (instrument) order
- [`validity_pid5()`](https://jmgirard.github.io/hitop/reference/validity_pid5.md)
  now warns when `srange` is not `c(0, 3)`, because the published PRD
  and SD-TD cut scores are raw sums against fixed thresholds that assume
  0-3 item coding and do not adapt to other codings
- Add runnable `@examples` to every exported function
- Correct the dataset documentation: fix the column counts in the
  `pid_items` and `hitopbr_items` `@format` blocks, document the
  `pid_scales` format, and fix the `sim_hitopbr` item-column names
  (`hitopbr_1` to `hitopbr_45`)
- Improve the package Title and Description
- [`score_hitopbr()`](https://jmgirard.github.io/hitop/reference/score_hitopbr.md)
  gains `alpha` and `omega` arguments (default `FALSE`) that print a
  per-scale reliability summary, matching
  [`score_pid5()`](https://jmgirard.github.io/hitop/reference/score_pid5.md)
  and
  [`score_hitopsr()`](https://jmgirard.github.io/hitop/reference/score_hitopsr.md)
- Internal refactor:
  [`score_pid5()`](https://jmgirard.github.io/hitop/reference/score_pid5.md),
  [`score_hitopsr()`](https://jmgirard.github.io/hitop/reference/score_hitopsr.md),
  and
  [`score_hitopbr()`](https://jmgirard.github.io/hitop/reference/score_hitopbr.md)
  now share a single internal scoring engine instead of three
  hand-maintained copies of the same pipeline (no change to scored
  output)
- Clearer input errors: `items` of the wrong length now reports the
  expected count and what was supplied, and supplying `items` names or
  positions that are not columns of `data` now raises an actionable
  error (naming the offenders) instead of a cryptic base-R subscript
  error
- Input-validation errors from the scoring, validity, reliability, and
  [`rank_scales()`](https://jmgirard.github.io/hitop/reference/rank_scales.md)
  functions are now attributed to the function you called rather than to
  an internal helper

## hitop 0.0.2

- Add initial HiTOP-SR and BR functions

## hitop 0.0.1

- Add initial PID-5 functions
