# Changelog

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

## hitop 0.0.2

- Add initial HiTOP-SR and BR functions

## hitop 0.0.1

- Add initial PID-5 functions
