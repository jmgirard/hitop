# Changelog

## hitop 0.2.0

This release makes several **breaking** API changes to stabilize the
interface before a CRAN submission.

- The scoring vignettes described the `calc_se` standard errors
  incorrectly and now describe what is actually computed. The divisor is
  the number of items a respondent answered, not the number of items on
  the scale. A PID-5 short-form domain score is a mean of three facet
  scores rather than of items, so its standard error is taken over those
  three facet scores. The vignettes also no longer suggest converting
  these standard errors into confidence intervals: they summarize how
  much a respondent’s answers varied within a scale, not how precisely
  the scale measures the trait.

- **Norm-referenced profile plots.** New
  [`plot_pid5()`](https://jmgirard.github.io/hitop/reference/plot_pid5.md)
  draws one respondent’s normed PID-5 scores as a profile against the
  published normative tables — the five domains (plus the brief form’s
  total), or all 25 facets grouped by domain, on a T-score or percentile
  axis. It presents scores against norms and characterizes none of them:
  there are no severity bands, no elevation thresholds, and no
  annotation about what a score means. The score axis spans the range
  the tables actually print, so two profiles on the same version are
  directly comparable. Returns an ordinary ggplot object, which stays in
  Suggests — install {ggplot2} to use it. Worked profile examples for
  all three forms:
  [`vignette("pid5_scoring")`](https://jmgirard.github.io/hitop/articles/pid5_scoring.md),
  [`vignette("pid5sf_scoring")`](https://jmgirard.github.io/hitop/articles/pid5sf_scoring.md),
  and
  [`vignette("pid5bf_scoring")`](https://jmgirard.github.io/hitop/articles/pid5bf_scoring.md).

- [`plot_pid5()`](https://jmgirard.github.io/hitop/reference/plot_pid5.md)
  now places each value label to the right of its point rather than
  above it, and pads the score axis to hold it. Offsetting upward took
  the room out of the panel’s height, where it ran out on a smaller
  figure and the top label in each panel was clipped. The labels fit on
  figures about 7 inches wide or more; see `labels` below for narrower
  ones.

- [`plot_pid5()`](https://jmgirard.github.io/hitop/reference/plot_pid5.md)
  gains a `labels` argument. The value labels need a figure about 7
  inches wide or more; set `labels = FALSE` for a narrower one and the
  points and profile line are drawn without them.

- **Scoring short forms.**
  [`score_hitopsr()`](https://jmgirard.github.io/hitop/reference/score_hitopsr.md)
  and
  [`reliability_hitopsr()`](https://jmgirard.github.io/hitop/reference/reliability_hitopsr.md)
  gain a `subset` argument taking the same short-form description that
  [`hitop_subset()`](https://jmgirard.github.io/hitop/reference/hitop_subset.md)
  builds and the `generate_*_hitopsr()` functions consume. Give it the
  item columns you actually collected and it scores only that short
  form’s scales, returning the same values a full 405-item
  administration would have produced for them. Without the argument both
  functions behave exactly as before. See
  [`vignette("hitopsr_scoring")`](https://jmgirard.github.io/hitop/articles/hitopsr_scoring.md).

- **Clearer errors for bad arguments.** Every argument check across the
  package now reports which argument was wrong, what was supplied, and
  which function was called, instead of printing the internal test that
  failed. This affects `data`, `prefix`, `name`, `append`, `calc_se`,
  `alpha`, `omega`, and `top` throughout the scoring, reliability,
  norming, labelling, and ranking functions. A bad `dir` in
  [`rank_scales()`](https://jmgirard.github.io/hitop/reference/rank_scales.md)
  now lists the permitted values and suggests the closest match. No
  function accepts or rejects anything it did not before — only the
  messages changed.

- **PID-5 normative tables.** The new `pid_norms` dataset carries the
  published normative score distributions for the PID-5, PID-5-SF, and
  PID-5-BF: the raw score and percentile at each T score for the five
  domain scales, for all 25 facet scales of the full and short forms,
  and for the brief form’s total score; and the percentile at each raw
  score for the INC, INC-S, ORS, and PRD validity scales. Scale names
  match the columns
  [`score_pid5()`](https://jmgirard.github.io/hitop/reference/score_pid5.md)
  and
  [`validity_pid5()`](https://jmgirard.github.io/hitop/reference/validity_pid5.md)
  return. Every value comes from Markon et al. (2024) and is verified
  cell by cell against that source. Note that most facet columns print
  raw scores above the 3.00 a mean of 0–3 items can reach, and 19 of
  them repeat a printed 4.00 across several T rows; those rows ship
  exactly as published and are simply unattainable.

- **PID-5 score conversion.** The new
  [`norm_pid5()`](https://jmgirard.github.io/hitop/reference/norm_pid5.md)
  converts scored PID-5, PID-5-SF, and PID-5-BF columns to normative T
  scores and percentiles from `pid_norms`, adding a `_t` column for
  every converted scale whose normative rows carry a T score and a
  `_ptl` column for every converted scale. Every returned value is a
  printed cell of Markon et al. (2024): the nearest printed row is
  selected and nothing is interpolated. Scores outside a printed range
  are capped to the nearest end with a warning rather than extrapolated,
  and scales the tables do not cover return `NA` with a warning naming
  them. Scores collected on any four-option response coding are
  accepted: a coding shifted off the official 0-3 range (1-4, say) is
  reconciled to it before lookup, per scale — item means by the coding’s
  low value, `PRD` by that value times its item count, and `INC`,
  `INC-S`, and `ORS` left alone as coding-invariant — and a warning
  names which scales were adjusted and which were not. A coding implying
  some other number of response options has no mapping onto the
  four-option tables and returns `NA` in every conversion column with a
  warning. Note that
  [`validity_pid5()`](https://jmgirard.github.io/hitop/reference/validity_pid5.md)’s
  published cut scores are still *not* adapted to a shifted coding, so a
  reconciled percentile and an unreconciled validity flag can appear
  together; see
  [`?norm_pid5`](https://jmgirard.github.io/hitop/reference/norm_pid5.md).
  Every report the function makes is a warning condition, so one
  [`suppressWarnings()`](https://rdrr.io/r/base/warning.html) call
  silences it entirely. All 25 facets convert on the full and short
  forms as well as the five domains; on the brief form, and for `SD-TD`
  on any form, the tables carry nothing and the conversion columns come
  back `NA` with the warning above. The PID-5, PID-5-SF, and PID-5-BF
  vignettes each gain a section demonstrating the conversion.

- **PID-5-BF total score** (breaking). `score_pid5(version = "BF")` now
  returns a `total` column after its five domains, so the brief form’s
  normed total score in `pid_norms` has something to convert. Following
  Markon et al. (2024, p. 23), it is the item-level mean over all 25
  items rather than the mean of the five domain means; the two agree on
  complete data and differ only when items are missing. Because each
  scale applies the `missing` rule independently, a total can be
  reported alongside one or more `NA` domains — see
  [`?score_pid5`](https://jmgirard.github.io/hitop/reference/score_pid5.md)
  for the exact bounds. Two consequences for existing code:
  `reliability_pid5(version = "BF")` now returns **six** rows rather
  than five, and the printed scoring table on the PID-5-BF Word forms
  gains a `Total` row listing all 25 items (both forms are rebuilt, with
  new `hitop_artifacts` entries). Code that counts the columns of
  `score_pid5(version = "BF")` or the rows of
  `reliability_pid5(version = "BF")` must be updated. The PID-5 and
  PID-5-SF are unaffected.

- **PID-5 Word forms print the response options on two lines.** The
  response scale printed above the items on the PID-5, PID-5-SF, and
  PID-5-BF Word forms now runs across two lines — `0` and `1` on the
  first, `2` and `3` on the second — so that no option phrase is broken
  partway through by the column width. The option values and wording are
  unchanged, and the HiTOP-SR and HiTOP-BR forms keep their single-line
  scale. All six PID Word files (US and A4) were regenerated, with new
  `hitop_artifacts` entries.

- **Generate a shortened HiTOP-SR from selected scales.** The new
  [`hitop_subset()`](https://jmgirard.github.io/hitop/reference/hitop_subset.md)
  describes a subset of an instrument’s scales, and
  [`generate_docx_hitopsr()`](https://jmgirard.github.io/hitop/reference/generate_docx_hitopsr.md),
  [`generate_qualtrics_hitopsr()`](https://jmgirard.github.io/hitop/reference/generate_qualtrics_hitopsr.md),
  and
  [`generate_redcap_hitopsr()`](https://jmgirard.github.io/hitop/reference/generate_redcap_hitopsr.md)
  each take it as a `subset` argument to emit a form containing only
  those scales’ items. Item numbers are **not** renumbered: each item
  keeps its original HiTOP-SR number, so data collected with the
  shortened form still maps onto the full instrument’s scoring key.
  Scale names may be given as printed on the instrument
  (`"Antisocial Behavior"`) or as the camelCase stems used in scored
  output (`"antisocialBehavior"`), in any mixture and ignoring case.
  Subsetting is currently available for the HiTOP-SR only.

- [`norm_pid5()`](https://jmgirard.github.io/hitop/reference/norm_pid5.md)
  now checks its `scores` argument before converting anything. Naming
  the same score column twice is an error rather than a silently
  duplicated pair of output columns, and a factor or character score
  column is an error rather than being coerced — a factor’s integer
  codes are not its scores, and a character column coerces to `NA`.
  Logical columns still convert. Every complaint about the argument
  names `scores`, not the `items` or `scales` of the shared validators
  behind it. That error now gives each offending column its own line
  with its full class (an ordered factor reads as `<ordered/factor>`
  rather than as `ordered`), and errors raised while reconciling a
  shifted response coding are attributed to
  [`norm_pid5()`](https://jmgirard.github.io/hitop/reference/norm_pid5.md)
  rather than to the internal helper that raised them.

- [`rank_scales()`](https://jmgirard.github.io/hitop/reference/rank_scales.md)’s
  `prefix` argument is now matched **literally** (breaking). It was
  previously compiled as a regular expression anchored to the start of
  the column name, which meant a prefix containing `(` failed with a
  regex error and one containing `.` could strip a prefix that was never
  there. A column name that does not begin with exactly `prefix` is now
  carried through whole. Code relying on a regex `prefix` must pre-strip
  the names instead.
  [`norm_pid5()`](https://jmgirard.github.io/hitop/reference/norm_pid5.md)
  matches `prefix` the same way.

- Qualtrics question IDs are now zero-padded to the width of the largest
  item number rather than the number of items. Output for every full
  instrument is unchanged; the change keeps IDs uniform in a subset
  file.

- **New instrument overview page.** A single “HiTOP Instruments” page
  presents the three self-report measures — HiTOP-SR, HiTOP-BR, and
  HiTOP-HSUM — as at-a-glance summary cards, each linking to its full
  download page. It is the first entry in the website’s “Instruments”
  menu. Its HiTOP-BR card now describes the eight scales at their true
  hierarchy levels — six spectra plus the Externalizing superspectrum
  and a general p-factor — rather than calling all eight “spectra”.

- **Redesigned instrument download pages.** Each download button on the
  website’s instrument pages now shows its file’s build date, and the
  version tables are replaced by a collapsible “Current builds & version
  history” panel rendered from the `hitop_artifacts` manifest. The
  manifest’s change notes were reworded for a general audience (data
  unchanged otherwise).

- **Centralized import instructions.** A new “Importing into Qualtrics &
  REDCap” article gives step-by-step instructions for all three import
  formats — Qualtrics survey files (`.qsf`), Qualtrics questions files
  (`.txt`), and REDCap instrument ZIPs — and every instrument download
  page now links its Qualtrics and REDCap cards to it. The REDCap import
  steps previously embedded in each `generate_redcap_*()` help page now
  live in that article, which the functions point to via “See also”.

- **Distribution artifacts are now versioned.** The new
  `hitop_artifacts` manifest dataset identifies every prebuilt file in
  `inst/extdata/` by build date and MD5 checksum (one row per build,
  history kept); the website’s download pages show each instrument’s
  current builds and a version history; and generated Word documents
  carry a build stamp in the footer (“Generated YYYY-MM-DD · hitop
  X.Y.Z”). A test suite locks the committed files to the manifest, so no
  distributed artifact can change again without a visible version bump.
  **Artifact filenames no longer carry the instrument version** (e.g.,
  `pid5_1.0_A4.docx` is now `pid5_A4.docx`, so previously shared
  download URLs no longer resolve), and the `generate_docx_*` default
  `file` arguments dropped `_1.0` accordingly

- **HiTOP-HSUM aligned to its authoritative source** (the HiTOP
  Society’s “revised SUD module-August 2024” development worksheet):
  `hitophsum_items` item text now matches the worksheet’s
  substance-specific wording (alcohol items use drink-specific phrasing;
  nicotine and other-drug items corrected; obvious worksheet typos
  repaired and logged), the free-text nicotine quantity item now shows
  only for non-cigarette, non-cigar forms, and `hitophsum_choices` gains
  the alcohol/cigarette/cigar quantity choice sets. In the REDCap
  export, the cigar quantity item is now a valid dropdown (it previously
  imported with an empty choice list), and “Prefer not to say” frequency
  responses no longer satisfy any symptom gate. **New `other_drug_rule`
  argument** on
  [`generate_redcap_hitophsum()`](https://jmgirard.github.io/hitop/reference/generate_redcap_hitophsum.md):
  the default (`"most_frequent"`) follows the worksheet’s looping rule —
  symptom items appear only for the most frequently used other drug used
  at least monthly (ties show all tied drugs) — while `"per_drug"`
  reproduces the previous looser behavior of gating every other drug
  independently. The overview DOCX now says “Street opioids” (previously
  “Heroin/opiates”) and “Goose bumps”, and its item matrix matches the
  corrected wording; the prebuilt DOCX and REDCap files were regenerated

- **The HiTOP-HSUM Qualtrics import file was rebuilt** from the
  corrected item data. The previous file predated the source alignment
  above and also contained an empty cigar-quantity dropdown and a
  duplicate copy of every question. Note one platform difference:
  Qualtrics display logic cannot compare answers across questions, so
  the Qualtrics survey shows symptom items for *every* other drug used
  at least monthly (the source module’s sanctioned loosening), whereas
  the REDCap export defaults to the most-frequently-used other drug only

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

- **Standardized item-text punctuation** in `hitopsr_items` (7 items)
  and `hitopbr_items` (1 item): every item now ends in a period. The
  affected items (HSR 5, 27, 30, 284, 314, 332, 382 and HBR 41) lack the
  period in the source instrument itself, where 398 of 405 HiTOP-SR
  items have one; the omissions are treated as typographical oversights.
  The derived `*_scales`/`*_subscales` tables and the prebuilt
  DOCX/Qualtrics/REDCap artifacts in `inst/extdata/` were regenerated to
  match

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
