# hitop 0.2.0

This release makes several **breaking** API changes to stabilize the interface
before a CRAN submission.

* **HiTOP-HSUM aligned to its authoritative source** (the HiTOP Society's
  "revised SUD module-August 2024" development worksheet):
  `hitophsum_items` item text now matches the worksheet's substance-specific
  wording (alcohol items use drink-specific phrasing; nicotine and other-drug
  items corrected; obvious worksheet typos repaired and logged), the
  free-text nicotine quantity item now shows only for non-cigarette,
  non-cigar forms, and `hitophsum_choices` gains the alcohol/cigarette/cigar
  quantity choice sets. In the REDCap export, the cigar quantity item is now
  a valid dropdown (it previously imported with an empty choice list), and
  "Prefer not to say" frequency responses no longer satisfy any symptom
  gate. **New `other_drug_rule` argument** on `generate_redcap_hitophsum()`:
  the default (`"most_frequent"`) follows the worksheet's looping rule —
  symptom items appear only for the most frequently used other drug used at
  least monthly (ties show all tied drugs) — while `"per_drug"` reproduces
  the previous looser behavior of gating every other drug independently.
  The overview DOCX now says "Street opioids" (previously "Heroin/opiates")
  and "Goose bumps", and its item matrix matches the corrected wording; the
  prebuilt DOCX and REDCap files were regenerated

* **New `reliability_pid5()`, `reliability_hitopsr()`, and `reliability_hitopbr()`
  functions** return a per-scale tibble (`scale`, `nItems`, `alpha`, `omega`).
  These replace the `alpha` and `omega` arguments of `score_pid5()`,
  `score_hitopsr()`, and `score_hitopbr()`, which only *printed* a reliability
  table as a side effect and have been **removed**
* **`score_pid5()`, `score_hitopsr()`, and `score_hitopbr()` now take a single
  `missing` argument** in place of the previous `na.rm` (and, for `score_pid5()`,
  `apa_scoring`) arguments. For `score_pid5()`, `missing = "apa"` (the default)
  applies the APA missing-data/proration rule, `"available"` averages the present
  items (the old `apa_scoring = FALSE, na.rm = TRUE`), and `"complete"` returns
  `NA` for any scale with a missing item (the old `na.rm = FALSE`).
  `score_hitopsr()`/`score_hitopbr()` offer `"available"` (default) and
  `"complete"`. Default behavior is unchanged
* **`rank_scales()` gains a `name` argument** (default `"top_scales"`) naming its
  output column, which was previously hard-coded as `"out"`. It also gains
  `reverse` and `srange` arguments: scales named in `reverse` are reflected via
  `sum(srange) - value` before ranking, so a reverse-directioned scale (e.g. a
  well-being scale, where higher = healthier) ranks on the same "higher = more
  elevated" metric as the other scales
* **The `tibble` argument has been removed** from `score_pid5()`,
  `score_hitopsr()`, `score_hitopbr()`, `validity_pid5()`, and `rank_scales()`;
  these functions now always return a tibble
* **Documentation accuracy and polish** across the scoring tutorials and pkgdown
  instrument pages: corrected stale column/dataset names in the HiTOP-SR tutorial
  (leftovers from an earlier "HiTOP-PRO" draft), updated the HiTOP-BR scale count
  (8, not 7) and the PID-5 appended-column count (now includes the 5 domains),
  finished the previously "work in progress" PID-5-BF tutorial, added the missing
  Scale Reliability sections to the HiTOP-BR and PID-5-BF tutorials, fixed a
  mis-targeted REDCap "Import Instructions" link on the PID-5 download page, and
  reconciled the instrument download pages so each describes only the resources it
  actually links
* **Standardized item-text punctuation** in `hitopsr_items` (7 items) and
  `hitopbr_items` (1 item): every item now ends in a period. The affected items
  (HSR 5, 27, 30, 284, 314, 332, 382 and HBR 41) lack the period in the source
  instrument itself, where 398 of 405 HiTOP-SR items have one; the omissions are
  treated as typographical oversights. The derived `*_scales`/`*_subscales`
  tables and the prebuilt DOCX/Qualtrics/REDCap artifacts in `inst/extdata/`
  were regenerated to match

# hitop 0.1.0

* Add initial HiTOP-HSUM functions
* Add data export functions
* Build out phase 1 website
* `score_pid5()` now returns the 5 personality-trait domain scores for the FULL
  and SF versions (APA scoring key Step 3), appended after the 25 facet scores
* Add the `pid_domains` dataset (the domain to primary-facet map used for FULL/SF
  domain scoring)
* `score_pid5()` gains an `apa_scoring` argument (default `TRUE`) that applies the
  published APA missing-data and proration rule: a facet (or BF domain) with more
  than 25% of its items unanswered is set to `NA`; otherwise the raw score is
  prorated to the full item count and rounded before averaging, and a FULL/SF
  domain is `NA` if any contributing facet is `NA`. **This changes the default
  scored output under missing data** (previously `rowMeans(na.rm = TRUE)` averaged
  whatever items were present). Pass `apa_scoring = FALSE` to restore the previous
  behavior. Under `apa_scoring = TRUE`, `na.rm` is ignored (with a warning if set
  to `FALSE`), and any standard error is `NA` wherever its scale score is `NA`
* Fix `validity_pid5()` erroring on single-row input for the FULL and SF forms
* Fix `score_pid5(calc_se = TRUE)` erroring on single-row input
* Add tests for the `generate_docx_*`, `generate_qualtrics_*`, and
  `generate_redcap_*` export families, verifying each generated file against the
  source instrument datasets (including the HiTOP-HSUM REDCap branching logic)
* `score_pid5()`, `score_hitopsr()`, `score_hitopbr()`, and `validity_pid5()` now
  guard against two ways a bad `items` mapping silently produces wrong scores:
  they error on duplicated `items` entries and warn when `items` column names
  share a common prefix and trailing number but those numbers are not in
  ascending (instrument) order
* `validity_pid5()` now warns when `srange` is not `c(0, 3)`, because the
  published PRD and SD-TD cut scores are raw sums against fixed thresholds that
  assume 0-3 item coding and do not adapt to other codings
* Add runnable `@examples` to every exported function
* Correct the dataset documentation: fix the column counts in the `pid_items`
  and `hitopbr_items` `@format` blocks, document the `pid_scales` format, and fix
  the `sim_hitopbr` item-column names (`hitopbr_1` to `hitopbr_45`)
* Improve the package Title and Description
* `score_hitopbr()` gains `alpha` and `omega` arguments (default `FALSE`) that
  print a per-scale reliability summary, matching `score_pid5()` and
  `score_hitopsr()`
* Internal refactor: `score_pid5()`, `score_hitopsr()`, and `score_hitopbr()` now
  share a single internal scoring engine instead of three hand-maintained copies
  of the same pipeline (no change to scored output)
* Clearer input errors: `items` of the wrong length now reports the expected
  count and what was supplied, and supplying `items` names or positions that are
  not columns of `data` now raises an actionable error (naming the offenders)
  instead of a cryptic base-R subscript error
* Input-validation errors from the scoring, validity, reliability, and
  `rank_scales()` functions are now attributed to the function you called rather
  than to an internal helper

# hitop 0.0.2

* Add initial HiTOP-SR and BR functions

# hitop 0.0.1

* Add initial PID-5 functions
