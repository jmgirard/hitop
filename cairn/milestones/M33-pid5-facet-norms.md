# M33: PID-5 facet-level norms

- **Status:** review
- **Branch:** `m33-pid5-facet-norms`
- **PR:** https://github.com/jmgirard/hitop/pull/36
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** IP2, IP3, GP2

## Goal

Ship the book's facet-level normative tables so `norm_pid5()` converts all 25
PID-5 facet scores for the full and short forms.

## Scope

**In:** Transcribing Appendix Tables A–6 (full form, p. 124) and A–8 (100-item
short form, p. 151) of markon2024 into `pid_norms`; extending
`data-raw/verify_norms_against_book.R` to check them cell by cell; classifying
the 25 facet stems in `norm_engine.R`'s metric partition; widening the
structural invariants and spot-value oracle in `tests/testthat/test-norms.R`;
re-pointing the tests that use a facet as their uncovered-scale example;
correcting every claim the widened dataset falsifies, in R, vignettes, NEWS,
and `cairn/`.

**Out:**
- Sex/age-stratified norms — absent from the Appendix entirely; stays the
  ROADMAP candidate row, narrowed to that half.
- Informant Form norms (A–10, A–11, A–12) — a new instrument version, not a
  `pid_norms` extension; its own ROADMAP candidate row.
- Norm-referenced profile plots — its own ROADMAP candidate row.
- Any change to how `norm_pid5()` selects a row. The nearest-row rule and its
  toward-50 tie-break (D-022, RR02) are applied to the new rows unchanged.

## Acceptance criteria

- [x] AC1. `Rscript data-raw/verify_norms_against_book.R` extracts Tables A–6
      and A–8 from `cairn/references/sources/markon2024.epub` block by block —
      recovering which facet each `Raw`/`Percentile` pair belongs to from that
      block's banner row rather than from an assumed facet order — reports zero
      discrepancies for all nine specified tables against their committed CSVs,
      and `stop()`s on any discrepancy.
- [x] AC2. `pid_norms` carries one row per (version, facet, tscore) printed in
      A–6 and A–8: 3,550 new rows over 50 new (version, scale) pairs, `version`
      in `FULL`/`SF`, `tscore` covering 30–100 for every *new* pair, no NA in
      `raw` or `percentile`. Each new `scale` is that facet's `camelCase` stem
      read from `pid_scales[[version]]`, and `data-raw/norms_pid5.R` asserts the
      mapped stems are `setequal()` to `pid_scales[[version]]$camelCase` so the
      book-label→stem crosswalk cannot silently drop or duplicate a facet.
- [x] AC3. `norm_pid5()` on `score_pid5(sim_pid5, items = 1:220, version =
      "FULL")` and on the SF equivalent returns a non-NA `_t` and `_ptl` for all
      25 facets with no uncovered-scale report; the 25 facet stems are named in
      `norm_engine.R`'s `norm_mean_scales`; and the four uncovered-scale tests in
      `test-norm_pid5.R` (lines 268, 317, 634, 667) are re-pointed at a scale
      `pid_norms` still does not cover.
- [x] AC4. Every printed row ships, ceiling included: where A–6 or A–8 prints
      the same `raw` on consecutive T rows at the top of a facet's column,
      `pid_norms` carries every one of those rows, and `norm_pid5()` converts
      that raw to the lowest `tscore` of the run. A test asserts both halves
      over all such columns, SF `anxiousness` (12 rows at 4.00) among them.
- [x] AC5. Over the widened `pid_norms`, for every (version, scale) carrying a T
      score: `percentile` is nondecreasing in `tscore`; and a minimax
      (Chebyshev) line in `tscore` reproduces every printed `raw` to within
      0.005 — half a unit in the last printed place — over the rows strictly
      above the column's printed 0.00 floor and, where its top raw repeats on
      consecutive T rows, strictly below that run. This replaces the current
      `lm`-based linearity test, which fails on 20 of the 50 new columns. The
      line is fitted inside the test and never ships (D-022).
- [x] AC6. Each of the 50 new (version, scale) pairs carries at least one spot
      value in `tests/testthat/test-norms.R`, transcribed from the rendered page
      rather than from `verify_norms_against_book.R`'s output (method recorded
      in the work log), anchored to its table label and that table's first page,
      and matching `pid_norms`. The existing "every scale in `pid_norms` has at
      least one spot value" test passes with its assertion unchanged.
- [x] AC7. No shipped or tracked text makes a statement about `pid_norms`'
      contents that the widened dataset falsifies. Updated: `R/data.R`'s
      `pid_norms` `@description`, `@format` (row count and the `raw` item),
      `@details`, and `@source`; `norm_pid5()`'s `@details` (including the
      3.00-ceiling paragraph) and `@return`; the uncovered-scale warning in
      `R/norm_pid5.R`; `R/norm_engine.R`'s floor-tie comment and facet note;
      `tests/testthat/test-norms.R`'s header and "seven shipped tables" test
      name; `vignettes/pid5_scoring.Rmd` (plus one worked facet example);
      `vignettes/pid5sf_scoring.Rmd`; `NEWS.md`; `cairn/SOURCES.md`;
      `cairn/references/markon2024.md` (including the 4.00-clamp anomaly as an
      open question about the source). A `cairn/DECISIONS.md` entry records the
      GP2 change — facet columns move from `NA` + warning to real values — and
      the decision to ship the book's unreachable rows verbatim.
- [x] AC8. `devtools::test()` passes and `devtools::check()` completes with 0
      errors and 0 warnings (Imports installed).

## Coverage

- AC1 → T1
- AC2 → T2
- AC3 → T3, T4
- AC4 → T3, T5
- AC5 → T5
- AC6 → T6
- AC7 → T7
- AC8 → T7

## Tasks

- [x] T1. Extract A–6 and A–8 into `data-raw/norms_pid5_facets.csv` and
      `norms_pid5sf_facets.csv` via `data-raw/extract_facet_norms.R` (3,550 cells
      a side is past hand transcription); give `verify_norms_against_book.R` a
      second, independently structured block reader (banner rows of width 6
      delimit five 5-facet blocks of 71 T rows within one `<table>`), add the two
      `spec` entries, make it `stop()` on discrepancies, and run it to zero.
- [x] T2. Extend `data-raw/norms_pid5.R` with a facet-block builder keyed on
      `pid_scales[[version]]$camelCase`, add the `setequal()` crosswalk guard,
      widen the `stopifnot()` block (`R/norm_engine.R` and `data-raw/norms_pid5.R:117`),
      and regenerate `data/pid_norms.rda`.
- [x] T3. Add the 25 facet stems to `norm_mean_scales` and correct the two now-false
      comments in `R/norm_engine.R` — the floor-tie claim at :46 and the
      "25 facets reach here on every full-form call" note at :135.
- [x] T4. Re-point `test-norm_pid5.R`'s four uncovered-scale fixtures at `SDTD`,
      and extend `grid_for()` (:186) with a facet branch.
- [x] T5. Replace the `lm` linearity test with the minimax form over the
      floor-to-ceiling span, and add the ceiling-run tests (rows present
      verbatim; lowest-T selection).
- [x] T6. Transcribe one spot value per new (version, scale) pair from the
      rendered pages; log the reading method.
- [x] T7. Docs and records sweep per AC7, then `devtools::document()`,
      `devtools::test()`, `devtools::check()`.

## Work log

- 2026-07-31: created by /milestone-plan.
- 2026-07-31: plan gate chose keeping the book's unreachable rows over truncating at raw 3.00 because truncation shifts a maximum scorer's T down one point on 18 of 50 facets by our edit rather than the book's; falsified by evidence that a real 0–3 response path can reach a raw above 3.00, or by the authors confirming the 4.00 clamp is an erratum.
- 2026-07-31: plan gate chose one hand-transcribed spot value per new pair (~50) over one per 5-facet block (~10) because the per-pair layer is what catches a single displaced column, the defect this dataset actually had; falsified by the block-level check catching a seeded single-column displacement.
- 2026-07-31: plan chose a minimax linearity fit at 0.005 over raising the `lm` tolerance because all 66 columns pass minimax at half-a-unit-in-the-last-place while 16 exceed 0.005 under `lm`; falsified by a printed column no line reproduces within its rounding bound.
- 2026-07-31: `cairn_validate`'s 8-AC split tripwire considered and declined — facet rows in `pid_norms` abort `norm_metric()` until the metric partition names them, so a data-only half would leave the default branch un-shippable; the work is one PR.
- 2026-07-31: implement started on `m33-pid5-facet-norms`; source probe confirms A-6/A-8 are `tables[[6]]`/`tables[[8]]`, five banner-delimited blocks of 5 facets x 71 T rows each (355 rows/table, 3,550 total), so AC2's counts hold.
- 2026-07-31: gate chose reading the AC6 spot values off the browser-rendered appendix page over a second extraction tool or waiting for a print copy — the defect this layer catches lives in block-splitting and column-pairing, not in reading the file, and only the EPUB is on the shelf.
- 2026-07-31: T1 done. Minor amendment: the plan's "transcribe" is machine extraction via a new `data-raw/extract_facet_norms.R` (7,100 cells is past hand transcription), so the verifier is now a cross-check of two independently structured reshapings — the extractor carries banner names forward through a sequential walk, the verifier cuts numeric rows at T restarts and matches columns by name — with the hand-read spot values (T6) as the layer that reads the rendered page. Verifier reports 0 discrepancies over all 9 tables (A-6 p. 124, A-8 p. 151); seeded a wrong cell → exit 1, seeded a one-row column displacement → 64 discrepancies.
- 2026-07-31: T2 code landed — `pid_norms` is now 4,606 rows (3,550 new over 50 new pairs, T 30–100 on every one, no NA), built through an explicit book-caption→`pid_scales$Facet` map whose stems still come from `camelCase`; seeded a duplicated caption → the `setequal()` guard aborts. Box left unticked: the widened data necessarily reddens `test-norms.R` until the metric partition (T3) and the two test rewrites (T4, T5) land, so T2–T5 are ticked together at the first green suite rather than checking off a task over a red tree.
- 2026-07-31: T3–T5 code landed. `norm_mean_scales` names the 25 facet stems (spelled out to avoid a load-time dependency on lazy-loaded `pid_scales`, with a new test holding the two in step); the floor-tie and "25 facets reach here" comments corrected; the four uncovered-scale fixtures re-pointed at `SDTD`/`PNA`, `grid_for()` given a facet branch reading `nItems` from `pid_scales`, and `covered_scales` widened from `pid_scales` rather than from the partition under test. The `lm` linearity test is replaced by an exact minimax (Chebyshev) fit over the interior span: all 66 T-scored columns pass, worst 0.004918 against the 0.005 bound. Ceiling-run test finds 19 runs, SF `anxiousness` 12 rows at 4.00, each converting to the run's lowest T. `test-norm_pid5.R` green (1,078); only T6's spot-value coverage test still red.
- 2026-07-31: T7 docs and records sweep landed — `R/data.R` (`@description`, 4,606-row `@format`, the `raw` item's unattainable-row note, `@details` sample text, `@source` A-1 to A-9), `norm_pid5()`'s `@details` (floor paragraph, the rewritten unattainable-rows paragraph, the item-mean list, the uncovered-columns sentence) and `@return`, the runtime uncovered-scale warning, both vignette norming sections plus a worked `anhedonia` example, `NEWS.md`, `cairn/SOURCES.md` (A-6/A-8 rows, caption-map paragraph, nine-table verification status), `cairn/references/markon2024.md` (provenance re-verified 2026-07-31, two new table anchors, traces, and the 4.00-clamp open question), and `D-027`. `devtools::document()` re-run.
- 2026-07-31: T6 blocked. The gate's chosen reading method needs the appendix rendered in the browser pane, and all three routes to it — a `file://` preview, a launch.json static server over the extracted EPUB, and unzipping it under the gitignored `docs/` — were denied by the permission classifier. Not routed around; surfaced to the maintainer instead. Everything not depending on T6 is done; the suite is green but for T6's own spot-value coverage test.
- 2026-07-31: T6 unblocked at the maintainer's direction and done. Reading method: the EPUB was extracted to a scratch dir and served over localhost; each of the ten 5-facet blocks was read in two screenshots of the rendered page — one at the banner row, confirming that block's facet names and order by eye, one at T = 65 — and the 50 raw/percentile pairs were typed from those images, never from the extractor. T = 65 is clear of every column's 0.00 floor and below every ceiling run, so a one-row displacement moves the value on all 50. All 50 match `pid_norms`.
- 2026-07-31: discovered sub-task under T5/T6 — `data-raw/mutate_norms_check.R` gains three facet mutations. A displaced `hostility` column is caught by the spot values *and by nothing else*, which is the case AC6 exists for; a `perseveration` raw pushed 0.02 off its line is caught by the minimax test; a truncated SF `anxiousness` ceiling run is caught by the ceiling-run test.
- 2026-07-31: T7 gate clean — `devtools::document()` no diff, `devtools::test()` 11,489 pass / 0 fail / 1 skip, `devtools::check()` 0 errors, 0 warnings, 0 notes. Status → review.
- 2026-07-31: criteria audit ([O], fresh context) returned 20 findings; 14 fixed into the wording above (minimax named, plateau defined, `lm` test replacement stated, crosswalk guard added, four breaking tests named, `R/data.R` `@details` and `R/norm_engine.R` added to the docs sweep, check bar made absolute), 3 became gate questions, 3 were confirmations.

## Decisions

## Review

Reviewed 2026-07-31 against PR #36. Evidence below is fresh — every line was
produced by running the command named, in this session, on the branch head.

### Acceptance criteria
- **AC1 — met.** `Rscript data-raw/verify_norms_against_book.R` prints all nine
  tables and `RESULT: every cell of all 9 tables matches the book`, exit 0; A-6
  anchors to p. 124 and A-8 to p. 151, the pages the plan names. Extraction is
  block-aware and banner-driven, not order-assuming: seeding a wrong cell in the
  SF facet CSV gave `T = 68, column Anhedonia_Raw -- book 1.79, csv 9.99` and
  exit 1, and swapping the `Hostility_Raw`/`Impulsivity_Raw` column *labels*
  (leaving every number in place) raised 118 discrepancies — a positional
  comparison would have passed it. Tree restored to clean after both probes.
- **AC2 — met.** `pid_norms` is 4,606 rows against M25's 1,056: 3,550 new, over
  exactly 50 new (version, scale) pairs, `version` in `FULL`/`SF` only. Every
  new pair's `tscore` set is `identical()` to `30:100`; no NA in `raw` or
  `percentile`. The new `scale` values are `setequal()` to
  `pid_scales[[version]]$camelCase` for both versions. The build guard bites:
  renaming the `Withdrawal` columns to `Anhedonia` in the FULL facet CSV aborts
  `data-raw/norms_pid5.R` at `setequal(unname(stem), scales$camelCase)` rather
  than writing a `pid_norms` missing a facet. Tree restored to clean.
- **AC3 — met.** `norm_pid5()` over all 25 facet columns of
  `score_pid5(sim_pid5, items = 1:220, version = "FULL")` returns 25 `_t` and 25
  `_ptl` columns, no NA in any, and raises no `not covered` warning; the SF
  equivalent on `sim_pid5sf` does the same. All 25 stems of both versions are in
  `norm_mean_scales`. The four uncovered-scale fixtures are re-pointed at scales
  the tables still miss — `SDTD`/`PNA` at `test-norm_pid5.R:290-292` and `:339`,
  `SDTD` at `:663` and `:693` — and `norm_covers()` confirms neither is covered.
- **AC4 — met.** Every T-scored column's `tscore` values form an unbroken run
  from its own minimum to its maximum, so no printed row was dropped. Nineteen
  columns repeat their top raw; each run is contiguous, and `norm_convert()` at
  that raw returns the run's lowest T on all nineteen. SF `anxiousness` carries
  12 rows at 4.00 spanning T = 89-100 and converts to T = 89. Both halves are
  asserted in `test-norms.R`'s ceiling-run test, and the mutation harness
  confirms it bites: truncating that run to one row is CAUGHT by it.
- **AC5 — met.** No percentile decreases anywhere: 0 of the 66 T-scored columns
  and 0 of the 4 validity columns. Recomputing the minimax bound independently of
  the test's own helper gives a worst column of 0.004918 and 0 of 66 at or above
  0.005. The plan's ground for the replacement is confirmed by measurement: the
  old `lm` form would fail on exactly 20 of the 50 new columns. Nothing fitted
  ships — `minimax_line_error()` occurs only in `tests/testthat/test-norms.R`,
  and no `lm(`, `chull`, or minimax code exists under `R/` or in the data-raw
  builders (D-022 intact). A raw pushed 0.02 off its line is CAUGHT.
- **AC6 — met.** `facet_spot` (`test-norms.R:256`) carries one anchor per new
  pair, all 50, at T = 65, labelled to their tables' first pages (124 and 151).
  Reading method is in the work log: the EPUB was served over localhost and each
  of the ten blocks read in two screenshots of the rendered page — banner row for
  the facet names and order, T = 65 for the values — with the numbers typed from
  those images, never from the extractor's output. `devtools::test(filter =
  "norms")` is 526 pass / 0 fail, so every one matches `pid_norms`. The
  coverage test's assertion is unchanged (`expect_setequal` over
  `paste(keys$version, keys$scale)`); only the frame it unions was widened. Its
  value is demonstrated, not asserted: displacing the FULL `hostility` raw column
  by one row is caught by the spot values AND BY NOTHING ELSE.
- **AC7 — met.** Every listed surface is updated, and a sweep for the falsified
  phrasings — `1056`/`1,056`, `seven shipped`, `all seven`, `do not cover the 25
  facets`, `facet-level and informant-form`, `the 25 facets, for instance`,
  `A-1 to A-5, A-7` — returns nothing across `R/`, `man/`, `tests/`,
  `vignettes/`, `NEWS.md`, `README.Rmd`, `cairn/SOURCES.md`, and
  `cairn/references/markon2024.md`. Spot-checked replacements: `R/data.R` 4,606
  rows and `Tables A-1 to A-9`; `norm_pid5()`'s rewritten unattainable-rows
  paragraph; the runtime warning naming facets per version; `test-norms.R`'s
  header and its `nine shipped tables` test name; both vignettes. D-027 records
  the GP2 change and the ship-verbatim decision; `markon2024.md` carries the
  4.00 clamp as an open question against the source.
- **AC8 — met.** `devtools::test()` 11,489 pass / 0 fail / 1 skip.
  `devtools::check()` Status: OK — 0 errors, 0 warnings, 0 notes, vignettes
  re-built.

### Consistency gate

- `cairn_validate` exit 0, every CHECK PASS. Two advisories, neither a gate
  failure: `sizing` flags M33's 8 acceptance criteria against the >7 split
  tripwire — considered and declined at plan time for a reason the work bore out
  (facet rows in `pid_norms` abort `norm_metric()` until the partition names
  them, so a data-only half would leave the default branch un-shippable); and
  `dangling id tokens` (20), the standing pre-migration references in
  `DESIGN.md`/`SOURCES.md` that the ROADMAP hygiene stamp already records.
- No `DESIGN.md` principle changed, so `cairn_impact` does not apply. The header's
  `Principles touched: IP2, IP3, GP2` names principles the work is held to, none
  it edits.
- Toolchain gate (`r-package` profile): `devtools::document()` produces no diff;
  `NAMESPACE`, `man/`, and `data/*.rda` are all generated (the two `man/` files
  in the diff come from `document()`, `pid_norms.rda` from `data-raw/`);
  README.md is in sync and untouched by this diff; `pkgdown::check_pkgdown()`
  reports no problems; `NEWS.md` carries the user-visible changes with no
  milestone numbers; no new top-level files, so no `.Rbuildignore` entry is owed;
  `devtools::check()` is 0/0/0.
- CI on PR #36: all 7 checks pass — `R CMD check` on ubuntu (devel, release,
  oldrel-1), macos, and windows, plus pkgdown and test-coverage.

### Independent review

Three fresh-context reviewers on distinct evidence bases, then a [S] scorer that
did not generate the findings. [S] blame-history: 0 findings — it traced every
deleted or rewritten region and found no prior deliberate work undone. [S]
prior-review record: 0 findings; the GitHub inline-comment probe returned an
empty array, so the archived `## Review` sections of M25-M32 were the evidence,
and none is regressed. [O] diff-bug: 21 findings, scored 8-88.

**Actioned (scored >= 80), all five fixed on the branch:**

- F1 (88) `R/data.R`'s `@format` said "Nineteen facet columns print raws above
  the 3.00"; the count for that claim is 42, and 19 is the count of columns that
  *repeat* their top raw — so the two shipped help pages contradicted each other.
  Fixed to state both numbers for what each measures. Verified: 42 of 50.
- F7 (85) `verify_norms_against_book.R`'s facet comparison used `which(b != cv)`,
  which drops NA, so a cell that failed to parse was reported as matching. Fixed
  to the NA-aware form the non-facet path already used, plus an `!anyNA()` guard
  on the book side. Verified: a cell typed `1.7O` now reports `csv NA` and
  exits 1, where before it printed "every cell ... matches the book", exit 0.
- F3 (82) `norm_pid5()`'s "Scores outside the table" bullet said a score above
  the highest printed row returns that row's values; on the 19 ceiling-run
  columns it returns the run's lowest T, which a later bullet in the same
  `@details` stated correctly. Fixed the earlier bullet.
- F15 (80) "Every T-scored column has" a 0.00 floor run is false for two: FULL
  detachment and FULL riskTaking print 0.00 exactly once. Corrected in both
  `R/norm_engine.R` and `R/norm_pid5.R` to 64 of 66, naming the two.
- F12 (80) a displaced *percentile* column, `raw` untouched, is caught by no test
  in the suite. Fixed in part and measured rather than assumed: two percentile
  displacements added to `mutate_norms_check.R` both report NOT CAUGHT, and the
  header comment in `test-norms.R` that claimed the anchors catch any displaced
  column now states both gaps and points at the script. `verify_norms_against_book.R`
  does catch it at the CSV; the residual exposure is a displacement introduced
  downstream, in the long-format assembly. Closing it needs a second anchor per
  column — a ROADMAP candidate, added 2026-07-31.

**Also fixed — surfaced by the scorer, not by any reviewer.** Scoring F16 (25)
showed its premise backwards and a real defect underneath it: `pid_scales[["BF"]]`
*does* carry a `camelCase` column (its domains and total), so `grid_for()`'s new
`v %in% names(pid_scales)` branch swallowed every BF lookup and left the two BF
branches below it dead. Harmless today only because the grids coincide. The
branch is now keyed on `c("FULL", "SF")`.

**Sub-threshold (16), logged not actioned:** F2 (78) two pairs of SF facets share
anchor values at T = 65, so neither can witness a swap of the other — same
remedy as F12, folded into that candidate. F4 (75) `norm_capped()`'s header
comment shares F3's staleness — fixed anyway while in the file. F8 (78) no test
locks the 4,606 row count or the 30-100 span, only contiguity. F9 (78) two new
`stopifnot()` comments overstate what their checks do (a range bound is not a
span check; the version table recycles). F10 (72) AC3's end-to-end claim is
verified by hand here but not locked by a test. F6 (68) a fixture comment in
`test-norm_pid5.R` says no covered scale is prorated, which facet coverage makes
false. F11 (62) the two reshapings are independent in row assembly but share the
banner-to-block pairing, so the scripts' "would have to occur identically"
wording is stronger than earned. F5 (60) facets now cap on simulated data,
emitting a capping warning where the columns used to return NA — real new
behavior, undocumented. F18 (60) "3,550 cells each side of the pair" reads as
7,100. F21 (58) the uncovered-scale message qualifies domains and facets by
version but lists the validity scales flat. F14 (52), F17 (50), F13 (48), F20
(42), F16 (25), F19 (8) — hypothetical, imprecise, or premise-wrong; F19 asks for
a page anchor AC6 explicitly specifies.

### Post-fix re-verification

`devtools::document()` no diff beyond the two regenerated `.Rd`; `devtools::test()`
11,489 pass / 0 fail / 1 skip; `devtools::check()` 0 errors, 0 warnings, 0 notes;
`cairn_validate` exit 0; `verify_norms_against_book.R` 0 discrepancies over 9
tables; mutation script 9 of 11 CAUGHT, the 2 NOT CAUGHT being F12's deliberately
seeded percentile displacements.
