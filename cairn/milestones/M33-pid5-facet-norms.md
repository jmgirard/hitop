# M33: PID-5 facet-level norms

- **Status:** in-progress
- **Branch:** `m33-pid5-facet-norms`
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

- [ ] AC1. `Rscript data-raw/verify_norms_against_book.R` extracts Tables A–6
      and A–8 from `cairn/references/sources/markon2024.epub` block by block —
      recovering which facet each `Raw`/`Percentile` pair belongs to from that
      block's banner row rather than from an assumed facet order — reports zero
      discrepancies for all nine specified tables against their committed CSVs,
      and `stop()`s on any discrepancy.
- [ ] AC2. `pid_norms` carries one row per (version, facet, tscore) printed in
      A–6 and A–8: 3,550 new rows over 50 new (version, scale) pairs, `version`
      in `FULL`/`SF`, `tscore` covering 30–100 for every *new* pair, no NA in
      `raw` or `percentile`. Each new `scale` is that facet's `camelCase` stem
      read from `pid_scales[[version]]`, and `data-raw/norms_pid5.R` asserts the
      mapped stems are `setequal()` to `pid_scales[[version]]$camelCase` so the
      book-label→stem crosswalk cannot silently drop or duplicate a facet.
- [ ] AC3. `norm_pid5()` on `score_pid5(sim_pid5, items = 1:220, version =
      "FULL")` and on the SF equivalent returns a non-NA `_t` and `_ptl` for all
      25 facets with no uncovered-scale report; the 25 facet stems are named in
      `norm_engine.R`'s `norm_mean_scales`; and the four uncovered-scale tests in
      `test-norm_pid5.R` (lines 268, 317, 634, 667) are re-pointed at a scale
      `pid_norms` still does not cover.
- [ ] AC4. Every printed row ships, ceiling included: where A–6 or A–8 prints
      the same `raw` on consecutive T rows at the top of a facet's column,
      `pid_norms` carries every one of those rows, and `norm_pid5()` converts
      that raw to the lowest `tscore` of the run. A test asserts both halves
      over all such columns, SF `anxiousness` (12 rows at 4.00) among them.
- [ ] AC5. Over the widened `pid_norms`, for every (version, scale) carrying a T
      score: `percentile` is nondecreasing in `tscore`; and a minimax
      (Chebyshev) line in `tscore` reproduces every printed `raw` to within
      0.005 — half a unit in the last printed place — over the rows strictly
      above the column's printed 0.00 floor and, where its top raw repeats on
      consecutive T rows, strictly below that run. This replaces the current
      `lm`-based linearity test, which fails on 20 of the 50 new columns. The
      line is fitted inside the test and never ships (D-022).
- [ ] AC6. Each of the 50 new (version, scale) pairs carries at least one spot
      value in `tests/testthat/test-norms.R`, transcribed from the rendered page
      rather than from `verify_norms_against_book.R`'s output (method recorded
      in the work log), anchored to its table label and that table's first page,
      and matching `pid_norms`. The existing "every scale in `pid_norms` has at
      least one spot value" test passes with its assertion unchanged.
- [ ] AC7. No shipped or tracked text makes a statement about `pid_norms`'
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
- [ ] AC8. `devtools::test()` passes and `devtools::check()` completes with 0
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

- [ ] T1. Transcribe A–6 and A–8 into `data-raw/norms_pid5_facets.csv` and
      `norms_pid5sf_facets.csv`; give `verify_norms_against_book.R` block-aware
      extraction (banner rows of width 6 delimit five 5-facet blocks of 71 T
      rows within one `<table>`), add the two `spec` entries, make it `stop()`
      on discrepancies, and run it to zero.
- [ ] T2. Extend `data-raw/norms_pid5.R` with a facet-block builder keyed on
      `pid_scales[[version]]$camelCase`, add the `setequal()` crosswalk guard,
      widen the `stopifnot()` block (`R/norm_engine.R` and `data-raw/norms_pid5.R:117`),
      and regenerate `data/pid_norms.rda`.
- [ ] T3. Add the 25 facet stems to `norm_mean_scales` and correct the two now-false
      comments in `R/norm_engine.R` — the floor-tie claim at :46 and the
      "25 facets reach here on every full-form call" note at :135.
- [ ] T4. Re-point `test-norm_pid5.R`'s four uncovered-scale fixtures at `SDTD`,
      and extend `grid_for()` (:186) with a facet branch.
- [ ] T5. Replace the `lm` linearity test with the minimax form over the
      floor-to-ceiling span, and add the ceiling-run tests (rows present
      verbatim; lowest-T selection).
- [ ] T6. Transcribe one spot value per new (version, scale) pair from the
      rendered pages; log the reading method.
- [ ] T7. Docs and records sweep per AC7, then `devtools::document()`,
      `devtools::test()`, `devtools::check()`.

## Work log

- 2026-07-31: created by /milestone-plan.
- 2026-07-31: plan gate chose keeping the book's unreachable rows over truncating at raw 3.00 because truncation shifts a maximum scorer's T down one point on 18 of 50 facets by our edit rather than the book's; falsified by evidence that a real 0–3 response path can reach a raw above 3.00, or by the authors confirming the 4.00 clamp is an erratum.
- 2026-07-31: plan gate chose one hand-transcribed spot value per new pair (~50) over one per 5-facet block (~10) because the per-pair layer is what catches a single displaced column, the defect this dataset actually had; falsified by the block-level check catching a seeded single-column displacement.
- 2026-07-31: plan chose a minimax linearity fit at 0.005 over raising the `lm` tolerance because all 66 columns pass minimax at half-a-unit-in-the-last-place while 16 exceed 0.005 under `lm`; falsified by a printed column no line reproduces within its rounding bound.
- 2026-07-31: `cairn_validate`'s 8-AC split tripwire considered and declined — facet rows in `pid_norms` abort `norm_metric()` until the metric partition names them, so a data-only half would leave the default branch un-shippable; the work is one PR.
- 2026-07-31: implement started on `m33-pid5-facet-norms`; source probe confirms A-6/A-8 are `tables[[6]]`/`tables[[8]]`, five banner-delimited blocks of 5 facets x 71 T rows each (355 rows/table, 3,550 total), so AC2's counts hold.
- 2026-07-31: gate chose reading the AC6 spot values off the browser-rendered appendix page over a second extraction tool or waiting for a print copy — the defect this layer catches lives in block-splitting and column-pairing, not in reading the file, and only the EPUB is on the shelf.
- 2026-07-31: criteria audit ([O], fresh context) returned 20 findings; 14 fixed into the wording above (minimax named, plateau defined, `lm` test replacement stated, crosswalk guard added, four breaking tests named, `R/data.R` `@details` and `R/norm_engine.R` added to the docs sweep, check bar made absolute), 3 became gate questions, 3 were confirmations.

## Decisions

## Review
