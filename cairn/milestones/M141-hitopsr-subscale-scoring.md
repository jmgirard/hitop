# M141: HiTOP-SR scoring can include the 17 subscales on request

- **Status:** in-progress
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** IP2, IP3, GP2, GP3
- **Resolves:** —
- **Surface tier:** user-facing — adds an argument to two exported scoring functions
- **Branch/PR:** m141-hitopsr-subscale-scoring

## Goal

A researcher can get HiTOP-SR subscale scores and subscale reliabilities by setting one argument.

## Scope

**In:** An `include_subscales = FALSE` argument on `score_hitopsr()` and `reliability_hitopsr()`. The 17 subscales come from the shipped `hitopsr_subscales` table, and no keying content changes. Under a `module`, the subscales whose parent scale is in the module. `label_hitopsr(target = "scales")` labels subscale columns. `interval_hitopsr()` gets a subscale test and a corrected help page, because it already converts a subscale column through `hitopsr_devstats`. NEWS, the HiTOP-SR scoring vignette and the DESIGN signature lines.

**Out:** Lifting `generate_docx_hitopsr()`'s refusal of `include_subscales` with a `module` goes to a candidate ROADMAP row (added with this plan). Subscale definitions in the builder picker stay in their existing candidate row. Recomputing Table 1's subscale statistics from the Prolific data stays in its existing candidate row. HiTOP-BR and PID-5 have no subscales.

## Acceptance criteria

- [ ] AC1: `score_hitopsr()` takes `include_subscales = FALSE`. Without a `module` and with `TRUE`, the output gains one column per `hitopsr_subscales` row, named `prefix` + that row's `camelCase`. These columns follow the scale columns in `hitopsr_subscales` row order. Under `calc_se = TRUE`, each subscale also gets an `_se` column, after the scale `_se` columns in the same order. The scale columns are identical to the default call's. With the default, no column named `prefix` + a `hitopsr_subscales$camelCase` is returned. A test in `tests/testthat/test-score_hitopsr.R` asserts each point under both `missing` modes.
- [x] AC2: Each subscale column equals the mean of that subscale's items under the call's `missing` rule. `tests/testthat/test-score_hitopsr.R` tests this with two oracle types. The first is hand-computed values, with the arithmetic in comments, for at least two subscales on a dedicated fixture. In that fixture, the items vary within each tested subscale, and one subscale item is `NA`, so the two `missing` modes give different values. The second is a recomputation for all 17 subscales from hardcoded item numbers, never read from `hitopsr_subscales`. It runs under both `missing` modes on data where every subscale has a missing item in some row. The same test asserts that the hardcoded numbers equal `hitopsr_subscales$itemNumbers`.
- [x] AC3: With a `module`, `include_subscales = TRUE` returns a column for exactly the subscales whose parent scale is in the module. Each such column equals the full 405-item run's column for that subscale, under both `missing` modes on data with missing responses. A test asserts the returned subscale set by name for three kinds of module. These are a one-scale module per parent scale (six), a module with two parent scales, and a module with none.
- [x] AC4: `reliability_hitopsr()` takes `include_subscales = FALSE`. With `TRUE`, it returns one more row per subscale after the scale rows. Under a `module`, these are the subscales that AC3 selects. Each row's `Scale` is its `hitopsr_subscales$Subscale`, and its `camelCase` and `nItems` come from the same row. For all 17 subscales, `alpha` equals `calc_alpha()` on the items that hardcoded item numbers select. With the default, it returns the scale rows only. A test in `tests/testthat/test-reliability.R` asserts each point.
- [x] AC5: `label_hitopsr(target = "scales")` labels every subscale column with its `hitopsr_subscales$Subscale`. A test asserts this for all 17 columns in one call, under the default and a non-default `prefix`. `interval_hitopsr()` converts all 17 subscale columns from `score_hitopsr(include_subscales = TRUE)` with no `hitop_interval_uncovered` warning. A test asserts that each value comes from that subscale's `hitopsr_devstats` row. In `man/interval_hitopsr.Rd`, the Description says that `score_hitopsr(include_subscales = TRUE)` produces subscale columns, and no sentence says that subscales lack a column. The `prefix` text of `man/label_hitopsr.Rd` names subscale columns.
- [x] AC6: Both functions refuse an `include_subscales` that is not a single `TRUE` or `FALSE`, tested with `NA`, `"yes"` and `c(TRUE, TRUE)`. The refusal is the `validate_flag()` error, and the test asserts that its message names `include_subscales`. With `append = TRUE` and `include_subscales = TRUE`, `score_hitopsr()` refuses a `data` column named as any of the 17 subscale columns with class `hitop_append_collision`. The same holds for their `_se` columns under `calc_se = TRUE`. With the default, a `data` column such as `hsr_cynicism` is not refused. Tests assert each point.
- [x] AC7: `NEWS.md` has a New features entry for the argument. `vignettes/hitopsr_scoring.Rmd` shows a call with `include_subscales = TRUE`. The scoring and reliability signature lines of `cairn/DESIGN.md` name the argument. `devtools::check()` reports 0 errors, 0 warnings, and no note that `main` does not also produce on the same machine.

## Coverage

- AC1 → T1, T5
- AC2 → T1
- AC3 → T1
- AC4 → T2
- AC5 → T3
- AC6 → T1, T2
- AC7 → T4

## Tasks

- [x] T1: Transcribe the 17 subscales' item numbers into the test from `HiTOP-SR-Final.xlsx`. Cite its "HiTOP-SR items by scale" sheet in a comment. Write the AC1, AC2, AC3 and AC6 tests and the dedicated fixture first. Then add `include_subscales` to `score_hitopsr()`, validated with `validate_flag()`. Extend `hitopsr_engine_inputs()` (`R/module.R:268`) so that the full and module paths append the subscale item lists.
- [x] T2: Write the AC4 and AC6 tests first in `tests/testthat/test-reliability.R`. Then add `include_subscales` to `reliability_hitopsr()`, with the subscale names and stems passed to `reliability_engine()`.
- [x] T3: Extend `label_hitopsr()` (`R/label_hitopsr.R:73`) to label subscale columns. Add the `interval_hitopsr()` subscale test. Rewrite the `interval_hitopsr()` Description and the `label_hitopsr()` `prefix` text (AC5).
- [x] T4: Write roxygen for both arguments and run `document()`. Add the NEWS entry, which also says that the abbreviation `i =` for `items` no longer works (LESSONS, M043). Add the vignette call and the DESIGN signature lines. Run `check()` on the branch and on `main` and compare the notes (AC7).
- [x] T5: Move the `calc_se = TRUE` block of the AC1 test inside the `missing` mode loop, so `_se` placement is asserted under both modes (review return 1).
- [x] T6: Review findings taken at the implement gate. Add a subscale test under a module with `layout = "printed"` and a shuffled `item_order` (O1). Name subscale columns in the `score_hitopsr()` title, `@return` and examples, and in the `reliability_hitopsr()` description (O3, O4, S9). Add a NEWS note that passing `subset` by position now fails (O6). Add a test that `i =` fails and `it =` works (S3). If a parent name does not match, make `add_hitopsr_subscales()` abort with `call` (O5, P1).

## Work log

- 2026-09-29: created by /milestone-plan.
- 2026-09-29: criteria audit (full mode, fresh [O] reader) returned nine findings, all fixed before the gate. `Scale` takes `Subscale`, not the parent name. A dedicated fixture replaces `fx_hitopsr()`, where every subscale scores 1, 4, 2, 3. Module probes cover all six parents. Other fixes cover `_se` placement, flag and collision probes, and the real reliability test file. Doc criteria replace a phrase search, and the notes bar is relative to `main`. The workbook-provenance clause moved to T1.
- 2026-09-29: plan gate chose `include_subscales` over `subscales` because it matches `generate_docx_hitopsr()`'s argument. Falsified by caller reports that the lost `i =` abbreviation breaks real code.
- 2026-09-29: plan gate chose to score the subscales whose parent scale is in a module over refusing the combination. Every subscale's items lie in its parent scale. Falsified by a keying change that puts a subscale item outside its parent scale.
- 2026-09-29: plan chose `_se` columns for subscales under `calc_se = TRUE` over none. The engine treats every item list alike, and `calc_se` is deprecated. Falsified by a user who reports the extra columns as noise.
- 2026-09-29: plan chose no new `type` column in `reliability_hitopsr()` output over adding one, because it changes the default return shape. Falsified by a user who cannot tell subscale rows from scale rows.
- 2026-09-29: implement started on branch m141-hitopsr-subscale-scoring; question gate settled argument placement (M141-D1).
- 2026-09-29: T1 done. The workbook sheet lists pool IDs, so the test places each subscale item at its HiTOP-SR number by item text; all 17 sets equal `hitopsr_subscales`. Two planted defects (no module filter, shifted item numbers) turned the new tests red. `devtools::test()` 0 failures.
- 2026-09-29: T2 done. `reliability_hitopsr()` takes `include_subscales`. The subscale key moved to `helper-fixtures.R` so both test files share it. `devtools::test()` 0 failures.
- 2026-09-29: T3 done. `label_hitopsr()` labels subscale columns, and its new test failed before the change. The `interval_hitopsr()` subscale test passed with no code change. Both help texts are rewritten. `devtools::test()` 0 failures.
- 2026-09-29: T4 done. NEWS entry, vignette "Subscales" section and DESIGN signature line added. `i =` was observed to fail on both functions and `it =` still works. `devtools::check()` gave 0 errors, 0 warnings, 0 notes on the branch and on `main`.
- claim audit: 41 claims read, 0 corrected — NEWS.md, R/score_hitopsr.R, R/reliability_hitopsr.R, R/interval_hitopsr.R, R/label_hitopsr.R, R/module.R, vignettes/hitopsr_scoring.Rmd, tests/testthat/helper-fixtures.R and four test files
- 2026-09-29: implement complete, status set to review.
- 2026-09-29: review checkpoint (partial). Evidence for AC1 to AC6 recorded, AC1 fails as written. The `check()` run and two reviewers are still pending.
- 2026-09-29: review return 1 (defect). AC1 fails as written, because the `calc_se = TRUE` block of `test-score_hitopsr.R:182` asserts `_se` placement under `missing = "available"` only, not under both modes. AC2 to AC7 and the consistency gate pass. The next pass fixes the test and can take up the 13 findings pending triage in the Review section. Status set to in-progress.
- 2026-09-29: implement resumed. Gate chose the AC1 fix plus the small in-scope findings over the AC1 fix alone. T5 and T6 added (minor amendment, no criterion changed).
- 2026-09-29: T5 done. The `calc_se` block of the AC1 test now runs inside the `missing` mode loop. The `score_hitopsr` test file passes.
- 2026-09-29: T6 done. New tests cover subscales under a shuffled printed-order module, the `i =` and `it =` pins, and two keying-fault guards. A planted remap from `item_order` turned the printed-order test red in 3 places. `add_hitopsr_subscales()` now aborts on an unmatched parent name and passes `call`. Help and NEWS text updated. `devtools::test()` 1035 tests, 0 failures.
- claim audit: 96 claims read, 2 corrected — R/module.R, tests/testthat/test-score_hitopsr.R (plus two descriptions tightened in R/score_hitopsr.R and R/reliability_hitopsr.R). Files read: NEWS.md, five R files, the vignette, helper-fixtures.R and five test files.

## Decisions

- M141-D1 (2026-09-29): `include_subscales` sits after `layout` and before the deprecated `subset` in `score_hitopsr()` and `reliability_hitopsr()`, so the deprecated argument stays last. Only a call that passes `subset` by position changes, and it then fails on the flag check. Chosen at the implement question gate over last place and a place before `module`.

## Review

Evidence run 2026-09-29 on branch head c256443a, which contains `origin/main`. No PR exists yet. `devtools::document()` gave no diff. `devtools::test()` ran 64 files and 1032 tests with 0 failures, 0 errors and 15 skips, all of them older skips.

- AC1: FAIL as written. The test at `test-score_hitopsr.R:182` asserts the column, order, scale-identity and default-absence points under both `missing` modes. It asserts the `_se` placement point only under the default `missing = "available"`, because lines 199 to 208 sit outside the mode loop. The clause "asserts each point under both `missing` modes" is not met.
- AC2: The hand-computed test covers Cynicism and Hallucinations on a dedicated fixture. Items vary, and Cynicism row 2 has an `NA` (2.5 and 7/3 available, 2.5 and NA complete). The recomputation test scores all 17 subscales from `subscale_key` in `helper-fixtures.R` under both modes on `holed_subscales()`. That fixture gives each subscale an `NA` in its own row. The same test asserts that `subscale_key` equals `hitopsr_subscales$itemNumbers`. Suite green.
- AC3: The module test probes the six one-parent modules, a mistrust, dishonesty and agoraphobia module, and an agoraphobia and appetiteLoss module. It asserts the subscale set by name. It asserts equality to the full run's columns under both modes on `holed_subscales()`. Suite green.
- AC4: The `test-reliability.R` subscale tests assert scale rows only by default. With `TRUE`, subscale rows follow the scale rows, with `Scale`, `camelCase` and `nItems` from `hitopsr_subscales`. All 17 `alpha` values equal `calc_alpha()` on the `subscale_key` items. Module rows equal the AC3 selection in three probes. Suite green.
- AC5: `test-label_scales.R` labels all 17 subscale columns under `"hsr_"` and `"sr."`. `test-interval_hitopsr.R` converts all 17 with no `hitop_interval_uncovered` warning. It checks each est, lo and hi value against its `hitopsr_devstats` row. The `\description` of `man/interval_hitopsr.Rd` (lines 45 to 53) names `score_hitopsr(include_subscales = TRUE)`. No sentence there says that subscales lack a column. The `prefix` text of `man/label_hitopsr.Rd` (lines 28 to 31) names subscale columns. Suite green.
- AC6: Both flag tests use `NA`, `"yes"` and `c(TRUE, TRUE)` and match `"include_subscales.*must be"`. The observed message is "The `include_subscales` argument must be `TRUE` or `FALSE`." The collision test covers all 17 columns and their `_se` columns. With the default, a `data$hsr_cynicism` column is kept. Suite green.
- AC7: `NEWS.md` lines 5 to 14 hold the New features entry. `vignettes/hitopsr_scoring.Rmd` has a "Subscales" section with an `include_subscales = TRUE` call. The `cairn/DESIGN.md` signature paragraph names the argument for both functions. `devtools::check()` on the branch gave 0 errors, 0 warnings and 0 notes, so no note is unique to the branch.

Consistency gate: `cairn_validate.py` exit 0, with 24 older advisory warnings. `document()` gave no diff. `pkgdown::check_pkgdown()` found no problems. README is untouched. No principle text changed, so `cairn_impact` was skipped.

Gate result: FAIL on AC1. Status returns to `in-progress` (first defect return).

Reviewer findings, reported before the return. The three reviewers ran in parallel with the evidence run. Triage of all but the first waits for the next merge gate.

- O2 (diff reviewer), matching the AC1 evidence: the `calc_se = TRUE` block in `test-score_hitopsr.R` runs under one `missing` mode only. Disposition: floor return, fixed in the next implement pass.
- O1: No test combines `include_subscales = TRUE` with `layout = "printed"` and an `item_order`, the one path where subscale positions pass a second remap. A probe showed the current code correct. Pending triage.
- O3 and S9: The `score_hitopsr()` title and `@return` do not name subscale columns, and no example shows the argument. Pending triage.
- O4: The `reliability_hitopsr()` description names scales only. Pending triage.
- O5: Under a module, a subscale whose parent name fails to match `hitopsr_scales$Scale` is dropped with no error. All 17 match today. Pending triage.
- O6 and S6: NEWS does not say that a call passing `subset` by position now fails on the flag check. The placement lives in M141-D1 and DESIGN only. Pending triage.
- O7 and S1: The `generate_docx_hitopsr()` refusal message and the modules article say that a subscale can draw items from outside the module, which M141 relies on being false. `test-module-doc-prose.R` pins the article text. The existing candidate row covers the refusal. Pending triage.
- O8: `rank_scales()` documents ties as broken in alphabetical column order, which subscale columns placed after the scales no longer follow. Pending triage.
- O9: The flag tests assert `rlang_error` and a message, not a class specific to `validate_flag()`. Pending triage.
- S2 and P2: Scoring keeps only in-module subscales while the Word generator refuses the combination. The plan gate chose this. Pending triage.
- S3: No test pins that `i =` fails and `it =` still works, as `test-deprecated.R` does for `m =`. Pending triage.
- S4: The `i =` break sits under New features, not Breaking changes, as M043 also did. Pending triage.
- S5: Subscale `_se` columns add to the deprecated `calc_se` surface with no D-entry. Pending triage.
- P1: The internal abort in `add_hitopsr_subscales()` has no `call =`. It is an internal-error guard. Pending triage.
