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
- [ ] AC2: Each subscale column equals the mean of that subscale's items under the call's `missing` rule. `tests/testthat/test-score_hitopsr.R` tests this with two oracle types. The first is hand-computed values, with the arithmetic in comments, for at least two subscales on a dedicated fixture. In that fixture, the items vary within each tested subscale, and one subscale item is `NA`, so the two `missing` modes give different values. The second is a recomputation for all 17 subscales from hardcoded item numbers, never read from `hitopsr_subscales`. It runs under both `missing` modes on data where every subscale has a missing item in some row. The same test asserts that the hardcoded numbers equal `hitopsr_subscales$itemNumbers`.
- [ ] AC3: With a `module`, `include_subscales = TRUE` returns a column for exactly the subscales whose parent scale is in the module. Each such column equals the full 405-item run's column for that subscale, under both `missing` modes on data with missing responses. A test asserts the returned subscale set by name for three kinds of module. These are a one-scale module per parent scale (six), a module with two parent scales, and a module with none.
- [ ] AC4: `reliability_hitopsr()` takes `include_subscales = FALSE`. With `TRUE`, it returns one more row per subscale after the scale rows. Under a `module`, these are the subscales that AC3 selects. Each row's `Scale` is its `hitopsr_subscales$Subscale`, and its `camelCase` and `nItems` come from the same row. For all 17 subscales, `alpha` equals `calc_alpha()` on the items that hardcoded item numbers select. With the default, it returns the scale rows only. A test in `tests/testthat/test-reliability.R` asserts each point.
- [ ] AC5: `label_hitopsr(target = "scales")` labels every subscale column with its `hitopsr_subscales$Subscale`. A test asserts this for all 17 columns in one call, under the default and a non-default `prefix`. `interval_hitopsr()` converts all 17 subscale columns from `score_hitopsr(include_subscales = TRUE)` with no `hitop_interval_uncovered` warning. A test asserts that each value comes from that subscale's `hitopsr_devstats` row. In `man/interval_hitopsr.Rd`, the Description says that `score_hitopsr(include_subscales = TRUE)` produces subscale columns, and no sentence says that subscales lack a column. The `prefix` text of `man/label_hitopsr.Rd` names subscale columns.
- [ ] AC6: Both functions refuse an `include_subscales` that is not a single `TRUE` or `FALSE`, tested with `NA`, `"yes"` and `c(TRUE, TRUE)`. The refusal is the `validate_flag()` error, and the test asserts that its message names `include_subscales`. With `append = TRUE` and `include_subscales = TRUE`, `score_hitopsr()` refuses a `data` column named as any of the 17 subscale columns with class `hitop_append_collision`. The same holds for their `_se` columns under `calc_se = TRUE`. With the default, a `data` column such as `hsr_cynicism` is not refused. Tests assert each point.
- [ ] AC7: `NEWS.md` has a New features entry for the argument. `vignettes/hitopsr_scoring.Rmd` shows a call with `include_subscales = TRUE`. The scoring and reliability signature lines of `cairn/DESIGN.md` name the argument. `devtools::check()` reports 0 errors, 0 warnings, and no note that `main` does not also produce on the same machine.

## Coverage

- AC1 → T1
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
- [ ] T4: Write roxygen for both arguments and run `document()`. Add the NEWS entry, which also says that the abbreviation `i =` for `items` no longer works (LESSONS, M043). Add the vignette call and the DESIGN signature lines. Run `check()` on the branch and on `main` and compare the notes (AC7).

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

## Decisions

- M141-D1 (2026-09-29): `include_subscales` sits after `layout` and before the deprecated `subset` in `score_hitopsr()` and `reliability_hitopsr()`, so the deprecated argument stays last. Only a call that passes `subset` by position changes, and it then fails on the flag check. Chosen at the implement question gate over last place and a place before `module`.

## Review
