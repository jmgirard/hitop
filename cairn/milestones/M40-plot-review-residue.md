# M40: Retire the M38/M39 plot-review residue

- **Status:** review
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** IP2, GP3, GP4
- **Branch/PR:** `m40-plot-review-residue` / https://github.com/jmgirard/hitop/pull/44

## Goal

Clear the four mechanical follow-ups the M38 and M39 reviews left on
`plot_pid5()` and its tests.

## Scope

**In:** dropping the label-side axis padding when `plot_pid5(labels = FALSE)`
is asked for; converting `tests/testthat/test-plot_pid5.R` off the plot
object's internal data frame onto built layer data (D-030); labelling the
in-loop assertions in that file so a failure names its iteration; factoring
the non-numeric-column guard shared by `norm_pid5()` and `plot_pid5()` into
`R/util.R`, each function keeping its own headline and both gaining the
per-column class detail.

**Out:** replacing the single hand-measured ~7-inch figure-width floor
documented on `?plot_pid5`'s `labels` argument with a per-level, multi-device
measurement → stays a `candidate` ROADMAP row; no change to any scored value,
axis limit, or break (IP2, D-029); no new plot argument.

## Acceptance criteria

- [x] AC1 With `labels = FALSE`, `plot_pid5()` pads the continuous score axis
      by 3% at both ends; with `labels = TRUE` the existing 3%/12% padding is
      unchanged. Asserted over all 10 legal `version` × `level` × `metric`
      combinations (3 × 2 × 2, less the two BF × facet cases `plot_pid5()`
      aborts), enumerated by the test from `pid_scales` rather than
      hand-listed, comparing `ggplot_build(p)$layout$panel_params[[i]]$x.range`
      for **every** panel `i` against that plot's `pid_norms`-derived scale
      limits (not against the range of the plotted values, which lie inside
      them).
- [x] AC2 No assertion in `tests/testthat/test-plot_pid5.R` reads a column of
      the plot object's internal data frame. Domain enumerated by parsing the
      file (`parse()` + AST walk for `$data` extraction on a ggplot object),
      which returns none; each such assertion instead reads
      `ggplot_build(p)$data`, `$layout`, or `layer_scales(p)`, recovering
      scale names through one shared test helper. Verified by mutation:
      consistently renaming the internal `stem` column in `R/plot_pid5.R`
      (the column *and* the `df$stem != "total"` filter at R/plot_pid5.R:366)
      leaves `devtools::test()` green; reverted after.
- [x] AC3 Every expectation call inside a `for` body in that file names its
      iteration on failure. Domain enumerated by the same AST walk, which
      reports no in-loop `expect_setequal()` or `expect_length()` call
      (neither accepts `info`) and an `info =` argument on every remaining
      in-loop expectation. One in-loop assertion is mutation-checked to show
      the iteration in the failure message.
- [x] AC4 `norm_pid5()` and `plot_pid5()` reject a non-numeric,
      non-logical score column through one helper defined in `R/util.R`. Each
      keeps its own headline naming its own argument; both emit one bullet per
      offending column carrying that column's class. Verified by mutation:
      inverting the helper's predicate turns both functions' guard tests red,
      so both demonstrably route through it. Tests fire each guard on a
      character column and on a factor column.
- [x] AC5 `NEWS.md` carries one entry per user-visible change in this
      milestone's Scope **In** list, and each entry has a test that fails
      without the behavior that entry asserts.
- [x] AC6 `Rscript -e 'devtools::document()'` and `Rscript -e 'devtools::test()'`
      clean, and `devtools::check()` clean (structural change to `R/util.R`).

## Coverage

- AC1 → T1, T2
- AC2 → T3, T4
- AC3 → T5
- AC4 → T6
- AC5 → T7
- AC6 → T8

## Tasks

- [x] T1 Write the failing test first: enumerate the 10 legal combinations
      from `pid_scales`, build with `labels = FALSE` and `labels = TRUE`, and
      assert `x.range` per panel against the scale limits.
- [x] T2 Make the `expand =` at R/plot_pid5.R:399 conditional on
      `show_labels`; update the comment block at R/plot_pid5.R:395-398 and
      R/plot_pid5.R:416-425 to say the padding is the label's room and is not
      taken when there are no labels.
- [x] T3 Add the shared test helper that recovers scale names and panel
      assignment from built layer data (`ggplot_build(p)$data` +
      `$layout$panel_scales_y`), plus the AST-walk check AC2/AC3 enumerate.
- [x] T4 Convert all 20 internal-frame reads in
      `tests/testthat/test-plot_pid5.R` through that helper; run the
      consistent-rename mutation and record it.
- [x] T5 Recast the in-loop `expect_setequal()`/`expect_length()` calls per
      the M32/M39 form in LESSONS and add `info =` to the remaining in-loop
      expectations; mutation-check one.
- [x] T6 Extract the shared numeric-column guard into `R/util.R` from
      R/norm_pid5.R:202-224 and R/plot_pid5.R:164-175, parameterized on the
      headline; point both call sites at it (keeping `plot_pid5()`'s guard
      ahead of its NA-drop branch) and run the predicate-inversion mutation.
- [x] T7 NEWS.md entry per Scope-In user-visible change.
- [x] T8 `devtools::document()`, `devtools::test()`, `devtools::check()`.

## Work log

- 2026-08-06: created by /milestone-plan.
- 2026-08-06: criteria audit ([O], fresh context) returned findings on all five drafted criteria — AC1 pinned no padding value and compared against the wrong range, AC2 scoped its mutation to `stem` alone and leaned on a grep proxy, AC3 was unsatisfiable as written, AC4's definition-site grep could not show both functions route through the helper, AC5 hand-listed the changes it quantified over. All fixed in the wording above before the gate; three residual either-way calls became the gate's questions.
- 2026-08-13: T3/T4/T5 done. New `built_profile()` recovers each drawn point's printed name, stem, value and panel from built layer data alone (the printed name by indexing that panel's own discrete scale with the point's y; the stem through `pid_scales`/`pid_domains`, never the plot), returning rows in drawn order. Cross-checked against the internal frame on every combination before the reads were removed: stem, printed name, value and panel all agreed.
- 2026-08-13: the AC2 self-check rejects `data` taken off a bare symbol by `$` **or** `[[` — checking only `$` would have made it a rule about spelling, since `b[["data"]]` reads exactly what `b$data` reads. Built data is reached through the `ggplot_build()` call itself or through `layer_data_for()`.
- 2026-08-13: AC2 mutation run — renaming the internal `stem` column to `scaleStem` (the column and the `df$stem != "total"` filter) left `test-plot_pid5.R` green at 251 passing; reverted, and `git diff` confirms R/plot_pid5.R is back to its committed state.
- 2026-08-13: AC3 mutation run — breaking the per-panel facet-membership assertion produced five failures each naming its domain ("Negative affectivity", "Detachment", ...) from `info`; reverted.
- 2026-08-13: the recast follows the M32/M39 LESSONS form but drops its `unique()`: a facet drawn twice is a real defect here, so comparing sorted values without de-duplicating is strictly stronger than the `expect_setequal()` it replaces.
- 2026-08-13: T6/T7/T8 done. `validate_numeric_columns()` in R/util.R now carries the shared predicate and the per-column class bullets; each caller passes its own headline and closing line as functions of the offending-column count, so `norm_pid5()`'s test-pinned wording is unchanged and `plot_pid5()` gains the per-column class detail it lacked.
- 2026-08-13: AC4 mutation run — inverting the helper's predicate broke both guard tests by name (`plot_pid5()`'s "a non-numeric normed column is refused, not reported as missing" and `norm_pid5()`'s "aborts on a non-numeric score column rather than coercing"), 29 and 26 tests broken in the two files; reverted. A first reading of that run reported 0 plot failures, which was a measurement error — testthat records a thrown error in `error`, not `failed`, and the mutation makes `plot_pid5()` abort outright rather than fail an expectation.
- 2026-08-13: two NEWS entries, one per user-visible Scope-In change (the conditional padding, and the per-column guard detail); the other two Scope-In items are test-only and get none.
- 2026-08-13: T8 clean — `devtools::document()` leaves no diff, `devtools::test()` 12035 passing, `devtools::check()` 0 errors / 0 warnings / 0 notes. Status to review.
- 2026-08-06: plan gate chose keeping each function's own error headline with shared per-column detail over one flattened message because flattening would degrade `norm_pid5()`'s existing `{.arg scores}` blaming; falsified by a user report that the two messages read as unrelated errors.
- 2026-08-06: plan gate chose converting all 20 internal-frame reads through one shared test helper over converting only the mechanically substitutable ones because a partial conversion leaves the fragility the item was raised about; falsified by the helper proving unable to recover a property some assertion needs from built data.
- 2026-08-06: plan gate chose 3% symmetric padding under `labels = FALSE` over ggplot2's 5% default because it matches the padding already used on the left; falsified by a report that label-free profiles read as cramped.
- 2026-08-13: in-progress on `m40-plot-review-residue`; ggplot2/flextable/officer/lavaan installed locally (absent before), baseline `devtools::test()` green at 11964 passing on ggplot2 4.0.3.
- 2026-08-13: measured the AC2/AC3 domains by AST walk before writing anything — 27 `$data` reads on a bare symbol (24 on the plot object, 3 on a `ggplot_build()` result) against T4's hand-written "20", and 39 in-loop expectations of which 3 `expect_setequal`, 1 `expect_length`, 2 `expect_gt` and 2 `expect_no_warning` take no `info`.
- 2026-08-13: T1/T2 done. The new test enumerates the 10 legal combinations from `pid_scales` (a version offers a facet level iff its scales table names Facets) and compares every panel's `x.range` against a `pid_norms`-derived span; it failed on all `labels = FALSE` cases and passed every `labels = TRUE` case before the fix, which is the control identifying the failure. `expand` is now conditional on `show_labels`. `devtools::test()` 12025 passing, 0 failures.
- 2026-08-13: `devtools::document()` also rewrote DESCRIPTION's `Config/roxygen2/version` 8.0.0 -> 8.1.0 (local roxygen2 is newer than the one that last documented the repo); committed because the review consistency-gate requires `document()` to leave no diff.
- 2026-08-06: plan scoped the ~7-inch figure-width floor out because M39's own lesson makes any such promise device- and font-dependent, so it needs a measurement procedure rather than a second hand measurement; it stays a candidate row.

## Decisions

- 2026-08-13 (M40-D1): The converted assertions claim the **drawn** top-to-bottom
  order, not the plot's internal row order. Built layer data carries no `stem`
  column, so the recoverable order is which panel a point sits in and how high
  in it; that is also the order a reader sees. Rejected: additionally asserting
  that the built layer preserves the input frame's row order, which would keep
  the two claims separate at the cost of pinning a ggplot2 internal this package
  makes no promise about. Chosen at the 2026-08-13 implementation gate.
  Falsified by a reordering defect that changes the internal frame while leaving
  the drawn order correct.

- 2026-08-13 (M40-D2): The AC2 self-check discriminates by **syntactic shape**:
  `$data` on a bare symbol is rejected, and built data is reached through a
  `ggplot_build()` call, whose left side is a call rather than a symbol. This
  needs no dataflow tracking and cannot be fooled by naming. Its cost is
  conservatism -- `b <- ggplot_build(p); b$data` is legitimate but rejected, so
  the three existing reads of that form are rewritten. Rejected: tracking which
  symbols are assigned from `ggplot_build()`, which puts a variable tracker in
  the test file. Chosen at the 2026-08-13 implementation gate.

- 2026-08-13 (M40-D3): The shared numeric-column guard takes each caller's
  headline and closing line as functions of the offending-column count, rather
  than composing both messages from an argument name. `norm_pid5()`'s wording is
  pinned character-for-character by existing tests, in singular and plural, and
  carries a closing sentence `plot_pid5()` does not; a template would change it
  and force those tests to be rewritten to match the refactor -- which is the
  test rewriting itself to fit the code. Chosen at the 2026-08-13 implementation
  gate.

## Review

### Acceptance criteria

- **AC1 — verified.** `profile_cases()` enumerates **10** combinations from `pid_scales` (FULL/SF x domain/facet x t/percentile, plus BF domain x 2); the two BF facet cases are excluded because `pid_scales[["BF"]]` names `Domain` and no `Facet` column, not because "BF" is named in the test. Measured `x.range` against the `pid_norms`-derived span, every panel: FULL facet t limits [30,100] -> [27.9,108.4] with labels (3.00%/12.00%) and [27.9,102.1] without (3.00%/3.00%); BF domain percentile limits [0,100] -> [-3,112] and [-3,103]. The test asserts this for every panel of all 10 combinations, both ways.

- **AC2 — verified.** The in-file AST walk (`test_file_calls()`, `parse()` + descent tracking `for` bodies) reports **no** `data` extraction on a bare symbol, by `$` or by `[[`; the self-check passes. Every such read now goes through `ggplot_build(p)$data`, `layer_data_for()`, or the new `built_profile()`. Mutation: renaming the internal `stem` column to `scaleStem` (the column *and* the `df$stem != "total"` filter) left the file green at 258 passing; reverted, `git diff` clean. Negative control: reintroducing one `p$data` read turned the self-check red, so it fails when its rule is broken.

- **AC3 — verified.** The same walk reports no in-loop `expect_setequal()` or `expect_length()` and an `info =` argument on every remaining in-loop expectation; the self-check passes and asserts it saw >20 in-loop expectations, so it cannot pass vacuously. The two other `info`-less kinds found in the measured domain (`expect_gt`, `expect_no_warning`) were recast rather than excused. Mutation: breaking the per-panel facet-membership assertion produced one failure per domain, each naming its iteration ("Negative affectivity", "Detachment", ...); reverted.

- **AC4 — verified.** Both functions route through `validate_numeric_columns()` in `R/util.R`; each passes its own headline and closing line as functions of the offending-column count, so `norm_pid5()`'s test-pinned wording is byte-identical (`test-norm_pid5.R` has zero diff on this branch) and `plot_pid5()` gains per-column class bullets. Mutation: inverting the helper's predicate broke **29** tests in `test-plot_pid5.R` and **26** in `test-norm_pid5.R`, including both guard tests by name -- "a non-numeric normed column is refused, not reported as missing" and "norm_pid5() aborts on a non-numeric score column rather than coercing" -- so both demonstrably route through it. Reverted, `git diff` clean. Each guard is fired on a character column and on a factor column; the plot guard is additionally fired on both at once and on an ordered factor, asserting `ordered/factor` rather than a collapsed `<ordered>`.

- **AC5 — verified.** Two of the four Scope-In changes are user-visible and each has one NEWS entry; the other two (converting the tests off the internal frame, labelling in-loop assertions) are test-only and get none. Each entry rests on a test that fails without its behavior, shown by mutation: reverting `expand` to unconditional reds the padding test (30 assertions); replacing the per-column class detail with a classless bullet reds the plot guard test. Both reverted.

- **AC6 — verified.** `devtools::document()` leaves no diff in `man/`, `NAMESPACE`, `DESCRIPTION` or `R/`. `devtools::test()` 12035 passing, 0 failures, 1 skip. `devtools::check()` **0 errors, 0 warnings, 0 notes** (2m 4s).
