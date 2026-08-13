# M40: Retire the M38/M39 plot-review residue

- **Status:** in-progress
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** IP2, GP3, GP4
- **Branch/PR:** `m40-plot-review-residue`

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

- [ ] AC1 With `labels = FALSE`, `plot_pid5()` pads the continuous score axis
      by 3% at both ends; with `labels = TRUE` the existing 3%/12% padding is
      unchanged. Asserted over all 10 legal `version` × `level` × `metric`
      combinations (3 × 2 × 2, less the two BF × facet cases `plot_pid5()`
      aborts), enumerated by the test from `pid_scales` rather than
      hand-listed, comparing `ggplot_build(p)$layout$panel_params[[i]]$x.range`
      for **every** panel `i` against that plot's `pid_norms`-derived scale
      limits (not against the range of the plotted values, which lie inside
      them).
- [ ] AC2 No assertion in `tests/testthat/test-plot_pid5.R` reads a column of
      the plot object's internal data frame. Domain enumerated by parsing the
      file (`parse()` + AST walk for `$data` extraction on a ggplot object),
      which returns none; each such assertion instead reads
      `ggplot_build(p)$data`, `$layout`, or `layer_scales(p)`, recovering
      scale names through one shared test helper. Verified by mutation:
      consistently renaming the internal `stem` column in `R/plot_pid5.R`
      (the column *and* the `df$stem != "total"` filter at R/plot_pid5.R:366)
      leaves `devtools::test()` green; reverted after.
- [ ] AC3 Every expectation call inside a `for` body in that file names its
      iteration on failure. Domain enumerated by the same AST walk, which
      reports no in-loop `expect_setequal()` or `expect_length()` call
      (neither accepts `info`) and an `info =` argument on every remaining
      in-loop expectation. One in-loop assertion is mutation-checked to show
      the iteration in the failure message.
- [ ] AC4 `norm_pid5()` and `plot_pid5()` reject a non-numeric,
      non-logical score column through one helper defined in `R/util.R`. Each
      keeps its own headline naming its own argument; both emit one bullet per
      offending column carrying that column's class. Verified by mutation:
      inverting the helper's predicate turns both functions' guard tests red,
      so both demonstrably route through it. Tests fire each guard on a
      character column and on a factor column.
- [ ] AC5 `NEWS.md` carries one entry per user-visible change in this
      milestone's Scope **In** list, and each entry has a test that fails
      without the behavior that entry asserts.
- [ ] AC6 `Rscript -e 'devtools::document()'` and `Rscript -e 'devtools::test()'`
      clean, and `devtools::check()` clean (structural change to `R/util.R`).

## Coverage

- AC1 → T1, T2
- AC2 → T3, T4
- AC3 → T5
- AC4 → T6
- AC5 → T7
- AC6 → T8

## Tasks

- [ ] T1 Write the failing test first: enumerate the 10 legal combinations
      from `pid_scales`, build with `labels = FALSE` and `labels = TRUE`, and
      assert `x.range` per panel against the scale limits.
- [ ] T2 Make the `expand =` at R/plot_pid5.R:399 conditional on
      `show_labels`; update the comment block at R/plot_pid5.R:395-398 and
      R/plot_pid5.R:416-425 to say the padding is the label's room and is not
      taken when there are no labels.
- [ ] T3 Add the shared test helper that recovers scale names and panel
      assignment from built layer data (`ggplot_build(p)$data` +
      `$layout$panel_scales_y`), plus the AST-walk check AC2/AC3 enumerate.
- [ ] T4 Convert all 20 internal-frame reads in
      `tests/testthat/test-plot_pid5.R` through that helper; run the
      consistent-rename mutation and record it.
- [ ] T5 Recast the in-loop `expect_setequal()`/`expect_length()` calls per
      the M32/M39 form in LESSONS and add `info =` to the remaining in-loop
      expectations; mutation-check one.
- [ ] T6 Extract the shared numeric-column guard into `R/util.R` from
      R/norm_pid5.R:202-224 and R/plot_pid5.R:164-175, parameterized on the
      headline; point both call sites at it (keeping `plot_pid5()`'s guard
      ahead of its NA-drop branch) and run the predicate-inversion mutation.
- [ ] T7 NEWS.md entry per Scope-In user-visible change.
- [ ] T8 `devtools::document()`, `devtools::test()`, `devtools::check()`.

## Work log

- 2026-08-06: created by /milestone-plan.
- 2026-08-06: criteria audit ([O], fresh context) returned findings on all five drafted criteria — AC1 pinned no padding value and compared against the wrong range, AC2 scoped its mutation to `stem` alone and leaned on a grep proxy, AC3 was unsatisfiable as written, AC4's definition-site grep could not show both functions route through the helper, AC5 hand-listed the changes it quantified over. All fixed in the wording above before the gate; three residual either-way calls became the gate's questions.
- 2026-08-06: plan gate chose keeping each function's own error headline with shared per-column detail over one flattened message because flattening would degrade `norm_pid5()`'s existing `{.arg scores}` blaming; falsified by a user report that the two messages read as unrelated errors.
- 2026-08-06: plan gate chose converting all 20 internal-frame reads through one shared test helper over converting only the mechanically substitutable ones because a partial conversion leaves the fragility the item was raised about; falsified by the helper proving unable to recover a property some assertion needs from built data.
- 2026-08-06: plan gate chose 3% symmetric padding under `labels = FALSE` over ggplot2's 5% default because it matches the padding already used on the left; falsified by a report that label-free profiles read as cramped.
- 2026-08-13: in-progress on `m40-plot-review-residue`; ggplot2/flextable/officer/lavaan installed locally (absent before), baseline `devtools::test()` green at 11964 passing on ggplot2 4.0.3.
- 2026-08-13: measured the AC2/AC3 domains by AST walk before writing anything — 27 `$data` reads on a bare symbol (24 on the plot object, 3 on a `ggplot_build()` result) against T4's hand-written "20", and 39 in-loop expectations of which 3 `expect_setequal`, 1 `expect_length`, 2 `expect_gt` and 2 `expect_no_warning` take no `info`.
- 2026-08-06: plan scoped the ~7-inch figure-width floor out because M39's own lesson makes any such promise device- and font-dependent, so it needs a measurement procedure rather than a second hand measurement; it stays a candidate row.

## Decisions

## Review
