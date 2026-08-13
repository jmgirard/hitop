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
- AC2 → T3, T4, T9, T12, T13
- AC3 → T5, T10
- AC4 → T6, T11
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
- [x] T9 (review return, AC2) Widen the self-check's receiver rule: reject `data`
      taken off ANY receiver except a `ggplot_build()` call, and name
      `getElement()` alongside `$` and `[[`.
- [x] T10 (review return, AC3) Resolve the called function's name through a
      `pkg::` qualifier so `testthat::expect_*()` is classified as an expectation.
- [x] T11 (review return, AC4) Restore `{.arg data}` to `plot_pid5()`'s headline
      and give its closing line an action, mirroring `norm_pid5()`.
- [x] T12 (review finding 8) Assert the axis's whole drawn vocabulary, not only
      the names carrying a point, on both the pinned and the trained branch.
- [x] T13 (review finding 7) Give `stem_for_label()` the unmapped-stem fallback
      `plot_scale_labels()` has, and state the dropped-scale claim positively.

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
- 2026-08-13: review returned M40 to in-progress (defect return 1). Three criteria fail as written, each verified in-session by injection or by rendering: AC2 — the self-check's `is.name(e[[2]])` receiver restriction lets `ps[[1]]$data` read the internal frame with both guards green; AC3 — its `is.name(call[[1]])` restriction never classifies `testthat::expect_equal()` as an expectation, so an in-loop qualified call with no `info` passes; AC4 — `plot_pid5()`'s message names no argument and offers no code action, where main's named `{.arg data}`. All other gate checks and CI passed.
- 2026-08-13: review evidence gathered fresh for all six criteria before the return; the three passing ones (AC1, AC5, AC6) stand on their own recorded evidence and their boxes are unticked only because the milestone reopened.

- 2026-08-13: return fixes done. AC2 — the self-check now rejects `data` off any receiver but a `ggplot_build()` call and names `getElement()`; AC3 — expectation names resolve through `::`. Both verified against the exact probe that defeated them: `ps[[1]]$data`, `(p1)$data`, `getElement(p1, "data")` and an in-loop `testthat::expect_equal()` with no `info` previously left both guards green and now red them, while the file without the probe stays green.
- 2026-08-13: AC4 — `plot_pid5()`'s headline names `{.arg data}` again and its closing line now says what to do, both pluralized off the count (which also gives `plot_pid5()`'s `info` closure a use for its `n`, review finding 12). Rendered and checked, not assumed.
- 2026-08-13: finding 8 — a ghost factor level turns out to be invisible in built data on BOTH branches (pinned limits unfacetted, unused levels dropped by per-panel training), so it never reaches the axis or the reader. What the old `levels(p$data$scale)` assertion actually protected is the axis vocabulary, now asserted on the drawn axis for the unfacetted branch and per panel for the facet branch; making `plot_scale_labels()` return stems reds it with 17 failures.
- 2026-08-13: finding 7 — the vacuity is fixed at its root (finding 6): `stem_for_label()` gained the unmapped-stem fallback its production counterpart has, and the dropped-scale claim is now positive (the four survivors in order). Forcing the recovery to `NA` reds it, where the old `expect_false(... %in% ...)` form passed.

- 2026-08-13: return fixes verified clean — `devtools::document()` no diff, `devtools::test()` 12052 passing, `devtools::check()` 0/0/0. `cairn_validate` passes; its one new advisory is the >10-task split tripwire, fired by five review-return tasks appended to finished work rather than by scope growth. Status back to review.

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

### Independent review (2026-08-13)

Three fresh-context lenses. The **[S] blame-history** lens returned zero findings
(it traced the padding change to PR #42/M39 and confirmed the M39 label-clipping
bug cannot recur where no label is drawn; it found no deleted assertion without an
equal-or-stronger replacement). The **[S] prior-review** lens returned zero findings
(M38's and M39's archived `## Review` findings are untouched; the `gh` probe found
no real inline PR comments, so the thread walk was skipped). The **[O] diff-bug**
lens returned 24. An **[S] scorer** that did not generate them scored all 24 against
the rubric; three cleared 80.

**Actioned (>=80), verbatim, each verified in this session before it was recorded:**

- **Finding 1 (84) — AC2 failure.** `tests/testthat/test-plot_pid5.R:199` — the AC2
  self-check's `is.name(e[[2]])` requirement lets any non-symbol receiver read the
  internal frame. `extracts_data()` only fires when the thing on the left of
  `$`/`[[` is a bare symbol. Verified: appending
  `ps <- list(plot_pid5(normed_one("BF"), version="BF")); expect_true(length(ps[[1]]$data$stem) == 6)`
  to the file left both self-checks and the entire file green. `(p)$data`,
  `plots[[1]]$data`, `getElement(p, "data")` and `p$layers[[1]]$data` all pass the
  guard. AC2 promises "No assertion in `tests/testthat/test-plot_pid5.R` reads a
  column of the plot object's internal data frame"; the check that certifies it
  enforces something considerably narrower, and `plots[[i]]$data` is the realistic
  form (a loop over several plots is exactly the shape this file uses).
  Falsifies AC2's "Domain enumerated by parsing the file (`parse()` + AST walk for
  `$data` extraction on a ggplot object), which returns none" -- a `[[`-indexed
  receiver is inside that procedure's own domain.

- **Finding 2 (84) — AC3 failure.** `tests/testthat/test-plot_pid5.R:213` — the AC3
  self-check's `is.name(hit$call[[1]])` requirement misses every namespace-qualified
  expectation. `testthat::expect_equal(...)` has a `::` *call* in position 1, not a
  name, so it is never classified as an expectation. Verified: injecting
  `for (v in c("BF")) { testthat::expect_equal(nrow(ps[[1]]$data), 6L) }` — an
  in-loop expectation with no `info` — left the file green. The file already uses
  `testthat::fail()` and `testthat::capture_warnings()`, so qualified calls are an
  established local style and this hole is live.
  Falsifies AC3's "an `info =` argument on every remaining in-loop expectation" --
  a `::`-qualified call in a `for` body is inside that procedure's own domain.

- **Finding 11 (87) — AC4 failure.** `R/plot_pid5.R:166-178` — `plot_pid5()`'s
  message lost its only pointer at the offending argument, and gained no remedy.
  Main's bullet was `"Not numeric in {.arg data}: {.val {cols[bad_type]}}."`. The new
  message is *"The normed columns this profile plots must be numeric. x
  "pid_detachment_t" is <character>. i A factor's integer codes are not its scores..."*
  — it never names `data`, and its `i` bullet contains no `{.code}` action (norm's
  does: *"Convert them before calling `norm_pid5()`"*). CLAUDE.md requires
  "actionable `{.code dplyr::filter(...)}` suggestions". NEWS.md presents this change
  as unambiguously an improvement; the per-column classes are new, but the argument
  attribution is a regression.
  Falsifies AC4's "Each keeps its own headline naming its own argument". Verified by
  rendering both messages: `norm_pid5()`'s names `scores` and offers
  `norm_pid5()` as an action; `plot_pid5()`'s matches neither `data` nor any code
  action, where `main`'s did name `{.arg data}`.

**Below 80 — excluded from the actioned list, logged (21 findings):**

- (78) F8 `test-plot_pid5.R` — the axis-label claim narrowed from the discrete scale's whole level vocabulary to the names at drawn points; a ghost factor level would no longer red.
- (75) F7 `test-plot_pid5.R:550` — `expect_false("detachment" %in% built_profile(...)$stem)` passes vacuously if the stem mapping ever returns `NA`.
- (62) F24 milestone file — AC2/AC3's own text is stronger than the checks that discharge them; overlaps F1 at the framing level.
- (55) F5 `test-plot_pid5.R:111` — `panel_scales_y` indexed by `PANEL` rather than `layout$SCALE_Y`; equal today, would diverge under `facet_wrap`.
- (55) F6 `test-plot_pid5.R:90` — `stem_for_label()` lacks `plot_scale_labels()`'s unmapped-stem fallback and returns `NA` silently.
- (55) F15 `test-plot_pid5.R:112` — `limits[[pts$y[[k]]]]` fails as an opaque subscript error rather than a named `testthat::fail()`.
- (50) F9 `test-plot_pid5.R` — two tests now make the identical FULL drawn-order claim, and a surviving comment still describes them as distinct.
- (45) F12 `R/plot_pid5.R` — `plot_pid5()`'s `info` closure ignores its `n` argument, so M40-D3's design is exercised by one caller of two.
- (42) F19 `test-plot_pid5.R:255` — the enumeration's legality rule (`"Facet" %in% names(...)`) is correlated with, not identical to, `plot_pid5()`'s own `BF` gate.
- (40) F4 `test-plot_pid5.R:233` — the check tests for an `info` argument's presence, never that its value varies by iteration.
- (40) F23 `test-plot_pid5.R:105` — `built_profile()`'s `version`/`level` are caller-supplied and unchecked against the plot.
- (38) F20 `DESCRIPTION` — the roxygen2 version bump is outside Scope In; justified by AC6 but arguably its own trivial commit.
- (35) F3 `test-plot_pid5.R:163` — only `for` is recognized as a loop; `while`/`repeat`/`lapply` bodies are not.
- (35) F17 `test-plot_pid5.R` — the `expect_gt()` -> `expect_true()` recast gains `info` but loses printed operand values.
- (32) F16 `test-plot_pid5.R` — the anti-vacuity thresholds (500, 20) are loose against measured 1354 and 39.
- (30) F10 `R/util.R:257` — the new helper takes callbacks and calls `cli_abort()` directly, unlike every sibling validator; M40-D3 records this as deliberate.
- (30) F13 `R/util.R:257` — `headline`/`info` are unvalidated required callbacks.
- (30) F14 `test-plot_pid5.R:106` — `built_profile()` builds the plot twice.
- (25) F18 `test-plot_pid5.R:309` — the expected padding restates the implementation's own constants.
- (20) F21 `NEWS.md` — both entries describe changes to a function introduced in the same unreleased section; pre-existing pattern.
- (15) F22 `R/plot_pid5.R:28` — an unreflowed roxygen line after the text splice.

### Gate result

Returned to `in-progress`. Three acceptance criteria fail as written: AC2 and AC3
inside the domain of the AST walk each names, and AC4 on `plot_pid5()`'s headline.
Everything else passed: `cairn_validate` clean, `devtools::document()` no diff,
`pkgdown::check_pkgdown()` clean, `devtools::check()` 0/0/0, and CI green on all
seven jobs of PR #44.
