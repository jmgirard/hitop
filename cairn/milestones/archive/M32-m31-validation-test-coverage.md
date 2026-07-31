# M32: Test coverage for M31's argument-validation additions

**Status:** done (2026-07-31, PR #35 https://github.com/jmgirard/hitop/pull/35)

**Goal:** Give M31's three untested argument-validation additions assertions that
fail when the addition is reverted, and make the `norm_shift()` loop name its failing case.

**Outcome:** Tests only; `R/` untouched. `test-validate.R` gained a supplied-type block
(`validate_scales()` must name the class it was handed) and an `arg`-interpolation block via
`norm_pid5(scores = TRUE)`, the sole caller overriding the `arg` default. `test-item-guards.R`
gained `warn_item_order()` attribution at both call sites — `score_pid5()` (threaded through
`score_engine()`/`prep_items()`) and `validity_pid5()` (`caller_env()` default) — via a `warner()`
helper substituting `quote(no_call())` for a NULL `conditionCall()`, so each site fails
independently instead of the first aborting the block. `test-norm_pid5.R:418`'s
`expect_setequal()` became a sorted-unique `expect_equal()` carrying the loop's `info`.

**Decisions:** None milestone-local. No plan amendments; the criteria audit corrected AC4's
verification clause before the file was written (`covered_scales` is version-keyed, so no
per-case expected set exists to mutate).

**Review:** Blame-history and prior-review lenses returned nothing — the first confirming the
reshape preserves the whole-set coverage M31 bought, the second that this closes M31's
findings 78/68. Diff-bug returned 13, all below 80, none actioned, all logged with scores
(top two: 72 dead `suppressMessages()`, 52 unpinned condition class); no candidate row.
