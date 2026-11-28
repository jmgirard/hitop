# M029: `norm_pid5()` hygiene and robustness

**Status:** done (2026-07-31, PR #32 https://github.com/jmgirard/hitop/pull/32)

**Goal:** Close the sub-threshold review findings M027 and M028 left on the PID-5
norming family: fail loudly on bad input, never return a silently wrong number.

**Outcome:** New `strip_prefix()` (`R/util.R`) matches `prefix` literally in
`norm_pid5()` and `rank_scales()`, whose roxygen dropped its regex promise
(breaking). `norm_pid5()` aborts on duplicated or non-numeric `scores`, with
`validate_scales`/`_items_present`/`_item_uniqueness` taking an `arg` so the
blame names `scores`. `norm_metric(scale, version)` aborts on a covered scale
its three metric vectors do not name. `norm_shift()` documents the PRD `na.rm`
coupling. All four reports are `cli_warn()`, so one call silences the function.

**Decisions:** None milestone-local; implements D-025 (all four reports warn)
and D-026 (literal `prefix`). Two gated amendments: Scope grew to NEWS and the
vignettes' norming prose; AC3's "passes unedited" clause conflicted with AC5.

**Review:** Blame-history and prior-review lenses found no regressions;
diff-bug returned 28, of which 4 scored >=80 and were fixed — three roxygen
edits a failed script silently dropped while the log recorded them as done,
and AC2's untested `scores`-naming claim. 24 logged; residue to a candidate.
