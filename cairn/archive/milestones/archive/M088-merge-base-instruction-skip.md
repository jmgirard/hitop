# M088: The instruction-object merge-base block skips when its comparison is vacuous

**Status:** done (2026-09-04, PR #96 https://github.com/jmgirard/hitop/pull/96)

**Goal:** `test-item-number-merge-base.R`'s instruction-object block honours the skip contract its
own header states, so the suite is green on every branch cut from a `main` carrying M086's retype.

**Outcome:** `merge_base_instructions(sha)` joins `merge_base_item_numbers()` and
`merge_base_responses()` in `tests/testthat/test-item-number-merge-base.R`: it reads all four
instruction objects from the merge base with `exists()`/`get(inherits = FALSE)`, computes each
object's `moved` as `!identical(retype_instructions(old), old)` through one retype definition the
block itself also applies, and decides a single `testthat::skip_if()` over all of them after the
read loop — never inside the per-object comparison. The block keeps its `expect_setequal()`
vacuity guard over the precomputed moved set; the header now states that every merge-base block
skips on those terms. Test-only: nothing exported, no dataset, no artifact changed.

**Decisions:** none. Three plan-gate choices sit in the work log (git): the precompute-then-skip
helper over deleting the four now-permanently-skipping blocks, over dropping the vacuity guard,
and no recurrence meta-check (M085's lesson: a source-scanning guard skips under `R CMD check`).

**Review:** full three-lens fan-out. Eight findings logged; seven triaged fix-now and fixed on the
branch, moving the helper onto the shape both siblings use; R1 (an off-by-two apposition in AC3's
wording) recorded as a correction with the criterion text standing. Re-verified: `devtools::test()`
0 failures / 17277 passes, `devtools::check()` 0/0/0, 8/8 CI green. Retired one LESSONS line
(M034's fixture-table technique) to fit M088's own lesson in budget.
