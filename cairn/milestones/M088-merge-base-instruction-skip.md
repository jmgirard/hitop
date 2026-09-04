# M088: The instruction-object merge-base block skips when its comparison is vacuous

- **Status:** in-progress
- **Priority:** high
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** —
- **Resolves:** —
- **Surface tier:** internal — a test-suite repair; no exported behavior, dataset column, or artifact changes
- **Branch/PR:** `m088-merge-base-instruction-skip`

## Goal

`test-item-number-merge-base.R`'s instruction-object block honours the skip contract its
own file header states, so the suite is green on every branch cut from a `main` that
already carries M086's retype.

## Scope

**In:** a `merge_base_instructions(sha)` helper in `tests/testthat/test-item-number-merge-base.R`,
matching `merge_base_item_numbers()` (`:81`) and `merge_base_responses()` (`:156`): all four
instruction objects read from the merge base first, per-object `moved` computed, the skip
decided once over all of them, the list returned. The block at `:222` consumes it and keeps
its `expect_setequal()` vacuity guard over the precomputed set. The file's header comment
extends to say the fourth block skips on the same terms.

**Out:** any meta-check asserting that merge-base blocks in general are skip-guarded → declined
at this plan gate (the other blocks are already guarded; M085's lesson makes a source-scanning
guard skip silently under `R CMD check`). Deleting the now-permanently-skipping blocks →
declined at this gate. The three sibling merge-base files → untouched; swept and found guarded.

## Acceptance criteria

- [ ] AC1: On a branch cut from `main` at `c60f5f35` or later, `Rscript -e 'devtools::test()'`
      reports 0 failures.
- [ ] AC2: On that same branch, a `reporter = "summary"` run of
      `test-item-number-merge-base.R` reports the block "retyping the instruction option
      values moved nothing else" as skipped, with a reason naming that the merge base already
      stores the instruction option values as integers.
- [ ] AC3: Called with `sha = "c3be8505"` — the commit immediately preceding `da1d6f09`, the
      merge that made the shipped instruction option values integers — `merge_base_instructions()`
      does not skip, and the set of objects it reports as moved equals
      `c("hitopsr_instructions", "hitopbr_instructions")`.
- [ ] AC4: The block calls no `testthat::skip_if()` inside its per-object loop, so no object is
      abandoned mid-loop and reported as one clean skip (the M086 lesson, already stated in the
      comments above both sibling helpers).

## Coverage

- AC1 → T1, T2, T3
- AC2 → T2, T3
- AC3 → T2, T4
- AC4 → T2

## Tasks

- [x] T1: RED evidence. Cut the milestone branch from `main`, run
      `testthat::test_local(filter = "item-number-merge-base", reporter = "summary")`, and record
      the failure at `test-item-number-merge-base.R:232` verbatim in the work log.
- [x] T2: Add `merge_base_instructions(sha)` to `tests/testthat/test-item-number-merge-base.R`,
      shaped after `merge_base_responses()` (`:156`): `lapply()` over `instruction_objects`
      reading each from `merge_base_sysdata(sha)`, each entry `list(old =, moved =)` where
      `moved` is `!is.null(old$options) && !is.integer(old$options$value)`; one
      `testthat::skip_if(!any(moved), "the merge base already stores every instruction option
      value as integer")` after the loop; return the named list. Rewrite the block at `:222` to
      consume it — compare each object, then `expect_setequal()` over
      `names(Filter(function(x) x$moved, bases))`. Extend the file's header comment (`:1-9`) to
      cover the fourth block.
- [ ] T3: GREEN evidence. On the branch, re-run the file (expect 5 skips, 0 failures) and then
      `Rscript -e 'devtools::test()'` for AC1. Record both.
- [ ] T4: Discrimination evidence. In one `devtools::load_all()` session, evaluate a
      `test_that()` block calling `merge_base_instructions("c3be8505")` and asserting AC3's
      outcome — it does not skip, and its moved set is the two instruction objects. Record the
      reporter output. No clone, no ref manipulation: `merge_base_sysdata()` takes the sha as an
      argument.

## Work log

- 2026-09-04: created by /milestone-plan.
- 2026-09-04: plan gate chose a precompute-then-skip helper matching the two siblings over deleting the file's four now-permanently-skipping blocks, because the file's own header states the skip contract ("never fail a later one") and deleting would end the merge-base proof for both M081's and M086's retypes; falsified by evidence that the shipped objects can be re-proved from a source other than the merge base.
- 2026-09-04: plan gate chose a precompute-then-skip helper over deleting the `expect_setequal()` vacuity guard, because dropping it reinstates the exact defect M086's review fixed — the block would pass on a merge base already carrying the change while asserting nothing; falsified by evidence that some other test would fail if the instruction option values regressed to double.
- 2026-09-04: plan gate chose no recurrence meta-check over adding one, because the sweep found every other merge-base block already guarded and M085's lesson makes a source-scanning guard skip silently under `R CMD check` — local-only, never in CI; falsified by a second unguarded merge-base block appearing in a later milestone.
- 2026-09-04: T1 RED evidence on `m088-merge-base-instruction-skip` at ca70359c (merge base cb409078). `test_local(filter = "item-number-merge-base", reporter = "summary")`: 4 skips, 1 failure — `Failure ('test-item-number-merge-base.R:232:3'): retyping the instruction / Expected `moved` to have the same values as `c("hitopsr_instructions", "hitopbr_instructions")`. / Actual: / Expected: "hitopsr_instructions", "hitopbr_instructions" / Absent: "hitopsr_instructions", "hitopbr_instructions"` — i.e. `moved` is empty because the merge base already stores the option values as integer.
- 2026-09-04: criteria audit ran in reduced mode ([O] fresh-context reader, internal tier). AC1 and AC2 clean on first pass. AC3 returned findings on all three questions and was rewritten twice — first wording unbounded over "any branch whose merge base predates the retype", second pinned the commit but proved it through a scratch clone with a manipulated `origin/HEAD` (disproportionate for internal tier). Third wording proves it by one in-process call and is clean on all three.

- 2026-09-04: T2 added `merge_base_instructions(sha)` to `test-item-number-merge-base.R` — one `merge_base_sysdata()` read, per-object `list(old =, moved =)` over the four instruction objects, a single `testthat::skip_if()` after the loop, the named list returned; the block now consumes it and keeps `expect_setequal()` over `names(Filter(function(x) x$moved, bases))`. The file header extended to say the instruction block skips on the same terms. `devtools::test()`: 0 failures, 0 warnings, 13 skips, 17277 passes.

## Decisions

## Review
