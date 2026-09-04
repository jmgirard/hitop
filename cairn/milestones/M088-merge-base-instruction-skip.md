# M088: The instruction-object merge-base block skips when its comparison is vacuous

- **Status:** review
- **Priority:** high
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** —
- **Resolves:** —
- **Surface tier:** internal — a test-suite repair; no exported behavior, dataset column, or artifact changes
- **Branch/PR:** `m088-merge-base-instruction-skip` / https://github.com/jmgirard/hitop/pull/96

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

- [x] AC1: On a branch cut from `main` at `c60f5f35` or later, `Rscript -e 'devtools::test()'`
      reports 0 failures.
- [x] AC2: On that same branch, a `reporter = "summary"` run of
      `test-item-number-merge-base.R` reports the block "retyping the instruction option
      values moved nothing else" as skipped, with a reason naming that the merge base already
      stores the instruction option values as integers.
- [x] AC3: Called with `sha = "c3be8505"` — the commit immediately preceding `da1d6f09`, the
      merge that made the shipped instruction option values integers — `merge_base_instructions()`
      does not skip, and the set of objects it reports as moved equals
      `c("hitopsr_instructions", "hitopbr_instructions")`.
- [x] AC4: The block calls no `testthat::skip_if()` inside its per-object loop, so no object is
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
- [x] T3: GREEN evidence. On the branch, re-run the file (expect 5 skips, 0 failures) and then
      `Rscript -e 'devtools::test()'` for AC1. Record both.
- [x] T4: Discrimination evidence. In one `devtools::load_all()` session, evaluate a
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

- 2026-09-04: T3 GREEN evidence at be2bc415. `test_local(filter = "item-number-merge-base", reporter = "summary")`: 5 skips, 0 failures — skip 5 is `retyping the instruction option values moved nothing else ('test-item-number-merge-base.R:244:3') - Reason: the merge base already stores every instruction option value as integer`. `devtools::test()` at the same commit: `[ FAIL 0 | WARN 0 | SKIP 13 | PASS 17277 ]`.

- 2026-09-04: T4 discrimination evidence at ce8a364c. One `load_all()` session with the working directory set to `tests/testthat` (what `test_local()` sets, and what `repo_root()`'s `test_path("..", "..")` requires) sourced the helper and the test file under `with_reporter("summary")`, then evaluated a `test_that()` block calling `merge_base_instructions("c3be8505")`. Reporter line `SSSS....S..`: the five branch-level skips, then the block's two `expect_setequal()` calls passing and no sixth skip — the helper did not skip against the pre-retype base, returned all four instruction objects, and reported `c("hitopsr_instructions", "hitopbr_instructions")` as moved.
- 2026-09-04: `repo_root()` resolves correctly only when the working directory is `tests/testthat`: under a testthat reporter context `testthat::test_path()` drops its `tests/testthat` prefix, so the same call from the repo root points one level above the repo and every merge-base read skips with "could not read". A harness property, not a defect in the helper — `test_local()` and `R CMD check` both set that directory.

## Decisions
- 2026-09-04: review opened PR #96 and recorded it in the header; `main` had not moved since the branch was cut (0 behind). AC1-AC4 executed with fresh evidence at 67bf3daf and their boxes ticked. Consistency gate: `cairn_validate` exit 0 (all checks passed; 24 pre-existing advisories, `release window` silent), `devtools::document()` no diff, `pkgdown::check_pkgdown()` no problems; no principle change, so `cairn_impact` skipped. `R CMD check` and the three-lens review fan-out still running at this checkpoint.

## Review

- AC1 — met. Branch head 67bf3daf; merge base with `origin/main` is cb409078, and `git merge-base --is-ancestor c60f5f35 cb409078` exits 0, so the branch is cut at `c60f5f35` or later. `Rscript -e 'devtools::test()'`: `[ FAIL 0 | WARN 0 | SKIP 13 | PASS 17277 ]`.
- AC2 — met. `testthat::test_local(filter = "item-number-merge-base", reporter = "summary")` at 67bf3daf: reporter line `SSSS....S`, 0 failures. Skip 5 reads `retyping the instruction option values moved nothing else ('test-item-number-merge-base.R:244:3') - Reason: the merge base already stores every instruction option value as integer` — the block named in the criterion, skipped, its reason naming the merge base already storing the instruction option values as integers.
- AC3 — met on its operative assertion, with one correction to its parenthetical. One `load_all()` session with the working directory set to `tests/testthat` sourced `helper-merge-base.R` and `test-item-number-merge-base.R` under `with_reporter("summary")`, then evaluated a `test_that()` block calling `merge_base_instructions("c3be8505")`. Reporter line `SSSS....S..`: the file's five branch-level skips, then this block's two `expect_setequal()` calls passing and no sixth skip — the helper did not skip against `c3be8505`, returned all four instruction objects, and its moved set printed as `hitopbr_instructions, hitopsr_instructions`. Correction to the criterion's apposition: `c3be8505` is not the commit immediately preceding `da1d6f09` — `git log --first-parent` gives `da1d6f09` <- `b1b523d1` <- `c3be8505`, so it is two commits earlier. The rest of the apposition holds: reading `R/sysdata.rda` at each of the three commits shows `hitopsr_instructions`/`hitopbr_instructions` option values `double` at `c3be8505` and `b1b523d1` and `integer` at `da1d6f09`, so `da1d6f09` is the commit that made them integers and `c3be8505` does predate the retype. Raised as finding R1 below.
- AC4 — met. `grep -n 'skip_if\|skip('` over `tests/testthat/test-item-number-merge-base.R` returns six lines: three comments (`:78`, `:152`, `:218`) and three `testthat::skip_if()` calls (`:92`, `:167`, `:235`), the last inside `merge_base_instructions()` after its `lapply()` read of all four objects and before the return. The block itself (`:242-260`) calls `skip_without_merge_base()` at `:243` and `merge_base_instructions()` at `:244`, both before the `for` loop at `:245-251`; the loop body (`:246-250`) holds only the conditional retype and one `expect_identical()`. No object can be abandoned mid-loop, and the AC2 run shows the block reported as one skip.
