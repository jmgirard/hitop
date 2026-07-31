# M32: Test coverage for M31's argument-validation additions

- **Status:** review
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** GP3
- **Branch/PR:** `m32-m31-validation-test-coverage` · https://github.com/jmgirard/hitop/pull/35

## Goal

Give M31's three untested argument-validation additions assertions that fail
when the addition is reverted, and make the `norm_shift()` loop name its failing
case.

## Scope

**In:** Tests only, for the three M31 additions that shipped with no assertion
of their own. (1) `validate_scales()`'s supplied-type bullet (`R/util.R:111`) —
the existing block at `tests/testthat/test-validate.R:46-51` asserts only *that*
an error fires. (2) `validate_scales()`'s `{arg}` interpolation
(`R/util.R:110`), exercised through the one caller that overrides the default,
`norm_pid5()`'s `scores` (`R/norm_pid5.R:170`). (3) `warn_item_order()`'s `call`
parameter (`R/util.R:158`), at both call sites — `score_pid5()`, where
`prep_items()` threads `call` explicitly (`R/util.R:304`), and `validity_pid5()`,
which relies on the `caller_env()` default (`R/validity_pid5.R:83`). Plus
reshaping the unlabelled set assertion at `tests/testthat/test-norm_pid5.R:418`
so a failure names which of the 12 version/`low` cases broke.

**Out:** Any change to `R/` behavior — a test that reveals a defect routes to
`/hotfix`, it is not fixed here. The other ten sub-threshold findings M31's
review logged stay logged in `milestones/archive/M31-validator-and-oracle-residue.md`.
No NEWS entry: nothing user-visible changes.

## Acceptance criteria

- [x] AC1: `validate_scales()` has a test asserting the supplied-type bullet
      names the class actually supplied — `logical` for `TRUE`, `list` for
      `list("a")` — matching the bare class word rather than the cli-styled
      `<logical>`, and it fails when that bullet is deleted from `R/util.R`.
- [x] AC2: `validate_scales()` has a test asserting its message names the
      caller's argument when a caller overrides the `arg` default, exercised
      through `norm_pid5(scores = TRUE)`, and it fails when the `{arg}`
      interpolation is removed from the message in `R/util.R`.
- [x] AC3: `warn_item_order()` has tests asserting the warning's
      `conditionCall()` names the exported function the user called — for both
      `score_pid5()` (explicit `call` thread) and `validity_pid5()`
      (`caller_env()` default) — each failing when `call = call` is dropped from
      the `cli_warn()` in `R/util.R`.
- [x] AC4: the set assertion in the `norm_shift()` loop
      (`tests/testthat/test-norm_pid5.R:418`) carries the same
      `info = paste(k$version, k$label, "low", low)` label its two neighbours
      already carry, so a failure identifies the iteration; verified by mutating
      `covered_scales[["SF"]]` and reading the reported label. `expect_setequal()`
      takes no `info`, so the assertion is reshaped to a sorted, de-duplicated
      `expect_equal()`, which keeps set semantics and still reports which scale
      names differ.
- [x] AC5: `devtools::test()` clean (0 failures) and `devtools::check()` clean
      (0 errors, 0 warnings; NOTEs justified).

## Coverage

- AC1 → T1
- AC2 → T1
- AC3 → T2
- AC4 → T3
- AC5 → T4

## Tasks

- [x] T1: add the supplied-type and `arg`-interpolation assertions beside the
      existing `validate_scales()` block (`tests/testthat/test-validate.R:46`);
      verify each reddens against a scratch revert of `R/util.R:110-111`.
- [x] T2: add the two `warn_item_order()` attribution assertions to
      `tests/testthat/test-item-guards.R`, reusing its misordered-items fixture
      (`:41,46-49`); verify both redden with `call = call` dropped from
      `R/util.R:158`.
- [x] T3: reshape `tests/testthat/test-norm_pid5.R:418` to a sorted-unique
      `expect_equal()` carrying the loop's existing `info`; verify by mutating
      `covered_scales[["SF"]]` and reading the reported label.
- [x] T4: run `devtools::test()` and `devtools::check()`; record both outputs.

## Work log

- 2026-07-31: created by /milestone-plan; absorbs the M31 follow-up candidate row (lineage M31, its top two sub-threshold review findings).
- 2026-07-31: criteria audit ([O], fresh context) returned one finding — AC4's verification clause named a per-case expected set that does not exist, since `covered_scales` (`test-norm_pid5.R:386`) is keyed by version only and `scales[keep]` depends on neither `low` nor `label`; clause rewritten to mutate `covered_scales[["SF"]]`. AC1/AC2/AC3/AC5 returned satisfiable as written, each stated mutation confirmed to redden.
- 2026-07-31: plan gate chose a sorted-unique `expect_equal()` over `expect_true(setequal(...), info = info)` for AC4 because the boolean wrapper prints only "was not TRUE" and loses the differing scale names M31 adopted `expect_setequal()` to see; falsified by a failure whose sorted-vector diff reads less legibly than a set difference.
- 2026-07-31: plan gate chose covering both `warn_item_order()` call sites over the explicit thread alone because the two carry the caller's identity by different mechanisms (explicit `call =` vs the `caller_env()` default); falsified by evidence the two mechanisms cannot break independently.

- 2026-07-31: T1 done — two `validate_scales()` blocks in `test-validate.R`; deleting the type bullet reddens both class assertions, hardcoding `{arg}` to "scales" reddens the `norm_pid5(scores=)` assertion. Suite clean (114 pass).
- 2026-07-31: T2 done — both `warn_item_order()` call sites asserted in `test-item-guards.R`; dropping `call = call` leaves `conditionCall()` NULL at both, and the first draft's raw `call_name()` erroring on NULL aborted the block before the second site, so the assertions were reshaped to fail independently (2 failures under mutation, not 1). Suite clean (30 pass).
- 2026-07-31: T3 done — `test-norm_pid5.R:418` reshaped to a sorted-unique `expect_equal()` carrying the loop's `info`; mutating `covered_scales[["SF"]]` fails the three SF iterations, each printing its own `SF complete low <n>` label alongside the differing scale names. Suite clean (440 pass).
- 2026-07-31: T4 done — `devtools::test()` 0 failures / 10490 pass / 1 skip; `devtools::check()` 0 errors, 0 warnings, 0 notes (3m 1s). All tasks complete; status to review.

## Decisions

## Review

Evidence gathered 2026-07-31 on `m32-m31-validation-test-coverage` @ 759de4f, PR #35.
Each mutation was re-run in an isolated `git archive` copy of HEAD so it could not
perturb the tree the review subagents were reading.

- AC1: deleting the `"x" = "You supplied {.cls {class(x)}}."` bullet from
  `validate_scales()` reddens both class assertions — `test-validate.R:58` and
  `:62` fail, suite 2 FAIL / 112 PASS; restored, 0 FAIL / 114 PASS.
- AC2: hardcoding `{.arg {arg}}` to `{.arg scales}` reddens `test-validate.R:75`
  only — 1 FAIL / 113 PASS. The assertion runs through `norm_pid5(scores = TRUE)`,
  the sole caller overriding the `arg` default.
- AC3: dropping `call = call` from `warn_item_order()`'s `cli_warn()` reddens
  both call sites independently — `test-item-guards.R:84` (score_pid5) and `:88`
  (validity_pid5), 2 FAIL / 28 PASS. Confirmed separately that both paths'
  `conditionCall()` become NULL under the mutation.
- AC4: mutating `covered_scales[["SF"]]` fails the three SF iterations, each
  printing its own label — `SF complete low -1`, `SF complete low 1`,
  `SF complete low 2` — alongside the differing scale names, 3 FAIL / 437 PASS.
- AC5: `devtools::test()` 0 failures / 10490 pass / 1 skip; `devtools::check()`
  0 errors, 0 warnings, 0 notes (2m 53s).

Consistency gate — universal: `cairn_validate` exit 0, all checks pass (20
`dangling id tokens` advisories are the standing pre-migration references, not
this milestone's). No `DESIGN.md` principle changed, so `cairn_impact` skipped.
Toolchain (`r-package` slot): `devtools::document()` produced no diff;
`pkgdown::check_pkgdown()` "No problems found"; README.Rmd untouched; no NEWS
entry owed (nothing user-visible changed, per Scope Out); no new top-level file,
so no `.Rbuildignore` entry needed; full `check()` clean as above.

Review fan-out — three fresh-context lenses. Blame-history [S]: zero findings;
verified the `expect_setequal()` → sorted-unique `expect_equal()` reshape
preserves what M31 bought (whole-set coverage including the PRD sum branch).
Prior-review [S]: zero findings; probe `gh api .../pulls/comments` returned `[]`
so the thread walk was skipped, and the archived M31 review confirms this diff
closes its findings 78 and 68 rather than contradicting them. Diff-bug [O]: 13
findings, all scored below the 80 threshold by the [S] scorer, so none actioned.

Logged sub-threshold findings (13, none actioned):
- F13 (72): `suppressMessages()` in the new attribution test is dead code —
  `catch_cnd()` exits at the first warning, which precedes every `cli_alert_*`.
- F4 (52): that test's `catch_cnd()` omits `classes = "error"`, unlike its
  sibling; correct today because the abort is the first condition raised.
- F1 (45): the warning assertion pins the caller but not the message, so a
  future earlier `cli_warn()` could satisfy it.
- F9 (45): `sort()` drops `NA` where `expect_setequal()` would not; the scorer
  found the scenario unreachable, as these are column names.
- F2 (40): the `no_call()` substitution reports "no warning fired" and "warning
  with NULL call" identically.
- F5 (35): `cnd$call` unguarded in `test-validate.R`, unlike the `warner()` helper.
- F8 (35): the `arg`-interpolation fixture calls `score_pid5()` where a bare
  data frame would reach the same validator.
- F3 (32): the test comment's "different mechanisms" framing — both paths start
  from a `caller_env()` default and differ in hop count.
- F12 (30): the new block repeats two calls the existing block already makes.
- F10 (28), F11 (22): comment-accuracy notes riding on F9's unreachable case.
- F6 (25), F7 (25): unanchored `expect_match()` substrings, the file's existing idiom.

No finding reached 80, so nothing was fixed, deferred, or rejected at this gate.
No candidate row spawned: the two highest (72, 52) are hardening suggestions
against unreachable paths, not the shipped-untested gaps M31's 78/68 recorded.
