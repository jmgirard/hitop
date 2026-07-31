# M32: Test coverage for M31's argument-validation additions

- **Status:** in-progress
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** GP3
- **Branch/PR:** `m32-m31-validation-test-coverage`

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

- [ ] AC1: `validate_scales()` has a test asserting the supplied-type bullet
      names the class actually supplied — `logical` for `TRUE`, `list` for
      `list("a")` — matching the bare class word rather than the cli-styled
      `<logical>`, and it fails when that bullet is deleted from `R/util.R`.
- [ ] AC2: `validate_scales()` has a test asserting its message names the
      caller's argument when a caller overrides the `arg` default, exercised
      through `norm_pid5(scores = TRUE)`, and it fails when the `{arg}`
      interpolation is removed from the message in `R/util.R`.
- [ ] AC3: `warn_item_order()` has tests asserting the warning's
      `conditionCall()` names the exported function the user called — for both
      `score_pid5()` (explicit `call` thread) and `validity_pid5()`
      (`caller_env()` default) — each failing when `call = call` is dropped from
      the `cli_warn()` in `R/util.R`.
- [ ] AC4: the set assertion in the `norm_shift()` loop
      (`tests/testthat/test-norm_pid5.R:418`) carries the same
      `info = paste(k$version, k$label, "low", low)` label its two neighbours
      already carry, so a failure identifies the iteration; verified by mutating
      `covered_scales[["SF"]]` and reading the reported label. `expect_setequal()`
      takes no `info`, so the assertion is reshaped to a sorted, de-duplicated
      `expect_equal()`, which keeps set semantics and still reports which scale
      names differ.
- [ ] AC5: `devtools::test()` clean (0 failures) and `devtools::check()` clean
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
- [ ] T2: add the two `warn_item_order()` attribution assertions to
      `tests/testthat/test-item-guards.R`, reusing its misordered-items fixture
      (`:41,46-49`); verify both redden with `call = call` dropped from
      `R/util.R:158`.
- [ ] T3: reshape `tests/testthat/test-norm_pid5.R:418` to a sorted-unique
      `expect_equal()` carrying the loop's existing `info`; verify by mutating
      `covered_scales[["SF"]]` and reading the reported label.
- [ ] T4: run `devtools::test()` and `devtools::check()`; record both outputs.

## Work log

- 2026-07-31: created by /milestone-plan; absorbs the M31 follow-up candidate row (lineage M31, its top two sub-threshold review findings).
- 2026-07-31: criteria audit ([O], fresh context) returned one finding — AC4's verification clause named a per-case expected set that does not exist, since `covered_scales` (`test-norm_pid5.R:386`) is keyed by version only and `scales[keep]` depends on neither `low` nor `label`; clause rewritten to mutate `covered_scales[["SF"]]`. AC1/AC2/AC3/AC5 returned satisfiable as written, each stated mutation confirmed to redden.
- 2026-07-31: plan gate chose a sorted-unique `expect_equal()` over `expect_true(setequal(...), info = info)` for AC4 because the boolean wrapper prints only "was not TRUE" and loses the differing scale names M31 adopted `expect_setequal()` to see; falsified by a failure whose sorted-vector diff reads less legibly than a set difference.
- 2026-07-31: plan gate chose covering both `warn_item_order()` call sites over the explicit thread alone because the two carry the caller's identity by different mechanisms (explicit `call =` vs the `caller_env()` default); falsified by evidence the two mechanisms cannot break independently.

- 2026-07-31: T1 done — two `validate_scales()` blocks in `test-validate.R`; deleting the type bullet reddens both class assertions, hardcoding `{arg}` to "scales" reddens the `norm_pid5(scores=)` assertion. Suite clean (114 pass).

## Decisions
