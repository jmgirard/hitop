# M31: Argument-validation consistency and a harder norming oracle

- **Status:** review
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** IP2, GP3
- **Branch/PR:** `m31-validator-and-oracle-residue`

## Goal

Close the residue M29 and M30 left behind, by putting every argument check in
`R/` on the one cli-flavored validator mechanism and making the norming
differential oracle fail under the regressions it currently tolerates.

## Scope

**In:** `R/util.R` — new validators for the scalar arguments the family checks
with `stopifnot()` today (`data`, `prefix`, `append`, `calc_se`, `alpha`,
`omega`, `top`, `name`, and the choice arguments `dir`/`missing`), each taking
`call` so the abort blames the exported function; `warn_item_order()` gains the
same `call`. The 22 `stopifnot()` call sites across `R/rank_scales.R`,
`R/score_engine.R`, `R/norm_pid5.R`, `R/validity_pid5.R`,
`R/reliability_engine.R`, `R/label_hitopsr.R`, `R/label_hitopbr.R`, and
`R/rename_hitopsr_items.R`. `tests/testthat/test-validate.R` — error-branch
tests. `tests/testthat/test-norm_pid5.R` — `observed_shift()` and the
`norm_shift()` test at `:366`. `NEWS.md` and `cairn/DESIGN.md:50` (the
`R/util.R` helper inventory line).

**Out:** any change to a returned score, T score, or percentile — AC7 forbids
it here and nothing plans it elsewhere. Unifying the `cli_alert_*` reporting
convention beyond the D-024/D-025 carve-out → not planned; D-025 settled the
`norm_pid5()` case and nothing else is disputed. The candidate row's
`.internal = TRUE` item → dropped as having no referent in `R/util.R` (grep
evidence in the work log), at the maintainer's direction at the plan gate.

## Acceptance criteria

- [ ] AC1. The `norm_shift()` test (`test-norm_pid5.R:366`) runs `observed_shift()`
      on an in-test copy of `sim_pid5` carrying one `NA` on a PRD item that
      belongs to no domain-contributing facet (11 such items exist; item 2 is
      one), so no scale the norms cover is ever prorated. The test fails when
      `R/validity_pid5.R:172`'s `rowSums()` gains `na.rm = TRUE`, shown by
      mutation: apply the change, run, require red, restore and diff.
- [ ] AC2. That test asserts the covered-scale set per version equals what
      `pid_norms` carries — FULL {the 5 domains, INC, ORS, PRD}, SF {the 5
      domains, INCS}, BF {total, the 5 domains} — replacing
      `expect_true(any(keep))` at `:379`, so PRD and with it the `"sum"` branch
      cannot silently drop out of the comparison. No *expected* value in the
      test is read from `norm_mean_scales`, `norm_sum_scales`,
      `norm_invariant_scales`, or `norm_metric()` (IP2); the existing
      `norm_metric()` call on the actual side (`:384`) stays.
- [ ] AC3. The `low` loop at `:372` covers a negative shift as well as the two
      positive ones, and every assertion in the test holds for it.
- [ ] AC4. `grep -rn "stopifnot" R/` returns no matches; every argument
      formerly checked there is checked by an `R/util.R` validator built on
      `cli_assert()`/`cli::cli_abort()` that takes `call`. `dir`
      (`R/rank_scales.R:79`, where nothing matches it today) goes through
      `rlang::arg_match()`, which lists the permitted values and suggests the
      near miss on a typo while still requiring an exact match, so accepted
      input does not change; `missing` and
      `version` keep base `match.arg()` in the exported functions, and
      `score_engine()`'s `stopifnot(rlang::is_string(missing))` — unreachable
      after `match.arg()` has already run — is removed rather than converted.
      `validate_scales()` (`R/util.R:106`) gains the `"x" = "You supplied
      {.cls {class(x)}}."` bullet its sibling `validate_items()` already
      carries, so the two report a type failure alike.
- [ ] AC5. Every validator added under AC4 that is reachable through an
      exported function has a test firing its error branch, asserting the
      abort's `conditionCall()` names that exported function and its message
      names the offending argument. No test is owed for `score_engine()`'s
      `missing` check, which AC4 removes as unreachable.
- [ ] AC6. `devtools::document()` leaves no diff, `devtools::test()` passes,
      and `devtools::check()` reports no new errors, warnings, or notes against
      the pre-milestone baseline recorded in the work log at T1.
- [ ] AC7. No returned value moves: `score_pid5()`, `validity_pid5()`,
      `reliability_pid5()`, `norm_pid5()`, and `rank_scales()` are
      `identical()` before and after across the versions and `missing` modes
      the suite already exercises (GP2).

## Coverage

- AC1 → T2
- AC2 → T3
- AC3 → T3
- AC4 → T4, T5
- AC5 → T4
- AC6 → T6
- AC7 → T1, T6

## Tasks

- [x] T1. Record the pre-milestone `devtools::check()` baseline in the work log
      and capture the AC7 characterization snapshot (the M30 harness pattern).
- [x] T2. Add the missing-PRD-item fixture to the `norm_shift()` test; verify it
      goes red under the `na.rm = TRUE` mutation at `R/validity_pid5.R:172`,
      then restore and diff.
- [x] T3. Replace `expect_true(any(keep))` with the per-version covered-set
      assertion; extend the `low` loop with a negative shift.
- [x] T4. Author the new validators in `R/util.R` with `call` threading and
      `arg_match()` for `dir`; write each error-branch test first.
      Thread `call` into `warn_item_order()`.
- [x] T5. Convert the 22 `stopifnot()` sites, one file at a time, verifying
      after each.
- [x] T6. Update roxygen where documented error behavior changes; `document()`;
      NEWS.md entry; `cairn/DESIGN.md:50` inventory line; re-run AC7 and
      `check()`.

## Work log

- 2026-07-31: created by /milestone-plan.
- 2026-07-31: criteria audit ([O], fresh context) returned six findings — four fixed pre-gate (AC1 fixture must be an in-test copy since `sim_pid5` has no NAs; AC2's "fails if any scale stops being reconciled" over-claimed what a set assertion can detect; AC5's `missing` check unreachable and `call=` invisible to `conditionMessage()`; AC6 had no recorded 0/0/0 baseline), one became the gate's fixture question, one confirmed no IP/D conflict.
- 2026-07-31: plan gate chose converting all 22 `stopifnot()` sites over converting only `prefix`/`append` because a partial conversion leaves one file giving two error idioms, the divergence D-026 had to close later; falsified by a conversion that changes behavior at any site rather than only its message.
- 2026-07-31: plan gate chose constraining the AC1 fixture's `NA` to a PRD item outside every domain-contributing facet over dropping the negative shift, because `round_half_up()` is not translation-equivariant across zero and proration is what exposes that; falsified by a covered scale showing a non-constant observed shift despite the placement.
- 2026-07-31: plan gate chose `rlang::arg_match()` for `dir`/`missing` over a plain allowed-set assertion, accepting that it widens accepted input with partial matching, because the suggestion-on-typo error is what GP3 asks of this package's errors; falsified by partial matching resolving a user's abbreviation to the wrong option.
- 2026-07-31: the candidate row's glue-interpolation item was examined and largely found sound — `{arg}` is already `{.arg {arg}}` at `R/util.R:109` and `:124`, and `{unit}` is a common noun where bare interpolation is correct; the one live residue, `validate_scales()` lacking the supplied-type bullet `validate_items()` has, is in AC4.
- 2026-07-31: `.internal = TRUE`, the candidate row's first item, has no referent — `grep -rn "\.internal" R tests cairn` matches only the row itself and unrelated `use_data(internal = TRUE)` prose; dropped at the maintainer's direction.

- 2026-07-31: branch `m31-validator-and-oracle-residue` cut from main at 954a0e7; status → in-progress.
- 2026-07-31: substantive amendment at the implementation gate — AC4's "`dir` and `missing` go through `rlang::arg_match()`" narrowed to `dir` only, with `missing`/`version` keeping `match.arg()` and the engine's redundant `missing` check removed; AC5's `missing` sentence follows. Maintainer chose this over converting the exported layer, since nothing is defective about `match.arg()` and converting it would move error text on three exported functions for no fault.
- 2026-07-31: T1 — pre-milestone `devtools::check()` baseline is 0 errors / 0 warnings / 0 notes, so AC6's "no new" bar is a clean 0/0/0. AC7 snapshot captured over 33 configs (3 versions × 3 `missing` modes × `calc_se` on/off for `score_pid5()`, plus `validity_pid5()`, `reliability_pid5()`, `norm_pid5()` official and shifted, `rank_scales()`); all 33 return without error. Harness committed to `devel/characterize_m31.R` so the comparison is reproducible at review.
- 2026-07-31: T2 — fixture `prd_na_pid5` (one NA on item 2, a PRD item in no domain-contributing facet) added as a fourth case. Mutation `na.rm = TRUE` at `R/validity_pid5.R:172` → 6 failures in the `norm_shift()` test (2 assertions × 3 `low` values); restored, `git diff` clean.
- 2026-07-31: T2 finding — the same mutation also reddens the pre-existing test at `test-norm_pid5.R:550` ("a PRD with a missing item stays NA through the shift correction", 2 failures), so the regression was not wholly uncovered as the candidate row assumed; what was uncovered is the *differential oracle*, which was green under it. The new case makes `observed_shift()` itself sensitive and exercises the shift correction against a measured quantity, which `:550` (an NA-propagation assertion) does not.
- 2026-07-31: T3 — `expect_true(any(keep))` replaced by `expect_setequal()` against a hardcoded per-version covered set read from `pid_norms`, not from the partition vectors (IP2); `low` loop extended to `c(-1, 1, 2)`. Mutation hiding PRD in `norm_covers()` → 6 failures where `any(keep)` stayed green; restored. Full suite 0 fail / 10418 pass / 1 pre-existing skip.
- 2026-07-31: T4 — three new validators in `R/util.R` (`validate_string()` with an `allow_null` for `rank_scales()`'s nullable `prefix`, `validate_flag()`, `validate_count()` splitting type from bounds), each on the `arg`/`call` convention; `validate_scales()` gained the supplied-type bullet; `warn_item_order()` now takes and forwards `call`. Tests written first and confirmed red (12 failures, 3 errors) before the implementation landed.
- 2026-07-31: T5 — all 22 sites converted across 8 files; `grep -rn "stopifnot" R/` now returns 0. `score_engine()`'s `missing` check removed per AC4. None of the 8 files is in the CRLF trio the M24 lesson names, so whole-file rewrites were safe.
- 2026-07-31: T5 correction — `rlang::arg_match()` does NOT partial-match: it requires an exact value and only *suggests* the near miss, so AC4's "which also enables partial matching" was false and is amended. The practical effect is that the change is strictly smaller than planned — accepted input for `dir` is unchanged and only the error message improves, so NEWS records no widening. Caught by the test asserting `dir = "l"` would newly succeed, which failed.
- 2026-07-31: AC7 evidence — `devel/characterize_m31.R` run before and after; all 33 configs `identical()`, no differing config. Full suite 0 fail / 10478 pass / 1 pre-existing skip (+60 assertions).
- 2026-07-31: T6 partial — NEWS.md entry added (message-only change, no widening), `cairn/DESIGN.md:50` inventory line names the three new validators and records that no argument check in `R/` uses a bare predicate assertion. `devtools::document()` produces no `man/`/`NAMESPACE` diff; no roxygen needed editing because accepted input is unchanged everywhere. T6 stays unchecked: the final `devtools::check()` and the AC7 re-run are still outstanding.
- 2026-07-31: T6 complete — final `devtools::check()` 0 errors / 0 warnings / 0 notes, matching the T1 baseline exactly. AC7 re-run against the final tree: all 33 configs `identical()` to the pre-milestone snapshot. `grep -rn "stopifnot" R/` returns 0. Status → review.

## Decisions

## Review
