# M069: `calc_se` is deprecated in favour of the interval functions

- **Status:** in-progress
- **Priority:** normal
- **Depends on:** M068
- **Driving RR:** —
- **Principles touched:** GP2, GP3
- **Branch/PR:** `m069-calc-se-deprecation`

## Goal

Calling any `score_*()` function with `calc_se = TRUE` warns that the argument is
deprecated and points at the interval functions, while returning exactly what it
returns today.

## Scope

Surface tier: **user-facing** — an exported argument, the `_se` columns it
returns, and the help pages and vignettes that describe them.

**In:** A classed deprecation warning from `score_pid5()`, `score_hitopsr()` and
`score_hitopbr()` when `calc_se = TRUE`, built on the hand-rolled
`deprecate_subset()` pattern at `R/util.R:509` (D-034(d) bars {lifecycle}).
Help pages and the `calc_se` vignette prose rewritten to say what the number is,
that it is deprecated, and which interval function replaces it per instrument.
NEWS entry, a D-entry naming the new condition class (D-034(c)), and a ROADMAP
candidate row for the removal.

**Out:** Removing `calc_se`, `calc_sem()` (`R/util.R:439`), the `mask_se_na`
engine flag or the `_se` columns → the candidate row this milestone files; that
is a GP2 scored-output change for the next breaking release. Unifying
`mask_se_na` (DESIGN Known issue #4) → retired as moot at this plan gate, not
fixed. A PID-5 interval surface → the standing ROADMAP candidate.

## Acceptance criteria

- [ ] AC1 Each of `score_pid5()`, `score_hitopsr()` and `score_hitopbr()` emits
      exactly one warning of class `hitop_deprecated_calc_se` when called with
      `calc_se = TRUE`, and no warning of that class when called with
      `calc_se = FALSE` or with the argument omitted.
- [ ] AC2 Scored output is unchanged over the configurations below: for each of
      the three functions, over every combination of `missing` and `append` that
      its signature accepts, on each shipped dataset that function can score —
      `score_pid5()` on `sim_pid5` (`version = "FULL"`), `sim_pid5sf` and
      `ku_pid5sf` (`"SF"`) and `sim_pid5bf` (`"BF"`); `score_hitopsr()` on
      `sim_hitopsr` and `ku_hitopsr`, each both without a `module` and with one;
      `score_hitopbr()` on `sim_hitopbr` and `ku_hitopbr` — a `calc_se = TRUE`
      call returns a result `identical()` to the same call at commit `1dec0a4`,
      the commit this milestone's branch was cut from, and signals the same
      warnings apart from the new `hitop_deprecated_calc_se` one — the D-011
      characterization-harness pattern.
- [ ] AC3 Each of the three `calc_se` help pages states that the argument is
      deprecated and names what replaces it for that instrument —
      `interval_hitopsr()`, `interval_hitopbr()`, or, on the PID-5 page, that the
      package has no interval function for it. The existing accurate description
      on each page stands unchanged, including the PID-5 page's two-case account
      of facet and domain standard errors (`R/score_pid5.R:30-39`) and its
      `NA`-masking sentence.
- [ ] AC4 Every file under `vignettes/` that `grep -rl "calc_se" vignettes/`
      returns carries the same two statements as AC3 — deprecated, and what
      replaces it for the instrument that file is about; a file that only points
      readers at another vignette's section says the argument is deprecated
      there too.
- [ ] AC5 NEWS.md's development section announces the deprecation, names the
      three functions, states that the argument and its `_se` columns will be
      removed in a future release, and names the replacement per instrument.
- [ ] AC6 The checks `cairn/PROFILE.md`'s `## consistency-gate` slot lists are
      clean, including `devtools::document()` with no diff, `devtools::test()`
      FAIL 0, and `devtools::check()` 0 errors and 0 warnings.

## Coverage

- AC1 → T1, T2
- AC2 → T3
- AC3 → T4
- AC4 → T5
- AC5 → T6
- AC6 → T7

## Tasks

- [x] T1 Add `deprecate_calc_se()` beside `deprecate_subset()` (`R/util.R:509`);
      fire it from `score_engine()` (`R/score_engine.R:119`) when `calc_se` is
      `TRUE`, with `call` threaded so the warning blames the wrapper the user
      called, not the engine.
- [x] T2 Tests for the warning class and its absence, three functions × three
      argument cases.
- [x] T3 Run the D-011 characterization harness (`data-raw/characterize_calc_se.R`)
      against commit `1dec0a4` over the AC2 matrix; record the config count and
      the `identical()` result.
- [ ] T4 Rewrite the `calc_se` roxygen on the three wrappers; extend
      `tests/testthat/test-help-se-prose.R` to the deprecation and replacement
      sentences and confirm it fails against the current wording.
- [ ] T5 Rewrite the `calc_se` prose in every vignette `grep -rl` returns (four
      today, including `vignettes/articles/modules-hitopsr.Rmd:271`); extend
      `tests/testthat/test-vignette-se-prose.R` the same way.
- [ ] T6 NEWS.md entry; D-entry naming `hitop_deprecated_calc_se` as a public
      condition class per D-034(c) and recording the deprecation's rationale and
      rejected alternative; ROADMAP candidate row for the removal; retire the
      `mask_se_na` candidate row and mark DESIGN Known issue #4 superseded.
- [ ] T7 Correct `cairn/PROFILE.md`'s `## test-doctrine` line that still says a
      deprecation cycle warns via `lifecycle::deprecate_warn()`, which D-034(d)
      overrode; run the full consistency gate.

## Work log

- 2026-08-30: created by /milestone-plan.
- 2026-08-30: criteria audit ran in FULL mode (user-facing tier) in the same fresh-context [O] reader; it returned 6 findings on this milestone's criteria — a blanket "no warning" universal, an unentried public condition class against D-034(c) plus a stale PROFILE lifecycle line, a mandated evidence quotation, a self-referential AC2 domain, a factually wrong mandated sentence for the PID-5 domain SEs, and a hand-listed vignette domain — every one fixed in the criteria above before committing.
- 2026-08-30: plan gate chose deprecating `calc_se` over removing it outright in one milestone, because the argument defaults to `FALSE` so the warning targets only deliberate use, and dropping the `_se` columns is a GP2 scored-output change belonging to the next breaking release; falsified by a user reporting the warning as noise on a call they did not opt into.
- 2026-08-30: plan gate chose retiring the `mask_se_na` ROADMAP row as moot over unifying the masking first, because unifying is a GP2 scored-output change on output already scheduled for deletion; falsified by the removal candidate row being dropped rather than promoted.
- 2026-08-30: implement gate chose a per-instrument replacement sentence, threaded from each wrapper into `score_engine()` as `se_instead`, over one shared message naming both interval functions — a PID-5 caller has no interval function to be sent to.
- 2026-08-30: implement gate chose switching the HiTOP-SR and HiTOP-BR vignette demonstrations over to their interval functions rather than showing or hiding the warning; the PID-5 short-form vignette and the `score_pid5()` help example keep their `calc_se = TRUE` calls, there being no replacement to switch to.
- 2026-08-30: T1, T2 — warning fires from `score_engine()` inside the `calc_se` block, so a call aborting on its data, items or an append collision reports that alone; both new checks proven able to fail (an unconditional warning reddens the absence test; a wrong `se_instead` reddens the routing test).
- 2026-08-30: [S] delegation wrapped 22 pre-existing `calc_se = TRUE` test call sites in a new class-targeted `hush_se()` helper so the deprecation does not bury the suite's warning channel; diff verified, `devtools::test()` FAIL 0 WARN 0 PASS 16098 (WARN 28 before the wrapping).
- 2026-08-30: amendment (substantive, mini gate): AC2's `v0.2.0` baseline is unsatisfiable through no fault of this milestone — M068's NEWS-flagged HiTOP-BR item-36 rekey landed after that tag and before this branch was cut, and moved 8 of 34 configurations. Baseline moved to commit `1dec0a4`, the branch point.
- 2026-08-30: criteria audit on the amended AC2 ran in FULL mode (user-facing tier) in a fresh-context [O] reader that did not author it; 3 findings — an unsatisfiable function x dataset cross product, a headline promising more than the walk covers, and a `missing` axis inert on all three PID-5 datasets (each carries no `NA`). All three fixed at the mini gate, which also widened AC2 by adding `ku_pid5sf`, the one shipped dataset with missing PID-5 data and the only one entering the PID-5 `_se`-masking path (34 configurations -> 40).
- 2026-08-30: the once-only re-entry of the fixed AC2 wording ran in a second fresh-context [O] reader; 2 findings, both taken to the user and adopted — `score_hitopsr()`'s `module` path unprobed, and a blanket `suppressWarnings()` discarding the condition channel the D-011 pattern compares. AC2 widened again: module runs added and warnings compared alongside values (40 -> 48). Criteria widened or added: AC2 only.
- 2026-08-30: T3 — `data-raw/characterize_calc_se.R` committed as the harness; 48 configurations, all 48 `identical()` (value and condition classes) between `1dec0a4` and the branch. Condition-channel comparison shown non-vacuous: a planted extra warning in `score_engine()` reddens all 48 while the values alone stay identical.

## Decisions

## Review
