# M069: `calc_se` is deprecated in favour of the interval functions

- **Status:** planned
- **Priority:** normal
- **Depends on:** M068
- **Driving RR:** —
- **Principles touched:** GP2, GP3
- **Branch/PR:** —

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
- [ ] AC2 Scored output is unchanged: for each of the three functions, over every
      combination of `version` (where the signature admits one), `missing` and
      `append` that signature accepts, on the shipped `sim_pid5`, `sim_pid5sf`,
      `sim_pid5bf`, `sim_hitopsr`, `sim_hitopbr`, `ku_hitopsr` and `ku_hitopbr`
      datasets, a `calc_se = TRUE` call under `suppressWarnings()` returns a
      result `identical()` to the same call at the `v0.2.0` tag — the D-011
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

- [ ] T1 Add `deprecate_calc_se()` beside `deprecate_subset()` (`R/util.R:509`);
      fire it from `score_engine()` (`R/score_engine.R:119`) when `calc_se` is
      `TRUE`, with `call` threaded so the warning blames the wrapper the user
      called, not the engine.
- [ ] T2 Tests for the warning class and its absence, three functions × three
      argument cases.
- [ ] T3 Run the D-011 characterization harness against the `v0.2.0` tag over the
      AC2 matrix; record the config count and the `identical()` result.
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

## Decisions

## Review
