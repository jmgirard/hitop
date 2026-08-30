# M069: `calc_se` is deprecated in favour of the interval functions

- **Status:** review
- **Priority:** normal
- **Depends on:** M068
- **Driving RR:** —
- **Principles touched:** GP2, GP3
- **Branch/PR:** `m069-calc-se-deprecation` — https://github.com/jmgirard/hitop/pull/75

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

- [x] AC1 Each of `score_pid5()`, `score_hitopsr()` and `score_hitopbr()` emits
      exactly one warning of class `hitop_deprecated_calc_se` when called with
      `calc_se = TRUE`, and no warning of that class when called with
      `calc_se = FALSE` or with the argument omitted.
- [x] AC2 Scored output is unchanged over the configurations below: for each of
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
- [x] AC3 Each of the three `calc_se` help pages states that the argument is
      deprecated and names what replaces it for that instrument —
      `interval_hitopsr()`, `interval_hitopbr()`, or, on the PID-5 page, that the
      package has no interval function for it. The existing accurate description
      on each page stands unchanged, including the PID-5 page's two-case account
      of facet and domain standard errors (`R/score_pid5.R:30-39`) and its
      `NA`-masking sentence.
- [x] AC4 Every file under `vignettes/` that `grep -rl "calc_se" vignettes/`
      returns carries the same two statements as AC3 — deprecated, and what
      replaces it for the instrument that file is about; a file that only points
      readers at another vignette's section says the argument is deprecated
      there too.
- [x] AC5 NEWS.md's development section announces the deprecation, names the
      three functions, states that the argument and its `_se` columns will be
      removed in a future release, and names the replacement per instrument.
- [x] AC6 The checks `cairn/PROFILE.md`'s `## consistency-gate` slot lists are
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
- [x] T4 Rewrite the `calc_se` roxygen on the three wrappers; extend
      `tests/testthat/test-help-se-prose.R` to the deprecation and replacement
      sentences and confirm it fails against the current wording.
- [x] T5 Rewrite the `calc_se` prose in every vignette `grep -rl` returns (four
      today, including `vignettes/articles/modules-hitopsr.Rmd:271`); extend
      `tests/testthat/test-vignette-se-prose.R` the same way.
- [x] T6 NEWS.md entry; D-entry naming `hitop_deprecated_calc_se` as a public
      condition class per D-034(c) and recording the deprecation's rationale and
      rejected alternative; ROADMAP candidate row for the removal; retire the
      `mask_se_na` candidate row and mark DESIGN Known issue #4 superseded.
- [x] T7 Correct `cairn/PROFILE.md`'s `## test-doctrine` line that still says a
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
- 2026-08-30: T4 — each `calc_se` help page now opens with the deprecation, what goes away with it, and its own instrument's replacement; the existing description stands after it unchanged. `test-help-se-prose.R` extended and shown to fail 9 assertions against the pre-change wording.
- 2026-08-30: T5 — the HiTOP-SR and HiTOP-BR vignettes' standard-error sections keep the description and hand the demonstration to their existing Confidence Intervals sections; the PID-5 short-form vignette keeps its call, having no replacement to switch to; `modules-hitopsr.Rmd` says the argument is deprecated where it points readers elsewhere. `test-vignette-se-prose.R` now discovers its file set by scanning `vignettes/` for the argument rather than listing it, refuses a discovered file it has no replacement for, and was shown to fail 2 assertions with one vignette's notice removed. T4 and T5 verified by the one `devtools::test()` run: FAIL 0, WARN 0, PASS 16151.
- 2026-08-30: T6 — NEWS gains a Deprecations section; D-049 records the deprecation, the public condition class, the per-instrument routing and the rejected shared-message and remove-outright alternatives; the ROADMAP's `mask_se_na` row is retired as moot and absorbed into a new removal candidate; DESIGN Known issue #4 corrected in place to say the inconsistency will not be repaired. `ROADMAP.md` was over its byte budget at 24,002 before this edit and is 23,976 after, three of its widest rows compressed to make room.
- 2026-08-30: T7 — `cairn/PROFILE.md`'s deprecation-mechanics line now names the hand-rolled classed warning this repo actually uses instead of `lifecycle::deprecate_warn()` (the edit rode in with T6's commit). Consistency gate: `devtools::document()` no diff, `pkgdown::check_pkgdown()` no problems, `data-raw/check_line_endings.R` passed, `devtools::check()` 0 errors / 0 warnings / 0 notes with vignettes rebuilt and the test run OK, `devtools::test()` FAIL 0, WARN 0, PASS 16151.
- 2026-08-30: all tasks complete; status to review.
- 2026-08-30: review — supersedes the T6 line above: only two ROADMAP rows were genuinely compressed, and the room the edit claimed came from deleting the maintainer-run verification-tooling candidate row, which the review restored. The byte figures in that line (24,002 before, 23,976 after) describe a file state that no longer stands.

## Decisions

## Review

PR: https://github.com/jmgirard/hitop/pull/75 · branch cut from `1dec0a4`, which is
still `origin/main` HEAD — no merge from the default branch was needed.

### Acceptance-criterion evidence (fresh, 2026-08-30)

- **AC1** — probed each wrapper under `calc_se = TRUE` / `FALSE` / omitted, counting
  conditions of class `hitop_deprecated_calc_se` through a calling handler. All three
  fire exactly one under `TRUE` and none in the other two cases (9 probes, 3 hits).
  `conditionCall()` on each hit names the wrapper the user called, not `score_engine()`,
  and the replacement text is per instrument: `interval_hitopsr()`, `interval_hitopbr()`,
  and for the PID-5 "no interval function … see `reliability_pid5()`".
- **AC2** — `data-raw/characterize_calc_se.R` run against a detached worktree at
  `1dec0a4` and against the branch: 48 configurations each side, names identical, and
  all 48 `identical()` on both the returned value and the recorded condition classes.
  Re-run after the gate fixes: still 48 of 48.
- **AC3** — all three `calc_se` help pages open with the deprecation, what goes away
  with it, and that instrument's replacement. `git diff` on the three roxygen blocks is
  a pure insertion: the existing description stands unchanged, including the PID-5
  page's two-case facet/domain account and its `NA`-masking sentence.
- **AC4** — `grep -rl "calc_se" vignettes/` returns four files. Each states the
  deprecation and its instrument's replacement: `hitopsr_scoring.Rmd` and
  `hitopbr_scoring.Rmd` point at their `interval_*()` sections, `pid5sf_scoring.Rmd`
  says the package has no PID-5 interval function, and `articles/modules-hitopsr.Rmd`
  — the file that only points readers elsewhere — says the argument is deprecated
  there too.
- **AC5** — NEWS.md's `# hitop (development version)` section carries a `## Deprecations`
  entry naming all three functions, the removal of the argument and its `_se` columns,
  and the replacement per instrument.
- **AC6** — `devtools::document()` no diff; `devtools::test()` FAIL 0, WARN 0,
  SKIP 4, PASS 16151; `devtools::check()` 0 errors / 0 warnings / 0 notes;
  `pkgdown::check_pkgdown()` no problems; `data-raw/check_line_endings.R` passed.
  All re-run after the gate fixes below.

No Driving RR on this milestone, so there are no projections to set against outcomes.

### Consistency gate

`cairn_validate.py` exit 0, all checks passed, 24 advisories (23 dangling pre-migration
`D-001`–`D-012` tokens and one references-staleness note, all pre-existing; the
`release window` advisory did not fire). No `DESIGN.md` principle changed — the edit is
to Known issues — so `cairn_impact.py` was not run. Toolchain half per `PROFILE.md`'s
`## consistency-gate` slot: recorded under AC6 above.

### Findings and dispositions

Three fresh-context reviewers ran on distinct evidence: an [O] diff-bug lens, an [S]
blame-history lens, and an [S] prior-review lens. The prior-review lens reported no
prior-review evidence bearing on the diff (the GitHub inline-comment probe returned
empty; no archived `## Review` finding on the touched files is reintroduced). The
blame-history lens returned one finding, the same one the diff lens ranked first.
Twelve distinct findings in all; none demonstrated an acceptance criterion failing, so
none met the return floor.

1. **Fixed.** `cairn/ROADMAP.md` lost the maintainer-run verification-tooling candidate
   row — the one Jeff dispositioned 2026-08-30 for promotion into a bounded milestone —
   and gained in its place a second copy of the browser-module-builder row already on
   file. Verified against `git diff origin/main..HEAD`. The row is restored; the
   duplicate is gone, the surviving browser-builder row keeping the compressed wording
   the botched edit had produced.
2. **Fixed.** NEWS.md's Deprecations entry said "nothing about the values they hold has
   changed", while the Breaking-changes entry four lines below in the same unreleased
   section moves `hbr_detachment_se` and `hbr_internalizing_se` via the HiTOP-BR item-36
   rekey. Narrowed to a claim about the deprecation, with the rekey named.
3. **Filed.** `data-raw/characterize_calc_se.R`'s condition-channel comparison is inert
   on its own matrix: all 48 configurations record no conditions, so it can catch an
   added condition but not a removed one. AC2 is met on the evidence — both sides
   signal the same nothing — but the guard is weaker than the criterion's wording
   suggests. Absorbed into the M068 test-reach candidate row.
4. **Filed.** The three `score_*()` functions now warn on their output path, but
   `test-append-collision.R`'s warnings-before-abort sweep still covers only
   `norm_pid5()` and the two `interval_*()` functions, so the ordering claim in
   `R/score_engine.R`'s new comment is asserted nowhere. Absorbed into the same row.
5. **Fixed.** `hitop_deprecated_calc_se` is declared a public contract by D-049(b) and
   NEWS tells readers to silence it by class, but no help page named it — against the
   package's own precedent, where `hitop_empty_selection` is named on all four raising
   functions' pages. One sentence added to each of the three `calc_se` help pages.
6. **Fixed here and at hygiene.** The ROADMAP hygiene stamp still recorded the
   disposition for the row finding 1 deleted, and the work log's byte figures did not
   match the file. Restoring the row settles the first; a superseding work-log line
   records the second, and the stamp is rewritten in the post-merge hygiene pass.
7. **Fixed.** `DESIGN.md` Known issue #4 said the masking inconsistency "will not be
   repaired", which outruns what was decided — the removal it defers to is a candidate
   row, and D-049's own falsifier is that row being dropped. Reworded to say the
   question reopens if the candidate is dropped.
8. **Fixed.** Prose seams in `vignettes/pid5sf_scoring.Rmd` from the rewrite: a full
   stop where a colon belonged, a leftover "We turn this on using `calc_se`" after two
   paragraphs telling the reader not to, and a sentence duplicated two paragraphs apart.
9. **Fixed.** `score_pid5()`'s help example still passed `calc_se = TRUE` with nothing
   saying why, on the page whose `@param` calls the argument deprecated. A two-line
   comment now says the call warns and that the PID-5 has no replacement.
10. **Fixed.** "Simple Standard Errors" headed a section that, in two of the three
    vignettes, now demonstrates nothing and redirects. The heading is now "Simple
    Standard Errors (deprecated)" in all three, and `test-vignette-se-prose.R`'s
    section-extraction regex was widened to keep matching it.
11. **Fixed.** Two in-loop assertions in `test-deprecated-calc_se.R` carried no `info =`,
    so a red result would not name the wrapper. Converted to `expect_equal(length(...))`
    with `info = fn`.
12. **Fixed.** `helper-fixtures.R`'s comment said the warning would be reported
    "~25 times per run"; the wrapper is used at 23 call sites. Corrected to the counted
    figure.

### Open item for the gate

`cairn/ROADMAP.md` is 24,185 bytes against its 24,000-byte budget. Restoring the row
finding 1 deleted put roughly 1,000 bytes back, and filing findings 3 and 4 added
about 570 more; eight of the widest rows were compressed in exchange, which recovered
most but not all of it. Dropping the filing of findings 3 and 4 lands the file at
about 23,615. The structural remedy — promoting the maintainer-verification row into
the bounded milestone it is already dispositioned for — is the maintainer's call, not
review's.
