# M43: Rename the HiTOP-SR subset family to modules

- **Status:** review
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** GP3
- **Branch/PR:** `m43-module-vocabulary` · https://github.com/jmgirard/hitop/pull/48

## Goal

Rename the scale-subset descriptor family to "module" across the exported API,
keeping every old name working behind a deprecation warning, and add a scale
browser so a researcher can see what is on offer without reading a data object.

## Scope

User-facing tier: the deliverables are exported functions, argument names, and
help pages that package users call directly.

**In:** `hitop_module()` exported alongside a deprecated `hitop_subset()`; a
`module =` argument on the five exported functions that take `subset =` today
(`score_hitopsr()`, `reliability_hitopsr()`, and the three
`generate_*_hitopsr()`), each still accepting `subset =` with a warning; a new
exported `available_scales()` listing an instrument's scales with their item
counts; the deprecation condition hand-rolled with `cli::cli_warn()`, adding no
dependency; NEWS, roxygen, and `_pkgdown.yml` updated to match.

**Out:** the article teaching the workflow → M44, which is written in the
vocabulary this milestone settles. The browser module builder → M45. Extending
modules to the HiTOP-BR and PID-5 → the standing "Generalize modularization to
BR/PID-5" candidate row, which this milestone deliberately precedes so the
generalization inherits one vocabulary. Scoring module-collected data with no
descriptor in hand → its own candidate row. Adding {lifecycle} as a dependency
→ rejected at the plan gate; a dependency change needs its own gate and
D-entry, and one warning does not earn one.

## Acceptance criteria

- [x] AC1 `hitop_module(instrument, scales)` is exported and returns an object
      of class `hitop_module` carrying the same fields `hitop_subset()` returns
      today. A test builds the same scale selection through both constructors
      and asserts the two objects are identical apart from their class
      attribute.
- [x] AC2 `hitop_subset()` still returns a usable descriptor and signals a
      deprecation condition carrying a stable class. A test fires it, asserts
      the condition by that class rather than by message text, and asserts the
      returned object still scores through `score_hitopsr()`.
- [x] AC3 Each of the five exported functions listed in Scope takes `module =`,
      accepts either a `hitop_module` or a legacy `hitop_subset` object, signals
      the AC2 deprecation condition when `subset =` is supplied, and errors when
      both are supplied. One test per function fires all three branches and
      asserts each condition by class.
- [x] AC4 The rename moves no number. A test compares, for three module shapes —
      a single-scale module, the four-scale module the current vignette uses,
      and a module whose scales include a reverse-keyed item — the scale scores,
      standard errors (`calc_se = TRUE`), and Cronbach's alpha produced through
      `module =`, through the deprecated `subset =`, and by a full-instrument
      run restricted to the same scales, asserting all three agree.
- [x] AC5 `available_scales("hitopsr")` returns one row for each row of
      `hitopsr_scales`, carrying the display name, the camelCase stem, and the
      item count; a test asserts row-for-row equality against that table. An
      unsupported instrument errors with the message `hitop_module()` gives,
      asserted by condition class.
- [x] AC6 A case-sensitive `grep -rn subset` over `R/`, `man/`, `NEWS.md`,
      `README.Rmd`, `vignettes/`, and `_pkgdown.yml` returns only the
      deprecation shims and their roxygen, plus any use of base R's own `subset`
      generic; the command and its full output are recorded in the Review.
- [ ] AC7 `devtools::document()` produces no diff; `devtools::test()` and
      `devtools::check()` are clean; `pkgdown::check_pkgdown()` passes with
      `hitop_module` and `available_scales` carrying `_pkgdown.yml` reference
      rows; NEWS.md records the rename and the deprecation.

## Coverage

- AC1 → T1, T5
- AC2 → T2, T5
- AC3 → T3, T5
- AC4 → T5
- AC5 → T4, T5
- AC6 → T6, T7
- AC7 → T6, T7

## Tasks

- [x] T1 Rename `R/subset.R` → `R/module.R`: `hitop_module()`,
      `print.hitop_module()`, and the internal helpers `apply_subset()` /
      `subset_engine_inputs()` renamed to match, with both classes accepted
      wherever a descriptor is validated (`R/subset.R:140`, `R/subset.R:180`).
- [x] T2 Deprecation shims: `hitop_subset()` and `print.hitop_subset()` delegate
      after one `cli::cli_warn()` carrying a stable condition class, emitted by
      a shared internal helper in `R/util.R`.
- [x] T3 Rename `subset =` → `module =` on the five exported functions, each
      accepting the legacy argument and erroring when both are given. Edit
      `R/generate_qualtrics.R` and `R/generate_redcap.R` in place — the M24
      lesson records them as CRLF files a whole-file rewrite silently converts.
- [x] T4 Write `available_scales()` reading `hitopsr_scales`, with its help page
      and `_pkgdown.yml` row.
- [x] T5 Tests for AC1-AC5, every condition fired and asserted by class.
- [x] T6 Docs sweep for AC6: roxygen text, `@examples`, NEWS entry, then
      `devtools::document()`.
- [x] T7 Full check: `devtools::test()`, `devtools::check()`,
      `pkgdown::check_pkgdown()`; record AC6's grep output.

## Work log

- 2026-08-21: created by /milestone-plan.
- 2026-08-21: plan gate chose renaming the exported API over changing the prose alone, because `subset` shadows base R's `subset()` generic and the vocabulary should settle before modularization spreads to the BR and PID-5; falsified by a user reporting the deprecation warning as more disruptive than the two-vocabulary state it removes.
- 2026-08-21: plan chose a hand-rolled `cli::cli_warn()` with a stable class over adding {lifecycle}, because one deprecation does not earn a dependency (GP4) and a dependency change needs its own gate and D-entry; falsified by a second deprecation arriving that wants the lifecycle badge machinery in the help pages.
- 2026-08-21: plan chose `available_scales()` over `hitop_scales()` for the browser, because the latter reads as a sibling of the `hitopsr_scales`/`hitopbr_scales` datasets and would collide with them in autocomplete; falsified by a second instrument-listing function arriving that wants a shared `hitop_*` prefix.
- 2026-08-21: the criteria audit ran in **full** mode over AC1-AC7 and returned three findings, all fixed before the criteria were written. AC4 probed one module shape, leaving reverse-keying, single-scale width, and standard errors unvaried — now three shapes with `calc_se = TRUE`. AC5 promised values were "read from rather than restated" from `hitopsr_scales`, an implementation property no test asserts — narrowed to the row-for-row equality the test actually checks. AC6's sweep was drafted as `\bsubset\b`, which cannot match inside `hitop_subset` because `_` is a word character, so the sweep would have missed the identifier it exists to find — now an unanchored `subset`. The audit ran inline in this session rather than in a fresh-context reader, because this session is instructed not to spawn subagents unless asked; the reader-freshness the instrument normally provides was not obtained.
- 2026-08-21: implement gate — the stray `jmgirard.r-universe.dev` clone (created inside the repo by a command this session handed the user) was moved to `~/github/`, leaving the tree clean before branching.
- 2026-08-21: implement gate held AC5 as written — `available_scales()` covers only the HiTOP-SR, matching what `hitop_module()` supports, over also listing the 8 HiTOP-BR scales (offered and declined by the maintainer); widening it later is additive, and listing scales no module can yet be built from would promise a reach the module family lacks.
- 2026-08-21: T1-T4 landed in one checkpoint rather than four: the package does not load consistently part-way through the rename (an exported function calling a renamed internal), so no intermediate state passes the profile's verify slot. Tasks are ticked individually; the commit is one.
- 2026-08-21: minor task refinement to T2 — `print.hitop_subset()` delegates to the module printer WITHOUT warning. T2's wording had it warn too, but printing an object is not a use of the deprecated API: the call that built it already warned, and warning again on every display would punish the user for looking at what they were handed. The printer reads its label off `class(x)[[1L]]` so the legacy object still prints as `<hitop_subset>`.
- 2026-08-21: the M29 lesson landed exactly as recorded — converting the deprecation to a `cli::cli_warn()` condition made every existing test that merely *called* `hitop_subset()` or passed `subset =` fail, not just the ones asserting on messages. All seven test files were swept, `test-subset.R`/`test-subset-generation.R`/`_snaps/subset.md` renamed to `module`, and the suite is green at 13456 passing, 0 failures, 0 warnings, 1 skip.
- 2026-08-21: the M24 CRLF lesson held — `R/generate_qualtrics.R` and `R/generate_redcap.R` were patched with newline-preserving edits and each shows a 12-line diff with CRLF endings intact, rather than the ~1,700-line inflation a whole-file rewrite causes.
- 2026-08-21: review opened draft PR #48; CI green so far on macOS, ubuntu release/oldrel-1, pkgdown, and test-coverage, with windows and ubuntu-devel still pending. Checkpoint: AC1-AC6 verified with fresh evidence and ticked; AC7 awaits the local `devtools::check()` still running, so its box stays unticked. Three fresh-context reviewers were spawned after the maintainer explicitly authorised subagents for this session (they are otherwise disabled here); the blame-history lens has reported zero findings, the diff-bug and prior-review lenses are still running.

## Decisions

## Review

Evidence gathered 2026-08-21 on `m43-module-vocabulary` @ 6b8b17e, PR #48.
Working tree clean, branch level with `origin/main` (0 commits behind).

**AC1** — `test-deprecated.R` "hitop_module() and hitop_subset() build identical
descriptors" passes: `expect_s3_class()` on each, then `expect_identical()` over
both `unclass()`ed, so nothing but the class attribute may differ.

**AC2** — same file, two tests pass: the constructor warns with
`class = "hitop_deprecated_subset"` (asserted by class, not message), and a
descriptor it returns scores through `score_hitopsr()` to the expected two
columns.

**AC3** — four tests iterate one table row per exported consumer
(`score_hitopsr`, `reliability_hitopsr`, and the three `generate_*_hitopsr`),
firing all three branches per function: `module =` silent, a legacy
`hitop_subset` object accepted, `subset =` warning by class, both arguments
erroring by `class = "hitop_both_module_args"`. All pass.

**AC4** — two tests sweep three module shapes (single-scale, reverse-keyed,
and the four-scale vignette set), comparing scores plus `calc_se = TRUE`
standard errors, and alpha, across `module =`, deprecated `subset =`, and a
full 405-item run restricted to the same columns. All three agree in every
cell. Test file totals: `test-deprecated.R` 38 passing, 0 failures.

**AC5** — `test-available_scales.R` 169 passing, 0 failures: row-for-row
equality against `hitopsr_scales` on all three columns and row count; every one
of the 76 scales built by display name and by camelCase stem; the unsupported
and unknown instrument branches asserted by condition class against
`hitop_module()`'s; and the two functions' messages asserted identical.

**AC6** — sweep re-run fresh: `grep -rn subset R/ man/ NEWS.md README.Rmd
vignettes/ _pkgdown.yml` returns 79 lines. Every one is a deprecation shim, its
roxygen, or the generated `.Rd` for it — except eight uses of the ordinary
English or mathematical word, each read and confirmed unrelated to this family:
`R/plot_pid5.R:127` (the verb, "subset {.arg data} to the row you want"),
`R/data.R:49` and `man/pid_domains.Rd:22` (the PID-5 "15-facet primary subset"),
`R/rank_scales.R:21` and `man/rank_scales.Rd:38` ("A subset of `scales`"),
`vignettes/pid5bf_scoring.Rmd:30`, `vignettes/articles/overview.Rmd:55`, and
`vignettes/articles/download-hitophsum.Rmd:11` ("a subset of possible items").

**Consistency gate** — universal cairn checks: `cairn_validate` exit 0, all 16
PASS, 4 OK, 21 pre-existing advisory `dangling id tokens` warnings (D-001..D-012
live in DESIGN.md's own decision log, not DECISIONS.md; not introduced here).
No principle changed, so `cairn_impact` is skipped. Toolchain checks from the
`r-package` profile: `devtools::document()` produces no diff (`git status` on
`man/` and `NAMESPACE` empty after a fresh run); `pkgdown::check_pkgdown()`
"No problems found"; README.Rmd/README.md untouched by this milestone.

- 2026-08-21: T5 — `test-deprecated.R` (AC1-AC4) and `test-available_scales.R` (AC5) added; suite 13456 -> 13663 passing, 0 failures, 0 warnings, 1 skip. Two branches gained condition classes so the criteria's "asserted by class" is honest rather than an `rlang_error` match: `hitop_both_module_args` on the both-arguments error, and `hitop_unknown_instrument`/`hitop_unsupported_instrument` on the instrument guard.
- 2026-08-21: refactor found at T5 — `hitop_module()` and `available_scales()` carried byte-identical instrument guards, and AC5 requires the browser to reject exactly what the constructor rejects with the same words. Extracted to `validate_module_instrument()` in `R/module.R`, with a test asserting the two messages are identical rather than trusting two copies to stay in step.
- 2026-08-21: T5 mutation check — five mutations each turned the new tests red against a clean baseline, and the tree was restored after each: dropping the deprecation condition class (2), removing the both-arguments error so `module` silently wins (1), muting the `subset =` warning (1), removing the browser's shared instrument guard (4), and zeroing module reverse-keying (2). The first harness attempt reported a non-zero BASELINE because `testthat::test_file()` runs without the package loaded; it was fixed with `pkgload::load_all()` before any mutation was read, so no verdict rests on it.
- 2026-08-21: T6 — the two unreleased 0.2.0 NEWS entries describing this feature were rewritten in module vocabulary rather than contradicted by a fourth entry, since 0.2.0 has not shipped; a separate entry covers the deprecation for anyone already on the development version. `_pkgdown.yml` swaps `hitop_subset` for `hitop_module` and adds `available_scales`; `hitop_subset` is `@keywords internal`, so pkgdown excludes it and `check_pkgdown()` is clean without a row for it. The scoring vignette's section is renamed and re-worded here; M44 replaces it with a link.
- 2026-08-21: T7 — `devtools::document()` no diff, `devtools::test()` 13663 passing / 0 failures / 0 warnings / 1 skip, `pkgdown::check_pkgdown()` "No problems found", `devtools::check()` Status: OK (0 errors, 0 warnings, 0 notes). AC6's sweep is clean: every remaining `subset` in the swept paths is either a deprecation shim or the ordinary English/mathematical word in code unrelated to this family.
- 2026-08-21: docs sweep surfaced a terminology collision recorded as a ROADMAP candidate — the HiTOP-HSUM's Society name is the Harmful Substance Use *Module*, so "module" now names two things. Nothing user-facing conflates them today; IP1 bars renaming the instrument, so any fix is package-side and waits for modularization to reach other instruments.
