# M43: Rename the HiTOP-SR subset family to modules

- **Status:** in-progress
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** GP3
- **Branch/PR:** `m43-module-vocabulary`

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

- [ ] AC1 `hitop_module(instrument, scales)` is exported and returns an object
      of class `hitop_module` carrying the same fields `hitop_subset()` returns
      today. A test builds the same scale selection through both constructors
      and asserts the two objects are identical apart from their class
      attribute.
- [ ] AC2 `hitop_subset()` still returns a usable descriptor and signals a
      deprecation condition carrying a stable class. A test fires it, asserts
      the condition by that class rather than by message text, and asserts the
      returned object still scores through `score_hitopsr()`.
- [ ] AC3 Each of the five exported functions listed in Scope takes `module =`,
      accepts either a `hitop_module` or a legacy `hitop_subset` object, signals
      the AC2 deprecation condition when `subset =` is supplied, and errors when
      both are supplied. One test per function fires all three branches and
      asserts each condition by class.
- [ ] AC4 The rename moves no number. A test compares, for three module shapes —
      a single-scale module, the four-scale module the current vignette uses,
      and a module whose scales include a reverse-keyed item — the scale scores,
      standard errors (`calc_se = TRUE`), and Cronbach's alpha produced through
      `module =`, through the deprecated `subset =`, and by a full-instrument
      run restricted to the same scales, asserting all three agree.
- [ ] AC5 `available_scales("hitopsr")` returns one row for each row of
      `hitopsr_scales`, carrying the display name, the camelCase stem, and the
      item count; a test asserts row-for-row equality against that table. An
      unsupported instrument errors with the message `hitop_module()` gives,
      asserted by condition class.
- [ ] AC6 A case-sensitive `grep -rn subset` over `R/`, `man/`, `NEWS.md`,
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
- [ ] T5 Tests for AC1-AC5, every condition fired and asserted by class.
- [ ] T6 Docs sweep for AC6: roxygen text, `@examples`, NEWS entry, then
      `devtools::document()`.
- [ ] T7 Full check: `devtools::test()`, `devtools::check()`,
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

## Decisions

## Review