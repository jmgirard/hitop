# M096: The package reads hitop-form response files into one data frame and scores them through a module descriptor

- **Status:** review
- **Priority:** normal
- **Depends on:** M095
- **Driving RR:** —
- **Principles touched:** IP2, GP3, GP4
- **Resolves:** —
- **Surface tier:** user-facing — a new exported function and an article section
- **Branch/PR:** `m096-form-responses-reader`

## Goal

Add an exported reader that turns hitop-form CSV files into one tibble the scoring functions take as they are, document the hand-off in the modules article, and prove a saved form round-trips through `score_hitopsr()` with `read_module()`.

## Scope

**In:** `read_form_responses(path)` in `R/read_form_responses.R`, base R plus the existing {tibble} Import; two classed conditions; tests; the three CSV fixtures M095 committed, copied with the module's descriptor; an article section; NEWS; the pkgdown reference index.

**Out:** reading the storage adapters' CSV exports → the online-form candidate row. Scoring the HSUM (IP3). PID-5 fixtures → the online-form candidate row.

## Acceptance criteria

- [x] AC1: `read_form_responses()` given a directory path or a character vector of hitop-form CSV paths returns a tibble with one row per file, the five lead columns first (`study`, `participant` and `instrument` as character, `form_build` as Date, `submitted` as POSIXct in UTC), then the item columns as integers in the column order of the first path after sorting (a directory is read as its CSV paths sorted with `sort()` in the C locale); tests cover a directory, a vector of two files, and one file.
- [x] AC2: Files whose item columns differ in name, in count, or in order abort under one classed condition naming the files that differ, and a directory holding no CSV aborts under a classed condition; tests fire each of the four cases and assert its class.
- [x] AC3: The three CSVs M095 committed (the full HiTOP-BR, the full HiTOP-SR, the shuffled two-scale module), copied under `tests/testthat/fixtures/` with the module's descriptor, read by `read_form_responses()` and scored with `score_hitopbr(data, items = <item columns>)`, `score_hitopsr(data, items = <item columns>)` and `score_hitopsr(data, items = <item columns>, module = read_module(<descriptor>), layout = "printed")`, yield scale scores equal to the expected values: for the module fixture, means hand-computed from its responses and written as literals; for the two full forms, means recomputed in the test from the fixture's responses and the `*_items` reverse flags and `*_scales` item lists, independently of the scoring engine.
- [x] AC4: The modules article gains a section on collecting responses with hitop-form and scoring them, with a runnable example over the module fixture; `NEWS.md` names the function; `_pkgdown.yml` lists it; `devtools::check()` is clean.

## Coverage

- AC1 → T1, T2
- AC2 → T1, T2
- AC3 → T1, T3
- AC4 → T4

## Tasks

- [x] T1: `R/read_form_responses.R` with its two conditions raised by `cli::cli_abort()` under classes on the D-034(c) pattern, recorded in a D-entry at implementation. (RB tripwire: irreversible-api)
- [x] T2: `tests/testthat/test-read_form_responses.R` for AC1 and AC2.
- [x] T3: Copy the three fixtures and the descriptor from hitop-form's `tests/fixtures/`; the round-trip tests with hand-computed expected values.
- [x] T4: Article section, NEWS, `_pkgdown.yml`, `devtools::document()`, `devtools::check()`.

## Work log

- 2026-09-20: created by /milestone-plan.
- 2026-09-20: criteria audit ran in full mode on a fresh [O] reader over the six-form draft (see M094's work log); this file carries its fixes: `data` in the scoring calls, three named ways columns differ, the literal-values clause dropped, AC3 mapped to T1 as well as T3.
- 2026-09-20: the audit's second pass (see M094's work log) fixed here: AC1 defines the column order by sorted path, and AC3 takes hand-computed literals for the module and independent recomputation for the two full forms.
- 2026-09-20: plan chose an exported `read_form_responses()` over an article recipe of `do.call(rbind, lapply(files, read.csv))` because the recipe leaves typing, ordering and disagreement checks to every researcher; falsified by the device-only route going unused.
- 2026-09-20: implement gate (Jeff): classes `hitop_form_responses_mismatch` and `hitop_form_responses_none` (D-064; the Fable escalation offered for the naming declined); the CSV fixtures stored as LF under the line-ending policy, CRLF covered by a test-written file; the article reads the fixture by a path relative to `vignettes/articles/`.
- 2026-09-20: T1 done. Two classed refusals; the other refusals (not a file, other lead columns, more than one row, a non-integer item value, a stamp that does not parse) are unclassed and name the file. Paths always sort in the C locale (`sort(method = "radix")`), a vector included, so AC1's "first path after sorting" holds either way. `_pkgdown.yml` row added with the export.
- 2026-09-20: T2 and T3 done in one file, `test-read_form_responses.R` (69 expectations): directory, vector of two, one file, C-locale order, first-file column order, blank item, LF and BOM inputs; the four AC2 cases by class with the differing files named; the plain refusals; the three fixtures scored against table-derived means (full forms) and hand-computed literals (module: Agoraphobia 3, Distress-Dysphoria 39/16), with a control showing the printed remap moves the result. Fixture provenance in `tests/testthat/fixtures/README.md` (hitop-form 21a1d1c).
- 2026-09-20: found while writing AC3's module test: `score_hitopsr(layout = "printed")` runs the ascending-name heuristic on the caller's `items`, which warns whenever printed-order columns are named by item number, though that order is the case `layout = "printed"` exists for. Positions (`match(item_cols, names(data))`) avoid it, so the test and the article pass positions. Not in scope (a `score_hitopsr()` behavior change); a candidate row records it.
- 2026-09-20: `devtools::test()` after T1–T3: one failure, `test-vignette-export-coverage.R` names `read_form_responses` as shown in no vignette; T4's article section clears it. Everything else green.
- 2026-09-20: T4 done. `modules-hitopsr.Rmd` gains "Collecting Responses Online with hitop-form" (the link builder, the file's shape, `read_form_responses()` over the module fixture by a path relative to the article, scoring by position under `layout = "printed"`, the full-form case); the article renders and its chunk prints Agoraphobia 3 and Distress-Dysphoria 2.44. NEWS entry; `_pkgdown.yml` row landed with T1. `devtools::test()` green, `pkgdown::check_pkgdown()` clean, `devtools::check()` 0 errors / 0 warnings / 0 notes.
- 2026-09-20: ROADMAP went to 24,592 bytes with the new candidate row; four of its widest rows compressed in place (online-form, verification-reach, builder smoke test, `pid_norms` schema; one M094 detail list now points at the archive summary) to 23,993.
- 2026-09-20: claim audit: 60 claims read, 1 corrected — vignettes/articles/modules-hitopsr.Rmd ("sends nothing anywhere" replaced by "no answer is sent anywhere", since the page fetches the instrument export); the reader also flagged the NEWS sentence on `items` as unqualified for shuffled modules, reworded; both re-read once, both hold. Noted by the reader, unchanged: the checked-out fixture CSVs hold CRLF locally while the index holds LF, both of which the reader parses.
- 2026-09-20: correction to the ROADMAP line above: the file measured 24,042 bytes at the T4 commit, not 23,993; the M096 candidate row was trimmed once more to bring it under 24,000. Two status-mirror edits in that commit went through `sed` rather than the Edit tool, against the tracking rules' edit-tool rule; noted, not repeated.
- 2026-09-20: review: AC1–AC4 verified, gate clean, three lenses run (13 diff-bug findings, the rest clean). One Review-section punctuation edit went through `sed`, against the edit-tool rule. Awaiting the merge gate.

## Decisions

## Review

Evidence gathered 2026-09-20 on `m096-form-responses-reader` at 68f2d055. `origin/main` did not move after the branch was cut, so no sync merge was needed.

- AC1: `devtools::test()` is green with 0 failures. The 11 skips all predate the branch and sit in other files. `test-read_form_responses.R` runs 22 tests and 69 expectations with 0 failures. Its AC1 tests cover a directory that also holds a non-CSV file, a vector of two files, and one file. They also cover the C-locale sort (`Zed` before `alpha`), first-file column order, and the five lead column types. Integer items, a blank item as `NA`, LF endings and a BOM are covered too. Verified.
- AC2: the same file fires the name, count and order mismatches and the empty directory. Each is caught by class, three under `hitop_form_responses_mismatch` and two under `hitop_form_responses_none`. The mismatch tests assert the differing file's basename and the word that names how it differs. One test shows that a conforming file is not named. Verified.
- AC3: the three fixtures under `tests/testthat/fixtures/` were copied with `module-shuffled.json`, and the fixtures README records their provenance. They score through `score_hitopbr()`, `score_hitopsr()` and `score_hitopsr(module = read_module(...), layout = "printed")`. For the full forms, the test recomputes the expected means from `*_items$Reverse` and `*_scales$itemNumbers` with a helper that never calls the scoring engine. For the module, the expected values are hand-worked literals 3 and 2.4375 in a comment table. A control test shows that instrument-order scoring gives a different value. Verified.

- AC4: `modules-hitopsr.Rmd` renders with the development package loaded and carries the section "Collecting Responses Online with hitop-form". Its chunk reads the module fixture and prints Agoraphobia 3 and Distress-Dysphoria 2.44. `NEWS.md` names `read_form_responses()` in a new-features entry. `_pkgdown.yml` lists it and `pkgdown::check_pkgdown()` finds no problem. `devtools::check()`: 0 errors, 0 warnings, 0 notes. A first run showed one NOTE for an ignored `Rplots.pdf` that the concurrent test run wrote at the top level. That file was deleted and the rerun was clean. Verified.

Consistency gate: `cairn_validate.py` exit 0, all checks pass, 24 advisory warnings that predate the branch. No `DESIGN.md` principle changed, so no impact report. `devtools::document()` leaves no diff. `README.Rmd` is untouched by the branch. No new top-level file. Driving RR is none, so no projection pairs.

Independent review (three fresh-context lenses, 2026-09-20). Blame-history lens: no conflict with any past milestone or D-entry. Prior-review lens: no prior-review evidence on the touched files, and the PR-comments probe returned empty. Diff-bug lens: 13 findings, ranked by the reviewer, verified against the implementation where marked. Dispositions are recorded after the gate.

1. The documented scoring route exists only to dodge a warning whose own remedy silently produces wrong scores. Following the `warn_item_order()` advice under `layout = "printed"` returns 2.6 and 2.5625 against the correct 3 and 2.4375, with no error. Reproduced. Pre-existing in `layout_items()`, already a candidate row.
2. An item value past the integer range passes the whole-number guard, then `as.integer()` returns `NA` with a base warning, so the value reads as blank. Reproduced.
3. A stamp with fractional seconds (`...44.123Z`) is refused. hitop-form trims them today, so the coupling is undocumented and untested.
4. A duplicate item column reaches `tibble::as_tibble()` and fails with a tibble error that names neither the file nor the package.
5. `form_build` parses leniently: `2026-09-20T99` reads as 2026-09-20. Reproduced.
6. The mismatch message pairs every differing file with the union of reasons ("count and order" for both files). Reproduced. AC2 asks only that the files be named, so it still passes.
7. `na.strings = character(0)` makes a literal `NA` cell abort as not a whole number.
8. The `.csv` directory scan is case-sensitive, so a folder of `.CSV` files aborts as empty.
9. The article reaches the fixture through `../../tests/testthat/fixtures`, coupling a published article to a test path.
10. AC3 writes `items = <item columns>` and the module test passes positions.
11. AC4's box was unticked at the time of the review (its evidence was still pending).
12. `cairn/ORACLES.md` gains no row for the AC3 oracles. Standing drift since M089.
13. Minor: a file with no trailing newline leaks the base "incomplete final line" warning. A subdirectory named `x.csv` is listed and hands `read.csv` a base error. A file with zero item columns reads without complaint, untested. `expect_error(read_form_responses(1), "path")` matches loosely.
