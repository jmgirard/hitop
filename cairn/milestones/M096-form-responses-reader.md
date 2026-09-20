# M096: The package reads hitop-form response files into one data frame and scores them through a module descriptor

- **Status:** planned
- **Priority:** normal
- **Depends on:** M095
- **Driving RR:** —
- **Principles touched:** IP2, GP3, GP4
- **Resolves:** —
- **Surface tier:** user-facing — a new exported function and an article section
- **Branch/PR:** —

## Goal

Add an exported reader that turns hitop-form CSV files into one tibble the scoring functions take as they are, document the hand-off in the modules article, and prove a saved form round-trips through `score_hitopsr()` with `read_module()`.

## Scope

**In:** `read_form_responses(path)` in `R/read_form_responses.R`, base R plus the existing {tibble} Import; two classed conditions; tests; the three CSV fixtures M095 committed, copied with the module's descriptor; an article section; NEWS; the pkgdown reference index.

**Out:** reading the storage adapters' CSV exports → the online-form candidate row. Scoring the HSUM (IP3). PID-5 fixtures → the online-form candidate row.

## Acceptance criteria

- [ ] AC1: `read_form_responses()` given a directory path or a character vector of hitop-form CSV paths returns a tibble with one row per file, the five lead columns first (`study`, `participant` and `instrument` as character, `form_build` as Date, `submitted` as POSIXct in UTC), then the item columns as integers in the column order of the first path after sorting (a directory is read as its CSV paths sorted with `sort()` in the C locale); tests cover a directory, a vector of two files, and one file.
- [ ] AC2: Files whose item columns differ in name, in count, or in order abort under one classed condition naming the files that differ, and a directory holding no CSV aborts under a classed condition; tests fire each of the four cases and assert its class.
- [ ] AC3: The three CSVs M095 committed (the full HiTOP-BR, the full HiTOP-SR, the shuffled two-scale module), copied under `tests/testthat/fixtures/` with the module's descriptor, read by `read_form_responses()` and scored with `score_hitopbr(data, items = <item columns>)`, `score_hitopsr(data, items = <item columns>)` and `score_hitopsr(data, items = <item columns>, module = read_module(<descriptor>), layout = "printed")`, yield scale scores equal to the expected values: for the module fixture, means hand-computed from its responses and written as literals; for the two full forms, means recomputed in the test from the fixture's responses and the `*_items` reverse flags and `*_scales` item lists, independently of the scoring engine.
- [ ] AC4: The modules article gains a section on collecting responses with hitop-form and scoring them, with a runnable example over the module fixture; `NEWS.md` names the function; `_pkgdown.yml` lists it; `devtools::check()` is clean.

## Coverage

- AC1 → T1, T2
- AC2 → T1, T2
- AC3 → T1, T3
- AC4 → T4

## Tasks

- [ ] T1: `R/read_form_responses.R` with its two conditions raised by `cli::cli_abort()` under classes on the D-034(c) pattern, recorded in a D-entry at implementation. (RB tripwire: irreversible-api)
- [ ] T2: `tests/testthat/test-read_form_responses.R` for AC1 and AC2.
- [ ] T3: Copy the three fixtures and the descriptor from hitop-form's `tests/fixtures/`; the round-trip tests with hand-computed expected values.
- [ ] T4: Article section, NEWS, `_pkgdown.yml`, `devtools::document()`, `devtools::check()`.

## Work log

- 2026-09-20: created by /milestone-plan.
- 2026-09-20: criteria audit ran in full mode on a fresh [O] reader over the six-form draft (see M094's work log); this file carries its fixes: `data` in the scoring calls, three named ways columns differ, the literal-values clause dropped, AC3 mapped to T1 as well as T3.
- 2026-09-20: the audit's second pass (see M094's work log) fixed here: AC1 defines the column order by sorted path, and AC3 takes hand-computed literals for the module and independent recomputation for the two full forms.
- 2026-09-20: plan chose an exported `read_form_responses()` over an article recipe of `do.call(rbind, lapply(files, read.csv))` because the recipe leaves typing, ordering and disagreement checks to every researcher; falsified by the device-only route going unused.

## Decisions

## Review
