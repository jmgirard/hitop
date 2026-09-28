# M139: read_form_responses() reads a file whose item columns span two or more instruments

- **Status:** planned
- **Priority:** normal
- **Depends on:** M136
- **Driving RR:** —
- **Principles touched:** GP2, GP3
- **Resolves:** —
- **Surface tier:** user-facing — an exported reader's column contract, its help page and the articles
- **Branch/PR:** —

## Goal

`read_form_responses()` reads the file that hitop-form will write for a link that fields several instruments in one session. The file holds one group of item columns per instrument, and the lead columns name every instrument and every form build.

## Scope

**In:** In hitop, the reader in `R/read_form_responses.R`, its tests, its help page, the articles that describe its columns, and NEWS. D-080, which T1 records at the pre-implementation gate, governs the file shape. This milestone ships before M140, so the reader accepts the file before any page writes it, as M115 shipped before M116.

**Out:** The page that fields several instruments goes to M140. Scoring stays one call per instrument, with the item columns chosen by stem, as the article shows today. Files of different instrument sets meet the mismatch of D-064 across files. A file whose rows differ in instrument set meets the refusal of AC4.

## Acceptance criteria

- [ ] AC1: A file whose item columns form two or more groups reads. Each group holds the columns of one instrument's stem, and no stem repeats. Once the optional lead columns and the `q_` columns are set aside, the columns of each group are contiguous. The `instrument` cell of each row holds the stems of the groups in file order, joined by single spaces. The result has the eight lead columns, then the item columns in file order, then the `q_` columns of M136. Tests read files with two and with three instruments, with and without `q_` columns. Further tests place a `q_` column and an `item_order` column between two groups and inside one group. Tests choose the columns of each instrument by stem and score them with its scoring function.
- [ ] AC2: The `form_build` cell of a multi-instrument row holds one date per stem, in the order of the `instrument` cell, joined by single spaces. Each date parses as the single date does today. The result's `form_build` is character for every file, holding each row's dates as written. (RB tripwire: irreversible-api) This changes the type of the column from `Date` for single-instrument files as well. T1 poses it beside the alternative of a `Date` column plus a new optional lead column. Tests read single-instrument and multi-instrument files and assert the column's type and values.
- [ ] AC3: Under shuffle, the `item_order` cell of a multi-instrument row holds one group per stem, in the order of the `instrument` cell, joined by ` | `. Each group is that instrument's item numbers in the file, each once, joined by single spaces. A blank cell reads as `NA`, as D-070(c) states. A single-instrument cell keeps the grammar of D-070. Tests read a valid cell and a blank cell. They also read a cell with a group missing, a group in the wrong order, and a number from another instrument. Further cells hold a group that repeats or omits one of its own numbers, and the separators `1|2` and `||`.
- [ ] AC4: The reader refuses each of these faults, as an unclassed `cli_abort()` that names the file, and the row where the fault is in a row. The faults are an `instrument` cell that differs from the stems of the groups, and a stem whose columns are split by another stem's columns. The third fault is a `form_build` cell whose date count differs from the stem count, or that holds a date that does not parse. The fourth fault is an `item_order` cell that breaks AC3. Tests assert the message of each refusal.
- [ ] AC5: A script runs on main and on the branch. It reads every file that the existing tests in `tests/testthat/test-read_form_responses.R` read. For each result, `form_build` on the branch equals `format()` of main's column, and every other column is `identical()`. For each refusal, the condition class and message match. The exceptions are the refusals this milestone replaces, which the work log lists by test name with the branch's message.
- [ ] AC6: `?read_form_responses`, the online-collection article and the modules-hitopsr article state the multi-instrument file shape and the `form_build` type. The help page and the online-collection article also state the `item_order` groups and how to choose one instrument's columns for scoring. A grep for `form_build` over `vignettes/` and `R/` finds no statement that it is a date. NEWS flags the `form_build` type change. `devtools::check()` gives 0 errors, 0 warnings and 0 notes, and `cairn_validate` exits 0.

## Coverage

- AC1 → T2, T3
- AC2 → T1, T2, T3
- AC3 → T2, T3
- AC4 → T2, T3
- AC5 → T3
- AC6 → T4, T5

## Tasks

- [ ] T1: At the pre-implementation gate, settle the `form_build` type of AC2 and the ` | ` separator of AC3 with Jeff, and record D-080 on the file shape. It annotates D-064, D-070(c) and D-079(c). Under the alternative, the item columns start tenth, which D-071 names as a breaking move, and the gate says so. If Jeff chooses the alternative, amend AC2 and AC5 here and AC3 and AC4 of M140 through the gate.
- [ ] T2: In `R/read_form_responses.R`, replace the one-stem rule with the grouping of AC1. Add the `instrument`, `form_build` and `item_order` rules of AC2 and AC3, and the refusals of AC4.
- [ ] T3: Write the tests of AC1 to AC4 with fixtures written by rule in `tests/testthat/fixtures/`. Plant each refusal and show it red before trusting its green. Write the comparison script of AC5 and record its result in the work log.
- [ ] T4: Update the roxygen help and run `devtools::document()`. Update the two articles and write the NEWS entry.
- [ ] T5: Run `devtools::check()` and `cairn_validate`.

## Work log

- 2026-09-28: created by /milestone-plan. Jeff asked at the plan gate to plan several instruments in one link now.
- 2026-09-28: criteria audit ran in full mode (user-facing tier) on a fresh [O] reader. It returned 7 findings, all fixed before the commit: the AC5 exceptions, contiguity beside `q_` columns, date parsing, the `item_order` probes, the articles that state the type, the planting step moved to T3, and the Out wording.
- 2026-09-28: plan proposes a character `form_build` over a `Date` column plus a new lead column, because one column keeps the item columns starting ninth. The choice stays open for the pre-implementation gate (T1). Falsified by callers who compare `form_build` as a date.

## Decisions

## Review
