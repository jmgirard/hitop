# M139: read_form_responses() reads a file whose item columns span two or more instruments

- **Status:** review
- **Priority:** normal
- **Depends on:** M136
- **Driving RR:** —
- **Principles touched:** GP2, GP3
- **Resolves:** —
- **Surface tier:** user-facing — an exported reader's column contract, its help page and the articles
- **Branch/PR:** m139-form-reader-multi-instrument

## Goal

`read_form_responses()` reads the file that hitop-form will write for a link that fields several instruments in one session. The file holds one group of item columns per instrument, and the lead columns name every instrument and every form build.

## Scope

**In:** In hitop, the reader in `R/read_form_responses.R`, its tests, its help page, the articles that describe its columns, and NEWS. D-080, which T1 records at the pre-implementation gate, governs the file shape. This milestone ships before M140, so the reader accepts the file before any page writes it, as M115 shipped before M116.

**Out:** The page that fields several instruments goes to M140. Scoring stays one call per instrument, with the item columns chosen by stem, as the article shows today. Files of different instrument sets meet the mismatch of D-064 across files. A file whose rows differ in instrument set meets the refusal of AC4.

## Acceptance criteria

- [x] AC1: A file whose item columns form two or more groups reads. Each group holds the columns of one instrument's stem, and no stem repeats. Once the optional lead columns and the `q_` columns are set aside, the columns of each group are contiguous. The `instrument` cell of each row holds the stems of the groups in file order, joined by single spaces. The result has the eight lead columns, then the item columns in file order, then the `q_` columns of M136. Tests read files with two and with three instruments, with and without `q_` columns. Further tests place a `q_` column and an `item_order` column between two groups and inside one group. Tests choose the columns of each instrument by stem and score them with its scoring function.
- [x] AC2: The `form_build` cell of a multi-instrument row holds one date per stem, in the order of the `instrument` cell, joined by single spaces. Each date parses as the single date does today. The result's `form_build` is character for every file, holding each row's dates as written. (RB tripwire: irreversible-api) This changes the type of the column from `Date` for single-instrument files as well. T1 poses it beside the alternative of a `Date` column plus a new optional lead column. Tests read single-instrument and multi-instrument files and assert the column's type and values.
- [ ] AC3: Under shuffle, the `item_order` cell of a multi-instrument row holds one group per stem, in the order of the `instrument` cell, joined by ` | `. Each group is that instrument's item numbers in the file, each once, joined by single spaces. A blank cell reads as `NA`, as D-070(c) states. A single-instrument cell keeps the grammar of D-070. Tests read a valid cell and a blank cell. They also read a cell with a group missing, a group in the wrong order, and a number from another instrument. Further cells hold a group that repeats or omits one of its own numbers, and the separators `1|2` and `||`.
- [x] AC4: The reader refuses each of these faults, as an unclassed `cli_abort()` that names the file, and the row where the fault is in a row. The faults are an `instrument` cell that differs from the stems of the groups, and a stem whose columns are split by another stem's columns. The third fault is a `form_build` cell whose date count differs from the stem count, or that holds a date that does not parse. The fourth fault is an `item_order` cell that breaks AC3. Tests assert the message of each refusal.
- [x] AC5: A script runs on main and on the branch. It reads every file that the existing tests in `tests/testthat/test-read_form_responses.R` read. For each result, `form_build` on the branch equals `format()` of main's column, and every other column is `identical()`. For each refusal, the condition class and message match. The exceptions are the refusals this milestone replaces, which the work log lists by test name with the branch's message.
- [x] AC6: `?read_form_responses`, the online-collection article and the modules-hitopsr article state the multi-instrument file shape and the `form_build` type. The help page and the online-collection article also state the `item_order` groups and how to choose one instrument's columns for scoring. A grep for `form_build` over `vignettes/` and `R/` finds no statement that it is a date. NEWS flags the `form_build` type change. `devtools::check()` gives 0 errors, 0 warnings and 0 notes, and `cairn_validate` exits 0.

## Coverage

- AC1 → T2, T3
- AC2 → T1, T2, T3
- AC3 → T2, T3
- AC4 → T2, T3
- AC5 → T3
- AC6 → T4, T5

## Tasks

- [x] T1: At the pre-implementation gate, settle the `form_build` type of AC2 and the ` | ` separator of AC3 with Jeff, and record D-080 on the file shape. It annotates D-064, D-070(c) and D-079(c). Under the alternative, the item columns start tenth, which D-071 names as a breaking move, and the gate says so. If Jeff chooses the alternative, amend AC2 and AC5 here and AC3 and AC4 of M140 through the gate.
- [x] T2: In `R/read_form_responses.R`, replace the one-stem rule with the grouping of AC1. Add the `instrument`, `form_build` and `item_order` rules of AC2 and AC3, and the refusals of AC4.
- [x] T3: Write the tests of AC1 to AC4 with fixtures written by rule in `tests/testthat/fixtures/`. Plant each refusal and show it red before trusting its green. Write the comparison script of AC5 and record its result in the work log.
- [x] T4: Update the roxygen help and run `devtools::document()`. Update the two articles and write the NEWS entry.
- [x] T5: Run `devtools::check()` and `cairn_validate`.

## Work log

- 2026-09-28: created by /milestone-plan. Jeff asked at the plan gate to plan several instruments in one link now.
- 2026-09-28: criteria audit ran in full mode (user-facing tier) on a fresh [O] reader. It returned 7 findings, all fixed before the commit: the AC5 exceptions, contiguity beside `q_` columns, date parsing, the `item_order` probes, the articles that state the type, the planting step moved to T3, and the Out wording.
- 2026-09-28: plan proposes a character `form_build` over a `Date` column plus a new lead column, because one column keeps the item columns starting ninth. The choice stays open for the pre-implementation gate (T1). Falsified by callers who compare `form_build` as a date.
- 2026-09-28: implement started on branch m139-form-reader-multi-instrument. At the gate Jeff chose a character `form_build` for every file, with no deprecation step because the reader is not in 0.2.0, and ` | ` between `item_order` groups. T1 done: D-080 recorded. AC2, AC5 and M140 stand unamended.
- 2026-09-28: T2 done. The reader groups item columns by stem, refuses a split stem, checks `instrument` against the stems in file order, reads `form_build` as one date per stem and returns it as written, and reads `item_order` as groups joined by ` | `. The one-stem refusal is gone.
- 2026-09-28: T3 done. Five fixtures written by `data-raw/form_multi_fixtures.R`, with a fixtures README row. New tests cover AC1 to AC4. Two old tests were rewritten for the replaced refusal, and two now assert a character `form_build`. Full suite: 0 failed, 15 skipped (merge-base and pkgload skips), 24028 expectations.
- 2026-09-28: T3 planting (scratch script, not committed): each of six mutations, which turn off the split, `instrument`, `form_build` parse, `form_build` count, `item_order` and group-count checks, turned 1, 15, 8, 3, 11 and 1 reader tests red.
- 2026-09-28: T3 AC5 run: `Rscript data-raw/compare_form_reader.R main` ran 226 base tests, captured 270 inputs (73 results, 197 refusals on main), and found every other input the same. It exits 1 on two plants: base ref 448db9d0, and an empty `replaced` list.
- 2026-09-28: AC5 exceptions, both with the branch message "holds an instrument cell that differs from the item columns' stems in file order." and the line `Response row 1: instrument "hitopbr", item columns "hitopbr pid5bf".`: test "item columns of two stems are refused naming the stems", and test "a two-stem file with an item_order of 1 1 is refused for the stems, not the cell".
- 2026-09-28: T4 done. The help page gains a multi-instrument paragraph, the `form_build` type and the `item_order` groups. The online-collection article gains the section "Several instruments in one file", with a chunk that scores each instrument by stem, and its two-form sheet paragraph now names the `instrument` cell as the fault. The modules article states the type and the shape. NEWS gains one entry and its first reader entry now says character. Both articles render against an installed branch. `hitop-form` at d5d91ac has no `instruments` field, so the docs say the page does not write the shape yet.
- 2026-09-28: T5 done. `devtools::check()`: 0 errors, 0 warnings, 0 notes. `cairn_validate` exits 0, with 24 older advisory warnings. Reader tests after the audit fixes: 0 failed, 1832 expectations.
- 2026-09-28: claim audit: 52 claims read, 4 corrected — R/read_form_responses.R (two code comments that said the page writes several stems), tests/testthat/test-read_form_responses.R (section comment), data-raw/compare_form_reader.R (what the wrapper copies). On the one re-read, two held and two took a further wording fix.
- 2026-09-28: status set to review.

## Decisions

## Review

Evidence gathered 2026-09-28 on m139-form-reader-multi-instrument at cd8ce6c7; main had not moved since the cut.

- AC1: `devtools::test(filter = "read_form_responses")`: 257 tests, 0 failed. The 13 AC1 tests cover the four fixtures with two and three instruments, with and without `q_` columns (names, `instrument` cell, item values against the restated rule, and scores per stem against the tables), the three placements of a `q_` and an `item_order` column (between the groups, inside the first, inside the second), the shuffled fixture, and the mismatch class across files. All pass.
- AC2: same run. The 7 AC2 tests pass. They assert that `form_build` is character with the value as written for the single-instrument fixture (`"2026-09-20"`) and for a two-row multi file (`"2026-09-20 2026-09-18"`, `"2026-09-21 2026-09-18"`), and that a date count other than the stem count and a date that does not parse (`2026-02-30`, doubled space, trailing space) are refused. The gate kept the character type (D-080(b)).
- AC3: same run. The 13 AC3 tests pass. They cover a valid cell (`2 1 | 3 1 2`), a blank cell read as `NA`, a missing group, groups in the wrong order, a number from another instrument, a group that repeats or omits its own number, the cells `1 2|1 2 3` and `1 2 || 1 2 3`, a trailing bar, and a single-instrument cell under D-070's grammar. Not ticked: the criterion names the separator `1|2`, and no test cell holds that string. The unspaced-bar cell holds `2|1`.
- AC4: same run. The 21 AC4 tests pass. They assert the class (`rlang_error`, neither package class), the file name, the headline and the bullets of each refusal. The cases are an `instrument` cell with the stems in the wrong order, one stem of two, a doubled space or a trailing space (row 2 named), a split stem with two and three stems, a `form_build` date count of one or three for two stems and an unparsed date (row 2 named), and each `item_order` fault (row 2 named). Planting at implement turned each refusal's tests red.
- AC5: `Rscript data-raw/compare_form_reader.R main` exits 0. It ran 226 base tests, captured 270 inputs (73 results, 197 refusals on main), and found every other input the same. The two differing inputs are the replaced refusals the work log lists by test name, each with the branch message it prints.
- AC6: `devtools::check()` gives 0 errors, 0 warnings and 0 notes (6m 54s), and `cairn_validate` exits 0. `?read_form_responses` states the multi-instrument shape, the character `form_build`, the `item_order` groups and the choice by stem (`grep("^pid5bf_", ...)`). The online-collection article has the section "Several instruments in one file", with a scoring chunk by stem. The modules article states the type and the shape. A grep for `form_build` over `vignettes/` and `R/` finds no statement that it is a date. NEWS flags the type change in bold.
- Consistency gate: `cairn_validate` exits 0 (24 older advisory warnings), `devtools::document()` gives no diff, `pkgdown::check_pkgdown()` finds no problems, README is untouched, and no top-level file is new. NEWS has an entry and no milestone or D-entry ids in the user-facing lines. No DESIGN principle changed, so `cairn_impact` was skipped.
- Independent review: three fresh reviewers ran ([O] diff-bug, [S] blame-history, [S] prior-review). None found a criterion failing. Their findings, merged where two or more reviewers raised the same point, are F1 to F12 below. Dispositions are recorded at the step-7 gate.
- F1 (blame): NEWS.md line 582, in an older entry of this development version, still says item columns of more than one stem are refused. Line 560 says "the item columns' stem" in the singular.
- F2 (diff-bug): swapped `item_order` groups go undetected when two stems have the same item-number set (`1 2 | 2 1` beside two two-item stems). Groups carry no label, so this is a limit of the D-080(c) grammar. It can arise only for a module whose item set matches another instrument's.
- F3 (diff-bug): the new `form_build` count refusal lists every row, with no cap of five. The sibling parse and `item_order` refusals are uncapped on main as well.
- F4 (diff-bug, blame): for a file with no item column, the count message says "one per stem" while the file has 0 stems.
- F5 (diff-bug): parse faults are reported before count faults, so a count fault on another row shows only after the parse fault is fixed.
- F6 (diff-bug): an item number that overflows in a column name, and two columns for one item, behave as on main.
- F7 (diff-bug, blame, prior-review): one roxygen line near `R/read_form_responses.R:113` is not rewrapped.
- F8 (diff-bug): the article's example cell `2 1 3 | 12 4 25 1` cannot be a valid cell, because each group must hold all of its instrument's numbers.
- F9 (diff-bug, blame, prior-review): the rewritten test "a two-stem file with an item_order of 1 1 ..." asserts only the word "instrument", which other messages also hold.
- F10 (blame): the article's two-form sheet paragraph gives no pointer to the new section.
- F11 (prior-review): the NEWS phrase "reads as before in every other column" was checked and is accurate. It needs no action.
- F12 (review step 3): AC3 names the separator `1|2`, and no test cell holds that string. AC3 stays unticked until a test holds it.
