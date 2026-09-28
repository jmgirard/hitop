# M136: read_form_responses() reads the researcher's own answer columns, named with a q_ prefix, and returns them after the item columns

- **Status:** in-progress
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** GP2, GP3
- **Resolves:** —
- **Surface tier:** user-facing — an exported reader's column contract, its help page and the article
- **Branch/PR:** m136-form-reader-custom-columns

## Goal

`read_form_responses()` accepts columns named `q_<name>` anywhere after `submitted`, returns them as text after the item columns, and leaves them out of the item-column comparison.

## Scope

**In:** In hitop, the reader in `R/read_form_responses.R`, its tests, its help page, a paragraph in the online-collection article, and NEWS. D-079 governs. The plan of M135 records it. This milestone ships before M137, so the reader accepts the columns before any page writes them, as M115 shipped before M116.

**Out:** The page that asks the questions and writes the columns goes to M137. Type conversion of an answer column, such as a number question read as an integer, stays with the researcher, and the article shows `as.integer()`. The new refusals carry no condition class. D-064 names the evidence that reopens that choice. Instrument content (IP1) is untouched.

## Acceptance criteria

- [ ] AC1: A file that holds one or more columns whose names start with `q_`, anywhere after `submitted`, reads. The result has the eight lead columns, then the item columns, then the `q_` columns. The `q_` columns come in the order of first appearance. The reader takes the files in path order and each file from left to right. Each `q_` column is character, and a blank cell is `NA`. Tests read `q_` columns placed after the item columns, before them, between two item columns, and between two optional lead columns. A further test reads cells that hold `=1+1`, `007`, a comma and a line break, and asserts each value as written.
- [ ] AC2: Files whose sets of `q_` columns differ read together. A row from a file without a given `q_` column has `NA` in it. The comparison takes the item columns of each file with the `q_` columns removed. When these differ in name, count or order, `hitop_form_responses_mismatch` fires, as D-064 states. Tests read two files with disjoint `q_` sets, and two files that hold the same `q_` columns in different orders, with no mismatch. A test also reads two files whose item columns differ and whose `q_` columns match, and asserts the class `hitop_form_responses_mismatch`.
- [ ] AC3: The reader refuses a column name that starts with `q_` and does not match `^q_[a-z][a-z0-9_]{0,29}$`. The refusal is an unclassed `cli_abort()` that names the file and the column, on the terms of D-064 for unclassed refusals. A file in which one `q_` name occurs twice meets the existing refusal of a repeated column. Tests assert the message of both refusals for a `q_` name.
- [ ] AC4: A script runs on main and on the branch. It reads every file that the existing tests in `tests/testthat/test-read_form_responses.R` read. It compares each result with `identical()`, or for a refusal it compares the condition class and message. Every pair matches.
- [ ] AC5: `?read_form_responses` states where the `q_` columns sit in the result, their type, the name pattern, and the result for files with different sets. The article states the same in one paragraph and shows `as.integer()` on a number answer. NEWS names the change. `devtools::check()` gives 0 errors, 0 warnings and 0 notes, and `cairn_validate` exits 0.

## Coverage

- AC1 → T1, T2
- AC2 → T1, T2
- AC3 → T1, T2
- AC4 → T2
- AC5 → T3, T4

## Tasks

- [x] T1: In `R/read_form_responses.R`, add the `q_` name pattern beside `item_order_pattern`. Split the columns after the lead columns into item columns and `q_` columns before the item-name check. Bind the `q_` columns across files by name, fill `NA`, and place them last. Add the two refusals of AC3.
- [x] T2: Write the tests of AC1 to AC3 in `tests/testthat/test-read_form_responses.R`. Plant the pattern refusal and show it red before trusting its green. Write the comparison script of AC4 in the scratchpad, on the main-against-branch pattern of D-011, and record its result in the work log.
- [x] T3: Update the roxygen help and run `devtools::document()`. Write the article paragraph and the NEWS entry.
- [x] T4: Run `devtools::check()` and `cairn_validate`.

## Work log

- 2026-09-28: created by /milestone-plan.
- 2026-09-28: criteria audit ran in full mode (user-facing tier) on a fresh [O] reader. It returned 4 findings, all fixed before the commit: the mismatch rule restated for files with `q_` columns, probes between item columns and for reordered `q_` sets, the duplicate refusal that exists on main, and a main-against-branch script for AC4.
- 2026-09-28: plan chose a `q_` prefix over bare question names, because a mistyped item column must still be refused and not read as an answer. Falsified by researchers who need their own column names in the file.
- 2026-09-28: plan chose answer columns after the item columns over columns among the lead columns, because the item columns keep starting ninth. Falsified by a caller who selects answer columns by position.
- 2026-09-28: implement started on branch m136-form-reader-custom-columns. Question gate skipped: the plan and D-079 leave no choice open.
- 2026-09-28: T1 done. The reader splits `q_` columns off before the item checks and refuses a `q_` name outside `answer_column_pattern`. It binds answer columns by name after the item columns.
- 2026-09-28: T2 done. 12 tests for AC1 to AC3 pass, and a planted `^q_` pattern turned the name-refusal test red. AC4 script: main's 213 reader tests made 249 calls, and all 249 match on main and the branch (60 results, 189 refusals). A planted blank-cell change gave 6 mismatches.
- 2026-09-28: T3 done. Help page, article paragraph with an `as.integer()` chunk, and NEWS entry. The article rendered against the branch code prints `"34" NA` and then `34 NA`.
- 2026-09-28: T4 done. `devtools::check()` gave 0 errors, 0 warnings and 0 notes. `cairn_validate` exits 0 with 24 advisory warnings.

## Decisions

## Review
