# M136: read_form_responses() reads the researcher's own answer columns, named with a q_ prefix, and returns them after the item columns

- **Status:** review
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

- [x] AC1: A file that holds one or more columns whose names start with `q_`, anywhere after `submitted`, reads. The result has the eight lead columns, then the item columns, then the `q_` columns. The `q_` columns come in the order of first appearance. The reader takes the files in path order and each file from left to right. Each `q_` column is character, and a blank cell is `NA`. Tests read `q_` columns placed after the item columns, before them, between two item columns, and between two optional lead columns. A further test reads cells that hold `=1+1`, `007`, a comma and a line break, and asserts each value as written.
- [x] AC2: Files whose sets of `q_` columns differ read together. A row from a file without a given `q_` column has `NA` in it. The comparison takes the item columns of each file with the `q_` columns removed. When these differ in name, count or order, `hitop_form_responses_mismatch` fires, as D-064 states. Tests read two files with disjoint `q_` sets, and two files that hold the same `q_` columns in different orders, with no mismatch. A test also reads two files whose item columns differ and whose `q_` columns match, and asserts the class `hitop_form_responses_mismatch`.
- [x] AC3: The reader refuses a column name that starts with `q_` and does not match `^q_[a-z][a-z0-9_]{0,29}$`. The refusal is an unclassed `cli_abort()` that names the file and the column, on the terms of D-064 for unclassed refusals. A file in which one `q_` name occurs twice meets the existing refusal of a repeated column. Tests assert the message of both refusals for a `q_` name.
- [x] AC4: A script runs on main and on the branch. It reads every file that the existing tests in `tests/testthat/test-read_form_responses.R` read. It compares each result with `identical()`, or for a refusal it compares the condition class and message. Every pair matches.
- [x] AC5: `?read_form_responses` states where the `q_` columns sit in the result, their type, the name pattern, and the result for files with different sets. The article states the same in one paragraph and shows `as.integer()` on a number answer. NEWS names the change. `devtools::check()` gives 0 errors, 0 warnings and 0 notes, and `cairn_validate` exits 0.

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
- 2026-09-28: claim audit: 23 claims read, 1 corrected — R/read_form_responses.R, man/read_form_responses.Rd, vignettes/articles/online-collection.Rmd, tests/testthat/test-read_form_responses.R. The corrected claim said a `q_` column answers a question added to the study link, which the page cannot do before M137. It now says a question of the researcher's own, and the reader's re-read found it holds.
- 2026-09-28: all tasks done, and status set to review.

## Decisions

## Review

- 2026-09-28 evidence AC1: `test_file()` on test-read_form_responses.R ran 225 tests with 1613 expectations, 0 failed, 0 errors. The `q_` tests passing include columns after the items, before them, between `hitopbr_01` and `hitopbr_02`, and between `item_order` and `prolific_study`. They also cover a blank cell as `NA`, order of first appearance across files, and `=1+1`, `007`, `a, b` and `first\nsecond` read as written.
- 2026-09-28 evidence AC2: the same run passes the disjoint-set test (NA fill, no condition) and the reordered-set test (values aligned by name). It also passes the test with differing item columns and matching `q_` columns, which asserts class `hitop_form_responses_mismatch`.
- 2026-09-28 evidence AC3: the same run passes the pattern test. It asserts that the message names the file and the column for `q_Age`, `q_1a`, `q_`, `q__a`, `q_a-b` and a 31-character tail. The 30-character tail reads. The repeated `q_age` test asserts "more than once" with the file and column.
- 2026-09-28 evidence AC4: scratchpad `ac4/compare.R` reran main's test file (213 tests, 0 failed, 0 errors) and captured each call's inputs. All 249 calls match between main's reader and the branch's. 60 match by `identical()` result and 189 by class and message.
- 2026-09-28 evidence AC5: `man/read_form_responses.Rd` states the answer columns' place after the items, character type, the pattern, and `NA` fill across files. The article holds one paragraph and a chunk with `as.integer(with_answers$q_age)`, which printed `34 NA` in a branch render. NEWS has the entry. `devtools::check()` gave 0 errors, 0 warnings, 0 notes. `cairn_validate` exits 0.
- 2026-09-28 gate: `devtools::document()` gives no diff. `pkgdown::check_pkgdown()` finds no problems. `cairn_validate` exits 0. No DESIGN principle changed, so `cairn_impact` is skipped. README and `.Rbuildignore` are untouched.
- 2026-09-28 reviewers: prior-review lens found no regression of an archived review point and no PR inline comments. The blame-history lens found no conflict with D-064, D-070, D-071 or D-079.
- 2026-09-28 diff-bug lens findings, ranked. No criterion fails, so none is a floor return. Proposed dispositions go to the approval gate.
  - F1: a file with stem-`q` item columns (`q_1`, `q_2`) reads on main and is refused on the branch, so the NEWS sentence "A file without answer columns reads as before" is not exact. Confirmed by probe. No instrument uses stem `q`. Proposed: fix now, reword NEWS to name files with no column starting `q_`.
  - F2: the reorder `p[c(...)]` in the binding step changes no result, because `rbind` takes the first part's order and the fill appends in `answers` order. Proposed: reject, it states the order and does not rely on `rbind`'s first-part rule.
  - F3: the mismatch-class test would pass with `q_` columns left in the comparison. Proposed: reject, the disjoint and reordered tests cover the exclusion.
  - F4: `Q_age` or a leading-space name meets the item-name refusal, and a trailing space is invisible in the message. Proposed: reject, correct by the pattern and the same display as main's item-name refusal.
  - F5: "a blank cell is `NA`" leaves a cell of spaces as written. Proposed: reject, the optional lead columns use the same word and rule.
  - F6: a file of `q_` columns and no item columns reads, with its `instrument` cell unchecked. Proposed: reject, a file with no item columns already reads on main.
  - F7: the comment for the item-column conversion now sits above the answer-column split. Confirmed. Proposed: fix now, move it back above the item code.
