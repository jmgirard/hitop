# M115: `read_form_responses()` accepts an `item_order` column recording the order a participant saw

- **Status:** review
- **Priority:** normal
- **Depends on:** M113
- **Driving RR:** —
- **Principles touched:** GP3
- **Resolves:** —
- **Surface tier:** user-facing — an exported reader's behavior
- **Branch/PR:** `m115-reader-item-order`

## Goal

Let the reader accept an optional lead column, `item_order`, that the page will write under a random display order (M116), so that such a file reads and scores as one without it does.

## Scope

**In:** the column read, checked and placed sixth in `read_form_responses()`, under D-070's reader clauses. Its help page, one sentence in the online-collection article, and NEWS.

**Out:** the page writing the column → M116. A new condition class → none (D-064 stands, and a bad cell is an unclassed refusal). Reordering item columns by name across files → not planned (the mismatch check is unchanged).

## Acceptance criteria

- [x] AC1: `read_form_responses()` reads a file holding an `item_order` column anywhere after `submitted` into a result whose `item_order` column is character and sits sixth, after `submitted` and before the item columns. A file without it reads as before. In one call that mixes files with and without it, the rows from files without it hold `NA`. `item_order` is not an item column for the `hitop_form_responses_mismatch` comparison, and both public classes keep their triggers. Tests: a file the test writes with the column sixth, one with it last (as a Google Sheet appends it), one without, and a directory holding one with and one without.
- [x] AC2: A non-blank `item_order` cell matches `^[1-9][0-9]*( [1-9][0-9]*)*$` and lists every item number parsed from the file's item column names (`hitopbr_01` is 1), each once. A blank cell reads as `NA`. Any other cell stops the read with an unclassed error naming the file and the data row, counted from 1 with the header not counted. Tests: one cell each for a missing number, a repeated number, a number outside the file's items, a letter, a leading zero, a double space, a leading space and a trailing space, plus the blank cell.
- [x] AC3: `?read_form_responses` documents the column as the order the participant saw, as item numbers, that it sits sixth, that files may lack it, and that scoring does not read it. The online-collection article says the same in one sentence. NEWS has an entry under "New features". `devtools::test()` is clean with the Imports installed, `devtools::document()` makes no diff, `pkgdown::check_pkgdown()` is clean, and `devtools::check()` has 0 errors and 0 warnings.

## Coverage

- AC1 → T1
- AC2 → T1
- AC3 → T2

## Tasks

- [x] T1: The lead-column check at `R/read_form_responses.R:187` learns the optional column, the cell check of AC2 runs beside the item-value checks, and the AC1 and AC2 tests land in `test-read_form_responses.R`.
- [x] T2: Roxygen (the `@details` paragraph at `R/read_form_responses.R:14`), the article sentence, the NEWS entry, `document()`, `test()`, `check_pkgdown()`, `check()`.

## Work log

- 2026-09-23: created by /milestone-plan, with M114 and M116. Depends on M113 because both edit `read_form_responses.R`.
- 2026-09-23: criteria audit ran in full mode on a fresh [O] reader (shared with M114 and M116): six findings on this file, all fixed in the wording. The column is read wherever it sits after `submitted`. The cell grammar is stated and probed on each format axis. The refusal is unclassed and names the data row. The mismatch check leaves `item_order` out. The test command names the Imports.
- 2026-09-23: plan gate chose recording the shown order in a column, reader first, over recording nothing, because order-effect analyses need it and a file the reader refuses is worse than none; falsified by no researcher ever reading the column.
- 2026-09-24: T1 done. The question gate was skipped because the plan fixes the column position, the cell grammar and the refusal shape. `read_form_response_file()` takes `item_order` out of the item columns and checks each cell against the grammar and the file's item numbers. If the column is absent, the sixth column is NA. Nine tests cover the two positions, the absent column, the mixed directory, the multi-row file, the eight bad cells, the blank cell and the row number. One position read in `test-layout.R` moved from 5 to 6 columns.
- 2026-09-24: T2 done. The help page's details and return sections, one sentence in the online-collection article, and a NEWS entry under "New features" describe the column. `document()` makes no diff, `check_pkgdown()` is clean, `test()` is clean, and `check()` gives 0 errors, 0 warnings and 0 notes. The two scoring vignettes still say a file has five lead columns, which stays true until the page writes the sixth in M116.
- 2026-09-24: claim audit: 22 claims read, 0 corrected — R/read_form_responses.R, NEWS.md, vignettes/articles/online-collection.Rmd, tests/testthat/test-read_form_responses.R. The reader noted that the help page did not say a leading zero or a stray space is refused, and one phrase was added after the audit.
- 2026-09-24: status set to review.
- 2026-09-24: review ran. Three criteria verified with fresh evidence. Eight findings from the diff-bug lens: three fixed on the branch (the modules article's item selection, the refusal's plural, one help-page phrase), three deferred (two to the malformed-download candidate row, one to M116), two rejected with reasons. That candidate row's promotion condition, "when the reader is next edited", has now fired.
- 2026-09-24: step-7 approval: m115-reader-item-order approved for merge.

## Decisions

## Review

- 2026-09-24 AC1: a probe script wrote a file with `item_order` sixth, one with it last, and one without, then read each and the directory holding all three. Every result names the six lead columns then the two item columns, `item_order` is character, the file with it last reads it sixth, the file without reads `NA`, and the directory read gives three rows with `2 1`, `1 2`, `NA`. Adding a file whose item columns differ raised `hitop_form_responses_mismatch`, and an empty directory raised `hitop_form_responses_none`. `devtools::test()` is clean, the five new AC1 tests included. Verified.
- 2026-09-24 AC2: the probe wrote two-row files whose second row held each of the eight bad cells (`1`, `1 1`, `1 3`, `1 a`, `01 2`, `1  2`, ` 1 2`, `1 2 `). Each was refused by an unclassed error whose message names the file and "row 2", and the blank cell read as `NA`. The three AC2 tests pass under `devtools::test()`. Verified.
- 2026-09-24 AC3: `man/read_form_responses.Rd` says the column is the order the participant saw, as item numbers, that the result places it sixth, that a file may lack it, and that scoring does not read it. The online-collection article says the same in one sentence at its line 104. NEWS carries the entry under "New features" with no milestone number. After the fix-now commits: `devtools::test()` has 0 failures, `devtools::document()` makes no diff, `pkgdown::check_pkgdown()` finds no problems, and `devtools::check()` gives 0 errors, 0 warnings, 0 notes. Verified.
- 2026-09-24 gate: `cairn_validate.py` passes with 24 advisory warnings, none from this milestone. No principle changed. README.md is newer than README.Rmd and the branch touches neither. Every check the profile's consistency-gate slot names is recorded under AC3.
- 2026-09-24 review fan-out: [O] diff-bug lens 8 findings, [S] blame-history lens none, [S] prior-review lens none (no archived finding on these files, and the PR-comment probe returned none).
- F1 (fix now): `vignettes/articles/modules-hitopsr.Rmd` built its item columns by removing the five old lead names, so `item_order` entered the item set and `score_hitopsr()` stopped with "Expected 21 items but got 22". `check()` passed because articles are not built there. Fixed by selecting `^hitopsr_` names, and the three vignette code paths that read a file were run from their purled code.
- F2 (fix now): the refusal printed "Response row 1 and 2" with no plural. Fixed with an explicit quantity, and the row test now also asserts "rows 2 and 3" for a two-bad-row file. The first form, `cli::qty(rows)`, failed on a vector, and the test caught it.
- F3 (follow-up): a column after `submitted` whose name has no numeric suffix gives an NA item number, so every non-blank `item_order` cell is refused with a message that blames the cell. Absorbed into the candidate row on malformed-download reader gaps.
- F4 (follow-up): item numbers are parsed without the stem, so a file mixing two instruments' columns accepts `1 1`. No page or store writes such a file. Absorbed into the same candidate row.
- F5 (reject): the row count differs from the physical line when a file holds blank lines, which `read.csv` drops. AC2 defines the row as the data row, and the page and stores write no blank lines.
- F6 (fix now): the details paragraph named the six columns as the file's where they are the result's. Reworded.
- F7 (reject): the mismatch test would not notice the comparison including `item_order`, because the column sits sixth in every reshaped part. That state gives the same result for every file, so the contract holds either way, and the test asserts the contract.
- F8 (follow-up): `vignettes/pid5_scoring.Rmd` and the modules article still say a file has five lead columns. True until M116 makes the page write the sixth. A work-log line on M116 names the two sentences.
