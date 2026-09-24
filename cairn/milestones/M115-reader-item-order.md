# M115: `read_form_responses()` accepts an `item_order` column recording the order a participant saw

- **Status:** in-progress
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

- [ ] AC1: `read_form_responses()` reads a file holding an `item_order` column anywhere after `submitted` into a result whose `item_order` column is character and sits sixth, after `submitted` and before the item columns. A file without it reads as before. In one call that mixes files with and without it, the rows from files without it hold `NA`. `item_order` is not an item column for the `hitop_form_responses_mismatch` comparison, and both public classes keep their triggers. Tests: a file the test writes with the column sixth, one with it last (as a Google Sheet appends it), one without, and a directory holding one with and one without.
- [ ] AC2: A non-blank `item_order` cell matches `^[1-9][0-9]*( [1-9][0-9]*)*$` and lists every item number parsed from the file's item column names (`hitopbr_01` is 1), each once. A blank cell reads as `NA`. Any other cell stops the read with an unclassed error naming the file and the data row, counted from 1 with the header not counted. Tests: one cell each for a missing number, a repeated number, a number outside the file's items, a letter, a leading zero, a double space, a leading space and a trailing space, plus the blank cell.
- [ ] AC3: `?read_form_responses` documents the column as the order the participant saw, as item numbers, that it sits sixth, that files may lack it, and that scoring does not read it. The online-collection article says the same in one sentence. NEWS has an entry under "New features". `devtools::test()` is clean with the Imports installed, `devtools::document()` makes no diff, `pkgdown::check_pkgdown()` is clean, and `devtools::check()` has 0 errors and 0 warnings.

## Coverage

- AC1 → T1
- AC2 → T1
- AC3 → T2

## Tasks

- [x] T1: The lead-column check at `R/read_form_responses.R:187` learns the optional column, the cell check of AC2 runs beside the item-value checks, and the AC1 and AC2 tests land in `test-read_form_responses.R`.
- [ ] T2: Roxygen (the `@details` paragraph at `R/read_form_responses.R:14`), the article sentence, the NEWS entry, `document()`, `test()`, `check_pkgdown()`, `check()`.

## Work log

- 2026-09-23: created by /milestone-plan, with M114 and M116. Depends on M113 because both edit `read_form_responses.R`.
- 2026-09-23: criteria audit ran in full mode on a fresh [O] reader (shared with M114 and M116): six findings on this file, all fixed in the wording. The column is read wherever it sits after `submitted`. The cell grammar is stated and probed on each format axis. The refusal is unclassed and names the data row. The mismatch check leaves `item_order` out. The test command names the Imports.
- 2026-09-23: plan gate chose recording the shown order in a column, reader first, over recording nothing, because order-effect analyses need it and a file the reader refuses is worse than none; falsified by no researcher ever reading the column.
- 2026-09-24: T1 done. The question gate was skipped because the plan fixes the column position, the cell grammar and the refusal shape. `read_form_response_file()` takes `item_order` out of the item columns and checks each cell against the grammar and the file's item numbers. If the column is absent, the sixth column is NA. Nine tests cover the two positions, the absent column, the mixed directory, the multi-row file, the eight bad cells, the blank cell and the row number. One position read in `test-layout.R` moved from 5 to 6 columns.

## Decisions

## Review
