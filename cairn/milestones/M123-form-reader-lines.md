# M123: `read_form_responses()` refuses a whitespace-only line, a byte that is not UTF-8, and an `instrument` cell that differs from the item stem, and names each cell in its value refusals

- **Status:** planned
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** GP3
- **Resolves:** —
- **Surface tier:** user-facing — the deliverable is an exported function's refusals and their documentation
- **Branch/PR:** —

## Goal

A hand-edited or mis-encoded hitop-form file stops at `read_form_responses()` with a message that names the file and the line, row or cell at fault, instead of a base R error, a wrong row count, or a silent read.

## Scope

**In:** four changes to `read_form_response_file()` in `R/read_form_responses.R`. A line outside a quoted cell made only of spaces and tabs is refused. A line holding a NUL byte or a byte sequence that is not UTF-8 is refused. An `instrument` cell that differs from the item-column stem is refused. The whole-number and integer-range refusals name each cell. Their tests, two fixtures copied from hitop-form, `?read_form_responses` Details, NEWS.

**Out:** the hitop-form page and its README (the page writes none of these files). A condition class for any refusal (D-064: unclassed until a caller needs to catch one, then a D-entry). Invalid UTF-8 or mixed invisible characters inside an item value reaching the scoring functions (the M110 candidate row). Checking the stem against a list of known instruments (the cell is the file's own claim, and no such list is a package contract). A file with bare-CR line endings, which the line split reads as one line (candidate row if reported). A byte-order mark after the first line, which passes as valid UTF-8 and reaches the later checks.

## Acceptance criteria

- [ ] AC1: `read_form_responses()` stops on a file holding a line, outside a quoted cell, made only of spaces and tabs, with an error naming the file and each such line by its number counted from the file's first line, raised after the byte check and before the field-count check. Tests in `tests/testthat/test-read_form_responses.R` cover a file of one spaces-only line, a tab-only line, a mixed spaces-and-tabs line, a file of three such lines naming all three, a CRLF file whose line is spaces then `\r\n`, a byte-order mark followed by such lines, two such lines before a valid header and response row (naming lines 1 and 2, not a field count), such a line after the last response row, and such lines before a header and a short row (this refusal, not the field-count one). Controls: a quoted `study` cell holding a line break, a spaces-only line and a further line break reads as one row with no condition, and a file with an empty line before the header still reads as one row with no condition.
- [ ] AC2: `read_form_responses()` stops on a file holding a NUL byte or a byte sequence that `validUTF8()` rejects, with an error naming the file and each such line by its number counted from the file's first line, raised before every other check, and with no warning. Tests cover a Latin-1 byte in a lead cell, in an item cell, in the header line, and on the second of two response rows (naming line 3 only), a lone continuation byte (`0x80`), a multibyte sequence cut off at the end of the file, and a UTF-16LE file, each asserting that no warning was raised. Control: a `participant` cell holding a UTF-8 multibyte character reads with no condition and the character intact.
- [ ] AC3: the whole-number and integer-range refusals name each offending cell on its own line as the response row, the column and the value as written, in the file's row then column order, the first five cells, and, when more than five, one line counting the rest. Tests cover, for each of the two refusals, one cell, two cells on different rows and columns, five cells (no count line), six cells ("1 more") and seven cells ("2 more"), and, for the whole-number refusal, a value holding braces and a value of spaces only, each shown as written.
- [ ] AC4: `read_form_responses()` stops on a file with item columns whose `instrument` cell on any response row differs from the item-column stem, with an error naming the file, each such row counted from the first row after the header, the cell's value and the stem, raised after the stem check and before the value checks. Tests cover one row differing, one of three rows differing (naming that row only), a blank cell, a cell with surrounding spaces, and a differing cell beside a non-whole item value (this refusal, not the value one). Controls: a file with no item columns reads with no condition, and every file `list.files("tests/testthat/fixtures", pattern = "[.]csv$")` returns, the copied module and full PID-5 files among them, reads with no condition.
- [ ] AC5: `?read_form_responses` Details lists the three new faults in its sentence of file faults, says that a line number is counted from the file's first line and a response row from the first row after the header, and says that a value error names the cells at fault, and NEWS.md carries one entry for the four changes under the development version.
- [ ] AC6: `devtools::test()` reports 0 failures, `devtools::check()` reports 0 errors, 0 warnings and 0 notes, and `devtools::document()` leaves no diff.

## Coverage

- AC1 → T2
- AC2 → T1
- AC3 → T3
- AC4 → T4
- AC5 → T5
- AC6 → T6

## Tasks

- [ ] T1: Tests first for AC2, then a helper `form_file_lines()` that reads the file's bytes with `readBin()`, refuses any NUL byte by line, converts with `rawToChar()`, splits on `\n`, drops a leading byte-order mark and a trailing `\r`, and refuses the lines `validUTF8()` rejects. `read_form_response_file()` calls it before `count_form_fields()` (`R/read_form_responses.R:242`). Message: "{file} holds a line that is not UTF-8.", one "x" line per line number, and "i" "The line is counted from the file's first line." Assert no warning through a `withCallingHandlers()` recorder, not `expect_no_warning()` (LESSONS M032).
- [ ] T2: Tests first for AC1, then the whitespace refusal over T1's lines: a line matching `^[ \t]+$` whose count from `count.fields(..., blank.lines.skip = FALSE)` is not `NA` (a line inside a quoted cell is `NA`, an empty line 0, so counts align with lines). Refused after T1's check and before the field-count check, same message shape and line convention.
- [ ] T3: Tests first for AC3, then rewrite the two refusals at `R/read_form_responses.R:353-382`: one "x" line per cell, "Response row {r}, column {col}: {.val {value}}", ordered by row then column, `head(, 5L)`, then "... and {n} more cell{?s}." when more than five. Read the lines through `cnd$body` (LESSONS M096).
- [ ] T4: Copy `responses-module-shuffled.csv` and `responses-pid5.csv` from `hitop-form/tests/fixtures/` into `tests/testthat/fixtures/` and list them in the fixtures README. Tests first for AC4, then the instrument check after the stem check (`R/read_form_responses.R:348`), guarded on `length(stems) == 1L`: rows where `raw$instrument != stems` (a blank or padded cell differs), one "x" line per row "Response row {r}: instrument {.val {value}}, item columns {.val {stem}}.", and the row-counting note.
- [ ] T5: `?read_form_responses` Details: extend the file-fault sentence with the three faults, add the line-counting sentence beside the row-counting one, change "names the response rows at fault" to cover cells, then `devtools::document()`. NEWS entry under the development version's "Improvements and fixes".
- [ ] T6: Run `devtools::test()`, `devtools::check()` and `devtools::document()`, and fix what they report.

## Work log

- 2026-09-24: created by /milestone-plan. All seven gaps of the M121 candidate row reproduced by a scratch probe before drafting.
- 2026-09-24: criteria audit ran in full mode (fresh Opus reader). Findings, all fixed before the gate: the whitespace scan needs `blank.lines.skip = FALSE` so counts align with lines; the quoted-cell control must hold a spaces-only line; NUL bytes must be refused before `rawToChar()`; probes added for tab, mixed and CRLF lines, a continuation byte, a truncated sequence, a UTF-16 file, the five and six cell boundaries, values with braces or spaces, and a padded instrument cell; the no-warning promise dropped its recorder wording; AC5 must revise the Details sentence that says value errors name rows; the stem comparison is guarded on one stem; two hitop-form fixtures are copied in.
- 2026-09-24: plan gate chose refusing a whitespace-only line by line number over skipping it as blank because a hand edit is shown rather than guessed around; falsified by a store export that writes such lines in normal use.
- 2026-09-24: plan gate chose five named cells plus a count over every cell uncapped and over the two lists plus one cell because it matches D-068's naming and keeps a large export readable, and M121's review rejected the uncapped form; falsified by a user needing the sixth cell and beyond named.
- 2026-09-24: plan gate chose refusing an instrument cell that differs from the stem over a warning and over leaving it unchecked because the column would otherwise disagree with the items beside it; falsified by a page or store writing a cell other than the stem in normal use.
- 2026-09-24: plan gate chose file line numbers from line 1 for the two line-level refusals over response-row numbers because those refusals can fire before a header is found; falsified by a reader confusing the two counts in a report.

## Decisions

## Review
