# M138: The hitop-form link builder reads the researcher's questions from a spreadsheet file and writes them back as one

- **Status:** review
- **Priority:** normal
- **Depends on:** M137
- **Driving RR:** —
- **Principles touched:** GP3
- **Resolves:** —
- **Surface tier:** user-facing — the link builder of the deployed form page and the package's article
- **Branch/PR:** m138-form-questions-file, companion: /Users/jmgirard/github/hitop-form m138-form-questions-file

## Goal

A researcher writes the questions of a study in a spreadsheet and saves it as a CSV file. The link builder loads that file and fills the question editor of M137 from it.

## Scope

**In:** In hitop-form link.html, the work covers these parts. A "Load questions from a file" control that reads a CSV file in the browser. A "Download these questions" control that writes the questions in the editor as a CSV file. A "Download a template" control that writes a CSV file with the header and one example question of each type. The file format, with its refusals. A README section and Playwright tests. In hitop, the work covers a paragraph in the questions section of the online-collection article and NEWS.

**Out:** Excel `.xlsx` files and other spreadsheet formats go to the question-features candidate row, since reading them needs a library the page does not load. An R function that writes the file goes to the same row. The page and the link format are unchanged, because the builder still writes the questions into the link.

## Acceptance criteria

- [x] AC1: The file is CSV as RFC 4180 describes it. It is UTF-8, with or without a byte-order mark, with CR LF or LF line ends. A quoted field can hold a comma, a double quote written twice, and a line break. The first row names the columns, in lower case and in any order. The columns `list`, `name`, `text` and `type` are required, and `options`, `required`, `min` and `max` are optional. Each further row is one question, and the questions keep the order of the file within each list. A row whose fields are all blank is skipped. `list` is `before` or `after`, and `type` is one of the four types of M137, both in lower case. `options` holds the option labels separated by `|`, each trimmed. `required` is `yes`, `no` or blank, in any letter case, and blank is `no`. `min` and `max` are blank or match `^-?[0-9]+$` after trimming. Each question then meets the rules of M137 AC1. A load replaces the questions in the editor. Tests load files that hold each of these forms and assert the questions the editor shows.
- [x] AC2: The builder refuses a file and names the fault. The faults are a file that is not UTF-8, an empty file, and a file with no question row. For a file that is not UTF-8, the message tells the researcher to save as "CSV UTF-8". They include a missing required column, an unknown or repeated column name, and a row whose field count differs from the header. They include an unclosed quote, a quote inside an unquoted field, and text after a closing quote. They also include a field value outside the rules of AC1 or of M137 AC1. The message names where the fault lies. A file that is not UTF-8, an empty file and a file with no question row name no row. A fault in the header row names row 1, and the field number of the field at fault or the missing column. Any other fault this criterion lists names its row, and the column of the field at fault, the column that has no field, or the field number of a field past the last column. Rows count as records, with the header as row 1. After a refusal, the editor keeps the questions it held. Tests assert the message of a file for each fault this criterion lists.
- [x] AC3: "Download these questions" writes a UTF-8 file with a byte-order mark and CR LF line ends. The file has the columns in the order of AC1 and one row per question. It writes `required` as `yes` or `no`. Loading that file fills the editor with the same questions, compared after the defaults of M137. "Download a template" writes a file of the same form with one example question of each type, and loading it gives four questions. Tests run both round trips. The questions cover each type, `required: true` and a negative `min`. A text and an option label hold a comma, a double quote and non-ASCII text.
- [x] AC4: Loading a file makes no network request. A test records the requests while it loads a file and asserts that none is made.
- [ ] AC5: The README section and the article paragraph each state the columns and the option separator `|`. They also state the "CSV UTF-8" save choice and that the file stays in the browser. NEWS names the controls. The hitop-form suite passes in its CI. In hitop, `devtools::check()` gives 0 errors, 0 warnings and 0 notes, and `cairn_validate` exits 0.

## Coverage

- AC1 → T1, T2
- AC2 → T1, T2, T6
- AC3 → T3, T6
- AC4 → T2
- AC5 → T4, T5

## Tasks

- [x] T1: In form.js, add `readQuestionsCsv(bytes)`, which decodes with `TextDecoder("utf-8", { fatal: true })`, parses the CSV and maps each row to a question. It passes the result through `checkQuestions()` from M137 and maps each fault to the row and column of the file. Add `writeQuestionsCsv(questions)`.
- [x] T2: In link.html, add the file control, which reads the file with `File.arrayBuffer()`. Fill the editor on success, and show the refusal otherwise. Write the load, refusal and network tests.
- [x] T3: Add the two download controls and the round-trip tests.
- [x] T4: Write the README section "Write your questions in a spreadsheet", the article paragraph and the NEWS entry.
- [x] T5: Run the full hitop-form suite locally, hitop `check()` and `cairn_validate`. The hitop-form PR and its CI come at the merge step of /milestone-review.
- [x] T6: After review pass 1: give one LF6 option label a comma, a double quote and non-ASCII text together. Add LF4 probes for a quote fault in the header row and in a field past the last column. Run the full hitop-form suite again.

## Work log

- 2026-09-28: created by /milestone-plan. Jeff asked at the plan gate for the questions to come from a spreadsheet file that researchers can make.
- 2026-09-28: criteria audit ran in full mode (user-facing tier) on a fresh [O] reader. It returned 3 findings, all fixed before the commit: the round-trip rules, the `required` value written, and the CSV grammar gaps.
- 2026-09-28: plan chose CSV over `.xlsx`, because the page reads CSV with no library. Falsified by researchers who cannot save a spreadsheet as "CSV UTF-8".
- 2026-09-28: implement started. Branch m138-form-questions-file cut in hitop and in the companion hitop-form. Question gate: Jeff chose to trim every cell and to refuse a download of a faulty or empty editor (M138-D1).
- 2026-09-28: T1 done. form.js gains `readQuestionsCsv()`, `writeQuestionsCsv()` and `QUESTION_COLUMNS`, and exports `saveFile()`. `checkQuestions()` gains a `field` option that names the field of a fault. Its default output is unchanged. The three question specs pass (125 tests).
- 2026-09-28: T2 done. link.html gains "Load questions from a file" with its refusal and status lines. The new spec link-questions-file.spec.js (LF1 to LF5) passes 39 tests. Planted defects turned LF1, LF4 and LF5 red: untrimmed fields, and a fetch during the load.
- 2026-09-28: T3 done. link.html gains "Download a template" and "Download these questions" (LF6 to LF8), with the refusals of M138-D1. The file, layout and link specs pass (162 tests). A planted LF line end turned LF6 and LF7 red. The phone layout has no sideways scroll at 375 px.
- 2026-09-28: T4 done. The hitop-form README gains "Write your questions in a spreadsheet" and a test-table row for the new spec. The online-collection article gains a paragraph in "Your own questions", and NEWS gains an entry.
- 2026-09-28: claim audit: 71 claims read, 8 corrected — hitop NEWS.md, vignettes/articles/online-collection.Rmd, hitop-form README.md, form.js, tests/link-questions-file.spec.js
- 2026-09-28: the [O] reader's one re-read passed 6 of the 8 corrections. NEWS and the article still left out a cell past the last column, and the reader's own wording was applied to both. The full hitop-form suite passes locally (630 tests). cairn_validate exits 0.
- 2026-09-28: T5 done. hitop `check()` gives 0 errors, 0 warnings and 0 notes on 37f2f6b6, and `document()` leaves no diff. Minor amendment: T5 now leaves the hitop-form PR and its CI to the merge step of review, because the git model opens a PR only after approval. Status set to review.
- 2026-09-28: review pass 1 returned the milestone at step 3 (defect return 1). AC3 fails: no LF6 option label holds a comma, a double quote and non-ASCII text together. AC1 and AC4 pass, and AC5 waits for the hitop-form CI at the merge step. Status set to in-progress.
- 2026-09-28: amendment return: AC2 — "The message names the row and the column." The whole-file faults have no row. Header and extra-field faults are named by field number.
- 2026-09-28: implement resumed on m138-form-questions-file in both checkouts. Both still contain their `origin/main`.
- 2026-09-28: re-audit: AC2 (full) — the first proposed wording failed for a name used twice, a short row and the 51-question fault. No probe covers the header or past-last-column quote faults. The reader proposed the wording adopted at the mini gate.
- 2026-09-28: re-audit: AC2 (full) — "Any other fault" covered a failed file read and the 51-question refusal named in a no-row sentence. M138-D1 overstated trimming. Also noted: a UTF-16 file without a mark gets no "CSV UTF-8" hint. Only the first missing column is named, and a name used twice is named in list order. Jeff chose to narrow both sentences.
- 2026-09-28: amendment return: AC2 — "The message names where the fault lies. A file that is not UTF-8, an empty file and a file with no question row name no row. A fault in the header row names row 1, and the field number of the field at fault or the missing column. Any other fault this criterion lists names its row, and the column of the field at fault, the column that has no field, or the field number of a field past the last column." The last sentence became "Tests assert the message of a file for each fault this criterion lists." This line executes the return logged at review, and it is the same return.
- 2026-09-28: minor amendment: T6 added for the AC3 test fix and two LF4 probes. Coverage maps AC2 and AC3 to T6. The probes are a task, bound by no criterion (Jeff's choice at the mini gate).
- 2026-09-28: T6 done in hitop-form f9834e2. The LF6 label `Café, "au" lait` holds a comma, a double quote and non-ASCII text. LF4 gains an unclosed quote in the header row and a quote in field 9 past the last column. A planted `field ${k}` turned both new probes and two header probes red on the field number. The full hitop-form suite passes locally (632 tests). The branch adds no R code since the check on 65dec017. The only new prose is the test file's header comment, so the earlier claim audit stands. Status set to review.
- 2026-09-28: step-7 approval: m138-form-questions-file approved for merge, with the companion /Users/jmgirard/github/hitop-form m138-form-questions-file merged first.

## Decisions

- M138-D1 (2026-09-28, question gate): The reader trims every field of the file before it reads it, header cells included. A hidden space at either end of a cell then does not stop a load. "Download these questions" runs the check of "Make the link" and refuses a faulty editor with the message of that check. It also refuses an empty editor. Every file it writes therefore loads back.
- M138-D2 (2026-09-28, AC2 amendment gate, corrects M138-D1): The reader trims every field once its quotes are read. A space outside the quotes of a quoted field is text outside them, and the file is refused as AC2 says. M138-D1's sentence that a space at either end of a cell does not stop a load holds for unquoted cells only.

## Review

Pass 1, 2026-09-28. Both branches contain their `origin/main` (hitop 69b18878, hitop-form 5594a82), so no merge was needed. The full hitop-form suite passed locally on a55b141: 630 tests, 43 of them in `tests/link-questions-file.spec.js`.

- AC1: pass. LF1 loads a file with a byte-order mark, CR LF and the columns in another order. The file holds a quoted comma, a doubled quote and a quoted line break in `options`. It holds `required` as yes, YES, no and blank, and `min`/`max` as ` 0010 `, ` -5 `, `-40` and blank. It has two blank rows and the two lists interleaved. LF1 asserts the editor and the built link. LF2 loads LF line ends, no mark and only the four required columns. LF3 asserts that a load replaces three questions. LF4 refuses `Before`, `Text` and the M137 AC1 rules.
- AC2: not met as written. LF4 asserts 35 refusals by their full message, and the editor keeps its question after each one. The "CSV UTF-8" request is asserted. A fault in a question row names its row and column, such as "row 2, column text". But AC2 says "The message names the row and the column" with no limit to faults that sit in a cell. The four whole-file faults (not UTF-8, empty, no question row, 51 questions) name no row or column. The header faults and the extra-field fault name "field N", because the field has no valid column name. The work is as NEWS and the README describe it, so the wording of AC2 is the fault. This is an amendment return.
- AC3: not met as written. LF6 asserts the byte-order mark, CR LF, the column order, one row per question and `yes`/`no`. It asserts the round trip through the editor and the link. LF7 asserts the template form and four questions. The LF6 questions cover each type, `required: true` and a `min` of -5. The text "Age, in "years" ñ" holds a comma, a double quote and non-ASCII text. But AC3 says "an option label" holds all three, and no single LF6 label does. "Café, au lait" holds a comma and non-ASCII text, and 'Tea "green"' holds the quote. The fix is one LF6 label, such as `Café, "au" lait`. This is a defect return.
- AC4: pass. LF5 records every request from the first network idle through a load of the LF1 file and asserts the record is empty.
- AC5: open. The README section and the article paragraph each state the columns, the `|` separator and the "CSV UTF-8" save choice. Each also states that the file stays in the browser. NEWS names the three controls. hitop `devtools::check()` on 65dec017 gives 0 errors, 0 warnings and 0 notes, and `cairn_validate` exits 0. The hitop-form suite in its CI runs only once its PR opens at the merge step, so this box waits for that run.
- Gate: `devtools::document()` leaves no diff, `pkgdown::check_pkgdown()` finds no problem, README.Rmd is not touched, and NEWS has an entry. `cairn_validate` passes with 24 advisory warnings, none about M138.
- Outcome: returned to in-progress at step 3, before the independent reviewers ran. AC3 fails in its test (defect return 1). AC2 is an amendment return. The reviewers run at the next review.

Pass 2, 2026-09-28. Both branches still contain their `origin/main` (hitop 69b18878, hitop-form 5594a82). The full hitop-form suite passed locally on f9834e2: 632 tests, 45 of them in `tests/link-questions-file.spec.js`.

- AC1: pass again. LF1 to LF3 and the AC1-rule probes of LF4 are unchanged since pass 1 and pass on f9834e2.
- AC2: pass. The amended wording is checked against LF4, whose 37 probes each assert a full message and that the editor keeps its question. Each listed fault has a probe: not UTF-8 (with the "CSV UTF-8" request), empty, no question row, a missing, unknown and repeated column, and fewer and more fields. The quote probes are an unclosed quote, a quote inside an unquoted field and text after a closing quote. The value probes cover `list`, `required`, `min`, `max` and the M137 rules on name, text, type, options and bounds. The three whole-file messages name no row. Header faults name "row 1, field N" or "row 1: it has no column text". Other faults name "row R, column X", "column min has no field" or "row 2, field 9".
- AC3: pass. LF6 asserts the byte-order mark, CR LF, the column order, one row per question and `yes`/`no`. It asserts the round trip through the editor and the link. Its questions cover each type, `required: true` and a `min` of -5. The text "Age, in "years" ñ" and the option label `Café, "au" lait` each hold a comma, a double quote and non-ASCII text. LF7 asserts the template's form and that it loads as four questions, one of each type.
- AC4: pass again. LF5 is unchanged and passes on f9834e2.
- AC5: open until the merge step. The README section, the article paragraph and NEWS are unchanged since pass 1 and state what pass 1 records. hitop `devtools::check()` on 845af3fc gives 0 errors, 0 warnings and 0 notes. The branch changes no file outside `cairn/` since 65dec017. `cairn_validate` exits 0. When its PR opens at the merge step, the hitop-form CI runs.
- Gate: `devtools::document()` leaves no diff, and `pkgdown::check_pkgdown()` finds no problem. NEWS has an entry, and `cairn_validate` passes with no FAIL.
- Reviewers: the history reviewer [S] reported no finding. The prior-review reviewer [S] reported P1. The diff reviewer [O] reported D1 to D15, ranked, and found no AC clearly failing. Its fuzz of 155,651 accepted question sets through write and read found 0 differences. Dispositions follow the gate.
  - P1: "Download these questions" writes text raw through `csvField()`, so a question text that starts with `=` opens as a formula. This is the class of the question-screen candidate row and the M111 lesson.
  - D1: Excel in a semicolon locale saves "CSV UTF-8" with `;`, and the refusal does not say why.
  - D2: a blank row before the header is read as the header and refused. AC1 skips blank rows, but AC1 also says "The first row names the columns", and AC2 counts the header as row 1.
  - D3: a UTF-16LE file with no byte-order mark decodes as UTF-8 with NUL characters, so its refusal has no "CSV UTF-8" hint.
  - D4: a trailing blank header cell or blank cells past the last column refuse the whole file.
  - D5: an Excel re-save can change a text such as `+1 if yes` or `1/2`, or the name `true`.
  - D6: a choice question in a file with no `options` column is named "column options".
  - D7: the fault reported is not always the first in the file, and a name used twice is named on its after-list row.
  - D8: after a load, an earlier built link and error message stay on screen.
  - D9: the README says the builder trims "spaces and line breaks", but `trim()` also removes tabs and other white space. It does not say that a closing quote ends the cell.
  - D10: the hint, README and article do not say that `list` and `type` values are lower case, and the hint does not say column names are.
  - D11: `#questionsErr` has an unused `tabindex`, and a screen reader does not always announce a second download refusal with the same text.
  - D12: no size limit on the chosen file.
  - D13: a CR-only file is one record, and its refusal does not name the line ends.
  - D14: two quick loads can race, and the older file can win.
  - D15: AC5 waits for the hitop-form CI.
- Triage (Jeff, at the gate, 2026-09-28): fix now D3, D9 and D10. Follow-up P1, D1, D4, D5, D8, D11, D12 and D14, in one new candidate row that cross-references the question-screen row. Reject D2, because AC1 says "The first row names the columns" and AC2 counts the header as row 1. Reject D6, because the missing `options` column is "the column that has no field". Reject D7, because AC2 does not order faults. Reject D13, because AC1 allows CR LF and LF only. D15 is noted.
- Fix now, hitop-form 574b89c: a decoded file that holds a NUL is refused with the "CSV UTF-8" request. A new LF4 probe loads a UTF-16LE file with no mark, and it turned red with the check planted off. The README states the white-space trim, the quote rule, lower-case `list` and `type` values and the UTF-16 refusal. The link.html hint states lower-case column names and values. hitop 45f08737 adds lower case to the article. The full hitop-form suite passes (633 tests).
- After the fixes, hitop `devtools::check()` on 45f08737 gives 0 errors, 0 warnings and 0 notes, and `cairn_validate` passes.
