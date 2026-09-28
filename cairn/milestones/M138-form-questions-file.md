# M138: The hitop-form link builder reads the researcher's questions from a spreadsheet file and writes them back as one

- **Status:** in-progress
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

- [ ] AC1: The file is CSV as RFC 4180 describes it. It is UTF-8, with or without a byte-order mark, with CR LF or LF line ends. A quoted field can hold a comma, a double quote written twice, and a line break. The first row names the columns, in lower case and in any order. The columns `list`, `name`, `text` and `type` are required, and `options`, `required`, `min` and `max` are optional. Each further row is one question, and the questions keep the order of the file within each list. A row whose fields are all blank is skipped. `list` is `before` or `after`, and `type` is one of the four types of M137, both in lower case. `options` holds the option labels separated by `|`, each trimmed. `required` is `yes`, `no` or blank, in any letter case, and blank is `no`. `min` and `max` are blank or match `^-?[0-9]+$` after trimming. Each question then meets the rules of M137 AC1. A load replaces the questions in the editor. Tests load files that hold each of these forms and assert the questions the editor shows.
- [ ] AC2: The builder refuses a file and names the fault. The faults are a file that is not UTF-8, an empty file, and a file with no question row. For a file that is not UTF-8, the message tells the researcher to save as "CSV UTF-8". They include a missing required column, an unknown or repeated column name, and a row whose field count differs from the header. They include an unclosed quote, a quote inside an unquoted field, and text after a closing quote. They also include a field value outside the rules of AC1 or of M137 AC1. The message names the row and the column. Rows count as records, with the header as row 1. After a refusal, the editor keeps the questions it held. Tests assert the message of each refusal.
- [ ] AC3: "Download these questions" writes a UTF-8 file with a byte-order mark and CR LF line ends. The file has the columns in the order of AC1 and one row per question. It writes `required` as `yes` or `no`. Loading that file fills the editor with the same questions, compared after the defaults of M137. "Download a template" writes a file of the same form with one example question of each type, and loading it gives four questions. Tests run both round trips. The questions cover each type, `required: true` and a negative `min`. A text and an option label hold a comma, a double quote and non-ASCII text.
- [ ] AC4: Loading a file makes no network request. A test records the requests while it loads a file and asserts that none is made.
- [ ] AC5: The README section and the article paragraph each state the columns and the option separator `|`. They also state the "CSV UTF-8" save choice and that the file stays in the browser. NEWS names the controls. The hitop-form suite passes in its CI. In hitop, `devtools::check()` gives 0 errors, 0 warnings and 0 notes, and `cairn_validate` exits 0.

## Coverage

- AC1 → T1, T2
- AC2 → T1, T2
- AC3 → T3
- AC4 → T2
- AC5 → T4, T5

## Tasks

- [x] T1: In form.js, add `readQuestionsCsv(bytes)`, which decodes with `TextDecoder("utf-8", { fatal: true })`, parses the CSV and maps each row to a question. It passes the result through `checkQuestions()` from M137 and maps each fault to the row and column of the file. Add `writeQuestionsCsv(questions)`.
- [x] T2: In link.html, add the file control, which reads the file with `File.arrayBuffer()`. Fill the editor on success, and show the refusal otherwise. Write the load, refusal and network tests.
- [x] T3: Add the two download controls and the round-trip tests.
- [x] T4: Write the README section "Write your questions in a spreadsheet", the article paragraph and the NEWS entry.
- [ ] T5: Get hitop-form CI green on its PR. Run hitop `check()` and `cairn_validate`.

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

## Decisions

- M138-D1 (2026-09-28, question gate): The reader trims every field of the file before it reads it, header cells included. A hidden space at either end of a cell then does not stop a load. "Download these questions" runs the check of "Make the link" and refuses a faulty editor with the message of that check. It also refuses an empty editor. Every file it writes therefore loads back.

## Review
