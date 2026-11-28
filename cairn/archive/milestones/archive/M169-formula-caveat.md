# M169: Spreadsheet formula caveat for response files

**Status:** done (2026-10-07, PR #177 https://github.com/jmgirard/hitop/pull/177; companion hitop-form PR #32 https://github.com/jmgirard/hitop-form/pull/32)

**Goal:** A researcher who uses hitop-form learns two facts: a spreadsheet program can run a participant's text in a response file as a formula, and `read_form_responses()` keeps it as text.

**Outcome:** Docs and tests only. The page's output is unchanged.
- hitop-form README: "Where the file lands", "Send responses to a Google Sheet" and "Send responses to Supabase" each state the caveat. If the file is opened in a spreadsheet program, a participant identifier or text answer such as `=1+1` can be read as a formula. `read_form_responses()` returns it as text. The sheet text names the apostrophe that `doPost` writes before each cell and says the CSV download does not keep it. The unchecked "open it in the spreadsheet as text" advice was removed.
- `vignettes/articles/online-collection.Rmd`: the same caveat in "3. Download the sheet as CSV" and "The Supabase route". The deploy step names the apostrophe. Every value claim is limited to the sheet or table itself.
- NEWS: one bullet under the development version's "Documentation and website".
- Tests in `test-read_form_responses.R`: `formula_copy()` writes a new `=1+1` participant into row 2 and a `q_note` of `=1+1` into row 1 of the sheet download and the Supabase export. A page-file test sets participant and `q_note` to `=1+1`. A planted `sub("^=", "'=", ...)` in the reader failed all of them. Suite: 1158 tests.

**Decisions:** M169-D1: the M095 review's rejection of formula escaping (O15) stands, with a second reason. Anyone with a study link can post a row directly, so an escape written by the page does not stop a planted formula.

**Review:** Two passes, user-facing tier. Pass 1 stopped at the gate because the plan had skipped a NEWS entry (review return 1, fixed by T6). Pass 2 spawned diff-bug, blame-history and prior-review: 16 findings, of which 8 were fixed, 3 followed up and 5 rejected. Fixed: a test half that changed nothing, an unexplained "protection", "as text" credited alone, misleading "before you open", the untested saved-file claim, and long lines. Follow-up: the "Formula caveat reach" candidate row (the `+`, `-` and `@` prefixes, and import as text). Criteria audit (full) 5 findings applied, claim audit 25 claims with 1 corrected. CI: hitop 8 of 8, hitop-form 1 of 1. The CI wait hit the ceiling once and resumed through route (c). The merge guard refused a `cd …;` spelling, and a plain `gh pr merge 177` passed. At hygiene, the formula clause left the "hitop-form question gaps" row, and the questions-file part stays. No lesson added (LESSONS.md at its byte budget).
