# M137: A study link can carry the researcher's own questions, which hitop-form asks before or after the form and writes as q_ columns

- **Status:** review
- **Priority:** normal
- **Depends on:** M135, M136
- **Driving RR:** —
- **Principles touched:** IP1, GP3
- **Resolves:** —
- **Surface tier:** user-facing — the deployed form page, its link builder, its stores and the package's article
- **Branch/PR:** m137-form-custom-questions, companion: /Users/jmgirard/github/hitop-form m137-form-custom-questions

## Goal

A study link's `questions` field holds the researcher's own questions. The page asks them on a screen before the start screen or after the last item page. It writes each answer in a `q_<name>` column after the item columns.

## Scope

**In:** In hitop-form, the work covers these parts. The `questions` link field (`before` and `after`, each a list of questions) in `parseLink()`, with its refusals. Four question types: `text` (one line), `number` (a whole number, with optional `min` and `max`), `choice` (one option) and `multi` (any number of options). A `required` flag, false by default. The two question screens and their checks. The `q_` columns in the row, the file and the Supabase SQL. A question editor in link.html, with its prefill through `?z=` from M135. A README section and Playwright tests. In hitop, the work covers a test fixture that the page saved, the questions section of the online-collection article, and NEWS. D-079 governs.

**Out:** Long text answers, drop-down menus, rating grids, display logic between questions, and more than one screen per block go to a candidate row. Reading and writing the questions as a spreadsheet file goes to M138. More than one instrument in one link goes to M139 (reader) and M140 (page). Instrument content (IP1) is untouched. The questions never share a screen with the items.

## Acceptance criteria

- [ ] AC1: `parseLink()` accepts `questions` as an object with the key `before`, `after` or both, and no other key. Each key holds a list of 1 or more questions, with at most 50 questions in all. A question has `name`, `text` and `type`. It can also have `required`, `options`, `min` and `max`, and no other key. `name` matches `^[a-z][a-z0-9_]{0,29}$` and is unique across both lists. `text` is a string of 1 to 1,000 characters, not blank after trimming. `type` is `text`, `number`, `choice` or `multi`. `options` is required for `choice` and `multi` and refused for the other types. It is a list of 2 to 20 strings, each 1 to 200 characters, none blank and no two the same after trimming. No question text or option label holds a line break, and no option label holds `|`. The page trims each question text and option label. `min` and `max` are allowed only for `number`. Each is a whole number from -2,147,483,647 to 2,147,483,647, and `min` is not above `max`. `required` is `true` or `false`. A lone surrogate in any string is refused. A character is one UTF-16 code unit. The page refuses each fault by name and shows the question's position. Before it builds a link, link.html refuses the faults its editor can produce. Tests assert the message of each refusal on each side where it can occur.
- [x] AC2: The `before` questions show on one screen before the start screen. If the link has a consent screen, they show after it. This screen has no Back. The `after` questions show on one screen after the last item page. Under `after`, the last item page carries Next, and the questions screen carries Back and Finish. The page writes each question text and option label as text, so no tag or entity in them is read as markup. A `text` answer that is blank after trimming counts as no answer. A `number` answer, after trimming, is an optional minus sign and digits. The screen refuses to go on while a required question has no answer. It also refuses a `number` answer outside that form or outside `min` to `max`, and a `text` answer that holds a lone surrogate. Each refusal names the question. Tests walk each type and each refusal, with the `number` probes `abc`, `2.5`, `1e3`, `-0`, `007`, `min - 1` and `max + 1`. A question text and an option label hold `<b>x</b>` and `&amp;`, and tests assert their text and that no `b` element exists. Tests also walk a link with only `before`, a link with only `after`, and a link with both.
- [x] AC3: The row and the file carry one column `q_<name>` per question, after the item columns. The columns follow the order of the link, with the `before` list first. A `text` answer is written as typed. A `number` answer is the whole number in decimal digits, with no leading zero and a minus sign only below zero. So `007` is `7` and `-0` is `0`. A `choice` answer is the position of the chosen option, counted from 1. A `multi` answer is the positions of the chosen options in ascending order, joined by single spaces. An unanswered question is an empty string. In the JSON row, every `q_` value is a string. Tests assert the row and the file for each type, answered and unanswered. They include a `multi` question clicked as option 3 then option 1. The tests run with shuffle off, with shuffle on, and with `prolific: true`.
- [x] AC4: For a link with `questions`, the Supabase SQL from the builder adds one `text` column per question. These columns follow the item columns in the order of AC3. The webhook send and the Supabase send carry every `q_` key, with an unanswered question sent as `""`. Tests compare the SQL with a committed fixture byte for byte and read the `q_` keys from the recording server.
- [x] AC5: The link.html editor adds and removes questions. For each question, it sets the list (before or after), the name, the text and the type. It also sets the options (one per line in a `<textarea>`), `required`, `min` and `max`. The builder writes a link with `questions` as `?z=`. This extends the `z` clause of M135 AC4 to a link with questions and no consent. A link loaded through `?z=` fills the editor with the same questions. Tests build a link with one question of each type, open it, and reload it through the prefill.
- [x] AC6: One file that the page saved under a link with one question of each type is committed in hitop as a test fixture. `read_form_responses()` reads it. Each `q_` value equals the answer the walk entered, as AC3 writes it, and an unanswered question is `NA`.
- [ ] AC7: The README section and the questions section of the article each state the four types and the value each type writes. They also state the position numbering of options and the 50-question limit. They state that a change to the order of options between links changes what the numbers mean. They state that a Supabase table made before the questions were added refuses the row, and that the page then saves the file. NEWS names the field. The hitop-form suite passes in its CI. In hitop, `devtools::check()` gives 0 errors, 0 warnings and 0 notes, and `cairn_validate` exits 0.

## Coverage

- AC1 → T1, T5
- AC2 → T2
- AC3 → T3
- AC4 → T4
- AC5 → T5
- AC6 → T6
- AC7 → T7, T8

## Tasks

- [x] T1: In form.js, add `checkQuestions()` to `parseLink()` with the refusals of AC1, and export it for link.html.
- [x] T2: Add the `before` and `after` question screens to `runForm()`. Build each input with the `el()` helper and label it for screen readers. Use a text input with `inputmode="numeric"` for `number`, so the page reads what the participant typed. Add the checks and move focus to the first refused question. Carry the answers into the record in `finish()`, and count them in the `beforeunload` guard. Write the walk tests of AC2.
- [x] T3: Add `questionColumns(config)` for the trailing columns, used by `buildCsv()`, `buildRow()` and `storeSql()`. Leave `leadColumns()` unchanged. Write the values of AC3 and the row and file tests.
- [x] T4: Extend `storeSql()` with the `text` columns. Add the SQL fixture under `tests/fixtures/`, written by rule and not captured from the builder, with its README line. Write the send tests.
- [x] T5: Add the question editor to link.html, its submit checks through `checkQuestions()`, the `?z=` output, and `prefill()`. Write the builder tests of AC5.
- [x] T6: Save a file from a walk with one question of each type. Commit it under `tests/testthat/fixtures/` in hitop, with a row in `tests/testthat/fixtures/README.md` that names the hitop-form commit and the command. Write the reader test of AC6.
- [x] T7: Write the README section "Ask your own questions", the questions section of the article, and the NEWS entry.
- [x] T8: Run the full hitop-form suite locally. Its CI runs on the companion PR that `/milestone-review` opens after approval. Run hitop `check()` and `cairn_validate`.

## Work log

- 2026-09-28: created by /milestone-plan.
- 2026-09-28: criteria audit ran in full mode (user-facing tier) on two fresh [O] readers. They returned 8 findings and 1 finding, all fixed before the commit. The fixes cover unknown keys, option limits, the integer range, number input rules, markup probes, Back and Next placement, unanswered values, a separate column helper, the fixture record, and labels without line breaks or `|`.
- 2026-09-28: plan chose option positions over option labels as the stored value of a choice, because positions match the integer answers of the instruments and survive a relabeling. Falsified by researchers who reorder options between links and misread the numbers.
- 2026-09-28: implement started. Branch m137-form-custom-questions cut in hitop and in the hitop-form companion. Gate chose headings, numbered questions, the editor with Move up and Move down, and a range line (M137-D1).
- 2026-09-28: T1 done in hitop-form 6db09b1. `checkQuestions()` in `parseLink()`, with a `where` argument so link.html names questions by its own numbers. tests/questions.spec.js Q1 and Q2: 59 passed. The edit tool wrote U+2028 as a literal character, which broke a regex, so the pattern is now built from code points.
- 2026-09-28: T2 done in hitop-form 25037e4 (screens code in 6db09b1). tests/question-screens.spec.js QS1 to QS8: 19 passed. QS8 is the first test of the unload guard, with a click-only control.
- 2026-09-28: T3 done in hitop-form fa81800. `questionColumns()` feeds `buildCsv()` and `buildRow()`. tests/question-columns.spec.js QC1: 6 passed, file and webhook row under shuffle off, shuffle on and prolific.
- 2026-09-28: T4 and T5 done in hitop-form 168360e, one commit because the SQL fixture test runs through the builder. `storeSql()` takes the questions. Fixture `supabase-hitopbr-questions.sql` written by rule. QC2 reads the q_ keys from a webhook and a Supabase send (2 passed). tests/link-questions.spec.js LQ1 to LQ6: 24 passed. `encodeLink()` writes `z` for questions too, per D-079(b). A first run found hidden option and range fields still shown, because `label { display: block }` beat `hidden`, and a scoped rule fixed it. Full suite before the editor: 539 passed. Builder specs after it: 164 passed.
- 2026-09-28: T6 done. hitop-form b088d3b captures `responses-hitopbr-questions.csv` (QC3, five questions, one left empty). The copy here is stored as LF, with a fixtures README row and a reader test of its five `q_` values. `devtools::test()`: 0 failed, 23,800 passed, 15 skipped.
- 2026-09-28: T7 done. hitop-form 68049f0 adds the README section "Ask your own questions" and the questions in the builder steps, the link paragraphs, Supabase, scoring and the test table. Here, the article section "Your own questions" with a chunk that splits a multi answer, a pointer from the reading section, and a NEWS entry.
- 2026-09-28: T8 checks: hitop-form full suite 564 passed. `cairn_validate` exits 0 with 24 advisory warnings.
- claim audit: 152 claims read, 6 corrected — hitop-form README.md, link.html, tests/fixtures/README.md, tests/link-questions.spec.js, tests/question-screens.spec.js, hitop vignettes/articles/online-collection.Rmd
- 2026-09-28: claim audit fixes in hitop-form a05f17a and here. The new-table advice now names question names, not any change. The editor gains two line-separator probes (U+2028 in a text and an option). Four wording fixes. The same reader re-read the six: all correct.
- 2026-09-28: T8 done. `devtools::check()`: 0 errors, 0 warnings, 0 notes. T8's wording changed (minor amendment): the git model opens both PRs at review after approval, so hitop-form CI for AC7 is read at `/milestone-review` step 8. Status set to review.

## Decisions

- M137-D1 (2026-09-28, implement gate): The before screen is headed "Before you begin" with a Next button. The after screen is headed "Before you finish" with Back and Finish. Questions are numbered on their screen, a required one ends in "(required)", and a refusal names the question by its number. The link.html editor sits after the consent fields, with "Add a question", and Remove, Move up and Move down in each group. Blank lines in the options box are skipped. A `number` question with `min` or `max` shows its range under the question.

## Review

Evidence run 2026-09-28 on hitop fe523870 and hitop-form a05f17a. Both branches are level with `origin/main`. The full hitop-form suite passed 566 tests locally (`npx playwright test`).

- AC1 evidence: `tests/questions.spec.js` Q1 asserts the page's message for each fault class that AC1 names, and Q2 asserts the accepted edges. `tests/link-questions.spec.js` LQ4 asserts the editor's message for each fault its fields can produce. LQ4 includes two U+2028 probes and a 51-question probe. The page checks CR, LF, U+2028 and U+2029 as line breaks (`form.js:291`). Tick held: review finding O6 contests the "line break" clause and goes to the gate.
- AC2 evidence: `tests/question-screens.spec.js` QS1 to QS3 walk a link with only `before`, a link with only `after`, and a link with consent and both lists. They assert the screen order and the placement of Next, Back and Finish. QS4 puts `<b>x</b>` and `&amp;` in a text and an option, asserts the text, and finds no `b` element. QS5 refuses each required type unanswered and a blank text. QS6 refuses `abc`, `2.5`, `1e3`, 17 and 100 against 18 to 99, and accepts `-0` and `007`. QS7 refuses a lone surrogate. Each refusal names the question by its number.
- AC3 evidence: `tests/question-columns.spec.js` QC1 walks nine questions of all four types, with the five `before` questions answered and the four `after` questions left empty. The walk types `007` and `-0`, and clicks multi option 3 and then option 1. It asserts the values `7`, `0`, `2`, `1 3` and `""` in the saved file and in the webhook row. It asserts the order after the item columns and that each `q_` value is a string. QC1 runs with shuffle off, shuffle on and `prolific: true`, 6 tests in all.
- AC4 evidence: `tests/link-questions.spec.js` LQ3 compares the builder's shown SQL with `tests/fixtures/supabase-hitopbr-questions.sql` byte for byte. That fixture has one `text` column per question after the item columns. `tests/question-columns.spec.js` QC2 walks with every question unanswered. It reads each `q_` key as `""` from the recording server, for a webhook send and for a Supabase send.
- AC5 evidence: `tests/link-questions.spec.js` LQ1 fills the editor with one question of each type across both lists. It asserts that the link holds only a `z` parameter with no consent text and that the decoded setup equals the questions. It then opens the link and meets each screen. LQ2 loads a `?z=` link and reads each editor field back, options and `required` included, and rebuilds the same setup. LQ6 moves and removes questions, and LQ5 asserts that blank option lines are skipped and hidden fields stay out.
- AC6 evidence: `tests/testthat/fixtures/responses-hitopbr-questions.csv` equals hitop-form's copy once CR bytes are removed (`diff`). Its README row names commit `b088d3b` and the command. The test "a file the page saved with one question of each type reads" asserts `7`, `2`, `1 3`, `Hello, "world"` and `NA`, all as character. `devtools::test(filter = "read_form_responses")` passed with 0 failures.
- AC7 evidence: the README section "Ask your own questions" and the article section "Your own questions" each state the four types and their values. Each states the numbering of options from 1 and the 50-question limit. Each warns that a new option order changes what the numbers mean. Each states that an older Supabase table refuses the row and that the page then saves the file. NEWS names the `questions` field. `devtools::check()`: 0 errors, 0 warnings, 0 notes. `cairn_validate` exits 0. The hitop-form suite passed 566 tests locally. Tick held until the companion PR's CI is green at step 8.

Consistency gate: `cairn_validate` passes all 16 checks and exits 0, with 2 advisory warnings (dangling id tokens, references staleness) that predate this branch. No DESIGN principle changed, so `cairn_impact` was skipped. `devtools::document()` leaves no diff. `pkgdown::check_pkgdown()` finds no problems. README.Rmd and README.md are unchanged on the branch. NEWS has the entry, and no new top-level file exists.

Independent review, three fresh-context lenses. [S] blame-history: no regression of a past commit or D-entry, 1 finding. [S] prior-review: no regression of a past review finding, and the GitHub comment probes on both repos are empty. [O] diff-bug: 11 findings, none shown by the reviewer to break a criterion. Proposed dispositions, most severe first, pending the gate:

- O6: the page's line-break set (CR, LF, U+2028, U+2029 at `form.js:291`) lets U+0085, vertical tab and form feed through. Unicode treats all three as line breaks, so AC1's "no line break" fails for them. Proposed: defect return, widen the set to Unicode's mandatory breaks with page and editor probes.
- O1: every `number` input has `inputmode="numeric"` (`form.js:1428`), and the iPhone keypad for it has no minus key. A participant on an iPhone cannot type `-3` for a question with a negative `min`. Proposed: fix now. If `min` is 0 or more, the page keeps the numeric keypad, and otherwise it drops it.
- B1: if a link holds consent text and questions and the browser cannot compress, `encodeLink()` names only "consent text" (`form.js:110`). Proposed: fix now.
- O4: the editor turns a very long digit string into `Number()`, so a refusal can read "it is null" or "it is 1e+23". Proposed: fix now, quoting what the researcher typed.
- O2: a participant's text answer is written raw, so Excel reads `=HYPERLINK(...)` in the saved file as a formula. `csvField()` predates this branch, and the participant identifier has the same exposure. Proposed: follow-up, a candidate row.
- O3: text answers have no length limit. Proposed: follow-up, in the same candidate row.
- O9: required choice and multi groups carry no required state for screen readers, and a refusal sets no `aria-invalid` or `aria-describedby`. Proposed: follow-up, in the same candidate row.
- O10: the unload-guard test covers a text answer on the before screen only. Proposed: follow-up, in the same candidate row.
- O5: some input variants of tested refusals are not probed (editor min below range, U+2029 on the page, a name repeated in one list, `before: null`). Proposed: reject, because each refusal message is asserted on each side where it occurs.
- O7: the 1,000 and 200 limits count characters before trimming. Proposed: reject, because AC1 states the limit on the string as given.
- O8: opening a hand-made link in the builder can drop a line break, stray options, a stray bound or `required: false`. Proposed: reject, because each dropped part is one the page refuses or one that changes nothing.
- O11: with `min` equal to `max`, the range line reads "from 3 to 3". Proposed: reject as wording only.
- P4 (prior-review, noted): the `z` prefill gap of the M135 candidate row stays as it was. M137 adds work inside the guarded block and leaves the gap's shape unchanged.
