# M140: A study link can field two or more instruments in one hitop-form session

- **Status:** review
- **Priority:** normal
- **Depends on:** M137, M139
- **Driving RR:** —
- **Principles touched:** IP1, GP3
- **Resolves:** —
- **Surface tier:** user-facing — the deployed form page, its link builder, its stores and the package's article
- **Branch/PR:** m140-form-multi-instrument; companion: /Users/jmgirard/github/hitop-form m140-form-multi-instrument

## Goal

A study link's `instruments` field lists two or more instruments. The page gives them one after another in one session. It writes one row with a group of item columns per instrument, in the file shape that M139 reads.

## Scope

**In:** In hitop-form, the work covers these parts. The `instruments` link field in `parseLink()`, with its refusals. The fetch of every export at load. One start screen and one run of item pages per instrument. The row, the file and the Supabase SQL in the shape of D-080. An ordered instrument list in link.html, with its prefill. A README section and Playwright tests. In hitop, the work covers a test fixture that the page saved, a section in the online-collection article, and NEWS.

**Out:** A HiTOP-SR module beside another instrument keeps the one `module` field, which applies to the HiTOP-SR entry. Modules for other instruments stay with the candidate row for modules of the BR and the PID-5. A random order of the instruments themselves goes to the question-features candidate row. Instrument content (IP1) is untouched. Each instrument shows its own instructions and items as its export holds them.

## Acceptance criteria

- [x] AC1: `parseLink()` accepts `instruments` as a list of 2 or 3 distinct instrument names that the page knows, in the order the page gives them. It refuses by name `instruments` beside `instrument`, a list that is not an array, and a list of fewer than 2 names. It also refuses a repeated name, an unknown name, and a list that holds two of `pid5`, `pid5sf` and `pid5bf`. If the list holds no `hitopsr`, it refuses a `module`. If the list holds `hitopsr`, it checks a `module` against it. A link with `instrument` behaves as on main. Before it builds a link, link.html refuses the faults its controls can produce. Tests assert the message of each refusal on each side where it can occur.
- [x] AC2: The page fetches every export in the list before it shows the first screen. A refusal of an export names the instrument. Each instrument has its own start screen, with its title, version line, instructions, and item and page counts, followed by its item pages. Item positions and page labels count within each instrument. If the link gives no participant identifier, the start screen of the first instrument asks for it. No later start screen asks. The notice of where the answers go shows on the first start screen only. The first item page of each instrument carries no Back. Each instrument's items start unanswered. The order of screens is the consent screen of M135, the `before` screen of M137, each instrument in link order, then the `after` screen. Tests walk links with two and with three instruments, with and without `consent` and `questions`. A test asserts that no radio is checked on the first page of a second instrument whose item numbers overlap the first's.
- [x] AC3: The row and the file follow D-080 as M139 reads it. The `instrument` cell holds the stems in link order, joined by single spaces. The `form_build` cell holds the build date of each export in the same order, joined by single spaces. The item columns come in one group per instrument, in link order. Under shuffle, each instrument's items are shuffled among that instrument's items, and none moves to another instrument's pages. The `item_order` cell then holds one group per instrument. The `q_` columns of M137 follow the item columns. Tests answer each instrument with a different pattern. They assert the row and the file with shuffle off and on, with `prolific: true`, and with `questions`.
- [x] AC4: For an `instruments` link, the Supabase SQL from the builder has the lead columns first. Then it has one `integer` column per item of each instrument in the order of AC3, then the `q_` columns. The webhook send and the Supabase send carry every column. Tests compare the SQL with a committed fixture written by rule, byte for byte, and read the keys from the recording server.
- [x] AC5: link.html lets the researcher choose one instrument, as on main, or build an ordered list of two or three. A single choice writes `instrument`, and a list writes `instruments`. A link loaded through `?c=` or `?z=` fills the same choice. Tests build, open and reload a link with two and with three instruments.
- [x] AC6: One file that the page saved under a link with three instruments, shuffle on and one question is committed in hitop as a test fixture. `read_form_responses()` reads it, and each instrument's columns, chosen by stem, score with its scoring function.
- [ ] AC7: The README section and the article section each state the field, the order of screens, the file shape, and how to score each instrument. NEWS names the field. The hitop-form suite passes in its CI. In hitop, `devtools::check()` gives 0 errors, 0 warnings and 0 notes, and `cairn_validate` exits 0.

## Coverage

- AC1 → T1, T5
- AC2 → T2
- AC3 → T3
- AC4 → T4
- AC5 → T5
- AC6 → T6
- AC7 → T7, T8

## Tasks

- [x] T1: In form.js, add `checkInstruments()` to `parseLink()` with the refusals of AC1, and the `module` rule.
- [x] T2: In `boot()` and `runForm()`, fetch every export, and plan one start screen and one run of pages per instrument. Key the answers by stem and item number, since item numbers repeat across instruments. Keep the page-number labels per instrument. Write the walk tests of AC2.
- [x] T3: Extend `leadValues()`, `buildCsv()` and `buildRow()` for the cells and groups of AC3. Write the row and file tests.
- [x] T4: Extend `storeSql()` for the groups, add the SQL fixture under `tests/fixtures/` with its README line, and write the send tests.
- [x] T5: Add the ordered instrument list to link.html, its submit checks, and `prefill()`. Write the builder tests.
- [x] T6: Save the file of AC6 from a walk. Commit it under `tests/testthat/fixtures/` in hitop, with a row in `tests/testthat/fixtures/README.md` that names the hitop-form commit and the command. Write the reader and scoring test.
- [x] T7: Write the README section "Give more than one instrument", the article section and the NEWS entry.
- [x] T8: Get hitop-form CI green on its PR. Run hitop `check()` and `cairn_validate`.
- [x] T9: (review F1, F2, F7) In `checkInstruments()`, refuse an entry that is not text as an unknown name, and word the PID-5 refusal for three forms. Add page tests for nested entries and three PID-5 forms. Add builder tests for each refusal an opened link can hold, and for three PID-5 rows.
- [x] T10: (review F8, F9, F13) Add a network test that walks a list link with no store. Add leave-page warning tests for an answered item and for answers held in an earlier instrument. Add a test that a single-instrument start screen shows no part line. Then remove the unload-guard clause from the question-screen gaps candidate row.
- [x] T11: (review F3, F5, F6, F11, F17) Remove the stale "does not yet write" sentences in NEWS.md and data-raw/form_multi_fixtures.R. Add the 5 new specs and the list-link network row to the hitop-form README table. Update the one-instrument wording the findings name, and rewrap the comment lines.
- [x] T12: (review F12) Reload the builder with a list built, and see whether the rows reset while the text fields stay. If they do, fix it and add a test.

## Work log

- 2026-09-28: created by /milestone-plan. Jeff asked at the plan gate to plan several instruments in one link now.
- 2026-09-28: criteria audit ran in full mode (user-facing tier) on a fresh [O] reader. It returned 4 findings, all fixed before the commit. The fixes cover a limit of 3 instruments (two PID-5 forms are refused), answers keyed by stem, the order of screens and the shuffle scope.
- 2026-09-28: plan chose one row with a group per instrument over one row per instrument, because one row keeps each participant in one record for every store. Falsified by a store that cannot hold the width of three instruments.
- 2026-09-28: implement started. Branch m140-form-multi-instrument cut in hitop and in the hitop-form companion checkout. Implement gate answered (M140-D1).
- 2026-09-28: T1 done. `checkInstruments()` and `linkStems()` in form.js, the module rule in `parseLink()`, and tests/instruments.spec.js (19 page-side refusal tests, a plant on the PID-5 rule seen red). Full suite 652 of 652.
- 2026-09-28: T2 done. `fetchExports()`, `planStems()` and a part per instrument in `runForm()`, with its own answers, start screen ("Part n of N") and pages. The row and file builders take one group per part, so T3's cells are in the same commit. tests/instruments-walk.spec.js has 10 tests. A plant of one shared answer map turned W4 red on its own assertion. Full suite 661 of 662. One question-screens test hit its 15-second wait under 6 workers. It then passed 5 of 5 alone and 60 of 60 with its spec.
- 2026-09-28: T3 done. tests/instruments-row.spec.js has 10 tests: file and webhook row for two instruments under 4 modes, and three instruments under shuffle. Plants of a "; " group separator (4 red) and a comma-joined `form_build` (10 red) were seen red.
- 2026-09-28: minor amendment, T5 moves before T4. T4's SQL test compares the builder's SQL, and the builder takes a list only after T5.
- 2026-09-28: T5 done. link.html has the instrument rows of M140-D1 and their list and module checks. Its SQL reads every export, and its prefill fills the rows from a `c` or `z` list. tests/link-instruments.spec.js has 21 tests. A plant that skipped the prefill check turned 5 red. The entry wording of `checkInstruments()` changed to "entry 2 is" so the builder can say "instrument 2". Screens checked at 760 and 375 pixels, light and dark, with no sideways scroll.
- 2026-09-28: T4 done. `storeSql()` needed no change, because link.html passes it every plan's items in link order. Two SQL fixtures were written by rule, with their README row. tests/instruments-store.spec.js has 6 tests: the builder's SQL byte for byte, and the keys of rows posted to a webhook and to a supabase store. A plant that reversed the group order turned both SQL tests red.
- 2026-09-28: T6 done. hitop-form dd7a241 captured `responses-multi-page-shuffled.csv` (HiTOP-BR, PID-5-BF and the whole HiTOP-SR, shuffle, `age`). Its test IR3 checks the file against its own `item_order` cell. The hitop copy is LF, with a fixtures README row. The new reader test works out each instrument's answers from the page's pattern and scores each group by stem. A plant that dropped the per-instrument shift turned it red. `devtools::test()`: 0 failures, 24,046 passes.
- 2026-09-28: T7 done. hitop-form b6c617b adds the README section "Give more than one instrument" and a scoring example. In hitop, the online-collection section is renamed "Several instruments in one session" and states the field and the screens, with its two anchors updated. The help page and a test comment no longer say the page cannot write the file. NEWS has the entry.
- 2026-09-28: T8 done locally. hitop-form full suite 700 of 700 on its third run. The two earlier runs each lost 1 or 2 tests to a 15-second wait or a failed export fetch under 6 workers. Each such test passed on rerun. Main passed 633 of 633 in one full run. hitop `devtools::check()`: 0 errors, 0 warnings, 0 notes. `cairn_validate` exits 0 with 24 advisory warnings. Deviation: no PR exists yet, because the git model opens both PRs at `/milestone-review`'s merge step. The hitop-form CI half of T8 and AC7 is therefore checked there.
- claim audit: 115 claims read, 6 corrected — hitop-form tests/instruments.spec.js, tests/link-instruments.spec.js (2), tests/instruments-row.spec.js, tests/helpers.mjs; hitop vignettes/articles/online-collection.Rmd. The same reader re-read the 6 and passed each.
- 2026-09-28: implement complete, status review.
- 2026-09-28: review in progress. AC1 to AC6 evidence recorded and ticked. AC7, the consistency gate and two of three reviewers still pending.
- 2026-09-28: review checkpoint. Consistency gate clean. AC7 open on hitop-form CI. 19 findings recorded, F1 an AC1 failure confirmed with node. Awaiting the step-7 gate.
- 2026-09-28: defect return 1. AC1 failed: `checkInstruments()` accepts a nested entry, so `["hitopbr", ["hitopbr"]]` and `["pid5", ["pid5bf"]]` are not refused by name (F1). Jeff chose to send it back at the step-7 gate. Status in-progress, with T9 to T12 added from the fix-now findings.
- 2026-09-28: T9 done. `checkInstruments()` refuses an entry that is not text as an unknown name, and names three PID-5 forms as three. tests/instruments.spec.js adds 5 probes (2 nested lists, null, an object, three PID-5 forms). The 2 nested and the three-form probes were red before the fix. tests/link-instruments.spec.js adds the three-row refusal and 11 opened-link refusals. Full suite 715 of 715.
- 2026-09-28: T10 done. network.spec.js N9 walks a list link of two instruments and sees only the page, form.js and the two exports. instruments-walk.spec.js W6 has 3 leave-page tests and W7 checks no part line on a single-instrument link. Three plants were seen red. A guard that reads the current part only failed the second-instrument test. A part line on every link failed W7, and a stray request under a list failed N9. A first W6 test on the second start screen passed that plant, so it moved to that instrument's first page. The unload-guard clause left the question-screen gaps row. Full suite 720 of 720.
- 2026-09-28: T11 done. The "does not yet write" sentences left NEWS.md and data-raw/form_multi_fixtures.R. The hitop-form README table has rows for the 5 new specs, and its network row names one export fetch per instrument. Several-instrument wording was added to the README's participant, file and Sheet sections, link.html's intro and the article's lines 19, 71 and 98. The form.js header lists the new link.html imports, and 4 long comment lines and the NEWS entry were rewrapped. Full suite 720 of 720.
- 2026-09-28: T12 done, no change. A probe spec (not committed) built three rows, typed the study and participant, checked shuffle, filled the module and set each menu. It then reloaded the builder, and in a second run went back from another page. In the suite's Chromium both times every control came back at its default, the rows as one HiTOP-SR row, so F12 does not occur there. Other browsers were not run.
- claim audit: 160 claims read, 1 corrected — hitop-form tests/network.spec.js (the walk set now names one export per instrument). The reader counted the claims as about 160 and re-read the corrected one, which passed.
- 2026-09-28: return work complete, status review. hitop-form full suite 720 of 720. hitop `devtools::test()`: 0 failures, 24,046 passes. `cairn_validate` exits 0 (25 advisory warnings). AC1 and AC7 stay open for review.
- 2026-09-28: second review pass in progress. AC1 to AC6 evidence recorded, AC1 ticked. AC7, `devtools::check()` and the three reviewers still pending.

## Decisions

- M140-D1 (2026-09-28, implement gate): link.html's instrument choice becomes a list editor of up to three rows. Each row is a menu with Move up, Move down and Remove, and an "Add an instrument" button adds a row. One row writes `instrument`, and two or three write `instruments`. A saved file of several instruments is named by the stems joined by "-". The page's name rule already writes the space-joined `instrument` cell so. Under an `instruments` link each start screen shows "Part n of N" above the instructions. Chosen by Jeff over three fixed menus, a file name of "multi", and no part line.

## Review

Fresh runs on 2026-09-28. Both branches contain origin/main (no merge needed). hitop-form full Playwright suite: 700 of 700 passed in 1.8 minutes, default workers, no reruns.

- AC1: tests/instruments.spec.js (19 tests) passed. Each test asserts the full message of one page-side refusal. The field probes are `instruments` beside `instrument`, a value that is not a list, and lists of 0, 1 and 4. The name probes are unknown names, a repeated name and the three PID-5 pairs. The module probes are a module beside a list without `hitopsr`, and three faulty modules beside a list with `hitopsr`. tests/link-instruments.spec.js (21 tests) passed. It asserts the builder-side refusals of repeated rows, PID-5 pairs and the two module rules by message. The single-`instrument` specs passed in the same run.
- AC2: tests/instruments-walk.spec.js (10 tests) passed. W1 holds one export back and sees no screen. W2 names the instrument of a missing export, of an export of another format, and the first of two refused. W3 walks two and three instruments. On each start screen it asserts the title, version line, part line, instructions and counts. It asserts the notice and the identifier field on the first start screen only. On each first page it asserts no Back and position 1. W4 asserts no radio checked on the PID-5-BF's first page after the HiTOP-BR, whose item numbers overlap. W5 walks two and three instruments with consent and questions, in the order consent, before, instruments, after.
- AC3: tests/instruments-row.spec.js (11 tests) passed. IR1 walks the HiTOP-BR then the PID-5-BF, each answered by its own pattern, under 4 modes: shuffle off, shuffle on, `prolific: true` and questions. In each mode it asserts the saved file and the webhook row. The asserted cells are the space-joined `instrument` and `form_build` cells, the item groups in link order with each value, and the `q_` columns last. It also asserts that each instrument's pages show only its own items and, under shuffle, one `item_order` group per instrument. IR2 asserts three instruments under shuffle, as a file and as a row. The expected cells are worked out from the exports, not from form.js.
- AC4: tests/instruments-store.spec.js (6 tests) passed. IS1 compares the builder's SQL byte for byte with 2 fixtures. The first is HiTOP-BR then PID-5-BF. The second is PID-5-BF then HiTOP-BR, with shuffle, Prolific and a question before and after. IS2 walks each link and posts to a webhook and to a supabase store on the recording server. It asserts that the keys of each posted row equal the fixture's column lines in order. The review rederived `supabase-hitopbr-pid5bf.sql` from `supabase-hitopbr.sql` by its README rule (rename the table, add the 25 `pid5bf_` lines after `hitopbr_45`). The only difference was those 26 lines.
- AC5: tests/link-instruments.spec.js (21 tests) passed. LI1 adds, moves and removes rows up to three. LI2 asserts that one row writes `instrument` alone and two or three rows write `instruments` in row order. LI4 builds links of two and of three instruments and opens each at "Part 1 of N". It reloads each on the builder through `c` (and `z` with a question) and asserts the same rows and the same rebuilt configuration. An opened link with one instrument fills one row.
- AC6: `tests/testthat/fixtures/responses-multi-page-shuffled.csv` equals hitop-form's `tests/fixtures/` copy with its CR bytes removed (diff empty). hitop-form saved that copy at dd7a241 under the HiTOP-BR, the PID-5-BF and the whole HiTOP-SR, with shuffle and the question `age`. IR3 in the suite run above checks it against a fresh walk's header. `test-read_form_responses.R`, run alone, gave 0 failures. Its new test reads the file and works out each instrument's answers from the `item_order` cell. It scores the HiTOP-BR, PID-5-BF and HiTOP-SR groups, each chosen by stem, against values computed from the keying tables.
- AC7 (not ticked): The hitop-form README section "Give more than one instrument" and the article section "Several instruments in one session" were read. Each states the field, the order of screens, the file shape and scoring by stem. NEWS names the field. hitop `devtools::check()`: 0 errors, 0 warnings, 0 notes. `cairn_validate` exits 0 (24 advisory warnings). The hitop-form CI run does not exist yet, because the PR opens at step 8. The box stays open until that run is green.

Consistency gate: `devtools::document()` gave no diff. README.Rmd and README.md are not in the diff. `pkgdown::check_pkgdown()` found no problems. NEWS has the entry. The one added file is under `tests/testthat/fixtures/`, so no `.Rbuildignore` entry is needed. No DESIGN principle changed, so `cairn_impact` was skipped.

Findings from 3 fresh reviewers ([O] diff-bug, [S] blame-history, [S] prior-review), merged and ranked. Dispositions are logged after the gate.

- F1: `checkInstruments()` (form.js:94) accepts an entry that is a nested list. The review ran the check in node and saw `["hitopbr", ["hitopbr"]]` and `["pid5", ["pid5bf"]]` pass. The page refuses such a link later, at the export check, with a message that does not name `instruments`. The builder's prefill loads it without a refusal. This is an AC1 failure: a repeated name and two PID-5 forms are not refused by name.
- F2: The page and the builder refuse a list of all three PID-5 forms with the message "two forms of the PID-5". The review ran the page's check in node to see this. No test covers three.
- F3: NEWS.md:69 (the M139 entry) still says the page does not yet write a file of several instruments. The new entry in the same section says it does.
- F4: DESIGN Known issue 12 reopens "if the page fields such a pair". A HiTOP-SR module of items 1 to 45 beside the HiTOP-BR is such a pair under shuffle. So is one of items 1 to 25 beside the PID-5-BF. Neither the milestone nor DESIGN records this.
- F5: data-raw/form_multi_fixtures.R:6 still says the page does not write this shape.
- F6: The hitop-form README table of spec files lacks the 5 new specs.
- F7: Six builder prefill refusals are untested: an unknown name, a repeated name, a number, null, an object and an empty list. The builder's three-PID-5 refusal is also untested.
- F8: No network test walks a list link. The README network row still says "the one export fetch", while form.js now says "the export fetches".
- F9: The unload guard was rewritten for several parts, and its one test covers a typed before-question only. The M137 candidate row's "when next edited" trigger fired.
- F10: link.html's prefill was edited, and the M135 row's gap (the submit handler waits on the `z` prefill) is still open. That row's trigger fired.
- F11: Single-instrument wording remains in 3 places. In the hitop-form README, it is in "Where the file lands", the Sheets section, "What the participant sees" and "Nine example files". It is also in link.html:95 and in the article at lines 19, 71 and 98.
- F12: A browser reload of the builder can restore the text fields and reset the instrument rows to one HiTOP-SR row (not run, inferred).
- F13: No test asserts that a single-instrument start screen shows no "Part 1 of 1".
- F14: `start()` now scrolls to the top on single-instrument links too.
- F15: The builder's submit checks now refuse instrument faults before a missing study name.
- F16: The where-answers-go notice shows on the first start screen only (as AC2 states).
- F17: Comment and text wrapping: form.js lines 9 to 12 and 1077, the NEWS entry, and a test comment. The form.js header's import list omits the new link.html imports.
- F18: Move up on the first row and Move down on the last are enabled and do nothing. Their aria-labels do not contain the visible text, as in the question editor.
- F19: The hitop-form fixture README cites dd7a241, which the squash merge leaves off main. Earlier rows do the same.

Gate dispositions (2026-09-28, Jeff at the step-7 gate): the milestone returns to implementation. AC1 is unticked, because F1 shows it failing.

- Fix now: F1, F2 and F7 go to T9. F8, F9 and F13 go to T10. F3, F5, F6, F11 and F17 go to T11. F12 goes to T12.
- F4: accepted again. DESIGN Known issue 12 now says that the page can field such a pair.
- Follow-up: F10 is a dated note on the `z` link gaps candidate row. F18 joins the question-screen gaps row.
- Rejected: F14 is harmless. No test or claim depends on the check order of F15. F16 is the plan's intent (AC2). F19 is the practice of the earlier fixture rows.

### Second pass (after defect return 1)

Fresh runs on 2026-09-28 at hitop-form bd65327. Both branches contain origin/main (no merge needed). hitop-form full Playwright suite: 720 of 720 passed in 1.9 minutes, default workers, no reruns.

- AC1: tests/instruments.spec.js (24 tests) passed. Its 19 field and name probes now include the entries `["hitopbr"]`, `["pid5bf"]`, `null` and `{"hitopbr":1}`, a name that is not text, and all three PID-5 forms. Each asserts the full page-side message. The 5 module probes cover a module beside a list without `hitopsr` and 4 faulty modules beside a list with it. tests/link-instruments.spec.js (31 tests) passed. It asserts by message the builder-side refusals of a repeated row, the three PID-5 pairs, three PID-5 rows and the two module rules. It also asserts 14 refusals of an opened link, the nested entries among them, and one of an opened `z` link. form.js:99 now refuses any entry that is not a string as an unknown name. The single-`instrument` specs passed in the same run.
- AC2: tests/instruments-walk.spec.js (14 tests) passed in the same run. Its 10 first-pass tests are unchanged in what they assert. The 4 new tests are the three leave-page tests and the check of no part line on a single-instrument link. network.spec.js (9 tests) passed, and N9 walks a list link of two instruments.
- AC3: tests/instruments-row.spec.js (11 tests) passed in the same run, with the assertions the first pass recorded.
- AC4: tests/instruments-store.spec.js (6 tests) passed in the same run. The two SQL fixtures are unchanged since the first pass.
- AC5: tests/link-instruments.spec.js (31 tests) passed in the same run. LI1, LI2 and LI4 and the one-row prefill test assert what the first pass recorded.
- AC6: the hitop fixture still equals hitop-form's copy with its CR bytes removed (diff empty). The hitop result is in the AC7 line.
- AC7 (not ticked): hitop `devtools::check()` at a5ba9b01 gave 0 errors, 0 warnings and 0 notes, and its tests passed. `cairn_validate` exits 0 (25 advisory warnings). The README section and the article section still state the field, the order of screens, the file shape and scoring by stem. NEWS names the field and no longer says that the page cannot write the file. The hitop-form CI run does not exist yet, because the PR opens at step 8. The box stays open until that run is green.

Consistency gate: `devtools::document()` gave no diff. `pkgdown::check_pkgdown()` found no problems. README.Rmd is not in the diff, and no file was added outside `tests/testthat/fixtures/`. No DESIGN principle changed, so `cairn_impact` was skipped.

Second-pass findings from 3 fresh reviewers ([O] diff-bug, [S] blame-history, [S] prior-review), merged and ranked. The reviewers found F1, F2, F7, F8, F9 and F13 closed, and no acceptance criterion failing. Both PR-comment probes were empty.

- G1: The article's `item_order` bullet (online-collection.Rmd:229) says each group is all of that instrument's item numbers. Under a list with a HiTOP-SR module, that group holds only the module's items. The page writes that now, and the reader accepts it.
- G2: The rule "Deploy one sheet and script per form" is not stated for list links. Two links that list the same instruments in different orders and post to one sheet give rows whose `instrument` cell disagrees with the column groups. The README Sheet section and the article do not say so.
- G3: F11 wording leftovers. The article at line 508 and the hitop-form README at lines 18, 128 and 149 name "the instrument" as one. So do playwright.config.js:5 ("one JSON file") and link.html:787 ("the export fetch").
- G4: T12 probed F12 in Chromium only, and no test, README note or candidate row records that limit.
- G5: The question-screen gaps row did not get a note that its trigger fired, although M140 edited the question-screen navigation. The Move up and Move down clause sits under a trigger that a builder edit does not fire.
- G6: `parseLink()` accepts an array in the single `instrument` field (form.js:707), and the export check refuses it later without naming the field. The same code is on main.
- G7: F17 rewrap incomplete. NEWS.md:14 is 87 characters and NEWS.md:67 is a short orphan line. form.js:14, README.md:441, the article near line 74 and the network.spec.js header have ragged lines. The link.html:635 comment reads "A field of another type than its control takes / is skipped".
- G8: The part line, the file name of a list and the notice on the first start screen rest on the milestone-local M140-D1 only, with no D-entry.
- G9: `checkInstruments()` quotes a refused entry in full, so a nested entry in a long `z` link fills the message.
- G10: The builder words one fault two ways: "instrument 2" and menu names at build, "entry 2" and stems for an opened link.
- G11: W1 waits a fixed 1000 ms before it asserts that no screen shows (instruments-walk.spec.js:84).
- G12: `instruments` beside `instrument: null` is refused as a link with both fields.
- G13: The notice on the first start screen only (F16) still meets the intent of b0c38a3 and 21a1d1c, which put it before the first answer.
