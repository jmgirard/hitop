# M140: A study link can field two or more instruments in one hitop-form session

- **Status:** in-progress
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

- [ ] AC1: `parseLink()` accepts `instruments` as a list of 2 or 3 distinct instrument names that the page knows, in the order the page gives them. It refuses by name `instruments` beside `instrument`, a list that is not an array, and a list of fewer than 2 names. It also refuses a repeated name, an unknown name, and a list that holds two of `pid5`, `pid5sf` and `pid5bf`. If the list holds no `hitopsr`, it refuses a `module`. If the list holds `hitopsr`, it checks a `module` against it. A link with `instrument` behaves as on main. Before it builds a link, link.html refuses the faults its controls can produce. Tests assert the message of each refusal on each side where it can occur.
- [ ] AC2: The page fetches every export in the list before it shows the first screen. A refusal of an export names the instrument. Each instrument has its own start screen, with its title, version line, instructions, and item and page counts, followed by its item pages. Item positions and page labels count within each instrument. If the link gives no participant identifier, the start screen of the first instrument asks for it. No later start screen asks. The notice of where the answers go shows on the first start screen only. The first item page of each instrument carries no Back. Each instrument's items start unanswered. The order of screens is the consent screen of M135, the `before` screen of M137, each instrument in link order, then the `after` screen. Tests walk links with two and with three instruments, with and without `consent` and `questions`. A test asserts that no radio is checked on the first page of a second instrument whose item numbers overlap the first's.
- [ ] AC3: The row and the file follow D-080 as M139 reads it. The `instrument` cell holds the stems in link order, joined by single spaces. The `form_build` cell holds the build date of each export in the same order, joined by single spaces. The item columns come in one group per instrument, in link order. Under shuffle, each instrument's items are shuffled among that instrument's items, and none moves to another instrument's pages. The `item_order` cell then holds one group per instrument. The `q_` columns of M137 follow the item columns. Tests answer each instrument with a different pattern. They assert the row and the file with shuffle off and on, with `prolific: true`, and with `questions`.
- [ ] AC4: For an `instruments` link, the Supabase SQL from the builder has the lead columns first. Then it has one `integer` column per item of each instrument in the order of AC3, then the `q_` columns. The webhook send and the Supabase send carry every column. Tests compare the SQL with a committed fixture written by rule, byte for byte, and read the keys from the recording server.
- [ ] AC5: link.html lets the researcher choose one instrument, as on main, or build an ordered list of two or three. A single choice writes `instrument`, and a list writes `instruments`. A link loaded through `?c=` or `?z=` fills the same choice. Tests build, open and reload a link with two and with three instruments.
- [ ] AC6: One file that the page saved under a link with three instruments, shuffle on and one question is committed in hitop as a test fixture. `read_form_responses()` reads it, and each instrument's columns, chosen by stem, score with its scoring function.
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
- [ ] T7: Write the README section "Give more than one instrument", the article section and the NEWS entry.
- [ ] T8: Get hitop-form CI green on its PR. Run hitop `check()` and `cairn_validate`.

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

## Decisions

- M140-D1 (2026-09-28, implement gate): link.html's instrument choice becomes a list editor of up to three rows. Each row is a menu with Move up, Move down and Remove, and an "Add an instrument" button adds a row. One row writes `instrument`, and two or three write `instruments`. A saved file of several instruments is named by the stems joined by "-". The page's name rule already writes the space-joined `instrument` cell so. Under an `instruments` link each start screen shows "Part n of N" above the instructions. Chosen by Jeff over three fixed menus, a file name of "multi", and no part line.

## Review
