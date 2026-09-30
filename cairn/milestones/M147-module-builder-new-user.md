# M147: Module Builder: short start, labelled picker, clear hand-off

- **Status:** in-progress
- **Priority:** normal
- **Depends on:** M146
- **Driving RR:** —
- **Principles touched:** GP3, IP1
- **Resolves:** —
- **Surface tier:** user-facing — the web page that builds HiTOP-SR modules and its hand-off to the Study Link Builder
- **Branch/PR:** m147-module-builder-new-user, companion: /Users/jmgirard/github/hitop-builder m147-module-builder-new-user, companion: /Users/jmgirard/github/hitop-form m147-module-builder-new-user

## Goal

A researcher who opens the Module Builder for the first time reads a short start and understands the scale list. The download button sits beside the format cards, and one step leads to the Study Link Builder.

## Scope

**In:** In hitop-builder `index.html`, the milestone changes six things. The header gets short. The host list and the R log move into a closed "Technical details" section. The scale picker gets labels, a definition button and an empty-filter message. Step 2 gets short text and no focus outline on a mouse click. The format names become the same everywhere, and so do the step names. The Online form card gets a hand-off panel. The bundle README.txt takes the D-083 names. hitop-builder `README.md` puts its use section first. In hitop-form `link.html`, the module can be chosen as a file. Scale names, item counts and definitions stay as the package gives them (IP1).

**Out:**
- M146 takes the Study Link Builder layout. M148 takes the participant form page.
- A scale list inside the Study Link Builder was not chosen (work log, M146).
- The picker's client-facing text and subscale rows stay in their candidate row.
- The missing generator arguments stay in their candidate row.

## Acceptance criteria

- [ ] AC1: The page text between the `<h1>` and the status line holds at most 50 words. The host list and the R log sit in one `<details>` element named "Technical details". It is closed during the load and after the page is ready. A load or build failure opens it, and the failure status points to it by name. A smoke-test step asserts these facts. It holds the loading state by delaying the webR request.
- [ ] AC2: In the scale picker, each scale checkbox's accessible name ends with "<n> items", where n is the scale's item count. When the package the page loads gives every scale a definition, each scale's row has a visible button that opens its definition by click and by keyboard, with no hover. A filter that matches no scale shows "No scales match". Smoke-test steps assert the name form on every listed scale, n on two named scales, the button's click, keyboard and hover behavior on every row, and the empty-filter line. Review reads n on every row against the package the page loads.
- [ ] AC3: In step 2, choose each of the four formats in turn, with its settings closed. The visible text between the format cards and the download button holds at most 60 words. Closed settings summary lines count. A mouse click on each control that changes the step leaves the new step's heading with a computed `outline-style` of `none`. A keyboard press on the same control shows a focus outline. Smoke-test steps assert both facts for each control.
- [ ] AC4: Each format has one name. Its card title, its download button and its build status all use that name. For Word, Qualtrics and REDCap, the zip file's README.txt title uses it too. Each control that changes the step holds the name of its target step, as the step bar shows that name. A smoke-test step reads these places against one table.
- [ ] AC5: Save the module file from the Online form card. A panel headed "Next: make the study link" then shows a button. The button's address opens the Study Link Builder, and its `c` value decodes to the saved file's module. A change to the scale selection or the format removes the panel. A smoke-test step asserts each fact. On the Study Link Builder, the researcher can choose the module file or paste its text. A hitop-form test chooses `tests/fixtures/module-plain.json` as a file. It asserts that the built link equals the link built from the pasted text.
- [ ] AC6: `node tests/prose.mjs --text` writes every string a visitor can read. That output holds none of the D-083 retired terms, apart from R output shown in the "Technical details" log and literal URLs. In `README.md`, the section on using the page comes first. A "For developers" heading follows and holds the verification notes and test records.
- [ ] AC7: The hitop-builder smoke test and `npm run prose` pass locally and on the PR's CI. `npm run plants` passes locally, and each new smoke assertion has a plant that turns it red. The hitop-form suite passes locally and on its PR's CI.

## Coverage

- AC1 → T1, T7, T9
- AC2 → T2, T7, T9
- AC3 → T3, T7
- AC4 → T4, T7, T11
- AC5 → T5, T6, T7, T9, T12
- AC6 → T4, T8, T11
- AC7 → T6, T7, T8, T9, T11, T12

## Tasks

- [x] T1: Shorten the header text (`index.html` lines 443-455). Move the host sentence and the Log section (lines 685-688) into a closed "Technical details" `<details>`. Open it on a failure, and reword the failure statuses (lines 1594, 1665-1671, 1904) to name it.
- [x] T2: In the picker (lines 1052-1266), add the item-count text to each checkbox's accessible name. Replace the hover popup (lines 1099-1160) with a definition button. Show "No scales match" for an empty filter.
- [x] T3: Cut the step-2 notices (lines 502-505, 589-664) to the 60-word limit. Move the zip file detail into the README.txt that `bundleReadme()` writes. Stop the mouse-click outline on the step heading (line 380, `showStep` at line 1037). Keep the keyboard focus outline.
- [x] T4: Set one name per format in `FORMATS` and use it on the card, the button, the status (line 1434) and `bundleReadme()` (lines 1323-1373). Name each step control by its target step (lines 472-501, 680). Apply the D-083 names to every string that `tests/prose.mjs` lists.
- [x] T5: Replace the one-line link under the button (lines 676-678, 1373-1380, 1482) with the "Next: make the study link" panel.
- [x] T6: In hitop-form `link.html`, add a file choice beside the module text box that fills it from a chosen file. Add the hitop-form test for AC5, and run that suite locally and on its PR.
- [x] T7: Add the smoke-test steps for AC1 to AC5 and a plant for each one. Update the `tests/prose.mjs` ledger. Run the smoke test, prose and plants locally, and the smoke test and prose on the PR.
- [x] T8: Reorder `README.md` into a use section and a "For developers" section. Take before and after screenshots at 375px and 1280px for Jeff's look at the merge gate.
- [ ] T9: Review return. Add three smoke steps. A build failure that the test forces opens "Technical details" (AC1). The Definition button's click, keyboard and hover checks run on every row (AC2). Panel removal is read apart from the anchor (AC5). A21 also reads whether the section is rendered. Add a plant for each.
- [x] T10: Add a hitop NEWS.md entry for the Module Builder changes.
- [ ] T11: Review findings in hitop-builder. Restore the REDCap inner-zip warning and give the Online form's file-name note a home. Announce "No scales match". Make `prose.mjs --compare` keep the retired-name exit. Fix the stale comments and labels. Narrow the README focus sentence to Chromium. Add plants for A24's hover and A28's README title.
- [ ] T12: Review findings in hitop-form `link.html`. Close the gap where "Make the link" runs during the file read, announce the chosen file, and rewrap the two long README lines.

## Work log

- 2026-09-29: created by /milestone-plan, with M146 and M148. The criteria audit ran in full mode with M146's reader, and each M147 finding was repaired as suggested. The Online form has no settings and no README.txt. The step bar is hidden during the load, so AC1 counts from the `<h1>` instead. The log's failure statuses now point to "Technical details". AC4 names one target per step control. Plants run locally, because CI does not run them.
- 2026-09-30: M146 review pass 2, finding 7, filed here at Jeff's gate. The Study Link Builder intro no longer links the Module Builder. The module route sits only in the closed "Item order and HiTOP-SR module" section. The plan gate for this milestone's hand-off reads it.
- 2026-09-30: implement started. Branch `m147-module-builder-new-user` cut in hitop, hitop-builder and hitop-form. Implement gate: Jeff took the recommended option on all four questions (M147-D1).
- 2026-09-30: T1 done (hitop-builder 1st commit). Header is 39 words. Every failure goes through a new `showFailure()`, which opens "Technical details" and adds a sentence that names it. No visitor setting can make a build fail, so the smoke step for AC1 drives a load failure. The build failure uses the same function. Smoke test green.
- 2026-09-30: T2 done (hitop-builder 2nd commit). Correction to the T1 line: the header is 40 words, not 39. Each row is a `.row` with the label and a "Definition" button outside it. The definition is a hidden `<p>` under the row, still named by the checkbox's `aria-describedby`. The hover, Escape and scroll handlers are gone. The smallest scale has 3 items, so the count always reads "items". An empty filter hides the list and shows "No scales match the filter." Prose ledger 21 sites, smoke green.
- 2026-09-30: T3 done (hitop-builder 3rd commit). One zip note and one Online form note replace the two notices and two hints. A "Choose at least one scale" hint sits under the button, outside the counted span. README.txt gains the naming paragraph, and its title line takes the format name now (AC4's README part). The heading ring moved from `:focus` to `:focus-visible`. Smoke green. The word counts and the focus rule are measured in T7.
- 2026-09-30: T4 checkpoint (hitop-builder 4th commit, not ticked). `FORMATS[].label` is the one name. Cards, buttons, the new `buildStatus()` and the README.txt title use it. The step controls read "Next: Choose a format and download" and "Back: Choose scales". `prose.mjs` now checks D-083's twelve patterns on every run and exits 1 on a hit. Log code, template holes, URLs and README code spans are set aside. It found 21 hits: 4 in the hand-off text T5 replaces, and 17 in README.md. Minor amendment: README.md's names land in T8's rewrite after T5, so T4 is ticked with T8's README commit.
- 2026-09-30: T5 done (hitop-builder 5th commit). `showNextStep()` clones the whole panel from a template, and `removeNextStep()` takes it out of the document on the same four triggers as before. The link is drawn as the primary button and opens a new tab. The smoke test's A14 to A19 now read the panel. Smoke green, and screenshots at 1280px and 375px look right.
- 2026-09-30: T6 done (hitop-form 1st commit). "Choose the module file" sits under the "Module file" box. It fills the box with the file's text, rewrites the section summary, and empties itself. `heldLabels()` skips file inputs. New `tests/link-module-file.spec.js`: MF1 is AC5's same-link test, and MF2 covers the summary and a second choice. Two plants, the missing summary rewrite and the missing reset, each turned MF2 red. The full suite passed locally, 773 tests. README gains a sentence and a test row. The PR's CI run is at review.
- 2026-09-30: T7 done (hitop-builder 6th commit). The smoke spec gains A20 to A29 and a second test, which refuses `webr.mjs` for A22. A20 holds the `webr.mjs` request to read the loading state. A28 adds a Qualtrics and a REDCap build to read their statuses and README.txt titles. Twelve new plants (aa to al), and plants m, p, r to z moved to the new text. `npm run plants`: unplanted passed, 39 of 39 plants red, all 29 assertions covered. The full local run passed with 4 tests. Measured: 40 header words while loading; step 2 at 48, 44, 45 and 41 words for Word, Qualtrics, REDCap and Online.
- 2026-09-30: T8 and T4 done (hitop-builder 7th commit). README.md starts with "Using the page", and "For developers" follows with "How it works", "Verification notes" and "Tests and repository layout". Every dated check moved to the verification notes. The new names are used throughout, so `npm run prose` reports no retired name in 176 passages. Before and after screenshots at 375px and 1280px (4 page states each) are in hitop-builder's gitignored `playwright-report/m147/`.
- 2026-09-30: claim audit: 240 claims read, 9 corrected, hitop-builder README.md, index.html, tests/smoke.spec.js, tests/plants.mjs (hitop-builder commits da853e2 and 3ea9636). The hitop diff adds nothing outside `cairn/`, so the reader read both companion diffs instead. Corrected: the study link's address carries the module to GitHub Pages; the load takes about twenty seconds each for R and the package; the module file does not score by itself; the Word and Qualtrics README.txt differences; the hand-off control is a link drawn as a button; A24 drives the first row only; three stale comments. The re-read found all nine hold, plus one wording nit (the tally control is a button drawn as a link), fixed in 3ea9636.
- 2026-09-30: implement complete. hitop-builder full local run passed (4 tests), `npm run prose` clean, `npm run plants` OK (39 of 39). hitop-form passed 773 tests locally. No hitop R code changed. Status set to review.
- 2026-09-30: review pass 1 returned the milestone to in-progress (defect return 1). What failed: AC1, because no smoke step asserts that a build failure opens "Technical details". AC2, because the smoke test holds `<n>` to the package on 2 rows and does not pin 76 rows. It also drives the Definition button on the first row only. AC5, because A15 and A18 count anchors and cannot see a panel left behind. The consistency gate, because NEWS.md has no entry. The page itself met AC1, AC2 and AC5 in a one-off check. Findings and proposed dispositions are in the Review section.
- 2026-09-30: implement resumed after review pass 1. Gate: Jeff chose to narrow AC2 and to fix the return reasons plus every finding proposed as fix-now. The two proposed follow-ups wait for triage at the merge gate.
- re-audit: AC2 (full) — first reader, on the text "The test checks every listed scale for that form, and checks n against the package on two named scales": the sentence bound the test, not the page. The closing sentence claimed more than the test checks, the button was opened on one row only, and "against the package" overstated hand-listed counts.
- re-audit: AC2 (full) — second reader, on the fixed text: the per-scale definition clause widened, because the page shows buttons only when every scale has a definition. The test clause lacked that condition, "the button on every row" read as presence only, and both known counts have one digit.
- 2026-09-30: AC2 amended at Jeff's gate after the second re-audit (the stop for AC2). "The test checks all 76 scales" left, because hitop-builder pins no scale count. The smoke test now covers the name form and the button's behavior on every row, and n on two named scales. Review reads n on every row. Minor amendment: T9 to T12 added, and the Coverage lines follow them.
- 2026-09-30: T9 and T11 checkpoint (hitop-builder commits 3cbb14c and a99e999, not ticked until the plant run). New A30 forces a build failure by making `URL.createObjectURL` throw. A24 drives all 76 rows, and A15, A16 and A18 count panel headings apart from the link. A21 reads the section on show. Plants am to ar added. The page has a REDCap line again, and step 2 now holds 55 words for REDCap and 56 for the Online form. Smoke green (2 tests), prose clean (177 passages).
- 2026-09-30: T10 done. hitop NEWS.md has a Module Builder entry under New features.

## Decisions

- M147-D1 (2026-09-30, implement gate): The four formats are named "Word form", "Qualtrics file", "REDCap dictionary" and "Online form". The card's second line keeps the file type. The steps keep the names "Choose scales" and "Choose a format and download". The Continue button reads "Next: Choose a format and download", and the Back button and the tally link read "Back: Choose scales". A "Definition" button on each scale row shows the definition as a line under the row, and a second press hides it. The row's count reads "<n> items". The hand-off button opens the Study Link Builder in a new tab. Chosen by Jeff. Rejected: bare system names, because a sentence needs a noun after them. Rejected: "Download" as the second step's name, because it does not say that a format is chosen there. Rejected: a floating popup, which needs script placement and a scroll handler. Rejected: the same tab, where the page loads R again and loses the ticks.

## Review

### Pass 1 (2026-09-30): returned to in-progress

All three branches were level with their `origin/main`, so no merge was needed. A one-off Playwright script drove the local page. It tested the page facts that the smoke test covers only in part. The script was not committed.

- AC1: not met. The smoke test passed: 40 header words while loading (A20). "Technical details" is closed while loading and at "Ready." (A20, A21), and a refused `webr.mjs` opens it and names it (A22). The one-off script forced a Word build failure by making `URL.createObjectURL` throw. The section went from closed to open, and the status read "The Word form build failed. The log under "Technical details" below says more." So the page meets the fact, but no smoke step asserts the build-failure fact the criterion names.
- AC2: not met. A23, A24 and A25 passed. The one-off script found 76 rows and 76 package scales. Each checkbox name equals the name plus the local hitop 0.2.0 `itemNumbers` count, with 0 mismatches. On all 76 rows, a click and Enter open the definition, a second click and Space close it, and a hover opens none. The smoke test holds `<n>` to the package count on 2 rows only and does not pin 76 rows. It drives the button by click and keyboard on the first row only.
- [x] AC3: smoke passed. Step 2 counts are 48 (Word), 44 (Qualtrics), 45 (REDCap) and 41 (Online) words, all with settings closed (A26). All five step controls give `outline-style: none` after a mouse click and a shown outline after Enter (A27).
- [x] AC4: smoke passed A28. All four formats' card, button and status match the table, as do the Word, Qualtrics and REDCap README.txt titles. The step bar reads the two step names, and each of the five step controls holds its target's name.
- AC5: not met. A13, A14, A15, A18, A19 and A29 passed. A14 decodes `c` to the saved file's module. hitop-form MF1 and MF2 passed. But A15 and A18 count the Study Link Builder anchors, and `readLinkState()` finds panels only through a surviving anchor. If a removal deletes only the anchor, the "Next: make the study link" panel stays and both steps still pass. So no smoke step asserts that the panel goes.
- [x] AC6: the `--text` flag takes an output path. `node tests/prose.mjs --text <file>` wrote 158 passages and exited 0, reporting "retired names: none in 176 passages". A separate grep of D-083's twelve patterns over that output found one hit. It is the R call `descriptor = desc_path` in a log line, which is R output in the log. Two `descriptor` hits in `README.md` are R argument code spans. `README.md` opens with "Using the page", and "For developers" holds "Verification notes" and "Tests and repository layout".
- AC7: not complete. Local smoke passed (2 tests) and `npm run prose` exited 0. The hitop-form suite passed locally (773 tests). No PR exists yet, so the CI parts wait for the merge gate. The first plant run shared the machine with the R check and the one-off script, and its unplanted copy went red on A16 and A28. A second run alone passed the unplanted copy. It was stopped after that, because the return was already certain.

Consistency gate: `cairn_validate` exit 0 (24 advisory warnings, none new). No principle text changed, so `cairn_impact` was skipped. `devtools::document()` left no diff and `pkgdown::check_pkgdown()` found no problems. `devtools::check()` gave 0 errors, 0 warnings and 0 notes. No new top-level files. **Failed: NEWS.md has no entry for the Module Builder changes.** M146 pass 1 was returned for the same gap.

Review findings from the three-lens fan-out. All are logged here, and none is triaged yet. Jeff triages them at the next gate.

- Diff-bug 1: the REDCap download's inner-zip warning left the page (T3). README.txt still says to upload the inner zip, but under the new names "this zip file" can read as the outer download. Proposed: fix now.
- Diff-bug 2: A15 and A18 cannot see a panel left behind without its anchor. Return reason under AC5.
- Diff-bug 3: no smoke step for a build failure. Return reason under AC1.
- Diff-bug 4: a throw after R starts (for example `library(hitop)`) reports "R did not start." The wording is older than M147. Proposed: follow-up.
- Diff-bug 5 and blame 3: in `link.html`, a press of "Make the link" during the file read builds a link from the old box text. The later fill does not count as a field change. Proposed: fix now.
- Diff-bug 6 and blame 2: a chosen module file is not announced, and the file control shows "No file chosen" again at once. The questions loader beside it announces "Loaded n questions". Proposed: fix now.
- Diff-bug 7: A23 holds `<n>` to the package on 2 rows and does not pin 76 rows. Return reason under AC2.
- Diff-bug 8: A21 does not read whether "Technical details" is rendered. Proposed: fix now (one field).
- Diff-bug 9: A27's keyboard half cannot go red on the page's CSS, and only Chromium runs. Proposed: follow-up, with Known issue 13.
- Diff-bug 10: some sub-claims have no plant (the A24 hover, the A23 known counts, the A28 card, button and README title parts). Proposed: fix now for the A28 README title and the hover.
- Diff-bug 11 and blame 7: "No scales match the filter." is not announced. Proposed: fix now.
- Diff-bug 12: `prose.mjs --compare` exits 0 over a retired-name hit. Proposed: fix now (one line).
- Diff-bug 13: the checkbox name has no text space between the name and the count. It is older than M147, and Chromium reads it correctly. Proposed: reject, older than M147 and not seen to fail.
- Diff-bug 14: 76 Definition buttons add 76 tab stops. Proposed: reject, the M147-D1 design.
- Diff-bug 15: the "Choose at least one scale" hint sits below the button, outside the counted words. Proposed: noted, recorded in the T3 work-log line.
- Diff-bug 16: the hand-off "button" is a link drawn as a button. Proposed: reject, a link is the right role for opening a page.
- Diff-bug 17: a later successful build does not close "Technical details". Proposed: reject, not a criterion.
- Diff-bug 18 and blame 8, prior-review 5: stale comments ("beside the tally", "scoring file", "bundle"). Proposed: fix now.
- Blame 1: the Online form's file name and name-collision note has no home, because the Online form has no README.txt. Proposed: fix now.
- Blame 4 and prior-review 2: the heading ring now depends on the browser's `:focus-visible` rule. The README states it in general terms from Chromium alone. Proposed: fix now (narrow the README sentence to Chromium).
- Blame 5: the page no longer says the format cards are off during a build. Proposed: noted, the README says it.
- Blame 6: the host list is now inside the closed section. Proposed: reject, the plan's AC1.
- Prior-review 1: no NEWS entry. Return reason (consistency gate).
- Prior-review 3: the same as diff-bug 5.
- Prior-review 4: two hitop-form README lines are 82 characters wide. Proposed: fix now.
- Prior-review 6: no plant for A24's hover clause. The same as diff-bug 10.
