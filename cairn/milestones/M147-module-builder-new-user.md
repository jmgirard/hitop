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
- [ ] AC2: In the scale picker, each scale checkbox's accessible name ends with "<n> items", where n is the scale's item count. The test checks all 76 scales. The installed package gives each scale a definition. Each scale then has a visible button that opens its definition by click and by keyboard, with no hover. A filter that matches no scale shows "No scales match". Smoke-test steps assert each fact.
- [ ] AC3: In step 2, choose each of the four formats in turn, with its settings closed. The visible text between the format cards and the download button holds at most 60 words. Closed settings summary lines count. A mouse click on each control that changes the step leaves the new step's heading with a computed `outline-style` of `none`. A keyboard press on the same control shows a focus outline. Smoke-test steps assert both facts for each control.
- [ ] AC4: Each format has one name. Its card title, its download button and its build status all use that name. For Word, Qualtrics and REDCap, the zip file's README.txt title uses it too. Each control that changes the step holds the name of its target step, as the step bar shows that name. A smoke-test step reads these places against one table.
- [ ] AC5: Save the module file from the Online form card. A panel headed "Next: make the study link" then shows a button. The button's address opens the Study Link Builder, and its `c` value decodes to the saved file's module. A change to the scale selection or the format removes the panel. A smoke-test step asserts each fact. On the Study Link Builder, the researcher can choose the module file or paste its text. A hitop-form test chooses `tests/fixtures/module-plain.json` as a file. It asserts that the built link equals the link built from the pasted text.
- [ ] AC6: `node tests/prose.mjs --text` writes every string a visitor can read. That output holds none of the D-083 retired terms, apart from R output shown in the "Technical details" log and literal URLs. In `README.md`, the section on using the page comes first. A "For developers" heading follows and holds the verification notes and test records.
- [ ] AC7: The hitop-builder smoke test and `npm run prose` pass locally and on the PR's CI. `npm run plants` passes locally, and each new smoke assertion has a plant that turns it red. The hitop-form suite passes locally and on its PR's CI.

## Coverage

- AC1 → T1, T7
- AC2 → T2, T7
- AC3 → T3, T7
- AC4 → T4, T7
- AC5 → T5, T6, T7
- AC6 → T4, T8
- AC7 → T6, T7, T8

## Tasks

- [x] T1: Shorten the header text (`index.html` lines 443-455). Move the host sentence and the Log section (lines 685-688) into a closed "Technical details" `<details>`. Open it on a failure, and reword the failure statuses (lines 1594, 1665-1671, 1904) to name it.
- [x] T2: In the picker (lines 1052-1266), add the item-count text to each checkbox's accessible name. Replace the hover popup (lines 1099-1160) with a definition button. Show "No scales match" for an empty filter.
- [x] T3: Cut the step-2 notices (lines 502-505, 589-664) to the 60-word limit. Move the zip file detail into the README.txt that `bundleReadme()` writes. Stop the mouse-click outline on the step heading (line 380, `showStep` at line 1037). Keep the keyboard focus outline.
- [x] T4: Set one name per format in `FORMATS` and use it on the card, the button, the status (line 1434) and `bundleReadme()` (lines 1323-1373). Name each step control by its target step (lines 472-501, 680). Apply the D-083 names to every string that `tests/prose.mjs` lists.
- [x] T5: Replace the one-line link under the button (lines 676-678, 1373-1380, 1482) with the "Next: make the study link" panel.
- [x] T6: In hitop-form `link.html`, add a file choice beside the module text box that fills it from a chosen file. Add the hitop-form test for AC5, and run that suite locally and on its PR.
- [x] T7: Add the smoke-test steps for AC1 to AC5 and a plant for each one. Update the `tests/prose.mjs` ledger. Run the smoke test, prose and plants locally, and the smoke test and prose on the PR.
- [x] T8: Reorder `README.md` into a use section and a "For developers" section. Take before and after screenshots at 375px and 1280px for Jeff's look at the merge gate.

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

## Decisions

- M147-D1 (2026-09-30, implement gate): The four formats are named "Word form", "Qualtrics file", "REDCap dictionary" and "Online form". The card's second line keeps the file type. The steps keep the names "Choose scales" and "Choose a format and download". The Continue button reads "Next: Choose a format and download", and the Back button and the tally link read "Back: Choose scales". A "Definition" button on each scale row shows the definition as a line under the row, and a second press hides it. The row's count reads "<n> items". The hand-off button opens the Study Link Builder in a new tab. Chosen by Jeff. Rejected: bare system names, because a sentence needs a noun after them. Rejected: "Download" as the second step's name, because it does not say that a format is chosen there. Rejected: a floating popup, which needs script placement and a scroll handler. Rejected: the same tab, where the page loads R again and loses the ticks.

## Review
