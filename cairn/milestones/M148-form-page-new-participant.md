# M148: Online form: text for participants, progress, and a way on from errors

- **Status:** in-progress
- **Priority:** normal
- **Depends on:** M146
- **Driving RR:** —
- **Principles touched:** GP3, IP1
- **Resolves:** —
- **Surface tier:** user-facing — the page participants fill in from a study link
- **Branch/PR:** m148-form-page-new-participant, companion: /Users/jmgirard/github/hitop-form m148-form-page-new-participant

## Goal

A participant who opens a study link for the first time reads text written for them and always knows where they are in the form. Every error or saved-file screen gives them a next step.

## Scope

**In:** In hitop-form `index.html` and `form.js`, the milestone changes six things. The error screen speaks to the participant, and its technical detail moves into a closed section for the study team. The build date and package version leave the participant screens. The identifier screen gets a hint and phone-safe input. The item pages get progress, a visible reminder of the instructions, and missed-item messages the participant can see. "I do not agree" asks once to confirm. The sending and saved-file screens get plain wording. Item text, response options and instruction text stay as the export gives them (IP1).

**Out:**
- M146 takes the Study Link Builder. M147 takes the Module Builder.
- Five larger changes join the "Extend the online form" candidate row, and each needs its own decision. The first is progress kept across a reload. The second is a study-team contact carried in the study link. The others are Back to the instrument before, a "Try sending again" button, and consent text with headings or links.
- A "prefer not to answer" choice changes instrument content and scored data (IP1, IP3). It is not planned.

## Acceptance criteria

- [ ] AC1: A grep lists each call of `showError` in `form.js`, apart from its definition. A Playwright test fires at least one refusal through each call. Each time, the screen shows the heading and one sentence that tells the participant what to do. A connection failure says to check the connection and reload. A browser that cannot read the link says to open it in another browser. Any other refusal says to contact the study team. The refusal's own text sits inside a closed `<details>` named "Details for the study team". Outside that element's body, no other text shows.
- [x] AC2: The page's own text is all shown text apart from the researcher's consent text, question text, study name and file name. A Playwright test makes three walks. The first link has consent text, questions before and after, two instruments and no store, and it ends on the saved-file screen. The second link has a web-address store and a `complete` address, and it ends on the sent screen. The third link has a `completeSaved` address and a store that answers HTTP 500, and it ends on the saved-file screen. On every screen, the page's own text holds no build date, no package version, no host name and no HTTP status. It holds no match of the patterns `\bstores?\b`, `\bendpoint\b`, `\bjson\b`, `\bdescriptor\b` and `\bmodule\b`, case-insensitive. The build date and package version appear in a `<footer>` inside a closed `<details>`, on every screen after the instruments load. A two-instrument link shows one line for each instrument there.
- [x] AC3: The identifier screen shows a hint under the "Participant identifier" label, tied to the input by `aria-describedby`. The input has `autocapitalize="off"`, `autocorrect="off"` and `spellcheck="false"`. The Enter key runs the same checks as the "Begin" button. An empty value shows the same alert, and a filled value starts the form. A Playwright test asserts each fact.
- [x] AC4: An item page shows "Page p of n" above the items and beside the Next or Finish button. Here n counts the pages of that instrument. With two or more instruments, it also shows "Part k of m" in both places. A press of Next or Finish with missed items puts the text "Please answer this item" inside each missed item. The page scrolls to the first missed item. A message directly above that item states how many items are missed. At 375px wide, each response option's clickable area is at least 44px high. A Playwright test asserts each fact on the first and the last page of each part. It uses a HiTOP-BR link and a PID-5-BF plus HiTOP-BR link, and it probes one missed item and three.
- [x] AC5: An item page holds a closed `<details>` named "Instructions". Its body text equals the instrument's `instructions.start` in the export the test fetches. A Playwright test checks the first and the last page of each of the five instruments.
- [x] AC6: The first press of "I do not agree" shows a confirmation with "Yes, I do not agree" and "Go back". "Go back" redraws the consent screen with both buttons, sends no request and saves no file. "Yes, I do not agree" reaches the declined screen. If the link gives a `completeDeclined` address, the page then goes there. While a send runs, the screen says "Sending your answers. Please keep this page open." After a send fails, the saved-file screen is headed "Your answers were not sent". It says that the file holds the participant's answers. It shows no HTTP detail outside the closed "Details for the study team" element. A Playwright test asserts each fact, for a send that answers HTTP 500 and for a connection failure.
- [ ] AC7: The hitop-form Playwright suite passes locally and on its PR's CI.

## Coverage

- AC1 → T1, T6, T8
- AC2 → T2, T6
- AC3 → T3, T6, T10
- AC4 → T4, T6, T9, T10
- AC5 → T4, T6
- AC6 → T5, T6, T9
- AC7 → T6, T7

## Tasks

- [x] T1: Rebuild `showError()` (`form.js` lines 1473-1478) with a participant sentence and a closed study-team section. List the `showError(` call sites for the Review section.
- [x] T2: Move the version line (`versionLine()`, lines 1494-1499, used at lines 1615, 1963 and 2019) into a closed `<details>` in the footer. Remove host names and HTTP detail from the start, sent and saved-file text (lines 1611, 1971).
- [x] T3: Add the identifier hint and input attributes (lines 1583-1628), and start the form on Enter.
- [x] T4: On the item pages (lines 1843-1893), add the progress lines, the "Instructions" section and the missed-item messages. Enlarge the option target (`index.html` lines 101-109).
- [x] T5: Add the decline confirmation (lines 1640-1675). Add the sending line (lines 1940-1941). Reword the saved-file screens (lines 1929-1936, 1969-1973, 1996-2022).
- [x] T6: Write the Playwright tests for AC1 to AC6. Update the existing specs that assert old text. Update the hitop-form README, hitop NEWS and the online-collection article where they describe the changed screens.
- [x] T7: Run the suite locally and on the PR. Take before and after screenshots of each screen at 375px for Jeff's look at the merge gate.
- [x] T8 (review R1): Make `canInflate()` also check that `new DecompressionStream('deflate-raw')` works, so a browser without `deflate-raw` gets the browser sentence. Add a P1 test with a constructor that throws on `deflate-raw`.
- [ ] T9 (review R2 to R4): Change the failed-send lead to say that the page got no confirmation, and keep the heading. Make sure that the article agrees. Draw `p.sending` empty on the last page and write its text at the send. Rewrite the missed count on each answer. Add a test for each change.
- [ ] T10 (review R5 to R8): In P4, leave the middle item blank in the one-missed probe. Make the identifier hint fit a recruiter participant. Change "responses were sent" at `online-collection.Rmd:99` and in the `send.spec.js` comment. Change the NEWS host sentence to "outside that section".

## Work log

- 2026-09-29: created by /milestone-plan, with M146 and M147. Jeff added this milestone at the plan gate.
- 2026-09-29: the criteria audit ran in full mode with a fresh [O] reader. It returned 18 findings, and each was repaired as suggested. A refusal the participant can fix (connection, browser) now says what to do, not only "contact the study team". The walks set `complete` and `completeSaved` and fail with HTTP 500. The word scan covers the page's own text with word-boundary patterns. The page gets a `<footer>`, which does not exist today. AC4 says "Next or Finish" and probes one and three missed items. AC5 compares with the fetched export, not with the start screen.
- 2026-09-30: M146 review pass 3 filed two notes on this page's text. The `checkStore` prefix "Where responses go could not be used:" no longer says the fault is in the study link, and `link.html` matches that prefix as literal text to pick the focused field. The `z` refusals now say "unpack" (`form.js:204`, `210`), which participants also see.
- 2026-09-30: M150 review filed one note on this page's text. An unknown instrument is worded three ways. `checkInstruments` (`form.js:105`) says "an instrument the online form does not know". `parseLink` (`form.js:719`) says "an instrument this page does not know". The builder (`link.html:909`) says "the Study Link Builder does not offer". The "this page" text left at `form.js:260`, `:675`, `:1610` and `:1945` shows on the online form too.
- 2026-09-30: /milestone-implement started. Branch `m148-form-page-new-participant` cut in hitop and in the hitop-form companion. Question gate: Jeff took the three recommendations (D1 to D3). T6 now also names the README, NEWS and article updates (minor amendment). The task line numbers in `form.js` predate M149 to M151, and the code is read fresh.
- 2026-09-30: T1 done. `showError()` takes the refusal and picks the sentence by its `kind`: `connection` (an export fetch that fails), `browser` (no `DecompressionStream`), or the study-team sentence. The refusal sits in `details.study-team .fault`. The three calls are in `boot()`: after `parseLink`, after `fetchExports`, and after `planStems`, which also passes the version footer. The new `tests/screens.spec.js` P1 lists the calls and fires six refusals through them. Eight specs read the refusal through the new `refusalText()` helper. Suite: 853 of 853.
- 2026-09-30: T2 done. Every screen of `runForm()` ends with `foot()`, the study-team section with a `<footer>` of every export's version line. The start screen names no host, and every completion link reads "Continue to the next step of the study" (D3). A failed send's fault moved from the saved screen's lead into the section. P2 in `tests/screens.spec.js` makes the three walks. A plant that put the store's host back on the start screen failed walk 2. Nine specs updated for the new text and order. Suite: 856 of 856, two of them rerun after a fix.
- 2026-09-30: T3 done. The identifier field is a `div.field` with a `label for`, a `#participant-hint` paragraph and the input, which carries `aria-describedby`, `autocapitalize`, `autocorrect` and `spellcheck` off. Enter in the input calls Begin's own handler. P3 checks each fact, Enter and Begin side by side, and a plant that ignored Enter failed the three Enter tests. Suite: 864 of 864.
- 2026-09-30: T4 done. An item page shows `p.where` ("Part k of m · Page p of n") above a closed `details.reminder` and the items. The nav holds `div.forward` with the same line beside the button, so a wrapped row keeps them together. `finish()` now takes the nav's last button. A press with items missed marks each with `p.missed` and `aria-describedby`, and puts `p.missed-count` (role alert) directly above the first. Option labels have `min-height: 44px`. P4 and P5 cover AC4 and AC5. A no-scroll plant and a no-min-height plant (34px) each failed P4. `walk.spec.js` and `layout.spec.js` assert the new marks. Full run: 869 passed, and 2 page loads timed out (`consent.spec.js` and `question-screens.spec.js`, h1 and Begin never shown). Those two specs then passed 71 of 71.
- 2026-09-30: T5 done. "I do not agree" calls `confirmDecline()`, which swaps the buttons for a `.confirm` group (D2). A send adds `p.sending` (role status) above the nav. The failed-send screen is headed "Your answers were not sent", and its trail opens "This file holds your answers." The sent and no-store screens say "answers" for "responses", and twelve specs follow the new text. P6 covers AC6. A plant of the old one-press decline failed both decline tests. Suite: 875 of 875.
- 2026-09-30: T6 done. The tests for AC1 to AC6 landed with T1 to T5 as P1 to P6 in `tests/screens.spec.js`. The hitop-form README describes the changed screens and lists the new spec in its test table. hitop NEWS has one entry. The online-collection article covers the decline question and the part line on item pages. No R code changed, so `devtools::test()` was not run. `link-sections.spec.js`, which reads the README's headings, passed 100 of 100.
- 2026-09-30: T7 done locally. Suite: 875 of 875. Twelve screens at 375 px, from hitop-form `main` and from the branch, are in the session scratchpad under `shots/` (24 PNGs, made by `shots.mjs` there). The run on the PR's CI waits for /milestone-review, which opens the PR after approval (D-138).
- 2026-09-30: claim audit: 135 claims read, 6 corrected — hitop NEWS.md, online-collection.Rmd; hitop-form README.md, form.js, index.html, tests/guard, walk, send and save specs
- 2026-09-30: the G1 test in `guard.spec.js` now reads the version line inside the closed study-team section, a fix the audit's finding 2 led to. `guard.spec.js` and `link-sections.spec.js` passed 239 of 239. Status set to review.
- 2026-09-30: /milestone-review: AC1 to AC6 verified on fresh runs (875 of 875), and the gate checks are clean. Review R1 shows AC1 failing: on Chrome and Edge 80 to 102, a `z` link gets "contact the study team", not the browser sentence. Defect return 1 of this milestone. AC1 unticked.
- 2026-09-30: step-7 gate: Jeff chose "Send back to fix". The proposed dispositions in the Review section stand. T8 to T10 hold the fix-now work (review send-back), and Coverage maps them. The six follow-ups go to a candidate row at the post-merge hygiene pass. Status set to in-progress.
- 2026-09-30: /milestone-implement resumed. Both branches are level with `origin/main`. Nothing was open for a question gate.
- 2026-09-30: T8 done. `canInflate()` now builds a `deflate-raw` `DecompressionStream` in a `try`. A new P1 test replaces the constructor with one that throws for `deflate-raw`. Before the fix it failed on the contact sentence, and after it passed with the browser sentence. The README states both browser cases. `screens`, `zlink`, `link` and `link-sections` specs: 266 passed.

## Decisions

- D1 (2026-09-30, question gate): every screen after the instruments load ends with one closed `<details>` named "Details for the study team". It holds the version lines in a `<footer>`. On an error or failed-send screen, the refusal or the send's fault comes first in it. An error screen before the instruments load holds the refusal alone.
- D2 (2026-09-30, question gate): "I do not agree" keeps the consent text on screen and replaces the two buttons with a question, "Yes, I do not agree" and "Go back". Focus moves to the question.
- D3 (2026-09-30, question gate): a link to a completion address reads "Continue to the next step of the study", in place of the address's host name.

## Review

Fresh run 2026-09-30, hitop-form branch head `995346f`, hitop branch head `94dfd960`. Both branches are level with `origin/main` (0 commits behind), so no merge was needed. Full hitop-form suite: 875 passed, 0 failed (2.2 min, local Chromium).

- AC1: `grep -n 'showError(' form.js` lists the definition at line 1505 and three calls in `boot()`, at lines 1556, 1563 and 1570. A P1 test in `screens.spec.js` pins the same three lines. Six P1 tests fire refusals through the calls. Call 1 gets an unreadable link (contact sentence) and a `z` link with no `DecompressionStream` (browser sentence). Call 2 gets an aborted fetch for one and for two instruments (connection sentence) and an HTTP 500 export (contact sentence). Call 3 gets a module item that the export lacks (contact sentence). Each test asserts the heading, the sentence, and the refusal inside a closed `details.study-team` named "Details for the study team". Each also asserts that the shown text is exactly those three lines. All 7 tests passed.
- AC2: The three P2 walks passed. Walk 1 is a `z` link with consent, a question before and after, PID-5-BF plus HiTOP-BR and no store. It passes 11 screens and ends on the saved-file screen with a file name. Walk 2 starts on the identifier screen, sends to a routed web address that answers 200, and ends on the sent screen. The page then asks once for the `complete` address. Walk 3 sends to a store that answers HTTP 500, ends on "Your answers were not sent", and links to `completeSaved`. The test cuts the researcher's text out of each screen's shown text. What is left holds no build date, package version, walk host, "HTTP" or "500". It also matches none of the five word patterns. On every screen, a closed `details.study-team` is the last child of `main`. Its `footer .version` lines are one per instrument, two in walk 1.
- AC3: Eight P3 tests passed. The hint sits under `label[for=participant]` and above the input, by bounding box. The input's `aria-describedby` names it, and the accessible description equals the hint text. The input has `autocapitalize="off"`, `autocorrect="off"` and `spellcheck="false"`, and its `spellcheck` property is false. Enter and Begin each run three cases on a fresh load. An empty value shows Begin's alert and keeps focus. A lone surrogate shows Begin's other alert. A filled value starts the form on "Page 1 of 3" with 15 items.
- AC4: Both P4 tests passed at 375 by 812 px. The HiTOP-BR link probes pages 1 and 3 of 3. The PID-5-BF plus HiTOP-BR link probes pages 1 and 2 of 2, then 1 and 3 of 3. The test states the page counts itself. On each probed page, `p.where` above the first item and `.nav .step` beside the Next or Finish button read "Page p of n". On the two-part link they read "Part k of m · Page p of n". The step line ends before the button, no more than 32 px from it, on the same row. The lowest option label is at least 44 px high. Three items are left blank, then one. Each press marks exactly those items "Please answer this item". The element directly above the first is `p.missed-count` with role alert and the right count. That item is in the viewport, and the page stays.
- AC5: Five P5 tests passed, one for each of HiTOP-SR, HiTOP-BR, PID-5, PID-5-SF and PID-5-BF. Each fetches the export and reads page 1 and page n, where n is the item count over 15, rounded up. On both pages, `main > details.reminder` is closed and named "Instructions". Its body text equals the export's `instructions.start`, and it comes before the first item.
- AC6: Four P6 tests passed. The first press of "I do not agree" keeps the consent text on screen. It shows the question with "Yes, I do not agree" and "Go back", and focus moves to the question. "Go back" redraws the consent screen with both buttons. In the next second, no request is made and no file is saved. "Yes, I do not agree" reaches "Thank you" with the declined sentence, again with no request or file. With `completeDeclined`, nothing loads before the confirming press, and the page then reaches the address. The C6 tests in `consent.spec.js` also pass, and they show the declined screen before that move. For a held send, `p.sending` reads "Sending your answers. Please keep this page open." while the button is disabled. This holds for a send that answers HTTP 500 and for a refused connection. Each then shows "Your answers were not sent", the saved file name and "This file holds your answers.". The send's fault is in the closed section. The shown text matches no "HTTP", "500" or "connection".
- AC7 (local half): the full suite passed 875 of 875 on `995346f`, as stated above. The CI half waits for the hitop-form PR, which opens only after the merge approval. The box stays unticked until that run is green.

Consistency gate: `cairn_validate.py` passed (exit 0, 24 advisory warnings, none new). No principle changed, so `cairn_impact` did not run. `devtools::document()` made no diff. `pkgdown::check_pkgdown()` found no problems. README.Rmd is unchanged. NEWS.md has one entry for the change. No new top-level files. `devtools::check()` gave 0 errors, 0 warnings and 0 notes.

Independent review: three fresh reviewers (Opus diff, Sonnet blame-history, Sonnet prior reviews). The PR-comment probes found no threads in either repo. Findings, ranked, with the proposed disposition for the gate:

- R1 (diff 2, AC1): `canInflate()` checks only that `DecompressionStream` exists. Chrome and Edge 80 to 102 have it without `deflate-raw`, so a `z` link throws a raw TypeError there. The participant gets "contact the study team", not "open it in another browser". Proposed: return to in-progress, because AC1 fails for that browser class.
- R2 (diff 1): every unconfirmed send now reads "Your answers could not be sent". A timeout or a reply that is not JSON can still store the row, and the article says so. The old text said "could not be confirmed". Proposed: fix now. The lead says that the page got no confirmation of the send. The AC6 heading stays.
- R3 (diff 4, prior 1): `p.sending` (role status) is inserted with its text, so many screen readers do not read it. M122 drew its status empty first for this reason. Proposed: fix now, with the status drawn empty on the last page and filled at the send.
- R4 (diff 5): after a press, answering one missed item leaves the count at its old number until all are answered. Proposed: fix now, with the count rewritten on each answer.
- R5 (diff 8): the one-missed scroll probe uses the last item, which is already in view after the press, so it cannot fail. Proposed: fix now, with the middle item left blank.
- R6 (diff 3): the hint "Type the identifier the study team gave you" is wrong for a Prolific or SONA participant whose address value is empty. Proposed: fix now, with recruiter-neutral wording.
- R7 (diff 12, blame 6, prior 3): "responses were sent" is left at `online-collection.Rmd:99` and in a `send.spec.js` comment. Proposed: fix now.
- R8 (blame 1, part): NEWS says "No screen shows a host name", but the closed section holds export addresses. Proposed: fix now, with "outside that section".
- R9 (diff 10, blame 3): about 20 refusal specs read only the closed fault text, not the shown sentence. Proposed: follow-up.
- R10 (blame 5): the Supabase `/rest/v1/` acceptance test lost its host check. Proposed: follow-up.
- R11 (diff 6): an export answered 503, or a captive portal page, gets "contact the study team", not "reload". Proposed: follow-up.
- R12 (diff 7): "Go back" puts focus on the heading, not on "I do not agree". Proposed: follow-up.
- R13 (blame 2): the missed count is moved on each press, and a repeat press with the same text is not always read. Proposed: follow-up.
- R14 (prior 2): the unknown-instrument refusal is still worded three ways (M150 note). It now shows only in the closed section. Proposed: follow-up.
- R15 (blame 1): the start screen and completion links no longer name a host. Proposed: reject, because D3 and AC2 chose this.
- R16 (blame 4): the version lines sit behind a closed section, also on the consent and declined screens. Proposed: reject, because D1 chose this.
- R17 (diff 9): the HTTP pattern checks only 500. Proposed: reject, because the walks produce only 500.
- R18 (diff 11): the error screen focuses the heading and also holds an alert. Proposed: reject, because the old error screen did the same.
- R19 (blame 7): `.progress` is now a test hook only. Proposed: reject, because nothing a participant sees changes.

Gate 2026-09-30: Jeff chose "Send back to fix". The dispositions above are final. R1 to R8 go to T8 to T10, and R9 to R14 go to a follow-up row. R15 to R19 are rejected for the reasons given. AC1 is unticked until R1 is fixed and verified again.
