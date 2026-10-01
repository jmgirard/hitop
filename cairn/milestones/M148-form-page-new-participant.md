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
- [ ] AC2: The page's own text is all shown text apart from the researcher's consent text, question text, study name and file name. A Playwright test makes three walks. The first link has consent text, questions before and after, two instruments and no store, and it ends on the saved-file screen. The second link has a web-address store and a `complete` address, and it ends on the sent screen. The third link has a `completeSaved` address and a store that answers HTTP 500, and it ends on the saved-file screen. On every screen, the page's own text holds no build date, no package version, no host name and no HTTP status. It holds no match of the patterns `\bstores?\b`, `\bendpoint\b`, `\bjson\b`, `\bdescriptor\b` and `\bmodule\b`, case-insensitive. The build date and package version appear in a `<footer>` inside a closed `<details>`, on every screen after the instruments load. A two-instrument link shows one line for each instrument there.
- [ ] AC3: The identifier screen shows a hint under the "Participant identifier" label, tied to the input by `aria-describedby`. The input has `autocapitalize="off"`, `autocorrect="off"` and `spellcheck="false"`. The Enter key runs the same checks as the "Begin" button. An empty value shows the same alert, and a filled value starts the form. A Playwright test asserts each fact.
- [ ] AC4: An item page shows "Page p of n" above the items and beside the Next or Finish button. Here n counts the pages of that instrument. With two or more instruments, it also shows "Part k of m" in both places. A press of Next or Finish with missed items puts the text "Please answer this item" inside each missed item. The page scrolls to the first missed item. A message directly above that item states how many items are missed. At 375px wide, each response option's clickable area is at least 44px high. A Playwright test asserts each fact on the first and the last page of each part. It uses a HiTOP-BR link and a PID-5-BF plus HiTOP-BR link, and it probes one missed item and three.
- [ ] AC5: An item page holds a closed `<details>` named "Instructions". Its body text equals the instrument's `instructions.start` in the export the test fetches. A Playwright test checks the first and the last page of each of the five instruments.
- [ ] AC6: The first press of "I do not agree" shows a confirmation with "Yes, I do not agree" and "Go back". "Go back" redraws the consent screen with both buttons, sends no request and saves no file. "Yes, I do not agree" reaches the declined screen. If the link gives a `completeDeclined` address, the page then goes there. While a send runs, the screen says "Sending your answers. Please keep this page open." After a send fails, the saved-file screen is headed "Your answers were not sent". It says that the file holds the participant's answers. It shows no HTTP detail outside the closed "Details for the study team" element. A Playwright test asserts each fact, for a send that answers HTTP 500 and for a connection failure.
- [ ] AC7: The hitop-form Playwright suite passes locally and on its PR's CI.

## Coverage

- AC1 → T1, T6
- AC2 → T2, T6
- AC3 → T3, T6
- AC4 → T4, T6
- AC5 → T4, T6
- AC6 → T5, T6
- AC7 → T6, T7

## Tasks

- [x] T1: Rebuild `showError()` (`form.js` lines 1473-1478) with a participant sentence and a closed study-team section. List the `showError(` call sites for the Review section.
- [x] T2: Move the version line (`versionLine()`, lines 1494-1499, used at lines 1615, 1963 and 2019) into a closed `<details>` in the footer. Remove host names and HTTP detail from the start, sent and saved-file text (lines 1611, 1971).
- [ ] T3: Add the identifier hint and input attributes (lines 1583-1628), and start the form on Enter.
- [ ] T4: On the item pages (lines 1843-1893), add the progress lines, the "Instructions" section and the missed-item messages. Enlarge the option target (`index.html` lines 101-109).
- [ ] T5: Add the decline confirmation (lines 1640-1675). Add the sending line (lines 1940-1941). Reword the saved-file screens (lines 1929-1936, 1969-1973, 1996-2022).
- [ ] T6: Write the Playwright tests for AC1 to AC6. Update the existing specs that assert old text. Update the hitop-form README, hitop NEWS and the online-collection article where they describe the changed screens.
- [ ] T7: Run the suite locally and on the PR. Take before and after screenshots of each screen at 375px for Jeff's look at the merge gate.

## Work log

- 2026-09-29: created by /milestone-plan, with M146 and M147. Jeff added this milestone at the plan gate.
- 2026-09-29: the criteria audit ran in full mode with a fresh [O] reader. It returned 18 findings, and each was repaired as suggested. A refusal the participant can fix (connection, browser) now says what to do, not only "contact the study team". The walks set `complete` and `completeSaved` and fail with HTTP 500. The word scan covers the page's own text with word-boundary patterns. The page gets a `<footer>`, which does not exist today. AC4 says "Next or Finish" and probes one and three missed items. AC5 compares with the fetched export, not with the start screen.
- 2026-09-30: M146 review pass 3 filed two notes on this page's text. The `checkStore` prefix "Where responses go could not be used:" no longer says the fault is in the study link, and `link.html` matches that prefix as literal text to pick the focused field. The `z` refusals now say "unpack" (`form.js:204`, `210`), which participants also see.
- 2026-09-30: M150 review filed one note on this page's text. An unknown instrument is worded three ways. `checkInstruments` (`form.js:105`) says "an instrument the online form does not know". `parseLink` (`form.js:719`) says "an instrument this page does not know". The builder (`link.html:909`) says "the Study Link Builder does not offer". The "this page" text left at `form.js:260`, `:675`, `:1610` and `:1945` shows on the online form too.
- 2026-09-30: /milestone-implement started. Branch `m148-form-page-new-participant` cut in hitop and in the hitop-form companion. Question gate: Jeff took the three recommendations (D1 to D3). T6 now also names the README, NEWS and article updates (minor amendment). The task line numbers in `form.js` predate M149 to M151, and the code is read fresh.
- 2026-09-30: T1 done. `showError()` takes the refusal and picks the sentence by its `kind`: `connection` (an export fetch that fails), `browser` (no `DecompressionStream`), or the study-team sentence. The refusal sits in `details.study-team .fault`. The three calls are in `boot()`: after `parseLink`, after `fetchExports`, and after `planStems`, which also passes the version footer. The new `tests/screens.spec.js` P1 lists the calls and fires six refusals through them. Eight specs read the refusal through the new `refusalText()` helper. Suite: 853 of 853.
- 2026-09-30: T2 done. Every screen of `runForm()` ends with `foot()`, the study-team section with a `<footer>` of every export's version line. The start screen names no host, and every completion link reads "Continue to the next step of the study" (D3). A failed send's fault moved from the saved screen's lead into the section. P2 in `tests/screens.spec.js` makes the three walks. A plant that put the store's host back on the start screen failed walk 2. Nine specs updated for the new text and order. Suite: 856 of 856, two of them rerun after a fix.

## Decisions

- D1 (2026-09-30, question gate): every screen after the instruments load ends with one closed `<details>` named "Details for the study team". It holds the version lines in a `<footer>`. On an error or failed-send screen, the refusal or the send's fault comes first in it. An error screen before the instruments load holds the refusal alone.
- D2 (2026-09-30, question gate): "I do not agree" keeps the consent text on screen and replaces the two buttons with a question, "Yes, I do not agree" and "Go back". Focus moves to the question.
- D3 (2026-09-30, question gate): a link to a completion address reads "Continue to the next step of the study", in place of the address's host name.

## Review
