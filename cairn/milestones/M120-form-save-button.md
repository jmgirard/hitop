# M120: hitop-form's saved-file screens carry a "Save the file" button that saves the same file again from the participant's own click

- **Status:** in-progress
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** GP3
- **Resolves:** —
- **Surface tier:** user-facing — the deployed form page's outcome screens and the package's article
- **Branch/PR:** `m120-form-save-button` in hitop and in hitop-form

## Goal

Both saved-file screens of hitop-form gain a "Save the file" button that saves the same CSV again from a click of the participant's own. The two screens are the one shown with no store in the link and the one shown after a send the store did not confirm. The button recovers a download the browser blocked or the participant dismissed before they leave.

## Scope

**In:** In hitop-form, `showSaved()` renders the button after the trail paragraph and before the completion link. A click calls `saveFile()` with the name and text saved at Finish. The trail paragraph gains one sentence naming the button. The README ("What the participant sees", the test table) and the tests follow. In hitop, the online-collection article's saved-file sentences and NEWS follow.

**Out:** Which browsers block the delayed download → nowhere. The M111 F5 premise is unverified, and a dismissed download alone warrants the button. A hand check in Safari during T3 is welcome but no criterion. Dropping the automatic download in favour of the button → rejected, work log. Another recruiter's parameters, the reader's gaps on a malformed download, and the non-ascending descriptor → their candidate rows. The deployed-page workflow run after the merge → T5, no criterion.

## Acceptance criteria

- [ ] AC1: On the unconfirmed screen, after a send the store did not confirm, a button labelled "Save the file" follows the trail paragraph, and a first and a second click on it each start a download whose suggested file name equals the text of `code.filename` and whose bytes equal the file the page saved at Finish. Tested in `tests/send.spec.js` inside the five-outcome loop (an HTML body, a 404, a 500, a refused connection, the hang), so the motivating hang case is among the walks.
- [ ] AC2: On the saved screen, with no store in the link, the same button follows the trail paragraph, and a first and a second click on it each start a download whose suggested file name equals the text of `code.filename` and whose bytes equal the file saved at Finish. Tested in `tests/save.spec.js` on the HiTOP-BR walk.
- [ ] AC3: On the sent screen, after a confirmed send, no "Save the file" button is in the document and no download starts. Asserted in the confirmed-send loop of `tests/send.spec.js` (no completion address) beside its no-download assertion, and in the completion-address test from the observer's report at the held navigation request.
- [ ] AC4: On both saved-file screens the trail paragraph keeps its sentences and ends with "If the file did not appear, press Save the file." The screen's order is the heading, the lead paragraph, the file name, the trail paragraph, the button, the completion link when the link carries one, then the version line. Tested in `tests/save.spec.js` without a completion URL and with one, and in `tests/send.spec.js` on the unconfirmed screen with a completion URL.
- [ ] AC5: The hitop-form README's "What the participant sees" section and its test table, the hitop online-collection article's saved-file sentences, and hitop's NEWS.md say that each saved-file screen has a "Save the file" button that saves the same file again, and that the participant presses it if the file did not appear. Each such claim is one AC1 to AC4 asserts.
- [ ] AC6: `npx playwright test` is green on the hitop-form checkout. In hitop, `devtools::test()` is clean and `pkgdown::check_pkgdown()` passes.

## Coverage

- AC1 → T2, T3
- AC2 → T2, T3
- AC3 → T2, T3
- AC4 → T2, T3
- AC5 → T4
- AC6 → T5

## Tasks

- [x] T1: Precondition: Jeff merges hitop-form PR #7 (M119) from his terminal. Cut `m120-form-save-button` from that main in hitop-form and from main in hitop.
- [ ] T2: Tests first, shown red before T3. In `tests/send.spec.js`, the five-outcome loop clicks the button twice. Each download promise is created before its click with its own timeout, and the name and bytes are compared with the Finish download. The confirmed-send loop and the completion-address test (its observer report at the held request) assert no button labelled "Save the file". The unconfirmed-with-`complete` test asserts the AC4 order. In `tests/save.spec.js`, the HiTOP-BR case clicks twice, the no-store test with `complete` asserts the order, and one test without `complete` asserts the trail sentence. `git add` the fix before any plant.
- [ ] T3: In `form.js`, `saveCsv()` returns the name and the text. `showSaved()` takes both, appends the sentence to the trail paragraph, and renders a `button` "Save the file" whose click calls `saveFile(name, text)`. The button sits between the trail paragraph and the completion link (`form.js:934-952`). The `finish()` comment names the button. Green.
- [ ] T4: Docs, derived from the page and T2's tests. In hitop-form, the README's "What the participant sees" and its save and send rows in the test table. In hitop, the saved-file sentences of `vignettes/articles/online-collection.Rmd` in the Google Sheet section and the Prolific route, and `NEWS.md`.
- [ ] T5: `npx playwright test`, `devtools::test()`, `pkgdown::check_pkgdown()`. Jeff merges the hitop-form PR from his terminal, because the merge guard reads the session's repo. Dispatch the deployed-page run after the merge.

## Work log

- 2026-09-24: created by /milestone-plan, from the ROADMAP's "Save the file" candidate row (lineage M111 review F5). D-072(c) reads outcome-screen copy as page copy outside IP1, so the button and its sentence take no sign-off entry. Open issue #87 does not overlap. hitop has no open PRs. hitop-form PR #7 (M119) is open, hence T1.
- 2026-09-24: criteria audit ran in full mode ([O] fresh reader, agent ab5637af219c45955): 5 findings. Fixed before the gate: "each click" became a first and a second click. AC3 names both confirmed-send tests, the completion one read at the held request. AC4 keeps the trail sentences, appends the quoted one and fixes the order. Two went to the gate: the hang walk (folded into the existing loop) and AC5's phrasing (restated as claims AC1 to AC4 assert).
- 2026-09-24: plan gate chose the button on both saved-file screens over the unconfirmed screen only because one `showSaved()` path serves both and a dismissed download happens on either; falsified by a participant confused by the button after a file that did arrive.
- 2026-09-24: plan gate chose "Save the file" with "If the file did not appear, press Save the file." over "Save the file again" because a first save is not certain; falsified by a participant reading the button as a first save and expecting a different file.
- 2026-09-24: plan gate chose asserting the button inside the five-outcome loop over one new fast test because the loop already walks the hang at no added run time; falsified by the loop's run time crossing the CI budget.
- 2026-09-24: plan gate chose merging hitop-form #7 first over stacking on the m119 branch because a stacked PR retargets and re-reviews; falsified by #7 staying open past the next session.
- 2026-09-24: chose keeping the automatic download beside the button over the button alone because the automatic save still lands when the participant closes the page without pressing anything; falsified by reports of duplicate files read as an error.
- 2026-09-24: implement started. T1: Jeff merged hitop-form #7 from his terminal (squash fa8ed78) after a first re-check found it open; `m120-form-save-button` cut from that main in hitop-form and from main in hitop. No implementation choice was open, so no question gate.

## Decisions

## Review
