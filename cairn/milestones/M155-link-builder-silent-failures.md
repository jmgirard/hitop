# M155: Study Link Builder: silent failures and module rows

- **Status:** in-progress
- **Priority:** high
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** GP3
- **Resolves:** —
- **Surface tier:** user-facing — the Study Link Builder page that researchers use
- **Branch/PR:** m155-link-builder-silent-failures; companion: /Users/jmgirard/github/hitop-form m155-link-builder-silent-failures

## Goal

The Study Link Builder gives a researcher a message and a usable form at each load, link-opening and module-row failure the reviews found silent or wrong.

## Scope

**In:** These changes are in hitop-form `link.html`:
- the prefill's failure path, and a `form.js` that does not load
- controls held while a link unpacks or a setup file is fetched
- a link whose module is too deep to show, and a label that nests its hint
- four module-row gaps: row numbers in the module controls' names, a switch away and back, the two-module-row refusal, and the Instruments hint
- the README rows, DESIGN Known issue 15, the hitop NEWS entry and the suite

**Out:**
- The test-reach gaps of the same candidate row go to M156, which depends on M155.
- The online form's own load failure on an old browser stays DESIGN Known issue 15. AC2 covers the Study Link Builder only.
- AC3 takes one clause of the "Hosted setup file gaps" row: fields typed during the fetch. The rest of that row stays there.
- The "Study link length-check gaps" row stays as it is.

## Acceptance criteria

- [ ] AC1: At each of four points, a throw in the prefill path ends with an enabled "Make the link" and a message. A test plants each throw by an init script that arms after the prefill starts, and asserts the result.
  - (a) A `c` link whose fill throws, then a second throw inside `resetForm()` in the prefill's `catch`: `#err` is not empty.
  - (b) A throw in `showFilled()` after `prefill()` returns, on a load with no `c`, `z`, `setup` or `sha256`: `#err` holds "The Study Link Builder did not start correctly. Reload the page."
  - (c) A throw in `showFilled()` after a link is refused by name: `#err` holds that named refusal, unchanged. The test fires one named refusal from each of `prefill()`, `openSetupFile()` and `fill()`.
  - (d) After a press of "Fill in the form from the current file", a throw in `fillFromCurrent()`'s `try`, then a second throw inside `resetForm()` in its `catch`: `#err` is not empty.
- [ ] AC2: When `form.js` does not load or does not parse, `link.html` shows this message: "The Study Link Builder did not start. Reload the page. If it still does not start, open it in a current version of Chrome, Edge, Firefox or Safari." One test answers the `form.js` request with HTTP 404, and one serves it with a syntax error. Each asserts the message is visible. With JavaScript off, a `<noscript>` message shows: "The Study Link Builder needs JavaScript. Turn on JavaScript in this browser, then reload the page." A second test asserts it. On a normal load, neither message shows.
- [ ] AC3: While the prefill waits on a `z` unpack or a setup-file fetch, the form takes no input. A test holds each of the two waits. During the hold, it types into the study box and presses "Add an instrument". The study box and the row count stay the same. After the release, the study box holds the link's value, and the box then takes typed text.
- [ ] AC4: Some links hold a `module` object nested too deeply for the builder to write into the module box. The Study Link Builder refuses such a `z` or setup-file link, when it would otherwise fill the form from it, with this message: "The study link you opened holds a module nested too deeply for this browser to show. Fill in the form above to make a new link." One test opens a `z` link, and one a setup-file link, whose `module` is an object holding an array nested 20,000 deep. Each asserts the message, and that no `pageerror` fires.
- [ ] AC5: `labelText()` leaves out a `.hint`, `select` or `textarea` that sits two or three levels inside a label. A test puts each of the three kinds at each of the two depths inside a section label. Each time, it asserts that the section summary lists the label text without that node's text. S10 in `tests/link-sections.spec.js` still passes.
- [ ] AC6: Module rows get four changes, and a test asserts each.
  - (a) Each module row's box, file control, alert and status have accessible names that hold "instrument N". N is the row's number. A test with two module rows checks all eight names, then moves a row and checks them again.
  - (b) A module row's menu can change to another instrument. Then the row's alert and status are emptied, and a file read still running drops its text. A test holds a read, switches away and back, and releases the read. Alert, status and box are then empty.
  - (c) Two module rows are refused with the repeat message and this added sentence: "A list holds one HiTOP-SR module."
  - (d) The Instruments hint holds this sentence: "At most one HiTOP-SR, whole or as a module."
- [ ] AC7: hitop-form's README names the new behavior in its module section and its test-table rows. hitop's NEWS.md has an entry. The full hitop-form suite passes locally and on its PR's CI. `devtools::check()` in hitop gives 0 errors, 0 warnings and 0 notes.

## Coverage

- AC1 → T1, T8, T9
- AC2 → T2, T8, T11, T12
- AC3 → T3, T8, T11
- AC4 → T4, T8, T10
- AC5 → T5, T8
- AC6 → T6, T7, T8
- AC7 → T8, T13

## Tasks

- [x] T1: In `link.html`, give two `catch` blocks an inner guard that writes `#err` directly: the prefill's (about `:1200-1203`) and `fillFromCurrent()`'s (about `:1222-1225`). Enable "Make the link" (about `:1236`) whatever the prefill path did. Write "could not be read" only when `#err` is empty and the address holds `c`, `z`, `setup` or `sha256`. With no link, write AC1(b)'s message. Each plant patches a named DOM method and arms only after the prefill starts, so no earlier call throws. Tests go in `tests/link-sections.spec.js` beside S11, and (d) in `tests/link-setupfile.spec.js`.
- [x] T2: Add a `<noscript>` message and a hidden load-failure message near the top of `<main>`. Set a ready flag as the module script's first statement after its imports. A small plain script shows the message on `load` when the flag is unset. Tests: a 404 route for `form.js`, a route that serves it with a syntax error, and a context with `javaScriptEnabled: false`.
- [x] T3: Make the form `inert` and `aria-busy` while `prefill()` waits on `inflateConfig()` or `openSetupFile()` (about `:940` and `:961`). Clear both in a `finally`. Tests hold the `z` unpack as L34 does, and hold the setup fetch as `link-setupfile.spec.js` does. An inert target makes Playwright wait, so the tests press with `force: true` and type by keyboard, as `pressEarly` does (`tests/link.spec.js:1245-1251`).
- [x] T4: In `fill()`, put the indented `JSON.stringify(config.module, null, 2)` (about `:1143`) in a `try`. On a `RangeError`, refuse with AC4's message. Tests go in `tests/link.spec.js` and `tests/link-setupfile.spec.js`.
- [x] T5: Rewrite `labelText()` (about `:620-627`) as a walk over child nodes at every depth that skips a `.hint`, `input`, `select` or `textarea`, with no `cloneNode`. The test goes in `tests/link-sections.spec.js`.
- [x] T6: In `addModuleFields()` (about `:478-526`), return the alert, status and read counter, or a reset function. `renumberInstruments()` (about `:379-391`) names the four module controls with the row number. The menu's `change` handler (about `:426-428`) empties the alert and status and drops a running read when the row leaves the module. Tests go in `tests/link-instruments.spec.js`.
- [x] T7: Add the two sentences of AC6(c) and (d). The first goes in the clash test (about `:1433-1439`) as its two-module-row case, and the second in the Instruments hint at about `:121`. Update `README.md:70-72` and the tests that read the old texts.
- [x] T8: Update the README module section and test-table rows. In DESIGN Known issue 15, say that the Study Link Builder now shows AC2's message on a browser that cannot parse `form.js`. Add the hitop NEWS entry. Run the full hitop-form suite locally and on the PR. Run `devtools::check()` in hitop.
- [ ] T9 (review R1): `armOnAddress()` in `tests/helpers.mjs` arms on the first statement of `prefill()`, the `opened.has('setup')` call at about `link.html:1002`. S13 and the LF7 offer test still fail with T1's guard removed.
- [ ] T10 (review R2): in `fill()`, the module box's write also refuses on an error named `InternalError`, which Firefox throws for deep recursion. DESIGN Known issue 13 says that no test runs this refusal in Firefox.
- [ ] T11 (review R3, R4): the README and NEWS text "does not load or does not run" names only what the ready flag covers: a `form.js` that does not load or cannot be read. DESIGN Known issue 15 adds two gaps. Browsers that parse `form.js` but lack `replaceChildren()` fail with no message. Browsers without `inert` take typing during a setup-file fetch.
- [ ] T12 (review R5, R6): `#loadFail` gets `tabindex="-1"`. When the script shows it, the script also focuses it. L39 asserts the focus. The normal-load test asserts that the `noscript` element is in the page before it asserts the message is hidden.
- [ ] T13 (review R8, R12): rename L18's ninth `BAD_C` entry and correct the L20 comment to say what they now check. Rewrap NEWS.md's long line and remove README.md's stray line break before "Change the row back". Run the full hitop-form suite and `devtools::check()`.

## Work log

- 2026-10-01: created by /milestone-plan.
- 2026-10-01: criteria audit, full mode, fresh Opus reader. Returned AC4 unsatisfiable, because Chromium's plain `JSON.stringify` writes any setup under 100,000 bytes, so only the module box's indented write fails. AC4 was narrowed to it. AC1 was narrowed to named throw points, with (d) added. AC2 got exact texts, and AC5 got per-kind probes. M156 AC4 was narrowed to page requests.
- 2026-10-01: second criteria audit of the rewritten AC1, AC2, AC4 and AC5, full mode, fresh Opus reader. Fixes: AC4's module is an object holding a deep array, and is probed by `z` and setup file, with `c` dropped. AC1's points are named by function, with one named refusal per source in (c). AC2 adds a syntax-error probe. AC5 drops `input` and the `cloneNode` clause, both unable to fail.
- 2026-10-01: plan chose a ready flag read by a plain script on `load` over an `onerror` on a separate module script, because the flag also covers a `form.js` that does not parse. Falsified by a browser that fires `load` before the module's first statement runs.
- 2026-10-01: plan chose an `inert` form during the prefill's waits over dropping the prefill when the researcher types, because a dropped prefill loses the opened link. Falsified by a report that a slow setup-file fetch leaves the form unusable too long.
- 2026-10-01: plan chose a refusal at the module box's write over a depth check in `form.js` `decodeLink()`, because the builder does not call `decodeLink()` and only the indented write fails in Chromium. Falsified by a deep `c` or `z` that throws elsewhere in the builder.
- 2026-10-01: plan gate chose two milestones (this one and M156) over one, because 13 criteria pass the size limit. Gate also fixed the six message texts and took the two items lost from the candidate row and the hosted setup row's typed-during-fetch clause.
- 2026-10-01: implement started on `m155-link-builder-silent-failures` in hitop and hitop-form. Question gate: the four module controls are named "… for instrument N", and the form stays shown under the load-failure message.
- 2026-10-01: T1 done. `reportThrow()` guards `resetForm()`, keeps a named refusal, and writes "could not be read" or the did-not-start message. Tests: S13 (5) in `link-sections.spec.js`, one LF7 test in `link-setupfile.spec.js`, and the shared `armOnAddress()` plant arm. All 6 failed before the fix. hitop-form suite: 1007 passed.
- 2026-10-01: T2 done. A `<noscript>` message and a hidden `#loadFail` alert sit between the nav and the h1, outside the S8 intro count. A plain script shows `#loadFail` on `load` unless the module set `window.linkBuilderStarted`. L39 (4 tests): 3 failed before, and the normal-load test failed with the flag line removed. Full suite: 1007 passed and 4 online-form specs timed out at 15 s. Those 4 specs passed on a re-run (139 tests).
- 2026-10-01: T3 done. `held()` makes `#f` inert and `aria-busy` around the `inflateConfig()` and `openSetupFile()` waits, cleared in a `finally`. Tests L40 (z unpack) and LF10 (setup fetch) share `expectHeldInput()` and `expectReleasedInput()` in `helpers.mjs`. Both failed before the fix, with the typed text in the box. Full suite: 1013 passed.
- 2026-10-01: T4 code landed, not ticked. `fill()` writes the module box text before any field and refuses on `RangeError`. In Chromium 141 the indented write first throws at module depth 6,151, so AC4's 6,000-deep probe fills the box. L41 and LF11 probe at 20,000 deep, pending the AC4 amendment gate. Both failed before the fix with "could not be read". L18's two planted-throw entries now expect the nested-too-deeply refusal.
- re-audit: AC4 (full) — the 20,000-deep wording is satisfiable, and no IP or D-entry blocks it. Read literally, it also covered links refused earlier for another fault and a changed-fingerprint offer. Fix: add "when it would otherwise fill the form from it".
- re-audit: AC4 (full) — the clause closes the gap, and the text is satisfiable with no IP or D-entry conflict. Minor: the builder never writes a deep bare-array `module` to the box and does not refuse it. The reader proposes "`module` object". This second line is the stop, so further wording goes to the user.
- 2026-10-01: T5 done. `labelText()` walks child nodes at every depth and skips a `.hint`, `input`, `select` or `textarea`, with no `cloneNode`. S14 (6 tests: hint, menu and box, each 2 and 3 levels deep) all failed before the fix. S10 passes. Full suite: 1021 passed.
- 2026-10-01: T6 done. `addModuleFields()` returns `{ fields, name, leave }`, kept per row in a `WeakMap`. `renumberInstruments()` names the four module controls "… for instrument N". The box and the file control keep their hints through `aria-describedby`. When the row leaves the module, the menu's `change` runs `leave()`. Tests: LI10 and two LI11 tests in `link-instruments.spec.js`, all 3 failed before the fix. Full suite: 1024 passed.
- 2026-10-01: T7 done. Two module rows add "A list holds one HiTOP-SR module." to the repeat refusal. The Instruments hint adds "At most one HiTOP-SR, whole or as a module." Deviation: the hint's first sentence became "Choose one to three." so the hint stays within S8's 40 words. Tests: LI8 gains a two-HiTOP-SR-row probe with neither sentence, S12 a two-module-row entry, S9 the hint sentence, and all failed first. README module section names both messages. Full suite: 1024 passed and 2 online-form specs timed out at 15 s. Those 2 specs passed on a re-run (42 tests).
- 2026-10-01: AC4 amended at a mini gate. The user chose the recommended text: "module object", "when it would otherwise fill the form from it", and a probe 20,000 deep in place of 6,000. Reason: in Chromium 141, the module box's write first throws at about 6,150 levels. "Object" came from the second re-audit, which was the stop, so the user approved it unaudited. T4 ticked.
- 2026-10-01: T8 docs landed, not ticked. hitop-form README: the module section, "Edit a study link", "The Study Link Builder", and the rows for `link.spec.js`, `link-sections.spec.js`, `link-setupfile.spec.js` and `link-instruments.spec.js`. hitop: a NEWS entry and DESIGN Known issue 15. `devtools::check()` and the claim audit are running.
- claim audit: 52 claims read, 5 corrected — hitop NEWS.md, hitop-form README.md (the two-module refusal is the repeat message plus a sentence, the held form covers only typing during the wait, the summaries skip hints and fields, the L41 and LF11 rows say what those tests check, the S13 row drops "always"). The same reader re-read the 5 and found each correct.
- 2026-10-01: T8 done. `devtools::check()`: 0 errors, 0 warnings, 0 notes. Full hitop-form suite: 1026 passed. The suite's run on the hitop-form PR's CI comes at `/milestone-review`. Status set to review.
- 2026-10-01: review started. Both repos contain `origin/main`, so no merge was needed. No PR exists for either branch.
- 2026-10-01: review return 1 (defect). AC1 fails as written: the test's plant arms before `prefill()` is called. At the merge gate the user sent it back with the fix-now findings as T9 to T13. The user kept the hint "Choose one to three.". Status set to in-progress.

## Review

Evidence run 2026-10-01 on hitop `834aab6a` and hitop-form `8508101`.

- AC1: not ticked. The full hitop-form suite passed (1026). S13's 5 tests in `link-sections.spec.js` and the LF7 offer test in `link-setupfile.spec.js` cover (a) to (d). They assert `#err` and an enabled button. But the criterion says the init script "arms after the prefill starts". `armOnAddress()` (`tests/helpers.mjs:150-160`) arms when the page builds `opened` (`link.html:979`), before `prefill()` is called at `:1293`. Its own comment says "just before its prefill starts". The first statement of `prefill()` is `opened.has('setup')` at `:1002`.
- [x] AC2 evidence: L39's 4 tests passed. `form.js` answered with 404 and served with a syntax error each show `#loadFail`, whose text matches the criterion. A context with JavaScript off shows the `<noscript>` text, which matches. On a normal load `#loadFail` is present and hidden. The `<noscript>` half of that test passes on an absent element (finding R6). With JavaScript on, the browser does not render `<noscript>` content, so the sentence holds.
- [x] AC3 evidence: L40 (`z` unpack) and LF10 (setup fetch) passed. During the hold, `expectHeldInput()` types into the study box and force-presses "Add an instrument". It then asserts an empty box, one row and `aria-busy`. After the release, `expectReleasedInput()` asserts the link's study value and that typed text lands.
- [x] AC4 evidence: L41 (`z`) and LF11 (setup file) passed, each with a `module` object holding an array nested 20,000 deep. Each asserts the refusal text and no `pageerror`, and `expectIndentThrows()` confirms the browser still throws on the indented write. Chromium only (finding R3 on Firefox).
- [x] AC5 evidence: S14's 6 tests (hint, menu and box, each 2 and 3 levels inside a section label) passed, and so did S10.
- [x] AC6 evidence: these tests passed. LI10 checks the eight names of two module rows, then checks them again after a move. The first LI11 test asserts that the alert and status are emptied. The second holds a read, switches away and back, and releases the read, then asserts that alert, status and box are empty. LI8's two-module-row probe, S12's two-module-row refusal entry and S9's hint sentence also passed. `link.html:126` holds the (d) sentence.
- AC7: pending. hitop-form README names the new behavior in the module section and the four test-table rows (diff read). hitop NEWS.md has the entry. The full hitop-form suite passed locally: 1026 passed in 2.4 minutes. `devtools::document()` produced no diff. `devtools::check()` gave 0 errors, 0 warnings and 0 notes in 6 minutes. The one open clause is the hitop-form PR's CI, which runs at the merge step.

Consistency gate: `cairn_validate.py` passed (exit 0, 24 advisory warnings that predate this branch). No principle changed, so `cairn_impact` was skipped. `pkgdown::check_pkgdown()` found no problems. README.Rmd and README.md were not touched. NEWS.md has the entry. No new top-level files.

Independent review: three fresh readers (Opus diff, Sonnet blame history, Sonnet prior reviews). The prior-review lens found that both repos have no PR review comments. Findings merged across lenses, most severe first, each with its proposed disposition for the merge gate.

- R1 (diff): AC1's init script arms when the page reads its address, before `prefill()` is called. The criterion says "after the prefill starts". Proposed: fix now. Arm on `prefill()`'s first statement, `opened.has('setup')`. This is a criterion failing, so the milestone returns to implement.
- R2 (diff): `fill()` catches only `RangeError`. Firefox throws `InternalError: too much recursion`, so there a deep module gets "could not be read", not AC4's message. Proposed: fix now. Also catch an error named `InternalError`, and add Firefox to Known issue 13 because no test runs it.
- R3 (diff): `window.linkBuilderStarted` is set before `setInstruments()` calls `replaceChildren()`. Chrome and Edge 80 to 85, Firefox 72 to 77 and Safari 13.1 parse `form.js` but lack `replaceChildren()`. There the page fails with no message. README and NEWS say "does not load or does not run". Proposed: fix now. Narrow both texts to "does not load or cannot be read", and add the gap to Known issue 15.
- R4 (diff): the hold rests on `inert`, which Firefox before 112 and Safari before 15.5 lack. There, typing during a setup-file fetch is taken and then overwritten. Those browsers cannot unpack a `z` link anyway. Proposed: fix now. Add this to Known issue 15.
- R5 (blame, prior): `#loadFail` is shown on `load` with no focus move. M128 moved focus to messages written at load, because an alert filled then can go unannounced. Proposed: fix now. Give `#loadFail` `tabindex="-1"`. When the script shows it, the script also focuses it. L39 asserts the focus.
- R6 (diff): in "a normal load shows neither message", the `<noscript>` half passes on an absent element. Proposed: fix now. Assert that the `noscript` element is in the page first.
- R7 (diff): if the fill and `resetForm()` both throw, a `c` link's addresses stay filled with no address notice. AC1(a) does not forbid this. Proposed: follow-up, a candidate row.
- R8 (blame, diff): L18's ninth `BAD_C` entry ("beside a completion URL") and the L20 comment now describe a throw after an address is filled. The refusal now comes before any field is filled. Proposed: fix now. Rename the entry and correct the comment. The lost probe goes in the R7 candidate row.
- R9 (blame): L34's `z` case no longer tests the disabled button, because the form is inert during that wait. The `form.js`-load case still does. Proposed: follow-up, in the R7 candidate row.
- R10 (all three): the Instruments hint's first sentence became "Choose one to three." to stay within S8's 40 words. No criterion asked for it. Proposed: maintainer's choice at the gate.
- R11 (diff): the module row's alert and status paragraphs get `aria-label`s. Some screen readers in browse mode read the label in place of the text. AC6(a) asks for the names. Proposed: follow-up, in the R7 candidate row, to check with a screen reader.
- R12 (diff): NEWS.md line 664 breaks the wrap, and README.md has a stray line break before "Change the row back". Proposed: fix now.
- R13 (blame): focus inside the form drops to the body when the form goes inert and is not restored. Proposed: reject. At load, focus is in the form only for typing during the wait, which the hold refuses by design.
- R14 (prior): the new `aria-label` strings are not covered by `prose.mjs --text`. Proposed: reject. LI10 pins all eight names.
- R15 (diff): L40's wait marker is set by `canInflate()`'s probe, before the real unpack. Proposed: reject. `expectHeldInput()` asserts `aria-busy`, which only the hold sets.
- R16 (prior): a setup fetch can hold the form for up to 30 seconds. Proposed: noted. The plan named this as the evidence that would reverse the `inert` choice.
- R17 (prior, blame): the clash message's two-module logic and L18's new expected refusal match M154 and the AC4 amendment. Proposed: noted.

Gate, 2026-10-01: the user sent M155 back and accepted every proposed disposition. Fix now: R1 (T9), R2 (T10), R3 and R4 (T11), R5 and R6 (T12), R8 and R12 (T13). Follow-up: R7, R9 and R11 in the new ROADMAP candidate row "Study Link Builder failure-path gaps". Rejected: R13, R14 and R15, for the reasons above. Noted: R16 and R17. R10: the user kept "Choose one to three.".
