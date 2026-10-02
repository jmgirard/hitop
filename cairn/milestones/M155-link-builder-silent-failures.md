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
- [ ] AC4: Some links hold a `module` nested too deeply for the builder to write into the module box. The Study Link Builder refuses such a `z` or setup-file link with this message: "The study link you opened holds a module nested too deeply for this browser to show. Fill in the form above to make a new link." One test opens a `z` link, and one a setup-file link, whose `module` is an object holding an array nested 6,000 deep. Each asserts the message, and that no `pageerror` fires.
- [ ] AC5: `labelText()` leaves out a `.hint`, `select` or `textarea` that sits two or three levels inside a label. A test puts each of the three kinds at each of the two depths inside a section label. Each time, it asserts that the section summary lists the label text without that node's text. S10 in `tests/link-sections.spec.js` still passes.
- [ ] AC6: Module rows get four changes, and a test asserts each.
  - (a) Each module row's box, file control, alert and status have accessible names that hold "instrument N". N is the row's number. A test with two module rows checks all eight names, then moves a row and checks them again.
  - (b) A module row's menu can change to another instrument. Then the row's alert and status are emptied, and a file read still running drops its text. A test holds a read, switches away and back, and releases the read. Alert, status and box are then empty.
  - (c) Two module rows are refused with the repeat message and this added sentence: "A list holds one HiTOP-SR module."
  - (d) The Instruments hint holds this sentence: "At most one HiTOP-SR, whole or as a module."
- [ ] AC7: hitop-form's README names the new behavior in its module section and its test-table rows. hitop's NEWS.md has an entry. The full hitop-form suite passes locally and on its PR's CI. `devtools::check()` in hitop gives 0 errors, 0 warnings and 0 notes.

## Coverage

- AC1 → T1, T8
- AC2 → T2, T8
- AC3 → T3, T8
- AC4 → T4, T8
- AC5 → T5, T8
- AC6 → T6, T7, T8
- AC7 → T8

## Tasks

- [x] T1: In `link.html`, give two `catch` blocks an inner guard that writes `#err` directly: the prefill's (about `:1200-1203`) and `fillFromCurrent()`'s (about `:1222-1225`). Enable "Make the link" (about `:1236`) whatever the prefill path did. Write "could not be read" only when `#err` is empty and the address holds `c`, `z`, `setup` or `sha256`. With no link, write AC1(b)'s message. Each plant patches a named DOM method and arms only after the prefill starts, so no earlier call throws. Tests go in `tests/link-sections.spec.js` beside S11, and (d) in `tests/link-setupfile.spec.js`.
- [x] T2: Add a `<noscript>` message and a hidden load-failure message near the top of `<main>`. Set a ready flag as the module script's first statement after its imports. A small plain script shows the message on `load` when the flag is unset. Tests: a 404 route for `form.js`, a route that serves it with a syntax error, and a context with `javaScriptEnabled: false`.
- [x] T3: Make the form `inert` and `aria-busy` while `prefill()` waits on `inflateConfig()` or `openSetupFile()` (about `:940` and `:961`). Clear both in a `finally`. Tests hold the `z` unpack as L34 does, and hold the setup fetch as `link-setupfile.spec.js` does. An inert target makes Playwright wait, so the tests press with `force: true` and type by keyboard, as `pressEarly` does (`tests/link.spec.js:1245-1251`).
- [ ] T4: In `fill()`, put the indented `JSON.stringify(config.module, null, 2)` (about `:1143`) in a `try`. On a `RangeError`, refuse with AC4's message. Tests go in `tests/link.spec.js` and `tests/link-setupfile.spec.js`.
- [ ] T5: Rewrite `labelText()` (about `:620-627`) as a walk over child nodes at every depth that skips a `.hint`, `input`, `select` or `textarea`, with no `cloneNode`. The test goes in `tests/link-sections.spec.js`.
- [ ] T6: In `addModuleFields()` (about `:478-526`), return the alert, status and read counter, or a reset function. `renumberInstruments()` (about `:379-391`) names the four module controls with the row number. The menu's `change` handler (about `:426-428`) empties the alert and status and drops a running read when the row leaves the module. Tests go in `tests/link-instruments.spec.js`.
- [ ] T7: Add the two sentences of AC6(c) and (d). The first goes in the clash test (about `:1433-1439`) as its two-module-row case, and the second in the Instruments hint at about `:121`. Update `README.md:70-72` and the tests that read the old texts.
- [ ] T8: Update the README module section and test-table rows. In DESIGN Known issue 15, say that the Study Link Builder now shows AC2's message on a browser that cannot parse `form.js`. Add the hitop NEWS entry. Run the full hitop-form suite locally and on the PR. Run `devtools::check()` in hitop.

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
