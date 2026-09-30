# M149: Study Link Builder: early presses, file reads and refusals

- **Status:** review
- **Priority:** high
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** GP3
- **Resolves:** —
- **Surface tier:** user-facing — the researcher page that makes study links, and one refusal on the online form
- **Branch/PR:** m149-link-builder-presses, companion: /Users/jmgirard/github/hitop-form m149-link-builder-presses

## Goal

On the Study Link Builder, an early press, an edit during a file read and a refused field each end as the researcher expects.

## Scope

**In:** In hitop-form `link.html`, the milestone changes six things. "Make the link" does nothing until the prefill ends. A typed edit to the module box wins over a file read that is still running. A failed file read and a refused instrument export get tested messages. The section summaries stop copying each labelled control on every keystroke. The setup steps after the prefill run inside the prefill's guard, and a throw there leaves a clean page. A refused instrument row gets focus, and every build refusal is fired by a test that checks focus. The `nextStep()` local `sql` gets a name that no page-level name uses. In `form.js`, `parseLink()` refuses an `instrument` that is not a string, and the module refusal of an instrument export stops saying "Paste". In hitop, NEWS.md gets an entry.

**Out:**
- M150 takes the hint facts, the "this page" wording and the tutorials. M151 takes the test-reach items that change no page behavior.
- M148 takes the online form's screens. This milestone changes one `form.js` refusal text and one check that the online form also runs, and leaves `showError()` to M148.
- The order of `change` and an Enter submit in other browsers (M146 review, unverified) stays DESIGN Known issue 13.

## Acceptance criteria

- [x] AC1: In `link.html`, "Make the link" has the `disabled` and `autocomplete="off"` attributes in the markup. The page's script removes `disabled` after the prefill's `try` and `catch`, on each of four outcomes: a filled form, a refused link, no link and a prefill that throws. A Playwright test holds the page in two states: while a `z` link's unpacking waits, and while the request for `form.js` waits. In each state it presses the button with a forced click and presses Enter in the study name box. The page's address does not change and no navigation request starts. After the release, a press of the button shows "Your study link".
- [x] AC2: While "Choose the module file" reads a file, a typed edit to the module box wins. When the read ends, the box keeps the typed text. A read that fails shows "The module file could not be read." in `#moduleFileErr`. A chosen instrument export is refused with a message that holds no "Paste" and names the Module Builder and `write_module()`. A Playwright test asserts each fact. It holds a read with an init-script patch of `File.prototype.text`, types during the hold, and makes a second read reject.
- [x] AC3: The section summaries are drawn with no copy of a control. A Playwright test sets the recruiting site to "Another site" and adds one question of each type. It then types 50 characters into each `input[type=text]` and `textarea` that `querySelectorAll` finds in `details.optional`. An init-script wrapper counts `Node.prototype.cloneNode` calls, and the count stays 0. A grep of the body of `labelText()` finds none of `cloneNode`, `importNode`, `cloneContents`, `innerHTML` and `outerHTML`. Each summary still lists the labels of its fields that hold a value.
- [x] AC4: Four steps after the prefill run inside the prefill's `try`: `showKind()`, `showSite()`, `showHeld()` and the loop that opens filled sections. A Playwright test opens a study link that sets SONA and consent text. An init-script patch of a DOM method makes one of those steps throw. After the throw, the message says that the link was not read. Every summary reads "Not used", every optional section is closed, and no site hint or destination field block shows. With the study name filled, a press of "Make the link" shows "Your study link".
- [x] AC5: On the online form, `parseLink()` refuses a link whose `instrument` field is present and is not a string. A Playwright test opens links with `instrument` set to `["hitopbr"]`, `[["pid5"]]`, `null`, `1`, `true` and `{}`. Each is refused with a message that names the `instrument` field, and the page requests no instrument export. Before the change, `["hitopbr"]` reached an export request.
- [x] AC6: A grep for `refuseAt(` in `link.html` lists each call apart from the definition. A Playwright test fires each listed call, and closes every optional section before each press.
  - Where the call passes a control, the message shows, the control's section (if any) is open, and focus is on the control. Where the call passes no control, focus is on the message.
  - For a call that passes a control chosen at run time, the test fires one case per control it can pass. For the question call, a grep of the question check lists each control it assigns to `e.control`, and one case has no control. For the store call, they are the four store fields.
  - For the instruments call, the control is the menu of the row at fault. The test fires a repeated instrument and a second PID-5 form, each at row 2 and at row 3.
  - The fired calls include the fetch failure, the encode failure and the four stale-build refusals. The fetch and the encode are made to fail by a route and an init-script patch.
- [ ] AC7: The hitop-form Playwright suite passes locally and on its PR's CI. In hitop, NEWS.md has an entry for the builder changes, and `devtools::check()` gives 0 errors and 0 warnings.

## Coverage

- AC1 → T1, T7
- AC2 → T2, T7
- AC3 → T3, T7
- AC4 → T3, T7
- AC5 → T4, T7
- AC6 → T5, T6, T7
- AC7 → T7, T8

## Tasks

- [x] T1: Put `disabled` and `autocomplete="off"` on "Make the link" (`link.html:241`). Remove `disabled` after the prefill's `try` and `catch` (`:952-965`). Write the AC1 test: delay `DecompressionStream` output for a `z` link, and hold the `form.js` route.
- [x] T2: In the module textarea's `input` listener (`link.html:657-660`), bump `moduleReads` so a pending read drops its text (`:661-679`). Reword `form.js:1047`. Write the AC2 tests in `tests/link-module-file.spec.js`, the `moduleFileErr` path (`:675`) among them.
- [x] T3: Rewrite `labelText()` (`link.html:507-511`) to read label text with no copy. Move the post-prefill steps (`:966-970`) into the `try` at `:952`. Make the `catch` hide the site hints and destination blocks, reset the summaries and close the sections. Rename the `nextStep()` local `sql` (`:1279`) to a name that no top-level `const`, `let` or `function` in the script uses. Write the AC3 and AC4 tests.
- [x] T4: Add the string check at `form.js:707`, before any fetch. Write the AC5 test in `tests/instruments.spec.js` with a request listener on the export host.
- [x] T5: Give `checkInstruments` refusals the index of the row at fault (`form.js:99-101`), the later row for a repeat or a second PID-5 form. Focus that row's menu at `link.html:1025`. Update the refusal-focus sentence at hitop-form `README.md:33-36`.
- [x] T6: Cut `QUESTION_CONTROLS` (`link.html:814`) to the five fields the editor can fault, with no `qList` fallback. Write the AC6 test as one table in `tests/link-sections.spec.js`, one entry per case. Record the grep's list in the work log for review to re-run.
- [x] T7: Update the README test-table rows of the spec files this milestone changes. Run the full suite locally and on the PR.
- [x] T8: Add the hitop NEWS.md entry. Run `devtools::check()`.

## Work log

- 2026-09-30: created by /milestone-plan from the "Study Link Builder follow-ups" row, with M150 and M151.
- 2026-09-30: plan chose a button disabled in the markup until the prefill ends over an early classic script that cancels submits. A cancelled press gives no sign, and a disabled button also blocks the Enter submit. Falsified by a report that the page looks broken during a slow `z` load.
- 2026-09-30: the criteria audit ran in full mode with a fresh Opus reader. It returned 13 findings, and each was repaired as suggested. AC1 adds the throw outcome and `autocomplete="off"`. AC3 names the fields it types into and greps for other copy methods. The throw test became AC4 and checks a clean page and a working build. AC5 adds the array probes. AC6 fires each run-time control and closes the sections first. The rename check moved from a criterion to T3, which keeps the count at seven. Four task line numbers were corrected.
- 2026-09-30: implement started. Branches `m149-link-builder-presses` in hitop and hitop-form, cut from the pushed `main` of each.
- 2026-09-30: gate: `checkInstruments` puts the row's index on the thrown error as `index`, and its `bad(why)` callback is unchanged. The instrument-type refusal reads "The study link's instrument field must be text, and it is …". The export refusal says "Use the file that the Module Builder or write_module() saved."
- 2026-09-30: gate: the question refusal's control map keeps only the five fields the editor can fault (name, text, options, min, max). Required, type and the `qList` fallback go, and a fault with no mapped field focuses the message. Sub-task added to T6.
- 2026-09-30: T1 done (hitop-form). The button is disabled in the markup and enabled after the prefill's try/catch. L34 in `tests/link.spec.js` failed before the fix: in each hold state the press sent the form as an HTML GET. Full suite 782 passed.
- 2026-09-30: T2 done (hitop-form). The module box's `input` listener bumps `moduleReads`, and the export refusal says "Use". MF6 and MF7 failed before the fix, MF6 on the file overwriting the typed text and MF7 on "Paste". Full suite 784 passed, the four pending T3/T4 tests left out.
- 2026-09-30: T3 done (hitop-form). `labelText()` reads the label's child nodes in place. The four setup steps run inside the prefill's `try`, and the notice is written after them. The `catch` hides the hints and blocks, resets the summaries and closes the sections. The `nextStep()` local is `runSql`. S10 and S11 in `tests/link-sections.spec.js` failed before the fix (11,357 `cloneNode` calls, the grep hit, no message after the throw).
- 2026-09-30: T3 verify: one full run had 1 failure in MF6, message not kept. MF6 then passed 135 runs, 120 of them at 12 workers, and a second full run passed 787. MF6 now waits for the file control to empty before its second choice.
- 2026-09-30: T4 done (hitop-form). `parseLink()` refuses an `instrument` that is present and not a string before the name lookup. I3 in `tests/instruments.spec.js` failed before the fix on all six values. A scratch probe, deleted after, showed `["hitopbr"]` and `[["pid5"]]` each requesting their export before the fix. Full suite 793 passed.
- 2026-09-30: T5 done (hitop-form). `checkInstruments` sets `index` on the error for an unknown entry, the later of a repeat and the second PID-5 form. The builder focuses that row's menu, and the README's focus sentence names it. The S12 test in T6 fires these cases. Full suite 793 passed.
- 2026-09-30: T6 done (hitop-form). `QUESTION_CONTROLS` holds name, text, options, min and max, and `e.control` is set only for a mapped field. S12 in `tests/link-sections.spec.js` has 36 case entries and one coverage test. The coverage test failed before the map change, on qType and qRequired. With the T5 focus planted back to row 1, all four row cases failed on focus. Full suite 830 passed.
- 2026-09-30: T6 grep for review to re-run: `grep -n "refuseAt(" link.html | grep -v "function refuseAt"` lists 23 calls, at lines 1063, 1069, 1078, 1091, 1120, 1124, 1136, 1146, 1152, 1169, 1173, 1179, 1185, 1194, 1205, 1214, 1221, 1227, 1256, 1268, 1272, 1284 and 1290 of hitop-form `link.html` at this commit. S12's coverage test reads the same list from the page.
- 2026-09-30: T7 done locally (hitop-form). README test-table rows updated for `link.spec.js`, `link-sections.spec.js`, `link-module-file.spec.js` and `instruments.spec.js`. Full suite 830 passed. The PR's CI run belongs to `/milestone-review`, which opens the PRs.
- 2026-09-30: T8 done. NEWS.md entry at the top of "Improvements and fixes". `devtools::check()` gave 0 errors, 0 warnings, 0 notes.
- 2026-09-30: claim audit: 60 claims read, 4 corrected — hitop `NEWS.md`, hitop-form `README.md`, `form.js`, `link.html`, `tests/link.spec.js`, `tests/link-module-file.spec.js`, `tests/instruments.spec.js`, `tests/link-sections.spec.js`. The four were the NEWS export-refusal sentence, the README's S12 question fields, the README and header wording of S10, and the link.html Firefox comment. The reader re-read all four and found them true. `link-sections.spec.js` and `link.spec.js` then passed 204.
- 2026-09-30: open concern for review: with no link opened, a throw in the setup steps would now show "The study link you opened could not be read". No criterion covers that case.
- 2026-09-30: review started (first pass, no PR yet). Both default branches unmoved since the branches were cut.

## Review

Evidence run 2026-09-30 on hitop-form `37661a8` and hitop `b95e2faf`. Full hitop-form suite: 830 passed (2.7 min).

- AC1: `link.html:241` carries `disabled autocomplete="off"`, and `makeButton.disabled = false` sits after the prefill's `try`/`catch`. L34 in `tests/link.spec.js` checks the markup and holds a `z` unpack and the `form.js` route. In each hold it forces a click and presses Enter in the study box. The address stays the same and no navigation request starts, and each hold builds after the release. The enabled tests cover no link, a refused link and a filled link. S11 covers the throw outcome because it builds after the throw. The form has `novalidate`, so without the fix Enter submits it. All passed in the full run.
- AC2: the module box's `input` listener now adds 1 to `moduleReads`, so a read that ends later drops its text. MF6 in `tests/link-module-file.spec.js` holds the first read with a `File.prototype.text` init-script patch and types "typed" during the hold. After the release the box holds "typed" and the status is empty. A second read rejects, and `#moduleFileErr` shows "The module file could not be read.". MF7 chooses the hitopsr export as a file. The message holds no "Paste" and names the Module Builder and `write_module()`. Both passed in the full run.
- AC3: `labelText()` reads the label's child nodes in place. S10 in `tests/link-sections.spec.js` sets "Another site" and adds a text, number, choice and multi question. It finds 28 fields in `details.optional`, a count the test states on its own. It types 50 characters into each visible field. A field that the question's type hides gets the 50 characters as 50 `input` events, since a hidden field takes no keys. The `cloneNode` count stays 0, and each summary lists its filled labels. A second S10 test and a fresh `awk` over the body of `labelText()` find none of the five copy methods (0 hits). Both passed in the full run.
- AC4: `showKind()`, `showSite()`, `showHeld()` and the section-opening loop now sit inside the prefill's `try` in `link.html`. S11 opens a `z` link with `participantParam: "id"`, which the page reads as SONA, and consent text. An init-script patch of `Element.prototype.setAttribute` throws on the call in `showSite()` that ties the SONA hint to the menu. The message reads "The study link you opened could not be read. Fill in the form above to make a new link.". Every summary reads "Not used" and every section is closed. The three site hints and the other-site, webhook and Supabase blocks are hidden. With a study name, "Make the link" shows "Your study link". Passed in the full run.
- AC5: `parseLink()` in `form.js` refuses an `instrument` that is present and not a string, before the name lookup. I3 in `tests/instruments.spec.js` opens six links, with `["hitopbr"]`, `[["pid5"]]`, `null`, `1`, `true` and `{}`. Each refusal reads "The study link's instrument field must be text, and it is …". A request listener on the export host records no request. The work log records that I3 failed on all six before the fix, and that a scratch probe saw `["hitopbr"]` request its export. All six passed in the full run.
- AC6: a fresh `grep -n "refuseAt(" link.html | grep -v "function refuseAt"` lists 23 calls, at lines 1064 to 1291. They are the 23 calls the T6 work-log line lists, each one line later after the claim-audit commit. The S12 coverage test reads the same list from the page. It maps each of its 36 cases to one line and misses no line. The instrument cases fire a repeat and a second PID-5 form at rows 2 and 3, and focus the menu of that row. The question cases fire each of the five controls in `QUESTION_CONTROLS`. A 51-question link has no control and focuses the message. The coverage test checks that the one `e.control =` line names no control of its own. The store cases fire the four store fields. The fetch failure is made by a route abort and the encode failure by an init script that deletes `CompressionStream`. The four stale-build refusals are also fired. Each case closes every section before the press, and then checks the message, the focus and which section is open. S12 passed 37 of 37 in a separate run and in the full run.
- AC7 (local half, box not ticked): the full hitop-form suite passed 830 of 830. NEWS.md has the entry at the top of "Improvements and fixes". `devtools::check()` gave 0 errors, 0 warnings and 0 notes in 6 min 49 s. The PR's CI half waits for step 8, where the PR opens, and the box is ticked only on green CI.

Consistency gate: `cairn_validate.py` exit 0, with 24 old advisory warnings. No principle text changed, so `cairn_impact` is skipped. `devtools::document()` leaves no diff, `pkgdown::check_pkgdown()` finds no problems, and README.Rmd is unchanged. NEWS.md names no milestone.

Reviewers: diff-bug (Opus) with 12 findings, blame-history (Sonnet) with 6, and prior-review (Sonnet) with 4. Neither repo has PR review threads. Merged into 13 items below, most severe first, each with its proposed disposition for the gate.

- R1 (diff-bug 1): S11 does not test the catch's reset. Its throw is in `showSite()`, so `showHeld()` and the open loop never run and the page is still clean. Verified: with the 12-line reset block cut from a scratch copy, S11 passed. AC4 as written still passes. Proposed: fix now, with the throw moved to the section-open loop so the reset has work to do, and the cut shown red.
- R2 (diff-bug 4, blame 1): hitop-form README's S12 row says "the four store fields", a term D-083 retires from that README. Proposed: fix now.
- R3 (diff-bug 2, prior-review 1): a throw inside the catch itself leaves "Make the link" disabled with no message. Proposed: follow-up.
- R4 (diff-bug 3, blame 6, prior-review 2, work-log open concern): the catch always gives the "could not be read" refusal of an opened link. With no link opened, or with a link already refused by name, the message is wrong. Proposed: follow-up.
- R5 (diff-bug 5, blame 4): if `form.js` never loads, the button stays disabled with no message. Before, a press sent a plain GET. Proposed: follow-up.
- R6 (diff-bug 6): other controls stay live during a `z` unpack, and the prefill then writes over them. Out of scope. Proposed: follow-up.
- R7 (diff-bug 7): a very deep `instrument` array makes `JSON.stringify` throw before the refusal names the field. Sibling refusals share the pattern. Proposed: follow-up.
- R8 (diff-bug 8): `labelText()` skips only direct children, where the old code removed at any depth. No label on the page nests a hint or control today. Proposed: follow-up.
- R9 (diff-bug 12, prior-review 4): NEWS.md has one 103-character line in a 75-character entry. Proposed: fix now.
- R10 (diff-bug 9, blame 3, prior-review 3): `autocomplete="off"` on a button is not valid HTML, and its Firefox effect has no test. Proposed: reject, since the plan chose it. Record it with DESIGN Known issue 13's Chromium-only reach at hygiene.
- R11 (diff-bug 10): the early-press check waits a fixed 500 ms. A slow runner gives a false pass, never a false fail, and L34 failed before the fix. Proposed: reject.
- R12 (diff-bug 11): the T6 work-log line numbers are one lower than at HEAD. Proposed: reject, since the AC6 evidence above records the shift.
- R13 (blame 2, blame 5): NEWS.md says "compressed", and a typed edit drops a file read with no note. NEWS.md is outside D-083's scope, and AC2 chose the drop. Proposed: reject.
