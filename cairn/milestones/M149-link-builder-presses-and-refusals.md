# M149: Study Link Builder: early presses, file reads and refusals

- **Status:** in-progress
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

- [ ] AC1: In `link.html`, "Make the link" has the `disabled` and `autocomplete="off"` attributes in the markup. The page's script removes `disabled` after the prefill's `try` and `catch`, on each of four outcomes: a filled form, a refused link, no link and a prefill that throws. A Playwright test holds the page in two states: while a `z` link's unpacking waits, and while the request for `form.js` waits. In each state it presses the button with a forced click and presses Enter in the study name box. The page's address does not change and no navigation request starts. After the release, a press of the button shows "Your study link".
- [ ] AC2: While "Choose the module file" reads a file, a typed edit to the module box wins. When the read ends, the box keeps the typed text. A read that fails shows "The module file could not be read." in `#moduleFileErr`. A chosen instrument export is refused with a message that holds no "Paste" and names the Module Builder and `write_module()`. A Playwright test asserts each fact. It holds a read with an init-script patch of `File.prototype.text`, types during the hold, and makes a second read reject.
- [ ] AC3: The section summaries are drawn with no copy of a control. A Playwright test sets the recruiting site to "Another site" and adds one question of each type. It then types 50 characters into each `input[type=text]` and `textarea` that `querySelectorAll` finds in `details.optional`. An init-script wrapper counts `Node.prototype.cloneNode` calls, and the count stays 0. A grep of the body of `labelText()` finds none of `cloneNode`, `importNode`, `cloneContents`, `innerHTML` and `outerHTML`. Each summary still lists the labels of its fields that hold a value.
- [ ] AC4: Four steps after the prefill run inside the prefill's `try`: `showKind()`, `showSite()`, `showHeld()` and the loop that opens filled sections. A Playwright test opens a study link that sets SONA and consent text. An init-script patch of a DOM method makes one of those steps throw. After the throw, the message says that the link was not read. Every summary reads "Not used", every optional section is closed, and no site hint or destination field block shows. With the study name filled, a press of "Make the link" shows "Your study link".
- [ ] AC5: On the online form, `parseLink()` refuses a link whose `instrument` field is present and is not a string. A Playwright test opens links with `instrument` set to `["hitopbr"]`, `[["pid5"]]`, `null`, `1`, `true` and `{}`. Each is refused with a message that names the `instrument` field, and the page requests no instrument export. Before the change, `["hitopbr"]` reached an export request.
- [ ] AC6: A grep for `refuseAt(` in `link.html` lists each call apart from the definition. A Playwright test fires each listed call, and closes every optional section before each press.
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

- [ ] T1: Put `disabled` and `autocomplete="off"` on "Make the link" (`link.html:241`). Remove `disabled` after the prefill's `try` and `catch` (`:952-965`). Write the AC1 test: delay `DecompressionStream` output for a `z` link, and hold the `form.js` route.
- [ ] T2: In the module textarea's `input` listener (`link.html:657-660`), bump `moduleReads` so a pending read drops its text (`:661-679`). Reword `form.js:1047`. Write the AC2 tests in `tests/link-module-file.spec.js`, the `moduleFileErr` path (`:675`) among them.
- [ ] T3: Rewrite `labelText()` (`link.html:507-511`) to read label text with no copy. Move the post-prefill steps (`:966-970`) into the `try` at `:952`. Make the `catch` hide the site hints and destination blocks, reset the summaries and close the sections. Rename the `nextStep()` local `sql` (`:1279`) to a name that no top-level `const`, `let` or `function` in the script uses. Write the AC3 and AC4 tests.
- [ ] T4: Add the string check at `form.js:707`, before any fetch. Write the AC5 test in `tests/instruments.spec.js` with a request listener on the export host.
- [ ] T5: Give `checkInstruments` refusals the index of the row at fault (`form.js:99-101`), the later row for a repeat or a second PID-5 form. Focus that row's menu at `link.html:1025`. Update the refusal-focus sentence at hitop-form `README.md:33-36`.
- [ ] T6: Cut `QUESTION_CONTROLS` (`link.html:814`) to the five fields the editor can fault, with no `qList` fallback. Write the AC6 test as one table in `tests/link-sections.spec.js`, one entry per case. Record the grep's list in the work log for review to re-run.
- [ ] T7: Update the README test-table rows of the spec files this milestone changes. Run the full suite locally and on the PR.
- [ ] T8: Add the hitop NEWS.md entry. Run `devtools::check()`.

## Work log

- 2026-09-30: created by /milestone-plan from the "Study Link Builder follow-ups" row, with M150 and M151.
- 2026-09-30: plan chose a button disabled in the markup until the prefill ends over an early classic script that cancels submits. A cancelled press gives no sign, and a disabled button also blocks the Enter submit. Falsified by a report that the page looks broken during a slow `z` load.
- 2026-09-30: the criteria audit ran in full mode with a fresh Opus reader. It returned 13 findings, and each was repaired as suggested. AC1 adds the throw outcome and `autocomplete="off"`. AC3 names the fields it types into and greps for other copy methods. The throw test became AC4 and checks a clean page and a working build. AC5 adds the array probes. AC6 fires each run-time control and closes the sections first. The rename check moved from a criterion to T3, which keeps the count at seven. Four task line numbers were corrected.
- 2026-09-30: implement started. Branches `m149-link-builder-presses` in hitop and hitop-form, cut from the pushed `main` of each.
- 2026-09-30: gate: `checkInstruments` puts the row's index on the thrown error as `index`, and its `bad(why)` callback is unchanged. The instrument-type refusal reads "The study link's instrument field must be text, and it is …". The export refusal says "Use the file that the Module Builder or write_module() saved."
- 2026-09-30: gate: the question refusal's control map keeps only the five fields the editor can fault (name, text, options, min, max). Required, type and the `qList` fallback go, and a fault with no mapped field focuses the message. Sub-task added to T6.
