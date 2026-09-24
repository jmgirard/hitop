# M122: hitop-form's "Save the file" button confirms each press in a status message a screen reader announces

- **Status:** planned
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** —
- **Resolves:** —
- **Surface tier:** user-facing — participant-facing page behavior on the saved-file screens
- **Branch/PR:** —

## Goal

A press of the "Save the file" button on either saved-file screen writes a short confirmation into a status region the screen renders empty, so a participant using a screen reader hears that the file was saved again while focus stays on the button.

## Scope

**In:** In hitop-form `showSaved()` (`form.js:947`), a `p` element with `role="status"` and class `saved-again`, rendered empty between the "Save the file" button and the completion link, so the live region exists before the first press. Each press calls `saveFile(name, text)` as today and then rewrites the region with "The file was saved again.", so a second press is a fresh write. Focus is not moved. Outcome-screen copy is page copy outside IP1, on the reading M120 took of D-072(c). Tests: the screen-order helper's token list, the save-again helper's assertions, and the README's participant section and tests table. A NEWS entry in hitop, as M120's page change carried one.

**Out:** A screen-reader announcement of the first download itself, which the browser owns. The sent screen, which has no button. Moving focus after a press: rejected at planning (work log).

## Acceptance criteria

- [ ] AC1: Before any press, each saved-file screen (no store; a send the store did not confirm; each with and without a completion link) holds a `p.saved-again` with `role="status"` and empty text, placed after the "Save the file" button and before the completion link or the version line; shown by the screen-order tests for those four screens, whose expected order carries the new token, and by an assertion of the role and the empty text on each.
- [ ] AC2: After a press of "Save the file", the status element's text is exactly "The file was saved again." and the press still yields a download with the Finish download's suggested name and bytes; a second press rewrites the region, so its text is the same and the region's content changed at that press; shown by the save-again helper, observing the rewrite with a mutation observer installed before the second press, on the no-store screen, the unconfirmed-send screen, and one saved screen with a completion link.
- [ ] AC3: After a press made from the keyboard (the button focused, then Enter), `document.activeElement` is the "Save the file" button; shown by the same helper. This guards behavior the page has today, so its test is green before the change.
- [ ] AC4: The hitop-form README's participant section says a press shows "The file was saved again." under the button, its tests table row for `save.spec.js` names the message, and hitop's NEWS holds an entry for the message, each claim enforced by a test in AC1 to AC3.
- [ ] AC5: `npx playwright test` is clean in hitop-form.

## Coverage

- AC1 → T1, T2
- AC2 → T1, T2
- AC3 → T1, T2
- AC4 → T3, T4
- AC5 → T2

## Tasks

- [ ] T1: In `tests/helpers.mjs`, add the `P.saved-again` token to `savedScreenOrder()` (`helpers.mjs:291`), assert the role and empty text where the order is asserted, and extend `expectSaveAgain()` (`helpers.mjs:273`) with the keyboard press, the text, the mutation and the focus assertions AC2 and AC3 name; add one call on a with-link screen (`save.spec.js:299`); run `save.spec.js` and `send.spec.js`: AC1 and AC2 red, AC3 green.
- [ ] T2: In `form.js` `showSaved()`, render the empty status element and give the button's `onclick` the rewrite after `saveFile()`; `npx playwright test` green.
- [ ] T3: README: the participant section's save-again paragraph (`README.md:155-161`) and the `save.spec.js` row of the tests table.
- [ ] T4: In hitop, a NEWS entry under the development heading.

## Work log

- 2026-09-24: created by /milestone-plan, absorbing the M120 F8 candidate row.
- 2026-09-24: criteria audit ran in full mode ([O] fresh reader, M121 and M122 together): 13 findings; 5 on this file, 4 fixed in the draft (the role and empty text asserted explicitly, a press on a with-link screen, the second-press rewrite stated as a page property with the observer as method, a keyboard press with AC3 named as a guard on existing behavior) and 1 settled by wording (the copy rests on M120's reading of D-072(c)).
- 2026-09-24: plan gate chose "The file was saved again." over "Saved again: <file name>." and "The file was offered again." because it matches the README's wording for the button and stays short across presses; falsified by a participant report that the sentence misled them when the browser blocked the download.
- 2026-09-24: planning chose leaving focus on the button over moving it to the region because a polite status region is announced without focus and a moved focus loses the button for a second press; falsified by a screen-reader user reporting the message unspoken.

## Decisions

## Review
