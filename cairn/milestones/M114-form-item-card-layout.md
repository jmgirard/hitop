# M114: hitop-form draws a wrapped item's text inside its card

- **Status:** in-progress
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** —
- **Resolves:** —
- **Surface tier:** user-facing — the participant-facing form page
- **Branch/PR:** `m114-form-item-card-layout` (hitop, tracking) and `m114-form-item-card-layout` (hitop-form, code)

## Goal

Change the item card's layout in `jmgirard/hitop-form` so that an item's text renders as a block inside the card, below its top border and within its width, with the fieldset and legend kept for assistive technology.

## Scope

**In:** the CSS for `fieldset.item` and its `legend` in `index.html` (the legend leaves the border notch, for example by floating it at full width), a space between the number span and the text span in `itemNode()` (`form.js`), and Playwright tests that measure the result. The hitop repo changes `cairn/` only.

**Out:** any change to item text, response options or instructions → none (IP1). Dropping the card border → rejected at the plan gate. The link builder's page → untouched. A "Save the file" button on the unconfirmed screen → its candidate row.

## Acceptance criteria

- [ ] AC1: A Playwright test walks every page of the HiTOP-SR and of the full PID-5 at viewport widths of 320 px, 375 px and Playwright's default. For each item, the legend's box lies inside the fieldset's padding box on all four sides, within 0.5 px: `legend.top >= fieldset.top + borderTop + paddingTop`, `legend.left >= fieldset.left + borderLeft + paddingLeft`, `legend.right <= fieldset.right - borderRight - paddingRight` and `legend.bottom <= fieldset.bottom - borderBottom - paddingBottom`, and the legend's `scrollWidth` is at most its `clientWidth`. Each walk asserts that at least one legend is taller than one line of its computed line height.
- [ ] AC2: Every item on the first page of each of the five forms is found by `getByRole('group', { name: '<position>. <text>', exact: true })`, where `<text>` is the export's item text. When Next is refused on a page with a blank item, that item's fieldset has all four computed border colours equal to the error colour and its legend meets AC1's inequalities, so the highlight is a continuous box.
- [ ] AC3: An item's DOM keeps the elements and classes `fieldset.item`, `legend .pos`, `legend .text` and `.options`, and every test present on hitop-form's `main` before this milestone passes without an edit to its body.

## Coverage

- AC1 → T1
- AC2 → T2
- AC3 → T1, T3

## Tasks

- [x] T1: The CSS change in `index.html` (`fieldset.item legend`, lines 85–89), the space in `itemNode()` (`form.js`, the legend at line 552), and `tests/layout.spec.js` with the three-width walks of AC1.
- [x] T2: The accessible-name test over the five forms and the refusal-border test of AC2.
- [ ] T3: The README test table row, the hitop-form PR, its CI, and the dispatched deployed-page run after the merge. The hitop PR carries tracking only.

## Work log

- 2026-09-23: created by /milestone-plan, from Jeff's report of wrapped item text cut by the card border, confirmed at 375 px and desktop widths on the deployed page.
- 2026-09-23: criteria audit ran in full mode on a fresh [O] reader (shared with M115 and M116): seven findings on this file, all fixed in the wording. The card test measures four sides against the padding box and the text's overflow at three widths. The accessible name gets a real space and an exact match. The refusal border is checked together with the legend's placement. The "111 tests pass unchanged" promise became "every test on main before the milestone passes without an edit". The PID-5 form is named.
- 2026-09-23: plan gate chose keeping the bordered card with the text inside over dropping the border, because the refused-item highlight and the tests rest on the card's box; falsified by a participant report that the cards make the page hard to read.
- 2026-09-23: IP1 read at planning: no participant-facing text changes, so no sign-off is needed.
- 2026-09-23: /milestone-implement started; branches cut from `main` at hitop `f96014c2` and hitop-form `c15e149`. Question gate skipped: the one open choice, how the legend leaves the notch, takes the standard `float: left; width: 100%` form, chosen over a `div[role=group]` rewrite because it keeps the fieldset semantics and the tests' selectors.
- 2026-09-23: T1 and T2 done in one spec, `tests/layout.spec.js` (Y1 six walks, Y2 five forms, Y3 the refusal): the legend floats at full width with the options cleared below it, and a space text node separates the number from the text. Plants: the old CSS reds Y1 and Y3 on the legend-top check, the missing space reds Y2 on the exact name. Suite 111 → 123, all passing.

## Decisions

## Review
