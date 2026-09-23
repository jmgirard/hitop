# M114: hitop-form draws a wrapped item's text inside its card

- **Status:** review
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

- [x] AC1: A Playwright test walks every page of the HiTOP-SR and of the full PID-5 at viewport widths of 320 px, 375 px and Playwright's default. For each item, the legend's box lies inside the fieldset's padding box on all four sides, within 0.5 px: `legend.top >= fieldset.top + borderTop + paddingTop`, `legend.left >= fieldset.left + borderLeft + paddingLeft`, `legend.right <= fieldset.right - borderRight - paddingRight` and `legend.bottom <= fieldset.bottom - borderBottom - paddingBottom`, and the legend's `scrollWidth` is at most its `clientWidth`. Each walk asserts that at least one legend is taller than one line of its computed line height.
- [x] AC2: Every item on the first page of each of the five forms is found by `getByRole('group', { name: '<position>. <text>', exact: true })`, where `<text>` is the export's item text. When Next is refused on a page with a blank item, that item's fieldset has all four computed border colours equal to the error colour and its legend meets AC1's inequalities, so the highlight is a continuous box.
- [x] AC3: An item's DOM keeps the elements and classes `fieldset.item`, `legend .pos`, `legend .text` and `.options`, and every test present on hitop-form's `main` before this milestone passes without an edit to its body.

## Coverage

- AC1 → T1
- AC2 → T2
- AC3 → T1, T3

## Tasks

- [x] T1: The CSS change in `index.html` (`fieldset.item legend`, lines 85–89), the space in `itemNode()` (`form.js`, the legend at line 552), and `tests/layout.spec.js` with the three-width walks of AC1.
- [x] T2: The accessible-name test over the five forms and the refusal-border test of AC2.
- [x] T3: The README test table row, the hitop-form PR, its CI, and the dispatched deployed-page run after the merge. The hitop PR carries tracking only.

## Work log

- 2026-09-23: created by /milestone-plan, from Jeff's report of wrapped item text cut by the card border, confirmed at 375 px and desktop widths on the deployed page.
- 2026-09-23: criteria audit ran in full mode on a fresh [O] reader (shared with M115 and M116): seven findings on this file, all fixed in the wording. The card test measures four sides against the padding box and the text's overflow at three widths. The accessible name gets a real space and an exact match. The refusal border is checked together with the legend's placement. The "111 tests pass unchanged" promise became "every test on main before the milestone passes without an edit". The PID-5 form is named.
- 2026-09-23: plan gate chose keeping the bordered card with the text inside over dropping the border, because the refused-item highlight and the tests rest on the card's box; falsified by a participant report that the cards make the page hard to read.
- 2026-09-23: IP1 read at planning: no participant-facing text changes, so no sign-off is needed.
- 2026-09-23: /milestone-implement started; branches cut from `main` at hitop `f96014c2` and hitop-form `c15e149`. Question gate skipped: the one open choice, how the legend leaves the notch, takes the standard `float: left; width: 100%` form, chosen over a `div[role=group]` rewrite because it keeps the fieldset semantics and the tests' selectors.
- 2026-09-23: T1 and T2 done in one spec, `tests/layout.spec.js` (Y1 six walks, Y2 five forms, Y3 the refusal): the legend floats at full width with the options cleared below it, and a space text node separates the number from the text. Plants: the old CSS reds Y1 and Y3 on the legend-top check, the missing space reds Y2 on the exact name. Suite 111 → 123, all passing.
- 2026-09-23: T3: the README test-table row landed. The PR, its CI and the deployed-page run are review's, opened after the merge approval (D-138).
- 2026-09-23: claim audit: 27 claims read, 1 corrected — tests/layout.spec.js, index.html. The reader served a copy of `main` and measured: a wrapped legend's upper half sat above the card's top border, and its right edge was 17 px inside the card, so the two comments saying the text spilled past the right edge were corrected to the top edge.
- 2026-09-23: all tasks checked; status set to review. hitop-form branch `m114-form-item-card-layout` at four commits over `c15e149`, suite 123 passing; the hitop branch carries tracking only.

## Decisions

## Review

- 2026-09-23 sync: the hitop branch contains `origin/main`, which did not move after the cut at `f96014c2`. The hitop-form branch sits three commits over `origin/main` at `c15e149` with nothing behind. Neither repo has a PR yet.
- AC1 evidence: `npx playwright test` on hitop-form at `7883276` passed 123 tests. The Y1 tests in `tests/layout.spec.js` walk every page of `hitopsr` and `pid5` at 320 px, 375 px and the default viewport, six tests. Each test reads every legend's box against its fieldset's padding box on four sides at 0.5 px. It asserts `scrollWidth <= clientWidth` and at least one legend taller than 1.5 line heights per walk. Plant: the old `index.html` CSS applied in reverse reds the six walks and Y3 on "legend top". The file was restored and the tree is clean.
- AC2 evidence: the Y2 tests run `getByRole('group', { name: \`${i + 1}. ${it.text}\`, exact: true })` with the export's item text for every item on the first page of the five forms. Five tests pass. Plant: the space text node removed from `itemNode()` reds all five on `toHaveCount(1)`. The file was restored. Y3 refuses Next with item 3 blank at 375 px. It reads the four computed border colours equal to the resolved `var(--error)` and re-checks the legend inequalities on that item, with an answered item as a control. It passes.
- AC3 evidence: `form.js` on the branch builds `fieldset.item`, `legend > span.pos`, `legend > span.text` and `div.options` at lines 611 to 625. The tests diff against `origin/main` lists `tests/layout.spec.js` alone, so no prior test body changed. The 111 prior tests all pass, 123 less the 12 new ones.
- Gate: `cairn_validate` exit 0 with 24 advisory warnings, none new. `document()` no diff. `check_pkgdown()` no problems. `check()` 0 errors, 0 warnings, 0 notes. No principle changed, so `cairn_impact` did not run. NEWS.md needs no entry because the hitop package did not change. No driving RR.
- Lenses: user-facing tier, three lenses over the hitop-form diff. [S] blame-history: no finding. The legend rule and the two-span legend trace to the first commit `208ef62` with no stated reason the change undoes, and no other test reads the legend's combined text. [S] prior-review: no finding. Seven archived hitop-form reviews read, the PR-comment probe returned an empty list, and the one LESSONS line on Playwright concerns downloads. [O] diff-bug: 13 findings, none a broken page, triaged below.
