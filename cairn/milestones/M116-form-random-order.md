# M116: A study link can ask hitop-form to show each participant the items in a fresh random order

- **Status:** in-progress
- **Priority:** normal
- **Depends on:** M114, M115
- **Driving RR:** —
- **Principles touched:** IP1, GP3
- **Resolves:** —
- **Surface tier:** user-facing — the participant-facing form page and its link builder
- **Branch/PR:** `m116-form-random-order` (hitop); `m116-form-random-order` in jmgirard/hitop-form

## Goal

A link field `shuffle: true` makes `jmgirard/hitop-form` draw a new random order of the planned items on each load, while the row and the file keep the item columns in the instrument's order under the official item names and record the shown order in an `item_order` column.

## Scope

**In:** `parseLink()`, `planItems()` and the render in `form.js`; `buildCsv()`, `buildRow()` and `storeSql()`; a checkbox in `link.html`; the README; Playwright tests and two SQL fixtures. In hitop: one captured fixture with a test, on this milestone's branch.

**Out:** one fixed order per link for a whole instrument → not planned (a module's `itemOrder` is that route). Random order of the response options → not planned. A start-screen sentence about the order → none, so no participant-facing text changes. A "Save the file" button → its candidate row.

## Acceptance criteria

- [ ] AC1: A link with `shuffle: true` renders the planned items (the export's items, or the module's `items`) in an order drawn on each load by a Fisher–Yates shuffle over `crypto.getRandomValues`, so that the shown order is a rearrangement of them, and the position printed beside each item is its place in the shown order, 1 to n. Two loads of a HiTOP-BR link render different orders, and two loads of a link carrying `module-shuffled.json` render different orders. A link with no `shuffle`, or with `shuffle: false`, renders as today: export order, or a module's `itemOrder` when present and otherwise its `items`. A `shuffle` that is not `true` or `false`, `null` included, is refused with a message naming the field and the value. Tests: the two double loads, an unshuffled module, and refusals for `"true"`, `1` and `null`.
- [ ] AC2: Under `shuffle`, the saved CSV and the posted row carry the five lead columns, then `item_order` holding the shown order as item numbers joined by single spaces, then one column per planned item in the export's order (a module's `items` order), named by the official item name and holding the answer chosen for that item. Without `shuffle`, the file and the row have no `item_order` column and are unchanged. Tests: a HiTOP-BR shuffle walk that saves a file, one that posts to the webhook route, one that inserts through the Supabase route, and a shuffled module walk that saves, each checked by item number against the answers the walk chose at each shown position, and an unshuffled walk against the existing fixture.
- [ ] AC3: The SQL the link builder shows for a Supabase store under `shuffle` has a text column `item_order` after `submitted` and the item columns in the export's order (a module's `items` order). Tests: the SQL compared to two hand-written fixtures, a HiTOP-BR and the shuffled module, and a shuffled Supabase walk whose posted keys equal the SQL's column names in order.
- [ ] AC4: `link.html` has a checkbox "Show the items in a random order" that sets `shuffle: true`. Its hint says that each participant sees a new order, that the file and the table list the items in the instrument's order under their item names, that `item_order` records the order seen, and that a module's printed order is not followed. A link built with the box checked opens a page that renders a rearrangement, by AC1's check.
- [ ] AC5: Every sentence that a grep for `order` finds in hitop-form's README, `link.html` and `form.js` comments and in the package's `?read_form_responses` and online-collection article, and that says the columns follow the order shown, is still true under `shuffle` or is rewritten to say what holds. The README says to start a new sheet for a shuffled link, because the Apps Script builds the header from the first row. The saved file of a shuffled HiTOP-BR walk is copied into hitop's `tests/testthat/fixtures/` as LF with a provenance row, and a test reads it, checks `item_order` against the file's text, and scores it with `score_hitopbr()` against means recomputed per row from the file's text with `hitopbr_items$Reverse` and `hitopbr_scales$itemNumbers`.

## Coverage

- AC1 → T1
- AC2 → T2
- AC3 → T3
- AC4 → T3
- AC5 → T4

## Tasks

- [x] T1: `parseLink()` (`form.js:60`) reads `shuffle`; `planItems()` (`form.js:344`) returns the canonical list and a shown permutation; the render and the shuffle; the render tests of AC1.
- [x] T2: `buildCsv()` and `buildRow()` (`form.js:360`, `form.js:389`) write `item_order` and the canonical columns; the save and send tests of AC2.
- [ ] T3: The `link.html` checkbox and hint, `storeSql()` (`form.js:304`), the two SQL fixtures, the link and SQL tests of AC3 and AC4.
- [ ] T4: The grep sweep of AC5 across both repos, the README sections, the fixtures README, the captured fixture and its test in hitop on this milestone's branch.
- [ ] T5: The hitop-form PR, its CI and the dispatched deployed-page run after the merge; the hitop PR with the fixture, the test and tracking.

## Work log

- 2026-09-23: created by /milestone-plan, with M114 and M115. Depends on M115 for the reader and on M114 because both edit `itemNode()`.
- 2026-09-23: criteria audit ran in full mode on a fresh [O] reader (shared with M114 and M115): eleven findings on this file. Ten fixed in the wording: the "two loads differ" claim is asserted on the 45-item and 21-item cases only, the module case must differ between loads, the refusal is probed with a string, a number and `null`, both store routes get a walk, the Supabase test compares keys to SQL columns, two SQL fixtures, the prose claim rests on a grep, the README's new-sheet note, M115 as a dependency, and the package fixture as a task on hitop's own branch. One carried to the gate: the IP1 sign-off. The reader's objection to recomputing means from the package tables was not taken, on M113's AC2 precedent.
- 2026-09-24: from M115's review. `vignettes/pid5_scoring.Rmd` line 202 and `vignettes/articles/modules-hitopsr.Rmd` line 420 say a file has five lead columns. When the page writes `item_order`, those two sentences need the sixth.
- 2026-09-23: plan gate chose a fresh order per participant over one fixed order per link (or both), because counterbalancing order effects is what random order is for and a module's `itemOrder` already gives a fixed order; falsified by a researcher needing one fixed random order for a whole instrument.
- 2026-09-23: plan chose ignoring a module's `itemOrder` under `shuffle` over refusing the combination, because every builder-saved descriptor of a shuffled Word form carries one; falsified by a researcher surprised that the printed order was not followed.
- 2026-09-23: plan dropped the drafted start-screen sentence about random order, so no participant-facing text changes and IP1's text clause is not touched.
- 2026-09-23: IP1 sign-off: Jeff signed off at the plan gate on shuffling the display order, with the stored columns labelled by the official item number and the participant's display number saved in `item_order` (D-070). The escalation to a review brief offered at the gate was not taken.
- 2026-09-24: implement started. Branches `m116-form-random-order` in both repos. Question gate skipped: the field name, the column shape, the module rule and the fixture route are fixed by the criteria and D-070.
- 2026-09-24: T1 done in hitop-form. `parseLink()` refuses a `shuffle` outside the two booleans by name. `planItems(exp, module, shuffle)` returns `{ items, shown }`; `shuffleItems()` is a Fisher–Yates over `crypto.getRandomValues` with rejection above the largest multiple of the range. Four render tests (R7 to R9) and three refusal tests (G9). Plants: a no-op shuffle reds R7 and R8; an accept-anything field reds the three G9 tests. Suite 123 to 130, all green.
- 2026-09-24: T2 done in hitop-form. The record carries `items: plan.items` and, under shuffle, `itemOrder` from `plan.shown`; `buildCsv()` and `buildRow()` write `item_order` sixth when the record has one. `expectShuffled()` in helpers.mjs checks a header and a row by item number against the pattern at the shown position. Tests: S8 (HiTOP-BR and module saves), S9 (the committed capture `responses-hitopbr-shuffled.csv` agrees with its own `item_order`), T12 (webhook and Supabase posts). Plants: columns in shown order reds S8 and T12 (4 tests); a dropped `item_order` key reds both T12 tests. Suite 135 green.

## Decisions

## Review
