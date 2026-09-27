# M128: hitop-form's `link.html` lists the addresses a study link filled, moves focus to its message after load, and the README names the host's view of `link.html?c=`

- **Status:** in-progress
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** —
- **Resolves:** —
- **Surface tier:** user-facing — a public web page researchers open, and its README
- **Branch/PR:** `m128-link-builder-prefill-loose-ends` (hitop and hitop-form)

## Goal

Close the three link-builder loose ends M125's review deferred (F3, F6, F10): a notice naming the addresses a study link's `c` filled, a focus move so a screen reader reads the load-time message, and README text on what the host sees when `link.html?c=` opens.

## Scope

**In:** hitop-form `link.html`: a notice between the steps list and the form, filled by `prefill()`, emptied and hidden by `build()`. After load, focus goes to `#err` or the notice when one holds text. `tests/link.spec.js` tests. README: privacy paragraph, prefill paragraph, `tests/link.spec.js` row. The page's intro line gains one sentence on what the host sees for a `c`. hitop: tracking only (no NEWS entry, plan gate).

**Out:** the form page's (`index.html`) own handling of a crafted link; M125's rejected findings F5, F7 to F9, F11 to F16, which stay rejected in M125's archive. Screen-reader speech checked in a real screen reader, beyond focus in Chromium: no row, as focus is the property the tests can show. The other link-builder and online-form rows keep their candidate rows.

## Acceptance criteria

- [ ] AC1: When `link.html` loads with a `c` that `prefill()` accepts and that fills at least one of four address fields with a non-empty string, a notice placed between the steps list and `#f` lists each filled address after that field's label name as the form shows it (Address, Project URL, Completion URL, Completion URL after a saved file), and asks the researcher to check them before making a link. No notice shows for: a `c` filling none of the four (the instrument and a module only; a Supabase store with key and table and no `url`; a web store whose `url` is not a string), each refused load of the L18 tests, and a load with no `c`. `tests/link.spec.js` asserts the notice's text for each of the four fields filled alone, and for the most one `c` can carry (both completion URLs and one store address, run once with a web address and once with a Supabase project URL); its absence on each silent load above; and its position by bounding box.
- [ ] AC2: A refused load whose `c` carries a completion URL and a module that makes `prefill()` throw after the completion URL is filled shows no notice, and focus is on `#err`, asserted in `tests/link.spec.js` as an added L18 load.
- [ ] AC3: The notice writes each address as text. A `c` holding markup (`"><img src=x>`) and an entity (`&amp;`) in the completion URL, the completion URL after a saved file, and the store address (one run per store kind) shows each string verbatim in the notice, whose element structure matches that of a plain address and contains no `img`, asserted in `tests/link.spec.js`.
- [ ] AC4: Pressing "Make the link" empties and hides the notice, asserted in `tests/link.spec.js` for a successful build and for the first refusal (the study left empty).
- [ ] AC5: After a load, focus is on `#err` for each refused load of L18 (AC2's included), on the notice for each AC1 load that shows it, and on `document.body` for a load with no `c` and for each AC1 silent load that `prefill()` accepts, asserted in `tests/link.spec.js` through `document.activeElement` in Playwright's Chromium.
- [ ] AC6: The hitop-form README's privacy paragraph states that opening `link.html?c=…` sends the config, a Supabase key included, to the host's request logs, as opening the study link does. Its prefill paragraph describes the notice and the focus move. Its `tests/link.spec.js` row names what the new tests assert. `link.html`'s intro line states that opening the builder with a `c` sends that `c` to the host.
- [ ] AC7: The hitop-form Playwright suite (`npx playwright test`) passes locally and on the hitop-form PR's CI, and hitop's `devtools::test()` is clean with Imports installed, or on the hitop PR's R-CMD-check CI.

## Coverage

- AC1 → T1
- AC2 → T1, T2
- AC3 → T1
- AC4 → T1
- AC5 → T2
- AC6 → T3
- AC7 → T3

## Tasks

- [x] T1: In `link.html`, add `<div id="prefilled" role="status" tabindex="-1" hidden>` between `ol.steps` and `#f`. `prefill()` collects the filled addresses and writes the notice only on its normal return, as text nodes, so the catch path at link.html:245 leaves none. `build()` empties and hides it first. Tests for AC1 to AC4 in `tests/link.spec.js`, beside L16 to L19, with the throwing load built by `page.addInitScript` as L18's seventh load is (M125 lesson: 16 KB address cap).
- [x] T2: `#err` takes `tabindex="-1"`. After the prefill block, focus `#err` when it holds text, else the notice when shown, else nothing. Extend the L18 loop and the AC1 loads with `document.activeElement` assertions (AC2, AC5).
- [ ] T3: README (privacy paragraph, prefill paragraph, `tests/link.spec.js` row) and the intro line at link.html:72, written against the page's observed behavior. Plants, each seen red then restored after the fix is staged (M118 lesson): the notice written through `innerHTML` (AC3), written from inside `text()` (AC2), keyed on `config.store` rather than a filled address (AC1), `build()` not clearing it (AC4), focus called only on `prefill()`'s normal return (AC5, AC2's load goes red). Full hitop-form suite; hitop `devtools::test()`. Jeff merges the hitop-form PR from his terminal (M116 lesson).

## Work log

- 2026-09-27: created by /milestone-plan. Promotes the M125 candidate row (review F3, F10, F6), removed from the ROADMAP in the plan commit. Inbox sweep: one open hitop issue (#87), no overlap; no open PRs; hitop-form has none.
- 2026-09-27: criteria audit (full mode, [O] fresh reader): 13 findings, 11 repaired before the gate (all-four case impossible, label names, empty strings, notice position, a refused load carrying a completion URL, a store with no address, markup and entity probes on all four fields, hidden as well as emptied, the catch-path focus plant, hitop test route, the intro line), 2 needing no change. The gate's no-NEWS answer removed AC7's NEWS clause, a narrowing re-read against the audit's questions with no finding.
- 2026-09-27: plan gate chose a notice listing the filled addresses over leaving the store and completion fields unfilled on prefill, because unfilled fields break editing an existing link (M125's round trip); falsified by a report of a researcher building a link from a crafted `c` with the notice shown.
- 2026-09-27: plan gate chose moving focus to the load-time message over writing it after a delay, because focus is testable and a delayed write is not; falsified by a screen reader that skips a focused `role="alert"` or `role="status"` element at load.
- 2026-09-27: plan gate declined a hitop NEWS entry; the change is recorded in hitop-form's history only.
- 2026-09-27: implement started; branch `m128-link-builder-prefill-loose-ends` cut in hitop and hitop-form from pushed main. No implement question gate: nothing left open.
- 2026-09-27: T1 done. `link.html` gains `#prefilled` (a status notice between the steps and the form); `prefill()` returns the filled addresses and the notice is written only after it returns; `build()` empties and hides it. Tests: an eighth L18 load (throw after a completion URL), L20 (six shown, six silent), L21 (markup per store kind), L22 (build, empty-study refusal). `tests/link.spec.js` 63 passed.
- 2026-09-27: T2 done. `#err` takes `tabindex="-1"`; after `showKind()` focus goes to `#err` when it holds text, else to the notice when shown. L23 assertions (`document.activeElement`) added to the eight L18 loads and the twelve L20 loads. `tests/link.spec.js` 63 passed.

## Decisions

## Review
