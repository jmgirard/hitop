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

- [x] AC1: When `link.html` loads with a `c` that `prefill()` accepts and that fills at least one of four address fields with a non-empty string, a notice placed between the steps list and `#f` lists each filled address after that field's label name as the form shows it (Address, Project URL, Completion URL, Completion URL after a saved file), and asks the researcher to check them before making a link. No notice shows for: a `c` filling none of the four (the instrument and a module only; a Supabase store with key and table and no `url`; a web store whose `url` is not a string), each refused load of the L18 tests, and a load with no `c`. `tests/link.spec.js` asserts the notice's text for each of the four fields filled alone, and for the most one `c` can carry (both completion URLs and one store address, run once with a web address and once with a Supabase project URL); its absence on each silent load above; and its position by bounding box.
- [x] AC2: A refused load whose `c` carries a completion URL and a module that makes `prefill()` throw after the completion URL is filled shows no notice, and focus is on `#err`, asserted in `tests/link.spec.js` as an added L18 load.
- [x] AC3: The notice writes each address as text. A `c` holding markup (`"><img src=x>`) and an entity (`&amp;`) in the completion URL, the completion URL after a saved file, and the store address (one run per store kind) shows each string verbatim in the notice, whose element structure matches that of a plain address and contains no `img`, asserted in `tests/link.spec.js`.
- [x] AC4: Pressing "Make the link" empties and hides the notice, asserted in `tests/link.spec.js` for a successful build and for the first refusal (the study left empty).
- [x] AC5: After a load, focus is on `#err` for each refused load of L18 (AC2's included), on the notice for each AC1 load that shows it, and on `document.body` for a load with no `c` and for each AC1 silent load that `prefill()` accepts, asserted in `tests/link.spec.js` through `document.activeElement` in Playwright's Chromium.
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
- [x] T3: README (privacy paragraph, prefill paragraph, `tests/link.spec.js` row) and the intro line at link.html:72, written against the page's observed behavior. Plants, each seen red then restored after the fix is staged (M118 lesson): the notice written through `innerHTML` (AC3), written from inside `text()` (AC2), keyed on `config.store` rather than a filled address (AC1), `build()` not clearing it (AC4), focus called only on `prefill()`'s normal return (AC5, AC2's load goes red). Full hitop-form suite; hitop `devtools::test()`. Jeff merges the hitop-form PR from his terminal (M116 lesson).

## Work log

- 2026-09-27: created by /milestone-plan. Promotes the M125 candidate row (review F3, F10, F6), removed from the ROADMAP in the plan commit. Inbox sweep: one open hitop issue (#87), no overlap; no open PRs; hitop-form has none.
- 2026-09-27: criteria audit (full mode, [O] fresh reader): 13 findings, 11 repaired before the gate (all-four case impossible, label names, empty strings, notice position, a refused load carrying a completion URL, a store with no address, markup and entity probes on all four fields, hidden as well as emptied, the catch-path focus plant, hitop test route, the intro line), 2 needing no change. The gate's no-NEWS answer removed AC7's NEWS clause, a narrowing re-read against the audit's questions with no finding.
- 2026-09-27: plan gate chose a notice listing the filled addresses over leaving the store and completion fields unfilled on prefill, because unfilled fields break editing an existing link (M125's round trip); falsified by a report of a researcher building a link from a crafted `c` with the notice shown.
- 2026-09-27: plan gate chose moving focus to the load-time message over writing it after a delay, because focus is testable and a delayed write is not; falsified by a screen reader that skips a focused `role="alert"` or `role="status"` element at load.
- 2026-09-27: plan gate declined a hitop NEWS entry; the change is recorded in hitop-form's history only.
- 2026-09-27: implement started; branch `m128-link-builder-prefill-loose-ends` cut in hitop and hitop-form from pushed main. No implement question gate: nothing left open.
- 2026-09-27: T1 done. `link.html` gains `#prefilled` (a status notice between the steps and the form); `prefill()` returns the filled addresses and the notice is written only after it returns; `build()` empties and hides it. Tests: an eighth L18 load (throw after a completion URL), L20 (six shown, six silent), L21 (markup per store kind), L22 (build, empty-study refusal). `tests/link.spec.js` 63 passed.
- 2026-09-27: T2 done. `#err` takes `tabindex="-1"`; after `showKind()` focus goes to `#err` when it holds text, else to the notice when shown. L23 assertions (`document.activeElement`) added to the eight L18 loads and the twelve L20 loads. `tests/link.spec.js` 63 passed.
- 2026-09-27: T3 done. README: a notice-and-focus paragraph after the prefill paragraph, a privacy sentence on `link.html?c=`, the `tests/link.spec.js` row extended. `link.html` intro gains one sentence on the host seeing a `c`. Notice checked by eye in the browser pane. Five plants, fixes staged first and restored with `git checkout`: innerHTML (both L21 runs red), notice written as fields fill (the eighth L18 load red), Project URL keyed on the store (the no-url Supabase load red), no clear in `build()` (both L22 red), focus only on normal return (both throwing L18 loads red). One extra red under the innerHTML plant, the `/rest/v1/` suffix test, fetches the export and passed in the full run: a network failure, not the plant. hitop-form full suite 233 passed; hitop `devtools::test()` FAIL 0, PASS 20487, SKIP 15.
- 2026-09-27: claim audit: 47 claims read, 7 corrected — hitop-form link.html, README.md, tests/link.spec.js (the hitop diff adds nothing outside cairn/, so the audit read the hitop-form branch). Corrected: the intro line (the browser sends the address), the privacy sentence (same `c`, not "again"), "when the page opens" and "can read it out", the non-empty qualifier, the focus comment, "sent or pointed", the test row's last clause. One re-read by the same reader: all seven match; the qualifier's placement and one comment wrap fixed (hitop-form c523c6e). `tests/link.spec.js` 63 passed after.
- 2026-09-27: implement done; status review.
- 2026-09-27: review: AC1 to AC5 pass, AC7 local half passes, AC6 fails as written (the notice and focus text is a paragraph after the prefill paragraph, not in it). Jeff chose to amend the criterion. Accepted fix-now wording (R4, R7, R8) landed in hitop-form 00aed9e.
- 2026-09-27: amendment return: AC6 — "A paragraph after its prefill paragraph describes the notice and the focus move."
- 2026-09-27: status in-progress for the AC6 amendment alone. Re-review follows it.

## Decisions

## Review

Sync 2026-09-27: both branches are cut from their current `origin/main` (hitop 0b7769b4, hitop-form 4bf0362). Nothing to merge. hitop-form full suite `npx playwright test`: 233 passed (1.3 m). `tests/link.spec.js` lists 63 tests.

- AC1: pass. Six "lists it in a notice between the steps and the form" tests (each of the four fields alone, then both completion URLs with a web address and with a Supabase project URL) assert the lead sentence, the lines `<label>: <address>` and the bounding-box order steps, notice, `#f`. Six "shows no notice" tests (no `c`, instrument only, instrument and module, Supabase with key and table and no url, web url `123`, empty completion URLs) and the eight L18 loads assert it hidden with no `li`. The four label names read from `link.html` lines 118, 122, 135, 141 match the test strings.
- AC2: pass. The eighth L18 load, "a module that makes JSON.stringify throw after a completion URL is filled", passes. It asserts the full refusal text, every control at its no-`c` value, `#prefilled` hidden with no `li`, focus on `err`, and a live submit handler. In `prefill()` the `complete` field is written (line 249) before the module's `JSON.stringify` (line 254), so the load reaches the case the criterion names.
- AC3: pass. The two "addresses holding markup show verbatim as text in the notice" tests (webhook, supabase) put `"><img src=x>&amp;` in all three addresses. Each asserts the three lines verbatim, the `#prefilled *` tag list equal to a plain-address load, and no `img`. The notice lines are written with `li.textContent`.
- AC4: pass. "a built link empties and hides the notice" and "a refused build with the study empty empties and hides the notice" each assert `#prefilled` hidden with no `li` after the press. The second asserts the refusal "Give the study a name."
- AC5: pass. The focus assertions read `document.activeElement` in Chromium. The result is `err` in all eight L18 loads, `prefilled` in the six notice loads, and `body` in the six silent loads (no `c` among them).
- AC6: fails as written (corrected at review, first recorded as a pass). The clause "Its prefill paragraph describes the notice and the focus move" is not met. The prefill paragraph ("A study link's own `c` parameter, opened on `link.html`…", README lines 112 to 121) is unchanged. The notice and focus text is a separate paragraph after it (lines 123 to 129). The other three clauses pass. The privacy paragraph gains two sentences: `link.html?c=…` "sends the same `c` to the host", and "The config, a Supabase key included, then reaches the host's request logs from that request too." The `tests/link.spec.js` row names eight bad loads, the notice cases, markup, the build clear and focus. The `link.html` intro says the browser sends the `c` to the page's host.
- AC7 (local half): hitop-form `npx playwright test` 233 passed. hitop `devtools::test()` with Imports installed: no failures, the same 15 skips as at implement. The hitop-form PR's CI half is read at step 8, after the PR opens. The box is ticked then.

Consistency gate: `cairn_validate` exit 0 (23 dangling-id and 1 staleness advisories, all older than this branch). `devtools::document()` leaves no diff. The branch changes no file under `R/`, README, NEWS or `_pkgdown.yml`, so README sync and a NEWS entry do not apply (the plan gate declined NEWS). `pkgdown::check_pkgdown()`: no problems. `devtools::check()`: 0 errors, 0 warnings, 0 notes (4 m 6 s).

Independent review, three lenses (user-facing tier). Prior-review lens: no regression of M125's F1, F2 or F4, and M125's F3, F6 and F10 each closed. Both PR-comment probes empty. Blame-history lens: nothing undone, the M125 try/catch kept, no D-entry contradicted. Diff-bug lens, ranked, with proposed dispositions:
- R1: AC7's CI half is not shown yet, the PR not being open. Proposed: noted, read at step 8.
- R2: AC6 fails as written (see AC6 above). Proposed: return, disposition at the gate.
- R3: `err.focus()` scrolls a refused load to the bottom of the form, the h1 and steps out of view at 1280x720. Proposed: reject. The refusal is the message to read, and it shows beside "Make the link".
- R4: `#prefilled` and `#err` have an empty accessible name. A screen reader can also announce `#err` twice (not verified). Proposed: reject for this milestone. Screen-reader speech is out of scope by the plan. The README's "so a screen reader can read it out" is reworded to the tested fact (fix now).
- R5: a throw inside the `li` loop would leave a stale `li` in the hidden notice. Proposed: reject. Nothing in the loop can throw, and `build()` clears the list.
- R6: a whitespace-only address is listed. Proposed: reject. AC1 says non-empty, `build()` trims it, and the notice shows what the link filled.
- R7: the README test row says focus is on the notice "when it shows", but focus is asserted only in the six notice loads. Proposed: fix now, name the six loads.
- R8: after the new intro sentence, "For a Supabase table it downloads" reads as if "it" were the browser or host. Proposed: fix now, "the builder downloads".
- R9: the load-time focus draws the focus outline. Proposed: noted. The outline shows keyboard users where focus is.
- R10: L21 compares tag names in order, not nesting or attributes. Proposed: reject. It goes red on the `innerHTML` plant.
- R11: the body-focus checks cannot tell guarded focus calls from unguarded ones, as hidden elements refuse focus. Proposed: reject. Both pages behave the same.

Gate 2026-09-27: every proposed disposition accepted. R4's README rewording, R7 and R8 are fixed in hitop-form 00aed9e, and `tests/link.spec.js` passes 63 of 63 after it. R2 routed to an amendment of AC6's wording, not to a README change. The step-7 merge chip was not posed, as AC6 fails.
