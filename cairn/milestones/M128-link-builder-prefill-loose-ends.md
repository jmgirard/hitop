# M128: hitop-form's `link.html` lists the addresses a study link filled, moves focus to its message after load, and the README names the host's view of `link.html?c=`

- **Status:** review
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

**In:** hitop-form `link.html`: a notice between the steps list and the form, filled by `prefill()`, emptied and hidden by `build()`. After load, focus goes to `#err` or the notice when one holds text. `tests/link.spec.js` tests. README: privacy paragraph, a notice-and-focus paragraph right after the prefill paragraph, `tests/link.spec.js` row. The page's intro line gains one sentence on what the host sees for a `c`. hitop: tracking only (no NEWS entry, plan gate).

**Out:** the form page's (`index.html`) own handling of a crafted link; M125's rejected findings F5, F7 to F9, F11 to F16, which stay rejected in M125's archive. Screen-reader speech checked in a real screen reader, beyond focus in Chromium: no row, as focus is the property the tests can show. The other link-builder and online-form rows keep their candidate rows.

## Acceptance criteria

- [x] AC1: When `link.html` loads with a `c` that `prefill()` accepts and that fills at least one of four address fields with a non-empty string, a notice placed between the steps list and `#f` lists each filled address after that field's label name as the form shows it (Address, Project URL, Completion URL, Completion URL after a saved file), and asks the researcher to check them before making a link. No notice shows for: a `c` filling none of the four (the instrument and a module only; a Supabase store with key and table and no `url`; a web store whose `url` is not a string), each refused load of the L18 tests, and a load with no `c`. `tests/link.spec.js` asserts the notice's text for each of the four fields filled alone, and for the most one `c` can carry (both completion URLs and one store address, run once with a web address and once with a Supabase project URL); its absence on each silent load above; and its position by bounding box.
- [x] AC2: A refused load whose `c` carries a completion URL and a module that makes `prefill()` throw after the completion URL is filled shows no notice, and focus is on `#err`, asserted in `tests/link.spec.js` as an added L18 load.
- [x] AC3: The notice writes each address as text. A `c` holding markup (`"><img src=x>`) and an entity (`&amp;`) in the completion URL, the completion URL after a saved file, and the store address (one run per store kind) shows each string verbatim in the notice, whose element structure matches that of a plain address and contains no `img`, asserted in `tests/link.spec.js`.
- [x] AC4: Pressing "Make the link" empties and hides the notice, asserted in `tests/link.spec.js` for a successful build and for the first refusal (the study left empty).
- [x] AC5: After a load, focus is on `#err` for each refused load of L18 (AC2's included), on the notice for each AC1 load that shows it, and on `document.body` for a load with no `c` and for each AC1 silent load that `prefill()` accepts, asserted in `tests/link.spec.js` through `document.activeElement` in Playwright's Chromium.
- [x] AC6: The hitop-form README's privacy paragraph states that opening `link.html?c=…` sends the config, a Supabase key included, to the host's request logs, as opening the study link does. The paragraph right after its prefill paragraph (the one opening "A study link's own `c` parameter") describes the notice and the focus move. Its `tests/link.spec.js` row names what the new tests assert. `link.html`'s intro line states that opening the builder with a `c` sends that `c` to the host.
- [ ] AC7: The hitop-form Playwright suite (`npx playwright test`) passes locally and on the hitop-form PR's CI, and hitop's `devtools::test()` is clean with Imports installed, or on the hitop PR's R-CMD-check CI.

## Coverage

- AC1 → T1, T4
- AC2 → T1, T2
- AC3 → T1
- AC4 → T1
- AC5 → T2, T4
- AC6 → T3
- AC7 → T3

## Tasks

- [x] T1: In `link.html`, add `<div id="prefilled" role="status" tabindex="-1" hidden>` between `ol.steps` and `#f`. `prefill()` collects the filled addresses and writes the notice only on its normal return, as text nodes, so the catch path at link.html:245 leaves none. `build()` empties and hides it first. Tests for AC1 to AC4 in `tests/link.spec.js`, beside L16 to L19, with the throwing load built by `page.addInitScript` as L18's seventh load is (M125 lesson: 16 KB address cap).
- [x] T2: `#err` takes `tabindex="-1"`. After the prefill block, focus `#err` when it holds text, else the notice when shown, else nothing. Extend the L18 loop and the AC1 loads with `document.activeElement` assertions (AC2, AC5).
- [x] T3: README (privacy paragraph, a notice-and-focus paragraph right after the prefill paragraph, `tests/link.spec.js` row) and the intro line at link.html:72, written against the page's observed behavior. Plants, each seen red then restored after the fix is staged (M118 lesson): the notice written through `innerHTML` (AC3), written from inside `text()` (AC2), keyed on `config.store` rather than a filled address (AC1), `build()` not clearing it (AC4), focus called only on `prefill()`'s normal return (AC5, AC2's load goes red). Full hitop-form suite; hitop `devtools::test()`. Jeff merges the hitop-form PR from his terminal (M116 lesson).
- [x] T4 (re-review S1, S3, S4): `address()` writes the field, then lists `f.elements[name].value` when it is non-empty, so the notice shows what each field holds. Tests: silent loads for `complete: "\n"` and a web `url: "\r\n"` (no notice, focus on `body`), and a shown load for a two-line `complete` whose line equals the joined field value. Plant: the notice listing the config string again, seen red. The `link.html` focus comment states focus only. The README test row names which six loads show the notice with focus asserted.

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
- 2026-09-27: implement resumed for the AC6 amendment. Both branches level with `origin/main`. No question gate: nothing open.
- 2026-09-27: re-audit: AC6 (full) — the branch meets it. "A paragraph after" matched any later paragraph, and "its prefill paragraph" had no anchor. Scope In still named the prefill paragraph. Optional: a content list, bounding the test-row clause.
- 2026-09-27: mini gate: Jeff chose the anchored sentence and the Scope change, and declined the content list and the test-row bound, both of which widen past the return. AC6's second sentence now reads "The paragraph right after its prefill paragraph (the one opening "A study link's own `c` parameter") describes the notice and the focus move." This wording replaces the clause the amendment-return line above recorded. Scope In and T3's wording changed to match (no work changed). The separate paragraph was kept because it reads as one topic.
- 2026-09-27: re-audit: AC6 (full) — nothing blocking. The branch meets the final text. The privacy paragraph and intro line have no locator but one reading each. The two declined bounds were noted again.
- 2026-09-27: hitop-form full suite at 00aed9e: 233 passed. hitop code unchanged since review's `devtools::test()` and `check()`. The claim audit above stands. 00aed9e only narrows three audited claims, and re-review reads it. Implement done. Status review.
- 2026-09-27: re-review: AC6 passes as amended. AC1 and AC5 fail: `address()` lists the config string, so a `c` whose completion URL is `"\n"` or whose web url is `"\r\n"` leaves the field empty yet shows the notice and focuses it. Defect return 1 (amendment returns: 1, on AC6). Jeff chose to fix the page. T4 added for S1, S3 and S4. Status in-progress.
- 2026-09-27: T4 done (hitop-form 4eb66c1). `address()` lists `f.elements[name].value` when non-empty. Tests: a two-line `complete` shown as the joined field value (the field checked too), and silent loads for `complete: "\n"` and a web `url: "\r\n"`. The three were red before the fix, each on the notice-lines assertion. Plant (config string listed again, fix staged first): the same three red, restored. Focus comment states focus only. README row: seven notice loads, eight silent loads, counted from `--list`. `tests/link.spec.js` 66 passed, full suite 236 passed.
- 2026-09-27: claim audit: 41 claims read, 1 corrected — hitop-form README.md, link.html, tests/link.spec.js (lines added by 00aed9e and 4eb66c1). Corrected: the L20 header's "with a non-empty string" (a line break is non-empty yet lists nothing). Also taken: the README focus sentence sharpened (S4's ambiguity) and the `NOTICE_SHOWN` comment naming the two-line case. The same reader re-read all three: they hold. hitop-form 8f3aeea, `tests/link.spec.js` 66 passed.
- 2026-09-27: implement done. 8f3aeea changes comments and README text only, after the full suite passed 236 at 4eb66c1. hitop R code unchanged. Status review.

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

Re-review 2026-09-27, after the AC6 amendment. Both `origin/main` heads are unmoved (hitop 0b7769b4, hitop-form 4bf0362), and no PR exists. hitop-form head 00aed9e. hitop's R code is unchanged since the first pass, so its `devtools::test()` and `check()` results above stand.

- AC6: pass against the amended text. The README paragraph opening "A study link's own `c` parameter" (lines 113 to 122) is followed, after one blank line, by the paragraph at lines 124 to 130. That paragraph names the four address fields, the notice between the steps and the form, "Check them", its removal on "Make the link", and focus moving to the refusal or the notice. The privacy paragraph (lines 132 to 143) and the `link.html` intro pass as recorded above. The test row now reads "on the notice in the six loads that show it, and on no element in the six loads that show neither".
- AC1 to AC5 at 00aed9e: the full suite `npx playwright test` passes 233 of 233 (1.3 m), all the tests named in the AC1 to AC5 lines above among them. 00aed9e changed only README text and one intro word, so those evidence lines stand at this head.
- AC1 and AC5: fail (found by the re-review diff-bug lens, reproduced by a scratch Playwright probe of the branch). `address()` (`link.html` line 245) lists the config string, not the value the field holds. A text input drops line breaks, so `complete: "\n"` or a web store `url: "\r\n"` leaves the field empty. The notice still shows "Completion URL: " or "Address: " and focus goes to `prefilled`. AC1 says a `c` filling none of the four shows no notice, and AC5 says such a load leaves focus on `body`. A two-line `complete` fills the field as one joined string while the notice shows the raw two lines. AC1 and AC5 were ticked at the first pass on the tests' loads, which have no such address. Both boxes are unticked at this pass.

Re-review consistency: `cairn_validate` exit 0. hitop R code unchanged, so `document()`, `check_pkgdown()` and `check()` above stand.

Re-review lenses. Prior-review: no finding, and R4, R7 and R8 landed. Blame-history: no finding, and 00aed9e's three edits trace to R4, R7 and R8. Diff-bug, ranked, with proposed dispositions:
- S1: the line-break address above. Proposed: return to in-progress. Write the field first, then list `f.elements[name].value` when non-empty. Add silent loads for `"\n"` and `"\r\n"` and a shown load whose line equals the joined field value.
- S2: AC7's CI half is still unread (as R1). Proposed: noted, read at step 8.
- S3: the `link.html` comment at line 283, "Focus is read out", keeps the screen-reader claim R4 removed from the README. Proposed: fix in the return pass.
- S4: the test row's "on the notice in the six loads that show it" can read as a count of every notice load, but L16, L21 and L22 also show it. Proposed: fix in the return pass by naming which six.
- S5: bidirectional control characters in an address show reordered in the notice, as in the field. Proposed: reject. The field shows the same, the host before the override stays readable, and no criterion covers display order.
- S6: R3 re-confirmed (a refused load scrolls to the refusal). Proposed: noted, R3's rejection stands.
- S7: R11 holds in Chromium only. An empty `p[tabindex=-1]` refuses focus there, so the `err.textContent` guard is untested in other engines. Proposed: noted. The plan tests focus in Chromium.
- S8: R4 unchanged (no accessible name). Proposed: noted, the Scope Out line stands.

Third pass 2026-09-27, after T4. Both `origin/main` heads are unmoved (hitop 0b7769b4, hitop-form 4bf0362), and no PR exists in either repo. hitop-form head 8f3aeea. Full suite `npx playwright test`: 236 passed (1.3 m).

- AC1: pass. `address()` (`link.html` lines 245 to 249) writes the field first. It then lists `f.elements[name].value` if that value is not empty. Seven shown loads pass. Four fill one field each. Two fill both completion URLs and one store address, once per store kind. One fills a two-line completion URL, and the notice lists the joined field value, which the test also reads from the field. Each shown load asserts the lead sentence, the lines, and the order steps, notice, `#f` by bounding box. Eight silent loads pass: no `c`, instrument only, instrument and module, Supabase with no url, web url `123`, empty completion URLs, `complete: "\n"`, web url `"\r\n"`. The eight L18 loads assert the notice hidden with no `li`. The four label names at `link.html` lines 118, 122, 135 and 141 match the test strings.
- AC5: pass. The focus assertions read `document.activeElement` in Chromium. The result is `err` in all eight L18 loads, AC2's load among them. It is `prefilled` in the seven shown loads of AC1. It is `body` in the eight silent loads of AC1, and the no-`c` load is one of them.
- AC2, AC3, AC4 and AC6 at 8f3aeea: the tests named in their lines above pass in the 236-test run. T4 changed only `address()`, one comment, the README test row and tests. The AC6 text read at re-review is unchanged apart from the test row.
- AC7 (local half): hitop-form `npx playwright test` 236 passed. hitop `devtools::test()` with Imports installed: no failures, the same 15 skips. The CI half is read at step 8.

Third-pass consistency: `cairn_validate` exit 0 (23 dangling-id and 1 staleness advisories, none new). `devtools::document()` leaves no diff. The hitop branch still changes only `cairn/` files, so the `check()` and `check_pkgdown()` results above stand.

Third-pass lenses. Prior-review: no finding. R4, R7, R8, S1, S3 and S4 landed, and both PR-comment probes are empty. Blame-history: no finding. The M125 try/catch, the submit-handler reach and the L16 round trip are intact. Diff-bug: no criterion fails. Its probes of `complete: "\r"`, a Supabase `url: "\n"` and `completeSaved: "\r"` each show no notice. Ranked findings, with proposed dispositions:
- T1: an address of only spaces is listed as a blank line (`Completion URL:  `) and takes focus, but `build()` trims it, so the link does not carry it (reproduced). Proposed: reject. AC1 requires a notice for a field filled with a non-empty string, and a space is one. A trim in `address()` would break AC1 as written.
- T2: a refused build (for example the study left empty) hides the notice while the filled addresses stay in the fields, so a second press makes a link with no list shown (reproduced). Proposed: reject. AC4 requires this, and the researcher saw the list before the first press.
- T3: the notice goes stale when the store kind changes or a listed field is edited before "Make the link" (reproduced). Proposed: reject. The lead sentence records what the opened link filled.
- T4: no test pins the space-only case either way. Proposed: reject with T1, as AC1 sets no rule beyond non-empty.
- T5: AC7's CI half is unread (as R1 and S2). Proposed: noted, read at step 8.
