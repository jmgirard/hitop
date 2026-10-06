# M165: Google Sheet choice in the Study Link Builder

- **Status:** review
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** GP3
- **Resolves:** —
- **Surface tier:** user-facing — the Study Link Builder page and the tutorials that researchers read
- **Branch/PR:** m165-google-sheet-choice, companion: /Users/jmgirard/github/hitop-form m165-google-sheet-choice

## Goal

The Study Link Builder offers "A Google Sheet" as a destination, with its setup steps and script on the same page.

## Scope

**In:**
- hitop-form `link.html`: the "Where responses go" menu gets "A Google Sheet" with a "Web app URL" field. "A web address" becomes "Another web address".
- Under "A Google Sheet": short setup steps, a "Copy the script" button, and a check of the address.
- The link format does not change. Both web choices write `store: {kind: 'webhook', url}`, so old links and setup files still open.
- Text: the menu hint, the hitop-form README, and the hitop tutorials `online-collection.Rmd`, `pid5_scoring.Rmd` and `modules-hitopsr.Rmd`. NEWS.md.

**Out:**
- The online form (`form.js` and `index.html`) does not change, because the link format does not change.
- The Supabase choice does not change.

## Acceptance criteria

- [x] AC1: The "Where responses go" menu in hitop-form's `link.html` lists four choices in this order: "A file on the participant's device", "A Google Sheet", "Another web address" and "A Supabase table". "A Google Sheet" shows one "Web app URL" field. "Another web address" shows one "Web address" field, and its placeholder does not contain `script.google.com`. A link and a downloaded setup file made under each of the two web choices hold `store: {kind: 'webhook', url}`. Playwright tests select each of the four choices and assert which field group is visible. They also decode the link and the setup file for each web choice.
- [x] AC2: Under "A Google Sheet", the builder shows the setup steps. The steps are: make a sheet, open Extensions and then Apps Script, and paste the script. Then deploy it as a web app that runs as you with access for anyone, and copy the URL that ends in `/exec`. The steps are not `.hint` elements and contain no term that D-083 retires. A "Copy the script" button puts the Apps Script code on the clipboard. A test captures the text that the button passes to `navigator.clipboard.writeText`. It asserts that the text equals the fenced `js` block in step 2 of the README's "Send responses to a Google Sheet" section. The comparison removes the block's 3-space list indent and ends the text with one newline.
- [x] AC3: Under "A Google Sheet", "Make the link" and "Download the setup file" refuse an address that fails a check. The check parses the address with `URL` and requires the hostname `script.google.com` and a `pathname` that ends in `/exec`. The query and the fragment are ignored. The check runs after the existing `checkStore()` check, so an `http:` address gets the existing message. The refusal says that the field needs the web app URL that ends in `/exec`. It also names "Another web address" for other servers. Tests press "Make the link" for each address that must fail and assert the message. The failing addresses are `https://docs.google.com/spreadsheets/d/x/edit`, `https://script.google.com/home/projects/x/edit`, `https://script.google.com/macros/s/x/dev`, `https://script.google.com/macros/s/x/exec/`, `https://script.google.com.evil.org/macros/s/x/exec`, `https://script.googleusercontent.com/macros/echo?x=1` and `https://example.org/exec`. One of them is also tried with "Download the setup file". These addresses pass: `https://script.google.com/macros/s/x/exec`, `https://SCRIPT.GOOGLE.COM/macros/s/x/exec`, `https://script.google.com/a/macros/example.edu/s/x/exec` and `https://script.google.com/macros/s/x/exec?y=1`. Under "Another web address", `https://example.org/rows` passes.
- [x] AC4: Take a link or a setup file whose `store` is a web address. If the address passes AC3's check, the builder opens with "A Google Sheet" chosen. Otherwise it opens with "Another web address" chosen. The address notice labels the address "Web app URL" or "Web address" to match. A link made under "Another web address" with an address that passes AC3's check therefore reopens as "A Google Sheet". Tests open a `c` link and a setup file for each of the two choices, and assert the chosen option and the notice line.
- [x] AC5: `grep -rF '"A web address"'` over hitop-form's `link.html` and `README.md` and hitop's `vignettes/` and `README.Rmd` returns no line. The `#destHint` text names "A Google Sheet", and a test asserts it. These sections tell the reader to choose "A Google Sheet" and paste the `/exec` URL into "Web app URL": hitop-form's README "Where responses go" section and step 4 of "Send responses to a Google Sheet", and hitop's `online-collection.Rmd` step 2. The sentences in `pid5_scoring.Rmd` and `modules-hitopsr.Rmd` that call a Google Sheet's script "one such web address" name the "A Google Sheet" choice.
- [x] AC6: hitop's NEWS.md has an entry for the new choice. The hitop-form Playwright suite passes. `devtools::check()` on hitop reports 0 errors and 0 warnings.

## Coverage

- AC1 → T1
- AC2 → T2
- AC3 → T3
- AC4 → T1
- AC5 → T4
- AC6 → T4

## Tasks

- [x] T1: In hitop-form `link.html`, split the menu. The new option value maps to kind `webhook` in `readForm()` (near line 1715). Add the two field groups, update `showKind()` (near line 353), and choose the option from the address at prefill (near line 1230). Change the old `Web address: https://script.google.com/...` assertions (`link.spec.js`, `link-consent.spec.js`) to "Web app URL". Write the AC1 and AC4 tests.
- [x] T2: Add the setup steps and the "Copy the script" button under "A Google Sheet", following the "Copy the SQL" button (near line 1782). Keep the script text in `link.html`. Write the test that compares it with the README block (AC2).
- [x] T3: Add the Google address check in `readForm()` after `checkStore()`, with the refusal at the "Web app URL" field. Write the AC3 tests.
- [x] T4: Rewrite the `#destHint` text (40 words or fewer, no retired term) and update the pinned hint test (`link-sections.spec.js` near line 1000). Update the README sections, the three hitop tutorials and NEWS.md. Run the Playwright suite and `devtools::check()` (AC5, AC6).

## Work log

- 2026-10-05: created by /milestone-plan.
- 2026-10-05: collision sweep: no ROADMAP row, archive entry or D-entry covers a Google Sheet choice. D-083 governs the new text: "Where responses go" stays, and "store" and "endpoint" stay out. One open issue (hitop #87, hosting data or links) does not overlap.
- 2026-10-05: question set: menu shape? Answer: "A Google Sheet" as its own choice. Setup help in the builder? Answer: steps and a "Copy the script" button. Address check? Answer: refuse an address that is not an Apps Script `/exec` URL.
- 2026-10-05: plan gate chose a separate "A Google Sheet" choice over one renamed choice and over a hint rewrite. Only a separate choice can show Sheet-only help and a Sheet-only check. Falsified by users who pick the wrong one of the two web choices.
- 2026-10-05: plan gate chose a copy of the script in `link.html`, kept equal to the README by a test, over a link to the README only. Falsified by the two copies drifting in a way the test cannot see.
- 2026-10-05: plan gate chose to refuse a non-`/exec` address over a warning. Falsified by a working Apps Script web app URL that the check refuses.
- 2026-10-05: plan chose to keep the link format (both web choices write kind `webhook`) over a new kind. The online form and old links then do not change. Falsified by a need to tell the two choices apart in the response data.
- 2026-10-05: criteria audit (full mode, fresh Opus reader) returned findings on AC1 to AC5, all applied. AC1 names the placeholder test and decodes setup files too. AC2 names the README block, the indent rule and the clipboard capture, and keeps the steps out of `.hint`. AC3 parses with `URL` and adds probes for case, look-alike hosts, `/dev`, a trailing slash, a query, `http:` and googleusercontent. AC4 covers `c` links and setup files and states the round trip. AC5 uses `grep -rF` and adds the hint and the two other tutorials. AC6 stays as a gate.
- 2026-10-05: branches cut in hitop and hitop-form. The untracked `devel/hitopdat_*` files in hitop are not this milestone's and stay unstaged.
- 2026-10-05: T1 done. The new option value is `sheet`, the field `sheetUrl`, and `isSheetUrl()` picks the choice at prefill. The "Another web address" placeholder is `https://example.org/responses`. New `tests/link-sheet.spec.js` (G1 to G3). Five old tests now expect the sheet choice. A plant that made `isSheetUrl()` return false turned 3 of the 8 new tests red. The builder test files pass (331).
- 2026-10-05: T2 done. The steps are an `ol#sheetSteps`, the script a `<script type="text/plain" id="sheetScript">` in `#sheetFields`, and a failed copy points to the README. Test G4 stubs `navigator.clipboard.writeText` and compares the text with the README block. A plant that changed `MAX_KEYS` in the page's copy turned G4 red. `link-sections.spec.js` and `link.spec.js` pass (263).
- 2026-10-05: T3 done. `readForm()` runs `isSheetUrl()` on the URL that `checkStore()` returns, so the host is read in lower case. Test G5 covers the 7 refused and 4 taken addresses of AC3, the setup-file download, and the `http:` order. Plants that loosened the host test or the `/exec` test each turned one G5 probe red. `link-sheet.spec.js` passes (23).
- 2026-10-05: T4 done. New hint, README "Where responses go" and step 4, `online-collection.Rmd` intro and steps 1 and 2, `pid5_scoring.Rmd`, `modules-hitopsr.Rmd`, NEWS entry. The full suite showed that S12 (`link-sections.spec.js`) lists every `refuseAt()` call, so the new refusal got a row there. The AC5 grep returns no line. Full Playwright suite: 1058 passed, 5 skipped. `devtools::check()`: 0 errors, 0 warnings, 0 notes.
- 2026-10-05: the full Playwright run after the T4 commit gave 1059 passed and none skipped. The line above, which says 1058 passed and 5 skipped, gave the earlier run's skip count.
- 2026-10-05: claim audit: 52 claims read, 2 corrected — hitop-form README.md (the localhost exception also covers "A Supabase table"), online-collection.Rmd (the field takes only a `script.google.com` address whose path ends in `/exec`). The audit also noted that the README test table had no row for `link-sheet.spec.js`. The row is added.
- 2026-10-06: step-7 approval: m165-google-sheet-choice approved for merge, with the companion /Users/jmgirard/github/hitop-form m165-google-sheet-choice merged first.

## Decisions

## Review

Pass 1, 2026-10-05. The evidence runs were on hitop `0a68f55e` and hitop-form `93d1be3`. Neither default branch moved after the branches were cut.

- AC1: `npx playwright test tests/link-sheet.spec.js` (23 passed). G1 asserts the four options as `[value, text]` pairs in order, the one visible field group per choice, the "Web app URL" and "Web address" labels, and a "Web address" placeholder without `script.google.com`. G2 decodes the link and the downloaded setup file under `sheet` and `webhook` to `{instrument, study, store: {kind: 'webhook', url}}`.
- AC2: same run. G4 asserts the four step texts of `#sheetSteps`, no `.hint` around or inside them, and no retired term in `#sheetFields`. It captures the one `navigator.clipboard.writeText` call and finds it equal to the README's step-2 `js` block with the 3-space indent removed and one final newline. A plant in pass 0 (T2) that changed `MAX_KEYS` turned it red.
- AC3: same run. G5 presses "Make the link" for the 7 addresses AC3 lists and asserts the exact refusal, focus on "Web app URL" and no result. One address is also refused by "Download the setup file", and nothing is saved. `http://script.google.com/macros/s/x/exec` gets the online form's `its url` message, not the sheet refusal. The 4 AC3 addresses that must pass make a link. Under "Another web address", `https://example.org/rows` makes a link.
- AC4: same run. G3 opens a `c` link and a setup file for each choice. A web app's address opens with `sheet` chosen and `Web app URL: <url>` in the notice. `https://example.org/rows` opens with `webhook` chosen and `Web address: <url>`. The round-trip test makes a link under "Another web address" with a web app's address and reopens it with `sheet` chosen.
- AC5: `grep -rnF '"A web address"'` over the four named paths printed nothing (exit 1). `link-sections.spec.js` "the hints keep the facts a researcher acts on" passed, and it asserts that `#destHint` contains "A Google Sheet, or another web address, ...". Read on disk: README "Where responses go" names "A Google Sheet" and "Web app URL", and README step 4 says to choose "A Google Sheet" and paste into "Web app URL". `online-collection.Rmd` step 2 says the same. `pid5_scoring.Rmd:291` and `modules-hitopsr.Rmd:411` name the "A Google Sheet" choice.
- AC6: NEWS.md has the entry "**"A Google Sheet" is a choice under "Where responses go" in the Study Link Builder.**". Full hitop-form Playwright suite on `93d1be3`: 1059 passed. Again on `66aa8c2` after the fix-now commit: 1065 passed. `devtools::check()` on hitop: 0 errors, 0 warnings, 0 notes.

Gate: `cairn_validate.py` passed (33 advisory warnings, none new to this milestone). No principle changed, so `cairn_impact` was skipped. `devtools::document()` left no diff. `pkgdown::check_pkgdown()` found no problems. NEWS has the entry. `README.Rmd` is untouched. `devtools::check()` gave 0 errors, 0 warnings and 0 notes.

spawned: diff-bug, blame-history, prior-review

- diff-bug #1: S11 never checks that `resetForm()` hides `#sheetFields` — fix now, fixed 66aa8c2 (a plant that dropped `sheetFields` from the reset turned S11 red).
- diff-bug #2: README test row for `link-sections.spec.js` says "the web address field" — fix now, fixed 66aa8c2.
- diff-bug #3: "Copy the script" shows with no clipboard, and its failure path is untested — fix now, fixed 66aa8c2 (hidden like the other copy buttons, two new tests).
- diff-bug #4: the copy-failure message says the link is "below the button", far from `#err` — fix now, fixed 66aa8c2 (the message names the link). Its "stays after a later copy" half went to the follow-up row "Study Link Builder failure-path gaps".
- diff-bug #5: under "A Google Sheet", an `http:` address first gets the online form's localhost hint — reject, planned change (AC3 sets that order). The missing localhost probe was added, fixed 66aa8c2.
- diff-bug #6: `isSheetUrl()` ignores the port — fix now, fixed 66aa8c2 (a plant that removed the port test turned the new probe red). A bare `script.google.com/exec` passing is rejected: planned change (AC3's check as written).
- diff-bug #7: the setup-file download refusal test does not assert focus — fix now, fixed 66aa8c2.
- diff-bug #8: the steps are not tied to the menu by `aria-describedby` — follow-up, row "Study Link Builder failure-path gaps".
- diff-bug #9: "Copied" is not announced — follow-up, same row.
- diff-bug #10: a URL typed under one web choice stays in the hidden field — reject, false (the reviewer found no leak, and `readForm()` reads only the chosen field).
- diff-bug #11: NEWS quotes "A web address" — reject, false (it names the old label as history, outside AC5's paths).
- diff-bug #12: the steps do not mention Google's unverified-app warning — follow-up, same row (needs a watched deploy before the text names it).
- blame-history #1: S11 reset check, as diff-bug #1 — fixed 66aa8c2.
- blame-history #2: the hint lost M146's JSON-row against column contrast — fix now, fixed 66aa8c2 ("one JSON row per participant" restored, test updated).
- blame-history #3: L20 lost both completion URLs with a plain web address — fix now, fixed 66aa8c2.
- blame-history #4: L21 markup test never reaches "Web app URL" — fix now, fixed 66aa8c2.
- blame-history #5: L16 lost the plain web address under Prolific, and its header is stale — fix now, fixed 66aa8c2.
- blame-history #6: stale README lines 367, 1107 and 1108 — fix now, fixed 66aa8c2. Lines 8 and 163 rejected, false (they use "a web address" for what a link names, which stays true).
- blame-history #7: copy failure in `#err`, as diff-bug #4 — fixed 66aa8c2 and follow-up as there.
- blame-history #8: port and userinfo in `isSheetUrl()` — port fixed 66aa8c2. Userinfo rejected, false (`checkStore()` refuses credentials first, `guard.spec.js` tests it).
- prior-review #1: README rows for `link.spec.js` and `link-sections.spec.js` not updated — fix now, fixed 66aa8c2.
- prior-review #2: the copy catch branch is untested — fix now, fixed 66aa8c2.
- prior-review #3: the sheet check after `checkStore()` — reject, false (the reviewer judged it consistent, no defect named).
- prior-review #4: stale comments at `link.spec.js:61` and S11, and S11 plants miss `sheetFields` — fix now, fixed 66aa8c2.
- prior-review #5: README lines 8 and 163 say "a web address" — reject, false (as blame-history #6).

No finding shows a criterion failing, so no return. Fix-now commit 66aa8c2: the hitop-form suite passed 1065 after it.
