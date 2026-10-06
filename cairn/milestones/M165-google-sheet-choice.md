# M165: Google Sheet choice in the Study Link Builder

- **Status:** in-progress
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

- [ ] AC1: The "Where responses go" menu in hitop-form's `link.html` lists four choices in this order: "A file on the participant's device", "A Google Sheet", "Another web address" and "A Supabase table". "A Google Sheet" shows one "Web app URL" field. "Another web address" shows one "Web address" field, and its placeholder does not contain `script.google.com`. A link and a downloaded setup file made under each of the two web choices hold `store: {kind: 'webhook', url}`. Playwright tests select each of the four choices and assert which field group is visible. They also decode the link and the setup file for each web choice.
- [ ] AC2: Under "A Google Sheet", the builder shows the setup steps. The steps are: make a sheet, open Extensions and then Apps Script, and paste the script. Then deploy it as a web app that runs as you with access for anyone, and copy the URL that ends in `/exec`. The steps are not `.hint` elements and contain no term that D-083 retires. A "Copy the script" button puts the Apps Script code on the clipboard. A test captures the text that the button passes to `navigator.clipboard.writeText`. It asserts that the text equals the fenced `js` block in step 2 of the README's "Send responses to a Google Sheet" section. The comparison removes the block's 3-space list indent and ends the text with one newline.
- [ ] AC3: Under "A Google Sheet", "Make the link" and "Download the setup file" refuse an address that fails a check. The check parses the address with `URL` and requires the hostname `script.google.com` and a `pathname` that ends in `/exec`. The query and the fragment are ignored. The check runs after the existing `checkStore()` check, so an `http:` address gets the existing message. The refusal says that the field needs the web app URL that ends in `/exec`. It also names "Another web address" for other servers. Tests press "Make the link" for each address that must fail and assert the message. The failing addresses are `https://docs.google.com/spreadsheets/d/x/edit`, `https://script.google.com/home/projects/x/edit`, `https://script.google.com/macros/s/x/dev`, `https://script.google.com/macros/s/x/exec/`, `https://script.google.com.evil.org/macros/s/x/exec`, `https://script.googleusercontent.com/macros/echo?x=1` and `https://example.org/exec`. One of them is also tried with "Download the setup file". These addresses pass: `https://script.google.com/macros/s/x/exec`, `https://SCRIPT.GOOGLE.COM/macros/s/x/exec`, `https://script.google.com/a/macros/example.edu/s/x/exec` and `https://script.google.com/macros/s/x/exec?y=1`. Under "Another web address", `https://example.org/rows` passes.
- [ ] AC4: Take a link or a setup file whose `store` is a web address. If the address passes AC3's check, the builder opens with "A Google Sheet" chosen. Otherwise it opens with "Another web address" chosen. The address notice labels the address "Web app URL" or "Web address" to match. A link made under "Another web address" with an address that passes AC3's check therefore reopens as "A Google Sheet". Tests open a `c` link and a setup file for each of the two choices, and assert the chosen option and the notice line.
- [ ] AC5: `grep -rF '"A web address"'` over hitop-form's `link.html` and `README.md` and hitop's `vignettes/` and `README.Rmd` returns no line. The `#destHint` text names "A Google Sheet", and a test asserts it. These sections tell the reader to choose "A Google Sheet" and paste the `/exec` URL into "Web app URL": hitop-form's README "Where responses go" section and step 4 of "Send responses to a Google Sheet", and hitop's `online-collection.Rmd` step 2. The sentences in `pid5_scoring.Rmd` and `modules-hitopsr.Rmd` that call a Google Sheet's script "one such web address" name the "A Google Sheet" choice.
- [ ] AC6: hitop's NEWS.md has an entry for the new choice. The hitop-form Playwright suite passes. `devtools::check()` on hitop reports 0 errors and 0 warnings.

## Coverage

- AC1 → T1
- AC2 → T2
- AC3 → T3
- AC4 → T1
- AC5 → T4
- AC6 → T4

## Tasks

- [x] T1: In hitop-form `link.html`, split the menu. The new option value maps to kind `webhook` in `readForm()` (near line 1715). Add the two field groups, update `showKind()` (near line 353), and choose the option from the address at prefill (near line 1230). Change the old `Web address: https://script.google.com/...` assertions (`link.spec.js`, `link-consent.spec.js`) to "Web app URL". Write the AC1 and AC4 tests.
- [ ] T2: Add the setup steps and the "Copy the script" button under "A Google Sheet", following the "Copy the SQL" button (near line 1782). Keep the script text in `link.html`. Write the test that compares it with the README block (AC2).
- [ ] T3: Add the Google address check in `readForm()` after `checkStore()`, with the refusal at the "Web app URL" field. Write the AC3 tests.
- [ ] T4: Rewrite the `#destHint` text (40 words or fewer, no retired term) and update the pinned hint test (`link-sections.spec.js` near line 1000). Update the README sections, the three hitop tutorials and NEWS.md. Run the Playwright suite and `devtools::check()` (AC5, AC6).

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
