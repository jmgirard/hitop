# M154: "HiTOP-SR module" as an instrument choice

- **Status:** review
- **Priority:** high
- **Depends on:** M152
- **Driving RR:** —
- **Principles touched:** —
- **Resolves:** —
- **Surface tier:** user-facing — changes how researchers choose a HiTOP-SR module in the Study Link Builder
- **Branch/PR:** m154-study-link-module-choice; companion: /Users/jmgirard/github/hitop-form m154-study-link-module-choice; companion: /Users/jmgirard/github/hitop-builder m154-study-link-module-choice

## Goal

A researcher who wants a HiTOP-SR module chooses "HiTOP-SR module" in the instrument list and gives its module file in that row.

## Scope

**In:** hitop-form `link.html` (the instrument rows, the renamed "Item order" section, the fill from a link, the refusals), its tests and README, hitop-builder's README, the hitop article, NEWS, and DESIGN.md Known issue 11. The study link's format does not change.

**Out:**
- Modules for other instruments wait on the "Generalize modularization" candidate row.
- The online form's handling of a module beside no HiTOP-SR stays as Known issue 11 records it.

## Acceptance criteria

- [x] AC1: Each instrument row's list offers "HiTOP-SR module" after "HiTOP-SR". A row set to it shows, inside the row, the "Module file" box and the "Choose the module file" control, which leave the optional section. That section is renamed "Item order" and holds only "Show the items in a random order". Only a row set to "HiTOP-SR module" writes its box into the link. A row switched away keeps its text out of view, and switching it back shows the text again. "Add an instrument" skips both HiTOP-SR entries when either is listed. Playwright tests cover each.
- [x] AC2: A "HiTOP-SR module" row writes the link as HiTOP-SR plus a module file writes it today. A test builds three fixed `c` setups (the module alone; a list with the module second; the module with a random order) and compares each link's query with the query that hitop-form `main` at commit `2af14f0` builds for the same setup, hardcoded in the test. Two rows set to HiTOP-SR and HiTOP-SR module, or two module rows, are refused as a repeated instrument, naming the second row. A module row with an empty box is refused, focusing its box.
- [x] AC3: Opening link.html with a `c`, `z` or `setup` link holding `hitopsr` and a `module` fills a "HiTOP-SR module" row with the module. Without `module`, it fills a "HiTOP-SR" row. A link with a `module` and no `hitopsr` is refused with "The link holds a module file but no HiTOP-SR." A test opens a `c` link of the shape the Module Builder's "Open the Study Link Builder" link writes (`{instrument: "hitopsr", module}`, in hitop-builder's `index.html`) and finds the module row filled.
- [x] AC4: A search with whitespace normalized for "Item order and HiTOP-SR module", and for the anchor `item-order-and-hitop-sr-module`, in hitop-form, hitop-builder and hitop's `vignettes/`, finds nothing. hitop's NEWS.md keeps its released entries as history. hitop-form's README, hitop-builder's README and the hitop article describe the new choice. Every hint in link.html is at most 40 words, the in-row module hint included. DESIGN.md Known issue 11 says the builder now takes a module file only in a "HiTOP-SR module" row, while the online form still checks a descriptor's `instrument` only against the link's.
- [x] AC5: hitop's NEWS.md names the new choice. hitop-form's and hitop-builder's Playwright suites pass. In hitop, `devtools::check()` reports 0 errors and 0 warnings, and `pkgdown::build_article("articles/online-collection")` renders without error.

## Coverage

- AC1 → T1, T4
- AC2 → T2, T4
- AC3 → T3, T4
- AC4 → T5
- AC5 → T4, T6

## Tasks

- [x] T1: In `link.html`, add "HiTOP-SR module" to `OFFERED`, move the module box and file control into the row, rename the section, and make "Add an instrument" skip both HiTOP-SR entries.
- [x] T2: Write `instrument`/`instruments` and `module` from a module row, with the repeated-instrument and empty-box refusals. Take the three expected queries from `main` at `2af14f0` before changing the build.
- [x] T3: Fill a module row from `c`, `z` and `setup` links, and refuse a module beside no HiTOP-SR.
- [x] T4: Rewrite the module tests (`link-module-file.spec.js`, `link-sections.spec.js`, `link.spec.js`, `link-instruments.spec.js`), add the in-row hint to S8's walk, and add the AC2 and AC3 tests.
- [x] T5: Update hitop-form's README, hitop-builder's README (lines 244 to 245 and the anchor), the hitop article and Known issue 11. Run AC4's search.
- [x] T6: hitop NEWS line, `pkgdown::build_article()`, `devtools::check()`, and the companion PRs.

## Work log

- 2026-10-01: created by /milestone-plan with M152 and M153. Jeff found choosing HiTOP-SR and then giving the module in a separate section awkward. Audits are recorded in M152's work log.
- 2026-10-01: plan chose "HiTOP-SR module" as its own list entry over a "Use a module" box inside the HiTOP-SR row, because Jeff asked for a separate choice. Falsified by researchers who look for the module under HiTOP-SR and miss the entry.
- 2026-10-01: implement started. Branches cut in hitop, hitop-form and hitop-builder. Gate: Jeff chose the entry text "HiTOP-SR module (scales you choose)", a README subsection "A HiTOP-SR module", and a repeat refusal that adds "A HiTOP-SR module is the HiTOP-SR, so a list holds one or the other." when one row of the pair is a module row.
- 2026-10-01: AC2 oracle taken from hitop-form `2af14f0` (git archive, the old builder driven by Playwright) for the three setups, study names "module alone", "module second" (rows HiTOP-BR, HiTOP-SR) and "module shuffled", each with `tests/fixtures/module-plain.json` pasted.
- 2026-10-01: T1 to T3 code checkpointed in hitop-form `28161c6`. The suite ran 930 of 966 passing, and the 36 failures are the old module-section tests that T4 rewrites, so T1 to T3 stay unticked until T4.
- 2026-10-01: T4 delegated to a Sonnet subagent (rewrite the 36 tests, add the AC1 to AC3 tests). T5 docs written meanwhile: hitop-form README (new "A HiTOP-SR module" subsection, "Item order"), hitop-builder README `c1cd0ea`, both hitop articles (`modules-hitopsr.Rmd` also named the old section), Known issue 11. T6 NEWS line written. Both articles render with `pkgdown::build_article()`.
- 2026-10-01: Sonnet subagent's T4 diff reviewed and committed with the README test rows in hitop-form `4c3c0e8`. It rewrote tests in five spec files (`link-setupfile.spec.js` too) and added LI6 to LI9 and MF8. It saw 34 failures at its start, not 36: `consent.spec.js` held no module test, so its failure in the first run was a flake. It made the oracle and refusal tests red with one changed character, and restored them. My own full run: 1000 of 1000 pass.
- 2026-10-01: AC4 search over 101 files of hitop-form, hitop-builder and hitop `vignettes/`, whitespace normalized: 0 hits. The pre-edit `online-collection.Rmd` holds the phrase, so the search can find it. `devtools::check()`: 0 errors, 0 warnings, 0 notes. T1 to T6 ticked. The companion PRs open at review's merge step, as the git model says.
- claim audit: 95 claims read, 3 corrected — hitop-form README.md, hitop-form link.html
- 2026-10-01: the claim audit (fresh Opus reader, all three repos' added lines) found `fill()` writing the module box before the study and completion URL, so the two BAD_C "throw after … is filled" tests reached no filled field. hitop-form `aea58f0` moves the fill back to main's place and rewords two README claims. The same reader re-read all three as true. Suite 1000 of 1000. Status set to review.

## Decisions

## Review

Review run 2026-10-01. All three branches were level with their `main` after a fetch, 0 commits behind, so no merge was needed.

- AC1 evidence: the full hitop-form suite at `aea58f0` passed 1000 of 1000 in 2.3 minutes. A first run beside `devtools::check()` passed 998. Its two failures were timeouts waiting for "Begin" in `save.spec.js` and `send.spec.js`. The branch does not change those specs, and they then passed 61 of 61. The AC1 tests in `link-instruments.spec.js` all passed. They cover the menu order, and the box and file control in a module row only. They cover "Item order" with only the shuffle box, and a module row alone writing its box. They cover the text shown again after a switch, and "Add an instrument" from 3 listed shapes.

- AC2 evidence: in the same 1000-test run, the three LI7 setups passed. They compare each built query with the query hardcoded from hitop-form `main` at `2af14f0` (`link-instruments.spec.js:382`). The fresh diff reviewer diffed `2af14f0..e6a38be` and found no change to how the config is assembled. The oracle therefore still holds at the branch base. The repeat refusal passed for 3 row shapes, each focusing row 2's menu. The empty-box refusal passed for 4 cases, each naming its row and focusing its box.
- AC3 evidence: in the same run, a `c`, a `z` and a setup file holding a module each filled one module row. Each also filled row 2 of a list as the module row. Without a module, each filled a plain HiTOP-SR row. The refusal of a module with no HiTOP-SR passed for 6 cases, 2 per link kind. The test that opens a `c` of the Module Builder's shape found the module row filled. The diff reviewer read hitop-builder's `showNextStep()` and found it writes `{instrument: 'hitopsr', module}`.
- AC4 evidence: the search covered 101 files in hitop-form, hitop-builder and hitop `vignettes/`. It looked for "Item order and HiTOP-SR module" and `item-order-and-hitop-sr-module`, with whitespace normalized, and found 0 hits. The same search finds the phrase twice in `main`'s `online-collection.Rmd`. The NEWS diff is one hunk at line 2, in the development block. The released 0.2.0 entries are not changed. hitop-form's README, hitop-builder's README and both hitop articles name the "HiTOP-SR module" row. A scratch Playwright script set row 1 to the module on the served page. It counted 25 hints, the longest at 40 words. The two in-row hints have 28 and 9 words. Known issue 11 says the builder takes a module only in a "HiTOP-SR module" row. It says the online form's `checkModule()` checks `instrument` only against the link's.
- AC5 evidence: NEWS.md's new first bullet under "New features" names "HiTOP-SR module (scales you choose)". hitop-form's suite passed 1000 of 1000, as AC1 records. hitop-builder's `tests/smoke.spec.js` at `c1cd0ea` passed 2 of 2 in 34 seconds, and its `npm run prose` passed with no retired names. Its CI runs those two. In hitop, `devtools::check()` reported 0 errors, 0 warnings and 0 notes. `pkgdown::build_article()` rendered `online-collection` and `modules-hitopsr` with no error. Each rendered page holds the new choice's text once.
- Consistency gate: `cairn_validate.py` exited 0, with only the standing advisories (23 dangling ids, 1 references staleness). `devtools::document()` left `NAMESPACE`, `man/` and `R/` unchanged. `pkgdown::check_pkgdown()` found no problems. The branch does not touch README.Rmd or add a top-level file. NEWS has an entry. `check()` is clean, as AC5 records. No principle changed, so `cairn_impact.py` was skipped.
- Independent review: three fresh reviewers ran, an Opus diff reviewer and two Sonnet reviewers (blame history, prior reviews). The prior-review probe found no GitHub review comments in any of the three repos. None of the findings shows a criterion failing. The 14 merged findings follow, most severe first, with the proposed disposition.
  - F1 (diff 1, blame 1): `tests/link-setupfile.spec.js:428-431` checks the new rows after "Fill in the form from the current file". Without the `!row.isConnected` guard at `link.html:510`, the stale read writes into the detached row and calls `hideResult()`, and the test stays green. Proposed: fix now. The new test builds a link while the read waits, then checks that the link stays shown.
  - F2 (diff 2, prior 1): NEWS.md:64 and NEWS.md:679-680 are unreleased entries. They name the old "Item order and HiTOP-SR module" section and the removed hint sentence about several instruments. Proposed: fix now.
  - F3 (diff 3, blame 5): the in-row module box, file control, alert and status carry no row number. Two module rows give two identical "Module file" names. Proposed: follow-up in the Study Link Builder edge-cases row.
  - F4 (diff 4, blame 4, prior 2): the module-with-no-HiTOP-SR refusal sets its text directly, not through `refuse()`. For a module from a setup file, it still says "The link holds". Proposed: reject, because AC3 fixes that sentence.
  - F5 (diff 5): two module rows are refused as naming "HiTOP-SR twice", with no added sentence. Proposed: follow-up, because the plan gate set the added sentence for a mixed pair only.
  - F6 (diff 6): no test opens a link with `instrument: "hitopsr-module"`. That value is menu-only, and only the `INSTRUMENTS` check keeps it out. Proposed: fix now, with one refusal test.
  - F7 (diff 7): no test takes a module row through "Download the setup file" and back. Proposed: follow-up in the hosted setup file row.
  - F8 (diff 8): the Instruments hint states the PID-5 limit but not the one-HiTOP-SR limit. Proposed: follow-up in the edge-cases row.
  - F9 (diff 9): the new NEWS entry does not name the empty-box refusal. Proposed: fix now, with F2.
  - F10 (blame 2): a `module` that is not an object now gives an empty module row. The header comment at `link.html:906-908` still says a field of the wrong type is skipped. The online form also refuses `module: null`. Proposed: fix now, by correcting the comment.
  - F11 (blame 3): a hand-made `pid5` link with a matching module is now refused when opened. Proposed: reject, because AC3 asks for this refusal.
  - F12 (blame 6): after a row's menu switches away and back, its read error and status still show. The old box had the same gap. Proposed: follow-up in the edge-cases row.
  - F13 (prior 3): the ROADMAP edge-cases row still says "M154 reworks the prefill". Proposed: fix at the post-merge hygiene pass.
  - F14 (prior 4): three added lines in the READMEs and `online-collection.Rmd` run past 90 characters. Proposed: reject as a wrap nit.
