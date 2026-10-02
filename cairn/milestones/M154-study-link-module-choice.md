# M154: "HiTOP-SR module" as an instrument choice

- **Status:** in-progress
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

- [ ] AC1: Each instrument row's list offers "HiTOP-SR module" after "HiTOP-SR". A row set to it shows, inside the row, the "Module file" box and the "Choose the module file" control, which leave the optional section. That section is renamed "Item order" and holds only "Show the items in a random order". Only a row set to "HiTOP-SR module" writes its box into the link. A row switched away keeps its text out of view, and switching it back shows the text again. "Add an instrument" skips both HiTOP-SR entries when either is listed. Playwright tests cover each.
- [ ] AC2: A "HiTOP-SR module" row writes the link as HiTOP-SR plus a module file writes it today. A test builds three fixed `c` setups (the module alone; a list with the module second; the module with a random order) and compares each link's query with the query that hitop-form `main` at commit `2af14f0` builds for the same setup, hardcoded in the test. Two rows set to HiTOP-SR and HiTOP-SR module, or two module rows, are refused as a repeated instrument, naming the second row. A module row with an empty box is refused, focusing its box.
- [ ] AC3: Opening link.html with a `c`, `z` or `setup` link holding `hitopsr` and a `module` fills a "HiTOP-SR module" row with the module. Without `module`, it fills a "HiTOP-SR" row. A link with a `module` and no `hitopsr` is refused with "The link holds a module file but no HiTOP-SR." A test opens a `c` link of the shape the Module Builder's "Open the Study Link Builder" link writes (`{instrument: "hitopsr", module}`, in hitop-builder's `index.html`) and finds the module row filled.
- [ ] AC4: A search with whitespace normalized for "Item order and HiTOP-SR module", and for the anchor `item-order-and-hitop-sr-module`, in hitop-form, hitop-builder and hitop's `vignettes/`, finds nothing. hitop's NEWS.md keeps its released entries as history. hitop-form's README, hitop-builder's README and the hitop article describe the new choice. Every hint in link.html is at most 40 words, the in-row module hint included. DESIGN.md Known issue 11 says the builder now takes a module file only in a "HiTOP-SR module" row, while the online form still checks a descriptor's `instrument` only against the link's.
- [ ] AC5: hitop's NEWS.md names the new choice. hitop-form's and hitop-builder's Playwright suites pass. In hitop, `devtools::check()` reports 0 errors and 0 warnings, and `pkgdown::build_article("articles/online-collection")` renders without error.

## Coverage

- AC1 → T1, T4
- AC2 → T2, T4
- AC3 → T3, T4
- AC4 → T5
- AC5 → T4, T6

## Tasks

- [ ] T1: In `link.html`, add "HiTOP-SR module" to `OFFERED`, move the module box and file control into the row, rename the section, and make "Add an instrument" skip both HiTOP-SR entries.
- [ ] T2: Write `instrument`/`instruments` and `module` from a module row, with the repeated-instrument and empty-box refusals. Take the three expected queries from `main` at `2af14f0` before changing the build.
- [ ] T3: Fill a module row from `c`, `z` and `setup` links, and refuse a module beside no HiTOP-SR.
- [ ] T4: Rewrite the module tests (`link-module-file.spec.js`, `link-sections.spec.js`, `link.spec.js`, `link-instruments.spec.js`), add the in-row hint to S8's walk, and add the AC2 and AC3 tests.
- [ ] T5: Update hitop-form's README, hitop-builder's README (lines 244 to 245 and the anchor), the hitop article and Known issue 11. Run AC4's search.
- [ ] T6: hitop NEWS line, `pkgdown::build_article()`, `devtools::check()`, and the companion PRs.

## Work log

- 2026-10-01: created by /milestone-plan with M152 and M153. Jeff found choosing HiTOP-SR and then giving the module in a separate section awkward. Audits are recorded in M152's work log.
- 2026-10-01: plan chose "HiTOP-SR module" as its own list entry over a "Use a module" box inside the HiTOP-SR row, because Jeff asked for a separate choice. Falsified by researchers who look for the module under HiTOP-SR and miss the entry.
- 2026-10-01: implement started. Branches cut in hitop, hitop-form and hitop-builder. Gate: Jeff chose the entry text "HiTOP-SR module (scales you choose)", a README subsection "A HiTOP-SR module", and a repeat refusal that adds "A HiTOP-SR module is the HiTOP-SR, so a list holds one or the other." when one row of the pair is a module row.
- 2026-10-01: AC2 oracle taken from hitop-form `2af14f0` (git archive, the old builder driven by Playwright) for the three setups, study names "module alone", "module second" (rows HiTOP-BR, HiTOP-SR) and "module shuffled", each with `tests/fixtures/module-plain.json` pasted.
- 2026-10-01: T1 to T3 code checkpointed in hitop-form `28161c6`. The suite ran 930 of 966 passing, and the 36 failures are the old module-section tests that T4 rewrites, so T1 to T3 stay unticked until T4.
- 2026-10-01: T4 delegated to a Sonnet subagent (rewrite the 36 tests, add the AC1 to AC3 tests). T5 docs written meanwhile: hitop-form README (new "A HiTOP-SR module" subsection, "Item order"), hitop-builder README `c1cd0ea`, both hitop articles (`modules-hitopsr.Rmd` also named the old section), Known issue 11. T6 NEWS line written. Both articles render with `pkgdown::build_article()`.

## Decisions

## Review
