# M098: hitop-form renders the three PID-5 forms from the package's JSON exports

- **Status:** review
- **Priority:** normal
- **Depends on:** M097
- **Driving RR:** —
- **Principles touched:** IP1
- **Resolves:** —
- **Surface tier:** user-facing — a public page participants fill in
- **Branch/PR:** `m098-hitop-form-pid5` (hitop, tracking), `m098-pid5` (jmgirard/hitop-form, code)

## Goal

Extend `jmgirard/hitop-form` so a study link naming `pid5`, `pid5sf` or `pid5bf` renders that PID-5 form from the package's export and saves the same response file it saves for the HiTOP forms. Three captured files go to the package's reader.

## Scope

**In:** the three PID-5 entries in `INSTRUMENTS` and the link builder's selector. The render and save specs over the new stems, with a value-0 assertion. Three captured response fixtures with provenance. The page README. The page renders the export's text and options unchanged, under D-038's page-behavior reading of IP1.

**Out:** the JSON exports themselves → M097 (this milestone's specs fetch the deployed export, so M097's site deploy must precede its CI run). The package-side reader tests and vignette → M099. Answers kept across a reload, a Back-button test, and the unload beacon → the online-form candidate row (Jeff, 2026-09-20 plan gate). Modules for the PID-5 → none exist, because `hitop_module()` takes the HiTOP-SR only. The page's module check is untouched.

## Acceptance criteria

- [ ] AC1: A study link whose `instrument` is `pid5`, `pid5sf` or `pid5bf` renders that export. The screen title names the form (`PID-5`, `PID-5-SF` or `PID-5-BF`). The items number 220, 100 or 25. Each item offers the export's four options in export order with values 0 to 3.
- [ ] AC2: A file saved for each PID-5 version has the header `study,participant,instrument,form_build,submitted` followed by the export's item names in export order. It has one data row. Its `instrument` equals the stem. Every answer is written as the option's `value`, and a chosen 0 is written as `0`.
- [ ] AC3: The instrument selector in `link.html` offers the three PID-5 versions with their item counts. A link it builds for each opens the matching form. Modules stay restricted to the HiTOP-SR in the builder's wording.
- [ ] AC4: `tests/fixtures/responses-pid5.csv`, `responses-pid5sf.csv` and `responses-pid5bf.csv` each equal a fresh capture by the page in every column but `form_build` and `submitted`.
- [ ] AC5: The README names the five instruments the page renders, and names `score_pid5()` as the scoring function for a PID-5 file.
- [ ] AC6: Every Playwright spec passes against the deployed export once M097's site deploy carries the three PID-5 files.

## Coverage

- AC1 → T1, T2
- AC2 → T3
- AC3 → T1
- AC4 → T3
- AC5 → T4
- AC6 → T5

## Tasks

- [x] T1: Add the three PID-5 entries to `INSTRUMENTS` (`form.js:14`) and the three `<option>`s to the selector (`link.html:67-70`). If the module hint (`link.html:81`) reads wrong with five instruments, reword it without widening what a module applies to. Add `tests/link.spec.js` for the selector, a built link per instrument, and the module hint.
- [x] T2: Loop `render.spec.js:20` over the five stems `hitopsr`, `hitopbr`, `pid5`, `pid5sf` and `pid5bf`, asserting title, item count and option values.
- [x] T3: Add three PID-5 cases to `save.spec.js:53-65`. Assert the header, one row, the stem, and that the answer pattern's value-0 pick (`helpers.mjs:112`, position 4) is written as `0`. Capture the three fixtures with `WRITE_FIXTURES=1 npx playwright test tests/save.spec.js` and record their provenance in `tests/fixtures/README.md`. Per LESSONS M095, create the download promise before the walk with its own timeout.
- [x] T4: README: the instruments list, a PID-5 file example beside the HiTOP ones, and the `score_pid5()` sentence beside `score_hitopsr()`.
- [x] T5: Confirm the deployed site serves the three PID-5 files. Open the PR and get the `tests.yml` run green. Record the run in the work log. The merge and one dispatched run against the deployed page happen at review's merge step.

## Work log

- 2026-09-20: created by /milestone-plan; part one of the online-form candidate row (lineage M095).
- 2026-09-20: criteria audit ran in full mode by a fresh [O] reader; findings on this file repaired before the gate: spec-coverage clauses moved from AC1 and AC2 to T2 and T3, AC3 restated as meaning rather than wording, AC4 restated as the checkable capture equality, AC6 restated as the deliverable with the CI bookkeeping in T5.
- 2026-09-20: plan gate chose leaving the M095 page follow-ups (reload persistence, Back test, unload beacon) on the candidate row over taking them here because this milestone is a rendering change for three new instruments; falsified by a researcher reporting one of them.
- 2026-09-20: implement gate: the hitop-form PR opens and goes green at implement, and it merges at review after Jeff approves, because the merge publishes the live participant page. T5 reworded to match (minor amendment).
- 2026-09-20: T1 done in hitop-form e88f462. The module hint already reads "Optional, HiTOP-SR only", so it is unchanged. No test covered `link.html`, so T1 gained `tests/link.spec.js` (7 tests, minor amendment). A plant that dropped `pid5sf` from `INSTRUMENTS` turned its test red.
- 2026-09-20: T2 done in hitop-form 2313fe7. The render spec loops over five forms and checks the heading, the item count and the option values against stated values and the export. 7 of 7 pass. A plant titling the BF form "PID-5 BF" turned it red.
- 2026-09-20: T3 done in hitop-form 0cec049. Three PID-5 save cases, 7 of 7 pass. Fixtures captured with `-g pid5` so the three HiTOP fixtures kept their bytes. Plants: a 0 written as blank reds the integer check, a 0 written as `00` reds the new value-0 check. The fixture note's "reader still to be written" line now names `read_form_responses()`.
- 2026-09-20: T4 done in hitop-form 1593f53. The README names the five forms, the PID-5 item names and value ranges, and `score_pid5()` with a `version` example. Its "reader under development" paragraph now names `read_form_responses()`, which read the pid5bf and pid5 fixtures and fed `score_pid5()` in this session.
- 2026-09-20: T5 done. The deployed site serves `pid5.json` (220 items), `pid5sf.json` (100) and `pid5bf.json` (25). Local suite 35 passed. PR https://github.com/jmgirard/hitop-form/pull/1 opened. The `tests.yml` run 35555027670 green at 1593f53, 35 passed. Merge and the deployed-page dispatch wait for review.
- 2026-09-20: claim audit: 24 claims read, 1 corrected — hitop-form README.md, form.js, link.html, tests/link.spec.js, tests/render.spec.js, tests/save.spec.js, tests/fixtures/README.md. Run on the hitop-form diff, because the hitop diff adds no lines outside `cairn/`. The fixture note claimed PID-5 reader tests that do not exist yet. Fixed in hitop-form f8edd9d, and the same reader re-read it as correct.
- 2026-09-20: verify: no R code changed, so `devtools::test()` was not re-run. `cairn_validate.py` passes. Status set to review.

## Decisions

## Review
