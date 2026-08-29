# M067: The modules article and the generator help pages describe what the generators do

- **Status:** planned
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** GP3
- **Branch/PR:** —

## Goal

The modules article and the three HiTOP-SR generator help pages state the module behavior the code has shipped since M048, M049 and M065, with the corrected sentences locked by a test.

## Scope

Surface tier: **user-facing** — the deliverables are `vignettes/articles/modules-hitopsr.Rmd` and three roxygen help pages, both published on the pkgdown site and read by researchers building forms.

**In:** four documentation gaps the ROADMAP row carries, plus whatever a sweep of the generators' `module` conditionals shows the article is silent on. (a) The article's module-vs-full enumeration omits the module-aware Word header (`R/generate_docx.R:222`). (b) Its `randomize` paragraph promises a crosswalk the code withholds under `renumber = FALSE` (`R/generate_docx.R:323`). (c) The reorder recipe `collected[order(item_order)]` is given unqualified at `modules-hitopsr.Rmd:118`, `:306` and `R/generate_docx.R:125`, though it holds only when the collected columns are in printed order. (d) The three `@param descriptor` blocks narrate the write and its rollback but not the success message (`generate_docx.R:372`, `generate_qualtrics.R:145`, `generate_redcap.R:145`). Also: a prose test locking the added claims, a NEWS bullet, and regenerated `man/`.

**Out:** any change to generator behavior — this milestone edits prose only; a behavior defect the sweep finds becomes its own ROADMAP row. The PID-5 vignette coverage gaps (`calc_se` on the 220/25-item forms, `rank_scales()`) stay on their own candidate row. The builder page's deferred wording items stay on theirs.

## Acceptance criteria

- [ ] AC1: The article's "Generating the Instrument" section states that a Word form built from a module is headed "HiTOP-SR Module (v1.0)" where a full-instrument form is headed "HiTOP-SR (v1.0)", and names `title =` as the override.
- [ ] AC2: The article's `randomize` paragraph states that no printed-number crosswalk appears under `renumber = FALSE`, agreeing with `?generate_docx_hitopsr`.
- [ ] AC3: Every hit of `grep -rn 'item_order' vignettes/ R/ NEWS.md` is either a reorder recipe carrying the precondition that the collected columns are in the order the form printed them, or is recorded in the work log as not a recipe. The grep is shown to return a non-empty hit list.
- [ ] AC4: The `@param descriptor` of `generate_docx_hitopsr()`, `generate_qualtrics_hitopsr()` and `generate_redcap_hitopsr()` each states that a successful write announces the descriptor's path on the console.
- [ ] AC5: Every conditional on `module` that `grep -n 'module' R/generate_docx.R R/generate_qualtrics.R R/generate_redcap.R R/module.R` returns is either described in the article or recorded in the work log as deliberately outside it, with a reason. The grep's hit list is shown non-empty and its conditional subset enumerated.
- [ ] AC6: `NEWS.md` carries one bullet under 0.2.0 naming the article and the three help pages corrected, claiming no more than the new prose test enforces.
- [ ] AC7: `man/` is regenerated from the edited roxygen, `devtools::test()` passes, and `devtools::check()` reports no ERROR/WARNING/NOTE absent from the same check run on the branch point.

## Coverage

- AC1 → T1, T6
- AC2 → T2, T6
- AC3 → T3, T6
- AC4 → T4, T6
- AC5 → T5
- AC6 → T7
- AC7 → T4, T7

## Tasks

- [ ] T1: Add the header difference to `modules-hitopsr.Rmd`'s "Generating the Instrument" section, beside the numbering paragraph (`:94-102`); the header itself is settled content, so quote it from `R/generate_docx.R:222` rather than from memory.
- [ ] T2: Correct the article's `randomize` paragraph (`:105-112`) for the `renumber = FALSE` case, matching the wording already at `R/generate_docx.R:117-119`.
- [ ] T3: Run the AC3 grep, record its hit list, and add the printed-order precondition at each recipe site (`modules-hitopsr.Rmd:118`, `:306`, `R/generate_docx.R:125`) plus any further hit the grep turns up.
- [ ] T4: Add the console-announcement sentence to the three `@param descriptor` blocks (`generate_docx.R:128-142`, `generate_qualtrics.R:67-78`, `generate_redcap.R:68-79`); avoid `[...]` in the tag, which roxygen parses as a link. Run `devtools::document()`.
- [ ] T5: Run the AC5 grep, enumerate its `module` conditionals, and for each either add article prose or log why it stays out. A behavior defect found here is captured as a candidate row, not fixed.
- [ ] T6: Write `tests/testthat/test-module-doc-prose.R` locking AC1-AC4's claims across the article and the three Rd files. Show every assertion red against the pre-edit text; assert both boundaries of any section cut were found, and `next` after any expectation whose value is reused in a loop.
- [ ] T7: Add the NEWS bullet, render the modules article against a fresh `devtools::install()`, and run the profile's verify and consistency-gate commands.

## Work log

- 2026-08-29: created by /milestone-plan; promotes the ROADMAP documentation row (lineage M048, M049, M055, M065), which is absorbed whole and removed.
- 2026-08-29: plan gate chose sweeping the generators' `module` conditionals over fixing only the four known gaps because the row had already been silently extended three times; falsified by the sweep returning nothing the article omits.
- 2026-08-29: plan gate chose a locking prose test over prose edits alone because the same sentences drifted out of step three times; falsified by the test proving unable to go red on a removed sentence.
- 2026-08-29: criteria audit ran in **full mode** (user-facing tier), inline rather than in a fresh-context reader — this session is configured not to spawn subagents, so the audit's freshness property was unavailable. Three findings, all fixed before the gate: a criterion binding "a prose test file fails when a sentence is removed" bound an instrument property and moved to T6; a criterion promising the article's module-vs-full enumeration was complete quantified over behavior differences that no grep enumerates, and was narrowed to the conditionals the AC5 grep returns; a criterion promising "every site giving the recipe" rested on a hand-list and was narrowed to the AC3 grep's hits.

## Decisions

## Review
