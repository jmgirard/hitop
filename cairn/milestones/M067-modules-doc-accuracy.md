# M067: The modules article and the generator help pages describe what the generators do

- **Status:** review
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** GP3
- **Branch/PR:** `m067-modules-doc-accuracy` / https://github.com/jmgirard/hitop/pull/73

## Goal

The modules article and the three HiTOP-SR generator help pages state the module behavior the code has shipped since M048, M049 and M065, with the corrected sentences locked by a test.

## Scope

Surface tier: **user-facing** — the deliverables are `vignettes/articles/modules-hitopsr.Rmd` and three roxygen help pages, both published on the pkgdown site and read by researchers building forms.

**In:** four documentation gaps the ROADMAP row carries, plus whatever a sweep of the generators' `module` conditionals shows the article is silent on. (a) The article's module-vs-full enumeration omits the module-aware Word header (`R/generate_docx.R:222`). (b) Its `randomize` paragraph promises a crosswalk the code withholds under `renumber = FALSE` (`R/generate_docx.R:323`). (c) The reorder recipe `collected[order(item_order)]` is given unqualified at `modules-hitopsr.Rmd:118`, `:306` and `R/generate_docx.R:125`, though it holds only when the collected columns are in printed order. (d) The three `@param descriptor` blocks narrate the write and its rollback but not the success message (`generate_docx.R:372`, `generate_qualtrics.R:145`, `generate_redcap.R:145`). Also: a prose test locking the added claims, a NEWS bullet, and regenerated `man/`.

**Out:** any change to generator behavior — this milestone edits prose only; a behavior defect the sweep finds becomes its own ROADMAP row. The PID-5 vignette coverage gaps (`calc_se` on the 220/25-item forms, `rank_scales()`) stay on their own candidate row. The builder page's deferred wording items stay on theirs.

## Acceptance criteria

- [x] AC1: The article's "Generating the Instrument" section states that a Word form built from a module is headed "HiTOP-SR Module (v1.0)" where a full-instrument form is headed "HiTOP-SR (v1.0)", and names `title =` as the override.
- [x] AC2: The article's `randomize` paragraph states that no printed-number crosswalk appears under `renumber = FALSE`, agreeing with `?generate_docx_hitopsr`.
- [x] AC3: Every hit of `grep -rn 'item_order' vignettes/ R/ NEWS.md` is either a reorder recipe carrying the precondition that the collected columns are in the order the form printed them, or is recorded in the work log as not a recipe. The grep is shown to return a non-empty hit list.
- [x] AC4: The `@param descriptor` of `generate_docx_hitopsr()`, `generate_qualtrics_hitopsr()` and `generate_redcap_hitopsr()` each states that a successful write announces the descriptor's path on the console.
- [x] AC5: Every conditional on `module` that `grep -n 'module' R/generate_docx.R R/generate_qualtrics.R R/generate_redcap.R R/module.R` returns is either described in the article or recorded in the work log as deliberately outside it, with a reason. The grep's hit list is shown non-empty and its conditional subset enumerated.
- [x] AC6: `NEWS.md` carries one bullet under 0.2.0 naming the article and the three help pages corrected, claiming no more than the new prose test enforces.
- [x] AC7: `man/` is regenerated from the edited roxygen, `devtools::test()` passes, and `devtools::check()` reports no ERROR/WARNING/NOTE absent from the same check run on the branch point.

## Coverage

- AC1 → T1, T6
- AC2 → T2, T6
- AC3 → T3, T6
- AC4 → T4, T6
- AC5 → T5
- AC6 → T7
- AC7 → T4, T7

## Tasks

- [x] T1: Add the header difference to `modules-hitopsr.Rmd`'s "Generating the Instrument" section, beside the numbering paragraph (`:94-102`); the header itself is settled content, so quote it from `R/generate_docx.R:222` rather than from memory.
- [x] T2: Correct the article's `randomize` paragraph (`:105-112`) for the `renumber = FALSE` case, matching the wording already at `R/generate_docx.R:117-119`.
- [x] T3: Run the AC3 grep, record its hit list, and add the printed-order precondition at each recipe site (`modules-hitopsr.Rmd:118`, `:306`, `R/generate_docx.R:125`) plus any further hit the grep turns up.
- [x] T4: Add the console-announcement sentence to the three `@param descriptor` blocks (`generate_docx.R:128-142`, `generate_qualtrics.R:67-78`, `generate_redcap.R:68-79`); avoid `[...]` in the tag, which roxygen parses as a link. Run `devtools::document()`.
- [x] T5: Run the AC5 grep, enumerate its `module` conditionals, and for each either add article prose or log why it stays out. A behavior defect found here is captured as a candidate row, not fixed.
- [x] T6: Write `tests/testthat/test-module-doc-prose.R` locking AC1-AC4's claims across the article and the three Rd files. Show every assertion red against the pre-edit text; assert both boundaries of any section cut were found, and `next` after any expectation whose value is reused in a loop.
- [x] T7: Add the NEWS bullet, render the modules article against a fresh `devtools::install()`, and run the profile's verify and consistency-gate commands.

## Work log

- 2026-08-29: created by /milestone-plan; promotes the ROADMAP documentation row (lineage M048, M049, M055, M065), which is absorbed whole and removed.
- 2026-08-29: plan gate chose sweeping the generators' `module` conditionals over fixing only the four known gaps because the row had already been silently extended three times; falsified by the sweep returning nothing the article omits.
- 2026-08-29: plan gate chose a locking prose test over prose edits alone because the same sentences drifted out of step three times; falsified by the test proving unable to go red on a removed sentence.
- 2026-08-30: T1-T5 done. Every claim added was derived from an execution, not composed: the two Word headers read out of two built DOCX files ("HiTOP-SR (v1.0)" / "HiTOP-SR Module (v1.0)"); the crosswalk arrow present under `renumber = TRUE` and absent under both `renumber = FALSE` and `module = NULL`; the three console messages captured in order, each descriptor line following its form's line.
- 2026-08-30: AC3 grep returned 47 hits (non-empty). Three are reorder-recipe passages and all three now carry the printed-order precondition: `modules-hitopsr.Rmd:118` and the `:300`/`:306` passage, plus `R/generate_docx.R:125`. The other 44 are not recipes -- 6 in `NEWS.md` and `module_file.R` roxygen describing the attribute, and 38 in R sources naming the variable, the `write_module()` argument, the `hitop_module_file_bad_item_order` class, or `warn_item_order()`, which is unrelated PID-5 code.
- 2026-08-30: AC5 grep returned 85 non-`#'` hits across the four files; the conditionals on `module` among them are five. `generate_docx.R:222` (module header) and `:227` (`include_subscales` refused with `module`) were both absent from the article and are now added -- `:227` is the gap the sweep found that the plan did not name. `:323` (crosswalk) is AC2. `module.R:154` (`apply_module` returns the full tables under `module = NULL`) and `:267` (`hitopsr_engine_inputs` scores all 405 under `module = NULL`) stay out of the article deliberately: both are the "pass a module, get only its items" and "items addressed by position in `module$items`" behavior the article already describes in its own terms, and naming the internal helpers would not help a reader.
- 2026-08-30: T6 done. `test-module-doc-prose.R` added (6 blocks). Discrimination shown per claim, one plant at a time: gutting the header sentence, restoring the old unconditional crosswalk sentence, removing the article precondition, removing the help-page precondition, and softening the `include_subscales` refusal each turned exactly its own block red and left the other five green; rewording an unrelated sentence left all six green. The descriptor block was proved separately against each of the three Rd files, red on each in turn, so the loop is not truncating. A first plant of the console sentence scored green because the Rd line-wraps the phrase and the plant's literal replace missed it -- the plant was wrong, not the test; re-run against the wrapped text it goes red.
- 2026-08-30: T7 done. NEWS bullet added under 0.2.0, claiming only what `test-module-doc-prose.R` asserts. Article rendered after `devtools::install()` (pkgdown reads the installed package) -- `pkgdown::build_article("articles/modules-hitopsr")` clean, and the six added phrases were read back out of `docs/articles/modules-hitopsr.html`. `devtools::test()`: 0 failures, 0 warnings, 4 skips, 15642 passes. `devtools::check()`: 0 errors, 0 warnings, 0 notes, so nothing is present that was absent at the branch point. `devtools::document()` leaves no diff.
- 2026-08-29: criteria audit ran in **full mode** (user-facing tier), inline rather than in a fresh-context reader — this session is configured not to spawn subagents, so the audit's freshness property was unavailable. Three findings, all fixed before the gate: a criterion binding "a prose test file fails when a sentence is removed" bound an instrument property and moved to T6; a criterion promising the article's module-vs-full enumeration was complete quantified over behavior differences that no grep enumerates, and was narrowed to the conditionals the AC5 grep returns; a criterion promising "every site giving the recipe" rested on a hand-list and was narrowed to the AC3 grep's hits.

## Decisions

## Review

PR: https://github.com/jmgirard/hitop/pull/73 (draft, opened 2026-08-30). Branch was level with `origin/main` at review start (0 commits behind), so no merge was needed.

### Acceptance criteria

- AC1 — `modules-hitopsr.Rmd:105-109` reads "heads a module form `HiTOP-SR Module (v1.0)` where it heads a full-instrument form `HiTOP-SR (v1.0)`" and "Pass `title =` to print something else". Both strings match `R/generate_docx.R:227` verbatim; the `title` sentinel at `:226` confirms an explicit `title` is used unchanged.
- AC2 — `modules-hitopsr.Rmd:113-124` reads "Under `renumber = FALSE` there is none, because the printed numbers already are the original ones", and names the full-instrument case separately. Agrees with `R/generate_docx.R:328` (`randomize && renumber && !is.null(module)`) and with `@param randomize` on the same help page.
- AC3 — `grep -rn 'item_order' vignettes/ R/ NEWS.md` returned 48 hits (non-empty). Three are recipes and all three carry the printed-order precondition: `modules-hitopsr.Rmd:130` (precondition at `:133-137`), `R/generate_docx.R:125` (at `:127-131`), `NEWS.md:196`. The other 45 are not recipes (`module_file.R` 20, `generate_docx.R` 15, `NEWS.md` 3, `util.R` 2, article 2, `generate_qualtrics.R`/`generate_redcap.R`/`validity_pid5.R` 1 each) — the work-log enumeration re-checked, the count 48 against the implement-time 47 because the NEWS bullet added its own mention. A wider sweep for the recipe's shape, `grep -rn '\[order(' vignettes/ R/ NEWS.md`, returned 17 hits and added no fourth recipe: the other 14 are internal sorts and the article's own `printed_order` line at `:326`, which carries the precondition inline.
- AC4 — "Once both files are on disk the descriptor's path is announced on the console" appears in the `descriptor` item of all three Rd files (`generate_docx_hitopsr.Rd:96`, `generate_qualtrics_hitopsr.Rd:53`, `generate_redcap_hitopsr.Rd:49`), each naming its own preceding message. Verified against the code: the `cli_alert_success` naming the descriptor fires after `built <- TRUE` in each generator (`generate_docx.R:377`, `generate_qualtrics.R:146`, `generate_redcap.R:146`), so it does follow the file message.
- AC5 — `grep -n 'module' R/generate_docx.R R/generate_qualtrics.R R/generate_redcap.R R/module.R` returned 140 hits (non-empty). Its conditional subset is five, unchanged from the work-log enumeration at the shifted line numbers: `generate_docx.R:227` (header, in the article), `:232` (`include_subscales` refused, in the article), `:328` (crosswalk, AC2); `module.R:154` and `:267` logged as deliberately out with reasons.
- AC6 — one bullet at `NEWS.md:188-200`, inside `# hitop 0.2.0` (lines 1-564). Each claim it makes is an assertion in `test-module-doc-prose.R`: the two headers and `title =`, the `renumber = FALSE` case, the `include_subscales` refusal, the recipe precondition in both the article and `?generate_docx_hitopsr`, and the console announcement on all three help pages.
- AC7 — `devtools::document()` leaves the tree clean (only this milestone file modified). `devtools::test()`: 0 failures, 0 warnings, 4 skips, 15642 passes. `devtools::check()`: `Status: OK` — 0 errors, 0 warnings, 0 notes, so nothing is present that was absent at the branch point.

### Consistency gate

- `cairn_validate.py` exit 0 — all 16 checks PASS, `coverage complete` and `scaffold present` among them; 21 advisories, all pre-existing (20 dangling `D-001`..`D-012` tokens from the pre-migration numbering, 1 references-staleness on `schmukle2026.md`). `release window` did not fire.
- No `DESIGN.md` principle changed on this branch, so `cairn_impact.py` was not run.
- Toolchain slot: `document()` no diff; no hand-edited generated file (`man/` regenerates clean); `README.Rmd`/`README.md` untouched; `pkgdown::check_pkgdown()` "No problems found"; `NEWS.md` bullet present with no milestone number in it; no new top-level file, so no `.Rbuildignore` entry owed; `devtools::check()` clean. `Rscript data-raw/check_line_endings.R` passed.

### Independent review

Surface tier is user-facing, so the full three-lens fan-out applies. All three lenses were run inline rather than in fresh-context subagents: this session is configured not to spawn agents, so the freshness property was unavailable (the same limitation logged at M051, M052 and at this milestone's own criteria audit).

- **[S] blame-history** — no conflict. The rewritten crosswalk paragraph descends from M046 (PR #52) and M055 (PR #61); M046's recorded gate decision was that the crosswalk is module-only with the hazard documented, which the rewrite preserves and extends to the `renumber = FALSE` case rather than undoing. The `include_subscales` guard the article now describes is M024's review fix — a bypassable `isTRUE()` replaced by plain truthiness with a comment saying why — and the article does not disturb it.
- **[S] prior-PR-comments** — the archived `## Review` sections touching these files are M044 (the article's origin, ten findings), M046, M055 and M065. Nothing the diff does reintroduces a point those reviews settled. M065's finding 7 was deferred to the documentation candidate row; that row was absorbed whole into this milestone's scope and its four items are (a)-(d) verbatim, so nothing deferred there was dropped. GitHub probe: `gh api repos/jmgirard/hitop/pulls/comments` returned 0 inline review comments repo-wide, so no per-PR walk was owed.
- **[O] diff-bug** — three findings, ranked, none demonstrating an acceptance criterion failing and none load-bearing in what the package does for users, so the return floor is not reached.

### Findings

1. `tests/testthat/test-module-doc-prose.R:118` anchors the recipe block on `expect_match(section, "order(", fixed = TRUE)`, which any `order(` inside a ~80-line section satisfies — including the unrelated `scale_menu[order(...)]` sort. The block's discrimination therefore rests entirely on the two precondition phrases beside it, in a test file whose purpose is to lock prose that has drifted three times.
2. The article states the refused combination as `include_subscales = TRUE` together with `module`, where `R/generate_docx.R:232` refuses any truthy `include_subscales` — deliberately not `isTRUE()`, per M024's review fix. A reader passing `include_subscales = 1` is also refused. The help page at `R/generate_docx.R:94-102` words it the same way, so the article is consistent with the surface it describes.
3. `vignettes/articles/modules-hitopsr.Rmd:322-326` leaves "with" alone on its own source line after the inserted appositive. Renders identically; source formatting only.

### Triage

- Finding 1 — **fixed at the gate** (Jeff's decision). `test-module-doc-prose.R:119` now anchors on `collected[order(`, the recipe's own shape, with a comment naming the unrelated `scale_menu[order(...)]` sort a bare `order(` would accept. Shown able to fail: renaming the article's `collected[...]` to `responses[...]` — a change the old anchor passed — turns that block and only that block red, then green again on restore. AC3 and AC7 re-verified after the change.
- Finding 2 — **logged, no change.** The article's `include_subscales = TRUE` wording matches the help page it describes; the guard's plain-truthiness form is M024's deliberate fix.
- Finding 3 — **logged, no change.** Source formatting only; the rendered article is identical.

### Work log addendum

- 2026-08-30: review pass 1. All seven criteria verified fresh, gate clean, three lenses run inline (session cannot spawn agents). Three findings, none floor-qualifying; finding 1 fixed at the gate. Defect returns: 0.
