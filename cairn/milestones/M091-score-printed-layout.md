# M091: The HiTOP-SR scoring functions score printed-order columns through a module's recorded item order

- **Status:** review
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** GP2, GP3, IP2
- **Resolves:** —
- **Surface tier:** user-facing — two exported scoring functions gain an argument
- **Branch/PR:** `m091-score-printed-layout`

## Goal

`score_hitopsr()` and `reliability_hitopsr()` take a `layout` argument, and `layout = "printed"` scores columns collected in a shuffled form's printed order through the module's `item_order` attribute.

## Scope

**In:** A `layout = c("instrument", "printed")` argument on `score_hitopsr()` and `reliability_hitopsr()`. The default keeps today's behavior. Under `"printed"`, the supplied `items` are permuted through `attr(module, "item_order")` before the engine runs. Column k then holds the answer to the form's printed item k. Three refusals under `"printed"`: no module, a module with no `item_order`, and an `item_order` that is not a permutation of `module$items`. The ascending-name heuristic keeps reading the caller's own `items`. The argument replaces the reorder recipe in the roxygen and the modules article. A NEWS entry.

**Out:** The hitop-builder README and bundle README still tell users to reorder by hand → candidate row (other repo). Automatic detection of a printed layout from the attribute → rejected at the plan gate (work log). A `layout` argument on the PID-5 or HiTOP-BR functions → no module support exists there (candidate row "Generalize modularization to BR/PID-5"). Classed conditions for the refusals → rejected at the plan gate (work log).

## Acceptance criteria

- [ ] AC1: `score_hitopsr()` and `reliability_hitopsr()` each take a `layout` argument that accepts `"instrument"` (the default) or `"printed"`. With `layout` omitted, a characterization script runs every `score_hitopsr()` and `reliability_hitopsr()` call in `tests/testthat/` at the merge base and on the branch. Grep enumerates the calls. Every returned value is `identical()` across the two runs (GP2).
- [ ] AC2: Take a module whose `item_order` is a permutation that is not its own inverse. The test checks that as `!identical(match(item_order, module$items), match(module$items, item_order))`. Add one 3-cycle and one random shuffle. With `layout = "printed"`, `score_hitopsr()` given columns in printed order returns correct scale scores. One two-scale fixture includes Romantic Disinterest, the instrument's only reverse-keyed item (HSR 310). Its expected scores are means over hand-reverse-keyed responses written out in the test (IP2). A second two-scale fixture with no reverse item covers the unkeyed case. For a four-scale module and for the whole-instrument module drawn from `sim_hitopsr`, the scores equal `score_hitopsr(layout = "instrument")` on the same responses in instrument order. That comparison is a consistency check on top of the hand fixtures in `test-score_hitopsr.R`, which already pin the instrument branch.
- [ ] AC3: With `layout = "printed"` and the four-scale module, `reliability_hitopsr(omega = FALSE)` on printed-order columns returns per-scale alphas equal to the `"instrument"` call on the same responses in instrument order. With `omega = TRUE`, a `calc_omega` mocked through `local_mocked_bindings()` records the item matrix it receives. Per-item-distinguishable responses make the column order identifiable. The recorded matrix equals the one the `"instrument"` call passes. This checks an internal contract, not returned output.
- [ ] AC4: `layout = "printed"` aborts, blaming the exported wrapper, in three cases: `module = NULL`, a module with no `item_order` attribute, and an `item_order` that is not a permutation of `module$items`. Each message names the `layout` argument. It says how to get an order (a descriptor written with `randomize = TRUE`) or to use `"instrument"`. A `layout` value outside the two choices aborts naming the exported function and both permitted values. Each abort branch has a test that fires it and asserts on the argument name and the blamed call. The branches stay unclassed, so a caller cannot catch them by name.
- [ ] AC5: Under `layout = "printed"`, the ascending-name heuristic `warn_item_order()` evaluates the `items` the caller supplied and never the permuted vector. Printed-order columns named `q_1` to `q_n` warn zero times. Printed-order columns with non-ascending names warn exactly once. Each case is asserted with a warning count.
- [ ] AC6: No passage in `R/`, `vignettes/`, or `README.Rmd` presents a hand reorder as the route to scoring printed-order columns. The command `grep -rn 'order(attr\|order(printed_order\|order(item_order' R/ vignettes/ README.Rmd` returns zero hits. That grep is a floor, and the passages were read. Five sources each name `layout = "printed"`: the roxygen of `score_hitopsr()`, `reliability_hitopsr()`, `generate_docx_hitopsr()`, and `read_module()`, and `vignettes/articles/modules-hitopsr.Rmd`.
- [ ] AC7: `devtools::document()` produces no diff, `devtools::test()` is clean, and `devtools::check()` reports 0 errors and 0 warnings.

## Coverage

- AC1 → T1, T2, T3
- AC2 → T1, T2
- AC3 → T1, T3
- AC4 → T1, T2, T3
- AC5 → T1, T2
- AC6 → T4
- AC7 → T4, T5

## Tasks

- [x] T1: Tests first. In `tests/testthat/test-score_hitopsr.R` and a new `test-layout.R`, add the AC2 fixtures (Romantic Disinterest pair, unkeyed pair, four-scale, whole-instrument), the AC3 alpha and mocked-omega checks, the AC4 abort tests, and the AC5 warning counts. Run them red.
- [x] T2: Implement in `score_hitopsr()` (`R/score_hitopsr.R:72`). Add `layout = c("instrument", "printed")` after `module`, resolved with `match.arg()`. Add an internal helper in `R/module.R` beside `hitopsr_engine_inputs()`. The helper validates the three refusals and returns `items[match(module$items, item_order)]`. Run `warn_item_order()` on the caller's `items` before the permute. Skip it inside `prep_items()` (`R/util.R:485`) for the permuted vector, for example through a flag argument.
- [x] T3: The same argument and helper on `reliability_hitopsr()` (`R/reliability_hitopsr.R:54`). Then the AC1 characterization script in `data-raw/` or the scratchpad: grep the calls, run them at the merge base and on the branch, compare with `identical()`.
- [x] T4: Documentation. Replace the recipe passages at `R/generate_docx.R:120-131`, `vignettes/articles/modules-hitopsr.Rmd:126-134` and `:320-328`, and the `item_order` paragraphs of `read_module()` and `write_module()` (`R/module_file.R:47-62`, `:208-210`). Document `layout` in both roxygen blocks. Run `devtools::document()`. Add the NEWS entry.
- [x] T5: `devtools::check()` clean, then the candidate row for the builder-side text.

## Work log

- 2026-09-11: created by /milestone-plan. Absorbs the 2026-09-11 candidate row "scoring functions consulting `item_order`" (M090 review gate).
- 2026-09-11: criteria audit ran in full mode (fresh Opus reader) and returned ten findings. Fixed: characterization enumerates calls by grep (the draft named a test file that does not exist). Fixed: permutation probes exclude involutions and add a 3-cycle and a random shuffle. Fixed: the hand fixture includes HSR 310. Fixed: the omega check is labelled an internal contract. Fixed: AC6 names the read as the enumeration and the grep as a floor. Fixed: the bad-`layout` abort names both values. Fixed: AC5 counts warnings. Fixed: AC2 states why the instrument-order comparison is not self-reference. Posed at the gate: classed or unclassed refusals.
- 2026-09-11: plan gate chose an explicit `layout` argument over automatic detection from `item_order` because a caller who already reordered by hand gets scrambled scores with no signal (GP2); falsified by evidence that no user reorders by hand once a descriptor exists.
- 2026-09-11: plan gate chose the argument over a separate exported reorder helper because the helper adds an export and a two-step workflow for one call site; falsified by a second consumer of the reordered frame.
- 2026-09-11: plan gate chose unclassed `cli_assert()` refusals over a classed condition with a D-entry because the refusals are argument misuse like the family's other validators; falsified by a caller who reports a need to catch one by name.
- 2026-09-11: the argument is named `layout` because `order` makes `o` ambiguous with `omega` in `reliability_hitopsr()` and `printed` clashes with `prefix` (M043 lesson); no existing argument in either function starts with `l`.
- 2026-09-11: /milestone-implement started on branch `m091-score-printed-layout`. Question gate skipped: the plan left nothing open for the user. Routine choices: the characterization script lives in the scratchpad, not `data-raw/` (it is a one-off, not a data generator). The prose tests in `test-module-doc-prose.R` that pin the reorder recipe move to `layout = "printed"` in T4 (discovered sub-task).
- 2026-09-11: substantive amendment at a mini gate, user accepted. AC2's check clause read `!identical(order(item_order), match(module$items, item_order))`, which is FALSE for every distinct `item_order` because `order(x)` equals `match(sort(x), x)`. It now reads `!identical(match(item_order, module$items), match(module$items, item_order))`, TRUE for a 3-cycle and FALSE for a swap (verified in R).
- 2026-09-11: re-audit: AC2 (full) — returned four findings, none against the corrected clause: the random shuffle names no seed, the prose is test-shaped, `reliability_hitopsr()` is absent from AC2, two fixtures use the instrument branch as oracle. Disposed without further wording change: the check clause binds every probe module and the test seeds the shuffle, AC3 covers reliability, AC2 already states the oracle justification.
- 2026-09-11: T1 done. `tests/testthat/test-layout.R` holds the AC2 hand fixtures (keyed pair with HSR 310, unkeyed pair, four-scale, whole instrument), the AC3 alpha and mocked-omega checks, the AC4 abort tests, and the AC5 warning counts. Run red: every call fails with `unused argument (layout = ...)`.
- 2026-09-11: T2 done. `layout_items()` in `R/module.R` validates the three refusals and permutes the caller's `items`. `prep_items()` gains `check_order`, passed through both engines. `score_hitopsr()` resolves `layout` with `rlang::arg_match()` instead of the plan's `match.arg()`, because `match.arg()` blames itself and names no argument (AC4 wants the exported function and the argument name). Consistency tests pass positions as `items`, because `hsr_` names in printed order are non-ascending and warn by design (AC5). Full suite green.
- 2026-09-11: checkpoint, half-done. T3 code is on disk: `layout` on `reliability_hitopsr()`, its help and example. T4 docs are on disk: the recipe passages in `generate_docx_hitopsr()`, the modules article, `read_module()` and `write_module()` now name `layout = "printed"`; the AC6 grep returns zero hits; the prose tests in `test-module-doc-prose.R` assert the argument instead of the recipe; NEWS entry and a DESIGN.md scoring-paragraph clause added. The builder-side candidate row T5 asks for already exists (added at the M091 plan gate). Pending before T3 and T4 tick: the AC1 characterization (two background runs in the scratchpad, `char/characterize.R` tracing 93 grep-enumerated call sites through the merge-base test files) and a full-suite re-run.
- 2026-09-11: T3 done. AC1 characterization: grep enumerates 93 call sites in 20 merge-base test files. `data-raw/characterize_layout/characterize.R` wraps both functions in the loaded namespace and runs the merge-base test files through `testthat::test_dir(load_package = "none")` at the merge base (c54ea081) and at the branch head. Each run recorded 251 calls (132 `score_hitopsr()`, 119 `reliability_hitopsr()`, 15 of them errors), and `compare.R` finds all 251 `identical()`. The scripts are committed under `data-raw/` rather than left in the scratchpad, so review can rerun them (revises the routine choice logged at session start). A first attempt through `devtools::test()` recorded zero calls because its `load_all()` replaced the wrapped bindings.
- 2026-09-11: T4 done. Full suite green after the doc edits (17383 pass, 0 fail, 13 skip). `devtools::document()` regenerated four Rd files.
- 2026-09-11: claim audit: 50 claims read, 0 corrected — NEWS.md, R/generate_docx.R, R/module.R, R/module_file.R, R/reliability_engine.R, R/reliability_hitopsr.R, R/score_engine.R, R/score_hitopsr.R, R/util.R, data-raw/characterize_layout/, tests/testthat/test-layout.R, tests/testthat/test-module-doc-prose.R, vignettes/articles/modules-hitopsr.Rmd. The reader noted that both new examples emitted the ascending-name warning (printed-order `hsr_` names) and that the `items` help did not say the warning fires by design under `layout = "printed"`. Fixed: examples pass positions, and both `items` entries say the warning reads the supplied names and can be ignored or avoided under `layout = "printed"`. The same reader re-read the four changed spots once: all hold, examples emit no warning.
- 2026-09-11: T5 done. `devtools::check()` on the final tree: 0 errors, 0 warnings, 0 notes once a stray untracked `Rplots.pdf` from an example run was removed (the one note named only that file). `devtools::document()` produces no diff. No new candidate row: the builder-side row already exists. All tasks ticked; status set to review.

## Decisions

## Review
