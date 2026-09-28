<!-- Section ownership + write-modes: see tracking-rules.md "Milestone-file
     section ownership". A phase skill never rewrites another phase's section. -->
# M132: Printed-layout names that match the module's order score without a warning, and a fractional `item_order` is refused

- **Status:** in-progress
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** GP2, GP3
- **Resolves:** —
- **Surface tier:** user-facing — two exported scoring functions change their warning, and three exported functions refuse new input
- **Branch/PR:** m132-printed-layout-loose-ends

## Goal

Close the M091 review's printed-layout loose ends, first the warning that fires on names in the module's own printed order.

## Scope

**In:**
- When the trailing numbers of the names equal the module's `item_order`, `warn_item_order()` stays silent under `layout = "printed"`. Other non-ascending names still warn. The default layout does not change.
- The printed-layout remedy text in the warning says that names in the module's printed order pass.
- One internal predicate for "`item_order` is a permutation of the module's items". It refuses a value that is not a finite whole number. `layout_items()` and `write_module()` both call it.
- A test of `layout = "printed"` through the deprecated `subset` argument.
- Delete `data-raw/characterize_layout/` (M091's one-off characterization).
- The `items` help of both scoring functions, the modules article (its hitop-form example scores by name), and two NEWS entries.

**Out:**
- A warning for printed-layout names that hold the module's items sorted ascending. The plan gate rejected it (work log). No row.
- Condition classes for these refusals. They stay unclassed, as M091's plan gate chose. The numeric score-column class question stays its own candidate row.
- `read_module()`'s `itemOrder` check. It already refuses non-whole values under `hitop_module_file_bad_item_order` and is unchanged.
- `write_descriptor_sidecar()`'s `as.integer(item_order)` (`R/module_file.R:757`). It receives only generator-built integers.

## Acceptance criteria

- [ ] AC1: When the trailing numbers of the `items` names equal the module's `item_order` element by element, `score_hitopsr()` and `reliability_hitopsr()` raise no ascending-order warning under `layout = "printed"`. Two tests show it. The first scores the hitop-form fixture `responses-module-shuffled.csv` with `module-shuffled.json` and `items` as its zero-padded item-column names. It raises no warning in either function. `score_hitopsr()` returns the same tibble as the call by positions, with `hsr_agoraphobia` 3 and `hsr_distressDysphoria` 2.4375 (hand-worked in M096). The second test uses unpadded names, `paste0("q_", item_order)`, on a test-built module, and it also raises no warning in either function.
- [ ] AC2: Under `layout = "printed"`, some names share one prefix but have trailing numbers that are neither ascending nor equal to `item_order`. Such names raise exactly one ascending-order warning in each function. Its text names positions, says that names in the module's printed order pass, and does not contain "Sort them". Ascending names raise no warning. Under the default layout, the warning's trigger and wording are unchanged. The probes are these: a random permutation of the fixture's names, `item_order` with one adjacent pair swapped, and reversed `q_n … q_1` (each warns once), and ascending `q_1 … q_n` (no warning). The existing tests in `test-item-guards.R` pass. The fixture's names under the default layout still warn with "Sort them".
- [ ] AC3: `score_hitopsr()` and `reliability_hitopsr()` (through `layout_items()`) and `write_module()` refuse a module whose `item_order` holds a fraction, where before they truncated it. They refuse an `item_order` holding `Inf` or `-Inf` with no base-R coercion warning. "Whole" means exactly whole, with no tolerance. Each of the three functions has these probes: one entry of a valid order raised by 0.5, one raised by 0.25, one entry `Inf`, and one entry `-Inf`. Each refusal is asserted by its message. A control passes the same valid order as whole-valued doubles. The scoring functions accept it and return results equal to the integer order's. `write_module()` writes a file whose `read_module()` `item_order` is identical to `as.integer(order)`. Both sites call one internal helper that holds the test.
- [ ] AC4: `score_hitopsr(subset = m, layout = "printed")` and `reliability_hitopsr(subset = m, layout = "printed")` each raise one `hitop_deprecated_subset` warning. Each returns a result equal to the same call with `module = m`.
- [ ] AC5: `data-raw/characterize_layout/` is deleted: `git ls-files data-raw/characterize_layout` prints nothing.
- [ ] AC6: `git grep -n -i -e printed -e position` runs over `R/score_hitopsr.R`, `R/reliability_hitopsr.R`, `R/util.R` and `vignettes/articles/modules-hitopsr.Rmd`. No text it returns says that names in the module's printed order warn. None says that positions are needed to avoid the warning. The `items` help of both scoring functions and the modules article say that such names pass with no warning. The article's hitop-form example scores the fixture with `items` as names. NEWS.md gains one entry for the warning change and one for the `item_order` refusal.
- [ ] AC7: `devtools::document()` leaves no diff. `devtools::test()` reports 0 failures. `devtools::check()` reports 0 errors, 0 warnings and 0 notes. The modules article's chunks are purled and sourced, and they run with no error.

## Coverage

- AC1 → T2
- AC2 → T2
- AC3 → T1
- AC4 → T3
- AC5 → T5
- AC6 → T1, T2, T4
- AC7 → T6

## Tasks

- [x] T1: Add one internal predicate in `R/module.R`: numeric, no `NA`, all finite, each value exactly whole, length equal to the module's items, and sorted values equal to the sorted items. Call it from `layout_items()` (`R/module.R:333`) and `write_module_impl()` (`R/module_file.R:236`). The messages stay as they are. Add the AC3 probes and controls to `test-layout.R` and `test-module_file.R`. Add the NEWS entry for the refusal.
- [ ] T2: Give `warn_item_order()` (`R/util.R:142`) an `item_order` argument that it reads only under `layout = "printed"`. When the trailing integers equal `item_order`, it returns silently. `layout_items()` passes it (`R/module.R:348`). Rewrite the printed-layout remedy text (`R/util.R:158`). Rewrite the PR #105 regression test in `test-layout.R` (the "says positions, not sort" test). It now asserts the remedy on a permuted name order and keeps its instrument-layout and scoring checks. Add the AC1 and AC2 tests. Add the NEWS entry for the warning change.
- [ ] T3: In `test-layout.R`, test `layout = "printed"` through `subset` in both functions (AC4). Count warnings by class.
- [ ] T4: Update the `items` roxygen (`R/score_hitopsr.R:9`, `R/reliability_hitopsr.R:12`). In the modules article's hitop-form section, pass names and drop the positions paragraph. Run `devtools::document()`. Purl and source the article (LESSONS, M115).
- [ ] T5: Delete `data-raw/characterize_layout/`. Git history keeps it, and the M091 archive summary names it as history.
- [ ] T6: Gate: `document()` with no diff, `test()`, `check()`, the purled article, and `cairn_validate`.

## Work log

- 2026-09-27: created by /milestone-plan. It absorbs two candidate rows: the printed-layout `warn_item_order()` row (M096 lineage) and the four M091 review loose ends (F3, F4, F7, F9).
- 2026-09-27: criteria audit, full mode, fresh [O] reader. There were 11 findings on AC1–AC3, AC5 and AC6, and all were fixed before the gate. In AC1 and AC2 they covered the PR #105 test rewrite, an unpadded-name probe, an adjacent-swap probe and the `R/util.R` remedy text. In AC3 they covered the `Inf` claim (already refused, now with no leaked warning), fraction probes beyond 0.5 and the read-back type. In AC5 and AC6 they covered a vacuous grep and AC6's domain and phrasing. AC4 and AC7 had none.
- 2026-09-27: plan gate chose silence on an exact `item_order` match over no check under the printed layout. Reversed `q_n … q_1` names still signal a misordered mapping. The gate also rejected an added warning for sorted columns, because a whole-form file named `q_1`…`q_405` then warns falsely. Falsified by a caller whose correct printed-order names neither ascend nor match `item_order`.
- 2026-09-27: plan gate chose to delete `data-raw/characterize_layout/` over making it self-contained, because it was one-off M091 evidence that nothing uses. Falsified by a later layout change that needs the same merge-base comparison and cannot rebuild it from git history.
- 2026-09-27: plan gate chose names over positions in the article's hitop-form example, because names no longer warn and are shorter to read. Falsified by a reader who scores by name and gets a warning the article did not predict.
- 2026-09-27: T1 done. `is_item_permutation()` in `R/module.R` is called by `layout_items()` and `write_module_impl()`. The fraction and infinity tests fail on the old code, and the whole-valued double controls pass on both. Full suite 0 failed.

## Decisions

## Review
