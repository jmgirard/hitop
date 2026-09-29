<!-- Section ownership + write-modes: see tracking-rules.md "Milestone-file
     section ownership". A phase skill never rewrites another phase's section. -->
# M142: HiTOP-SR module functions refuse a module whose items do not match its scales

- **Status:** in-progress
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** GP2, GP3
- **Resolves:** —
- **Surface tier:** user-facing — six exported functions refuse a new input, and the Word form gains subscale rows
- **Branch/PR:** m142-module-mismatch-refusal

## Goal

Every HiTOP-SR function that takes a module accepts only a module whose items are the items its scales cover. That guarantee lets `generate_docx_hitopsr()` add the module's own subscales to its scoring page.

## Scope

**In:** One shared check, moved out of `write_module()` (`R/module_file.R:154-200`). It compares a module with a fresh `hitop_module()` build of its `instrument` and `scales`. It compares `items`, `nItems` and `camelCase`, and it raises the new public class `hitop_module_mismatch` (D-081). `write_module()`, `score_hitopsr()`, `reliability_hitopsr()` and the three HiTOP-SR generators run it. `generate_docx_hitopsr()` drops its refusal of `module` with `include_subscales = TRUE` (`R/generate_docx.R:230-239`). It adds the subscales whose parent scale the module holds, as `score_hitopsr()` does since M141. It checks `include_subscales` with `validate_flag()`. Help pages, the modules article, NEWS and the tests follow.

**Out:** A class for the other module refusals in `module_engine_inputs()` (wrong object class, wrong instrument). They stay unclassed until a caller needs one. A check of the `reverse` field, which no consumer reads, because they read reverse keys from the package tables. Subscale rows in the Qualtrics and REDCap exports, which carry no scoring key. The article sentence that a subscale is not a selectable unit stays, because it stays true. Modules for other instruments stay with the "Generalize modularization to BR/PID-5" candidate row.

## Acceptance criteria

- [ ] AC1: Six functions refuse a `module` that does not match a fresh `hitop_module()` build of its `instrument` and `scales`. The six are `write_module()`, `score_hitopsr()`, `reliability_hitopsr()`, `generate_docx_hitopsr()`, `generate_qualtrics_hitopsr()` and `generate_redcap_hitopsr()`. A module matches when its `items` are numeric, hold no `NA`, and equal the items of the build by value and in order. Its `nItems` must equal their count. Its `camelCase` must be identical to the `camelCase` of the build. Every refusal is an error of class `hitop_module_mismatch`. If the module lacks items of its scales or holds items outside them, the message names those items. The message also names each field that differs. An `NA` or a value that is not a number is a fault of the `items` field. If `hitop_module()` refuses a name in `scales`, the message names that scale, and the `hitop_module()` error is its parent.
- [ ] AC2: For each of the six functions, a test fires the AC1 refusal on eleven modules. It asserts the class and the message content that AC1 states. Three modules lack one item, removed from the start, the middle and the end of one scale. One module has an item of a scale that it does not hold appended. One has two items swapped, and one has an item appended a second time. One has an item replaced by `NA`, and one has its items converted to character strings. One has only `nItems` changed. One has only one `camelCase` entry replaced by the name of another scale. One has an unknown scale name appended to `scales`. A test of `write_module()` asserts that no file exists at its `file` after the call. A generator test runs once with `descriptor = NULL` and once with `descriptor` set, each at a new path. It asserts that no file exists at `file` or at `descriptor` after the call. For `score_hitopsr()` and `reliability_hitopsr()`, the test also runs the three lacking modules under `include_subscales = TRUE`, on a parent scale with subscales. It asserts the class and that the message does not match "Internal error".
- [ ] AC3: The six functions accept three kinds of module with no error, and the call itself raises no warning. The three are a module built beforehand by `hitop_module()`, one built beforehand by the deprecated `hitop_subset()`, and a `hitop_module()` build whose `items` are converted to doubles. The last kind is what a module saved before item numbers were integers holds. A test for each function shows this over two modules of different scales. `reliability_hitopsr()` runs with `omega = FALSE`, because the omega fit can warn on simulated data.
- [ ] AC4: `generate_docx_hitopsr(module = m, include_subscales = TRUE)` writes a scoring page with one row for each scale of the module and one `(Subscale)` row for each `hitopsr_subscales` row whose parent scale `m` holds. The page holds no other subscale row. All rows are sorted by printed name, read down the left column and then down the right, as on a full-instrument form with `include_subscales = TRUE`. Each subscale row lists the items and reverse marks of its subscale in the printed numbers of the form. Tests cover a module that holds two parents with subscales and a module that holds no parent, which gets no subscale row. They also cover `renumber = FALSE`, and `randomize = TRUE` with the default `renumber`, where the returned order is shown not to be ascending. The tests read the expected items from `hitopsr_subscales` and the returned `item_order`. `generate_docx_hitopsr()` refuses an `include_subscales` that is not a single `TRUE` or `FALSE`, as `score_hitopsr()` does. A test fires this refusal for `1`, `1L`, `"yes"`, `NA` and `c(TRUE, FALSE)`.
- [ ] AC5: A case-insensitive search for "subscale" in `R/`, `man/`, `vignettes/`, `tests/` and `README.Rmd` finds each passage about subscales. No passage it finds says that `include_subscales` cannot be combined with `module`, or that a subscale draws items from outside a module. The modules article and `?generate_docx_hitopsr` say that a module form adds the subscales of the scales it holds. The help page of each of the six functions names the class `hitop_module_mismatch`.
- [ ] AC6: The development-version section of NEWS.md lists the AC1 refusal under Breaking changes. That entry says that a module saved before a scale rename, such as Body Focus to Appearance Focus, is now refused. The section lists the module subscales of the Word form (AC4) under New features. Each entry asserts only behavior that an AC2 to AC4 test enforces.
- [ ] AC7: `devtools::check()` reports 0 errors and 0 warnings. The chunks of the modules article run without error against the branch, purled and run, because check does not run `vignettes/articles/`.

## Coverage

- AC1 → T1, T2
- AC2 → T1, T2
- AC3 → T1, T2
- AC4 → T3
- AC5 → T4
- AC6 → T4
- AC7 → T5

## Tasks

- [x] T1: Write the AC2 probe tests and the AC3 accept tests for `write_module()` first. Then move its rebuild comparison (`R/module_file.R:154-200`) into one helper in `R/module.R`. The helper adds the `camelCase` comparison, the item naming and the class that AC1 states. `write_module()` calls the helper before it opens the path. Keep the "Cannot rebuild" test at `tests/testthat/test-module_file.R:657` passing or move it to the new class.
- [x] T2: Write the AC2 and AC3 tests for the other five functions first, with both `descriptor` modes for the generators. Then call the helper from `module_engine_inputs()` (`R/module.R:195`) and `apply_module()` (`R/module.R:146`), before any file is opened. Correct the invariant comments at `R/module.R:191-193` and in `add_hitopsr_subscales()`.
- [x] T3: Write the AC4 tests first. Replace the refusal test at `tests/testthat/test-generate_docx.R:173-191` and the comment at `tests/testthat/test-docx-numbering.R:328-336`. Then remove the refusal at `R/generate_docx.R:230-239` and add `validate_flag(include_subscales)`. Choose the in-module subscales through one helper that `add_hitopsr_subscales()` shares.
- [x] T4: Update the `module` help of the six functions to name the class. Update the `include_subscales` and `module` help of `generate_docx_hitopsr()`, the `remap_itemdata()` comment (`R/generate_docx.R:388-391`), and the article paragraph (`vignettes/articles/modules-hitopsr.Rmd:110-113`). Change `tests/testthat/test-module-doc-prose.R:158-163` to assert the new article statement. Run `devtools::document()`, run the AC5 search and read each hit, and write the two NEWS entries.
- [ ] T5: Run `devtools::check()`. Purl the modules article and run it after `devtools::load_all()` (LESSONS, M115 and M096).

## Work log

- 2026-09-29: created by /milestone-plan. Absorbs two candidate rows added 2026-09-29 (lineage M024, M141).
- 2026-09-29: criteria audit, full mode, fresh [O] reader, two passes. Pass 1 returned 10 findings: 6 fixed in the wording, 4 posed at the gate. Pass 2 returned 8: 5 fixed in the wording, 3 with no finding.
- 2026-09-29: plan gate chose all five module functions plus `write_module()` over the scoring pair only, because otherwise the Word form keeps printing NA and cannot add subscales; falsified by a caller who needs a generator to accept an edited module.
- 2026-09-29: plan gate chose to compare `camelCase` too over `items` and `nItems` only, because the consumers choose scales by `camelCase`; falsified by a consumer that selects scales by another field.
- 2026-09-29: plan gate chose the public class `hitop_module_mismatch` over an unclassed refusal like the one in `write_module()`; falsified by no caller ever catching it and the class costing a rename.
- 2026-09-29: plan gate chose an immediate refusal under Breaking changes over a one-release warning, because the old behavior gave wrong scores (pre-1.0 waiver, D-081); falsified by a user report of a study that relied on scoring an edited module.
- 2026-09-29: implement started on m142-module-mismatch-refusal. Question gate skipped: the plan left no implementation choice open.
- 2026-09-29: T1 done. `check_module_build()` in `R/module.R` holds the rebuild comparison, adds `camelCase` and the class, and names lacking, extra and repeated items and unknown scales. `write_module()` calls it. New `test-module-mismatch.R` runs the 11 probes and 3 accepted kinds over `write_module()`. Suite 0 failed, 0 errors.
- 2026-09-29: T2 done. `apply_module()` and `module_engine_inputs()` call `check_module_build()`. The old unclassed nItems-consistency refusal in `module_engine_inputs()` is gone, and its three tests in `test-module.R` now assert the new class. The probe matrix covers all six functions, both `descriptor` modes, and the subscale path. Plant: with the T2 wiring stashed, the refusal and subscale tests go red (24 failures in the latter). Suite 0 failed, 0 errors.
- 2026-09-29: amendment (mini gate, Jeff chose the recommended option): AC4 promised subscale rows after the scale rows in `hitopsr_subscales` order, but `make_scoring_table()` sorts every row by name in two columns, as the full form with subscales already does. AC4 now states that sorted order. Rejected: changing the table, which moves the full form's layout too; falsified by a reader who needs subscale rows grouped apart.
- 2026-09-29: re-audit: AC4 (full) — 6 findings: column reading order, "each subscale row", and a shuffle that must be shown to move applied to the wording; `NA` and `c(TRUE, FALSE)` added to the flag test; collation left unstated (current names sort the same under C and en_US); reachability and tier clean.
- 2026-09-29: T3 done. `module_subscales()` in `R/module.R` picks the subscales of held parents for both `add_hitopsr_subscales()` and the Word form. `generate_docx_hitopsr()` drops its refusal and checks `include_subscales` with `validate_flag()`. The `remap_itemdata()` comment is corrected here, not in T4. Four module-subscale tests in `test-docx-numbering.R`; the refusal test in `test-generate_docx.R` now tests the flag. Plant: all 17 subscales on a module form turns the four tests red. Suite 0 failed, 0 errors.
- 2026-09-29: T4 done. The help of the six functions names `hitop_module_mismatch`; the modules article and `?generate_docx_hitopsr` state the module subscale rows, and `test-module-doc-prose.R` asserts the article sentence. NEWS: one Breaking changes entry and one New features entry. Added one test beyond the plan: a module naming Body Focus is refused, because the NEWS entry names that case. AC5 search: 348 "subscale" lines; the lines naming a module or a refusal word were read, and none repeats the retired claim. Suite 0 failed, 0 errors.

## Decisions

## Review
