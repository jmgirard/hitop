# M081: Every item number the package ships is an integer

- **Status:** in-progress
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** IP1, GP2, GP3
- **Resolves:** —
- **Branch/PR:** `m081-item-number-integers`

## Goal

Every item-number column the package ships stores whole numbers as integers, so a caller
comparing item numbers across the package's own boundaries meets one type and the coercions
compensating for the mismatch are gone, with no shipped value moved and no exported artifact
changed.

## Scope

Surface tier: **user-facing** — the retyped columns are documented dataset columns and
`hitop_module()`'s `items` is an exported return value.

**In:** the 29 item-number paths across eight shipped datasets — `pid_items`' `FULL`, `SF`,
`BF`, `INC`, `INCS`, `ORS`, `ORSS`, `PRD`, `PRDS`, `SDTD`, `SDTDS`; `hitopsr_items$HSR`;
`hitopbr_items$HBR` and `$HSR`; `hitophsum_items$Item`; and the `itemNumbers` list-column
elements and nested `itemdata` item-number columns of `pid_scales` (FULL/SF/BF),
`hitopsr_scales`, `hitopsr_subscales`, `hitopbr_scales`. The four `data-raw/` readers that
decide the type. `hitop_module()$items`, which inherits from `hitopsr_scales$itemNumbers`.
The seven package-side coercions that exist only because these columns are doubles:
`item_order` in `generate_docx.R`'s `reduce_hitopsr()`, the `items` and `nItems` fields
`write_module()` builds for its JSON payload and its `item_order` usability check, and the
`covered`, `nItems` and `itemOrder` comparisons in `read_module()`'s verification; plus
`as.integer(hitopsr_scales$itemNumbers[[...]])` in `test-scale-name-hitopsr.R`. Roxygen
dataset docs, the `cairn/DESIGN.md` data-model bullets, `NEWS.md`, a `DECISIONS.md` entry.

**Out:** the 550 response columns of `ku_hitopsr`, `ku_hitopbr` and `ku_pid5sf`, which hold
answers rather than item numbers and disagree with the five already-integer `sim_*` datasets
→ ROADMAP candidate row. The `as.integer()` calls inside `item_names()` (`R/util.R:615-616`),
which take caller-supplied `n` → stays. The hitop-builder repo's now-redundant
`as.integer()` at `index.html:1492` → the existing builder-comment candidate row.
`rename_pid5_items()`/`label_pid5()` → its own candidate row.

## Acceptance criteria

- [ ] AC1: No shipped dataset stores an item number as a double at any nesting depth. A sweep
      over the datasets `data(package = "hitop")` lists reports every unclassed double column
      whose non-NA values are all finite and whole, and that set is exactly the 550 response
      columns of `ku_hitopsr`, `ku_hitopbr` and `ku_pid5sf` plus `hitophsum_choices$Value`.
- [ ] AC2: For each of the eight regenerated datasets, the object as it stood at the branch's
      merge base — with the Scope-named item-number columns coerced to integer at every
      nesting depth and its reader `spec` attribute retyped to match — is identical to the
      shipped object: every other column, row, list-column element, nested frame and
      attribute. Skips when the merge base already stores those columns as integer.
- [ ] AC3: No file under `inst/extdata/` or `pkgdown/assets/downloads/` differs from the merge
      base, and the generator tests that build each export fresh from the retyped tables and
      compare its item numbers, names and text against the keying tables pass.
- [ ] AC4: `hitop_module()$items` is an integer vector for every module the function can
      build, checked over a one-scale module, a several-scale module and the all-scales
      module; for each, `write_module()` emits a descriptor byte-identical to the one written
      for that module at the merge base, and `read_module()` returns from it an object
      identical to the module it was written from.
- [ ] AC5: None of the seven package-side coercions named in Scope still coerces an item
      number, and the behavior each defended holds: the `item_order` attribute of a HiTOP-SR
      Word build is an integer vector; `read_module()` accepts a descriptor `write_module()`
      wrote; `read_module()` still aborts on a descriptor whose recorded items disagree with
      the module's.
- [ ] AC6: `R/data.R`'s roxygen entries for the retyped columns state the values are
      integers, `cairn/DESIGN.md`'s `*_items` and `*_scales` data-model bullets state it,
      `NEWS.md` carries a breaking-change entry naming the retyped columns and
      `hitop_module()$items`, and `cairn/DECISIONS.md` carries an entry recording the retype
      and the waived deprecation cycle. `devtools::test()` is clean, `devtools::document()`
      leaves no diff, and `devtools::check()` reports 0 errors, 0 warnings, 0 notes.

## Coverage

- AC1 → T1, T2
- AC2 → T1, T3
- AC3 → T1, T5
- AC4 → T1, T5
- AC5 → T4
- AC6 → T6

## Tasks

- [x] T1: Add an explicit `col_types` with `readr::col_integer()` on the item-number columns
      to `data-raw/pid_info.R:6`, `hitopsr_info.R:6`, `hitopbr_info.R:6` and
      `hitophsum_info.R:2`; re-run all four builders; confirm the eight shipped objects and
      their `spec` attributes carry integer collectors on those columns. Setting the type at
      the read (never a post-hoc `as.integer()`) is what keeps `spec` truthful.
- [ ] T2: Write the whole-number-double sweep test — dataset list from
      `data(package = "hitop")`, asserted non-empty and to contain the eight keying datasets
      by name; recursing into list elements, list-columns and nested frames; columns carrying
      a class attribute excluded, so `hitop_artifacts$build_date` (a `Date`, and so a double)
      does not register. Prove it able to fail with three plants, each seen red on its own: a
      plain column (`hitopsr_items$HSR`), a list-column element
      (`pid_scales$FULL$itemNumbers[[1]]`) and a nested-frame column
      (`hitopsr_scales$itemdata[[1]]$HSR`).
- [ ] T3: Write the merge-base identity tests for the eight objects with a skip-guard on the
      merge base already carrying the change, following `tests/testthat/test-column-shape.R:61-98`.
      No existing test needs repair: that file's own `nItems` identity tests and
      `test-scale-name-hitopsr.R:205-250`'s rename-diff tests each already skip on a merge
      base cut from the current default branch (verified 2026-09-02).
- [ ] T4: Delete the seven package-side coercions named in Scope and the comments explaining
      them; retype `R/rename_hitopsr_items.R:43`'s `numeric(0)` initializer and
      `tests/testthat/test-generate_docx.R:218`'s `as.numeric(1:25)`; run `devtools::test()`.
      The file-side coercions in `read_module_numbers()` stay — JSON gives whatever its author
      wrote.
- [ ] T5: Confirm `inst/extdata/` and `pkgdown/assets/downloads/` are unmodified against the
      merge base; run the generator tests; produce the module round-trip evidence AC4 names.
- [ ] T6: Update `R/data.R`'s roxygen for the retyped columns, the two `cairn/DESIGN.md`
      data-model bullets and `NEWS.md`; append the `DECISIONS.md` entry; run
      `devtools::document()` and `devtools::check()`.

## Work log

- 2026-09-02: created by /milestone-plan.
- 2026-09-02: criteria audit ran in full mode ([O], fresh context, user-facing tier) and returned ten findings; seven fixed here — AC1 was unsatisfiable because the sweep as drafted flagged `hitop_artifacts$build_date`, AC1's recursion and non-empty-domain clauses bound the harness rather than the data and moved to T2, AC4 quantified over one module of a family, AC5 addressed its sites by line numbers that drift once T4's first deletion lands, AC6 required no decision entry where the same class of change at M078 was decision-recorded, T2 planted one defect where the walk has three structurally distinct depths, and T1's `spec` claim was read by no criterion and folded into AC2; the remaining three were posed at the gate.
- 2026-09-02: plan gate chose regenerating the four `data-raw/` builders over an in-place coercion script against a pinned commit, because an in-place script leaves the builders still emitting doubles (M078's own reason for rejecting it) and M080's review found such scripts carry no already-applied guard; falsified by a source CSV that does not reproduce its shipped table, which T1 surfaces.
- 2026-09-02: plan gate chose retyping all 29 item-number paths over the narrower set the ROADMAP candidate row named, because one `col_types` edit per builder reaches all of them and a partial retype leaves an odd one out as M078's did; falsified by a downstream caller depending on a double in one of the added paths.
- 2026-09-02: plan gate chose leaving the three `ku_*` datasets' 550 response columns as doubles over evening them out with the five already-integer `sim_*` datasets, because response values are not item numbers and `data-raw/ku_data.R` reads a university network drive so cannot be re-run; falsified by a user reporting the simulated/real disagreement.
- 2026-09-02: Jeff signed off at the plan gate on regenerating the four keying-table files and waived the deprecation cycle as pre-1.0, as at M078's and M080's gates.
- 2026-09-02: T1 — the four builders read their item-number columns with `readr::col_integer()`; all eight shipped objects now store every Scope-named item-number path as integer at every depth, and the `spec` attributes of `pid_items`, `hitopbr_items` and `hitophsum_items` carry integer collectors on those columns (`hitopsr_items` carries no `spec`: `dplyr::select()` drops it, at the merge base too). `devtools::test()`: 0 failures, 16431 passes, 7 skips.

## Decisions

## Review
