# M081: Every item number the package ships is an integer

- **Status:** review
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** IP1, GP2, GP3
- **Resolves:** —
- **Branch/PR:** `m081-item-number-integers` / https://github.com/jmgirard/hitop/pull/88

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

- [x] AC1: No shipped dataset stores an item number as a double at any nesting depth. A sweep
      over the datasets `data(package = "hitop")` lists reports every unclassed double column
      whose non-NA values are all finite and whole, and that set is exactly the 550 response
      columns of `ku_hitopsr`, `ku_hitopbr` and `ku_pid5sf` plus `hitophsum_choices$Value`.
- [x] AC2: For each of the eight regenerated datasets, the object as it stood at the branch's
      merge base — with the Scope-named item-number columns coerced to integer at every
      nesting depth and its reader `spec` attribute retyped to match — is identical to the
      shipped object: every other column, row, list-column element, nested frame and
      attribute. Skips when the merge base already stores those columns as integer.
- [x] AC3: No file under `inst/extdata/` or `pkgdown/assets/downloads/` differs from the merge
      base, and the generator tests that build each export fresh from the retyped tables and
      compare its item numbers, names and text against the keying tables pass.
- [x] AC4: `hitop_module()$items` is an integer vector for every module the function can
      build, checked over a one-scale module, a several-scale module and the all-scales
      module; for each, `write_module()` emits a descriptor byte-identical to the one written
      for that module at the merge base, and `read_module()` returns from it an object
      identical to the module it was written from.
- [x] AC5: None of the seven package-side coercions named in Scope still coerces an item
      number, and the behavior each defended holds: the `item_order` attribute of a HiTOP-SR
      Word build is an integer vector; `read_module()` accepts a descriptor `write_module()`
      wrote; `read_module()` still aborts on a descriptor whose recorded items disagree with
      the module's.
- [x] AC6: `R/data.R`'s roxygen entries for the retyped columns state the values are
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
- [x] T2: Write the whole-number-double sweep test — dataset list from
      `data(package = "hitop")`, asserted non-empty and to contain the eight keying datasets
      by name; recursing into list elements, list-columns and nested frames; columns carrying
      a class attribute excluded, so `hitop_artifacts$build_date` (a `Date`, and so a double)
      does not register. Prove it able to fail with three plants, each seen red on its own: a
      plain column (`hitopsr_items$HSR`), a list-column element
      (`pid_scales$FULL$itemNumbers[[1]]`) and a nested-frame column
      (`hitopsr_scales$itemdata[[1]]$HSR`).
- [x] T3: Write the merge-base identity tests for the eight objects with a skip-guard on the
      merge base already carrying the change, following `tests/testthat/test-column-shape.R:61-98`.
      No existing test needs repair: that file's own `nItems` identity tests and
      `test-scale-name-hitopsr.R:205-250`'s rename-diff tests each already skip on a merge
      base cut from the current default branch (verified 2026-09-02).
- [x] T4: Delete the seven package-side coercions named in Scope and the comments explaining
      them; retype `R/rename_hitopsr_items.R:43`'s `numeric(0)` initializer and
      `tests/testthat/test-generate_docx.R:218`'s `as.numeric(1:25)`; run `devtools::test()`.
      The file-side coercions in `read_module_numbers()` stay — JSON gives whatever its author
      wrote.
- [x] T5: Confirm `inst/extdata/` and `pkgdown/assets/downloads/` are unmodified against the
      merge base; run the generator tests; produce the module round-trip evidence AC4 names.
- [x] T6: Update `R/data.R`'s roxygen for the retyped columns, the two `cairn/DESIGN.md`
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
- 2026-09-02: T2 — `tests/testthat/test-item-number-type.R` sweeps the 23 datasets `data(package = "hitop")` lists, recursing through list elements, list-columns and nested frames, and reports exactly the 550 `ku_*` response columns plus `hitophsum_choices$Value`; the three plants (plain column, list-column element, nested-frame column) were each substituted into the sweep on their own and the criterion assertion went red naming that path, with the unplanted baseline green.
- 2026-09-02: T3 — `tests/testthat/test-item-number-merge-base.R` rebuilds each of the eight merge-base objects with its item-number columns retyped at every depth and its `spec` collectors retyped, and finds them identical to the shipped objects; a second test pins that `expect_equal()` passes over the difference `expect_identical()` catches. All eight ran (no skips). `devtools::test()`: 0 failures, 16455 passes, 7 skips.
- 2026-09-02: T4 — the seven coercions are gone: `item_order` in `reduce_hitopsr()` and its comment, the `items`/`nItems` payload fields, the `item_order` usability check's `module$items` coercion, and `read_module()`'s `covered`, `nItems` and `itemOrder` comparisons; `matched_hsr` initializes as `integer(0)` and the two tests compare item numbers as stored. `as.integer(item_order)` stays at both sites that normalize a hand-set attribute, as does `read_module_numbers()`. The three behaviors AC5 names are each held by a named test already in the suite (`test-docx-numbering.R:281`, `test-generator-descriptor.R:179`, `test-module_file.R:105`). `devtools::test()`: 0 failures, 16455 passes, 7 skips.
- 2026-09-02: T5 — `git diff` against merge base `8dd3942` reports no file changed under `inst/extdata/` or `pkgdown/assets/downloads/`; the generator and artifact test files pass (0 failures, 10338 passes, 0 skips). Module round trip over a one-scale, a three-scale and the 405-item all-scales module: `$items` is integer on the branch where the merge base returned a double for all three, each descriptor `write_module()` emits is byte-identical to the merge base's for the same module, and `read_module()` returns an object identical to the module written.
- 2026-09-02: T6 — `R/data.R`'s roxygen names the type on all 13 retyped column entries and on the `itemdata`/`itemNumbers` list-column entries, `hitop_module()`'s `@return` calls `items` integer, the two `cairn/DESIGN.md` data-model bullets state it, `NEWS.md` carries the breaking-change entry, and `DECISIONS.md` carries D-056. First `devtools::check()` run raised one WARNING — the new merge-base test called `readr::col_integer()`, and {readr} is not a package dependency; the test now renames the merge base's own collector class instead. `devtools::check()`: 0 errors, 0 warnings, 0 notes. `devtools::document()` leaves no diff. `devtools::test()`: 0 failures, 16469 passes, 7 skips.

## Decisions

## Review

Reviewed 2026-09-02 against branch `m081-item-number-integers` at merge base `8dd3942`
(the default branch had not moved). PR: https://github.com/jmgirard/hitop/pull/88.

### Acceptance-criteria evidence

- AC1 — an independent sweep (written at review, not the branch's test) over the 23 datasets
  `data(package = "hitop")` lists, recursing through list elements, list-columns and nested
  frames and excluding classed doubles, reports 551 whole-number double paths: the 405
  `ku_hitopsr`, 45 `ku_hitopbr` and 100 `ku_pid5sf` response columns, plus
  `hitophsum_choices$Value`. Nothing else. The branch's own `test-item-number-type.R` passes
  (12 expectations, no skips).
- AC2 — `test-item-number-merge-base.R` ran for real, no skips: 26 expectations green,
  rebuilding each merge-base object with its item-number columns and `spec` collectors
  retyped and finding it `identical()` to the shipped object. Independently confirmed that
  the guard could not have vacuously passed: at `8dd3942` all 337 item-number paths across
  the eight objects are doubles, and on the branch all 337 are integers.
- AC3 — `git diff 8dd3942..HEAD -- inst/extdata pkgdown/assets/downloads` reports no changed
  file (24 files in each directory). The generator, artifact, docx, qualtrics, redcap and
  module test files ran together: 0 failures, exit 0.
- AC4 — over a one-scale (5 items), a three-scale (18 items) and the all-scales (405 items)
  HiTOP-SR module — the only instrument `hitop_module()` builds — `$items` is `integer` on
  the branch where the merge base gave `double`; each descriptor `write_module()` emits is
  byte-identical (`cmp`) to the one the merge-base package writes for the same module, built
  from a `git archive` of `8dd3942`; `read_module()` returns an object `identical()` to the
  module written, in all three cases and at both revisions.
- AC5 — the seven coercions are absent from the diff; what remains in the touched files
  coerces only caller- or file-supplied numbers (`write_module()`'s hand-settable
  `item_order`, `read_module_numbers()`, `validate_items()`'s `nItems`). The three named
  behaviors, run fresh: a HiTOP-SR Word build's `item_order` is `integer` and
  `identical()` to `1:405`; `read_module()` returns the module `write_module()` wrote;
  a descriptor with one item number altered aborts with class
  `hitop_module_file_items_mismatch`, naming the covered and recorded numbers.
- AC6 — `R/data.R`'s roxygen names the integer type on all 13 retyped column entries and on
  the `itemdata`/`itemNumbers` list-column entries; `hitop_module()`'s `@return` says
  integer `items`; both `cairn/DESIGN.md` data-model bullets state it; `NEWS.md` carries the
  breaking-change entry naming every retyped column and `hitop_module()$items`;
  `cairn/DECISIONS.md` carries D-056. `devtools::document()` leaves no diff;
  `devtools::check()` is 0 errors, 0 warnings, 0 notes; `devtools::test()` is 0 failures, 0 warnings, 16469 passes, 7 skips.

### Consistency gate

- `cairn_validate.py`: exit 0, all checks passed (24 advisories, all pre-existing — dangling
  `D-001`..`D-012` id tokens and one references-staleness note; the `release window` advisory
  did not fire).
- `cairn_impact.py`: not run — the diff changes no DESIGN principle text.
- Toolchain slot: `devtools::document()` no diff; no hand-edited generated files;
  `pkgdown::check_pkgdown()` "No problems found"; `NEWS.md` entry present and free of
  milestone numbers; no new top-level files; `devtools::check()` 0/0/0.

### Independent review

Full three-lens fan-out (the diff touches R code and tests). The blame-history lens traced
each of the seven deleted coercions to the commit that added it and found no defense no
longer met and no recorded decision contradicted: no findings. The prior-review lens found
the touched files last reviewed at M077, M078 and M080, whose deferred item-number-type
finding is what this milestone closes, and confirmed the two lessons in play (a `spec`
attribute going stale under a retype; `expect_equal()` masking a type regression) are
honored rather than repeated: no findings. The diff-bug lens returned ten, ranked; each with
its disposition:

1. **`write_module()` aborted on a module whose `items` are doubles.** `R/module_file.R:149`
   dropped the coercion on one side of the comparison only, leaving
   `identical(sort(as.integer(item_order)), module$items)`. Reproduced at review: a module
   with double `items` and a valid `item_order` permutation aborts on the branch where the
   merge-base package wrote the file — reachable through a `hitop_module` saved to `.rds`
   under 0.2.0, and through `generate_docx_hitopsr(module = <that module>, descriptor = ...)`.
   **Fixed here:** the check compares the order against the module's items by value and by
   length, coercing only the caller-supplied `item_order`. Two tests added — one that the
   double-items module writes (seen red against the pre-fix line, aborting with the
   "unusable `item_order`" message), one that a substituted, a repeated and a short order
   are each still refused, which passed under both versions.
2. **`hitop_module()$items` had no permanent type test.** AC4 held at review, but nothing in
   the suite pinned it: `test-module_file.R` wrapped `as.integer()` around both sides of
   every item comparison, and `test-module.R` used `expect_equal()`, which ignores the
   difference. **Fixed here** together with finding 5.
3. **The `item_order`-is-integer guard sits behind `skip_if_no_docx()`.** True, and a
   property of the suite the branch did not introduce; finding 2's fix now pins the type on
   a path that needs no Word toolchain. **Rejected** as pre-existing.
4. **The merge-base test's skip guard was vacuous for four of its eight objects.**
   `skip_without_double_base()` probed a named column on the top-level frame, which is `NULL`
   for the four `*_scales` objects that hold item numbers only in list-columns and nested
   frames; the test skipped post-merge only because `pid_items` is iterated first.
   **Fixed here:** the guard is now the retype itself being a no-op on that object.
5. **Five equivalent coercions left in `test-module_file.R`.** The same class D-056 deletes,
   masking the type on both sides. **Fixed here:** removed, so those assertions now hold the
   type; `test-module.R`'s first module test compares against integer literals with
   `expect_identical()`.
6. **D-056 does not restate the IP1 reasoning D-053 carried.** The sign-off itself is
   recorded — the work log names Jeff's plan-gate sign-off on regenerating the four
   keying-table files, and D-056 names the plan gate. **Rejected:** the fact is on record,
   and `DECISIONS.md` is append-only.
7. **NEWS said "arithmetic ... behave as before" without qualification.** Integer arithmetic
   overflows where double arithmetic does not, though not at item-number magnitudes.
   **Fixed here:** the sentence names the limit.
8. **`R/sysdata.rda` carries the same disagreement between response-value columns.**
   Confirmed: `pid_instructions$options$value` is integer where `hitopsr_instructions`' and
   `hitopbr_instructions`' are doubles. Response values, not item numbers, and internal data
   the sweep's `data(package = "hitop")` domain cannot reach. **Follow-up:** absorbed into
   the existing `ku_*` response-type candidate row, which covers the same class.
9. **`bare_whole_double()` excludes any classed double, not only `Date`.** An item-number
   column later stored as a labelled or units vector would pass the sweep. No such column
   exists. **Rejected** as speculative; the exclusion is what keeps `hitop_artifacts$build_date`
   out of the report.
10. **`matched_hsr`'s `integer(0)` retype is cosmetic.** Only `length(unique())` is taken
    from it. **Rejected:** true, and harmless.

No finding demonstrated an acceptance criterion failing. Finding 1 is a defect no criterion
covers; it is fixed on the branch rather than returned, and the fix is re-verified below.

### Re-verification after the review fixes

`devtools::test()`: 0 failures, 0 warnings, 16480 passes, 7 skips (the same 7, so no
merge-base test newly skipped). `devtools::check()`: 0 errors, 0 warnings, 0 notes.
`devtools::document()` leaves no diff. `cairn_validate.py`: exit 0. AC1's sweep, AC4's three
module round trips (descriptors still byte-identical to the merge-base package's) and AC5's
three named behaviors were each re-run and hold.
