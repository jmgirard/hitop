<!-- Section ownership + write-modes: see tracking-rules.md "Milestone-file
     section ownership". A phase skill never rewrites another phase's section.
     Per-section owners are tagged below. The one size check that can fail is
     cairn_validate's <150 over the plan-owned body. -->
# M085: The `rename_*`/`label_*` family's warning contract

- **Status:** in-progress   <!-- owner: transitioning skill · mirror-update; cairn/ROADMAP.md is the authority -->
- **Priority:** normal   <!-- owner: plan · create/amend-via-gate; high | normal | low -->
- **Depends on:** —   <!-- owner: plan · create/amend-via-gate; M<xx>, M<yy> or — -->
- **Driving RR:** —   <!-- owner: plan · create/amend-via-gate -->
- **Principles touched:** GP3   <!-- owner: plan · create/amend-via-gate -->
- **Resolves:** —   <!-- owner: plan · create/amend-via-gate -->
- **Branch/PR:** `m085-rename-label-warning-contract`   <!-- owner: implement (branch) / review (PR URL) · create -->

## Goal
<!-- owner: plan · create; a wrong goal returns to plan, never edited in place -->

Every warning the two `rename_*()` and three `label_*()` helpers raise carries a
condition class a caller can catch, with the family's unpinned edge paths
covered by tests asserting that class rather than message prose.

## Scope
<!-- owner: plan · create/amend-via-gate -->

Surface tier: **user-facing** — condition classes are a public contract
(D-034(c)), and the change ships in docs and NEWS.

**In:** the eleven inline `cli::cli_warn()` sites in `R/rename_pid5_items.R`,
`R/rename_hitopsr_items.R`, `R/label_pid5.R`, `R/label_hitopsr.R` and
`R/label_hitopbr.R`. The eight nothing-matched reports gain
`hitop_no_columns_matched`; the two completeness reports gain
`hitop_incomplete_rename`; `rename_hitopsr_items()`'s unmatched item-text
report adopts the existing `hitop_unmatched_items` through
`warn_unmatched_items()` (`R/util.R:675`), the retrofit D-057 deferred. The
stray base-R coercion warning `rename_pid5_items()` leaks on a past-integer
item number is silenced. Tests pin each path by class, plus the out-of-range
classification of `<prefix>000` and a past-integer-range column.

**Out:** classing the warnings outside this family (`norm_pid5()`,
`validity_pid5()`, `interval_*()`, `warn_item_order()`) → the standing
message-content candidate row. Any change to what the messages say, to what
matches, or to the padding rule. The three blocked PID-5 rename/label
remainders (validity-scale labels, `_se` labels, the `version` argument) →
their own candidate row.

## Acceptance criteria
<!-- owner: plan · create/amend-via-gate; review reads, never reinterprets. -->

- [ ] AC1: `rename_hitopsr_items(method = "text")` raises its unmatched
      item-text report through `warn_unmatched_items()`, so the warning carries
      class `hitop_unmatched_items`; a test catches it by class.
- [ ] AC2: Each of the eight nothing-matched paths raises class
      `hitop_no_columns_matched` — `rename_pid5_items(method = "number")`,
      `rename_hitopsr_items(method = "original")`, and each of `label_pid5()`,
      `label_hitopsr()` and `label_hitopbr()` under both `target = "items"` and
      `target = "scales"` — with one test per path catching it by class.
- [ ] AC3: Both completeness reports — `rename_pid5_items()`'s and
      `rename_hitopsr_items()`'s "only N of M items renamed" — raise class
      `hitop_incomplete_rename`, caught by class in a test for each helper.
- [ ] AC4: `rename_pid5_items(method = "number")` given a column whose digits
      exceed R's integer range raises no warning outside this package's
      `hitop_*` classes, and still names that column in its
      `hitop_unmatched_items` report; one test asserts both.
- [ ] AC5: `label_pid5()`, `label_hitopsr()` and `label_hitopbr()` each report a
      `<prefix>000` column and a past-integer-range column as out of range: a
      test per helper asserts `hitop_unpadded_items` is raised, and asserts
      `unpadded_item_cols()` places both columns in its `out_of_range` group.
- [ ] AC6: The `@return` section of each of the five family functions names
      every condition class that function raises; `NEWS.md` carries a bullet
      for the two new classes and the sibling's adopted one; `DECISIONS.md`
      carries the entry naming them and superseding D-057's completeness
      clause.
- [ ] AC7: `Rscript -e 'devtools::document()'` leaves no diff and
      `Rscript -e 'devtools::test()'` is clean (the profile's `verify` slot).

## Coverage
<!-- owner: plan · create/amend-via-gate -->

- AC1 → T1, T5
- AC2 → T2, T5, T6
- AC3 → T3, T5
- AC4 → T4, T5
- AC5 → T5
- AC6 → T7
- AC7 → T7

## Tasks
<!-- owner: plan (create) / implement (check-off, minor edits) -->

- [x] T1: In `R/rename_hitopsr_items.R:89-101`, replace the inline
      unmatched-item-text warning with `warn_unmatched_items(item_text[missing_idx],
      "item text")` (`R/util.R:675`); check the helper's brace escaping and
      pluralization against what the inline block produced.
- [x] T2: Add `class = "hitop_no_columns_matched"` at the eight nothing-matched
      sites: `rename_pid5_items.R:106`, `rename_hitopsr_items.R:51`,
      `label_pid5.R:86` and `:128`, `label_hitopsr.R:53` and `:78`,
      `label_hitopbr.R:54` and `:78`.
- [x] T3: Add `class = "hitop_incomplete_rename"` at the two completeness sites:
      `rename_pid5_items.R:164`, `rename_hitopsr_items.R:112`.
- [x] T4: Route `rename_pid5_items()`'s number matching through
      `item_col_numbers()` (`R/util.R:592`), or wrap its `as.integer()`
      (`rename_pid5_items.R:114`) the way that helper does, so no base-R
      coercion warning escapes; the past-range column still reports unmatched.
- [x] T5: Tests, one per path, asserting class not prose: the eight
      nothing-matched paths, the two completeness paths, the HiTOP text report;
      the past-integer-range rename path (written and seen red before T4, both
      on the stray warning and on the column still being named); and per
      `label_*()` helper the `<prefix>000` and past-range out-of-range
      classification. Convert the prose assertions this duplicates
      (`test-rename_hitopsr_items.R:50` and `:102`, the `"No columns matched"`
      matches in `test-label_pid5.R` and `test-label_scales.R`).
- [x] T6: Guard test scanning the five family files for `cli::cli_warn(` call
      sites and failing on any passing no `class =`; assert the scan's domain
      is non-empty, and plant a classless call in both call shapes present
      (single-line and multi-line) to see it red.
- [x] T7: `@return` for the five functions names each class raised; `NEWS.md`
      bullet; append the `DECISIONS.md` entry; `devtools::document()`, then
      `devtools::test()` and `devtools::check()`.

## Work log
<!-- owner: any skill · append-only; one line per entry; absolute dates. -->

- 2026-09-02: created by /milestone-plan.
- 2026-09-02: criteria audit ran in full mode ([O] fresh reader, user-facing tier), returning six findings: a source-scan criterion bound two test-harness properties and its grep could not enumerate the base-R warning the probe found (dropped to T6); the unmatched-text criterion asserted message prose the package promises nothing about (narrowed to class); the edge-path criterion bundled an instrument clause and an internal-helper lock (split into AC4/AC5, clauses moved to T5); "every function raising a new class" quantified over an unenumerated domain (replaced by the five named functions in AC6); the completeness class contradicted D-057 (posed at the gate, signed off); AC2's eight paths passed all six questions.
- 2026-09-02: plan gate chose classing all eight nothing-matched reports family-wide over classing only the two rename helpers, because the label helpers' tests would otherwise keep asserting prose the package promises nothing about; falsified by a caller needing to tell a rename's report from a label's, which one shared class cannot do.
- 2026-09-02: plan gate chose one shared class name `hitop_no_columns_matched` over per-helper names, because the condition is the same event at every site and D-034(c) names conditions rather than functions; falsified by the same evidence class as above.
- 2026-09-02: plan chose adding `class =` at each call site over a shared `warn_no_columns_matched()` helper, because the eight messages differ by prefix, by items-vs-scales and by legacy-name wording, so a helper would carry the differences as arguments and centralize nothing; falsified by the messages converging to one wording.
- 2026-09-02: plan gate chose folding the stray base-R coercion warning into this milestone over a separate hotfix, because it sits in the code T2/T4 already rewrite and two review cycles over one file buy nothing; falsified by a user reporting the stray warning before this milestone merges.
- 2026-09-02: T1 done — `rename_hitopsr_items(method = "text")` raises its unmatched item-text report through `warn_unmatched_items()`; the helper's header and pluralization reproduce the inline block's wording, and its brace escaping now leaves a literal `{x}` in an item text intact. Suite clean (17186 pass, 0 fail).
- 2026-09-02: T2 done — all eight nothing-matched reports raise `hitop_no_columns_matched`, at the two rename sites and at both targets of each `label_*()` helper. Suite clean (17186 pass, 0 fail).
- 2026-09-02: T3 done — both completeness reports raise `hitop_incomplete_rename`. Suite clean (17186 pass, 0 fail).
- 2026-09-02: T4 done — `rename_pid5_items(method = "number")` reads its item numbers through `item_col_numbers()`. The escaping warning was first identified as a base-R `simpleWarning`, "NAs introduced by coercion to integer range", raised alongside the two `hitop_*` reports; the AC4 test was written and seen red on it before the fix.
- 2026-09-02: T5 done — new `tests/testthat/test-warning-classes.R` pins the eight nothing-matched paths, both completeness paths, the HiTOP item-text report, the past-integer-range rename path and the two out-of-range shapes per `label_*()` helper, all by class; it builds on the existing `collect_warnings()`/`frame_of_cols()` helpers. Ten prose assertions converted to class assertions across `test-rename_hitopsr_items.R`, `test-label_pid5.R` and `test-label_scales.R`.
- 2026-09-02: T6 done — the guard parses the five family files rather than grepping them, so a multi-line call is read whole and `class =` is a named argument; it pins the domain at the 10 inline sites (11 less the one T1 moved into `warn_unmatched_items()`) and is seen red against planted classless calls in both shapes. A silence control asserts a fully matching `label_hitopbr()` call raises nothing and that an unraised class is not reported.
- 2026-09-02: T4, T5 and T6 share one commit and one suite run — the tests T5 owns are the evidence T4's fix is checked by, and the guard T6 owns lives in the file T5 creates. Suite clean (17217 pass, 0 fail).
- 2026-09-02: T7 done — the `@return` of each of the five functions names every class it raises; the "under either method" clause in both rename helpers was checked by running each helper's text method and reading back `hitop_incomplete_rename`. `NEWS.md` gained a bullet under Improvements and fixes. `DECISIONS.md` already carried D-059, appended at plan time. `devtools::document()` regenerated the five Rd files; `R CMD check` 0 errors, 0 warnings, 0 notes (14m 44s), its test stage OK.


## Decisions
<!-- owner: implement / review · append-only; milestone-local -->

## Review
<!-- owner: review · exclusive -->
