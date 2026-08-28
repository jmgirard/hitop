# M059: The HiTOP-SR's Body Focus scale is named Appearance Focus

- **Status:** planned
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** IP1, IP2, GP2
- **Branch/PR:** —

## Goal

The HiTOP-SR scale this package calls `Body Focus` carries the name the
introduction paper's Table 1 prints, `Appearance Focus`, wherever the package
prints or returns it.

## Scope

Surface tier: **user-facing** — it renames a scored output column and reprints
two distributed questionnaires.

**In:** `Appearance Focus` replaces `Body Focus` in `data-raw/hitopsr_items.csv`
and `data-raw/hitopsr_definitions.csv` and the keying tables rebuilt from them,
which renames the scored columns to `hsr_appearanceFocus`/`_se` and moves them
where the tables sort by name; a rebuild of the two Word questionnaires, their
`pkgdown/assets/downloads/` copies and their `hitop_artifacts` rows; NEWS and an
also-known-as note; a maintainer-run source check anchored on the scale's item
numbers; a maintainer-run reconciliation of every Table 1 label against both
shipped name tables; and a `SOURCES.md` provenance entry citing D-042, the
source allowance this plan's gate recorded.

**Out:** the Qualtrics `.txt` and REDCap `.zip` are not rebuilt — neither prints
a scale name, and the zip's member mtime would churn a D-016 manifest revision
that records nothing (D-041's reasoning, unchanged here). HiTOP-SR development
statistics and their name map → M041, which depends on this milestone. The known
gaps in M058's two verifier scripts → the existing `data-raw/` maintainer-tooling
candidate row. Any further Table 1 divergence the reconciliation turns up → a
`candidate` row, never a rename folded into this milestone.

## Acceptance criteria

- [ ] AC1 `score_hitopsr()` returns `hsr_appearanceFocus` and
      `hsr_appearanceFocus_se`, and no column matching `body ?focus`
      case-insensitively, over a probe set varying `calc_se`, both `missing`
      settings and an NA-injected copy of `sim_hitopsr`. The scale's scored
      column equals a row mean recomputed by hand from items 16, 79, 201, 335
      and 350 — an independent recomputation, never the package's own output
      as truth — so no scored value changed with the name.
- [ ] AC2 A keyed diff of `hitopsr_items`, `hitopsr_scales` and
      `hitopsr_definitions` against their merge-base copies — joined on `HSR`
      for the first and on the renamed scale for the other two, comparing every
      column and, for `hitopsr_scales`, the element names of the `itemNumbers`
      list-column, which are keyed by the camelCase stem the rename changes —
      reports differences only in the printed name, the stem derived from it,
      those element names, and the row order the name induces. Item membership,
      reverse flags, item text and subscale assignment are unchanged, and
      `hitopsr_subscales` is identical to its merge-base copy.
- [ ] AC3 `data-raw/verify_hitopsr_scale_name.R` reads the shelved manuscript at
      run time and reports no discrepancy for either renamed scale — this one
      and M058's — comparing each committed name character for character
      against its Table 1 cell. It identifies the committed name by the scale's
      pinned item numbers rather than by grepping item text, and identifies the
      source cell on a row whose label and numeric cells share an extracted
      line as well as on one where the label stands alone.
- [ ] AC4 `data-raw/verify_hitopsr_names.R` extracts every scale and subscale
      label Table 1 prints, checks the count it extracted against Table 1's own
      row numbering, and reports the symmetric difference against
      `hitopsr_scales$Scale` and `hitopsr_subscales$Subscale`. The run reports
      that difference in full; each member is either `Manic Energy†` (a
      footnote marker on a name that otherwise matches) or `p-factor` (a
      HiTOP-BR scale, outside both tables), or is filed as a `candidate`
      ROADMAP row naming it.
- [ ] AC5 Within `inst/extdata/` and `pkgdown/assets/downloads/`, `git diff
      --name-only` against the merge base names exactly
      `hitopsr_{US,A4}.docx` and their two staged copies and nothing else;
      `hitop_artifacts` gains one row per rebuilt artifact — two, the manifest
      keying on `inst/extdata/` basenames — and
      `tests/testthat/test-artifacts.R` passes, which is what locks the staged
      copies to those rows.
- [ ] AC6 No shipped surface still carries the old name: a test enumerates the
      package's exported datasets with `utils::data()` — never a hand-written
      list — and walks every character leaf and list-element name in each,
      asserting no match for `body ?focus` case-insensitively; and it extracts
      the document text of the four rebuilt Word files and asserts the same.
      NEWS names both renamed columns and the module-name break, and the
      `?hitopsr_items` note names `Body Focus` as the former name.
- [ ] AC7 `devtools::document()` produces no diff, and `devtools::test()` and
      `devtools::check()` are clean.

## Coverage

- AC1 → T2, T6
- AC2 → T2, T6
- AC3 → T3, T5
- AC4 → T4, T5
- AC5 → T2, T6
- AC6 → T6, T7
- AC7 → T6, T7

## Tasks

- [ ] T1 Extend `cairn/SOURCES.md`'s HiTOP-SR scale-names section to this
      scale, quoting Table 1's cell and the manuscript's five prose uses of
      `Appearance Focus`, and cite D-042, which the plan gate already recorded
      as the source allowance this rename runs on.
- [ ] T2 Rename in `data-raw/hitopsr_items.csv` (5 rows) and
      `data-raw/hitopsr_definitions.csv` (1 row), rerun the `data-raw/` scripts
      that build the four keying tables, then `data-raw/artifacts.R` for the two
      Word files, their pkgdown copies and the manifest rows.
- [ ] T3 Generalize `data-raw/verify_hitopsr_scale_name.R` from one hardcoded
      scale to a table of (item numbers → committed name) pairs covering M058's
      scale and this one. Two changes, not one: the committed side stops
      grepping item text (why a sanctioned item-text edit would break the check
      — LESSONS, M058 finding 12), and the source side stops assuming the name
      is alone on its extracted line, which holds for `Non-suicidal
      Self-injury` but not for `Appearance Focus`, whose Table 1 rows extract
      with their numeric cells on the same line.
- [ ] T4 Write `data-raw/verify_hitopsr_names.R`: extract Table 1 with
      `pdftotext -layout`, handling both row shapes — label and numeric cells
      on one line, and label alone with its cells on the next — strip the
      manuscript's line numbers and the section-header rows, keep the
      Superspectra and Spectra block's members so `p-factor` reaches the
      exception set rather than vanishing with its heading, and diff against
      the two shipped name tables.
- [ ] T5 Run both verifier scripts and capture their output for the Review.
- [ ] T6 Tests: AC1's probe set and hand recomputation, AC2's keyed diff of the
      three tables plus the `hitopsr_subscales` identity, AC5's file
      enumeration, and AC6's dataset walk and Word-text sweep, on the
      `character_leaves()` pattern already in
      `tests/testthat/test-scale-name-hitopsr.R`.
- [ ] T7 Docs: NEWS entry naming the renamed columns and the module break, the
      also-known-as note on `?hitopsr_items`, and `devtools::document()`.

## Work log

- 2026-08-27: created by /milestone-plan, from the M041 plan gate's reconciliation of every Table 1 label against `hitopsr_scales$Scale` and `hitopsr_subscales$Subscale`: of 93 committed names exactly one diverges, and seven apparent mismatches were `pdftotext` truncating a final character under the review watermark plus one footnote dagger.
- 2026-08-27: plan gate chose to rename against the under-review manuscript over waiting for the accepted version, on Jeff's direction; recorded as D-042, which supersedes D-041's "one purpose only" clause rather than reading around it. Falsified by the accepted version printing a name other than `Appearance Focus`, which D-042's reconciliation clause is the mechanism for.
- 2026-08-27: plan chose to anchor the source check on the scale's pinned item numbers over M058's item-text grep, since a sanctioned item-text edit would break the grep (LESSONS, M058 finding 12); falsified by an item's number changing, which a keying revision could do and item text alone would survive.
- 2026-08-27: plan chose one reconciliation over all 93 names over checking only the scale being renamed, so a second divergence surfaces as a candidate row rather than after a user reports it; falsified by the extraction proving unmaintainable across source revisions.
- 2026-08-27: the fresh-context criteria audit ran in full mode (declared surface tier: user-facing) and returned 16 findings on AC1-AC6, none on AC7; all 16 were accepted and the criteria and T3/T4/T6 rewritten. AC1 was unsatisfiable — it demanded identical score matrices from a rename the same file says reorders columns — and named a merge-base build the test suite cannot reach, so it now asserts the renamed columns over a probe set varying `calc_se`, `missing` and NA patterns, against a hand recomputation from the pinned item numbers. AC2 diffed four tables where `hitopsr_subscales` neither changes nor joins, and its allow-list omitted `itemNumbers`' element names, which are keyed by the stem the rename changes — the trap that made widening M058's allow-list a criterion amendment. AC3 and AC4 described what a script does without requiring it to pass, and bound "run output recorded in the Review", an instrument property now carried by T5. AC4 also predicted an exception set that T4's own stripping rule could empty, and both it and T4 assumed `pdftotext -layout` puts every label and its cells on one line, which two Table 1 blocks contradict. AC5 demanded four manifest rows where the manifest keys on `inst/extdata/` basenames and gains two, and quantified over "built files" while diffing two directories. AC6 leaned on a file-path allow-list that would have excluded the sweep's own test and could not see the binary surfaces at all; it now walks the datasets `utils::data()` enumerates and extracts the Word documents' text.

## Decisions

## Review
