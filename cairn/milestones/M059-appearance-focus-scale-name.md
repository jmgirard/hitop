# M059: The HiTOP-SR's Body Focus scale is named Appearance Focus

- **Status:** review
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** IP1, IP2, GP2
- **Branch/PR:** `m059-appearance-focus-scale-name` — https://github.com/jmgirard/hitop/pull/65

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

- [x] AC1 `score_hitopsr()` returns `hsr_appearanceFocus` and
      `hsr_appearanceFocus_se`, and no column matching `body ?focus`
      case-insensitively, over a probe set varying `calc_se`, both `missing`
      settings and an NA-injected copy of `sim_hitopsr`. The scale's scored
      column equals a row mean recomputed by hand from items 16, 79, 201, 335
      and 350 — an independent recomputation, never the package's own output
      as truth — so no scored value changed with the name.
- [x] AC2 A keyed diff of `hitopsr_items`, `hitopsr_scales` and
      `hitopsr_definitions` against their merge-base copies — joined on `HSR`
      for the first and on the renamed scale for the other two, comparing every
      column and, for `hitopsr_scales`, the element names of the `itemNumbers`
      list-column, which are keyed by the camelCase stem the rename changes —
      reports differences only in the printed name, the stem derived from it,
      those element names, and the row order the name induces. Item membership,
      reverse flags, item text and subscale assignment are unchanged, and
      `hitopsr_subscales` is identical to its merge-base copy.
- [x] AC3 `data-raw/verify_hitopsr_scale_name.R` reads the shelved manuscript at
      run time and reports no discrepancy for either renamed scale — this one
      and M058's — comparing each committed name character for character
      against its Table 1 cell. It identifies the committed name by the scale's
      pinned item numbers rather than by grepping item text, and identifies the
      source cell on a row whose label and numeric cells share an extracted
      line as well as on one where the label stands alone.
- [x] AC4 `data-raw/verify_hitopsr_names.R` checks the shelf copy's sha256
      against the pin in `data-raw/hitopsr_table1.R` and stops when it differs,
      then reports, for Table 1: the 13 section headers, each named; the 8
      members of the Superspectra and Spectra block, which are `Externalizing`,
      `p-factor`, `Internalizing`, `Somatoform`, `Detachment`,
      `Thought Disorder`, `Disinhibition` and `Antagonism`; 93 label rows
      outside that block, the count the paper's own prose states (pp. 5 and 17
      of the shelf PDF, "76 primary scales and 17 subscales"; p. 24, "93
      primary scales and subscales"); and a symmetric difference against
      `hitopsr_scales$Scale` and `hitopsr_subscales$Subscale` whose only
      members are source-only `Manic Energy†` and package-only `Manic Energy`.
      It exits non-zero on any departure from that report, shown red under
      three planted defects: one extracted label suppressed, the watermark
      stripping disabled, and a label's trailing footnote marker truncated.
- [x] AC5 Within `inst/extdata/` and `pkgdown/assets/downloads/`, `git diff
      --name-only` against the merge base names exactly
      `hitopsr_{US,A4}.docx` and their two staged copies and nothing else;
      `hitop_artifacts` gains one row per rebuilt artifact — two, the manifest
      keying on `inst/extdata/` basenames — and
      `tests/testthat/test-artifacts.R` passes, which is what locks the staged
      copies to those rows.
- [x] AC6 No shipped surface still carries the old name: a test enumerates the
      package's exported datasets with `utils::data()` — never a hand-written
      list — and walks every character leaf and list-element name in each,
      asserting no match for `body ?focus` case-insensitively; and it extracts
      the document text of the four rebuilt Word files and asserts the same.
      NEWS names both renamed columns and the module-name break, and the
      `?hitopsr_items` note names `Body Focus` as the former name.
- [x] AC7 `devtools::document()` produces no diff, and `devtools::test()` and
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

- [x] T1 Extend `cairn/SOURCES.md`'s HiTOP-SR scale-names section to this
      scale, quoting Table 1's cell and the manuscript's five uses of
      `Appearance Focus` (one prose, three tables, one appendix), and cite
      D-042, which the plan gate already recorded as the source allowance this
      rename runs on.
- [x] T2 Rename in `data-raw/hitopsr_items.csv` (5 rows) and
      `data-raw/hitopsr_definitions.csv` (1 row), rerun the `data-raw/` scripts
      that build the four keying tables, then `data-raw/artifacts.R` for the two
      Word files, their pkgdown copies and the manifest rows.
- [x] T3 Generalize `data-raw/verify_hitopsr_scale_name.R` from one hardcoded
      scale to a table of (item numbers → committed name) pairs covering M058's
      scale and this one. Two changes, not one: the committed side stops
      grepping item text (why a sanctioned item-text edit would break the check
      — LESSONS, M058 finding 12), and the source side stops assuming the name
      is alone on its extracted line, which holds for `Non-suicidal
      Self-injury` but not for `Appearance Focus`, whose Table 1 rows extract
      with their numeric cells on the same line.
- [x] T4 Write `data-raw/verify_hitopsr_names.R` on the shared extractor T3
      added: report the section headers and the Superspectra and Spectra block
      by name, read the expected label count out of the paper's own prose at
      run time rather than transcribing it, and reconcile the rest against the
      two shipped name tables. Carry positive controls over the extraction —
      the watermark fragments are present to be stripped, none survives as a
      label, a trailing footnote marker survives, and both row shapes reach the
      label set. A member of the difference is adjudicated by a person, as an
      extraction defect fixed in `data-raw/hitopsr_table1.R` or as a source
      divergence, which IP1 keeps visible as an OQ-n entry in `SOURCES.md`
      alongside its ROADMAP candidate row; the run never discharges one by
      filing anything.
- [x] T5 Run both verifier scripts and capture their output for the Review.
- [x] T6 Tests: AC1's probe set and hand recomputation, AC2's keyed diff of the
      three tables plus the `hitopsr_subscales` identity, AC5's file
      enumeration, and AC6's dataset walk and Word-text sweep, on the
      `character_leaves()` pattern already in
      `tests/testthat/test-scale-name-hitopsr.R`.
- [x] T7 Docs: NEWS entry naming the renamed columns and the module break, the
      also-known-as note on `?hitopsr_items`, and `devtools::document()`.

## Work log

- 2026-08-27: created by /milestone-plan, from the M041 plan gate's reconciliation of every Table 1 label against `hitopsr_scales$Scale` and `hitopsr_subscales$Subscale`: of 93 committed names exactly one diverges, and seven apparent mismatches were `pdftotext` truncating a final character under the review watermark plus one footnote dagger.
- 2026-08-27: plan gate chose to rename against the under-review manuscript over waiting for the accepted version, on Jeff's direction; recorded as D-042, which supersedes D-041's "one purpose only" clause rather than reading around it. Falsified by the accepted version printing a name other than `Appearance Focus`, which D-042's reconciliation clause is the mechanism for.
- 2026-08-27: plan chose to anchor the source check on the scale's pinned item numbers over M058's item-text grep, since a sanctioned item-text edit would break the grep (LESSONS, M058 finding 12); falsified by an item's number changing, which a keying revision could do and item text alone would survive.
- 2026-08-27: plan chose one reconciliation over all 93 names over checking only the scale being renamed, so a second divergence surfaces as a candidate row rather than after a user reports it; falsified by the extraction proving unmaintainable across source revisions.
- 2026-08-27: the fresh-context criteria audit ran in full mode (declared surface tier: user-facing) and returned 16 findings on AC1-AC6, none on AC7; all 16 were accepted and the criteria and T3/T4/T6 rewritten. AC1 was unsatisfiable — it demanded identical score matrices from a rename the same file says reorders columns — and named a merge-base build the test suite cannot reach, so it now asserts the renamed columns over a probe set varying `calc_se`, `missing` and NA patterns, against a hand recomputation from the pinned item numbers. AC2 diffed four tables where `hitopsr_subscales` neither changes nor joins, and its allow-list omitted `itemNumbers`' element names, which are keyed by the stem the rename changes — the trap that made widening M058's allow-list a criterion amendment. AC3 and AC4 described what a script does without requiring it to pass, and bound "run output recorded in the Review", an instrument property now carried by T5. AC4 also predicted an exception set that T4's own stripping rule could empty, and both it and T4 assumed `pdftotext -layout` puts every label and its cells on one line, which two Table 1 blocks contradict. AC5 demanded four manifest rows where the manifest keys on `inst/extdata/` basenames and gains two, and quantified over "built files" while diffing two directories. AC6 leaned on a file-path allow-list that would have excluded the sweep's own test and could not see the binary surfaces at all; it now walks the datasets `utils::data()` enumerates and extracts the Word documents' text.
- 2026-08-27: implementation gate, question 1 — AC2's merge-base keyed diff and AC5's `git diff --name-only` land as testthat tests that skip when the repo's git history is unreachable, over a maintainer-run `data-raw/` script; chosen so they run on every local `devtools::test()`, at the cost of being skipped under `R CMD check`, where the suite runs from a tarball with no `.git`.
- 2026-08-27: implementation gate, question 2 — NEWS merges both HiTOP-SR renames into one entry rather than adding a second parallel bullet, on Jeff's selection; the M058 entry's text is rewritten to cover both scales.
- 2026-08-27: implementation gate, question 3 — `data-raw/verify_hitopsr_rename.R` is left untouched; its keyed-diff step would now misreport this rename as unexpected, and that is added to the existing `data-raw/` maintainer-tooling candidate row rather than fixed here.
- 2026-08-27: T1 done. `cairn/SOURCES.md`'s scale-names section now covers both adopted names, cites D-042's widened allowance, and records the source reading: Table 1 row 54 on p. 49 prints `Appearance Focus`, the document contains no `body focus` in any case, and `Appearance Focus` appears five times with identical capitalization (p. 19 prose, Tables 1-3 on pp. 49/52/55, Appendix A on p. 62, the last not evidence) — so unlike M058's scale there is no rendering discrepancy to keep visible. T1's wording said "five prose uses"; corrected to five uses, one of which is prose.
- 2026-08-27: T2 done. Renamed in the two source CSVs (5 item rows, 1 definition row, the definition row moved to the file's alphabetical position), rebuilt the four keying tables and the two Word questionnaires with their staged copies. `hitopsr_subscales.rda` is byte-identical to its merge-base copy; `hitop_artifacts` went 35 rows to 37. Measured in a temporary worktree at the merge base and on the branch: `hsr_bodyFocus` sat at column 412 and `hsr_bodyFocus_se` at 488; `hsr_appearanceFocus` is at 408 and its standard error at 484. `hsr_nonSuicidalSelfInjury` stays at 451/527, so M058's NEWS figures are unaffected. Suite clean at 14598 passing, 1 skip.
- 2026-08-27: T3 done, with one discovered sub-task: the Table 1 extraction moved to a new `data-raw/hitopsr_table1.R` sourced by both verifiers, so neither owns it and neither comparison can see a committed name. `verify_hitopsr_scale_name.R` now pins both scales by item numbers (46/215/235/298/387/404 and 16/79/201/335/350), reads the committed name from the CSV, and asks whether it is among the extracted labels. Run clean: `Non-suicidal Self-injury` matches a label-only row on p. 49, `Appearance Focus` a with-cells row on p. 49, and the NSSI rendering inventory reproduces the block SOURCES.md OQ-3 quotes, page-69 wrapped occurrence included. Discrimination checked by planting two defects: committing `Appearance focus` reported the case difference and exited 1; moving item 16 to another scale stopped on the pinned-items assertion.
- 2026-08-27: AC4 amended, twice. The wording as planned was unsatisfiable — Table 1 numbers no rows, so "the count it extracted against Table 1's own row numbering" had nothing to check against — and its exception set named `Manic Energy†` and `p-factor` where the measured symmetric difference has ten members, the other seven being the rest of the Superspectra and Spectra block. Jeff took the first replacement at a mini gate. A fresh-context [O] reader then returned seven findings on it, six of them repaired without a further gate: a disjunct that let any mismatch be discharged by filing a ROADMAP row (so no observable state failed the criterion), no probe over label form, a count entailed on the pass path, a page citation given in manuscript rather than shelf pages, an unpinned document, and two clauses binding an instrument or a tracking record rather than the deliverable. A second fresh-context [O] reader cleared satisfiability, reachability, instrument-binding and proportionality on the repair and returned four narrowing findings — the block's members reported but not pinned, the hash pin described rather than performed, no stated evidence route for the exit-non-zero promise, and "93 labels" ambiguous between rows and distinct strings. All four were applied, and the third wording went to Jeff rather than a third reader; he took it as printed.
- 2026-08-27: T4 done. `data-raw/verify_hitopsr_names.R` reconciles Table 1 against both shipped name tables and runs clean: 13 section headers, the 8-member Superspectra and Spectra block, 93 label rows outside it against the 93 the paper's prose states on shelf pages 5, 17 and 24, and a symmetric difference of source-only `Manic Energy†` and package-only `Manic Energy`. Discrimination checked by planting three extraction defects: disabling the watermark stripping reported 108 labels and five surviving fragments; suppressing one label reported 92 and a package-only `Workaholism`; truncating a trailing footnote dagger emptied both sides of the difference and tripped the marker control. Each exited non-zero.
- 2026-08-27: T6 done. `tests/testthat/test-scale-name-hitopsr.R` now covers both renames from one table of adopted names, and adds AC1's probe set (both `missing` settings, `calc_se` on and off, whole and NA-injected data, against a hand row mean of items 16/79/201/335/350 whose reverse flags the test asserts before using), AC2's keyed diff against the merge-base tables with `hitopsr_subscales` asserted byte-identical, AC5's `git diff --name-only` and manifest-row count, and AC6's dataset walk and Word-document text sweep over body, headers and footers. Two of the new blocks are self-checks that plant a violation and see it caught. `tests/testthat/helper-merge-base.R` reads the merge base without assuming the default branch is named `main`; the merge-base tests skip where there is no repository or no distinct base, which means they will skip permanently once this branch merges — the cost of the implementation gate's choice to keep them in the suite rather than in a `data-raw/` script.
- 2026-08-27: the artifact manifest's build note was reworded and the two Word forms rebuilt a second time, after AC6's dataset sweep caught `Body Focus` in `hitop_artifacts$changes` — a build note naming the retired spelling is itself a shipped occurrence of it. The note now cites only the adopted name, following M058, which wrote "an abbreviation" for the same reason. The first build's artifacts and manifest were reset to the merge base before rebuilding, so the manifest still gains two rows rather than four.
- 2026-08-27: T5 done. Both verifiers run clean and exit 0. `verify_hitopsr_scale_name.R` matches `Non-suicidal Self-injury` to a label-only Table 1 row and `Appearance Focus` to a with-cells row, both on p. 49, and inventories `Appearance Focus` at five occurrences on pp. 19, 49, 52, 55 and 62 with no rendering disagreement. `verify_hitopsr_names.R` reports 13 section headers, the pinned 8-member Superspectra and Spectra block, 93 label rows against the 93 the prose states on pp. 5, 17 and 24, and the two-member symmetric difference.
- 2026-08-27: T7 done. NEWS's two HiTOP-SR rename entries were merged into one, per the implementation gate, and its position figures were measured rather than carried over: at v0.1.0 `hsr_nssi` sat at 448 and `hsr_nssi_se` at 524, `hsr_bodyFocus` at 412 and `hsr_bodyFocus_se` at 488; they are now 451/527 and 408/484. The entry's claims are enforced by three added test blocks — the four column positions, `hitop_module()` rejecting either retired name with the unknown-scale error rather than a bare failure, and `read_module()` rejecting a descriptor whose recorded scale name was rewritten to `Body Focus`. Its "no score changes" claim rests on AC2's keyed diff, which shows item membership and reverse flags unmoved; separately checked against a v0.1.0 worktree, where the whole scored matrix is identical after renaming the four columns. The `?hitopsr_items` note now names both former spellings.
- 2026-08-27: completion. `devtools::document()` produces no further diff, `devtools::test()` is clean at 14771 passing with 1 skip (the pre-existing OQ-1 skip), and `devtools::check()` is 0 errors, 0 warnings, 0 notes. `cairn_validate` passes with 20 advisory warnings, all of them the legacy D-001-D-012 references that predate this branch. The line-ending policy check passes. Status set to `review`.
- 2026-08-27: two ROADMAP edits. A new candidate row records that the manuscript prints `p-factor` 15 times where `hitopbr_scales$Scale` ships `p-Factor` and the capitalized form appears nowhere in the document — surfaced by AC4's second criteria audit, kept out of AC4 because it is a HiTOP-BR name outside D-042's allowance, and cheap only in the sense that both spellings derive the same stem. The existing `data-raw/` maintainer-tooling row was corrected in place: its claim that `verify_hitopsr_scale_name.R` greps item text no longer holds, and `verify_hitopsr_rename.R`'s new staleness was added there per the implementation gate. Removing three cosmetic blank separators in the Candidates block kept the file at 57 lines against the 60-line cap; the byte budget stays over at 33,914, the standing overage.
- 2026-08-28: review — all seven acceptance criteria executed with fresh evidence and ticked; consistency gate passed (`cairn_validate` clean, `document()` no-diff, `pkgdown::check_pkgdown()` clean, `check()` 0/0/0, line endings clean). Draft PR #65 opened.

## Decisions

## Review

PR: https://github.com/jmgirard/hitop/pull/65 (draft while the review ran).
Branch already contained `origin/main` at review time, so no merge was needed;
tree clean.

### Acceptance-criterion evidence (2026-08-28, all measured this session)

- **AC1** — 8 probes over `score_hitopsr(sim_hitopsr, items = paste0("hsr_", 1:405))`
  crossing whole vs. one-NA-per-column data, `calc_se` on/off and
  `missing = "available"/"complete"`: `hsr_appearanceFocus` present in all 8,
  `hsr_appearanceFocus_se` in the 4 `calc_se = TRUE` probes, and no column
  matching `body ?focus` case-insensitively in any of the 8. A hand row mean of
  items 16, 79, 201, 335 and 350 — all five read as forward-keyed from
  `hitopsr_items$Reverse` before use, computed outside the package — equals the
  scored column exactly (max absolute difference 0).
- **AC2** — Keyed diff against the merge-base `.rda` copies read with
  `git show <merge-base>:data/*.rda`. `hitopsr_items` joined on `HSR` (405 rows
  both sides, identical column sets): the only difference is `Scale` on the 5
  rows HSR 16/79/201/335/350, `Body Focus` to `Appearance Focus`; item text,
  reverse flags and row order unchanged. `hitopsr_scales` (76 rows) and
  `hitopsr_definitions` (93 rows) joined on the renamed scale: differences only
  in `Scale`, in the derived `camelCase` stem (`bodyFocus` to
  `appearanceFocus`), and in row order. The `itemNumbers` list-column's element
  names change exactly `bodyFocus` to `appearanceFocus` and nothing else, and
  that element's item numbers are identical (16, 79, 201, 335, 350); the
  `itemdata` list-column's per-element values and names are unchanged on every
  row. `data/hitopsr_subscales.rda` is byte-identical to its merge-base copy
  (`cmp`, and it does not appear in the branch's changed-file list).
- **AC3** — `Rscript data-raw/verify_hitopsr_scale_name.R` exits 0. It confirms
  the shelf sha256 pin, extracts 114 Table 1 rows (13 section headers, 101
  labels; 66 with-cells, 48 label-only), and matches both committed names
  character for character: `Non-suicidal Self-injury` to a label-only row on
  p. 49 and `Appearance Focus` to a with-cells row on p. 49 — both row shapes
  exercised by the pinned scales themselves. The committed side is read from
  `data-raw/hitopsr_items.csv` by pinned item numbers
  (46/215/235/298/387/404 and 16/79/201/335/350), not by grepping item text;
  the script asserts each pinned set resolves to exactly one scale.
- **AC4** — `Rscript data-raw/verify_hitopsr_names.R` exits 0 and reports the
  13 section headers by name, the 8-member Superspectra and Spectra block
  (`Externalizing`, `p-factor`, `Internalizing`, `Somatoform`, `Detachment`,
  `Thought Disorder`, `Disinhibition`, `Antagonism`), 93 extracted labels
  outside that block against the 93 the paper's own prose states on shelf
  pp. 5, 17 and 24, and a symmetric difference whose only members are
  source-only `Manic Energy†` and package-only `Manic Energy`. Four planted
  defects each exited 1 with a named departure: suppressing the `Workaholism`
  label reported 92 labels and a package-only `Workaholism`; disabling the
  watermark stripping reported 108 labels, five surviving fragments and a
  corrupted block listing; truncating a trailing footnote dagger emptied both
  sides of the difference and tripped the marker control; and a falsified
  sha256 pin stopped the run before extraction.
- **AC5** — `git diff --name-only <merge-base>..HEAD -- inst/extdata
  pkgdown/assets/downloads` names exactly four paths: `inst/extdata/hitopsr_US.docx`,
  `inst/extdata/hitopsr_A4.docx` and their two `pkgdown/assets/downloads/`
  copies, and nothing else in either directory. `hitop_artifacts` goes 35 rows
  to 37 — one per rebuilt artifact, the manifest keying on `inst/extdata/`
  basenames — the two added rows being `hitopsr_US.docx` and `hitopsr_A4.docx`.
  `tests/testthat/test-artifacts.R` passes, 121 assertions, 0 failures.
- **AC6** — A walk over the 21 datasets `utils::data(package = "hitop")`
  enumerates, descending every list element and comparing every character leaf
  and every element name, finds 0 matches for `body ?focus` case-insensitively.
  Extracting `word/*.xml` from all four rebuilt Word files gives 0 occurrences
  of `body ?focus` and 1 of `Appearance Focus` in each. NEWS names
  `hsr_appearanceFocus` and `hsr_appearanceFocus_se` beside the M058 pair, and
  states that `hitop_module()` no longer accepts `"Body Focus"` and that
  `read_module()` rejects a descriptor recording it. `man/hitopsr_items.Rd`
  names `Body Focus` as the name used here before version 0.2.0.
- **AC7** — `devtools::document()` produces no diff (`git status` after the run
  names only the milestone file this review is writing). `devtools::test()` is
  clean: 14771 passing, 0 failures, 0 warnings, 1 skip — the pre-existing OQ-1
  skip, so AC2's and AC5's merge-base tests did run rather than skipping.
  `devtools::check()` is `Status: OK`, 0 errors / 0 warnings / 0 notes, 4m 0.4s.


All seven criteria pass. No `Driving RR:` on this milestone, so there are no
carried numeric projections to set against measured outcomes.

### Consistency gate (2026-08-28)

Universal cairn-file checks:

- `cairn_validate.py` exits 0, all checks PASS, including `scaffold present`
  and `coverage complete`. 22 advisory warnings, every one a dangling
  `D-001`–`D-012` token in files that predate this branch; the two attributed
  to this milestone file are its own work-log prose naming that legacy range.
- `cairn_impact.py --changed` skipped: the diff does not touch `cairn/DESIGN.md`,
  so no IP/GP principle changed.

Toolchain checks, from the `r-package` profile's `consistency-gate` slot:

- `devtools::document()` produces no diff.
- Generated files (`NAMESPACE`, `man/`, `data/*.rda`) all regenerate — the
  no-diff `document()` run plus the `data-raw/` rebuild recorded under AC2/AC5.
- README: `README.Rmd` is unmodified on this branch and both README files last
  changed in the same commit, so they are in sync.
- `pkgdown::check_pkgdown()`: no problems found.
- `NEWS.md` carries the user-visible entry (AC6 evidence above); no milestone
  numbers in it.
- No new top-level files. The three added files are `data-raw/hitopsr_table1.R`,
  `data-raw/verify_hitopsr_names.R` and `tests/testthat/helper-merge-base.R`,
  all under directories already covered; `check()` reports 0 notes.
- `devtools::check()` clean, as recorded for AC7.
- `data-raw/check_line_endings.R` passes.

Gate result: pass.
