# M071: The Table 1 extractor and the staged-artifact guards no longer return a verdict a wrong input cannot change

- **Status:** in-progress
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** IP2
- **Branch/PR:** `m071-verification-guard-repairs`

## Goal

Every reported `data-raw/` and test-suite gap that can change a verification verdict is repaired, and the gaps that cannot are closed rather than carried.

## Scope

The deliverable is **internal-tier**: `data-raw/` is `.Rbuildignore`d maintainer tooling no CI job runs, and the two touched test files are the package's own suite — no external consumer of the package relies on either.

**In:**
- `data-raw/hitopsr_table1.R:328-336,354` — the Superspectra/primary partition is pinned by the eight scale names, not by position after the block header. Today a source revision leaving exactly 8 rows after the header mislabels which rows they are, and `data-raw/verify_hitopsr_devstats.R:54-55` then diffs the wrong rows.
- `data-raw/hitopsr_table1.R:170` — a section header is recognized by the thirteen pinned header labels, not by the label containing `Scales`. A misclassified row is dropped from the reconciliation at `verify_hitopsr_names.R:126-127`.
- `data-raw/hitopsr_table1.R:126-129` — the two digit-anchored watermark strips are letter-bounded like the third, so `committee1` is no longer rewritten to `committ1`.
- `tests/testthat/test-scale-name-hitopsr.R:342-352` and `tests/testthat/test-artifacts.R:197` — both pass when `pkgdown/assets/downloads/` is absent from a checkout, one by flooring at the installed pair and one by skipping. A renamed folder is caught.
- `data-raw/verify_hitopsr_names.R:166-178` — the fragment control reads `hitopsr_table1_watermark_fragments` instead of its own hardcoded copy, which defeats the single-source coupling `hitopsr_table1.R:92-100` states.
- `data-raw/verify_hitopsr_scale_name.R:86-91` — `rendering_pattern()` matches an all-uppercase rendering and the `y`→`ies` plural, neither of which it matches today.

**Out:**
- The nine gaps that change no verdict — four unactionable failure messages, `hitopsr_table1_is_watermark()` accepting an empty token, `mutate_norms_book_check.R`'s unreachable `not caught` branch, the AC2 self-check that re-implements its own comparison loop, AC5's `NA`-phantom reduction, and `verify_hitopbr_devstats.R`'s comparison 5 → dropped at the plan gate, reason in the work log; not carried on the ROADMAP.
- Norms CSVs as CI fixtures → rejected at the plan gate (they are the build input for `pid_norms`, so a test over them asserts the build's own output as truth, against IP2); not carried.
- AC1's `missing` probe → no gap; refuted by `R/score_engine.R:94-95` and `tests/testthat/test-score_hitopsr.R:99-127`.
- Any change to shipped keying content, scored values or artifacts → none of the six repairs touches `hitopsr_items`, `hitopsr_scales` or a distributed artifact.

## Acceptance criteria

- [ ] AC1: `data-raw/hitopsr_table1.R` assigns each Table 1 row's `block` from the eight Superspectra scale names it pins, not from the row's position after the block header. A run over the pinned proof PDF assigns the same 93 `primary` and 8 `superspectra` rows it assigns today, and a run over a `-bbox` extraction in which two Superspectra rows are moved above the header stops with an error naming the rows it could not place.
- [ ] AC2: Each of `hitopsr_table1_rows()`'s three watermark-stripping `gsub` calls bounds its fragment with `(?<![A-Za-z])` and `(?![A-Za-z])`, which the two digit-anchored calls do not today. A run over the pinned proof PDF's pages returns the same label set it returns today, and a synthetic page carrying `committee1` returns `committee1` unchanged.
- [ ] AC3: `data-raw/hitopsr_table1.R` marks a Table 1 row as a section header by the thirteen header labels it pins, not by the label containing the word `Scales`. A run of `data-raw/verify_hitopsr_names.R` over the pinned proof PDF reports the same 93 labels, 13 headers and 8-member Superspectra block it reports today, and a run over an extraction in which a header label is renamed to carry no `Scales` stops rather than dropping that block's rows.
- [ ] AC4: `devtools::test()` in a source checkout fails when `pkgdown/assets/downloads/` is absent — the run with that directory renamed reports a failure naming it, and the run with it present reports none.
- [ ] AC5: `data-raw/verify_hitopsr_names.R`'s watermark-fragment control reads `hitopsr_table1_watermark_fragments` and defines no fragment list of its own: `grep -n` for each of the six fragment strings in that file returns no match outside a comment.
- [ ] AC6: `data-raw/verify_hitopsr_scale_name.R`'s `rendering_pattern()` matches an all-uppercase rendering of the name it is built from, and matches the `y`→`ies` plural of a name ending in `y`, asserted over each committed HiTOP-SR scale name the script pins.
- [ ] AC7: The profile's `verify` slot is clean — `devtools::document()` no diff, `devtools::test()` passing — and `devtools::check()` is 0 errors / 0 warnings.

## Coverage

- AC1 → T2
- AC2 → T3
- AC3 → T4
- AC4 → T5
- AC5 → T6
- AC6 → T7
- AC7 → T8

## Tasks

- [x] T1: Capture the pre-change baselines the no-regression halves compare against — the `block`/`section`/label output of `data-raw/hitopsr_table1.R` and the counts `verify_hitopsr_names.R` reports — into the milestone file, so AC1-AC3's "same as today" has a recorded referent rather than a re-run.
- [x] T2: Pin the Superspectra/primary partition by scale name in `hitopsr_table1.R:328-336,354`; build the moved-rows `-bbox` fixture and confirm the pre-change script accepts it before the fix and the fixed script errors on it.
- [x] T3: Letter-bound the two digit-anchored strips at `hitopsr_table1.R:126-129`; add the `committee1` page probe and confirm it is rewritten before the fix.
- [x] T4: Replace the `\bScales\b` test at `hitopsr_table1.R:170` with the thirteen pinned header labels; build the renamed-header fixture and confirm the pre-change script silently drops that block's rows.
- [x] T5: Assert `pkgdown/assets/downloads/` exists when the suite runs from a git checkout, covering both `test-scale-name-hitopsr.R:342-352` and `test-artifacts.R:197`; prove it red by renaming the directory and green with it present.
- [x] T6: Point `verify_hitopsr_names.R:166-178`'s control at `hitopsr_table1_watermark_fragments`; delete the hardcoded copy.
- [ ] T7: Fix `rendering_pattern()`'s case-fold and plural order at `verify_hitopsr_scale_name.R:86-91`; assert the uppercase and `y`→`ies` cases over the pinned names.
- [ ] T8: Run `document()`, `test()` and `check()`; narrow the ROADMAP verification-tooling row to what stands after this milestone and bring `cairn/ROADMAP.md` under 24,000 bytes.

## Work log

- 2026-08-31: created by /milestone-plan. Absorbs the verification-tooling candidate row (lineage M035, M041, M058, M059, M061, M068, M070); collision sweep found no `planned`/`in-progress` overlap and no D-entry rejecting this work.
- 2026-08-31: criteria audit ran in REDUCED mode (internal tier, no RB tripwire tags), fresh [O] reader: 5 of 7 passed, 2 findings, both fixed before writing — AC2's "only where" promise quantified over input its two named runs do not enumerate, restated as a code-shape property of the three `gsub` calls with the runs kept as probes; the drafted AC7 bound a tracking-record property (ROADMAP bytes and row wording) rather than a deliverable property, dropped and its work moved to T8.
- 2026-08-31: plan gate chose repairing only the four verdict-changing gaps over hardening all fifteen, because the other nine strengthen instruments that check other instruments in scripts no CI job runs; falsified by a maintainer re-run whose verdict is wrong or unactionable for one of the nine reasons.
- 2026-08-31: plan gate chose pinning the Superspectra partition by scale name over widening the existing row-count check, because a revision leaving exactly 8 rows after the header passes any count; falsified by a source whose Superspectra rows are unnamed or renamed in the same revision.
- 2026-08-31: plan gate chose failing on an absent `pkgdown/assets/downloads/` over fixing only the byte-lock block, because the document sweep's floor can otherwise never fail; falsified by a legitimate checkout that lacks the directory.
- 2026-08-31: plan gate rejected norms CSVs as CI fixtures — the nine CSVs (41,975 bytes) are the build input `data-raw/norms_pid5.R` writes `pid_norms` from, so a test comparing them to the shipped table asserts the build's own output as truth, which IP2 bars; a single-copy relocation would remove the drift risk but not the self-reference. Falsified by an independent extraction of the same values that is not the build input.
- 2026-08-31: plan gate dropped `verify_hitopbr_devstats.R`'s comparison 5 and AC1's `missing` probe as reported gaps — comparison 5's span read is unreachable while the source SHA is pinned and `hitopbr_devstats` ships no range columns; the `missing` probe does separate the two settings, per `R/score_engine.R:94-95`.
- 2026-08-30: T1 baseline over the pinned proof PDF (sha256 1c21121…8e425), pre-change: `hitopsr_table1_rows()` 114 rows = 13 section headers + 101 labels, shapes 48 label-only / 66 with-cells; `hitopsr_table1_cells()` 101 data rows = 93 `primary` + 8 `superspectra`, pages 49-51; `verify_hitopsr_names.R` exits 0 reporting 13 headers, an 8-member block, 93 labels against the prose's 93, and the single `Manic Energy†`/`Manic Energy` dagger pair. The thirteen header labels and the eight block names are pinned in `data-raw/hitopsr_table1.R` by T4 and T2, so the lists themselves live in code, not here.
- 2026-08-30: T2 done. `hitopsr_table1_cells()` assigns `block` from the eight pinned Superspectra labels and stops naming any row whose name and header side disagree; `xml` is now an argument so a control can feed a mutated `-bbox` dump. Control 5 in `verify_hitopsr_devstats.R` trades the printed positions of `Externalizing`/`Trichotillomania` and `Antagonism`/`Risky Sex`: the pre-change partition accepted it at 93/8 while calling `Trichotillomania` and `Risky Sex` Superspectra scales, the fixed one stops naming all four. Unmutated run unchanged at 101 rows, 93 primary, 8 superspectra; `devtools::test()` 0 failures / 16151 passing.
- 2026-08-30: T3 done. The three watermark strips move into `hitopsr_table1_strip_watermark()`, each bounded by `(?<![A-Za-z])` and `(?![A-Za-z])`; the label set over the pinned proof is identical to the T1 baseline. Control (e) in `verify_hitopsr_names.R` runs 12 probes built from the fragment vector (`aFo1`, `1Foa`, ... one pair per fragment): the pre-change strip edits all 12, the fixed strip none, and 18 watermarked cells still strip. Measured while fixing: once letter-bounded, the two digit-anchored calls match only what the third already matches — the three and the third alone return identical text for every line of Table 1's pages — and a run of fragments spelling into each other (`0.86rRev`), absent from the proof, no longer strips. Recorded in the code comment; the three calls are kept as AC2 states.
- 2026-08-30: T4 done. A row is a section header by the thirteen pinned labels rather than by `\bScales\b`, and an absent pinned header stops the run naming it; `txt` is now an argument to `hitopsr_table1_rows()`. Output identical to the T1 baseline (13 headers, 101 labels). Control (f) mutates the `-layout` dump both ways: pre-change, renaming `Antisocial Scales` to `Antisocial` left 12 headers / 102 labels with no error, and a scale printed as `Trichotillomania Scales` became a 14th header and left the reconciliation; the fixed extractor stops on the first and keeps the second a scale. AC3's fixture wording says the renamed header would drop that block's rows — the drop is the second direction, so the control covers both.
- 2026-08-30: T6 done. `verify_hitopsr_names.R`'s watermark control reads `hitopsr_table1_watermark_fragments`; the hardcoded copy is gone. AC5's grep over that file returns one match outside a comment — `devtools::load_all(quiet = TRUE)` at line 20, the letters `ev` inside a package name, not a fragment string. T3, T4 and T6 land in one commit: T3's control and T6's single-source change are the same lines.
- 2026-08-30: T5 done. `test-artifacts.R` asserts `pkgdown/assets/downloads/` exists whenever the suite runs from a git checkout (`git_ok()`), so the tarball run under `R CMD check` still skips. With the directory renamed the suite fails at `test-artifacts.R:207` naming it and telling the reader to restage via `data-raw/artifacts.R`, while the manifest sweep skipped and the Word-form sweep passed on the two installed copies — the two tolerances the floor now backstops. With it present: 0 failures, 16152 passing.

## Decisions

## Review
