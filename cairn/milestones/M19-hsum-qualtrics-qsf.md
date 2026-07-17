<!-- Section ownership + write-modes: see the cairn plugin's tracking-rules.md
     "Milestone-file section ownership". -->
# M19: HSUM Qualtrics QSF rebuild (API script + verification test)

- **Status:** review
- **Priority:** normal
- **Depends on:** —
- **Principles touched:** IP1, IP2
- **Branch/PR:** `m19-hsum-qualtrics-qsf` · https://github.com/jmgirard/hitop/pull/21

## Goal

Replace the stale hand-built `inst/extdata/hitophsum_qualtrics.qsf` with one rebuilt from the corrected keying tables via a modernized maintainer API script, and lock the committed artifact to `hitophsum_items`/`hitophsum_choices` with a parse-and-compare test so QSF staleness becomes a test failure.

## Scope

**In:** modernizing `devel/qualtrics_test.R` → `devel/qualtrics_hitophsum.R` (Choice_Set resolution, drop `any_other`, run recipe); Jeff's API run + manual QSF export replacing the committed artifact; the QSF verification test (full structural bar); jsonlite → Suggests (D-015); SOURCES.md/NEWS updates.

**Out:** an exported `generate_qualtrics_hitophsum()` (QSF stays maintainer-generated; the TXT format the exported family uses cannot carry display logic). API-based end-user import functions — declined 2026-07-16 (auth burden vs. GP3/GP4; QSF download stays the distribution contract). The most-frequent other-drug rule in Qualtrics — platform cannot express cross-question comparison declaratively; the QSF encodes the per-drug rule (sheet-sanctioned loosening), documented as a divergence in SOURCES.md.

## Acceptance criteria

- [x] New `tests/testthat/test-qualtrics-hitophsum.R` parses the committed QSF (jsonlite, `skip_if_not_installed`) and passes: exactly one question element per `hitophsum_items` row (+ instructions) with unique `DataExportTag`s (kills the current duplicate-block defect: 1302 SQ for ~650 items)
- [x] QSF content fidelity, machine-checked: item text matches `hitophsum_items$Text` modulo the adaptations documented in SOURCES.md (withdrawal-symptom bolding, piped "other" substance name); every choice-bearing question's values/labels match its `Choice_Set` (incl. the M18 `quant_*` sets)
- [x] QSF logic fidelity, machine-checked: every gated item's display logic is the correct expansion of `Gate_Variable`/`Gate_Value` against `hitophsum_choices` (discrete lists, threshold enumerations, `count>1` via `SelectedChoicesCount`), and no display-logic condition anywhere selects choice 99 (PNTS)
- [x] `devel/qualtrics_hitophsum.R` resolves field types/choices from `Choice_Set` (no variable-name regexes, no `any_other` branch), with a header documenting required local packages (httr2, cli), token/datacenter inputs, and the run→export→commit recipe; jsonlite added to Suggests (D-015)
- [x] SOURCES.md: HSUM verification-table QSF row flips to machine-checked; per-drug-rule platform divergence and text adaptations documented; NEWS.md bullet
- [x] `devtools::document()` no-diff; `devtools::test()` passes; `devtools::check()` clean
- [ ] PR merged with Jeff's approval (regenerated participant-facing artifact: IP1)

## Coverage

- AC1 → T1, T4
- AC2 → T1, T4
- AC3 → T1, T3, T4
- AC4 → T2, T3
- AC5 → T5
- AC6 → T5
- AC7 → T6

## Tasks

- [x] T1 Failing test first: `tests/testthat/test-qualtrics-hitophsum.R` per the full structural bar (AC1–AC3), expectations derived from `hitophsum_items`/`hitophsum_choices`, never from the QSF itself; deliberately red against the stale committed file (documents the defect)
- [x] T2 DESCRIPTION: jsonlite → Suggests; `skip_if_not_installed("jsonlite")` guard in the test file
- [x] T3 Modernize `devel/qualtrics_test.R` → `devel/qualtrics_hitophsum.R`: quantity types/choices from `Choice_Set` (mirror the M18 fix in R/generate_redcap.R); delete the dead `any_other` branch; keep the inherent PNTS exclusion and `SelectedChoicesCount` logic; header = run recipe + local-package note (M18 lesson) + text-adaptation list the test mirrors
- [x] T4 **Jeff (user action):** run the script against a fresh survey with his API token, export the QSF from Qualtrics, replace `inst/extdata/hitophsum_qualtrics.qsf`; T1 test goes green
- [x] T5 SOURCES.md (QSF row machine-checked; divergence + adaptation notes; generator provenance line), NEWS.md bullet; `document()`/`test()`/`check()`
- [x] T6 PR, record URL, merge on Jeff's approval — https://github.com/jmgirard/hitop/pull/21 (merge itself gated at review, AC7)

## Work log

- 2026-07-16: created by /milestone-plan. Absorbs the 2026-07-16 ROADMAP candidate "Rebuild inst/extdata/hitophsum_qualtrics.qsf…" (M18's open remainder). Gate decisions (Jeff): pipeline approach confirmed; jsonlite → Suggests (D-015); full structural test bar.
- 2026-07-16: implementation started; branch cut. Pre-impl question gate skipped — all choices settled at the plan gate this session.
- 2026-07-16: T1 done — test red against the stale file exactly where staleness lives (duplicate tags, 476 stale texts, any_other gate on nic_quant_oth). Discovery: the stale QSF has NULL choices on hsum_nic_quant_cgr — the cigar-quantity defect M18 fixed in REDCap exists in the hand-built Qualtrics survey too (same ancestral regex bug).
- 2026-07-16: T2 done (jsonlite → Suggests). T3 done — script rewritten as devel/qualtrics_hitophsum.R (old qualtrics_test.R removed): Field_Type/Choice_Set resolution with a stop() on unresolved sets, any_other deleted, header carries run recipe + adaptation list; offline dry-run confirms all 650 items resolve type/choices/gates cleanly. Full suite: failures confined to the deliberate-red QSF file. Next: T4 requires Jeff at the Qualtrics console.
- 2026-07-16: T3 amended (minor, Jeff's request): full automation — rebuild_hitophsum_qsf() creates the survey, pushes, exports via ?format=qsf (shape-validated, loud manual fallback), and overwrites the artifact; credentials from ~/.Renviron (never repo-local). T4 shrinks to setting ~/.Renviron once + one function call.
- 2026-07-16: T4 done — Jeff ran rebuild_hitophsum_qsf(); the ?format=qsf export worked. One export quirk reconciled in the test (not content): dense zero-based choice maps (symptom_4pt 0–3) serialize as JSON arrays, keys recovered positionally from ChoiceOrder. QSF suite fully green: 6421 assertions / 0 fail.
- 2026-07-16: T5 done — SOURCES.md QSF row → machine-checked + new "Qualtrics artifact" subsection (derived-artifact provenance, PNTS enumeration, per-drug platform divergence); NEWS bullet added to the existing 0.2.0 HSUM block. document() no diff; test() 7704/0; check() 0/0/0.

## Decisions

## Review

### Acceptance-criteria evidence (2026-07-16, fresh by command)

- AC1 ✅ QSF suite re-run at review: structure block passes (one SQ per hitophsum_items row + instructions; unique tags; 651 elements — the old file's 1302 duplicates gone). File total 6421 pass / 0 fail.
- AC2 ✅ text block 650/650; choices block 638/638 (values + labels per Choice_Set, incl. quant_* sets; array-serialization normalization documented in-test).
- AC3 ✅ logic block 2554/2554 (discrete, threshold-enumeration, SelectedChoicesCount); PNTS sweep 1274/1274 (no condition selects choice 99).
- AC4 ✅ devel/qualtrics_hitophsum.R greps clean of variable-name regexes and any_other; header carries ~/.Renviron setup, run recipe, adaptation list; DESCRIPTION Suggests has jsonlite (D-015); 6 skip guards in the test file.
- AC5 ✅ SOURCES.md QSF row machine-checked + "Qualtrics artifact" subsection (derived provenance, PNTS enumeration, per-drug divergence); NEWS 0.2.0 bullet present.
- AC6 ✅ document() no diff (clean tree after review-run); devtools::test() 7704 pass / 0 fail (review-run); devtools::check() 0 errors / 0 warnings / 0 notes (T5 this session, after the last package-content change; only cairn/ tracking edits since).
- AC7 ⏳ pending merge-approval gate below.

### Independent review fan-out (2026-07-16)

Three fresh-context reviewers → 6 findings → [S] scorer. The [O] diff-bug reviewer independently re-extracted the QSF with its own code and confirmed the committed artifact faithful to the keying tables (651 unique questions, cgr choices, count logic, gate enumerations, per-drug divergence as documented); [S] blame-history confirmed no M18/D-014/D-015 regression and the any_other/regex removals kill ancestral bugs, not deliberate work; [S] prior-PR: no prior-PR evidence (no GitHub review threads exist).

Actioned (≥80): F1 (82) rebuild_hitophsum_qsf could overwrite the good artifact after partial push failures → **fixed**: push now stops loudly listing failed questions before any export. F2 (85) gate test never asserted Or-conjunctions (an And regression hides 597 items and stays green) → **fixed**: first-expression-bare + all-Or assertions added.

Logged sub-80, fixed anyway as cheap hardening: F3 (78) ungated items never checked for absence of display logic → new converse test. F5 (76) choice display order unchecked for dict-serialized questions → ChoiceOrder assertion added. F6 (72) DESIGN.md cited the deleted devel/qualtrics_test.R (Generators family + Known issue #5) → both updated; issue #5 narrowed to the .txt generators, its .qsf half resolved by M19.

Logged sub-80, rejected as framed: F4 (38) envelope-unwrap re-serialization corruption — scorer empirically refuted the auto_unbox mechanism; the never-exercised fallback was nonetheless deleted in favor of a loud stop, eliminating the residual digits risk.

Post-fix: QSF file 8305 assertions / 0 fail; full suite 9588 / 0; cairn_validate all pass.

### Consistency gate (2026-07-16)

cairn_validate: all checks pass after one review-side repair — the cairn plugin updated mid-session (script mtime 21:24) and its new "profile valid" check requires a `## changelog` slot; copied verbatim from the plugin's shipped r-package reference profile into cairn/PROFILE.md (NEWS.md). Advisory warnings: the known legacy dangling-token set. Toolchain slot: document() no-diff ✅; README untouched by this branch ✅; pkgdown::check_pkgdown() no problems ✅; NEWS entry ✅; no new top-level files ✅; check() clean ✅.
