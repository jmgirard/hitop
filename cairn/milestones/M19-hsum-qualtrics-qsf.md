<!-- Section ownership + write-modes: see the cairn plugin's tracking-rules.md
     "Milestone-file section ownership". -->
# M19: HSUM Qualtrics QSF rebuild (API script + verification test)

- **Status:** in-progress
- **Priority:** normal
- **Depends on:** —
- **Principles touched:** IP1, IP2
- **Branch/PR:** `m19-hsum-qualtrics-qsf`

## Goal

Replace the stale hand-built `inst/extdata/hitophsum_qualtrics.qsf` with one rebuilt from the corrected keying tables via a modernized maintainer API script, and lock the committed artifact to `hitophsum_items`/`hitophsum_choices` with a parse-and-compare test so QSF staleness becomes a test failure.

## Scope

**In:** modernizing `devel/qualtrics_test.R` → `devel/qualtrics_hitophsum.R` (Choice_Set resolution, drop `any_other`, run recipe); Jeff's API run + manual QSF export replacing the committed artifact; the QSF verification test (full structural bar); jsonlite → Suggests (D-015); SOURCES.md/NEWS updates.

**Out:** an exported `generate_qualtrics_hitophsum()` (QSF stays maintainer-generated; the TXT format the exported family uses cannot carry display logic). API-based end-user import functions — declined 2026-07-16 (auth burden vs. GP3/GP4; QSF download stays the distribution contract). The most-frequent other-drug rule in Qualtrics — platform cannot express cross-question comparison declaratively; the QSF encodes the per-drug rule (sheet-sanctioned loosening), documented as a divergence in SOURCES.md.

## Acceptance criteria

- [ ] New `tests/testthat/test-qualtrics-hitophsum.R` parses the committed QSF (jsonlite, `skip_if_not_installed`) and passes: exactly one question element per `hitophsum_items` row (+ instructions) with unique `DataExportTag`s (kills the current duplicate-block defect: 1302 SQ for ~650 items)
- [ ] QSF content fidelity, machine-checked: item text matches `hitophsum_items$Text` modulo the adaptations documented in SOURCES.md (withdrawal-symptom bolding, piped "other" substance name); every choice-bearing question's values/labels match its `Choice_Set` (incl. the M18 `quant_*` sets)
- [ ] QSF logic fidelity, machine-checked: every gated item's display logic is the correct expansion of `Gate_Variable`/`Gate_Value` against `hitophsum_choices` (discrete lists, threshold enumerations, `count>1` via `SelectedChoicesCount`), and no display-logic condition anywhere selects choice 99 (PNTS)
- [ ] `devel/qualtrics_hitophsum.R` resolves field types/choices from `Choice_Set` (no variable-name regexes, no `any_other` branch), with a header documenting required local packages (httr2, cli), token/datacenter inputs, and the run→export→commit recipe; jsonlite added to Suggests (D-015)
- [ ] SOURCES.md: HSUM verification-table QSF row flips to machine-checked; per-drug-rule platform divergence and text adaptations documented; NEWS.md bullet
- [ ] `devtools::document()` no-diff; `devtools::test()` passes; `devtools::check()` clean
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
- [ ] T4 **Jeff (user action):** run the script against a fresh survey with his API token, export the QSF from Qualtrics, replace `inst/extdata/hitophsum_qualtrics.qsf`; T1 test goes green
- [ ] T5 SOURCES.md (QSF row machine-checked; divergence + adaptation notes; generator provenance line), NEWS.md bullet; `document()`/`test()`/`check()`
- [ ] T6 PR, record URL, merge on Jeff's approval

## Work log

- 2026-07-16: created by /milestone-plan. Absorbs the 2026-07-16 ROADMAP candidate "Rebuild inst/extdata/hitophsum_qualtrics.qsf…" (M18's open remainder). Gate decisions (Jeff): pipeline approach confirmed; jsonlite → Suggests (D-015); full structural test bar.
- 2026-07-16: implementation started; branch cut. Pre-impl question gate skipped — all choices settled at the plan gate this session.
- 2026-07-16: T1 done — test red against the stale file exactly where staleness lives (duplicate tags, 476 stale texts, any_other gate on nic_quant_oth). Discovery: the stale QSF has NULL choices on hsum_nic_quant_cgr — the cigar-quantity defect M18 fixed in REDCap exists in the hand-built Qualtrics survey too (same ancestral regex bug).
- 2026-07-16: T2 done (jsonlite → Suggests). T3 done — script rewritten as devel/qualtrics_hitophsum.R (old qualtrics_test.R removed): Field_Type/Choice_Set resolution with a stop() on unresolved sets, any_other deleted, header carries run recipe + adaptation list; offline dry-run confirms all 650 items resolve type/choices/gates cleanly. Full suite: failures confined to the deliberate-red QSF file. Next: T4 requires Jeff at the Qualtrics console.

## Decisions

## Review
