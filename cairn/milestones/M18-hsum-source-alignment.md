<!-- Section ownership + write-modes: see the cairn plugin's tracking-rules.md
     "Milestone-file section ownership". Translated 2026-07-16 from the legacy
     project/MILESTONES.md draft (entombed at cairn/legacy/MILESTONES.md). -->
# M18: HiTOP-HSUM source alignment (revised SUD module, August 2024)

- **Status:** planned
- **Priority:** normal
- **Depends on:** —
- **Principles touched:** —
- **Branch/PR:** —

## Goal

Make `hitophsum_items`/`hitophsum_choices` and the HSUM DOCX/REDCap generators faithful to the authoritative "revised SUD module-August 2024" sheet (Jeff confirmed it as the source to build from, 2026-07-16), fixing the broken cigar-quantity field, the PNTS gate leak, and the item-text divergences, with provenance recorded in SOURCES.md.

## Scope

**In:** `hitophsum_items`/`hitophsum_choices` item text + gate corrections, the HSUM REDCap/DOCX generator fixes, the new `other_drug_rule` argument, and HSUM provenance in `cairn/SOURCES.md`.

**Out:** HSUM *scoring* (awaiting Society feedback → ROADMAP candidate). Deriving the docx overview's item matrix from `hitophsum_items` (known accepted duplication — note in DESIGN when work starts).

## Acceptance criteria

<!-- Drafted 2026-07-16 in the legacy tracker; Jeff asked to RE-CONFIRM these
     at planning/implementation start (see Work log). -->

- [ ] SOURCES.md has a HiTOP-HSUM provenance section: source citation (xlsx + sheet name), verification-status table, and a deliberate-divergence log (typo fixes, label normalizations)
- [ ] `hitophsum_items$Text` matches the sheet's substance-specific wording (typos repaired) — machine-checked against strings hand-transcribed from the sheet, plus count invariants (17 SUD non-nicotine / 13 nicotine / 33 WITH per substance)
- [ ] Generated REDCap dictionary: `hsum_nic_quant_cgr` is a dropdown with 1–60+ & Prefer-not-to-say choices (currently a dropdown with **empty** choices — invalid import)
- [ ] No symptom gate is satisfied by frequency = 99 (PNTS): emitted comparison gates carry a `<> 99` guard, asserted via hand-derived branching strings
- [ ] `hsum_nic_quant_oth` shows only for nicotine forms 2/4/5/6 (currently also fires for cigars-only respondents)
- [ ] New `other_drug_rule = c("most_frequent", "per_drug")` arg on `generate_redcap_hitophsum()`: default emits argmax branching (only the most-frequently-used other drug ≥ monthly gets symptom items; ties show both, documented); `"per_drug"` reproduces the current loosened per-drug gate — both modes asserted from parsed output
- [ ] docx overview says "Street opioids" (not "Heroin/opiates") and "Goose bumps"; its SUD matrix matches the corrected wording
- [ ] `devtools::document()` no-diff beyond intended; `devtools::test()` passes; `devtools::check()` clean
- [ ] PR merged **with Jeff's explicit sign-off** (keying content: `hitophsum_items`/`hitophsum_choices` change)

## Coverage

- AC1 → T8
- AC2 → T1, T2, T3
- AC3 → T1, T3, T4
- AC4 → T1, T5
- AC5 → T1, T2
- AC6 → T1, T6
- AC7 → T7
- AC8 → T8
- AC9 → T9

## Tasks

- [ ] T1 Write failing tests first: extend the HSUM block in tests/testthat/test-generate_redcap.R (~lines 147–190) — cgr dropdown + choices, `<> 99` guards, discrete-list `any_other`, both `other_drug_rule` modes; new keying-test block pinning corrected `hitophsum_items` text to hand-transcribed sheet strings
- [ ] T2 Correct data-raw/hitophsum_items.csv Text per the sheet's three columns: alcohol-specific wording (sud01–17, incl. the wrong-item sud16), nicotine sud13/14/17, other-drug sud04/16 template; fix obvious sheet typos ("urge to drink"→"use [substance]", "sunstacne") and log each in SOURCES.md; change `hsum_nic_quant_oth` Gate_Value → `2,4,5,6`
- [ ] T3 Add `quant_alcohol` / `quant_nic_cig` / `quant_nic_cgr` choice sets to data-raw/hitophsum_choices.csv; regenerate both `.rda` via data-raw/hitophsum_info.R
- [ ] T4 R/generate_redcap.R:526–556: drop the fragile variable-name regexes; resolve quantity field types/choices from `Choice_Set` (fixes cgr — regex `(cig|cigar)` never matches `_cgr`)
- [ ] T5 R/generate_redcap.R:456–499: append `and [var] <> '99'` to comparison gates whose parent choice set contains 99
- [ ] T6 Add `other_drug_rule` argument + argmax branching builder (other-drug set derived from `hitophsum_items`, never hardcoded); roxygen documents both modes and tie behavior; uses REDCap `if()` in branching (already required by the `count>1` gate, so no new version constraint)
- [ ] T7 R/generate_docx.R:451 "Heroin/opiates"→"Street opioids"; :675 "Goosebumps"→"Goose bumps"; sync the hardcoded SUD matrix with the corrected wording
- [ ] T8 SOURCES.md HSUM section; NEWS.md bullet; `document()`/`test()`/`check()`
- [ ] T9 Branch `m18-hsum-source-alignment`, PR, record URL, await sign-off

## Notes

Source: "revised SUD module-August 2024" sheet of `SUD module final analyses July 2024.xlsx` (Jeff's copy in ~/Downloads, webflow-hashed filename — likely a HiTOP-site asset; get/record the canonical URL). Decisions already made by Jeff (2026-07-16): fix obvious sheet typos with divergences logged (SR/BR punctuation precedent); most-frequent-other-drug rule is the default, customizable via `other_drug_rule`. Full verification findings (faithful-logic table + all divergences) in the 2026-07-16 session. Add a D-entry for the source-of-truth + `other_drug_rule` design when work starts.

## Work log

- 2026-07-16: drafted in the legacy tracker (project/MILESTONES.md) from the HSUM verification session; AC marked "re-confirm at planning".
- 2026-07-16: translated to cairn format at migration (cairn-init); content unchanged. /milestone-implement must re-confirm the AC with Jeff before starting.

## Decisions

## Review
