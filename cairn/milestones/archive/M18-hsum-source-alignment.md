# M18: HiTOP-HSUM source alignment (revised SUD module, August 2024) — done 2026-07-16

**Goal:** make `hitophsum_items`/`hitophsum_choices` and the HSUM DOCX/REDCap generators faithful to the authoritative "revised SUD module-August 2024" sheet (D-014), fixing the broken cigar-quantity field, the PNTS gate leak, and the item-text divergences, with provenance in SOURCES.md.

**Outcome:** 66 item text/gate corrections (alcohol drink-specific wording incl. wrong-item sud16; nicotine sud01/09/11/13/14/17; other-drug template sud03/04/13/16; `hsum_nic_quant_oth` gate → forms 2/4/5/6); `quant_alcohol`/`quant_nic_cig`/`quant_nic_cgr` choice sets + `freq_heavy` label alignment; REDCap generator resolves quantity fields from `Choice_Set` (cgr dropdown was empty/invalid) with a cli_abort guard; `<> '99'` PNTS guards on comparison gates; new `other_drug_rule` arg (`"most_frequent"` argmax default per the sheet's looping rule, ties show all; `"per_drug"` keeps old behavior); docx overview synced (Street opioids, Goose bumps, SUD01 repair); prebuilt DOCX/ZIP regenerated. Provenance: SOURCES.md "HiTOP-HSUM provenance" (canonical hitop-system.org URL, sha256 prefix, 4 typo repairs + 6 normalizations logged).

**Verification:** new test-keying-hitophsum.R (643 sheet-transcribed assertions; counts 17/13/33) + extended REDCap/docx blocks (both rule modes from parsed output, global PNTS sweep); tests 1283/0; check() 0/0/0; document() no-diff; pkgdown clean; cairn_validate pass (one review-side repair: DESIGN principle bullets reformatted to validator-canonical `- IPn: …`, content unchanged); review fan-out (diff-bug [O], blame-history [S], prior-PR [S]) zero findings; CI 7/7 green.

**Decisions:** D-014 (source of truth + other_drug_rule design). All 9 AC verified incl. merge with Jeff's explicit keying sign-off (AC9, 2026-07-16 chip).

**Open remainder:** `inst/extdata/hitophsum_qualtrics.qsf` is hand-built in Qualtrics and remains stale → ROADMAP candidate.

**PR:** https://github.com/jmgirard/hitop/pull/20 (squash d55f3f8, branch `m18-hsum-source-alignment`)
