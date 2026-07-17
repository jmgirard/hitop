# Decisions

<!-- Append-only cross-cutting decisions. Never renumber; supersede with a new
     entry. Deferrals are ROADMAP facts, not decisions. -->

> **Legacy decision log:** D-001–D-012 (2026-07-09/-10) live in the embedded
> "Decision Log" section of [DESIGN.md](DESIGN.md), kept verbatim at the cairn
> migration (2026-07-16), and remain valid citations. To avoid ID collisions,
> new entries here continue the numbering at **D-013**.

### D-013 (2026-07-16): Adopt design principles IP1–IP4 / GP1–GP4 from the design interview

**Context:** The cairn migration left DESIGN.md rich on architecture but without formalized principles. The `/design-interview` elicited audience, boundary, governance, distribution, entry-bar, and dependency facts (Phase 1, commit c94f7db), banked nine candidates, and added history-mined and domain-derived ones in Phase 2.
**Decision:** Adopt four inviolables — IP1 instrument content sacrosanct (scope: item text and generated artifacts included), IP2 ground-truth oracles (scope: shipped numeric constants included), IP3 no scoring without an authoritative key / no norms without published tables, IP4 scores-never-judgment (line: cited-threshold flags in, generated interpretive prose out) — and four guiding principles: GP1 published rules win defaults, GP2 scored output never changes silently, GP3 researcher-first ergonomics, GP4 lean base-R core with family-earned Imports. B3/B6/B9 (CRAN discipline, pre-CRAN breakage, deliberate 4.1 floor) remain prose facts in DESIGN's "Audience, boundary & governance", deliberately unnumbered.
**Consequences:** Milestone plans can cite "touches IPn/GPn"; changing an IP requires a new D-entry; the trailing-periods class of item-text change now formally requires source verification (IP1); norms work inherits the IP2/IP3 bars before it begins.

### D-014 (2026-07-16): HiTOP-HSUM source of truth and `other_drug_rule` design

**Context:** M18 aligns `hitophsum_items`/`hitophsum_choices` and the HSUM generators to an authoritative source. The Society's workbook (`SUD module final analyses July 2024.xlsx`) contains multiple candidate sheets; the current package content diverges from all of them in wording, one quantity field, and gate logic. The sheet's design intends symptom items for only the most-frequently-used "other drug," but the shipped generator emits a looser per-drug gate.
**Decision:** (a) The **"revised SUD module-August 2024" sheet** is the single source of truth for HSUM item text and structure (Jeff, 2026-07-16); obvious sheet typos are repaired in package text with every divergence logged in `cairn/SOURCES.md` (SR/BR punctuation precedent). (b) `generate_redcap_hitophsum()` gains `other_drug_rule = c("most_frequent", "per_drug")`: the default emits argmax branching per the sheet's design (ties show both, documented); `"per_drug"` preserves the previous looser behavior as an explicit opt-in.
**Consequences:** IP1 verification for HSUM traces to this sheet (provenance in SOURCES.md); future HSUM scoring work inherits it. Default REDCap output changes for other-drug symptom gating — acceptable pre-1.0, noted in NEWS.

### D-015 (2026-07-16): jsonlite added to Suggests for QSF verification

**Context:** M19 locks the committed Qualtrics QSF artifact to `hitophsum_items`/`hitophsum_choices` via a parse-and-compare test (the oracle pattern of the REDCap export tests). QSF is JSON; base R has no JSON parser, and regex extraction from a 65k-character single-line file is too fragile to serve as an IP2 oracle.
**Decision:** Add **jsonlite to Suggests** — test-only, guarded by `skip_if_not_installed("jsonlite")`. No Imports change; the maintainer API script's httr2 dependency stays out of DESCRIPTION entirely (devel/ is .Rbuildignore'd).
**Consequences:** The QSF verification test parses structure rather than text; contributors without jsonlite skip those tests. GP4's lean-Imports posture is preserved (Suggests, family-earned by the export-verification family).
