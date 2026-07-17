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
