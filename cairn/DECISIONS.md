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

### D-016 (2026-07-16): Build-date versioning + checksum-locked manifest for distributed artifacts

**Context:** Committed `inst/extdata/` artifacts are distributed via raw/main links on the pkgdown download pages; the M19 QSF rebuild changed a file at a stable URL with no user-visible version signal, and filenames carried the instrument version (`_1.0_`) inconsistently while identifying builds not at all.
**Decision:** (a) Artifact revisions are identified by **build date** (YYYY-MM-DD); the instrument version (the Society's/APA's "1.0") stays a separate concept. (b) A committed manifest dataset **`hitop_artifacts`** (`file`, `instrument`, `format`, `instrument_version`, `build_date`, `md5`, `changes`; one row per build, history kept) is the single source of truth; a test locks every committed artifact's checksum to its current manifest row, so no artifact changes without a version bump — GP2 extended to artifacts. (c) Stamps are embedded where the format allows: DOCX footer ("Generated YYYY-MM-DD · hitop X.Y.Z"), QSF `SurveyName`; the flat Qualtrics .txt and REDCap zips have no safe metadata slot and are covered by the manifest and download pages only. (d) **Filenames carry no version** — stable URLs, the latest build always at the same link; the old `_1.0_` URLs break once (accepted, Jeff 2026-07-16). (e) `tools::md5sum()`, not sha256 — change-detection needs no cryptographic strength and md5 costs zero new dependencies (GP4).
**Consequences:** Every future artifact regeneration must add a manifest row or tests fail; download pages render version history from the manifest; generator default `file` arguments drop `_1.0`; embedding the DOCX footer stamp is an IP1-sanctioned metadata addition (signed off at the M20 plan gate), never a license for content drift — the parse-and-compare tests must pass unchanged.

### D-017 (2026-07-30): Add a PID-5-BF total score to `score_pid5()` (a GP2 scored-output change, planned into M26)

**Context:** The PID-5-BF normative table the maintainer transcribed from the PID-5 book (`data-raw/norms_pid5bf_domains.csv`) norms a **total** score (`TOT_Raw`/`TOT_Ptl`, T 35–95) alongside the five domains, but `score_pid5(version = "BF")` returns only the five domains — so the normed total has nothing to convert. The M25 plan-gate criteria audit surfaced this as an unreachable state: satisfying it means adding a scale, which IP1 gates behind maintainer sign-off.
**Decision:** `score_pid5(version = "BF")` gains a total-score column (`prefix` + `total`), signed off by Jeff at the 2026-07-30 plan gate. It is implemented in **M26**, not M25 — M25 stays data-only and ships the TOT rows in `pid_norms`. The exact rule (item-level mean over all 25 items versus the mean of the five domain means, and its behavior under `missing = "apa"`) is resolved from the book at M26 T2 and traced in `cairn/SOURCES.md`; the two agree on complete data and diverge only under missingness. Rejected: shipping the TOT norm rows with no scorer (a norm with nothing to convert), and dropping the TOT rows (discards transcribed data the book publishes).
**Consequences:** BF scored output gains a column — a GP2 change, so it is NEWS-flagged and covered by a hand-computed oracle test; FULL/SF output is untouched. `pid_scales` gains a BF total entry, which is IP1 content and traces to a cited source before it ships (IP2). Downstream code that counts `score_pid5(version = "BF")` columns must migrate; acceptable pre-CRAN.
