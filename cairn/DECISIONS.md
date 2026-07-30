# Decisions

<!-- Append-only cross-cutting decisions. Never renumber; supersede with a new
     entry. Deferrals are ROADMAP facts, not decisions. -->

> **Legacy decision log:** D-001–D-012 (2026-07-09/-10) live in the embedded
> "Decision Log" section of [DESIGN.md](DESIGN.md), kept verbatim at the cairn
> migration (2026-07-16), and remain valid citations. To avoid ID collisions,
> new entries here continue the numbering at **D-013**.

### D-018 (2026-07-30): Validity-scale names follow the scale-development papers, not Markon et al. (2024) — `INC` is not renamed to `VRIN` (annotates D-017's M25/M26 norms arc)

The PID-5 book (Markon, Fossati, Somma & Krueger, 2024) calls Keeley's full-form
inconsistency scale "VRIN" where this package calls it `INC`. Raised at M25's
implementation gate as a possible harmonization; escalated as an irreversible
exported-API decision and reviewed independently (RB01/RR01, now in
`reviews/archive/`). **Decision: keep the current names on every surface** —
`validity_pid5()` output columns, the `pid_items` keying columns, and
`pid_norms$scale` alike. The book's labels are recorded as provenance in
`SOURCES.md` and surfaced to users as an "also known as" note in the
documentation, never as a second naming scheme in the data.

Three grounds. The book is not a reliable naming authority even about itself: its
Appendix captions call *both* inconsistency tables VRIN, while Chapter 4 names the
100-item scale PID-5-INC-S. The primary sources favor `INC` — Keeley et al. (2016)
and Lowmaster et al. (2020) are both titled "Response Inconsistency Scale", and
Lowmaster's PID-5-INC-S treats `INC` as the parent stem, a lineage the package's
`INC`/`INCS` pair encodes and a `VRIN`/`INCS` pair would sever. And no rename of the
pair is simultaneously consistent with the book, the development papers, and that
lineage, so harmonization has no reachable end state.

The general rule this settles, beyond the one scale: **authority is per-content-type,
not per-document.** The book is the primary source for its own normative tables and is
cited as such (IP2, IP3); for the identity and name of a scale it did not develop, the
development paper is primary and the book is a secondary description — regardless of
its authors having created the instrument itself. An exported alias dataset mapping
book names to columns was considered and rejected (GP3, GP4): two documentation
sentences do the same work without a permanent exported surface.

If a future maintainer overrules this, RR01 records the path — one release, all
surfaces at once, NEWS entry, no dual columns and no `lifecycle` (GP2 licenses a
signature break outright; `lifecycle` is for functions and arguments, not output
column names, and is not a current Import).

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

### D-019 (2026-07-30): The PID-5-BF total enters `pid_scales`, and its ripple into `reliability_pid5()` and the BF paper forms is accepted (extends D-017; re-points D-017's M26 task references after the M26/M27 split)

**Context:** D-017 signed off the BF total and noted that "`pid_scales` gains a BF total entry, which is IP1 content", but scoped its consequences to `score_pid5()`. The M26 plan-gate criteria audit found that `pid_scales[[version]]$itemNumbers` has three consumers, not one: `score_pid5()` (`R/score_pid5.R:107`), `reliability_pid5()` (`R/reliability_pid5.R:60`), and the printed scoring table of `generate_docx_pid5(version = "BF", include_scoring = TRUE)` (`R/generate_docx.R:1136`). The third is participant-facing artifact content, which IP1 gates behind maintainer sign-off and D-016 manifest-locks by md5, so adding the row would silently stale two committed DOCX artifacts.
**Decision:** The total is a real `pid_scales[["BF"]]` row, not a `score_pid5()` special case — signed off by Jeff at the 2026-07-30 plan gate, which is the IP1 sign-off for the artifact change as well as for the keying-table change. `reliability_pid5(version = "BF")` therefore returns six rows including `total`, and `pid5bf_A4.docx` / `pid5bf_US.docx` are regenerated with the total in their scoring table plus new `hitop_artifacts` rows per D-016. Rejected: a scorer-only special case leaving `pid_scales` untouched (invisible to every other consumer, needs permanent special-casing), and adding the row while suppressing it in the reliability and DOCX consumers (two new flags whose only purpose is to keep the artifacts frozen).
**Consequences:** `reliability_pid5(version = "BF")` gains a row — an exported-output change, NEWS-flagged under GP2 alongside the scored-output change. The two BF DOCX artifacts change bytes and need manifest rows; the BF Qualtrics and REDCap artifacts do not, because their generators never read `pid_scales`, and M26 AC4 locks them byte-identical as evidence. D-017's "implemented in M26" and "M26 T2" pointers now resolve to **M26 T1** — the norming half of the original M26 became M27 at the same gate.

### D-020 (2026-07-30): `norm_pid5()` reconciles a shifted response coding per scale, including the validity scales; `validity_pid5()`'s cut scores stay deferred

**Context:** `pid_norms` mixes metrics. Domain and BF-total rows are item means; `INC`/`INCS` are sums of within-pair absolute differences; `ORS` is a count of items at the range maximum (`R/validity_pid5.R:153`); `PRD` is a 22-item raw sum. A user scoring on a shifted four-option coding (1–4 against the official 0–3) needs each reconciled to the official range before lookup, and a single per-item shift — the obvious reading of a scale-agnostic rescale — is right for the means and wrong for the other three. DESIGN Known issue #3 defers the parallel question for `validity_pid5()`'s cut scores pending maintainer sign-off.
**Decision:** `norm_pid5()` reconciles each named column by its own metric — item means shifted by `low`, `PRD` by `low × nItems`, `ORS` re-derived against the shifted maximum, `INC`/`INCS` left unchanged as already coding-invariant. Signed off by Jeff at the 2026-07-30 plan gate. No published source states these formulas, so each is documented in `@details` on its merits (GP1's "where no published rule exists" branch) and the adjustment is reported once via `cli::cli_alert_info()` naming which scales were adjusted and which were not. Rejected: reconciling only the mean-metric domains and returning `NA` for the four validity scales (discards a derivable conversion), and returning `NA` for every column on any non-official coding (discards the well-defined domain conversion too).
**Consequences:** This is a norming-lookup decision only. `validity_pid5()`'s cut scores keep their current behavior — a warning when `srange != c(0, 3)`, no auto-adjustment — and DESIGN Known issue #3 stays open, so a user on a shifted coding can receive a reconciled norm percentile and an unreconciled cut-score flag in the same session; M27's `@details` must say so. Closing issue #3 is separate work.

### D-021 (2026-07-30): The PID-5-BF total prorates independently of the five domains under `missing = "apa"` (resolves the behavior D-017 deferred; promotes M26-D1)

**Context:** D-017 added the PID-5-BF total but explicitly left "its behavior under `missing = "apa"`" to be resolved in M26, and Markon et al. (2024) state a computation rule for the total (p. 23) but no missing-data rule for it anywhere. Because `score_pid5()` applies `missing` per scale, the total's 25-item basis and a domain's 5-item basis reach the APA 25%-unanswered threshold at different points: the total drops at 7 unanswered, a domain at 2. M26 recorded the resolution as the milestone-local entry M26-D1; the maintainer directed at the M26 merge gate (2026-07-30) that it be promoted here, since D-017 raised the question as a cross-cutting one.
**Decision:** The total is scored as its own 25-item scale and prorates independently of the domains — no special case couples them. GP1 governs: the book's stated computation is the published rule and takes the default, and the alternative would impose a coupling the source does not ask for. Rejected: blanking the total whenever any domain blanks (internally tidier profiles, but a deviation from the published rule with no source behind it), and blanking it only when every domain blanks (unreachable — see below).
**Consequences:** A total can be reported beside as many as **3 `NA` domains** (6 unanswered, two in each of three domains). The converse cannot occur: blanking all five domains needs 10 unanswered items, and the total drops at 7 — so "all five domains `NA` beside a reported total" is impossible, and the reverse case (an `NA` total beside reported domains) is reachable at 7+ unanswered. Documented in `score_pid5()`'s `@details`, `cairn/SOURCES.md` ("Note on the BF total score"), and NEWS; covered by tests at the 6/7-unanswered boundary. This governs the norming lookup M27 builds on: a scored total that is `NA` has no T score to convert, independently of its domains. Supersedes M26-D1, whose stated reason for staying milestone-local no longer holds.

### D-022 (2026-07-30): Norming functions return published cells only — no constant fitted from a norms table ships (from RR02; extends IP2/IP3 across the norming family)

**Context:** RR02 established that the 16 domain/total tables of Markon et al. (2024) are exact linear renderings of `raw = M + (T − 50)/10 × SD` at 2-dp rounding — 844 of 850 printed non-zero cells reproduce exactly — while the book prints M and SD nowhere. The generator is therefore recoverable by regression from the shipped data, which raised whether `norm_pid5()` may return a between-rows T by inverting that line instead of selecting a printed row. The question generalizes: the HiTOP-SR/BR source workbooks that arrived 2026-07-30 carry sample descriptives rather than lookup tables, so the same temptation will recur with a *better* excuse.
**Decision:** Every T score and percentile a norming function returns is a value printed in a shipped normative table, selected by arithmetic on printed cells only. No constant derived by fitting a shipped norms table — mean, SD, slope, intercept, or equivalent — ships in package code, package data, or a documented formula. Fitting is licensed at test time only, as IP2's "independent recomputation": constants derived inside a test from the shipped rows never touch a returned value. Rejected: shipping the fitted parameters (returned numbers would trace to a regression run in this repo rather than to a cited authority, failing IP2 directly and IP3 in spirit — the norms applied would be a reconstruction of the tables' generator, not the tables), and interpolating between printed rows (manufactures resolution the source does not print, with no oracle able to check it).
**Consequences:** Binds the whole norming family, not only PID-5 — a future SR/BR norming milestone working from descriptives rather than tables must publish a table or escalate, not fit one here. Costs a little accuracy at knife-edge midpoints, where RR02 found nearest-row and rounded inversion agree to within the tables' own 2-dp rounding anyway, and where the printed table is the more authoritative of the two. Does not touch D-020's `srange` reconciliation, whose per-scale formulas are coding-metric derivations rather than anything read off `pid_norms`.
