# Roadmap

_The only authority on milestone status. Grouped by status, not ID._
_Last hygiene check: 2026-07-31 (status audit after M28: all 16 checks pass; the 20 `dangling id tokens` advisories all resolve to pre-migration IDs — `D-001`–`D-012` in DESIGN.md's embedded Decision Log, `M13`/`M14` in `cairn/legacy/` — reported, not fixed; both GitHub inboxes empty; M29 planned from the `norm_pid5()` hygiene candidate)_
_Pre-migration history: see `cairn/legacy/` and git log (M1–M17 done there; IDs continue — next new milestone is M29)._

## Milestones

| ID | Title | Status | Depends on | Priority | File/Archive |
|---|---|---|---|---|---|
| M29 | `norm_pid5()` hygiene and robustness | review | — | normal | milestones/M29-norm-pid5-hygiene.md |
| M27 | PID-5 raw → T / percentile conversion (`norm_pid5()` on the official coding) | done | M26 | normal | milestones/archive/M27-pid5-norming-functions.md |
| M28 | PID-5 norming under shifted response codings, and the vignette norming sections | done | M27 | normal | milestones/archive/M28-pid5-norming-shifted-codings.md |
| M26 | PID-5-BF total score across scoring, reliability, and the BF paper forms | done | — | normal | milestones/archive/M26-pid5bf-total-score.md |
| M25 | PID-5 normative tables — verification and ingest | done | — | normal | milestones/archive/M25-pid5-norms-ingest.md |
| M24 | HiTOP-SR scale-subset generation (subset descriptor + docx/Qualtrics/REDCap) | done | — | normal | milestones/archive/M24-hitopsr-subset-generation.md |

## Candidates

- PID-5-BFP (36-item) data, scoring, exports — awaiting materials — added 2026-07-16 — `legacy/ROADMAP.md` Phase 1
- HiTOP-HSUM scoring + reliability + tutorial — awaiting Society feedback — added 2026-07-16 — `legacy/ROADMAP.md` Phase 1
- Norm-referenced profile plots for PID-5 (scored-profile visualization against `pid_norms`) — added 2026-07-16, narrowed 2026-07-30 (the norming-functions half graduated to M26, renumbered M27 at the same-day split) — `legacy/ROADMAP.md` Phase 3
- PID-5 facet-level and demographically stratified norms — the book's facet tables arrived 2026-07-30 (markon2024 A–6 SRF, A–8 SF); the sex/age-stratified half is not in the appendix and still awaits the maintainer; extends `pid_norms` with no new export — added 2026-07-30 — lineage: M25
- PID-5 Informant Form (IRF) norms — markon2024 A–10/A–11 (domain, trait) and A–12 (SRF/IRF descriptives + T-score difference) arrived 2026-07-30; the package has no IRF surface today, so this is a new instrument version, not a `pid_norms` extension — added 2026-07-30 — lineage: M25
- HiTOP-SR/BR normative data and norming functions — source workbooks arrived 2026-07-30 (`HiTOP-SR-Final.xlsx`, `B-HiTOP overview.xlsx`); both carry sample descriptives (N/mean/SD/skew/kurtosis on a Prolific sample), not raw→T lookup tables, so the norming design differs from `pid_norms` — added 2026-07-30 — `legacy/ROADMAP.md` Phase 3
- Clinical reporting & release (individual reports, bass-ackwards analyses, CRAN submission + package paper) — added 2026-07-16 — `legacy/ROADMAP.md` Phase 4
- Someday-maybe cluster (Shiny scoring app stub `inst/shiny/app.R`, plotting helpers `devel/plotting.R`, HiTOP-SR/BR validity scales) — added 2026-07-16 — `legacy/ROADMAP.md`
- Multi-language download UI: per-language buttons on the instrument download pages won't scale once translations arrive; design a language selector/grouped layout — added 2026-07-17 — lineage: M21
- Response-option legend wraps mid-phrase on the PID paper forms: `make_items_table()` builds one 126-character string that breaks wherever the column ends; split it two-per-line at a bullet separator (break style chosen by the maintainer 2026-07-30). PID-only — the HiTOP-SR/BR legend is 58 chars and fits on one line — but the fix is in shared generator code and rebuilds 6 PID DOCX with manifest rows per D-016; pure layout, no wording change (IP1 style-fix carve-out) — added 2026-07-30 — lineage: M26
- Score HiTOP-SR subset-collected data: score data gathered from a subset instrument (columns = subset items, original HSR numbering) via the `hitop_subset` descriptor — added 2026-07-17 — depends on M24 (plan after M24 lands; second half of the modularization arc)
- Norming-family review residue: `norm_pid5()`/`norm_engine.R` test oracles that transcribe the implementation's own constants rather than reading `pid_norms` (against IP2), plus internal-consistency polish — assert the three metric vectors are pairwise disjoint, blame the exported caller in `norm_metric()`'s abort, list `strip_prefix()` and D-024/D-025's alert-convention carve-out in DESIGN.md/CLAUDE.md — added 2026-07-31 — lineage: M29 (its 24 sub-threshold review findings, highest 78)
- Generalize modularization to BR/PID-5: extend the subset-descriptor + subset generation/scoring to HiTOP-BR (overlapping scales, e.g. p-Factor spans all items) and PID-5 (facets partition, domains derive from facets) — added 2026-07-17 — lineage: M24
