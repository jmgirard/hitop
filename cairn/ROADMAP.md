# Roadmap

_The only authority on milestone status. Grouped by status, not ID._
_Last hygiene check: 2026-07-31 (status audit, no work since M31's post-merge pass; all checks pass, the 20 `dangling id tokens` advisories are the standing pre-migration references in DESIGN.md/SOURCES.md; GitHub issue and PR inboxes both empty; nothing in flight — next-milestone ID corrected to M32)_
_Pre-migration history: see `cairn/legacy/` and git log (M1–M17 done there; IDs continue — next new milestone is M32 (corrected 2026-07-31; the line still said M29 after M29–M31 were assigned))._

## Milestones

| ID | Title | Status | Depends on | Priority | File/Archive |
|---|---|---|---|---|---|
| M32 | Test coverage for M31's argument-validation additions | review | — | normal | milestones/M32-m31-validation-test-coverage.md |
| M31 | Argument-validation consistency and a harder norming oracle | done | — | normal | milestones/archive/M31-validator-and-oracle-residue.md |
| M30 | Norming-family test oracles and internal consistency | done | — | normal | milestones/archive/M30-norming-oracle-residue.md |
| M29 | `norm_pid5()` hygiene and robustness | done | — | normal | milestones/archive/M29-norm-pid5-hygiene.md |
| M27 | PID-5 raw → T / percentile conversion (`norm_pid5()` on the official coding) | done | M26 | normal | milestones/archive/M27-pid5-norming-functions.md |
| M28 | PID-5 norming under shifted response codings, and the vignette norming sections | done | M27 | normal | milestones/archive/M28-pid5-norming-shifted-codings.md |

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
- Generalize modularization to BR/PID-5: extend the subset-descriptor + subset generation/scoring to HiTOP-BR (overlapping scales, e.g. p-Factor spans all items) and PID-5 (facets partition, domains derive from facets) — added 2026-07-17 — lineage: M24
