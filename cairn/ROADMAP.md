# Roadmap

_The only authority on milestone status. Grouped by status, not ID._
_Last hygiene check: 2026-07-31 (M34 and M35 planned; M33's second-anchor candidate absorbed into M34, and two narrower candidates spawned in its place — the upward-displacement direction M34 leaves open, and CI-runnable fixtures for M35's comparison)_
_Pre-migration history: see `cairn/legacy/` and git log (M1–M17 done there; IDs continue — next new milestone is M36)._

## Milestones

| ID | Title | Status | Depends on | Priority | File/Archive |
|---|---|---|---|---|---|
| M34 | A second spot-value anchor per normed PID-5 column | review | — | normal | milestones/M34-second-norm-anchors.md |
| M35 | Cell-by-cell verification of the shipped norms against the book | planned | M34 | normal | milestones/M35-norms-cellwise-verification.md |
| M33 | PID-5 facet-level norms | done | — | normal | milestones/archive/M33-pid5-facet-norms.md |
| M32 | Test coverage for M31's argument-validation additions | done | — | normal | milestones/archive/M32-m31-validation-test-coverage.md |
| M31 | Argument-validation consistency and a harder norming oracle | done | — | normal | milestones/archive/M31-validator-and-oracle-residue.md |
| M30 | Norming-family test oracles and internal consistency | done | — | normal | milestones/archive/M30-norming-oracle-residue.md |
| M29 | `norm_pid5()` hygiene and robustness | done | — | normal | milestones/archive/M29-norm-pid5-hygiene.md |

## Candidates

- An anchor catching an *upward* percentile displacement (every row taking its successor's value) — M34 closes the downward direction only; its second anchors happen to cover the upward direction on 34 of the 66 T-scored columns, so 32 would need a third hand-read anchor, and no single anchor can serve both directions on the 14 columns with no interior T differing from both neighbours. Promote if an upward displacement ever reaches the shipped dataset — added 2026-07-31, figure corrected 2026-07-31 at M34 review (the row was written pre-M34 and said 41, which was the pre-M34 count) — lineage: M34 (which absorbed M33's original second-anchor row)
- Commit the norms CSVs or the book extraction as test fixtures so M35's cell-by-cell comparison can run in CI rather than only when the maintainer runs it — costs ~42 KB duplicating data already in `pid_norms`, and a second copy can drift; needs its own decision. Promote if a norms defect ever reaches a release through a path where the maintainer did not re-run the script — added 2026-07-31 — lineage: M35
- PID-5-BFP (36-item) data, scoring, exports — awaiting materials — added 2026-07-16 — `legacy/ROADMAP.md` Phase 1
- HiTOP-HSUM scoring + reliability + tutorial — awaiting Society feedback — added 2026-07-16 — `legacy/ROADMAP.md` Phase 1
- Norm-referenced profile plots for PID-5 (scored-profile visualization against `pid_norms`) — added 2026-07-16, narrowed 2026-07-30 (the norming-functions half graduated to M26, renumbered M27 at the same-day split) — `legacy/ROADMAP.md` Phase 3
- PID-5 demographically stratified norms — the sex/age-stratified tables appear nowhere in the markon2024 appendix and still await the maintainer; would extend `pid_norms` with no new export — added 2026-07-30, narrowed 2026-07-31 (the facet half graduated to M33) — lineage: M25
- PID-5 Informant Form (IRF) norms — markon2024 A–10/A–11 (domain, trait) and A–12 (SRF/IRF descriptives + T-score difference) arrived 2026-07-30; the package has no IRF surface today, so this is a new instrument version, not a `pid_norms` extension — added 2026-07-30 — lineage: M25
- HiTOP-SR/BR normative data and norming functions — source workbooks arrived 2026-07-30 (`HiTOP-SR-Final.xlsx`, `B-HiTOP overview.xlsx`); both carry sample descriptives (N/mean/SD/skew/kurtosis on a Prolific sample), not raw→T lookup tables, so the norming design differs from `pid_norms` — added 2026-07-30 — `legacy/ROADMAP.md` Phase 3
- Clinical reporting & release (individual reports, bass-ackwards analyses, CRAN submission + package paper) — added 2026-07-16 — `legacy/ROADMAP.md` Phase 4
- Someday-maybe cluster (Shiny scoring app stub `inst/shiny/app.R`, plotting helpers `devel/plotting.R`, HiTOP-SR/BR validity scales) — added 2026-07-16 — `legacy/ROADMAP.md`
- Multi-language download UI: per-language buttons on the instrument download pages won't scale once translations arrive; design a language selector/grouped layout — added 2026-07-17 — lineage: M21
- Response-option legend wraps mid-phrase on the PID paper forms: `make_items_table()` builds one 126-character string that breaks wherever the column ends; split it two-per-line at a bullet separator (break style chosen by the maintainer 2026-07-30). PID-only — the HiTOP-SR/BR legend is 58 chars and fits on one line — but the fix is in shared generator code and rebuilds 6 PID DOCX with manifest rows per D-016; pure layout, no wording change (IP1 style-fix carve-out) — added 2026-07-30 — lineage: M26
- Score HiTOP-SR subset-collected data: score data gathered from a subset instrument (columns = subset items, original HSR numbering) via the `hitop_subset` descriptor — added 2026-07-17 — depends on M24 (plan after M24 lands; second half of the modularization arc)
- Generalize modularization to BR/PID-5: extend the subset-descriptor + subset generation/scoring to HiTOP-BR (overlapping scales, e.g. p-Factor spans all items) and PID-5 (facets partition, domains derive from facets) — added 2026-07-17 — lineage: M24
