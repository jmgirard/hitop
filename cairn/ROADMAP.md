# Roadmap

_The only authority on milestone status. Grouped by status, not ID._
_Last hygiene check: 2026-07-17 (M23 close-out)_
_Pre-migration history: see `cairn/legacy/` and git log (M1–M17 done there; IDs continue — next new milestone is M22)._

## Milestones

| ID | Title | Status | Depends on | Priority | File/Archive |
|---|---|---|---|---|---|
| M23 | Overview instrument page (SR/BR/HSUM link cards) | done | — | normal | milestones/archive/M23-overview-instrument-page.md |
| M22 | Centralize import instructions (Qualtrics QSF+TXT, REDCap ZIP) | done | — | normal | milestones/archive/M22-centralize-import-instructions.md |
| M21 | Beautify instrument download pages (manifest badges + collapsible history) | done | — | normal | milestones/archive/M21-beautify-download-pages.md |
| M20 | Artifact versioning — build-date manifest + checksum lock | done | — | normal | milestones/archive/M20-artifact-versioning.md |
| M19 | HSUM Qualtrics QSF rebuild (API script + verification test) | done | — | normal | milestones/archive/M19-hsum-qualtrics-qsf.md |

## Candidates

- Norm-ready response ranges (legacy M16: official-range rescaling in the norming layer; loud warning on option-count mismatch) — blocked on PID-5 normative data — added 2026-07-16 — detail in `legacy/MILESTONES.md` (M16)
- PID-5-BFP (36-item) data, scoring, exports — awaiting materials — added 2026-07-16 — `legacy/ROADMAP.md` Phase 1
- HiTOP-HSUM scoring + reliability + tutorial — awaiting Society feedback — added 2026-07-16 — `legacy/ROADMAP.md` Phase 1
- Norms & visualization (PID-5 norming functions, scored-profile plots) — awaiting normative data — added 2026-07-16 — `legacy/ROADMAP.md` Phase 3
- Clinical reporting & release (individual reports, bass-ackwards analyses, CRAN submission + package paper) — added 2026-07-16 — `legacy/ROADMAP.md` Phase 4
- Someday-maybe cluster (Shiny scoring app stub `inst/shiny/app.R`, plotting helpers `devel/plotting.R`, HiTOP-SR/BR validity scales) — added 2026-07-16 — `legacy/ROADMAP.md`
- Multi-language download UI: per-language buttons on the instrument download pages won't scale once translations arrive; design a language selector/grouped layout — added 2026-07-17 — lineage: M21
