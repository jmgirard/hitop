# Roadmap

_The only authority on milestone status. Grouped by status, not ID._
_Last hygiene check: 2026-07-16_
_Pre-migration history: see `cairn/legacy/` and git log (M1–M17 done there; IDs continue — next new milestone is M20)._

## Milestones

| ID | Title | Status | Depends on | Priority | File/Archive |
|---|---|---|---|---|---|
| M19 | HSUM Qualtrics QSF rebuild (API script + verification test) | review | — | normal | milestones/M19-hsum-qualtrics-qsf.md |
| M18 | HiTOP-HSUM source alignment (revised SUD module, Aug 2024) | done | — | normal | milestones/archive/M18-hsum-source-alignment.md |

## Candidates

- Norm-ready response ranges (legacy M16: official-range rescaling in the norming layer; loud warning on option-count mismatch) — blocked on PID-5 normative data — added 2026-07-16 — detail in `legacy/MILESTONES.md` (M16)
- PID-5-BFP (36-item) data, scoring, exports — awaiting materials — added 2026-07-16 — `legacy/ROADMAP.md` Phase 1
- HiTOP-HSUM scoring + reliability + tutorial — awaiting Society feedback — added 2026-07-16 — `legacy/ROADMAP.md` Phase 1
- Norms & visualization (PID-5 norming functions, scored-profile plots) — awaiting normative data — added 2026-07-16 — `legacy/ROADMAP.md` Phase 3
- Clinical reporting & release (individual reports, bass-ackwards analyses, CRAN submission + package paper) — added 2026-07-16 — `legacy/ROADMAP.md` Phase 4
- Someday-maybe cluster (Shiny scoring app stub `inst/shiny/app.R`, plotting helpers `devel/plotting.R`, HiTOP-SR/BR validity scales) — added 2026-07-16 — `legacy/ROADMAP.md`
- M17 follow-ups: import `@details` for `generate_redcap_pid5sf`/`pid5bf`; purge git-tracked `vignettes/.quarto/_freeze/` (stale HiTOP-PRO content) — added 2026-07-16 — `legacy/LOG.md` (2026-07-10 M17 close-out)
