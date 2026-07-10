# Roadmap

<!-- Phases are direction, not commitments. Outcomes are end-states, not tasks; tasks live in MILESTONES.md. -->
<!-- Boundary rule: Architecture → DESIGN. Direction → ROADMAP. Tasks → MILESTONES. History → LOG. -->
<!-- Mirrors the maintainer's "Development Progress" checklist in README.Rmd; keep the two in sync. -->

## Vision

{hitop} is the HiTOP Society's home for its questionnaire instruments in R: a centralized, version-tracked registry of instruments (item data, scoring keys, paper forms, Qualtrics/REDCap exports); reliable tools to score, validity-screen, and reliability-check responses; and, eventually, norms and clinical report generation. It should exemplify modern R package practice — tested against published scoring sources, continuously checked, documented with accessible tutorials.

## Phase 1: Instrument coverage & distribution (largely complete)

- **Theme:** Every current Society instrument has data, scoring, tutorials, and export files.
- **Outcomes:**
  - Item/scale data, scoring, reliability, tutorials, and DOCX/Qualtrics/REDCap exports for HiTOP-SR, HiTOP-BR, PID-5, PID-5-SF, PID-5-BF. *(done)*
  - HiTOP-HSUM scoring + reliability + tutorial. *(waiting on Society feedback)*
  - PID-5-BFP (36-item) data, scoring, and exports. *(todo)*
- **Related milestones:** (to be planned when HSUM feedback / BFP materials arrive)

## Phase 2: Trustworthiness (complete)

- **Theme:** Make the shipped scoring provably correct and keep it that way.
- **Outcomes:**
  - The keying tables are machine-verified against the published sources (ported `test-keying.R`; open questions like OQ-1 stay visible, never silently patched). *(done)*
  - A ground-truth oracle test suite covers PID-5 scoring/validity, HiTOP-SR/BR scoring, and the reliability functions. *(done)*
  - `devtools::check()` is clean (datasets documented, dependencies declared, unused Imports removed) and runs in CI with coverage reporting on every push. *(done)*
  - BF keying provenance is documented and verified against the APA PID-5-BF Domain Scoring table. *(M6, done)*
  - FULL/SF domain scores (APA key Step 3) are computed and their domain→facet map is machine-verified. *(M7, done)*
  - Scoring honors the published APA missing-data/proration rules (opt-out via `apa_scoring = FALSE`). *(M8, done)*
  - Every implemented user-facing surface is tested, including the `generate_{docx,qualtrics,redcap}_*` export family (output parsed back and checked against the source datasets). *(M10, done)*
- **Related milestones:** M1–M10 (all DONE)

## Phase 3: Norms & visualization (current focus)

- **Theme:** Interpretation support for researchers (README Phase 2).
- **Outcomes:**
  - PID-5 normative data and norming functions (percentiles/T-scores).
  - Visualization functions for scored profiles.
- **Related milestones:** (to be planned)

## Phase 4: Clinical reporting & release (README Phase 3)

- **Theme:** Practitioner-facing outputs and public release milestones.
- **Outcomes:**
  - HiTOP normative data and norming functions (waiting for data).
  - Individual report generation; bass-ackwards (and extended) analysis functions.
  - Pre-CRAN API stabilization from the 2026-07 design audit (M15: `reliability_*` family, argument cleanup) lands before submission freezes signatures.
  - CRAN submission and a package paper.
- **Related milestones:** (to be planned)

## Someday-maybe

- Shiny scoring app for non-R users (a stub exists at `inst/shiny/app.R`).
- Plotting helpers (exploration in `devel/plotting.R`).
- HiTOP-SR/BR validity scales (e.g., inconsistency screening; the naive `sim_*` datasets were built with this in mind).
