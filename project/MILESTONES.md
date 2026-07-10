# Milestones

<!-- Status vocabulary (exactly): PLANNED | READY | IN PROGRESS | BLOCKED | DONE | DROPPED -->
<!-- PLANNED = idea captured. READY = has tasks + acceptance criteria. DONE requires all acceptance boxes checked (verified by /review-milestone). -->
<!-- IDs are M<n>, monotonically increasing, never reused. Keep at most one milestone IN PROGRESS. -->
<!-- Managed by skills (/plan-milestone, /work-milestone, /review-milestone); humans may edit freely. -->
<!-- Boundary rule: Architecture → DESIGN. Direction → ROADMAP. Tasks → MILESTONES. History → LOG. -->
<!-- Numbering starts fresh at M1 for this repo (2026-07-09); the old fork's M1–M8 do not exist here. -->

## Active

### M3: Check & dependency hygiene

- **Status:** IN PROGRESS
- **Depends on:** —
- **Goal:** `devtools::check()` runs clean (0 errors/warnings, no undocumented-data or unused-import NOTEs) so CI (M4) can gate on it.
- **Acceptance criteria:**
  - [ ] `Rscript -e 'devtools::check()'` — 0 errors, 0 warnings; no "Undocumented data sets" or "Namespaces in Imports field not imported from" NOTEs
  - [ ] `Rscript -e 'devtools::test()'` passes
  - [ ] Every object in `data/` is documented in `R/data.R`; no object exists in both `data/` and `R/sysdata.rda` (verified programmatically)
  - [ ] `R/sysdata.rda` is regenerable from `data-raw/` (instructions written with `internal = TRUE`)
  - [ ] All `pkg::fun()` uses declared in DESCRIPTION; `glue`/`lifecycle`/`jsonlite` removed from Imports (or justified); no `httr2::` in `R/`
  - [ ] `validity_pid5(version = "FULL")` returns correct values on single-row input (regression test), and SDTD warning percentages use the SDTD denominator
- **Tasks:**
  1. [x] Install missing local dev deps (`flextable`, `officer`, `snakecase`, `lavaan`) so check/test run at all; capture a baseline `check()` NOTE/WARNING list. *(All deps already installed. Baseline: 0 errors, 4 warnings, 2 notes — see discovered subtasks below.)*
  2. [x] **Datasets:** document `hitophsum_choices` + `hitophsum_items` in `R/data.R` (registry item data used by `R/generate_redcap.R`); confirm the `data/` and `sysdata.rda` copies of the 4 `*_instructions` are byte-identical, then drop them from `data/` and switch the four `usethis::use_data(..._instructions)` calls (`data-raw/{pid,hitopsr,hitopbr,hitophsum}_info.R`) to a single `internal = TRUE` sysdata write; re-run the data-raw scripts to regenerate `R/sysdata.rda`; verify generators still resolve `*_instructions` from the internal copy.
  3. [x] **Dependencies:** remove `@importFrom glue glue` and `@importFrom lifecycle deprecated` from R/hitop-package.R:6-7; drop `glue`, `lifecycle`, `jsonlite` from DESCRIPTION Imports; move `R/qualtrics_test.R` → `devel/` (removes the undeclared `httr2` use from the check surface); `devtools::document()` to regenerate NAMESPACE.
  4. [x] **Redundant `utils::data()`:** remove all 8 calls (R/score_pid5.R:98, R/score_hitopsr.R:67 and :201, R/score_hitopbr.R:53, R/label_hitopsr.R:27 and :47, R/label_hitopbr.R:28 and :46), relying on lazy data as `validity_pid5()` already does.
  5. [x] **Bug fixes (test-first):** add `drop = FALSE` at R/validity_pid5.R:128, :147, :166 (#9) with a hand-computed single-row FULL fixture (reuse `helper-fixtures.R`) asserting exact ORS/PRD/SDTD values instead of erroring; fix the SDTD denominator `length(prd_vec)` → `length(sdtd_vec)` at :172, :177 with an expectation on the warning-percentage wording.
  6. [x] Remove stale local vignette artifact dirs (`vignettes/bhitop_scoring_files/`, `vignettes/hitoppro_scoring_files/`; untracked, local cleanup only).
  7. [x] *(discovered)* Fix the non-ASCII WARNING: 18 `•` (U+2022) bullets in HSUM lists in R/generate_docx.R → `•` escapes.
  8. [x] *(discovered)* Fix the undocumented-arguments WARNING: add `@inheritParams` to the pid5sf/pid5bf variants of `generate_{docx,qualtrics,redcap}_*` (6 Rd files) from their FULL counterparts.
  9. [x] *(discovered)* Silence the no-visible-binding NOTE: add `utils::globalVariables()` for the lazy-data object names and `@importFrom stats setNames` / `@importFrom utils write.csv` in R/hitop-package.R.
  10. [x] *(discovered)* `.Rbuildignore` the top-level `hitop_hex.png` (non-standard-file NOTE).
  11. [x] `document()` → `test()` → `check()`; confirm acceptance criteria. *(check: 0 errors / 0 warnings / 0 notes; test: FAIL 0 WARN 0 SKIP 1 PASS 259.)*
- **Notes/links:** DESIGN Known issues #3, #4, #5, #8, #9. Oracle (per tracking-rules 3b): the single-row `drop=FALSE` fix carries a hand-computed 1-row fixture; the SDTD denominator fix carries a message-wording expectation. Branch `m3-check-hygiene`; PR URL to be recorded here.

### M4: R CMD check + coverage CI

- **Status:** PLANNED
- **Depends on:** M3 (check must pass first; M1–M2 make coverage meaningful)
- **Goal:** Every push/PR to main runs R CMD check and uploads coverage, with README badges.
- **Acceptance criteria:**
  - [ ] `R-CMD-check` and `test-coverage` workflows green on main (alongside the existing pkgdown.yaml)
  - [ ] Check + coverage badges render in README next to the lifecycle badge
- **Tasks:**
  - [ ] `usethis::use_github_action("check-standard")` and `usethis::use_github_action("test-coverage")` + `usethis::use_coverage("codecov")`
  - [ ] Add badges to README.Rmd's badges block; re-knit README.md
- **Notes/links:** Only `.github/workflows/pkgdown.yaml` exists today (DESIGN Known issue #2).

### M5: HiTOP-SR/BR scoring oracle tests

- **Status:** PLANNED
- **Depends on:** M2 (reuse fixture patterns)
- **Goal:** `score_hitopsr()`, `score_hitopbr()`, and the label/rank utilities have ground-truth tests, not just the existing rename/pipeline tests.
- **Acceptance criteria:**
  - [ ] Hand-computed fixtures assert exact scale scores for at least a subset of SR and BR scales; ≥ 1 scale per function independently recomputed from `hitopsr_items`/`hitopbr_items` source data
  - [ ] `calc_alpha()`/`calc_omega()` verified against independently computed reference values (if not already done in M2)
  - [ ] `devtools::check()` clean
- **Tasks:**
  - [ ] Fixtures + invariants (reverse-keying, `_se` columns, prefix behavior) for both instruments
  - [ ] Tests for `label_hitopsr()`/`label_hitopbr()`/`rank_scales()` beyond the current pipeline test
- **Notes/links:** Existing tests: tests/testthat/test-rename_hitopsr_items.R, test-test-pipeline_hitopsr.R.

### M6: BF keying provenance + tests

- **Status:** PLANNED
- **Depends on:** M1
- **Goal:** The `pid_items$BF` selection and `Domain` column (5 domains × 5 items) are documented in SOURCES.md against the primary source and machine-verified, closing the gap that BF keying is currently undocumented and untested.
- **Acceptance criteria (draft — refine via /plan-milestone):**
  - [ ] SOURCES.md gains a BF row/section citing the primary source, with verification status
  - [ ] `tests/testthat/test-keying.R` (or a BF block) asserts the 5 domains × 5 BF items against the APA BF Domain Scoring table, transcribed from the source
  - [ ] `pid_items` unchanged unless a genuine discrepancy is found (then maintainer sign-off)
- **Notes/links:** Primary source = APA *The Personality Inventory for DSM-5—Brief Form (PID-5-BF)—Adult* (Krueger, Derringer, Markon, Watson, & Skodol, 2013; APA "emerging measures"). Domain Scoring table (BF item numbers): Negative Affect = 8,9,10,11,15; Detachment = 4,13,14,16,18; Antagonism = 17,19,20,22,25; Disinhibition = 1,2,3,5,6; Psychoticism = 7,12,21,23,24. Spot-checked 2026-07-09: BF item 1 (Risk Taking) and 2 (Impulsivity) → Disinhibition match `pid_items`. Secondary/validation: Anderson, Sellbom, & Salekin (2018), *Assessment, 25*(5), 596–607, doi:10.1177/1073191116676889. BF averages 5 items/domain — a different algorithm from FULL/SF (see M7).

### M7: PID-5 FULL/SF domain scoring

- **Status:** PLANNED
- **Depends on:** M1
- **Goal:** `score_pid5(version="FULL"/"SF")` also returns the 5 personality-trait *domain* average scores (per the APA key Step 3), and the domain→primary-facet map is stored as data and machine-verified against the APA Domain Table.
- **Acceptance criteria (draft — refine via /plan-milestone):**
  - [ ] FULL/SF output includes 5 domain columns (`prefix` + camelCase domain name) alongside the 25 facets
  - [ ] Domain average = mean of the 3 primary facets' average facet scores (APA Step 3); missing-data rule honored ("domain not computed if any of its 3 contributing facets is missing")
  - [ ] The 5×3 primary-facet map is stored as data (not hardcoded in a function) and a keying test verifies it against the APA Domain Table
  - [ ] `devtools::check()` clean; oracle fixture asserts an exact domain value hand-computed from the published algorithm
- **Notes/links:** APA full-form scoring key (Krueger et al., 2013), Step 3 Domain Table + instructions ("average domain scores are calculated by summing and then averaging the 3 facet scores contributing primarily to a specific domain"). Primary-facet map (verified from source 2026-07-09): Negative Affect = Emotional Lability, Anxiousness, Separation Insecurity; Detachment = Withdrawal, Anhedonia, Intimacy Avoidance; Antagonism = Manipulativeness, Deceitfulness, Grandiosity; Disinhibition = Irresponsibility, Impulsivity, Distractibility; Psychoticism = Unusual Beliefs & Experiences, Eccentricity, Perceptual Dysregulation. This is a *feature* gap (FULL/SF currently score no domains) that carries the domain→facet oracle test the old fork's test-keying.R had. Architectural: adding domain output touches `score_pid5()` and likely `pid_scales` — read DESIGN.md before implementing; may warrant a D-entry.

## Completed

<!-- DONE entries move here as: ### M<n>: Title — DONE YYYY-MM-DD. One-line outcome. -->

### M2: Port PID-5 scoring/validity oracle tests — DONE 2026-07-09. Added `helper-fixtures.R` + 5 test files (FULL/SF facets, BF domains, all validity scales) with hand-computed fixtures, hardcoded-number independent recomputation, and invariants; net-new BF fixtures. Fixed an SE-column prefix bug in `score_pid5()` surfaced by the `calc_se` invariant. Full suite FAIL 0 WARN 0 SKIP 1 PASS 249; `check()` gained no new problems. Single-row `validity_pid5()` bug recorded as DESIGN #9 (→ M3). PR [#3](https://github.com/jmgirard/hitop/pull/3).

### M1: Keying-verification tests (port test-keying.R) — DONE 2026-07-09. Ported `test-keying.R` (11 blocks / 111 assertions) verifying `pid_items` FULL/SF keying against the published sources; FAIL 0 SKIP 1 PASS 111, OQ-1 skipped, `pid_items` unchanged. PR [#2](https://github.com/jmgirard/hitop/pull/2).
