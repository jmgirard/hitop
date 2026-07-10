# Milestones

<!-- Status vocabulary (exactly): PLANNED | READY | IN PROGRESS | BLOCKED | DONE | DROPPED -->
<!-- PLANNED = idea captured. READY = has tasks + acceptance criteria. DONE requires all acceptance boxes checked (verified by /review-milestone). -->
<!-- IDs are M<n>, monotonically increasing, never reused. Keep at most one milestone IN PROGRESS. -->
<!-- Managed by skills (/plan-milestone, /work-milestone, /review-milestone); humans may edit freely. -->
<!-- Boundary rule: Architecture → DESIGN. Direction → ROADMAP. Tasks → MILESTONES. History → LOG. -->
<!-- Numbering starts fresh at M1 for this repo (2026-07-09); the old fork's M1–M8 do not exist here. -->

## Active

### M4: R CMD check + coverage CI

- **Status:** IN PROGRESS
- **Depends on:** M3 (check must pass first; M1–M2 make coverage meaningful)
- **Goal:** Every push/PR to main runs R CMD check and uploads coverage, with check + coverage badges in the README.
- **Acceptance criteria:**
  - [ ] `R-CMD-check.yaml` and `test-coverage.yaml` exist in `.github/workflows/` (alongside `pkgdown.yaml`) and both report green on main (verify via `gh run list`)
  - [ ] R-CMD-check job is green on all matrix platforms it defines (no new ERRORs/WARNINGs; NOTEs acceptable only if pre-existing)
  - [ ] Check + coverage badges render in `README.md` inside the `<!-- badges: start -->` block, next to the lifecycle badge; `README.md` is regenerated from `README.Rmd`, not hand-edited
  - [ ] `covr` added to DESCRIPTION `Suggests`; `devtools::check()` still clean locally (0/0/0)
- **Tasks:**
  - [x] `usethis::use_github_action("check-standard")` → `.github/workflows/R-CMD-check.yaml` (keep the full standard matrix: macOS, Windows, Ubuntu release/devel/oldrel)
  - [x] `usethis::use_github_action("test-coverage")` → `.github/workflows/test-coverage.yaml`; `usethis::use_coverage("codecov")` created `codecov.yml` + `.Rbuildignore` entry (its README badge step errored on this repo's single-line badges block, so `covr` was added to Suggests by hand)
  - [x] Add the two badges (R-CMD-check status + codecov) to the badges block in README.Rmd:17; `Rscript -e 'devtools::build_readme()'` to re-knit
  - [x] Confirm the new workflow files are covered by the existing `^\.github$` `.Rbuildignore` entry (they are — no new entry needed; `codecov.yml` got its own `^codecov\.yml$` entry from `use_coverage()`)
  - [ ] Push branch, open PR, watch both workflows run green on the PR before merge
- **Notes/links:** Only `.github/workflows/pkgdown.yaml` exists today (DESIGN Known issue #2). Repo is public; `main` is the default branch. `CODECOV_TOKEN` secret already added by Jeff (2026-07-09), so the coverage upload should authenticate on first run.

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

### M3: Check & dependency hygiene — DONE 2026-07-09. Took `devtools::check()` from 0E/4W/2N to **0/0/0**: documented the 2 HiTOP-HSUM datasets and consolidated the 4 `*_instructions` into a regenerable `data-raw/sysdata.R` (`internal = TRUE`, D-007) with the `data/` duplicates dropped; removed unused Imports (glue/lifecycle/jsonlite) and moved `qualtrics_test.R` to `devel/` (drops httr2); removed 8 `utils::data()` calls; fixed non-ASCII bullets, generator `@inheritParams`, no-visible-binding globals, `.Rbuildignore`. Bug fix (test-first): `validity_pid5()` no longer errors on single-row FULL/SF input (`drop = FALSE`, DESIGN #9) with hand-computed 1-row oracle tests. Suite FAIL 0 WARN 0 SKIP 1 PASS 259. PR [#4](https://github.com/jmgirard/hitop/pull/4).

### M2: Port PID-5 scoring/validity oracle tests — DONE 2026-07-09. Added `helper-fixtures.R` + 5 test files (FULL/SF facets, BF domains, all validity scales) with hand-computed fixtures, hardcoded-number independent recomputation, and invariants; net-new BF fixtures. Fixed an SE-column prefix bug in `score_pid5()` surfaced by the `calc_se` invariant. Full suite FAIL 0 WARN 0 SKIP 1 PASS 249; `check()` gained no new problems. Single-row `validity_pid5()` bug recorded as DESIGN #9 (→ M3). PR [#3](https://github.com/jmgirard/hitop/pull/3).

### M1: Keying-verification tests (port test-keying.R) — DONE 2026-07-09. Ported `test-keying.R` (11 blocks / 111 assertions) verifying `pid_items` FULL/SF keying against the published sources; FAIL 0 SKIP 1 PASS 111, OQ-1 skipped, `pid_items` unchanged. PR [#2](https://github.com/jmgirard/hitop/pull/2).
