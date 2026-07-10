# Milestones

<!-- Status vocabulary (exactly): PLANNED | READY | IN PROGRESS | BLOCKED | DONE | DROPPED -->
<!-- PLANNED = idea captured. READY = has tasks + acceptance criteria. DONE requires all acceptance boxes checked (verified by /review-milestone). -->
<!-- IDs are M<n>, monotonically increasing, never reused. Keep at most one milestone IN PROGRESS. -->
<!-- Managed by skills (/plan-milestone, /work-milestone, /review-milestone); humans may edit freely. -->
<!-- Boundary rule: Architecture → DESIGN. Direction → ROADMAP. Tasks → MILESTONES. History → LOG. -->
<!-- Numbering starts fresh at M1 for this repo (2026-07-09); the old fork's M1–M8 do not exist here. -->

## Active

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

### M5: HiTOP-SR/BR scoring + reliability oracle tests — DONE 2026-07-10. Added ground-truth oracle tests for the previously-untested HiTOP-SR/BR scoring and reliability functions: `fx_hitopsr()`/`fx_hitopbr()` hand-computed fixtures (arithmetic in comments, memberships copied from source CSVs), `test-score_hitopsr.R` (incl. the lone reverse item HSR 310, independent recompute of `appetiteLoss`), `test-score_hitopbr.R` (overlapping `externalizing`/`pFactor` recomputed from the marker columns), `test-reliability.R` (`calc_alpha` vs 42/45 + error paths; `calc_omega` vs a direct independent `lavaan::cfa` fit + warning), `test-label_scales.R` (`label_*` + `rank_scales`). Hardened `score_hitopbr()` with `drop = FALSE` (behavior-preserving). Suite FAIL 0 WARN 0 SKIP 1 PASS 320 (was 259); `check()` **0/0/0**; fresh-context review PASS-WITH-NITS, no blockers. Closed the reliability + SR/BR half of DESIGN Known issue #1. PR [#6](https://github.com/jmgirard/hitop/pull/6).
  - [x] Hand-computed fixtures assert exact SR/BR scale scores (reverse item HSR 310 included); all values independently re-derived at review, none back-filled
  - [x] ≥ 1 scale per function independently recomputed from source (BR `externalizing`+`pFactor` from marker columns; SR `appetiteLoss` from hardcoded numbers) — confirmed non-circular
  - [x] Invariants: reverse-keying (SR) / no reversal (BR), `_se` iff `calc_se`, prefix on every scale, row count, scale = member row-mean
  - [x] `calc_alpha()` vs 42/45 + error paths (`k<2`, `n<2`, zero variance); `calc_omega()` vs direct `lavaan::cfa` (`skip_if_not_installed`) + mixed-sign warning
  - [x] `label_hitopsr`/`label_hitopbr` (items+scales+no-match) and `rank_scales` (`dir`/`top`/prefix/tie) dedicated tests
  - [x] `devtools::test()` passes; `devtools::check()` clean (0/0/0)

### M4: R CMD check + coverage CI — DONE 2026-07-10. Added `R-CMD-check.yaml` (full standard matrix: macOS/Windows/Ubuntu devel-release-oldrel) and `test-coverage.yaml` (covr → Cobertura → codecov-action@v7) workflows via `usethis`, plus `codecov.yml` and its `.Rbuildignore` entry; added `covr` to Suggests (usethis aborted its README badge step on the single-line badges block) and R-CMD-check + Codecov badges to `README.Rmd`, re-knit. All 7 checks green on PR [#5](https://github.com/jmgirard/hitop/pull/5) (5-platform matrix + coverage + pkgdown) and on the post-merge `main` push; local `devtools::check()` clean (0/0/0); fresh-context review PASS, no blockers. Resolved DESIGN Known issue #2.
  - [x] `R-CMD-check.yaml` and `test-coverage.yaml` exist alongside `pkgdown.yaml` and both green on main
  - [x] R-CMD-check green on all 5 matrix platforms (no ERRORs/WARNINGs)
  - [x] Check + coverage badges render in `README.md` badges block, knitted from `README.Rmd`
  - [x] `covr` in DESCRIPTION `Suggests`; `devtools::check()` clean (0/0/0)

### M3: Check & dependency hygiene — DONE 2026-07-09. Took `devtools::check()` from 0E/4W/2N to **0/0/0**: documented the 2 HiTOP-HSUM datasets and consolidated the 4 `*_instructions` into a regenerable `data-raw/sysdata.R` (`internal = TRUE`, D-007) with the `data/` duplicates dropped; removed unused Imports (glue/lifecycle/jsonlite) and moved `qualtrics_test.R` to `devel/` (drops httr2); removed 8 `utils::data()` calls; fixed non-ASCII bullets, generator `@inheritParams`, no-visible-binding globals, `.Rbuildignore`. Bug fix (test-first): `validity_pid5()` no longer errors on single-row FULL/SF input (`drop = FALSE`, DESIGN #9) with hand-computed 1-row oracle tests. Suite FAIL 0 WARN 0 SKIP 1 PASS 259. PR [#4](https://github.com/jmgirard/hitop/pull/4).

### M2: Port PID-5 scoring/validity oracle tests — DONE 2026-07-09. Added `helper-fixtures.R` + 5 test files (FULL/SF facets, BF domains, all validity scales) with hand-computed fixtures, hardcoded-number independent recomputation, and invariants; net-new BF fixtures. Fixed an SE-column prefix bug in `score_pid5()` surfaced by the `calc_se` invariant. Full suite FAIL 0 WARN 0 SKIP 1 PASS 249; `check()` gained no new problems. Single-row `validity_pid5()` bug recorded as DESIGN #9 (→ M3). PR [#3](https://github.com/jmgirard/hitop/pull/3).

### M1: Keying-verification tests (port test-keying.R) — DONE 2026-07-09. Ported `test-keying.R` (11 blocks / 111 assertions) verifying `pid_items` FULL/SF keying against the published sources; FAIL 0 SKIP 1 PASS 111, OQ-1 skipped, `pid_items` unchanged. PR [#2](https://github.com/jmgirard/hitop/pull/2).
