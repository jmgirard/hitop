# Milestones

<!-- Status vocabulary (exactly): PLANNED | READY | IN PROGRESS | BLOCKED | DONE | DROPPED -->
<!-- PLANNED = idea captured. READY = has tasks + acceptance criteria. DONE requires all acceptance boxes checked (verified by /review-milestone). -->
<!-- IDs are M<n>, monotonically increasing, never reused. Keep at most one milestone IN PROGRESS. -->
<!-- Managed by skills (/plan-milestone, /work-milestone, /review-milestone); humans may edit freely. -->
<!-- Boundary rule: Architecture → DESIGN. Direction → ROADMAP. Tasks → MILESTONES. History → LOG. -->
<!-- Numbering starts fresh at M1 for this repo (2026-07-09); the old fork's M1–M8 do not exist here. -->

## Active

### M1: Keying-verification tests (port test-keying.R)

- **Status:** IN PROGRESS
- **Depends on:** —
- **Goal:** `pid_items` FULL/SF keying is machine-verified against the authoritative published sources (the external oracle from D-005), adapted to this repo's `FULL/SF/BF` column names.
- **Acceptance criteria:**
  - [ ] `tests/testthat/test-keying.R` exists and `Rscript -e 'devtools::test(filter="keying")'` runs with **0 failures** (all 11 source `test_that` blocks port over: 10 active + the OQ-1 skip; 111 passing assertions)
  - [ ] Assertions cover, each with expected values transcribed from the cited source (never from `pid_items.csv`): 16 reverse items; all 25 facet→item memberships; INC 20 pairs (Keeley 2016 Table 1); INC-S 10 pairs (Lowmaster 2021 Correction Table 1) **and** the Lowmaster-2020 "10 pairs, each a Keeley pair present in SF" block; ORS 10 items (Sellbom 2018); PRD 22 items (Williams 2019); SDTD 16-item subset (Williams 2019 Table 5 note); SF 100-item selection + per-facet 4-item assignments (Maples 2015) + the FSF-carries-no-reverse-items check
  - [ ] OQ-1 (SDTD item 38) remains an explicit `skip()` pointing to SOURCES.md OQ-1; `pid_items` is **unchanged** (no keying edits)
- **Tasks:**
  - [x] Cut branch `m1-keying-tests` from up-to-date main
  - [x] Ensure the test toolchain runs: install local dev deps `flextable`, `officer`, `snakecase` if missing (`load_all` needs the package's Imports available; harmless env setup, overlaps M3)
  - [x] Port `/Users/jmgirard/hitop/tests/testthat/test-keying.R` → `tests/testthat/test-keying.R`, substituting `pid_items$PID5→FULL` and `pid_items$PID5FSF→SF` throughout; reference `pid_items` directly (exported lazy data, no `utils::data()` needed). Converted tibble-unsafe `pid_items[rows, "col"]` subsets to `pid_items$FULL[rows]` vector extraction.
  - [x] Domain→facet blocks: **none existed in the source file** to delete (the fork's domain assertions lived in its `R/pid5.R`-based tests, not `test-keying.R`). Confirmed domain verification correctly belongs to M7: `score_pid5(version="FULL"/"SF")` outputs 25 facets and no domains ([R/score_pid5.R:118](../R/score_pid5.R)); the `pid_items$Domain` column is the *BF* structure, verified in M6. Header comment documents this.
  - [x] Re-check each hardcoded source item list against SOURCES.md's verification table while porting (SOURCES marks all ✅; no silent edits — `pid_items` untouched). Independently confirmed counts: 16 reverse, 100 SF, 25 facets, 20 INC pairs, 10 INC-S, 10 ORS, 22 PRD, 17 SDTD.
  - [x] Run `Rscript -e 'devtools::test(filter="keying")'`; confirm 0 failures, 1 skip (OQ-1) → **FAIL 0 | SKIP 1 | PASS 111**
  - [ ] Push branch, open PR, record URL in Notes/links
- **Notes/links:** SOURCES.md "Canonical-repo note" + OQ-1/OQ-2; D-005, D-006. Full `devtools::check()` clean is **M3's** deliverable, not M1's. Never edit `pid_items` without maintainer sign-off. APA full-form scoring key (Krueger et al., 2013) confirmed the reverse items (Step 1) and 25-facet Facet Table (Step 2) from the primary source on 2026-07-09. **PR:** https://github.com/jmgirard/hitop/pull/2

### M2: Port PID-5 scoring/validity oracle tests

- **Status:** PLANNED
- **Depends on:** M1
- **Goal:** `score_pid5()` and `validity_pid5()` (all three versions) are covered by ground-truth oracle tests per the D-004 strategy.
- **Acceptance criteria:**
  - [ ] Hand-computed fixture tests assert exact scale and validity values for FULL and SF (arithmetic derivable from test comments); BF covered at least by fixtures or independent recomputation
  - [ ] ≥ 1 scale per function independently recomputed in-test from hardcoded official item numbers
  - [ ] Invariant tests pass: reverse-keying applied, row count preserved, `prefix`/`append`/`tibble`/`calc_se` behavior, validity columns per version (PNA only for BF)
  - [ ] `Rscript -e 'devtools::test()'` 0 failures; `Rscript -e 'devtools::check()'` clean
- **Tasks:**
  - [ ] Port from `/Users/jmgirard/hitop/tests/testthat/`: `test-score_pid5.R`, `test-validity_pid5.R`, `test-score_pid5fsf.R`, `test-validity_pid5fsf.R`, `test-interface.R`, `test-util.R`, `test-validate.R`, `helper-fixtures.R`
  - [ ] Adapt to this repo's API: single functions with `version = "FULL"/"SF"/"BF"` (no `score_pid5fsf`), no `scales`/`id` args (all validity scales computed unconditionally), `srange` not `range`, `prefix`-based column names, `append = TRUE`/`tibble = TRUE` defaults, reliability-augmented output (`alpha`/`omega` print a summary)
  - [ ] Adapt dataset names: `sim_pid5sf`/`sim_pid5bf`/`ku_pid5sf` (not `sim_pid5fsf`/`ku_pid5fsf`)
  - [ ] Drop the old fork's SDTD-guard-bug regression tests (the bug does not exist here — no `scales` argument)
  - [ ] Add oracle tests for `calc_alpha()`/`calc_omega()` if not covered (else split into M5)
- **Notes/links:** Oracle strategy: DESIGN.md "Testing & oracle strategy"; D-004, D-006.

### M3: Check & dependency hygiene

- **Status:** PLANNED
- **Depends on:** —
- **Goal:** `devtools::check()` runs clean so CI (M4) can gate on it.
- **Acceptance criteria:**
  - [ ] `Rscript -e 'devtools::check()'` — 0 errors, 0 warnings
  - [ ] Every dataset in `data/` is documented in `R/data.R` or removed; no object exists in both `data/` and `R/sysdata.rda`
  - [ ] All `pkg::fun()` uses are declared in DESCRIPTION; no Imports without uses in R/
- **Tasks:**
  - [ ] Install missing local dev deps (flextable, officer, snakecase) so test/check can run at all
  - [ ] Resolve the 6 undocumented `data/*.rda` (4 × `*_instructions` duplicated in sysdata since commit 08e3d88, plus `hitophsum_choices`, `hitophsum_items`): document or drop from `data/` — confirm which copy the generators actually use before deleting
  - [ ] Make `R/sysdata.rda` regenerable: switch the instructions `use_data()` calls in `data-raw/*_info.R` (e.g. data-raw/pid_info.R:73) to `internal = TRUE` once the data/ copies are dropped
  - [ ] DESCRIPTION: declare {httr2} (or move R/qualtrics_test.R to devel/); remove or justify unused Imports {glue}, {lifecycle}, {jsonlite}
  - [ ] Replace redundant `utils::data()` calls with direct lazy-data references (R/score_pid5.R:98, R/score_hitopsr.R:67 and :201, R/score_hitopbr.R:53)
  - [ ] Fix SDTD percentage denominators (R/validity_pid5.R:172, :177); remove stale `vignettes/bhitop_scoring_files/` and `vignettes/hitoppro_scoring_files/`
- **Notes/links:** DESIGN Known issues #3–#5, #8.

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

*(none yet)*
