# Milestones

<!-- Status vocabulary (exactly): PLANNED | READY | IN PROGRESS | BLOCKED | DONE | DROPPED -->
<!-- PLANNED = idea captured. READY = has tasks + acceptance criteria. DONE requires all acceptance boxes checked (verified by /review-milestone). -->
<!-- IDs are M<n>, monotonically increasing, never reused. Keep at most one milestone IN PROGRESS. -->
<!-- Managed by skills (/plan-milestone, /work-milestone, /review-milestone); humans may edit freely. -->
<!-- Boundary rule: Architecture → DESIGN. Direction → ROADMAP. Tasks → MILESTONES. History → LOG. -->
<!-- Numbering starts fresh at M1 for this repo (2026-07-09); the old fork's M1–M8 do not exist here. -->

## Active

### M2: Port PID-5 scoring/validity oracle tests

- **Status:** READY
- **Depends on:** M1
- **Goal:** `score_pid5()` (FULL facets / SF facets / BF domains) and `validity_pid5()` (all three versions) are covered by ground-truth oracle tests per the D-004 strategy.
- **Acceptance criteria:**
  - [ ] Hand-computed fixtures assert exact values with the arithmetic in comments: FULL facets + all 5 validity scales; SF facets + all 5 S-variant validity scales; BF 5 domains + PNA
  - [ ] Independent recomputation from hardcoded official item numbers, ≥ 1 per function per version: a facet/domain for FULL/SF/BF in `score_pid5`; INC (FULL) and INCS (SF) in `validity_pid5`
  - [ ] Invariants pass: FULL reverse-keying applied (a facet with a reverse item ≠ 0 on all-0 input); SF & BF apply no reverse-keying (all-0 → 0); row count preserved; `prefix`/`append`/`tibble`/`calc_se` behavior; NA propagation (score `na.rm = TRUE` tolerates missing, validity → NA); validity column set per version (5 for FULL/SF, `pid_PNA` only for BF); `score_pid5` scale-column count 25/25/5
  - [ ] `Rscript -e 'devtools::test()'` — 0 failures
  - [ ] `R CMD check` gains no new ERROR/WARNING attributable to the added test files (full `check()`-clean is M3's goal, not M2's)
- **Tasks:**
  - [ ] Port `helper-fixtures.R` from `/Users/jmgirard/hitop/tests/testthat/`: keep `fx_pid5()`; rename `fx_pid5fsf()` → `fx_pid5sf()`; add new `fx_pid5bf()` (5 domains × 5 BF items, no reverse — hand-worked in comments). Item→scale numbers copied from the official key, not read from the package
  - [ ] `tests/testthat/test-score_pid5.R` — consolidate the fork's `test-score_pid5.R` + `test-score_pid5fsf.R` into one file with FULL/SF/BF sections. Adapt columns to `pid_<camelCase>` (e.g. `pid_anhedonia`), add `items = 1:N, version =` to every call. Drop the FULL/SF *domain* fixtures and the `d_* = mean(3 facets)` invariant — they move to M7; leave a pointer comment (mirroring `tests/testthat/test-keying.R:11`)
  - [ ] `tests/testthat/test-validity_pid5.R` — consolidate the fork's `test-validity_pid5.R` + `test-validity_pid5fsf.R`. All scales compute unconditionally, so DROP the "each scale alone", "column iff requested", and SDTD-guard-bug regression tests. Keep the PNA/INC/ORS/PRD/SDTD (+ S-variant) fixture values, NA-propagation, and cutoff-warning (`expect_message`) tests; adapt columns to `pid_INC`/`pid_INCS`/etc. Add a BF case asserting the output column set is exactly `pid_PNA`
  - [ ] `tests/testthat/test-util.R` — port `reverse`/`bind_columns`/`adiff`/`drop_na`; adapt `reverse()` to the current `low=`/`high=` signature (not `min`/`max`); add coverage for `calc_sem()` and `cli_assert()`
  - [ ] `tests/testthat/test-validate.R` — port `validate_data`/`validate_items`/`validate_range`; DROP all `validate_id` tests (no `id` in this API); adapt end-to-end calls to `srange =` and the required `items =` argument
  - [ ] `tests/testthat/test-interface.R` — interface invariants on `sim_pid5`/`sim_pid5sf`/`sim_pid5bf`/`ku_pid5sf`. DROP the `scales`/`id`/partial-matching/domain tests; add `append =`, `prefix =`, `version` case-insensitivity, and per-version scale-column-count checks. Add a smoke check that `alpha = TRUE`/`omega = TRUE` prints a summary without error and returns unchanged scores (ground-truth reliability values deferred to M5)
  - [ ] Local dev-env step: ensure {flextable}/{officer}/{snakecase} are installed so `devtools::test()` can load the package (full dependency hygiene stays in M3)
  - [ ] Run `Rscript -e 'devtools::test()'`; iterate to 0 failures
- **Notes/links:** Oracle strategy: DESIGN.md "Testing & oracle strategy"; D-004, D-006. Current `score_pid5()` outputs 25 facets for FULL/SF and 5 domains for BF — no FULL/SF domains, so FULL/SF domain scoring + its domain→facet oracle live in **M7**; BF domain→item structure is verified in **M6**; reliability oracle tests in **M5**. `validity_pid5` has no `scales` argument, so the fork's guard-bug (SDTD) does not exist here.

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

### M1: Keying-verification tests (port test-keying.R) — DONE 2026-07-09. Ported `test-keying.R` (11 blocks / 111 assertions) verifying `pid_items` FULL/SF keying against the published sources; FAIL 0 SKIP 1 PASS 111, OQ-1 skipped, `pid_items` unchanged. PR [#2](https://github.com/jmgirard/hitop/pull/2).
