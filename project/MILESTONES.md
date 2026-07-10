# Milestones

<!-- Status vocabulary (exactly): PLANNED | READY | IN PROGRESS | BLOCKED | DONE | DROPPED -->
<!-- PLANNED = idea captured. READY = has tasks + acceptance criteria. DONE requires all acceptance boxes checked (verified by /review-milestone). -->
<!-- IDs are M<n>, monotonically increasing, never reused. Keep at most one milestone IN PROGRESS. -->
<!-- Managed by skills (/plan-milestone, /work-milestone, /review-milestone); humans may edit freely. -->
<!-- Boundary rule: Architecture → DESIGN. Direction → ROADMAP. Tasks → MILESTONES. History → LOG. -->
<!-- Numbering starts fresh at M1 for this repo (2026-07-09); the old fork's M1–M8 do not exist here. -->

## Active

### M1: Keying-verification tests (port test-keying.R)

- **Status:** PLANNED
- **Depends on:** —
- **Goal:** `pid_items` is machine-verified against the authoritative published sources (the external oracle from D-005), adapted to this repo's column names.
- **Acceptance criteria:**
  - [ ] `tests/testthat/test-keying.R` exists and `Rscript -e 'devtools::test()'` runs with 0 failures
  - [ ] Assertions cover: 16 reverse items, all 25 facet memberships, 5 domain→facet mappings, INC 20 pairs, INC-S 10 pairs (Lowmaster 2021 Correction, Table 1), ORS 10 items, PRD 22 items, full SF 100-item selection
  - [ ] OQ-1 (SDTD item 38) remains an explicit `skip()` with a pointer to SOURCES.md; `pid_items` is unchanged
- **Tasks:**
  - [ ] Port `/Users/jmgirard/hitop/tests/testthat/test-keying.R`, substituting columns `PID5→FULL`, `PID5FSF→SF`, `PID5BF→BF` and accessing `pid_items` as this repo stores it (exported lazy data in `data/pid_items.rda`)
  - [ ] Re-check each hardcoded source item list against SOURCES.md while porting (no silent edits)
- **Notes/links:** SOURCES.md "Canonical-repo note" and OQ-1; D-005, D-006. Never edit `pid_items` without maintainer sign-off.

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

## Completed

<!-- DONE entries move here as: ### M<n>: Title — DONE YYYY-MM-DD. One-line outcome. -->

*(none yet)*
