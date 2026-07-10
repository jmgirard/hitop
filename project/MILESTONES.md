# Milestones

<!-- Status vocabulary (exactly): PLANNED | READY | IN PROGRESS | BLOCKED | DONE | DROPPED -->
<!-- PLANNED = idea captured. READY = has tasks + acceptance criteria. DONE requires all acceptance boxes checked (verified by /review-milestone). -->
<!-- IDs are M<n>, monotonically increasing, never reused. Keep at most one milestone IN PROGRESS. -->
<!-- Managed by skills (/plan-milestone, /work-milestone, /review-milestone); humans may edit freely. -->
<!-- Boundary rule: Architecture → DESIGN. Direction → ROADMAP. Tasks → MILESTONES. History → LOG. -->
<!-- Numbering starts fresh at M1 for this repo (2026-07-09); the old fork's M1–M8 do not exist here. -->

## Active

### M5: HiTOP-SR/BR scoring + reliability oracle tests

- **Status:** IN PROGRESS
- **Depends on:** M2 (reuse fixture patterns)
- **Goal:** `score_hitopsr()`, `score_hitopbr()`, the reliability functions (`calc_alpha()`/`calc_omega()`), and the `label_*`/`rank_scales()` utilities all have ground-truth tests, not just the existing rename/pipeline tests.
- **Acceptance criteria:**
  - [ ] Hand-computed fixtures assert exact scale scores for a representative subset of SR and BR scales, with the arithmetic worked in comments (M2 style); the SR fixture includes the lone reverse-keyed item (HSR 310) to prove reversal
  - [ ] ≥ 1 scale per function independently recomputed from source: BR `externalizing` **and** `pFactor` `itemNumbers` recomputed from the `hitopbr_items$Externalizing`/`$Pfactor` marker columns; ≥ 1 SR scale recomputed from `hitopsr_items$Scale`
  - [ ] Invariants asserted for both: reverse-keying applied (SR) / no reversal (BR), `_se` columns present iff `calc_se = TRUE`, `prefix` applied to every scale column, output row count = input row count, each scale = row mean of its member items
  - [ ] `calc_alpha()` verified against an independent reference value (hand- or `psych`-computed on a tiny fixture) and its assertion-error paths (`k < 2`, `n < 2`, zero variance); `calc_omega()` verified against a direct `lavaan::cfa` fit on the same data (`skip_if_not_installed("lavaan")`), incl. the mixed-sign-loading warning
  - [ ] `label_hitopsr()`/`label_hitopbr()` (items + scales targets, no-match warning) and `rank_scales()` (`dir`, `top`, `prefix`-stripping, tie order) have dedicated tests beyond the pipeline test
  - [ ] `devtools::test()` passes; `devtools::check()` clean (0/0/0)
- **Tasks:**
  1. [x] Add `fx_hitopsr()` + `fx_hitopbr()` to `tests/testthat/helper-fixtures.R`, item→scale memberships copied from source (never read back from the package), arithmetic in comments; SR fixture exercises reverse item 310 (srange `c(1,4)` → `reverse = 5 - x`)
  2. [x] `test-score_hitopsr.R`: fixture exact-value assertions (incl. the reverse scale `romanticDisinterest` + a small n=3 scale e.g. `appetiteLoss` + one larger), one independent recomputation from `hitopsr_items$Scale`, invariants (`_se`, prefix, row count, `na.rm` on a missing cell)
  3. [x] `test-score_hitopbr.R`: fixture exact-value assertions, independent recompute of `externalizing` & `pFactor` from the marker columns, invariants (no reverse-keying, `_se`, prefix, row count)
  4. [x] `test-reliability.R`: `calc_alpha()` vs reference value + assertion-error paths (`k < 2`, `n < 2`, zero variance); `calc_omega()` vs direct `lavaan::cfa` on the same data (guarded by `skip_if_not_installed`), mixed-sign-loading warning
  5. [x] `label_*`/`rank_scales` tests: `label_hitopsr`/`label_hitopbr` item & scale labels attach the correct text, no-match → warning + unchanged data; extend `rank_scales` coverage (`dir = "low"`, custom `top`, prefix strip, tie handling)
  6. [x] Run `devtools::test()` then `devtools::check()`; fix any surfaced bug test-first (added defensive `drop = FALSE` to `R/score_hitopbr.R` mean/se, matching `score_hitopsr`)
- **Notes/links:** Existing tests: `tests/testthat/test-rename_hitopsr_items.R`, `test-test-pipeline_hitopsr.R`. `calc_alpha`/`calc_omega` have **no** tests today (only `calc_sem`, test-util.R:35) — M5 owns them. BR scale overlaps: `externalizing` = {1,13,15,16,25,32,34,35,40,45}, `pFactor` = {1,6,11,14,22,23,25,28,31,32,35,37}. SR reverse item = HSR 310 (romanticDisinterest = 42,152,187,310,338); BR has 0 reverse items. `score_hitopbr()` has no `alpha`/`omega` args (SR does) — reliability tests target `calc_*` directly. `R/score_hitopbr.R:76` `rowMeans(data_items[, x], na.rm = na.rm)` lacks `drop = FALSE` — safe while all BR scales have ≥2 items; assert it or add the guard. Representative-subset scope + reliability-in-M5 per Jeff (2026-07-10). PR [#6](https://github.com/jmgirard/hitop/pull/6).

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

### M4: R CMD check + coverage CI — DONE 2026-07-10. Added `R-CMD-check.yaml` (full standard matrix: macOS/Windows/Ubuntu devel-release-oldrel) and `test-coverage.yaml` (covr → Cobertura → codecov-action@v7) workflows via `usethis`, plus `codecov.yml` and its `.Rbuildignore` entry; added `covr` to Suggests (usethis aborted its README badge step on the single-line badges block) and R-CMD-check + Codecov badges to `README.Rmd`, re-knit. All 7 checks green on PR [#5](https://github.com/jmgirard/hitop/pull/5) (5-platform matrix + coverage + pkgdown) and on the post-merge `main` push; local `devtools::check()` clean (0/0/0); fresh-context review PASS, no blockers. Resolved DESIGN Known issue #2.
  - [x] `R-CMD-check.yaml` and `test-coverage.yaml` exist alongside `pkgdown.yaml` and both green on main
  - [x] R-CMD-check green on all 5 matrix platforms (no ERRORs/WARNINGs)
  - [x] Check + coverage badges render in `README.md` badges block, knitted from `README.Rmd`
  - [x] `covr` in DESCRIPTION `Suggests`; `devtools::check()` clean (0/0/0)

### M3: Check & dependency hygiene — DONE 2026-07-09. Took `devtools::check()` from 0E/4W/2N to **0/0/0**: documented the 2 HiTOP-HSUM datasets and consolidated the 4 `*_instructions` into a regenerable `data-raw/sysdata.R` (`internal = TRUE`, D-007) with the `data/` duplicates dropped; removed unused Imports (glue/lifecycle/jsonlite) and moved `qualtrics_test.R` to `devel/` (drops httr2); removed 8 `utils::data()` calls; fixed non-ASCII bullets, generator `@inheritParams`, no-visible-binding globals, `.Rbuildignore`. Bug fix (test-first): `validity_pid5()` no longer errors on single-row FULL/SF input (`drop = FALSE`, DESIGN #9) with hand-computed 1-row oracle tests. Suite FAIL 0 WARN 0 SKIP 1 PASS 259. PR [#4](https://github.com/jmgirard/hitop/pull/4).

### M2: Port PID-5 scoring/validity oracle tests — DONE 2026-07-09. Added `helper-fixtures.R` + 5 test files (FULL/SF facets, BF domains, all validity scales) with hand-computed fixtures, hardcoded-number independent recomputation, and invariants; net-new BF fixtures. Fixed an SE-column prefix bug in `score_pid5()` surfaced by the `calc_se` invariant. Full suite FAIL 0 WARN 0 SKIP 1 PASS 249; `check()` gained no new problems. Single-row `validity_pid5()` bug recorded as DESIGN #9 (→ M3). PR [#3](https://github.com/jmgirard/hitop/pull/3).

### M1: Keying-verification tests (port test-keying.R) — DONE 2026-07-09. Ported `test-keying.R` (11 blocks / 111 assertions) verifying `pid_items` FULL/SF keying against the published sources; FAIL 0 SKIP 1 PASS 111, OQ-1 skipped, `pid_items` unchanged. PR [#2](https://github.com/jmgirard/hitop/pull/2).
