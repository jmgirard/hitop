# Milestones

<!-- Status vocabulary (exactly): PLANNED | READY | IN PROGRESS | BLOCKED | DONE | DROPPED -->
<!-- PLANNED = idea captured. READY = has tasks + acceptance criteria. DONE requires all acceptance boxes checked (verified by /review-milestone). -->
<!-- IDs are M<n>, monotonically increasing, never reused. Keep at most one milestone IN PROGRESS. -->
<!-- Managed by skills (/plan-milestone, /work-milestone, /review-milestone); humans may edit freely. -->
<!-- Boundary rule: Architecture → DESIGN. Direction → ROADMAP. Tasks → MILESTONES. History → LOG. -->
<!-- Numbering starts fresh at M1 for this repo (2026-07-09); the old fork's M1–M8 do not exist here. -->

## Active

### M7: PID-5 FULL/SF domain scoring

- **Status:** IN PROGRESS
- **Depends on:** M1, M2
- **Goal:** `score_pid5(version="FULL"/"SF")` also returns the 5 personality-trait *domain* average scores (APA key Step 3, each = mean of its 3 primary facet scores), with the domain→primary-facet map stored as a new exported `pid_domains` dataset and machine-verified against the APA Domain Table.
- **Acceptance criteria:**
  - [ ] FULL & SF output gain 5 domain columns — `pid_detachment`, `pid_antagonism`, `pid_disinhibition`, `pid_negativeAffectivity`, `pid_psychoticism` — appended **after** the 25 facet columns (existing facet columns/positions unchanged); BF output unchanged.
  - [ ] Each domain = `rowMeans` of its **3 primary facet** average scores (two-stage mean per APA Step 3), honoring `na.rm` consistently with facet scoring (strict APA ≥2-missing/proration deferred to M8).
  - [ ] With `calc_se=TRUE`, each domain gets a `_se` column computed from its 3 facet scores via the existing `calc_sem` path; alpha/omega reliability printout stays facet-level (domains excluded).
  - [ ] Domain→primary-facet map is stored as exported `pid_domains` data (built in `data-raw/pid_info.R`, not hardcoded in `score_pid5`), documented in `R/data.R`, and listed in `_pkgdown.yml`.
  - [ ] `test-keying.R` gains a FULL/SF domain-map oracle: `pid_domains`' 3 primary facets per domain are set-equal to the hardcoded APA primary-facet map for all 5 domains (mirrors the M6 BF block); stale header comment ("that lives in M7") updated.
  - [ ] Oracle fixture in `test-score_pid5.R` asserts an exact domain value hand-computed from the published algorithm (reverse-keyed facet included) for both FULL and SF; ≥1 domain independently recomputed from hardcoded facet stems.
  - [ ] `Rscript -e 'devtools::document()'` regenerates cleanly; `devtools::test()` passes; `devtools::check()` clean (0/0/0).
- **Tasks:**
  1. [x] **Test-first** — add failing tests: `test-keying.R` FULL/SF primary-facet-map oracle (hardcoded APA sets, set-equal to `pid_domains`); `test-score_pid5.R` domain fixtures (FULL+SF, arithmetic in comments), independent recompute of one domain from hardcoded facet stems, invariants (5 domain cols present + named, domain = mean of its 3 facet cols, row count preserved, BF still 5 domains). Also updated `test-interface.R` column-count assertions (25→30, 50→60).
  2. [x] `data-raw/pid_info.R` — define `pid_domains` (tibble: `Domain`, `camelCase`, `primaryFacets` labels + derived `facetStems`, with an APA-source comment); `usethis::use_data(pid_domains)`; regenerated `data/pid_domains.rda` only (does **not** touch `pid_items`/`pid_scales`).
  3. [x] `R/score_pid5.R` — after the facet-means block, for FULL/SF compute the 5 domain columns from the facet score columns via `pid_domains`, respecting `na.rm`; appended after facets; domain `_se` in the `calc_se` block. BF path untouched. `pid_domains` added to `globalVariables`.
  4. [x] `R/data.R` — documented the `pid_domains` dataset; `_pkgdown.yml` reference index — added it; `NEWS.md` — noted FULL/SF domain scoring + new dataset; `devtools::document()` wrote `pid_domains.Rd` + `score_pid5.Rd`.
  5. [x] `project/SOURCES.md` — updated the FULL/SF Domain row (now cites `pid_domains`, not the old fork's `R/pid5.R`) + added a "Note on FULL/SF domain scoring" (15-facet primary map, na.rm now, strict APA → M8).
  6. [x] `project/DESIGN.md` — D-008 added (during planning); updated the data-model + scoring-family descriptions and the testing-narrative line; `project/ROADMAP.md` — M7 outcome ticked.
  7. [ ] Run `devtools::document()` → `devtools::test()` → `devtools::check()`; open PR on branch `m7-fullsf-domains`, record URL in Notes/links.
- **Notes/links:** APA full-form scoring key (Krueger et al., 2013) Step 3 Domain Table ("average domain scores are calculated by summing and then averaging the 3 facet scores contributing primarily to a specific domain"). Primary-facet map (verified 2026-07-09): Negative Affectivity = Emotional Lability + Anxiousness + Separation Insecurity; Detachment = Withdrawal + Anhedonia + Intimacy Avoidance; Antagonism = Manipulativeness + Deceitfulness + Grandiosity; Disinhibition = Irresponsibility + Impulsivity + Distractibility; Psychoticism = Unusual Beliefs & Experiences + Eccentricity + Perceptual Dysregulation. All 15 stems confirmed present in `pid_scales[["FULL"]]$camelCase`; the 5 domain camelCase names match BF's existing domain output names. Architectural change → D-008. `pid_items$Domain` (a broad 21-facet DSM-5 grouping) is intentionally **not** the scoring map — domain scores use the 15 primary facets only. Design decisions (2026-07-10): traditional na.rm domain scoring (strict APA → M8); map stored as exported `pid_domains`; domain SE via `calc_sem` over the 3 facet scores; reliability stays facet-level.

### M8: APA-compliant scoring toggle for `score_pid5()`

- **Status:** PLANNED
- **Depends on:** M6 (BF rule sourced); shares design with M7
- **Goal:** Add an `apa_scoring` argument to `score_pid5()` that applies the APA missing-data + proration rules, so scored output honors the published algorithm; a `FALSE` value selects the current `rowMeans(na.rm = TRUE)` ("traditional") behavior.
- **Open items to resolve in `/plan-milestone`:**
  - Argument name/default settled with maintainer: `apa_scoring = TRUE` (APA rules are the **default**; `apa_scoring = FALSE` = current traditional `rowMeans(na.rm=TRUE)`). This changes default output under missing data → needs a **D-entry** and a NEWS.md note.
  - Scope: BF only in v1, or FULL/SF/BF? FULL/SF need their APA missing-data rules pulled from the APA full-form key first (a sourcing task; not yet confirmed in hand).
  - Proration rounding semantics: APA rounds the prorated *raw* domain score to the nearest whole number, then averages — this differs from a plain mean of available items → capture exact algorithm + hand-computed oracle fixtures.
  - Interaction with existing `na.rm`, `calc_se`, `alpha`/`omega` args; and rewriting `tests/testthat/test-score_pid5.R:122-127`, which currently enshrines scoring a domain from a single surviving item (contradicts the APA ≥2-missing rule).
- **Notes/links:** APA BF missing-data/proration rule text captured in M6 Notes + SOURCES.md. Relates to M7's "domain not computed if any contributing facet is missing" rule — plan the two missing-data behaviors coherently.

## Completed

<!-- DONE entries move here as: ### M<n>: Title — DONE YYYY-MM-DD. One-line outcome. -->

### M6: BF keying provenance + tests — DONE 2026-07-10. Documented and machine-verified the PID-5-BF domain keying against the APA primary source, closing the gap that BF keying was undocumented and only 2/5 domains were checked (transitively, via `score_pid5`). Pulled the APA *PID-5-BF—Adult* PDF and transcribed its "Personality Trait Domain Scoring" table; `pid_items$Domain` for the 25 BF items matches exactly. New `test-keying.R` BF block: domain-membership oracle asserting `pid_items$BF[Domain==d]` set-equal to the hardcoded APA numbers for all 5 domains, plus invariants (25 items / complete 1:25 / 5-per-domain / no reverse). SOURCES.md gained 3 BF verification rows, a PID-5-BF source bullet + APA PDF URL, and a "Note on BF domain scoring" (all-forward, avg=raw÷5, and the unenforced APA missing-data/proration rule → M8). `pid_items` untouched. Suite PASS 329 (was 320); `check()` **0/0/0**; fresh-context review PASS (re-confirmed the APA numbers from the primary PDF and proved the oracle non-tautological), no blockers. PR [#7](https://github.com/jmgirard/hitop/pull/7).
  - [x] SOURCES.md verification-table BF rows + Sources bullet (APA PDF URL) + "Note on BF domain scoring" (all-forward, avg=raw/5, unenforced APA missing-data/proration rule flagged → M8)
  - [x] `test-keying.R` BF block: all 5 domains' `pid_items$BF`/`Domain` membership set-equal to hardcoded APA numbers; invariants (25 items / 1:25 / 5-per-domain / no reverse-keying)
  - [x] Stale `test-keying.R` header comment updated (no longer forward-references M6 as future work)
  - [x] `pid_items` unchanged (`data/pid_items.rda` + `data-raw/pid_items.csv` absent from the diff)
  - [x] `devtools::test()` passes; `devtools::check()` clean (0/0/0)

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
