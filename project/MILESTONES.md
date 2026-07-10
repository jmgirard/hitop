# Milestones

<!-- Status vocabulary (exactly): PLANNED | READY | IN PROGRESS | BLOCKED | DONE | DROPPED -->
<!-- PLANNED = idea captured. READY = has tasks + acceptance criteria. DONE requires all acceptance boxes checked (verified by /review-milestone). -->
<!-- IDs are M<n>, monotonically increasing, never reused. Keep at most one milestone IN PROGRESS. -->
<!-- Managed by skills (/plan-milestone, /work-milestone, /review-milestone); humans may edit freely. -->
<!-- Boundary rule: Architecture → DESIGN. Direction → ROADMAP. Tasks → MILESTONES. History → LOG. -->
<!-- Numbering starts fresh at M1 for this repo (2026-07-09); the old fork's M1–M8 do not exist here. -->

## Active

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

### M7: PID-5 FULL/SF domain scoring — DONE 2026-07-10. `score_pid5(version = "FULL"/"SF")` now appends the 5 personality-trait domain scores after the 25 facet scores, each the mean of its 3 primary facet scores (APA key Step 3; honors `na.rm`, domain `_se` via `calc_sem` over the 3 facet scores when `calc_se=TRUE`). Added the exported `pid_domains` dataset (the 5×3 domain→primary-facet map: `Domain`, `camelCase`, `primaryFacets` labels, derived `facetStems`), built in `data-raw/pid_info.R`, documented + pkgdown-listed, and machine-verified against a hardcoded transcription of the APA full-form Domain Table in `test-keying.R`. Crucial distinction encoded: the scoring map is the **15 primary facets**, NOT the broader 21-facet `pid_items$Domain` grouping. New `test-score_pid5.R` domain block (hand-computed FULL R1/R2 + SF R3 fixtures, independent recompute of Psychoticism/Antagonism from hardcoded facet stems, na.rm both ways, BF-unchanged, `_se` iff `calc_se`); `test-interface.R` column counts updated (25→30, 50→60). Traditional na.rm now; strict APA missing-data/proration deferred to M8 (D-008). `pid_items`/`pid_scales` untouched. Suite PASS 365 (was 329); `check()` **0/0/0**; fresh-context review PASS (independently re-derived the fixture values, confirmed the map against the DSM-5 structure, and proved the oracle non-tautological), no blockers. PR [#8](https://github.com/jmgirard/hitop/pull/8).
  - [x] FULL & SF gain 5 domain columns (`pid_negativeAffectivity`/`detachment`/`antagonism`/`disinhibition`/`psychoticism`, names matching BF) appended after the 25 facets; facet positions unchanged; BF output unchanged
  - [x] Each domain = `rowMeans` of its 3 primary facet average scores (two-stage mean, APA Step 3), honoring `na.rm` (drops a fully-missing facet under TRUE; NA under FALSE)
  - [x] `calc_se=TRUE` adds domain `_se` from the 3 facet scores via `calc_sem`; alpha/omega reliability print stays facet-level (domains excluded)
  - [x] Domain→primary-facet map stored as exported `pid_domains` (built in `data-raw/pid_info.R`, not hardcoded), documented in `R/data.R`, listed in `_pkgdown.yml`
  - [x] `test-keying.R` FULL/SF domain-map oracle: `pid_domains` primary facets set-equal to a hardcoded APA map for all 5 domains (non-tautological); stale header comment updated
  - [x] `test-score_pid5.R` asserts hand-computed FULL+SF domain values + ≥1 domain independently recomputed from hardcoded facet stems
  - [x] `devtools::document()` no-diff; `devtools::test()` PASS 365; `devtools::check()` clean (0/0/0)

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
