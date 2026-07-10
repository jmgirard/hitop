# Design

<!-- Describes the package as it IS. Aspirations go to ROADMAP.md. -->
<!-- Boundary rule: Architecture → DESIGN. Direction → ROADMAP. Tasks → MILESTONES. History → LOG. -->

## Purpose & scope

The {hitop} package serves the HiTOP (Hierarchical Taxonomy of Psychopathology) Society with three goals:

1. **Instrument registry:** a centralized place for version-tracking the Society's questionnaire instruments (item text, scoring keys, administration materials, platform-ready exports).
2. **Response processing:** tools for researchers to manage and process responses (scoring, validity screening, reliability estimation).
3. **Clinical reporting:** eventually, norms, interpretation, and rendered individual reports.

Goals 1 and 2 are substantially implemented for four instrument families: **PID-5** (FULL 220 / SF 100 / BF 25 items), **HiTOP-SR** (405 items), **HiTOP-BR** (45 items), and **HiTOP-HSUM** (650 items; data + export only — scoring awaits Society feedback). Goal 3 is future work (ROADMAP Phases 3–4).

## Architecture overview

### Instrument data model

Each instrument is driven by data, not code:

- **`*_items` tables** (`pid_items`, `hitopsr_items`, `hitopbr_items`) — one row per item: item number per form (for PID-5: `FULL`/`SF`/`BF`), `Reverse` keying flag, scale/facet membership, validity-scale membership (`INC`/`INCS`/`ORS`/`ORSS`/`PRD`/`PRDS`/`SDTD`/`SDTDS` for PID-5), and item `Text`. `pid_items` keying is versioned instrument content (D-001); provenance and verification status live in [SOURCES.md](SOURCES.md).
- **`*_scales` tables** (`pid_scales`, `hitopsr_scales`, `hitopbr_scales`, `hitopsr_subscales`) — one row per scale with list-columns `itemdata` and `itemNumbers` plus `nItems` and `camelCase` (the output column stem). Scoring functions iterate `itemNumbers` (e.g., `pid_scales[[version]]$itemNumbers` at [R/score_pid5.R:116](../R/score_pid5.R)); they never hardcode item numbers.
- **`pid_domains` table** — the FULL/SF domain→primary-facet map (5 rows: `Domain`, `camelCase`, `primaryFacets` labels, `facetStems`). `score_pid5()` uses it to compute each domain as the mean of its 3 primary facet scores (APA Step 3, D-008); verified against the APA Domain Table in `test-keying.R`.
- **Internal data** (`R/sysdata.rda`) — administration instructions (`pid_instructions`, `hitopsr_instructions`, `hitopbr_instructions`, `hitophsum_instructions`) used by the generators.
- **Example data** — simulated (`sim_pid5`, `sim_pid5sf`, `sim_pid5bf`, `sim_hitopsr`, `sim_hitopbr`; naive uniform-random responses) and real de-identified KU student data (`ku_pid5sf` n=386, `ku_hitopsr`/`ku_hitopbr` n=411).

Adding a form variant = a new item-number column + `*_scales` entry; adding an instrument = new items/scales tables + a `score_*` (+ `validity_*`) + `generate_*` family.

### Function families

- **Scoring** — `score_pid5()` ([R/score_pid5.R](../R/score_pid5.R)), `score_hitopsr()` ([R/score_hitopsr.R](../R/score_hitopsr.R)), `score_hitopbr()` ([R/score_hitopbr.R](../R/score_hitopbr.R)): reverse-key per the items table, compute per-scale means, optionally add `_se` columns (`calc_se`), optionally print per-scale alpha/omega reliability (all three now expose `alpha`/`omega`). One PID-5 function covers all three forms via `version = c("FULL", "SF", "BF")`; FULL/SF output is 25 facets + 5 domains (domains = mean of 3 primary facets via `pid_domains`, D-008), BF is its 5 domains scored directly from items. The three exported functions are **thin wrappers** over one unexported, data-driven engine `score_engine()` ([R/score_engine.R](../R/score_engine.R), D-011): each wrapper resolves only its instrument data (which items reverse, the per-scale item-number lists, and — FULL/SF — the domain map) and the engine runs the shared validate → reverse → score → SE → reliability → append pipeline. This mirrors the generators' `build_*` pattern; `apa_scoring` and domain scoring are optional engine features PID-5 opts into.
- **Validity** — `validity_pid5()` ([R/validity_pid5.R](../R/validity_pid5.R)): computes PNA (percent missing) for all versions and INC/ORS/PRD/SDTD (S-variants for SF) unconditionally for FULL/SF, with cli warnings at published cutoffs (INC ≥ 17, ORS ≥ 3, PRD ≤ 10, SDTD ≤ 11 / ≥ 19, INC-S ≥ 8; SF ORS-S/PRD-S/SDTD-S cutoffs not yet validated — a runtime warning says so at [R/validity_pid5.R:199](../R/validity_pid5.R)). Because PRD/SDTD are raw sums vs fixed thresholds that assume 0–3 coding, FULL/SF also warn when `srange != c(0, 3)` (M11, Known issue #4). Shared `items` guards (misorder heuristic, duplicate check) run across the scoring and validity functions via `warn_item_order()`/`validate_item_uniqueness()` in [R/util.R](../R/util.R). No HiTOP validity scales yet.
- **Reliability** — `calc_alpha()` (covariance-based Cronbach's alpha, pairwise deletion) and `calc_omega()` (omega-total via one-factor lavaan CFA, MLR + FIML) in [R/reliability.R](../R/reliability.R); also embedded in scoring via `alpha`/`omega` arguments.
- **Utilities** — `rename_hitopsr_items()` (map legacy/text-matched columns to standard `HSR_*` names), `label_hitopsr()`/`label_hitopbr()` (attach label attributes to item/scale columns), `rank_scales()`.
- **Generators (instrument export)** — `generate_docx_*` (paper forms via {officer}/{flextable}; US + A4), `generate_qualtrics_*` (import .txt/.qsf), `generate_redcap_*` (data-dictionary zips) for all instruments; prebuilt outputs ship in `inst/extdata/`. `devel/qualtrics_test.R` holds unexported Qualtrics-API experiments (uses {httr2}).

### Internal utilities ([R/util.R](../R/util.R))

`reverse(x, low, high)`, `bind_columns()`, `adiff()` (inconsistency pair |difference|), `drop_na()`, `calc_sem()`, `cli_assert(condition, message)` (cli-flavored abort), and `validate_data()`/`validate_items()`/`validate_scales()`/`validate_range()`.

## Conventions

### Function signatures & naming

Scoring functions share `(data, items, [version,] srange, prefix, na.rm, calc_se, [alpha, omega,] append = TRUE, tibble = TRUE)`: `items` maps user column names/positions in instrument order; `srange` gives item min/max for reverse-keying; `prefix` (default `"pid_"`, `"hsr_"`, `"hbr_"`) is prepended to output columns; `append = TRUE` binds scores onto the input; `tibble = TRUE` returns a tibble. Output columns are `prefix` + camelCase scale stem (`pid_anhedonia`), validity `prefix` + abbreviation (`pid_INC`), standard errors `_se`. Exported names: `<verb>_<instrument>` with PID-5 forms selected by `version`, not by separate functions.

### User communication

{cli} throughout: `cli_alert_warning()` for counts of flagged observations + `cli_alert_info()` with an actionable `{.code dplyr::filter(...)}` suggestion; input errors via `cli_assert()`/`cli::cli_abort()`.

### Internal style

Base-R data manipulation internally (subsetting, `rowMeans`, `cbind`). {tibble} is an Import (tibble output is the default); {lavaan} stays in Suggests and is checked with `rlang::is_installed()` before `calc_omega()` runs. Document/generator work uses {officer}/{flextable}.

### Testing & oracle strategy

Scoring correctness is the package's core promise, so tests must verify against ground truth, never against the code's own output. In priority order: (1) hand-computed fixtures with the arithmetic in comments; (2) published reference values, cited; (3) independent recomputation from hardcoded official item numbers (the only check that catches transcription errors in the keying tables); (4) invariant tests. Snapshot tests only for message wording. Full rules: `.claude/skills/shared/tracking-rules.md`. Ground-truth oracle coverage is now in place for `pid_items` keying — including the BF 5-domain structure verified against the APA PID-5-BF Domain Scoring table and the FULL/SF primary-facet→domain map (`pid_domains`) verified against the APA full-form Domain Table (`test-keying.R`, M1/M6/M7) — PID-5 scoring/validity incl. FULL/SF domain scores (M2/M7), HiTOP-SR/BR scoring, and the reliability functions (M5); and the `generate_{docx,qualtrics,redcap}_*` export family (M10, D-010), whose output is parsed back and checked against the source instrument datasets. All implemented user-facing surfaces now have automated tests.

### Data workflow

`data-raw/` scripts (CSV → `usethis::use_data()`) regenerate everything in `data/` and `R/sysdata.rda` (`internal = TRUE`); never edit `.rda` files directly. Keying content changes require maintainer sign-off; sources in [SOURCES.md](SOURCES.md).

## Known issues & tech debt

<!-- Numbered list; remove items when fixed (note the fix in LOG.md and the milestone). -->

1. **SDTD item 38 unverified (keying, OQ-1)** — `pid_items` lists 17 SDTD items; Williams et al. (2019) Table 5's note enumerates 16 (no item 38) while its text says 17. Maintainer to check the physical PID-5 manual; `pid_items` unchanged pending sign-off. See [SOURCES.md](SOURCES.md) OQ-1.
2. **SF validity cutoffs unavailable** — ORS-S/PRD-S/SDTD-S have no validated cut scores; `validity_pid5(version = "SF")` warns at runtime. Literature watch; no milestone yet.
3. **Published-metric assumptions vs `srange`** — the PID-5 PRD/SDTD validity cutoffs (raw sums vs 10/11/19) assume 0–3 item coding and, unlike INC/ORS, do not adapt to `srange`. `validity_pid5()` now **warns** when `srange != c(0, 3)` (M11), so the silent case is closed; auto-adjusting the cutoffs for shifted codings (cutoff + k×low) remains deferred — it changes validity-scale semantics and needs maintainer sign-off.
4. **`_se`-where-`NA` masking is inconsistent across instruments** — `score_pid5()` sets a standard error to `NA` wherever its scale score is `NA`; `score_hitopsr()`/`score_hitopbr()` do not (they report a `calc_sem` value even where `na.rm = FALSE` made the score `NA`). M13's engine consolidation preserved this pre-existing difference via a `mask_se_na` flag (D-011) rather than silently unifying it — unifying would change HiTOP-SR/BR SE output under `na.rm = FALSE` and needs maintainer sign-off.

## Decision Log

<!-- Append-only; never renumber. Format: ### D-00n (YYYY-MM-DD): Title / Context / Decision / Consequences. -->
<!-- D-001–D-005 were written in the PID-5-only fork this tracking system was bootstrapped from; kept as history. D-006 records the re-baseline. -->

### D-001 (2026-07-09): Keying table as single source of truth

**Context:** Multiple PID-5 forms (220/100/25 items) share items with different numbering, and validity scales draw on overlapping item sets.
**Decision:** One master table (`pid_items`) maps every item to its number on each form and its scale memberships; all functions look up membership at runtime.
**Consequences:** New forms need only a new column; scoring code never hardcodes item numbers; the table itself must be treated as versioned instrument content.

### D-002 (2026-07-09): Base-R internals, {cli} messaging, optional {tibble}

**Context:** Keep the dependency footprint small for a package aimed at applied researchers.
**Decision:** Internal code uses base R; user-facing messaging uses {cli}; {tibble} and {ggplot2} stay in Suggests with opt-in behavior.
**Consequences:** No dplyr Import; contributors must write base-R internals even when tidyverse would be terser.

### D-003 (2026-07-09): Adopt project/-based tracking with Claude Code skills

**Context:** Development needs persistent, version-controlled planning that both humans and Claude can maintain.
**Decision:** Five tracking files (CLAUDE.md + project/{DESIGN,ROADMAP,MILESTONES,LOG}.md) with ownership boundaries, managed by four skills (plan-milestone, work-milestone, review-milestone, sync-docs).
**Consequences:** Milestone work should flow through the skills; tracking files are committed and `.Rbuildignore`d.

### D-004 (2026-07-09): Ground-truth oracles and fresh-context review

**Context:** Tests that assert the code's own output enshrine bugs (e.g., the SDTD guard bug); same-context review of one's own implementation invites self-review bias.
**Decision:** Scoring tests require ground-truth oracles (hand-computed fixtures, independent recomputation from the official key, invariants); /review-milestone uses a fresh-context subagent for diff review; accuracy-critical subagents never run on Haiku.
**Consequences:** Writing tests is slower but they can actually catch keying-table and algorithm errors; milestone review costs one extra subagent run.

### D-005 (2026-07-09): External-source verification of the keying table

**Context:** Independent recomputation copies item numbers out of `pid_items.csv`, so it verifies code-vs-table but not the table itself. The maintainer supplied the authoritative source PDFs.
**Decision:** Verify every `pid_items` key against its published source, record provenance in [SOURCES.md](SOURCES.md), and machine-check it in `test-keying.R`. Discrepancies that need author/supplement adjudication are documented as open questions and `skip()`-ed, **never** silently resolved by editing `pid_items`.
**Consequences:** The keying table now has a true external oracle (priority-2 in the oracle strategy). Two open questions remain (SDTD item 38, INC-S 38–92) pending source materials; `pid_items` is unchanged until sign-off.

### D-006 (2026-07-09): Re-baseline tracking docs on the canonical multi-instrument repo

**Context:** The tracking system (D-003–D-005) was written in an older PID-5-only fork (v0.0.0.9000). This canonical repo (v0.1.0) has a different API (`version` argument, `prefix` columns, no `scales`/`id` args), four instrument families, reliability functions, generators, vignettes, and pkgdown.
**Decision:** Rewrite CLAUDE.md/DESIGN/ROADMAP/MILESTONES/LOG from this repo's code; keep D-001–D-005 as history and SOURCES.md as the still-valid keying provenance. D-002's Suggests-only posture is partially superseded: {tibble} is now an Import and {officer}/{flextable}/{snakecase} joined Imports for the generator family.
**Consequences:** Old fork file:line references (R/pid5.R, R/pid5_fsf.R, sim_pid5fsf, the SDTD guard bug) are void here; the fork's oracle test suite must be *ported and adapted*, not copied (M1, M2).

### D-007 (2026-07-09): Instrument instructions are internal data owned by data-raw/sysdata.R

**Context:** The four `*_instructions` lists (administration text used only by the `generate_*` families) were duplicated in both `data/` (user-facing, but undocumented) and `R/sysdata.rda`, and each was written by its per-instrument `data-raw/*_info.R` script with `use_data()` (not `internal = TRUE`), so `R/sysdata.rda` was not regenerable from `data-raw/` (M3, DESIGN #3).
**Decision:** Treat instructions as internal data only: a single `data-raw/sysdata.R` defines all four and writes them together with `use_data(internal = TRUE)`; the `data/` copies are dropped and the per-instrument scripts no longer touch them. The two HiTOP-HSUM registry datasets (`hitophsum_items`, `hitophsum_choices`) stay user-facing and are now documented in `R/data.R`.
**Consequences:** `R/sysdata.rda` is regenerable and internal/user-facing data no longer overlap; new internal constants belong in `data-raw/sysdata.R`, not the `*_info.R` scripts.

### D-008 (2026-07-10): FULL/SF domain scoring via an exported `pid_domains` map

**Context:** `score_pid5()` outputs 25 facets for FULL/SF but no personality-trait domains (only BF, which nests `pid_scales` on `Domain`, returns domains). APA Step 3 computes each domain as the mean of just 3 *primary* facet average scores — a two-stage mean over a 15-facet subset, not the 21-facet `pid_items$Domain` grouping and not a pooled item mean (M7).
**Decision:** Store the 5×3 domain→primary-facet map as a new exported dataset `pid_domains` (built in `data-raw/pid_info.R`, documented, pkgdown-listed, oracle-verified against the APA table in `test-keying.R`); `score_pid5()` computes FULL/SF domains from the just-computed facet columns via this map. Domains use traditional `na.rm`-respecting means for now (consistent with facet scoring); the strict APA ≥2-missing/proration rule is deferred to M8. Domain `_se` reuses `calc_sem` over the 3 facet scores; alpha/omega stay facet-level.
**Consequences:** FULL/SF output grows by 5 domain columns (appended after facets; existing columns unchanged) whose camelCase names match BF's existing domain outputs. A new keying-style dataset must stay in sync with its APA oracle. M8 will layer strict missing-data semantics on top of this map.

### D-009 (2026-07-10): APA missing-data/proration scoring is the default, selected by `apa_scoring`

**Context:** `score_pid5()` currently averages available items with `rowMeans(na.rm = TRUE)`, computing a score from even a single surviving item. The published APA algorithm (full-form key p. 8, sourced verbatim; the BF key is the same rule on 5-item domains, M6) instead drops a scale when >25% of its items are unanswered and, at ≤25% missing, prorates to a rounded raw score — differing from a plain mean both by the >25% NA cutoff and by rounding the prorated raw before averaging.
**Decision:** Add `apa_scoring = TRUE` (default) to `score_pid5()`, applying the APA rule uniformly across FULL/SF/BF via an `apa_mean()` helper (facet/domain-item level: NA if `(n−a)/n > 0.25`, else `round(partial_sum × n / a) / n`; FULL/SF domains NA if any of the 3 primary facets is NA). `apa_scoring = TRUE` governs missing data entirely — `na.rm` is ignored under it (cli warning if `na.rm = FALSE`); `apa_scoring = FALSE` restores the traditional `rowMeans(na.rm = TRUE)` path. SF applies the full-form rule by analogy (Maples 2015 specifies none; noted in SOURCES.md).
**Consequences:** Default scored output changes under missing data (rounding + >25% NA gating) — a behavior change flagged in NEWS.md; users wanting the old behavior pass `apa_scoring = FALSE`. `_se` columns become NA wherever their scale is NA. No keying content (`pid_items`/`pid_scales`/`pid_domains`) changes.

### D-010 (2026-07-10): Parse-and-compare oracle for the `generate_*` export family

**Context:** The DOCX/Qualtrics/REDCap generators (M10, DESIGN Known issue #3) produce files, not numbers, so the scoring oracle strategy doesn't map directly and a byte-for-byte snapshot of the output would just enshrine whatever the code emits.
**Decision:** Test each generator by **parsing its output back into R** (`tests/testthat/helper-generators.R`: `read_qualtrics`/`read_redcap_csv`/`read_docx_xml`) and asserting the parsed content against the source instrument datasets (`*_items`, `*_instructions$options`, `hitophsum_choices`) derived independently — the file-generator analog of independent recomputation. Structural facts (page-break positions, zero-pad width, the 15 REDCap columns, `<w:pgSz>` per `papersize`) and the HSUM branching-logic rule strings are hand-derived and hardcoded as expectations; prebuilt `inst/extdata/` artifacts are not used as an oracle. zip-writing tests skip when no `zip` utility is present; docx tests skip when {officer}/{flextable} are absent.
**Consequences:** Generator regressions that change emitted item text, IDs, choices, pagination, page size, or HSUM logic are now caught. New generators/instruments should add a parse-and-compare test in the same style rather than a snapshot; the HSUM discrete-list (comma) branch stays unexercised until data uses it.

### D-011 (2026-07-10): Single internal scoring engine behind thin wrappers

**Context:** `score_pid5()`/`score_hitopsr()`/`score_hitopbr()` were three hand-maintained copies of the same ~100-line pipeline (former Known issue #3); the same `drop = FALSE` bug had to be fixed twice (M5 in BR, M9 in PID SE), and reliability (`alpha`/`omega`) had been wired into `score_pid5()`/`score_hitopsr()` but not `score_hitopbr()` — an incompleteness, not a design choice (BR runs the identical `rowMeans` pipeline; M13).
**Decision:** Extract the pipeline into one unexported, data-driven `score_engine()` ([R/score_engine.R](../R/score_engine.R), `@noRd`); each `score_*()` is now a thin wrapper that resolves only its instrument data (reverse items, per-scale item-number lists, FULL/SF domain map) and calls the engine — the scoring analog of the generators' `build_*` pattern. `apa_scoring` and domain scoring are optional engine features PID-5 opts into (`domain_map = NULL`, `apa_scoring = FALSE` for SR/BR); `score_hitopbr()` gains `alpha`/`omega` for parity. The pre-existing `_se`-where-`NA` masking difference (pid5 masks, SR/BR don't — Known issue #4) is preserved behind a `mask_se_na` flag (pid passes `TRUE`) rather than silently unified.
**Consequences:** A future PID-5-BFP is a wrapper + data, not a fourth copy; pipeline bugs are fixed once. Behavior is byte-identical on every pre-existing path — verified by a before/after characterization harness across the full argument matrix (version × `apa_scoring`/`na.rm`/`calc_se`/`append` × `alpha`/`omega`) on all `sim_*`/`ku_*` datasets (114 configs, all `identical()`). Unifying `mask_se_na` and moving reliability out of scoring into a returning `reliability_*()` family are deferred to M15 (both need maintainer sign-off).
