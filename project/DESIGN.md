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
- **`*_scales` tables** (`pid_scales`, `hitopsr_scales`, `hitopbr_scales`, `hitopsr_subscales`) — one row per scale with list-columns `itemdata` and `itemNumbers` plus `nItems` and `camelCase` (the output column stem). Scoring functions iterate `itemNumbers` (e.g., `pid_scales[[version]]$itemNumbers` at [R/score_pid5.R:117](../R/score_pid5.R)); they never hardcode item numbers.
- **Internal data** (`R/sysdata.rda`) — administration instructions (`pid_instructions`, `hitopsr_instructions`, `hitopbr_instructions`, `hitophsum_instructions`) used by the generators.
- **Example data** — simulated (`sim_pid5`, `sim_pid5sf`, `sim_pid5bf`, `sim_hitopsr`, `sim_hitopbr`; naive uniform-random responses) and real de-identified KU student data (`ku_pid5sf` n=386, `ku_hitopsr`/`ku_hitopbr` n=411).

Adding a form variant = a new item-number column + `*_scales` entry; adding an instrument = new items/scales tables + a `score_*` (+ `validity_*`) + `generate_*` family.

### Function families

- **Scoring** — `score_pid5()` ([R/score_pid5.R](../R/score_pid5.R)), `score_hitopsr()` ([R/score_hitopsr.R](../R/score_hitopsr.R)), `score_hitopbr()` ([R/score_hitopbr.R](../R/score_hitopbr.R)): reverse-key per the items table, compute per-scale means, optionally add `_se` columns (`calc_se`), optionally print per-scale alpha/omega reliability. One PID-5 function covers all three forms via `version = c("FULL", "SF", "BF")`.
- **Validity** — `validity_pid5()` ([R/validity_pid5.R](../R/validity_pid5.R)): computes PNA (percent missing) for all versions and INC/ORS/PRD/SDTD (S-variants for SF) unconditionally for FULL/SF, with cli warnings at published cutoffs (INC ≥ 17, ORS ≥ 3, PRD ≤ 10, SDTD ≤ 11 / ≥ 19, INC-S ≥ 8; SF ORS-S/PRD-S/SDTD-S cutoffs not yet validated — a runtime warning says so at [R/validity_pid5.R:199](../R/validity_pid5.R)). No HiTOP validity scales yet.
- **Reliability** — `calc_alpha()` (covariance-based Cronbach's alpha, pairwise deletion) and `calc_omega()` (omega-total via one-factor lavaan CFA, MLR + FIML) in [R/reliability.R](../R/reliability.R); also embedded in scoring via `alpha`/`omega` arguments.
- **Utilities** — `rename_hitopsr_items()` (map legacy/text-matched columns to standard `HSR_*` names), `label_hitopsr()`/`label_hitopbr()` (attach label attributes to item/scale columns), `rank_scales()`.
- **Generators (instrument export)** — `generate_docx_*` (paper forms via {officer}/{flextable}; US + A4), `generate_qualtrics_*` (import .txt/.qsf), `generate_redcap_*` (data-dictionary zips) for all instruments; prebuilt outputs ship in `inst/extdata/`. `R/qualtrics_test.R` holds unexported Qualtrics-API experiments (uses {httr2} — see Known issues #4).

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

Scoring correctness is the package's core promise, so tests must verify against ground truth, never against the code's own output. In priority order: (1) hand-computed fixtures with the arithmetic in comments; (2) published reference values, cited; (3) independent recomputation from hardcoded official item numbers (the only check that catches transcription errors in the keying tables); (4) invariant tests. Snapshot tests only for message wording. Full rules: `.claude/skills/shared/tracking-rules.md`. **Current coverage falls far short of this** — see Known issues #1.

### Data workflow

`data-raw/` scripts (CSV → `usethis::use_data()`) regenerate everything in `data/` and `R/sysdata.rda` (`internal = TRUE`); never edit `.rda` files directly. Keying content changes require maintainer sign-off; sources in [SOURCES.md](SOURCES.md).

## Known issues & tech debt

<!-- Numbered list; remove items when fixed (note the fix in LOG.md and the milestone). -->

1. **No PID-5 scoring/validity or reliability tests** — `pid_items` keying is now machine-verified against the published sources (`tests/testthat/test-keying.R`, M1 done 2026-07-09). Still missing: tests of `score_pid5()`/`validity_pid5()` output (M2) and the reliability functions / HiTOP-SR/BR scoring (M5). Otherwise `tests/testthat/` has only the two HiTOP-SR files (rename + pipeline).
2. **No R CMD check or coverage CI** — `.github/workflows/` contains only `pkgdown.yaml`. Tracked as M4.
3. **Six undocumented datasets in `data/`** — `hitopbr_instructions`, `hitopsr_instructions`, `hitophsum_instructions`, `pid_instructions`, `hitophsum_choices`, `hitophsum_items` have no roxygen entries in [R/data.R](../R/data.R); the four `*_instructions` are duplicated in `R/sysdata.rda` (leftover from commit 08e3d88 "use internal data"), and no `data-raw/` script uses `internal = TRUE` — e.g. [data-raw/pid_info.R:73](../data-raw/pid_info.R) still writes `pid_instructions` to `data/` — so `R/sysdata.rda` is not currently regenerable from data-raw. Likely R CMD check warning. Tracked as M3.
4. **Dependency declarations out of sync** — [R/qualtrics_test.R:14](../R/qualtrics_test.R) uses `httr2::` but {httr2} is not in DESCRIPTION; {glue}, {lifecycle}, and {jsonlite} are Imports with no uses in R/. Tracked as M3.
5. **Redundant `utils::data()` calls** — [R/score_pid5.R:98](../R/score_pid5.R), [R/score_hitopsr.R:67](../R/score_hitopsr.R), [R/score_hitopsr.R:201](../R/score_hitopsr.R), [R/score_hitopbr.R:53](../R/score_hitopbr.R). Verified 2026-07-09: they *work* (datasets are exported with LazyData) but are unnecessary — `validity_pid5()` uses lazy data directly — and load copies into the user's global environment. Tracked as M3.
6. **SDTD item 38 unverified (keying, OQ-1)** — `pid_items` lists 17 SDTD items; Williams et al. (2019) Table 5's note enumerates 16 (no item 38) while its text says 17. Maintainer to check the physical PID-5 manual; `pid_items` unchanged pending sign-off. See [SOURCES.md](SOURCES.md) OQ-1.
7. **SF validity cutoffs unavailable** — ORS-S/PRD-S/SDTD-S have no validated cut scores; `validity_pid5(version = "SF")` warns at runtime. Literature watch; no milestone yet.
8. **Minor** — SDTD warning percentages use `length(prd_vec)` as denominator ([R/validity_pid5.R:172](../R/validity_pid5.R), :177; harmless, same length); stale vignette asset dirs `vignettes/bhitop_scoring_files/`, `vignettes/hitoppro_scoring_files/`. Fold into M3.

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
