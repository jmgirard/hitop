# M37: Score HiTOP-SR subset-collected data

- **Status:** in-progress
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** IP2, GP2, GP3
- **Branch/PR:** `m37-hitopsr-subset-scoring`

## Goal

Let a researcher score, and estimate reliability for, data collected with a
`hitop_subset()`-generated HiTOP-SR short form by supplying only the subset's
item columns.

## Scope

**In:** A `subset = NULL` argument, last in the signature, on `score_hitopsr()`
and `reliability_hitopsr()`, accepting the `hitop_subset` object the generators
already take. An internal helper remaps that descriptor into the three inputs
the shared engines need — `n_items`, `reverse_items`, `items_scales` — expressed
as positions within the supplied columns rather than original HSR numbers.
Validation of the new argument, tests, `@param` docs, a NEWS entry, and a worked
short-form section in `vignettes/hitopsr_scoring.Rmd`.

**Out:** Extending subsets to HiTOP-BR or the PID-5 → the standing "Generalize
modularization to BR/PID-5" candidate row. Accepting a bare character vector of
scale names in place of the descriptor → rejected at the plan gate, one input
shape only. Inferring the subset from column names when no descriptor is
supplied → new candidate row added by this plan. Subset interplay with
`label_hitopsr()`, `rank_scales()` and the norming family → not needed (those
match by column name and already tolerate a partial column set).

## Acceptance criteria

- [ ] AC1. `score_hitopsr(data_sub, items = <the subset's columns>, subset = s,
      append = FALSE)` returns a tibble whose columns are exactly `prefix` pasted
      to the subset's `camelCase` stems, in `hitopsr_scales` row order.
- [ ] AC2. For every scale in the subset, the values the subset path returns are
      equal to the corresponding columns of `score_hitopsr()` run on the full
      405-item data, under both `missing` modes (`"available"`, `"complete"`),
      checked on `sim_hitopsr` both unmodified and with injected `NA`s.
- [ ] AC3. The test subset includes Romantic Disinterest (which holds HSR 310,
      the instrument's only reverse-keyed item), and for one respondent that
      scale's returned score equals a hardcoded expected value, with the
      reverse-key arithmetic (`5 - x` under `srange = c(1, 4)`) shown in a
      comment rather than recomputed from `hitopsr_items` at test time.
- [ ] AC4. With `calc_se = TRUE` the subset path returns `_se` columns for
      exactly the subset's scales, equal to the corresponding full-run `_se`
      values.
- [ ] AC5. `reliability_hitopsr(..., subset = s)` returns one row per subset
      scale with `nItems` matching `hitopsr_scales$nItems`, and alpha equal to
      the full-run alpha for those scales.
- [ ] AC6. Three error paths fire with a cli message attributed to the exported
      wrapper rather than the internal engine (checked on the condition's
      `call`): a `subset` that is not a `hitop_subset`; a hand-constructed
      `hitop_subset` naming an instrument other than `"hitopsr"` (a new check —
      `apply_subset()` has none today); and the existing `validate_items()`
      length check re-pointed at `subset$nItems`, naming `items` and reporting
      `subset$nItems` as the expected count.
- [ ] AC7. With `subset = NULL` the full-instrument path is unchanged: a
      differential probe against a `git archive` export of the base ref shows
      `identical()` returned values and `identical()` error conditions across
      `missing` × `calc_se` × `append` × items-as-names/positions for
      `score_hitopsr()`, and `alpha` × `omega` for `reliability_hitopsr()`.
- [ ] AC8. `devtools::test()` and `devtools::check()` are clean.

## Coverage

- AC1 → T2, T3
- AC2 → T2, T3
- AC3 → T3
- AC4 → T3
- AC5 → T4
- AC6 → T2, T4, T5
- AC7 → T6
- AC8 → T7

## Tasks

- [x] T1. Add the internal remap helper beside `apply_subset()` in `R/subset.R`,
      turning a `hitop_subset` plus an instrument's items/scales tables into
      `n_items`, `reverse_items` and `items_scales` in subset-column positions
      (`match(itemNumbers, subset$items)`); direct unit tests for the remap.
- [x] T2. Wire `subset = NULL` into `score_hitopsr()` (`R/score_hitopsr.R:41`)
      as the final argument, with the instrument check and the re-pointed
      `n_items`; guard truthiness the same way its consumer does (M24 lesson).
- [x] T3. Score-path tests: column set and order, subset-vs-full equality under
      both `missing` modes with and without injected `NA`s, the Romantic
      Disinterest reverse-key fixture, and `calc_se = TRUE`.
- [x] T4. Wire `subset` into `reliability_hitopsr()`
      (`R/reliability_hitopsr.R:34`) and test row set, `nItems`, and alpha
      equality against the full run.
- [x] T5. Error-branch tests for all three AC6 paths, asserting the attributed
      `call` as well as the message.
- [ ] T6. Differential probe script under `devel/` following the M31 pattern:
      `git archive` the base ref to a temp dir, run both versions in separate R
      subprocesses over the AC7 argument grid, compare values and conditions.
- [ ] T7. `@param` docs on both functions, NEWS entry, a worked short-form
      section in `vignettes/hitopsr_scoring.Rmd`, `devtools::document()`, then
      `devtools::test()` and `devtools::check()`.

## Work log

- 2026-08-01: created by /milestone-plan; promotes the "Score HiTOP-SR subset-collected data" candidate row (lineage M24).
- 2026-08-01: criteria audit ([O], fresh context) returned six findings — AC2 named three `missing` modes where HiTOP-SR has two; AC3's "computed by hand" was ambiguous between a hardcoded literal and an IP2-violating read of `hitopsr_items` at test time; AC6 called a re-pointed existing length check a new branch and left "the offending argument" ambiguous; AC7 left both the comparison semantics and the argument matrix unnamed. All four had one clear answer and were fixed pre-gate; AC1/AC4/AC5 clean.
- 2026-08-01: plan gate chose the `hitop_subset` descriptor as the only subset input over also accepting a bare character vector of scale names, because one input shape keeps validation at construction and avoids a second error surface (GP3 ergonomics traded against a narrower contract); falsified by evidence that researchers routinely score subset data they did not generate and so hold no descriptor.
- 2026-08-01: plan gate chose to carry `reliability_hitopsr()` in the same milestone over deferring it, because both wrappers hand the same three inputs to a shared engine and the audit confirmed subset alpha is numerically identical to full-run alpha, so the second wrapper costs one task rather than a second design; falsified by the reliability path needing its own remap semantics once implemented.
- 2026-08-01: plan chose to express the remap as positions within the supplied columns over renumbering the data to full-instrument width and padding absent items with `NA`, because padding would make every subset scale's siblings silently `NA`-scored and would defeat `missing = "complete"`; falsified by a scale whose items are not fully contained in the subset — impossible today, since the 76 scales partition the 405 items exactly.

- 2026-08-01: implementation gate chose to place AC6's new instrument check in the scoring-path remap helper only, leaving `apply_subset()` and the three `generate_*_hitopsr()` functions untouched, because the check fires only on a hand-assembled descriptor and extending it would add an error branch to functions this milestone scoped out; falsified by a generator ever being handed a foreign-instrument subset.
- 2026-08-01: T1 done — `subset_engine_inputs()` in `R/subset.R` remaps a descriptor into `n_items`/`reverse_items`/`items_scales` as positions within the supplied columns; the reverse key is read from `hitopsr_items` rather than trusted from the descriptor's parallel flags. Five direct unit tests; `devtools::test()` clean (11715 pass).
- 2026-08-01: T2/T3 done — `subset` is the last argument of `score_hitopsr()`, resolving through a shared `hitopsr_engine_inputs()` branch (`is.null()` on both sides, per the M24 truthiness lesson). Six score-path tests: column set in row order, subset-vs-full equality across both `missing` modes with and without injected `NA`s, the HSR 310 reverse-key fixture, `calc_se`, and items-as-names. The `NA` injection carries an explicit guard that the two `missing` modes differ on it, so the equality check cannot pass vacuously (M36 lesson). `devtools::test()` clean (11726 pass).
- 2026-08-01: T4/T5 done — `reliability_hitopsr()` takes `subset` through the same `hitopsr_engine_inputs()` helper; tests fix the row set and order, `nItems` against `hitopsr_scales`, and whole-row equality with the full run. All three AC6 error paths assert the attributed `call` on both wrappers (six attribution checks), using the M32 placeholder idiom so a NULL `call` fails one assertion rather than aborting the block. `devtools::test()` clean (11739 pass).
- 2026-08-01: `cairn_validate` advises 8 acceptance criteria against the >7 split tripwire; not split — AC8 is the mandated profile-verify criterion, leaving 7 substantive, and the only natural cut (reliability into its own milestone) is the one the plan gate explicitly declined. 7 tasks, one PR, 102/149 lines.

## Decisions

## Review
