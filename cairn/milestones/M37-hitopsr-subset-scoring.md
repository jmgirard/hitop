# M37: Score HiTOP-SR subset-collected data

- **Status:** review
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** IP2, GP2, GP3
- **Branch/PR:** `m37-hitopsr-subset-scoring` · https://github.com/jmgirard/hitop/pull/40

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

- [x] AC1. `score_hitopsr(data_sub, items = <the subset's columns>, subset = s,
      append = FALSE)` returns a tibble whose columns are exactly `prefix` pasted
      to the subset's `camelCase` stems, in `hitopsr_scales` row order.
- [x] AC2. For every scale in the subset, the values the subset path returns are
      equal to the corresponding columns of `score_hitopsr()` run on the full
      405-item data, under both `missing` modes (`"available"`, `"complete"`),
      checked on `sim_hitopsr` both unmodified and with injected `NA`s.
- [x] AC3. The test subset includes Romantic Disinterest (which holds HSR 310,
      the instrument's only reverse-keyed item), and for one respondent that
      scale's returned score equals a hardcoded expected value, with the
      reverse-key arithmetic (`5 - x` under `srange = c(1, 4)`) shown in a
      comment rather than recomputed from `hitopsr_items` at test time.
- [x] AC4. With `calc_se = TRUE` the subset path returns `_se` columns for
      exactly the subset's scales, equal to the corresponding full-run `_se`
      values.
- [x] AC5. `reliability_hitopsr(..., subset = s)` returns one row per subset
      scale with `nItems` matching `hitopsr_scales$nItems`, and alpha equal to
      the full-run alpha for those scales.
- [x] AC6. Three error paths fire with a cli message attributed to the exported
      wrapper rather than the internal engine (checked on the condition's
      `call`): a `subset` that is not a `hitop_subset`; a hand-constructed
      `hitop_subset` naming an instrument other than `"hitopsr"` (a new check —
      `apply_subset()` has none today); and the existing `validate_items()`
      length check re-pointed at `subset$nItems`, naming `items` and reporting
      `subset$nItems` as the expected count.
- [x] AC7. With `subset = NULL` the full-instrument path is unchanged: a
      differential probe against a `git archive` export of the base ref shows
      `identical()` returned values and `identical()` error conditions across
      `missing` × `calc_se` × `append` × items-as-names/positions for
      `score_hitopsr()`, and `alpha` × `omega` for `reliability_hitopsr()`.
- [x] AC8. `devtools::test()` and `devtools::check()` are clean.

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
- [x] T6. Differential probe script under `devel/` following the M31 pattern:
      `git archive` the base ref to a temp dir, run both versions in separate R
      subprocesses over the AC7 argument grid, compare values and conditions.
- [x] T7. `@param` docs on both functions, NEWS entry, a worked short-form
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
- 2026-08-01: T6 done — `devel/regression_probe_m37.R` runs 43 cells (score's `missing` × `calc_se` × `append` × names/positions grid, a non-default `srange` and `prefix`, reliability's `alpha` × `omega`, and 21 error/warning conditions) against a `git archive` export of the merge-base, in separate R subprocesses per tree; 43/43 `identical()`. Unlike M31's probe it compares returned values and full conditions, not accept-vs-reject, because M37 changes neither. ~8 min per run (76 omega CFAs, twice).
- 2026-08-01: probe sensitivity confirmed by mutation rather than by eye — forcing the full path's `reverse_items` to `integer(0)` turned the probe red, the reported differences naming `hsr_romanticDisinterest` (and its `_se`) across the score grid and `alpha` on the reliability side, which is exactly the scale holding HSR 310; mutation reverted, the restored file verified byte-identical to the committed version, and the clean probe re-run afterwards.
- 2026-08-01: T7 done — `@param subset` on both wrappers plus a runnable `@examples` line each, a NEWS entry, and a "Scoring a Short Form" vignette section that fields a four-scale form, scores it, and shows `all.equal()` against the full run. `devtools::document()` rewrote only the two `.Rd` files; `devtools::test()` clean (11739 pass); `devtools::check()` clean (0 errors, 0 warnings, 0 notes, 3m38s). AC7 probe re-run on the restored tree: 43/43 identical.
- 2026-08-01: reviewed at PR #40 — all 8 criteria verified with fresh evidence, consistency gate clean, three fresh-context reviewers (two returned zero findings, the diff-bug lens returned 14 scored by a fourth agent). One finding actioned (F2, the now-false `@param data`/`@param items` wording, fixed on both wrappers); F11 carried to a candidate row; 12 logged below threshold. First review pass, no returns.
- 2026-08-01: `cairn_validate` advises 8 acceptance criteria against the >7 split tripwire; not split — AC8 is the mandated profile-verify criterion, leaving 7 substantive, and the only natural cut (reliability into its own milestone) is the one the plan gate explicitly declined. 7 tasks, one PR, 102/149 lines.

## Decisions

## Review

Reviewed 2026-08-01 on `m37-hitopsr-subset-scoring` at PR #40. Evidence gathered
by running each criterion fresh, not from the implementation session's record.

**Acceptance criteria**

- AC1 — `score_hitopsr(subset=)` on a four-scale form returned exactly
  `hsr_agoraphobia`, `hsr_antisocialBehavior`, `hsr_appetiteLoss`,
  `hsr_romanticDisinterest`; `identical()` against `prefix` pasted to the
  `hitopsr_scales`-row-ordered `camelCase` stems is TRUE, and the scales were
  requested in a different order than they came back.
- AC2 — subset output equalled the corresponding full-run columns across
  2 datasets × 2 `missing` modes (4 comparisons, all `all.equal()` TRUE). The
  `NA`-injected dataset is non-vacuous: 8 `NA`s per scale under `"complete"`
  against 0 under `"available"`.
- AC3 — Romantic Disinterest for fixture respondent 3 returned 1.8, the
  hardcoded expected value; unreversed it would be 2.4. The `5 - x` arithmetic
  is a test comment, not a runtime read of `hitopsr_items`.
- AC4 — with `calc_se = TRUE` the subset returned 8 columns, 4 of them `_se`,
  one per subset scale and no others; `all.equal()` against the full run's
  corresponding columns TRUE.
- AC5 — `reliability_hitopsr(subset=)` returned 4 rows in `hitopsr_scales` row
  order with `nItems` 5, 8, 3, 5, and alpha `all.equal()` to the full run.
  `nItems` is integer, byte-identical to the full path's; the table column
  `hitopsr_scales$nItems` is double, so a strict `identical()` against the table
  is FALSE on both paths alike — pre-existing, not introduced here.
- AC6 — all three paths fired with the abort attributed to the exported wrapper,
  read from `conditionCall()`: a non-`hitop_subset` ("must be a <hitop_subset>
  object"), a hand-built foreign-instrument descriptor ("describes the wrong
  instrument … Expected a "hitopsr" subset but got "hitopbr""), and the
  re-pointed length check ("The `items` argument has the wrong length. Expected
  21 items but got 405"). Checked on `score_hitopsr()` and
  `reliability_hitopsr()` alike; each blamed itself.
- AC7 — `devel/regression_probe_m37.R` re-run at review: 43/43 cells
  `identical()` against a `git archive` export of merge-base `fb926b4`.
- AC8 — `devtools::test()` 11739 pass / 0 fail / 1 skip (on-CRAN);
  `devtools::check()` 0 errors, 0 warnings, 0 notes.

**Consistency gate**

- `cairn_validate` exit 0, all 16 checks pass including `coverage complete`;
  2 standing advisories (M37's 8th criterion, disposed of at plan; the 20
  pre-migration id tokens).
- No `DESIGN.md` principle changed — `cairn_impact` not applicable.
- Toolchain slot: `devtools::document()` left no diff; `pkgdown::check_pkgdown()`
  "No problems found"; NEWS entry present; `devel/` already carries its
  `.Rbuildignore` entry; full `check()` clean.

**Independent review**

Three fresh-context reviewers with distinct evidence bases. The blame-history
lens (Sonnet) and the prior-review lens (Sonnet) each reported zero findings —
the latter confirming the new `is.null()` guards do not repeat M24's bypassable
`isTRUE()` guard, that the M32 placeholder idiom is reproduced correctly, and
that a GitHub inline-comment probe returned empty, so archived `## Review`
sections were the whole surface. The diff-bug lens (Opus) reported 14 candidate
findings, scored by a fourth agent that generated none of them.

Actioned (scored >= 80), 1 of 14:

- F2 (82) — `@param data` and `@param items` on both wrappers still read "all"
  and "405" unconditionally, contradicting the new `@param subset` a reader
  meets later and telling a short-form user their data cannot be scored. Fixed:
  both params now say "all 405, or, when `subset` is supplied, that short form's
  items", on `score_hitopsr()` and `reliability_hitopsr()` alike.

Logged below threshold (13), surfaced not dropped:

- F11 (78) — the new `@examples` use `sim_hitopsr[s$items]`, correct only
  because that dataset is exactly the 405 items with no ID columns; `ku_hitopsr`
  has leading `participant`/`biosex`. The vignette uses the safe name-based
  idiom. Carried to a candidate row.
- F4 (75) — `n_items` is trusted from `subset$nItems` rather than recomputed
  from `length(subset$items)`; a hand-mutated descriptor scores silently wrong.
- F7 (70) — subset-path tests never vary `append`, `prefix`, or `srange`, and
  the probe varies those only on the full path.
- F10 (65) — the probe's `outcome()` drops the returned value when a warning
  fires, so the `items_misordered` cell compares the warning only.
- F1 (55) — `DESIGN.md`'s shared-signature line does not yet mention `subset`.
- F5 (55) — the containment invariant is a comment, not an assertion;
  unreachable through `hitop_subset()` today.
- F13 (55) — the additive NEWS bullet sits under a "breaking changes" lead-in.
- F12 (40) — `subset_engine_inputs()` carries generality parameters no test
  varies.
- F6 (35) — the subset path reads `Reverse` by logical indexing, the full path
  by `== TRUE`; identical while `Reverse` stays logical.
- F8 (30) — two equality tests index the oracle by what the implementation
  returned; the scale set is separately pinned elsewhere.
- F3 (8) and F9 (8) — refuted on inspection: the wrong-instrument assertion does
  pass `call = call`, and `info=` does label failures under testthat 3e.
- F14 (5) — stale; the criteria boxes and this section are the review gate's own
  output.
