# M041: Confidence intervals for HiTOP-SR scale scores

- **Status:** in-progress
- **Priority:** normal
- **Depends on:** M059
- **Driving RR:** —
- **Principles touched:** IP2, IP3
- **Branch/PR:** `m041-hitopsr-score-intervals`

## Goal

Give each HiTOP-SR scale score a regression-based true-score estimate and a
confidence interval, computed from the instrument paper's published
development-sample statistics and labelled as such everywhere it appears.

## Scope

**In:** a new exported dataset carrying, per HiTOP-SR primary scale and
subscale, the item count, Cronbach's alpha, mean, and SD printed in Table 1 of
the HiTOP-SR introduction paper (Development Sample 2), built by a `data-raw/`
script from a committed CSV and checked cell-by-cell against the source by a
companion verification script; an unexported interval engine and an exported
`interval_hitopsr()` returning an estimate and bounds per score column, using
the regression-based true score with scale correction (Schmukle, 2026);
`references/` source notes for both papers; and user-facing text — help page,
NEWS, pkgdown, vignette — stating the reference group is a development sample
rather than a community norm.

**Out:** the eight HiTOP-BR spectrum scales → a follow-up milestone, because
Table 1 and `hitopbr_scales` disagree on the item counts of Detachment and
Internalizing and the final paper must settle it first. PID-5 intervals → the
"Confidence intervals on PID-5 profile plots" candidate row; no reliability
source for those norms exists. Model-based (omega) reliability → the same row;
alpha is what the paper prints. The multi-source `pid_norms` schema and the
German rows → their own candidate rows. Correcting the `_se` help-page
wording → its own hotfix. Plotting the intervals → not this milestone.

## Acceptance criteria

- [ ] AC1 `hitopsr_devstats` ships one row for each HiTOP-SR primary scale and
      subscale Table 1 covers, carrying item count, alpha, mean, and SD as
      printed in the final paper. A committed, source-cited name map resolves
      Table 1's labels onto `hitopsr_scales$Scale` and
      `hitopsr_subscales$Subscale` and declares an exception set naming every
      Table 1 row with no package scale and every package scale with no Table 1
      row. A test iterating those two shipped tables — never a hand-written
      list — shows the join is complete modulo the declared exception set, and
      that the exception set is exactly what the map declares.
- [ ] AC2 `data-raw/verify_hitopsr_devstats.R` extracts Table 1 from the source
      file on the gitignored `references/sources/` shelf and reports zero
      differing cells against both the committed CSV and the built
      `hitopsr_devstats`; its run output is recorded in the Review. Because
      that script cannot run in CI, a second, CI-runnable oracle asserts for
      every joined row that Table 1's item count equals that scale's `nItems`
      in `hitopsr_scales`/`hitopsr_subscales`. Where the final paper's format
      admits no deterministic extraction, an independent second transcription
      diffed against the first stands in for the extraction half.
- [ ] AC3 `interval_hitopsr()` returns `_est`, `_lo`, and `_hi` per requested
      score column at a `level` argument defaulting to 0.95, and its help page
      states the estimate and bound formulas with a page anchor into Schmukle
      (2025). A score column with no `hitopsr_devstats` row, and a call whose
      `srange` is not the response coding Table 1's mean and SD are printed on,
      each return `NA` in all three columns and report it as its own catchable
      `cli::cli_warn()` condition, mirroring `norm_pid5()`. Subset-scored
      columns are not detectable and are not claimed to be: the help page
      states that a scale scored from fewer than its full items is not
      comparable to the reference statistics.
- [ ] AC4 A test recomputes the estimate and both bounds in hand-written
      arithmetic, the arithmetic in comments, for two scales at opposite ends
      of the shipped alpha range, each at a score below, at, and above the
      reference mean, and at two confidence levels, matching the function's
      output to within 1e-8 (closed-form oracle). Each oracle is recorded by
      id, type, and asserting `test:line` at the location `cairn/DESIGN.md`
      declares for oracle records.
- [ ] AC5 A test asserts *marginal* coverage — true scores drawn from the
      reference population and observed scores generated from them under the
      measurement model the ingested Schmukle (2026) source note records, never
      a single fixed true score, whose conditional coverage a mean-shrunken
      estimator does not promise — swept over the lowest, median, and highest
      alpha in `hitopsr_devstats` and two nominal levels, with seed and
      replication count fixed in the test and the tolerance stated as a
      Monte-Carlo bound (simulation-coverage oracle, the primary oracle for an
      interval method).
- [ ] AC6 The help page, the NEWS entry, the `_pkgdown.yml` reference entry,
      and the vignette section each name the reference group as the paper's
      development sample and its N, and say it is not a community norm; a test
      asserts that wording as text over all four artifacts — the generated
      `man/*.Rd`, `NEWS.md`, `_pkgdown.yml`, and the vignette `.Rmd` — and
      asserts the N it finds equals the N documented for `hitopsr_devstats`.
- [ ] AC7 `devtools::document()` produces no diff and `devtools::test()` and
      `devtools::check()` are clean.

## Coverage

- AC1 → T3, T6
- AC2 → T4, T6
- AC3 → T2, T5, T7
- AC4 → T6
- AC5 → T6
- AC6 → T6, T7
- AC7 → T5, T6, T7

## Tasks

- [ ] T1 Ingest the HiTOP-SR introduction paper as a `cairn/references/`
      source note with Table 1's anchors, plus its `INDEX.md` line; the
      `SOURCES.md` provenance section M058 opened is extended, not duplicated.
      The shelved manuscript is the source, on Jeff's 2026-08-27 direction that
      it stands in for the accepted version here as it does for the two scale
      names (M059's D-entry).
- [ ] T2 Ingest Schmukle (2026, *Assessment*, 33(5), 817-825,
      `cairn/references/sources/schmukle2026.pdf`) as a `cairn/references/`
      source note, quoting Eqs (5)-(12) verbatim with page anchors.
- [ ] T3 Transcribe Table 1 to `data-raw/hitopsr_devstats.csv` and write
      `data-raw/hitopsr_devstats.R` building `data/hitopsr_devstats.rda`,
      committing the source-cited name map and its exception set. After M059
      renames `Body Focus` to `Appearance Focus`, two Table 1 labels do not
      join — `Manic Energy†`, a footnote marker, and `p-factor`, a HiTOP-BR
      scale outside this milestone's scope — and M059's
      `data-raw/verify_hitopsr_names.R` is what establishes that, so the map
      cites its run rather than re-deriving the set (IP1: discrepancies stay
      visible, never silently patched).
- [ ] T4 Write `data-raw/verify_hitopsr_devstats.R` on the pattern of
      `data-raw/verify_norms_against_book.R`, extracting Table 1 from the
      shelved source and diffing every cell against the committed CSV.
- [ ] T5 Implement the unexported interval engine and the exported
      `interval_hitopsr()`, following the wrapper-over-engine pattern of
      `R/score_engine.R` so HiTOP-BR and PID-5 can reuse it, with the shared
      validators from `R/util.R` and `call` threading.
- [ ] T6 Tests: the two oracles of AC4 and AC5, AC2's CI-runnable item-count
      oracle, the join completeness of AC1, every error branch fired with its
      condition asserted by name, and the wording guard of AC6. Add the
      validation doctrine's oracle-registry pointer line to `cairn/DESIGN.md`
      Conventions — the repo does numeric work and carries none today — and
      record this milestone's oracles there.
- [ ] T7 Docs: roxygen help page, NEWS entry, `_pkgdown.yml` reference index,
      and a vignette section demonstrating the function.

## Work log

- 2026-08-13: created by /milestone-plan.
- 2026-08-13: plan gate settled the IP3 question in favour of building — a descriptive table in a peer-reviewed instrument paper clears the published-source bar that the development workbooks did not; recorded as D-032, which narrows the 2026-08-06 ROADMAP correction rather than overturning it.
- 2026-08-13: plan gate chose a separate exported `interval_hitopsr()` over a `calc_ci` argument on `score_hitopsr()` because the package already separates scoring from conversion (`score_pid5()` vs `norm_pid5()`) and an interval needs the reference dataset a scoring call has no business loading; falsified by a researcher workflow that cannot get scores and intervals in one call without awkward re-plumbing.
- 2026-08-13: plan chose `hitopsr_devstats` and `_est`/`_lo`/`_hi` over `hitopsr_refstats` and `_ci_lo`/`_ci_hi` because the dataset name carries the development-sample provenance the maintainer asked to be visible everywhere; falsified by a name collision with a later community-norm dataset for the same instrument.
- 2026-08-13: plan chose the paper's Cronbach's alpha over model-based omega at the maintainer's direction, since alpha is what the source prints; falsified by omega estimates for these scales being published, which would make the alpha figures the weaker of two available sources.
- 2026-08-13: checkpoint commit — the fresh-context criteria audit was spawned over AC1-AC7 and had not reported when this landed; its findings and their disposition are recorded in the following line.
- 2026-08-13: the fresh-context criteria audit returned findings on AC1-AC6 and none on AC7; all six were accepted and the criteria rewritten. AC1 asserted a bijection its own task contradicted (four Table 1 labels do not join, one of them unresolvable) — now a committed name map plus a declared exception set. AC2 verified the CSV rather than the shipped object and named a script that cannot run in CI, leaving the constants with no CI-visible oracle — now diffs the built object too and adds a CI-runnable item-count oracle. AC3 constrained output shape only — now names the `level` default and the two mismatch branches, and declines to claim subset detection it cannot perform. AC4's single hand-computed point left the sign of the mean deviation, the alpha range, and the level unprobed — now two scales, three score positions, two levels. AC5 asserted coverage at a fixed true score, which a mean-shrunken estimator does not promise — now marginal coverage under the source's own measurement model, with seed, replications, and a Monte-Carlo tolerance fixed in the test. AC6 quantified over four surfaces and tested two, one of them untestable against an uninstalled package — now asserted over all four as text.
- 2026-08-13: the audit also surfaced a substantive open question for T2: it derives the interval half-width as `z * SD * sqrt(rel * (1 - rel))`, the classical standard error of the true-score estimate, where Johannes Zimmermann's proposal states `z * SD * sqrt(1 - rel)`. Which is Schmukle's scale-corrected form is not decidable from any secondary description in hand and is settled by the primary source, not here.
- 2026-08-13: two external inputs block implementation — the final submitted paper (T1) and the Schmukle PDF (T2); T5's engine and T6's oracles can be built ahead of both.

- 2026-08-27: both blockers cleared. `schmukle2026.pdf` is on the shelf, and Jeff directed that the under-review HiTOP-SR manuscript stands in for the accepted version here (D-042), so T1 no longer waits on Len.
- 2026-08-27: the T2 formula question is settled by the primary source and neither candidate was right. Schmukle (2026, *Assessment*, 33(5), 817-825) Eqs (10)-(12), p. 821: `RETS = M + sqrt(rel) * (x - M)`, `SERE = SD * sqrt(1 - rel)` (which the paper notes equals the SEM), `CI = RETS +/- z * SERE`. The half-width is the `sqrt(1 - rel)` form, and the estimator shrinks by `sqrt(rel)`, not by `rel`; `SD * sqrt(rel * (1 - rel))` is Eq (6), the uncorrected regression approach the paper argues against. T2 still ingests the source note; nothing ships from this line.
- 2026-08-27: `pdftotext -layout` extracts Table 1's labels and all five numeric columns cleanly, so AC2's deterministic-extraction half is viable and the second-transcription fallback should not be needed; two blocks put the label and its cells on separate lines, which M059's T4 handles first.
- 2026-08-27: amended at the M059 plan gate — `Depends on: M059`, the Schmukle citation corrected to 2026 (33(5)), T1 and T2 re-pointed at the shelved sources, and T3's exception set corrected: `Appearance Focus` does match a package scale, `Body Focus`, which M059 renames, leaving `Manic Energy†` and `p-factor`.
- 2026-08-28: /milestone-implement started; status in-progress on branch `m041-hitopsr-score-intervals`, cut from the pushed default branch.

## Decisions

## Review
