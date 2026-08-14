# M41: Confidence intervals for HiTOP-SR scale scores

- **Status:** planned
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** IP2, IP3
- **Branch/PR:** —

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
the regression-based true score with scale correction (Schmukle, 2025);
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

- [ ] AC1 `hitopsr_devstats` ships one row per HiTOP-SR primary scale and
      subscale, each carrying item count, alpha, mean, and SD as printed in the
      final paper's Table 1; a test iterating `hitopsr_scales` and
      `hitopsr_subscales` — not a hand-written list — shows every scale in
      those two tables has exactly one row and every row a scale.
- [ ] AC2 `data-raw/verify_hitopsr_devstats.R` extracts Table 1 from the source
      file on the `references/sources/` shelf and reports zero differing cells
      against the committed CSV; its run output is recorded in the Review.
- [ ] AC3 `interval_hitopsr()` returns `_est`, `_lo`, and `_hi` per requested
      score column, and its help page states the estimate and bound formulas
      with a page anchor into Schmukle (2025).
- [ ] AC4 A test recomputes one scale's estimate and both bounds from the
      published formula in hand-written arithmetic, the arithmetic in comments,
      and matches the function's output to within 1e-8 (closed-form oracle).
- [ ] AC5 A test simulates responses from a known true score and error variance
      at a known reliability, and shows the interval's empirical coverage sits
      within a stated tolerance of nominal (simulation-coverage oracle, the
      primary oracle for an interval method).
- [ ] AC6 The help page, NEWS entry, pkgdown reference, and vignette section
      each name the reference group as the paper's development sample and its
      N, and say it is not a community norm; a test asserts that wording in the
      rendered help page and the vignette.
- [ ] AC7 `devtools::document()` produces no diff and `devtools::test()` and
      `devtools::check()` are clean.

## Coverage

- AC1 → T3, T6
- AC2 → T4
- AC3 → T2, T5, T7
- AC4 → T6
- AC5 → T6
- AC6 → T6, T7
- AC7 → T5, T6, T7

## Tasks

- [ ] T1 Ingest the final submitted HiTOP-SR introduction paper as a
      `cairn/references/` source note with Table 1's anchors, plus its
      `SOURCES.md` provenance entry and `INDEX.md` line. Needs the final
      version from Len; the 2026-03-24 tracked-changes draft is scoping
      evidence only and no number ships from it.
- [ ] T2 Ingest Schmukle (2025, *Assessment*, 33, 817-825) as a
      `cairn/references/` source note, quoting the estimate and interval
      formulas verbatim with page anchors. Needs the PDF from the maintainer.
- [ ] T3 Transcribe Table 1 to `data-raw/hitopsr_devstats.csv` and write
      `data-raw/hitopsr_devstats.R` building `data/hitopsr_devstats.rda`,
      resolving the four scale names that do not join — `Manic Energy†`,
      `Non-suicidal Self-injury`, `p-factor`, and `Appearance Focus`, the last
      of which matches no package scale and may or may not be `Body Focus`.
- [ ] T4 Write `data-raw/verify_hitopsr_devstats.R` on the pattern of
      `data-raw/verify_norms_against_book.R`, extracting Table 1 from the
      shelved source and diffing every cell against the committed CSV.
- [ ] T5 Implement the unexported interval engine and the exported
      `interval_hitopsr()`, following the wrapper-over-engine pattern of
      `R/score_engine.R` so HiTOP-BR and PID-5 can reuse it, with the shared
      validators from `R/util.R` and `call` threading.
- [ ] T6 Tests: the two oracles of AC4 and AC5, the join completeness of AC1,
      every error branch fired with its condition asserted by name, and the
      wording guard of AC6.
- [ ] T7 Docs: roxygen help page, NEWS entry, `_pkgdown.yml` reference index,
      and a vignette section demonstrating the function.

## Work log

- 2026-08-13: created by /milestone-plan.
- 2026-08-13: plan gate settled the IP3 question in favour of building — a descriptive table in a peer-reviewed instrument paper clears the published-source bar that the development workbooks did not; recorded as D-032, which narrows the 2026-08-06 ROADMAP correction rather than overturning it.
- 2026-08-13: plan gate chose a separate exported `interval_hitopsr()` over a `calc_ci` argument on `score_hitopsr()` because the package already separates scoring from conversion (`score_pid5()` vs `norm_pid5()`) and an interval needs the reference dataset a scoring call has no business loading; falsified by a researcher workflow that cannot get scores and intervals in one call without awkward re-plumbing.
- 2026-08-13: plan chose `hitopsr_devstats` and `_est`/`_lo`/`_hi` over `hitopsr_refstats` and `_ci_lo`/`_ci_hi` because the dataset name carries the development-sample provenance the maintainer asked to be visible everywhere; falsified by a name collision with a later community-norm dataset for the same instrument.
- 2026-08-13: plan chose the paper's Cronbach's alpha over model-based omega at the maintainer's direction, since alpha is what the source prints; falsified by omega estimates for these scales being published, which would make the alpha figures the weaker of two available sources.
- 2026-08-13: checkpoint commit — the fresh-context criteria audit was spawned over AC1-AC7 and had not reported when this landed; its findings and their disposition are recorded in the following line.
- 2026-08-13: two external inputs block implementation — the final submitted paper (T1) and the Schmukle PDF (T2); T5's engine and T6's oracles can be built ahead of both.

## Decisions

## Review
