# M041: Confidence intervals for HiTOP-SR scale scores

- **Status:** review
- **Priority:** normal
- **Depends on:** M059
- **Driving RR:** —
- **Principles touched:** IP2, IP3
- **Branch/PR:** `m041-hitopsr-score-intervals` — https://github.com/jmgirard/hitop/pull/66

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

- [x] AC1 `hitopsr_devstats` ships one row per HiTOP-SR primary scale and
      subscale Table 1 covers, keyed by scale name and carrying item count,
      mean, SD, and a `reliability`/`reliabilityType` pair holding Table 1's
      printed Cronbach's alpha and the constant `"alpha"`. The four
      source-drawn cells are as printed in the manuscript D-042 admits, at
      the SHA-256 `data-raw/hitopsr_table1.R` pins, which AC2's cell-by-cell
      diff establishes; `reliabilityType` is a package-supplied label, not a
      printed cell. A committed, source-cited name map resolves Table 1's
      labels onto `hitopsr_scales$Scale` and `hitopsr_subscales$Subscale` and
      declares an exception set over Table 1's primary-scale and subscale
      rows only — the "Superspectra and Spectra Scales" block being out of
      Scope — each member named with why it does not join and citing the
      `data-raw/verify_hitopsr_names.R` run that established it. A test
      iterating those two shipped tables — never a hand-written list — shows
      every package scale outside the exception set has exactly one row, and
      that the package-side residue it computes is exactly what the map
      declares; the Table 1 side is not computable in CI and is not claimed
      to be.
- [x] AC2 `data-raw/verify_hitopsr_devstats.R` extracts Table 1 from the source
      file on the gitignored `references/sources/` shelf and reports zero
      differing cells against both the committed CSV and the built
      `hitopsr_devstats`; its run output is recorded in the Review. Because
      that script cannot run in CI, a second, CI-runnable oracle asserts for
      every joined row that Table 1's item count equals that scale's `nItems`
      in `hitopsr_scales`/`hitopsr_subscales`. Where the source's format
      admits no deterministic extraction, an independent second transcription
      diffed against the first stands in for the extraction half.
- [x] AC3 `interval_hitopsr()` returns `_est`, `_lo`, and `_hi` per requested
      score column at a `level` argument defaulting to 0.95, and its help page
      states the estimate and bound formulas with a page anchor into Schmukle
      (2026). A score column with no `hitopsr_devstats` row, and a call whose
      `srange` is not the response coding Table 1's mean and SD are printed on,
      each return `NA` in all three columns and report it as its own catchable
      `cli::cli_warn()` condition, mirroring `norm_pid5()`. Subset-scored
      columns are not detectable and are not claimed to be: the help page
      states that a scale scored from fewer than its full items is not
      comparable to the reference statistics.
- [x] AC4 A test recomputes the estimate and both bounds in hand-written
      arithmetic, the arithmetic in comments, for two scales at opposite ends
      of the shipped `reliability` range, each at a score below, at, and above the
      reference mean, and at two confidence levels, matching the function's
      output to within 1e-8 (closed-form oracle). Each oracle is recorded by
      id, type, and asserting `test:line` at the location `cairn/DESIGN.md`
      declares for oracle records.
- [x] AC5 A test asserts *marginal* coverage — true scores drawn from the
      reference population and observed scores generated from them under the
      measurement model the ingested Schmukle (2026) source note records, never
      a single fixed true score, whose conditional coverage a mean-shrunken
      estimator does not promise — swept over the lowest, median, and highest
      `reliability` in `hitopsr_devstats` and two nominal levels, with seed and
      replication count fixed in the test and the tolerance stated as a
      Monte-Carlo bound (simulation-coverage oracle, the primary oracle for an
      interval method).
- [x] AC6 The help page, the NEWS entry, the `_pkgdown.yml` reference entry,
      and the vignette section each name the reference group as the paper's
      development sample and its N, and say it is not a community norm; a test
      asserts that wording as text over all four artifacts — the generated
      `man/*.Rd`, `NEWS.md`, `_pkgdown.yml`, and the vignette `.Rmd` — and
      asserts the N it finds equals the N documented for `hitopsr_devstats`.
- [x] AC7 `devtools::document()` produces no diff and `devtools::test()` and
      `devtools::check()` are clean.

## Coverage

- AC1 → T3, T6
- AC2 → T4, T6
- AC3 → T2, T5, T6, T7
- AC4 → T6
- AC5 → T6
- AC6 → T6, T7
- AC7 → T5, T6, T7

## Tasks

- [x] T1 Ingest the HiTOP-SR introduction paper — the manuscript D-042 admits
      — as a `cairn/references/` source note with Table 1's anchors, plus its
      `INDEX.md` line, extending the `SOURCES.md` section M058 opened.
- [x] T2 Ingest Schmukle (2026, *Assessment*, 33(5), 817-825,
      `cairn/references/sources/schmukle2026.pdf`) as a `cairn/references/`
      source note, quoting Eqs (5)-(12) verbatim with page anchors.
- [x] T3 Transcribe Table 1 to `data-raw/hitopsr_devstats.csv` and write
      `data-raw/hitopsr_devstats.R` building `data/hitopsr_devstats.rda`,
      committing the source-cited name map and its exception set. The map
      cites the `data-raw/verify_hitopsr_names.R` run rather than re-deriving
      the set (IP1: discrepancies stay visible): that run reconciles all 93 SR
      labels but `Manic Energy†`, whose footnote marker its package name lacks,
      and excludes the eight-scale Superspectra and Spectra block wholesale as
      out of Scope rather than itemizing it.
- [x] T4 Write `data-raw/verify_hitopsr_devstats.R` on `verify_norms_against_book.R`'s
      pattern: extract Table 1 from the shelf, diff every cell against the CSV.
- [x] T5 Implement the unexported interval engine and the exported
      `interval_hitopsr()`, following the wrapper-over-engine pattern of
      `R/score_engine.R` so HiTOP-BR and PID-5 can reuse it, with the shared
      validators from `R/util.R` and `call` threading.
- [x] T6 Tests: the two oracles of AC4 and AC5, AC2's CI-runnable item-count
      oracle, the join completeness of AC1, every error branch fired with its
      condition asserted by name, and the wording guard of AC6. Prove the join
      test able to fail — one row removed, one duplicated, one exception-set
      member deleted, each in turn naming the offending row. Pin that bounds
      are not clamped to `srange`. Add the validation doctrine's
      oracle-registry pointer line to `cairn/DESIGN.md` Conventions — the repo
      does numeric work and carries none today — and record this milestone's
      oracles there.
- [x] T7 Docs: roxygen help page, NEWS entry, `_pkgdown.yml` reference index,
      and a vignette section demonstrating the function. The help page states
      RR05's two limitations: bounds are Eq (12)'s symmetric, constant-width
      CTT interval and may fall outside the response range on a strongly
      skewed scale, and coverage is marginal over the reference population
      under the source's linear-normal measurement model.

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
- 2026-08-28: implement gate, reliability coefficient — Jeff chose an outside review over settling it here, so the alpha-vs-omega-ordinal question escalates before T3 or T5 begin. The session's recommendation was alpha, on three grounds: Schmukle (2026, p. 822) estimated its own validating simulation's CIs with Cronbach's alpha on 10 six-category Likert items, the item format the HiTOP-SR uses; Eq (11)'s `s_x` is the observed-score SD, the same metric Table 1's mean and SD are on, whereas omega-ordinal is a latent-response-metric coefficient; and alpha is printed, so it is transcribable and cell-verifiable as AC2 assumes, while omega-ordinal would pull the raw-data recomputation candidate into this milestone.
- 2026-08-28: implement gate, `hitopsr_devstats` schema — Jeff chose `reliability` plus `reliabilityType` over a column named `alpha`, so the coefficient's identity travels with its value. Departs from the `alpha`/`omega` column pattern of the `reliability_*` family; falsified by a later coefficient that cannot be labelled in one string. Whether AC1's "alpha" wording needs an amendment is held until the escalation returns, since the two answers may need amending together.
- 2026-08-28: blocked on RB05 (`cairn/reviews/RB05-hitopsr-interval-reliability.md`) — which reliability coefficient supplies `rel` in the interval formula. Committed on the milestone branch rather than the default branch, since M041's status mirror and gate lines already live here.
- 2026-08-28: RB05 answered by RR05 (`cairn/reviews/archive/`). Alpha, on the equations rather than on convenience: Eq (8) defines the reliability as true-score over observed-composite variance, the same variable Eq (11)'s SD belongs to, so omega-ordinal — a latent-response-metric coefficient — makes `SERE` the standard error of nothing. Promoted to D-043, which also fixes the four IP2 conditions a recomputed coefficient would have to meet and records Green & Yang categorical omega as unscheduled downstream work.
- 2026-08-28: RR05's eight recommendations triaged — four applied (ship the printed alpha as `reliability`/`reliabilityType`; never omega-ordinal for this use; the two help-page limitations, now in T7; amend AC1's `alpha` wording), one applied in the narrower home Jeff chose (the planted-defect probe for the join test, added to T6 as a task rather than as an added criterion), one filed (Green & Yang omega, onto the standing ROADMAP recomputation row), and two rejections adopted as written (do not restrict the function on skewed scales; do not recompute `rel` inside M041).
- 2026-08-28: amendment gate — AC1 rewritten and AC2, AC4 and AC5 re-pointed off the deleted `alpha` column, adopted by Jeff as shown. A fresh-context [O] criteria audit ran in full mode over the amended AC1 before it was written and returned six findings: the draft's `every value is as printed` was false of the package-supplied `reliabilityType`; no key column was required; `every Table 1 row` overran the Scope by pulling in the eight-scale Superspectra block; the exception-set clause reduced to the map agreeing with itself on the half CI cannot see; the fidelity universal named no procedure that enumerates cells; and no probe was cited. All six are answered in the adopted text or in T6. Its request to hold the coefficient open was moot — it did not have RR05.
- 2026-08-28: T3's exception set corrected against a run of `data-raw/verify_hitopsr_names.R`, which reconciles all 93 SR labels but `Manic Energy†` and excludes the eight-scale Superspectra and Spectra block wholesale; the prior text naming `p-factor` as an individual exception was wrong. Minor task edit.
- 2026-08-28: the amendment pushed the plan-owned body to 157 lines, so the Tasks section — heaviest after Acceptance criteria, and the only one of the two implement may edit — was compressed in one pass to 149. Recompressing the just-gated criteria would have taken a second gate and a second audit.
- 2026-08-28: RB05/RR05 archived; status back to in-progress. `cairn/ROADMAP.md` grew ~360 bytes on the candidate-row extension and stands at 36,160 against the 24,000 budget, an overage already standing by Jeff's 2026-08-24 choice.
- 2026-08-28: implement gate, four choices. Table 1's numbers are extracted by word coordinates (`pdftotext -bbox`, rows banded by y and columns by x) rather than by grouping the layout dump's equal-length runs: the two watermark-split page blocks and the two clean ones then read through one code path, and a re-typeset source fails the banding rather than pairing a label with another row's cells. `hitopsr_devstats` carries `scale`, `camelCase`, `type`, `nItems`, `reliability`, `reliabilityType`, `mean`, `sd` — the stem so a scored column finds its row, the type so the 17 subscale rows are distinguishable from the 76 scale rows without a join. Oracle records go in one file, `cairn/ORACLES.md`, with `cairn/DESIGN.md` Conventions carrying the pointer line. `interval_hitopsr(data, scores, srange = c(1, 4), prefix = "hsr_", level = 0.95, append = TRUE)` mirrors `norm_pid5()`; falsified by a caller needing every prefixed column converted without naming them.
- 2026-08-28: T1 and T2 done. `cairn/references/simms2026.md` anchors Table 1 (shelf pp. 49-51, 101 rows in 13 sections, columns `# Items`/`alpha`/`M`/`SD`/`Range`/`Skewness`/`Kurtosis` on the 1-4 item-mean coding) and Development Sample 2 (N = 780, pp. 15-16); `cairn/references/schmukle2026.md` quotes Eqs (1)-(12) with page anchors. Both carry their `INDEX.md` line. Schmukle's Box 1 (p. 823) was recomputed: the estimates and standard errors reproduce exactly, and both printed upper bounds are 0.01 below the unrounded value because the article substitutes its own rounded intermediates — so Box 1 is not usable as a last-digit pin, and the page says so.
- 2026-08-28: T3 done. `data-raw/hitopsr_devstats.csv` transcribes Table 1's 93 primary-scale and subscale rows -- read off the three table pages rendered at 200 dpi, a route independent of any text extraction. `data-raw/hitopsr_devstats.R` builds the 93x8 tibble and refuses to write a stale `.rda`: it re-derives the package-side residue at every rebuild and stops unless it equals the declared exception set. That set is empty, and empty is the finding -- the 2026-08-28 run of `data-raw/verify_hitopsr_names.R` reconciles all 93 labels once `Manic Energy†`'s footnote marker is mapped away, so every shipped scale and subscale is covered and there is no member to name. Item counts agree with `hitopsr_scales`/`hitopsr_subscales` on all 93 rows. `reliability` spans 0.61 (Situational Phobias) to 0.96 (Distress-Dysphoria).
- 2026-08-28: T4 done. `hitopsr_table1_cells()` joins the shared extractor in `data-raw/hitopsr_table1.R`, reading Table 1 from `pdftotext -bbox` word coordinates: rows banded on the vertical axis, columns split on the horizontal one, so the two watermark-split page blocks and the two clean ones read through one path. It also recovers the label indentation, which is Table 1's only marker of a subscale, and the page-range detection M059 wrote is now shared by both extractors rather than duplicated. `data-raw/verify_hitopsr_devstats.R` compared 372 cells against the CSV and 372 against the shipped object over 93 rows each and reported none differing, with 17 indented labels matching the 17 `subscale` rows. Proven able to fail on six planted defects in turn, each named and exiting 1, clean controls either side; the run and the plants are recorded in the script header and in `cairn/SOURCES.md`.
- 2026-08-28: discovered sub-task, outside the plan: `devtools::test()` was already red on this branch before any M041 code landed -- 5 failures in `test-scale-name-hitopsr.R`, M059's rename-diff tests. `skip_without_merge_base()` skips only when the merge base *is* HEAD, so on the M059 branch the tests ran and on the default branch they skipped, but on any branch cut after M059 the merge base is distinct and already carries the new name, and the "only the name moved" assertions then report that branch's own work as an unexpected change. A test that fails on a change it was never about is a defect in the test, so a `skip_without_rename_base()` helper now asks the merge base what it called the scale and skips when the rename is already there. Verified to discriminate: the pre-M059 commit's `hitopsr_scales` still carries `Body Focus`, so the guard would not have skipped on M059's own branch. Three call sites, tests only, no runtime surface.
- 2026-08-28: T5 done. `R/interval_engine.R` computes Schmukle Eqs (10)-(12) and takes its reference statistics as an argument rather than reading a dataset, so a HiTOP-BR or PID-5 wrapper can hand it a different table without the engine learning about instruments; `R/interval_hitopsr.R` is the thin wrapper, signature as gated. Both mismatch branches carry their own condition class -- `hitop_interval_uncovered` and `hitop_interval_coding` -- so either can be caught alone and one `suppressWarnings()` silences both (D-025's posture, D-034 making the classes public). `validate_level()` joins `R/util.R`, reporting the type and the range separately so `level = 95` is told it wanted `0.95`. Hand-checked against the engine on `sim_hitopsr`: `hsr_agoraphobia` respondent 1 returns 2.714287 [2.149606, 3.278968], which is what Eqs (10)-(12) give by hand from `hitopsr_devstats`.
- 2026-08-28: amendment gate, AC3 and Coverage. AC3's citation year corrected 2025 -> 2026, the one word the M059 plan gate's citation correction missed; every other mention in this file, both references pages and D-043 already read 2026, and 2026 is the issue the article prints. Coverage for AC3 gains T6, the task that fires its two error branches. Adopted by Jeff as shown. A fresh-context [O] criteria audit ran in full mode over the amended AC3 before it was written -- the milestone's tier is user-facing, an exported function and an exported dataset -- and returned six findings, two questions returning nothing. Disposal: the two-branch collision is fixed in the code rather than in the criterion, since the criterion's reading that each situation reports its own condition is the right one (the engine had suppressed the coverage report on a coding mismatch); the `norm_pid5()` mirror stands, the shipped condition classes being stronger than what that function does; `level` being bound only by AC4 stands, AC4 sweeping two levels; the unasserted `c(1, 4)` reference coding is repaired in T6 and in the maintainer-run verifier rather than by widening AC3; and D-034(c)'s requirement that every exported catchable condition be named in a D-entry is met by D-044 below.
- 2026-08-28: D-044 written, naming `hitop_interval_uncovered` and `hitop_interval_coding` public per D-034(c). The engine now raises the coverage report whether or not the coding matched, so each situation is reported on its own as AC3 reads. `data-raw/verify_hitopsr_devstats.R` gains a fourth comparison: Table 1's Range column against the `c(1, 4)` coding the wrapper converts on -- the one number that function hardcodes and that nothing else traced back to the source. Its run reports Table 1 spanning [1, 4].
- 2026-08-28: T6 oracles and behavior tests landed; the AC6 wording guard waits on T7's artifacts. `cairn/ORACLES.md` holds four records and `cairn/DESIGN.md`'s Testing section declares it as the location. The coverage oracle's generative model was worked out against the source rather than assumed, and the first model tried was wrong: drawing the true score at the classical true score's spread makes Eq (12) conservative (0.967 at reliability 0.80) and Eq (7) nominal, the reverse of the source's Table 1. The model that reproduces the source draws the true score on the observed score's own metric, so the two share an SD and correlate at sqrt(rel); `cairn/references/schmukle2026.md` now states it in that form and says why the metric is load-bearing. Discrimination checked against four wrong estimators, all outside tolerance at reliability 0.61 and three of four inside it at 0.96 -- so the low-reliability cell carries the oracle, which the record says. The join checker's third probe was adjusted: with the exception set empty there is no member to delete, so the test builds the world where one exists (row gone and declared, checker silent) and deletes the declaration. Minor task edit. Full suite: 14786 passing, 0 failures, 4 skips.
- 2026-08-28: T7 and the rest of T6 done. NEWS entry, a Confidence Intervals section in `vignettes/hitopsr_scoring.Rmd` (with a pointer to it from the standard-error section, which already said those numbers give no interval), and a `Score Intervals` pkgdown section carrying the reference group in its `desc` -- a bare `contents:` row holds no prose, so the reference entry AC6 names had to become its own section for the statement to have somewhere to live. `pkgdown::check_pkgdown()` clean. `tests/testthat/test-interval-prose.R` cuts the one passage about this function out of each of the four artifacts, asserts `Development Sample 2`, a plain-words `development sample`, and `not a community norm` on each, and asserts every `N =` each states against the figure `?hitopsr_devstats` documents rather than a typed literal. Writing that guard found the changelog and the vignette naming the sample only by its proper noun; both now say in plain words that it is a development sample.
- 2026-08-28: `hitopsr_devstats` added to the `utils::globalVariables()` list in `R/hitop-package.R`, alongside the twelve datasets already there; without it `R CMD check` flags the lazy-loaded dataset `interval_hitopsr()` names as an undefined global. `devtools::check()` then 0 errors / 0 warnings / 0 notes, `devtools::test()` 14904 passing with 4 skips, `devtools::document()` no diff, `pkgdown::check_pkgdown()` clean, line-ending policy check passed. All seven tasks done; status review.
- 2026-08-28: /milestone-review — draft PR #66 opened, all seven criteria verified with fresh evidence, three-lens fan-out returned 11 findings (two lenses clean) plus 2 from the review session; none showed a criterion failing, so the return floor did not fire. Jeff approved with the five recommended fixes applied.

## Decisions

- 2026-08-28 (from RR05): `interval_hitopsr()` does not clamp its bounds to `srange`. On a strongly skewed scale a bound can fall outside the response range — RR05 works the case through for Conversion Symptoms at the floor — and clamping would stop the function computing the cited Eq (12), would force carve-outs in AC4's closed-form oracle, and would hide a discrepancy IP1's posture keeps visible. T6 pins the non-clamping so it reads as intended rather than as an oversight.
- 2026-08-28 (from RR05): the source's validating simulation chose item thresholds giving approximately normal item responses, so its coverage result does not certify the badly skewed HiTOP-SR scales. AC5's simulation oracle generates under that same linear-normal model and so verifies the implementation, not robustness to skew; T7's help-page limitation is what carries the difference, and neither is claimed to do the other's job.

## Review

Reviewed 2026-08-28 on branch `m041-hitopsr-score-intervals`, PR
https://github.com/jmgirard/hitop/pull/66. The default branch was fetched
first and is an ancestor of HEAD at `1710e25`, so no merge was needed and the
evidence below is from the branch as it will merge.

### Acceptance criteria

- **AC1 — pass.** `hitopsr_devstats` loads as 93 rows x 8 columns
  (`scale`, `camelCase`, `type`, `nItems`, `reliability`, `reliabilityType`,
  `mean`, `sd`), keyed by `camelCase` with zero duplicates; `type` splits 76
  scale / 17 subscale and `reliabilityType` is the constant `"alpha"` on every
  row, with `reliability` spanning 0.61 to 0.96. The four source-drawn cells
  are established as printed by AC2's cell-by-cell diff below, against the
  SHA-256 `data-raw/hitopsr_table1.R` pins, which the verifier reports as
  matching. The name map and its exception set live in
  `data-raw/hitopsr_devstats.R`, citing the `data-raw/verify_hitopsr_names.R`
  run; the set is empty, and empty is the finding. The join test at
  `tests/testthat/test-interval_hitopsr.R:52` iterates `hitopsr_scales` and
  `hitopsr_subscales` rather than a hand-written list and passes, and the probe
  at `:79` shows the checker naming a removed, a duplicated and an undeclared
  row.
- **AC2 — pass.** Fresh run of `Rscript data-raw/verify_hitopsr_devstats.R`,
  exit 0: source sha256 matches the pin; Table 1 found on pages 49-51; 101 data
  rows extracted (93 primary scales and subscales, 8 Superspectra and Spectra);
  372 cells compared against the CSV over 93 rows and 372 against the shipped
  `hitopsr_devstats` over 93 rows, with none differing; 93 rows compared on
  `type` against Table 1's indentation (17 indented); Table 1's Range column
  reported as spanning [1, 4]. The CI-runnable half is the item-count oracle at
  `test-interval_hitopsr.R:119`, which passes.
- **AC3 — pass.** Live call on `sim_hitopsr` returns `_est`, `_lo`, `_hi` per
  requested score column; `formals(interval_hitopsr)$level` is `0.95`. A score
  column with no reference row raises a warning of class
  `hitop_interval_uncovered` and returns `NA` in all three columns; a call with
  `srange = c(0, 3)` raises `hitop_interval_coding` and converts nothing. Both
  classes are named in D-044. The help page states Eqs (10)-(12) with the
  p. 821 anchor and states the subset-scoring limitation. Tests at `:439`,
  `:463`, `:484`, `:513`, `:538`, `:568`, `:609`, `:634` all pass.
- **AC4 — pass.** The closed-form oracle at `test-interval_hitopsr.R:172`
  passes: Situational Phobias (alpha 0.61) and Distress-Dysphoria (0.96), each
  at a score below, at and above its reference mean, at levels 0.95 and 0.80,
  with the arithmetic in comments, constants read off Table 1 rather than from
  the shipped table, and agreement to 1e-8. Recorded as O-001 in
  `cairn/ORACLES.md`, the location `cairn/DESIGN.md`'s Testing section declares.
- **AC5 — pass.** The simulation-coverage oracle at
  `test-interval_hitopsr.R:351` passes: true scores drawn from the reference
  population and observed scores generated under the model
  `cairn/references/schmukle2026.md` records, swept over reliabilities 0.61,
  0.84 and 0.96 (read from the shipped table) at levels 0.95 and 0.80, seed
  20260828 + cell index and 200,000 replications fixed in the test, tolerance
  stated as a four-standard-error Monte-Carlo bound. The companion at `:394`
  pins that conditional coverage at a fixed true score is not nominal.
  Recorded as O-003.
- **AC6 — pass.** `tests/testthat/test-interval-prose.R` passes all three
  tests: each of `man/interval_hitopsr.Rd`, `NEWS.md`, `_pkgdown.yml` and
  `vignettes/hitopsr_scoring.Rmd` carries "Development Sample 2", plain-words
  "development sample" and "not a community norm" in the passage about this
  function, every `N =` each states equals the 780 documented in
  `man/hitopsr_devstats.Rd`, and the guard is shown able to fail on a stripped
  phrase and a changed N.
- **AC7 — pass.** `devtools::test()`: 14904 passing, 0 failures, 0 warnings, 4
  skips. `devtools::document()`: no diff. `devtools::check()`: 0 errors, 0
  warnings, 0 notes. Re-run after the gate's fix-now work landed:
  `devtools::check()` again 0 errors / 0 warnings / 0 notes in 7m 14s, its test
  phase OK, and `devtools::document()` no diff.

### Consistency gate

- `cairn_validate.py`: exit 0, all checks passed, 22 advisory warnings (20
  pre-existing dangling legacy decision tokens from the pre-migration numbering; 2 references-staleness
  advisories on this milestone's two new source notes, logged as a finding
  below).
- No `DESIGN.md` IP/GP principle text changed -- the diff adds an oracle-records
  paragraph to the Testing section -- so `cairn_impact.py` was skipped.
- `r-package` profile `consistency-gate`: `devtools::document()` no diff;
  `NAMESPACE`, `man/` and `data/*.rda` all regenerated, none hand-edited;
  README.Rmd untouched so README.md is in sync; `pkgdown::check_pkgdown()`
  clean; `NEWS.md` carries the user-visible entry with no milestone numbers;
  no new top-level files needing `.Rbuildignore`; `devtools::check()` clean.
  Line-ending policy check passed.

### Findings

Three fresh-context lenses ran in parallel over distinct evidence bases (the
milestone's tier is user-facing and the diff touches executable surface, so the
full fan-out applied). The **[S] blame-history lens reported no findings**,
having checked every modified pre-existing file against its history; it
specifically cleared the `skip_without_rename_base()` change as strengthening
rather than weakening M059's guard, and confirmed the `hitopsr_table1.R` page-
range extraction is behaviour-preserving. The **[S] prior-review lens reported
no findings**: it found real prior-review evidence (RB05/RR05, on exactly these
files) and judged every applied recommendation implemented without
contradiction; the GitHub inline-comment probe returned `[]`, so that surface
contributed nothing. The **[O] diff-bug lens verified the math independently**
— Eqs (10)-(12) reproduced, the AC4 constants matched against the CSV, the
shipped table checked for NA/duplicates/type split, the `hsr_conversionSymptoms`
floor claim confirmed at lo = 0.754 — and reported eleven findings. Two more
were raised by the review session itself. **None demonstrates an acceptance
criterion failing, so the return floor did not fire.**

1. *"The AC6 wording guard does not scope the help-page passage
   (`tests/testthat/test-interval-prose.R:56`). `between()` cuts to the
   terminator `"\\section"`, but `man/interval_hitopsr.Rd` contains zero
   occurrences of the string `section` (verified), so `regexpr` returns -1 and
   the 'passage' is the entire remainder of the Rd — `\references`,
   `\examples`, everything. It passes, but not for the reason the file's
   comment states, and any future `N = ...` elsewhere in that Rd would silently
   enter the sample-size assertion. Separately, on that surface the
   `expect_match(passage, "development sample")` is satisfied by the cut's own
   anchor text (`"The reference group is a development sample"`), so it asserts
   nothing."* — **verified independently** (`grep -c 'section'
   man/interval_hitopsr.Rd` returns 0). AC6 as written still passes: the
   assertion does run over all four artifacts and the Rd does carry the wording.
   **Disposition: fix now.**
2. *"`validate_level()` rejects integer input with a self-contradicting message
   (`R/util.R:314`). `rlang::is_double(1L, n = 1)` is `FALSE` (verified), so an
   integer aborts with 'must be a single number. You supplied `<integer>` of
   length 1'. `validate_range()` next door uses `is_integerish`;
   `is.numeric`/`is_bare_numeric` would be the consistent predicate."* —
   **verified independently.** **Disposition: fix now.**
3. *"Empty `scores` produces a raw base-R error. Nothing rejects
   `scores = character(0)`; the engine then builds a 0x0 `out` and
   `cbind(data, out)` aborts with 'arguments imply differing number of rows: 3,
   0' (reproduced directly). Same shape as `norm_pid5()`, so a pre-existing
   family pattern rather than a regression, but the new function inherits
   it."* — **verified independently, and `norm_pid5()` reproduces the identical
   message**, so it is a family-wide gap the diff did not introduce.
   **Disposition: follow-up (candidate row, family-wide).**
4. *"Output-name collision aborts obscurely. With `append = TRUE` and an input
   already carrying `<score>_est`, `tibble::as_tibble()` raises 'Column name
   must not be duplicated' (reproduced). Also shared with `norm_pid5()`."* —
   **verified independently.** **Disposition: follow-up (same row as 3).**
5. *"The engine joins on `camelCase` but nothing asserts that key is unique.
   `hit <- match(stems, refstats$camelCase)` silently takes the first row; the
   AC1 join test checks duplicates on `scale` only (`join_duplicates()`). The
   shipped table is clean today, so this is a gap, not a defect."* — **verified
   independently** (0 duplicate `camelCase` shipped; the test's
   `join_duplicates()` reads `devstats$scale`). **Disposition: fix now** (one
   added assertion).
6. *"`Situational Phobias` is a subscale, and `score_hitopsr()` cannot produce
   it. `score_hitopsr()` scores only `hitopsr_scales$itemNumbers` (76 scales);
   the 17 `type == 'subscale'` rows of `hitopsr_devstats` have no column the
   package's own scoring emits. AC4/`cairn/ORACLES.md` O-001 call these 'two
   scales at the ends of the reliability range' — one is a subscale — and
   `interval_hitopsr()`'s opening line ('Scores are produced by
   `score_hitopsr()`') is not true for those 17 rows. The oracle itself is fine;
   it builds the column by hand."* — **verified independently**: 17 subscale
   rows, 0 of them in `hitopsr_scales$Scale`, and `score_hitopsr()` emits 76
   columns. AC4 is not falsified — it asks for the ends of *the shipped
   `reliability` range*, and 0.61 is that minimum — but the help page's opening
   sentence overstates. **Disposition: fix now** (the doc sentence); the wider
   question of whether the package should score subscales is out of scope.
7. *"The coverage oracle never exercises the wrapper or the shipped statistics.
   O-003 calls `interval_engine()` directly with synthetic `refstats` at M = 50,
   SD = 10, sweeping only the shipped reliabilities. Coverage is scale-invariant
   under the linear-normal model so it is mathematically sound, but AC5's 'drawn
   from the reference population' is met only in the reliability dimension, and
   the wiring of `hitopsr_devstats$mean`/`$sd` into the right formula slots is
   pinned solely by O-001."* — **Disposition: reject.** Coverage is invariant to
   the location and scale of the metric under this model, so reliability is the
   only dimension in which the sweep can carry information; AC5 names the
   reliability sweep and the measurement model, both of which are met, and O-001
   pins the wiring against Table 1's own constants.
8. *"Warning style diverges from the `norm_pid5()` mirror AC3 names. The
   interval warnings pass `call =`, so they print `In interval_hitopsr() : ...`;
   `norm_pid5()`'s do not pass `call` and print bare. Cosmetic, but the two
   functions are meant to read alike."* — **Disposition: reject.** The classed,
   `call`-carrying conditions are the deliberate improvement D-044 records; the
   divergence runs in the direction the decision chose.
9. *"`hitopsr_devstats` is filed under the new 'Score Intervals' pkgdown
   section, whereas its closest precedent `pid_norms` sits under 'Instrument
   Data'. Deliberate per the work log (the section exists so the `desc` has
   somewhere to live), but it is an index-organisation departure worth a
   conscious sign-off."* — **Disposition: maintainer's call at the gate.**
10. *"Loose watermark filter in the maintainer extractor
    (`data-raw/hitopsr_table1.R`): `grepl(t, "ForPeerReview", fixed = TRUE)`
    uses the token as the *pattern* and the phrase as the *subject*, so any
    substring — `"e"`, `"or"`, `"view"` — is accepted as watermark rather than
    reported as stray text. Weakens the 'nothing lost in silence' guard."* —
    **verified independently** (`data-raw/hitopsr_table1.R:289`). Maintainer
    tooling only; the verifier run above reported no stray text and matched all
    372 cells. **Disposition: follow-up onto the standing `data-raw/`
    maintainer-tooling candidate row.**
11. *"Nits. `sim_coverage()` names a parameter `mean` and calls `mean()` in the
    same body (works only because R skips non-function bindings during call
    lookup); `table1$name[!is.na(hit)] <- label_map$package[stats::na.omit(hit)]`
    in `data-raw/hitopsr_devstats.R` would read plainer as `hit[!is.na(hit)]`;
    the prose guard's NEWS cut (`"* **Two HiTOP-SR scales"` as terminator)
    breaks if an entry is inserted between the two; `hitopsr_table1_rows()` now
    shells out to `pdftotext` twice."* — **Disposition: reject** as style
    nitpicks under the out-of-scope taxonomy, except the NEWS-cut fragility,
    which is folded into finding 1's fix.
12. *(review session)* `cairn/references/simms2026.md` quotes the paper's p. 24
    prose — "The remaining 16 scales (17.2%) fell below this threshold, with
    most of these alphas still in the acceptable range (between .66 and .79)",
    naming Blood-Injection Phobia (.66), Trichotillomania (.67) and Purging
    (.66) as the lowest — without recording that Table 1 prints .61 for
    Situational Phobias, below all three. The shipped table is correct: the
    independent extraction confirms Table 1 prints 0.61, and exactly 16 rows
    fall below .80 as the prose says. So the source contradicts itself on its
    own lowest alpha, and the note does not say so. This is material because
    0.61 anchors both O-001 and the discriminating cell of O-003.
    **Disposition: fix now** (an Open-questions bullet, IP1's posture).
13. *(review session)* `cairn_validate.py` raises two `references staleness`
    advisories on the new source notes: `schmukle2026.md` "provenance records no
    extraction status" (the note does say `Extraction: verified 2026-08-28`, but
    across a line break the checker does not read), and `simms2026.md`
    "extraction records no verified re-check against the source" — whose text
    still reads "have not yet been re-read against the machine extraction",
    which stopped being true once `data-raw/verify_hitopsr_devstats.R` ran and
    matched. **Disposition: fix now** (`simms2026.md`'s stale sentence; the
    `schmukle2026.md` advisory is a checker line-wrap artifact and is left).

### Triage outcome

Put to the maintainer at the approval gate, 2026-08-28. Jeff selected "apply the
five fixes, then merge", and — on finding 9, put as its own question — chose to
leave `hitopsr_devstats` indexed under the pkgdown "Score Intervals" section
rather than moving it beside `pid_norms` under "Instrument Data", the section's
`desc` being where AC6's development-sample statement lives.

Fixed on the branch after the gate, before the approval marker:

- **1** — `between()` in `tests/testthat/test-interval-prose.R` now asserts the
  terminator was found as well as the anchor, and drops the anchor from the
  passage it returns, so no assertion can be answered by the words that located
  it. The help-page cut runs between the Rd's two bold headings; the changelog
  cut terminates on the start of whatever the next entry is (`"* **"`) rather
  than on that entry's wording, which also closes finding 11's NEWS fragility.
  Dropping the anchor made the help page's plain-words assertion fail honestly —
  the phrase lived only in the heading — so the roxygen paragraph now states it
  in the body: "That is a development sample and not a community norm."
- **2** — `validate_level()` takes `rlang::is_bare_numeric` in place of
  `rlang::is_double`, so `level = 1L` is reported as out of range rather than as
  the wrong type; the new case is pinned in the level-validation test.
- **5** — the AC1 join test now asserts `hitopsr_devstats$camelCase` carries no
  duplicate, the key the engine actually joins on.
- **6** — `interval_hitopsr()`'s opening paragraph no longer says scores come
  from `score_hitopsr()` unqualified: it names the 76 scale columns that function
  produces and says the 17 subscale rows have no column it emits.
- **12** — `cairn/references/simms2026.md` gains an Open-questions bullet
  recording that the paper's p. 24 prose names .66 as the lowest alpha while its
  own Table 1 prints .61 for Situational Phobias, with the extraction that
  confirms the cell and the two oracles the constant anchors.
- **13** — the same file's provenance block, which still said the transcription
  had not been re-read against the machine extraction, now records the M041
  review's run and its counts. `cairn_validate.py`'s references-staleness
  advisories drop from 2 to 1; the remaining one is a line-wrap artifact on
  `schmukle2026.md`, which does state `Extraction: verified`.

Filed as follow-ups (findings 3, 4, 10) and rejected (7, 8, 11) as recorded
above.
