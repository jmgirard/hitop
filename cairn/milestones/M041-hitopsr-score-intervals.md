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

- [ ] AC1 `hitopsr_devstats` ships one row per HiTOP-SR primary scale and
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
- [ ] AC2 `data-raw/verify_hitopsr_devstats.R` extracts Table 1 from the source
      file on the gitignored `references/sources/` shelf and reports zero
      differing cells against both the committed CSV and the built
      `hitopsr_devstats`; its run output is recorded in the Review. Because
      that script cannot run in CI, a second, CI-runnable oracle asserts for
      every joined row that Table 1's item count equals that scale's `nItems`
      in `hitopsr_scales`/`hitopsr_subscales`. Where the source's format
      admits no deterministic extraction, an independent second transcription
      diffed against the first stands in for the extraction half.
- [ ] AC3 `interval_hitopsr()` returns `_est`, `_lo`, and `_hi` per requested
      score column at a `level` argument defaulting to 0.95, and its help page
      states the estimate and bound formulas with a page anchor into Schmukle
      (2026). A score column with no `hitopsr_devstats` row, and a call whose
      `srange` is not the response coding Table 1's mean and SD are printed on,
      each return `NA` in all three columns and report it as its own catchable
      `cli::cli_warn()` condition, mirroring `norm_pid5()`. Subset-scored
      columns are not detectable and are not claimed to be: the help page
      states that a scale scored from fewer than its full items is not
      comparable to the reference statistics.
- [ ] AC4 A test recomputes the estimate and both bounds in hand-written
      arithmetic, the arithmetic in comments, for two scales at opposite ends
      of the shipped `reliability` range, each at a score below, at, and above the
      reference mean, and at two confidence levels, matching the function's
      output to within 1e-8 (closed-form oracle). Each oracle is recorded by
      id, type, and asserting `test:line` at the location `cairn/DESIGN.md`
      declares for oracle records.
- [ ] AC5 A test asserts *marginal* coverage — true scores drawn from the
      reference population and observed scores generated from them under the
      measurement model the ingested Schmukle (2026) source note records, never
      a single fixed true score, whose conditional coverage a mean-shrunken
      estimator does not promise — swept over the lowest, median, and highest
      `reliability` in `hitopsr_devstats` and two nominal levels, with seed and
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

## Decisions

- 2026-08-28 (from RR05): `interval_hitopsr()` does not clamp its bounds to `srange`. On a strongly skewed scale a bound can fall outside the response range — RR05 works the case through for Conversion Symptoms at the floor — and clamping would stop the function computing the cited Eq (12), would force carve-outs in AC4's closed-form oracle, and would hide a discrepancy IP1's posture keeps visible. T6 pins the non-clamping so it reads as intended rather than as an oversight.
- 2026-08-28 (from RR05): the source's validating simulation chose item thresholds giving approximately normal item responses, so its coverage result does not certify the badly skewed HiTOP-SR scales. AC5's simulation oracle generates under that same linear-normal model and so verifies the implementation, not robustness to skew; T7's help-page limitation is what carries the difference, and neither is claimed to do the other's job.

## Review
