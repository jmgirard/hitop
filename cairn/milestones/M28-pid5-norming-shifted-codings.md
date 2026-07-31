# M28: PID-5 norming under shifted response codings, and the vignette norming sections

- **Status:** review
- **Priority:** normal
- **Depends on:** M27
- **Driving RR:** —
- **Principles touched:** IP2, IP4, GP1, GP2, GP3
- **Branch/PR:** `m28-pid5-norming-shifted-codings` / https://github.com/jmgirard/hitop/pull/31

## Goal

Let `norm_pid5()` convert scores collected on a non-official four-option coding by
reconciling each scale to the official 0–3 range per D-020 and D-023, and show the norming
workflow in the three PID vignettes.

## Scope

**In:** the option-count check; the two per-metric rescale branches and the three
coding-invariant no-ops; the `cli` report naming what was adjusted; replacing M27's `srange`
guard; `@details` for the formulas; the three vignette norming sections; NEWS, tests.

**Out:** the conversion itself — the selection primitives, capping, and the RR02 rules stay
M27's and are unchanged here. `validity_pid5()`'s cut-score adaptation → stays deferred as
DESIGN Known issue #3, which this milestone documents but does not close. Any prose about
what a T score means (IP4) — the vignette sections show the workflow, never an
interpretation.

## Acceptance criteria

- [x] **AC1.** An `srange` implying an option count other than the PID-5's official four
      returns `NA` in every conversion column with one `cli` warning naming the mismatch —
      never a partial conversion, never an error. This is not D-020's rejected blanket-`NA`
      option: no mapping is defined from a k≠4 metric onto a four-option norm table.
- [x] **AC2.** A shifted four-option coding (1–4 against the official 0–3, say) is
      reconciled per scale before lookup: item means shifted by `low`, `PRD` by
      `low × nItems`, and `INC`/`INCS`/`ORS` left unchanged as coding-invariant (D-023,
      superseding D-020's `ORS` clause). The M27 row-selection primitive is not changed.
- [ ] **AC3.** Each of the two adjustments and each of the three no-ops is documented in
      `@details` on its merits (GP1's no-published-rule branch — no source states them),
      with `ORS`'s invariance traced to `R/validity_pid5.R:153`, and with the standing
      consequence that `validity_pid5()`'s cut scores are *not* reconciled (DESIGN Known
      issue #3), so one session can pair a reconciled percentile with an unreconciled flag.
- [x] **AC4.** The reconciliation is reported once per call via `cli::cli_warn()` — a
      catchable condition, matching what `validity_pid5()` already emits on the same input
      — naming which scales were adjusted and which were not; nothing fires on the official
      coding. M27's AC6 guard is removed in the same change and NEWS records the behavior
      change (GP2).
- [x] **AC5.** Tests cover: the non-four count returning all-`NA` with its warning; for each
      of the two adjusted metrics a hand-computed fixture showing a 1–4 score reconciling to
      the `_t`/`_ptl` its 0–3 equivalent produces; and for each of the three no-ops an
      assertion that the score is unchanged by reconciliation. Expected values come from the
      shifted arithmetic and the printed `pid_norms` cells, never the function's own output
      (IP2), and are version-pinned — `INC`/`ORS`/`PRD` exist only under FULL, `INCS` only
      under SF.
- [x] **AC6.** `vignettes/pid5_scoring.Rmd`, `pid5sf_scoring.Rmd`, and `pid5bf_scoring.Rmd`
      each gain a norming section executing against the package's example data;
      `devtools::document()` leaves the tree clean and `devtools::check()` reports 0 errors
      and 0 warnings, with every NOTE justified in this file's Review section.

## Coverage

- AC1 → T1, T3
- AC2 → T2, T3
- AC3 → T4
- AC4 → T1, T2, T3
- AC5 → T3
- AC6 → T4

## Tasks

- [x] **T1.** Replace M27's `srange` guard: the option-count check, the all-`NA` path, and
      the `cli::cli_warn()` report.
- [x] **T2.** Implement the two per-metric rescale branches and assert the three no-ops.
- [x] **T3.** Tests per AC5 — the count branch, two adjusted-metric fixtures, three
      invariance assertions.
- [x] **T4.** `@details` for the formulas and the Known-issue-#3 consequence; the three
      vignette norming sections; NEWS; run `document()` / `test()` / `check()`.

## Work log

- 2026-07-30: created by /milestone-plan, split from M27 after the RR02 ingest pushed that file 20 lines over the plan-owned cap with 63 of those lines frozen; M27 keeps the conversion and the ten binding criteria, this milestone takes the shifted-coding reconciliation and the vignette sections.
- 2026-07-30: the pre-write criteria audit ([O], fresh context) returned that D-020's `ORS` clause describes something `norm_pid5()` cannot perform, that M27 would otherwise ship `srange` inert, and that `cli_alert_info()` is not a catchable condition; all three went to the plan gate and were settled there (D-023, M27 AC6, AC4's `cli_warn()`).
- 2026-07-30: plan gate chose `cli::cli_warn()` over the `cli::cli_alert_info()` the pre-split plan had picked, because `validity_pid5()` already warns on the same shifted coding and an info message is not a catchable condition for tests to assert; falsified by the warning proving noisy enough on ordinary shifted-coding data that users suppress it.
- 2026-07-30: implement gate chose `cli::cli_warn()` for AC1's option-count refusal too (AC1 said only "one `cli` warning"), so both shifted-coding reports in this one function are the same catchable mechanism rather than one warning and one printed alert.
- 2026-07-30: implement gate chose vignette norming sections on the official coding only, with a prose pointer to `?norm_pid5` for shifted codings — the three `sim_pid5*` datasets are all 0-3, so a shifted demo would need data invented for the demo.
- 2026-07-30: T1+T2+T3 landed in one commit — T1's option-count check alone would have left a shifted four-option coding converting unreconciled for one commit, a worse state than M27 shipped; AC5's tests are the tests for both branches, so they came with them.
- 2026-07-30: metric classification is by scale name in `norm_metric()` (`PRD` sums, `INC`/`INCS`/`ORS` invariant, everything else an item mean); `PRD`'s 22-item count is read from `pid_items` at run time, never hardcoded.
- 2026-07-30: AC3's `ORS` trace is split — `@details` states the invariance from how `validity_pid5()` computes the score (a line number would rot in a shipped man page), and the literal `R/validity_pid5.R:153` pointer sits in `norm_metric()`'s source comment.
- 2026-07-30: T4's `@details` half landed with T1-T3 rather than in T4's commit, so no commit ships behavior whose documented contract contradicts it.
- 2026-07-30: T4 — norming sections added to all three PID vignettes (FULL on `sim_pid5` + `validity_pid5()` output, SF on the real `ku_pid5sf` data, BF on `sim_pid5bf`), NEWS's `norm_pid5()` bullet rewritten to the shipped coding behavior; `document()` no diff, `test()` 10283 pass / 0 fail, `check()` 0 errors / 0 warnings / 0 notes.
- 2026-07-30: review fixed the two actioned findings (F8 the vignette's "four validity scales" claim, false for FULL; F2 `@details` overstating when the reconciliation warning fires) plus F3 alongside F2 (the report claimed a reconciliation on an all-invariant request); two new tests lock both, suite 10288 pass / 0 fail.
- 2026-07-30: plan gate chose to supersede D-020's `ORS` clause (D-023) over planning to the four-formula text, because `R/validity_pid5.R:153` already counts `ORS` against `srange[[2]]` and the function receives scores rather than items; falsified by an `ORS` coding shift that changes the count — none exists while the scale is defined as a count at the range maximum.

## Decisions

## Review

Reviewed 2026-07-30 on `m28-pid5-norming-shifted-codings` @ 1448833, PR #31.

### Acceptance-criteria evidence

- **AC1** — `norm_pid5()` on `srange = c(0, 4)` (5 options) and `c(2, 3)` (2 options), four FULL
  score columns: each returned exactly **1** warning, all conversion columns `NA`, correct
  column count, no error raised. Warning text names the mismatch ("implies 5 response options,
  but the PID-5 normative tables are built on the official four-option 0-3 coding"). Locked by
  `test_that("a coding with a different option count converts nothing")`.
- **AC2** — one call on `srange = c(1, 4)` with four FULL columns, each checked against a value
  derived outside the function. `pid_detachment` 2.20 → 2.20 − 1 = 1.20 → nearest printed row
  raw 1.19 → returned **T 58, ptl 0.80**, matching that row. `pid_PRD` 52 → 52 − (1 × 22) = 30 →
  returned **0.971**, the cell printed at raw 30. `pid_ORS` 2 → returned **0.993** and `pid_INC`
  12 → returned **0.83**, the cells printed at those raws unshifted. Second clause: `git diff
  --numstat main..HEAD -- R/norm_engine.R` is **38 added, 0 deleted** — `norm_select()`,
  `norm_convert()`, `norm_t_to_raw()`, and `norm_capped()` are byte-identical to M27.
- **AC4** — the `srange = c(1, 4)` call above raised exactly **1** warning, whose text names
  both groups: `Adjusted: "pid_detachment" and "pid_PRD"` · `Left unchanged as
  coding-invariant: "pid_ORS" and "pid_INC"`, plus an `i` bullet on the unreconciled
  `validity_pid5()` cut scores. The same four columns on `srange = c(0, 3)` raised **0**
  warnings. Both reports are `cli::cli_warn()` (catchable — the tests capture them via
  `withCallingHandlers`), matching `R/validity_pid5.R:92`. M27's guard is gone from the diff:
  the `official <-` binding and its `cli_alert_warning()`/`cli_alert_info()` pair are deleted
  lines. NEWS's `norm_pid5()` bullet is rewritten to the shipped behavior, including the
  unreconciled-cut-score consequence.
- **AC5** — six new `test_that()` blocks: the option-count branch (5-option and 2-option), an
  item-mean fixture, a `PRD` fixture, the three no-ops in one loop, the report's content, and
  official-coding silence. Expected values are read from `pid_norms` cells or hand arithmetic
  stated in comments; the fixtures also pin their own arithmetic (`expect_equal(row$raw, 1.18)`,
  `expect_equal(sum(!is.na(pid_items$PRD)), 22L)`) and assert each looked-up cell is unique
  before using it. No expectation is taken from `norm_pid5()`'s output (IP2). Version-pinned:
  `INC`/`ORS`/`PRD` cases run `version = "FULL"`, `INCS` runs `"SF"`, the item-mean fixture
  `"BF"`. Full file: 0 failures.
- **AC3** — *not ticked; one clause unmet as written.* `man/norm_pid5.Rd`'s "Response coding"
  section documents all five treatments on their merits, each from the scale's own definition
  and flagged as this package's rather than Markon et al.'s: item means (shift by
  `srange[[1]]`), `PRD` (`srange[[1]]` × its items, count read from `pid_items`), `INC`/`INC-S`
  (a constant cancels in a within-pair difference), and `ORS` (a count against `srange[[2]]`,
  which moves with the range). The Known-issue-#3 consequence is a closing paragraph stating
  that a respondent on a 1-4 coding can get a reconciled percentile from `norm_pid5()` and an
  unreconciled flag from `validity_pid5()` in the same session. **Unmet:** AC3 asks for `ORS`'s
  invariance "traced to `R/validity_pid5.R:153`"; `@details` traces it to `validity_pid5()` at
  function granularity, and the literal line pointer sits in `norm_metric()`'s source comment
  in `R/norm_engine.R` instead — the implementer's reason being that a line number in a shipped
  man page rots on the next edit to that file. Not reinterpreted review-side; referred to the
  maintainer at the merge gate.
- **AC6** — a `## Normative Scores` section exists in all three files
  (`pid5_scoring.Rmd:85`, `pid5sf_scoring.Rmd:91`, `pid5bf_scoring.Rmd:57`), each calling
  `norm_pid5()` on package example data — `sim_pid5` plus `validity_pid5()` output for FULL,
  the real `ku_pid5sf` for SF, `sim_pid5bf` for BF. `devtools::document()` re-run at review
  left the tree clean (only this tracking file modified). Fresh `devtools::check(document =
  FALSE)` at review: **0 errors, 0 warnings, 0 notes** — no NOTE to justify. Vignettes rebuild
  inside that check, so the sections are executed evidence, not just present text.

### Consistency gate

- `cairn_validate.py` exit 0 — every CHECK PASS. 20 advisory `dangling id tokens` WARNs, all
  pre-existing in `cairn/DESIGN.md` and `cairn/SOURCES.md` (legacy D-001–D-012 live in
  DESIGN's own decision log; M13/M14 are pre-migration). None introduced here.
- No `DESIGN.md` principle changed → `cairn_impact.py` not applicable.
- Toolchain slot (`r-package`): `document()` no diff · generated files regenerated not
  hand-edited · README.Rmd/README.md untouched and in sync · `pkgdown::check_pkgdown()` "No
  problems found" · NEWS.md entry present · no new top-level files · full `check()` clean.
- CI on PR #31: all 7 checks pass (ubuntu release/devel/oldrel-1, macOS, Windows, pkgdown,
  test-coverage).

### Independent review

Three fresh-context lenses, then a Sonnet scorer that did not generate the findings.

- **Prior-review lens: 0 findings.** Confirmed M27's five fixed findings all still intact
  (clamping, the `@details` low-end text, per-observation capping, both above-table test
  gaps), and that the four sub-threshold M27 findings parked as a ROADMAP candidate are
  untouched — neither worsened nor accidentally fixed.
- **Blame-history lens:** the per-observation capping fix is intact (only its guard variable
  was renamed); the deleted `srange`-refusal test was superseded by intent, not lost; the
  `official` → `usable`/`shifted` split matches D-023's corrected partition.
- **Diff-bug lens: 16 findings.** It also built an oracle the suite lacks — `sim_pid5[1:20, ]`
  with +1 added to every item, scored under each coding — and found the 0-3 and 1-4 conversion
  columns byte-identical across all five domains, `INC`, `ORS`, `PRD`, and the BF total, with
  measured raw deltas of exactly +1 per mean, 0 for `INC`/`ORS`, +22 for `PRD`. Reverse-keying
  survives a shift (`reverse()` is `low + high - x`). Negative `low` (`c(-2, 1)`) reproduces
  the official result exactly. No arithmetic defect found.

**Actioned (score ≥ 80), both fixed on the branch:**

- **F8 (88)** — `vignettes/pid5_scoring.Rmd` claimed the FULL tables cover "the four validity
  scales"; `pid_norms` carries three under FULL (`INC`, `ORS`, `PRD`) and none for `SD-TD`. A
  reader would have expected `pid_SDTD` to convert. Prose corrected to name the three and say
  SD-TD is not normed.
- **F2 (80)** — `@details` said the reconciliation warning fires "whenever `srange` is a
  shifted coding", but the code guards on `shifted && any(covered)`, so a facets-only request
  on a 1-4 coding said nothing about coding at all. `@details` rewritten to the actual
  condition.

**Fixed alongside F2** (same message surface; the scorer flagged them as one repair):

- **F3 (78)** — the report's headline said scores "were reconciled" even when every requested
  scale was coding-invariant and nothing had been adjusted. The headline is now conditional on
  a non-empty adjusted set, reading "needed no reconciliation" otherwise.

Both fixes are locked by new tests (all-invariant request reports no adjustment; uncovered-only
request raises the coverage message and zero warnings). Re-verified after the fixes:
`document()` no diff, suite 10288 pass / 0 fail, `pid5_scoring.Rmd` re-renders,
`devtools::check()` again **0 errors, 0 warnings, 0 notes**, `cairn_validate` exit 0.

**Logged below threshold (13), not actioned:** F7 (72) the shipped `cli_warn()` contradicts
D-020's `cli_alert_info()` text with no D-entry — raised at the merge gate · F9 (68) two
vignettes describe an uncovered-scale message their chunks never trigger · F13 (62) untested
interaction paths (negative `low`; a `k≠4` coding that is also shifted) · F10 (50) `INC-S` vs
`INCS` in one man page · F12 (45) the test helper `capture_warnings()` shadows a `testthat`
export with different semantics · F6 (40) two condition classes coexist in the function, so no
single suppressor silences it · F14 (40) AC5's fixtures check the stated formula rather than
round-tripping against `score_pid5()` · F4 (30) `PRD`'s 22-item correction depends on
`validity_pid5()` summing without `na.rm`, unasserted · F1 (25) `norm_metric()` defaults
unclassified scales to `"mean"` rather than failing loud · F5 (25) a `k≠4` coding also
suppresses the coverage report · F11 (25) `@details` states PRD's item count in prose · F15
(25) one differential assertion is weaker than the printed table allows · F16 (3) premise
stale — the reviewer read the milestone file before this Review section existed.
