# M28: PID-5 norming under shifted response codings, and the vignette norming sections

- **Status:** in-progress
- **Priority:** normal
- **Depends on:** M27
- **Driving RR:** —
- **Principles touched:** IP2, IP4, GP1, GP2, GP3
- **Branch/PR:** `m28-pid5-norming-shifted-codings` / —

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

- [ ] **AC1.** An `srange` implying an option count other than the PID-5's official four
      returns `NA` in every conversion column with one `cli` warning naming the mismatch —
      never a partial conversion, never an error. This is not D-020's rejected blanket-`NA`
      option: no mapping is defined from a k≠4 metric onto a four-option norm table.
- [ ] **AC2.** A shifted four-option coding (1–4 against the official 0–3, say) is
      reconciled per scale before lookup: item means shifted by `low`, `PRD` by
      `low × nItems`, and `INC`/`INCS`/`ORS` left unchanged as coding-invariant (D-023,
      superseding D-020's `ORS` clause). The M27 row-selection primitive is not changed.
- [ ] **AC3.** Each of the two adjustments and each of the three no-ops is documented in
      `@details` on its merits (GP1's no-published-rule branch — no source states them),
      with `ORS`'s invariance traced to `R/validity_pid5.R:153`, and with the standing
      consequence that `validity_pid5()`'s cut scores are *not* reconciled (DESIGN Known
      issue #3), so one session can pair a reconciled percentile with an unreconciled flag.
- [ ] **AC4.** The reconciliation is reported once per call via `cli::cli_warn()` — a
      catchable condition, matching what `validity_pid5()` already emits on the same input
      — naming which scales were adjusted and which were not; nothing fires on the official
      coding. M27's AC6 guard is removed in the same change and NEWS records the behavior
      change (GP2).
- [ ] **AC5.** Tests cover: the non-four count returning all-`NA` with its warning; for each
      of the two adjusted metrics a hand-computed fixture showing a 1–4 score reconciling to
      the `_t`/`_ptl` its 0–3 equivalent produces; and for each of the three no-ops an
      assertion that the score is unchanged by reconciliation. Expected values come from the
      shifted arithmetic and the printed `pid_norms` cells, never the function's own output
      (IP2), and are version-pinned — `INC`/`ORS`/`PRD` exist only under FULL, `INCS` only
      under SF.
- [ ] **AC6.** `vignettes/pid5_scoring.Rmd`, `pid5sf_scoring.Rmd`, and `pid5bf_scoring.Rmd`
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
- [ ] **T4.** `@details` for the formulas and the Known-issue-#3 consequence; the three
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
- 2026-07-30: plan gate chose to supersede D-020's `ORS` clause (D-023) over planning to the four-formula text, because `R/validity_pid5.R:153` already counts `ORS` against `srange[[2]]` and the function receives scores rather than items; falsified by an `ORS` coding shift that changes the count — none exists while the scale is defined as a count at the range maximum.

## Decisions

## Review
