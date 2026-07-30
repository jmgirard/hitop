# M26: PID-5 norming and score conversion (raw ↔ T ↔ percentile)

- **Status:** planned
- **Priority:** normal
- **Depends on:** M25
- **Driving RR:** —
- **Principles touched:** IP1, IP2, IP3, IP4, GP1, GP2, GP3
- **Branch/PR:** —

## Goal

Convert PID-5 / SF / BF scale scores to normative T scores and percentiles, and back,
from the `pid_norms` tables shipped by M25.

## Scope

**In:** `norm_pid5()` over scored columns; the vectorized conversion primitives it wraps;
capping and unnormed-scale behavior; official-range reconciliation at lookup (absorbs the
legacy-M16 candidate); the PID-5-BF total scale in `score_pid5()` per D-017; roxygen,
three vignette sections, NEWS, tests.

**Out:** facet-level norms (no published tables) and sex/age-stratified norms → ROADMAP
candidate rows. HiTOP-SR/BR norms → candidate row. Profile plots and rendered individual
reports → the existing "Clinical reporting & release" candidate. Any prose about what a
T score means (IP4).

## Acceptance criteria

- [ ] **AC1.** `score_pid5(version = "BF")` returns a total-score column (`prefix` +
      `total`) alongside its five domains per D-017; its scoring rule — item-level mean
      versus mean of the five domain means, and its behavior under `missing = "apa"` —
      is documented in `@details`, traced to a cited source in `cairn/SOURCES.md`, and
      covered by a hand-computed oracle test; FULL and SF output are unchanged, shown by
      the existing `tests/testthat/test-score_pid5.R` expectations passing untouched.
- [ ] **AC2.** `norm_pid5()` takes a data frame of PID-5 scale scores plus a `scores`
      argument naming the score columns (mirroring the scoring family's `items`), under
      the signature `(data, scores, version, srange, prefix, append = TRUE)`, and returns
      a tibble carrying a `_ptl` column for every named column `pid_norms` covers for
      that `version`, and a `_t` column for every covered column whose `pid_norms` rows
      carry a T (the three domain tables; the validity tables are score→percentile only
      and get no `_t` column).
- [ ] **AC3.** A named score column that `pid_norms` does not cover for that version (for
      example any of the 25 PID-5 facets) yields `NA` conversion columns plus one `cli`
      message naming those scales — never an error, and never a silently absent column.
- [ ] **AC4.** Conversion is verified against the shipped tables rather than against the
      function's own output: for every domain scale in `pid_norms`, T → raw reproduces
      every printed row exactly, and raw → T → raw is the identity on every printed row;
      for every validity scale, each printed score converts to that row's percentile.
- [ ] **AC5.** A raw score above the highest printed row for its scale returns that row's
      T score rather than an extrapolated value, and `norm_pid5()` emits one `cli`
      warning naming how many observations were capped; a raw score falling between two
      printed rows, or matching a raw value printed at several T scores, converts by the
      lookup and tie rules documented in the function's `@details` — either the rules the
      book states, cited by page, or, where the book states none, the rules recorded with
      their rejected alternatives as a milestone-local decision.
      *(RB tripwire: no-oracle)*
- [ ] **AC6.** When `srange` implies an option count other than the PID-5's official four,
      `norm_pid5()` returns `NA` for every conversion column and warns naming the
      mismatch; when the count is four but the coding is shifted (for example 1–4 against
      the official 0–3), scores are rescaled to the official range before lookup and the
      rescaling is reported once via `cli::cli_alert_info()`.
- [ ] **AC7.** `vignettes/pid5_scoring.Rmd`, `pid5sf_scoring.Rmd`, and `pid5bf_scoring.Rmd`
      each gain a norming section that executes against the package's example data;
      NEWS.md records `norm_pid5()` and the BF total; `devtools::document()` leaves the
      tree clean and `devtools::check()` reports 0 errors and 0 warnings with every NOTE
      justified in this file's Review section.

## Coverage

- AC1 → T1
- AC2 → T4
- AC3 → T4, T6
- AC4 → T3, T6
- AC5 → T2, T3, T6
- AC6 → T5, T6
- AC7 → T7

## Tasks

- [ ] **T1.** Add the BF total scale to `pid_scales` / `score_pid5()` per D-017 with a
      hand-computed oracle test and a NEWS line; confirm FULL/SF output is untouched.
- [ ] **T2.** Read the book for its stated lookup rule (between printed rows) and tie rule
      (a raw value printed at several T scores); where it states none, record the chosen
      rules and their rejected alternatives in this file's Decisions section.
- [ ] **T3.** Implement the conversion primitives over `pid_norms` — T → raw, raw → T,
      raw → percentile — applying T2's rules.
- [ ] **T4.** Implement `norm_pid5()`: signature, per-scale `_t`/`_ptl` columns, the
      unnormed-scale message.
- [ ] **T5.** Implement the official-range reconciliation — `srange` option-count check
      and shifted-coding rescale with its `cli_alert_info()` report.
- [ ] **T6.** Tests: printed-row conversions and round-trips, capping, unnormed scales,
      both `srange` branches, and the R edge cases the profile's test-doctrine names.
- [ ] **T7.** Roxygen with the cited lookup rule, three vignette norming sections,
      `_pkgdown.yml` "Score" entry, NEWS; run `document()` / `test()` / `check()`.

## Work log

- 2026-07-30: created by /milestone-plan.
- 2026-07-30: plan gate chose to add a PID-5-BF total to `score_pid5()` (D-017) over shipping the book's TOT norm rows as unusable data or dropping them, because the book publishes a normed total and a norm with nothing to convert is dead weight; falsified by the book defining the total on a metric `score_pid5()` cannot produce.
- 2026-07-30: plan chose `cli::cli_alert_info()` for the shifted-coding rescale over the silent rescale the legacy-M16 note proposed, because GP1 requires deviations to be loud and the rescale changes the numbers a user reads; falsified by the message proving noisy enough on ordinary data that users suppress it.
- 2026-07-30: plan absorbed the `Norm-ready response ranges (legacy M16)` candidate row into AC6 rather than planning it separately, because the rescale has no home outside a norming lookup (D-012 deferred it here explicitly).

## Decisions

## Review
