# M27: PID-5 norming and score conversion (raw ↔ T ↔ percentile)

- **Status:** blocked
- **Priority:** normal
- **Depends on:** M26
- **Driving RR:** —
- **Principles touched:** IP2, IP3, IP4, GP1, GP2, GP3
- **Branch/PR:** —

## Goal

Convert PID-5 / SF / BF scale scores to normative T scores and percentiles, and back, from
the `pid_norms` tables shipped by M25.

## Scope

**In:** `norm_pid5()` over scored columns; the vectorized conversion primitives it wraps;
out-of-table and above-ceiling behavior; unnormed-scale handling; per-scale reconciliation
to the official response range (D-020, absorbing the legacy-M16 candidate); roxygen, three
vignette sections, `_pkgdown.yml`, NEWS, tests.

**Out:** the BF total scorer and its `pid_scales` row → M26. Facet-level and sex/age
stratified norms, IRF norms, and HiTOP-SR/BR norms → their ROADMAP candidate rows. Profile
plots and rendered individual reports → the "Clinical reporting & release" candidate.
`validity_pid5()`'s cut-score adaptation under shifted codings → stays deferred as DESIGN
Known issue #3 (D-020 changes the norming lookup only). Any prose about what a T score
means (IP4).

## Acceptance criteria

- [ ] **AC1.** A Review Brief is filed and its Review Report ingested before the conversion
      primitives ship, settling three numeric rules: the between-rows lookup rule for
      mean-metric scales, the tie rule where one raw value is printed at several T scores,
      and the treatment of the 29 printed rows whose raw exceeds the attainable 0–3 ceiling
      (BF `negativeAffectivity` from T=85, `detachment` from 88, `disinhibition` from 94,
      and the FULL/SF `negativeAffectivity` rows). Each rule is reproduced in
      `norm_pid5()`'s `@details` with its source and covered by a test whose expected
      values come from the report or the book, never from the function's own output (IP2).
      *(RB tripwire: no-oracle)*
- [ ] **AC2.** `norm_pid5()` takes a data frame of PID-5 scale scores plus a `scores`
      argument naming the score columns (mirroring the scoring family's `items`), under the
      signature `(data, scores, version, srange, prefix, append = TRUE)`, and returns a
      tibble carrying a `_ptl` column for every named column `pid_norms` covers for that
      `version` and a `_t` column for every covered column whose `pid_norms` rows carry a
      non-`NA` `tscore` — the domain scales and the BF `total`. The four validity scales
      (`INC`, `INCS`, `ORS`, `PRD`) are score→percentile only and get no `_t` column.
- [ ] **AC3.** A named score column that `pid_norms` does not cover for that version (for
      example any of the 25 PID-5 facets) yields both a `_t` and a `_ptl` column, present
      and filled with `NA`, plus one `cli` message naming those scales — never an error,
      and never a silently absent column.
- [ ] **AC4.** Conversion is verified against the shipped tables rather than against the
      function's own output, over **every** scale in `pid_norms` — the five domains per
      version, the BF `total`, and the four validity scales. For T-carrying scales, T → raw
      reproduces every printed row exactly and raw → T → raw is the identity on every
      printed row; for every scale, each printed raw converts to that row's printed
      percentile.
- [ ] **AC5.** Out-of-table raw scores are handled at both ends and on both output columns:
      a raw above the highest printed row for its scale returns that row's T (where the
      scale has one) and that row's percentile rather than an extrapolated value, a raw
      below the lowest printed row returns the lowest row's values, and `norm_pid5()` emits
      one `cli` warning naming how many observations were capped at each end. The four
      validity scales, whose rows carry no T, are capped on percentile alone.
- [ ] **AC6.** When `srange` implies an option count other than the PID-5's official four,
      `norm_pid5()` returns `NA` for every conversion column and warns naming the mismatch.
      When the count is four but the coding is shifted (for example 1–4 against the official
      0–3), each named column is reconciled by its own metric per D-020 — item means shifted
      by `low`, `PRD` by `low × nItems`, `ORS` re-derived against the shifted maximum,
      `INC`/`INCS` left unchanged as already coding-invariant — the per-scale formulas are
      documented in `@details` on their merits (GP1), and the reconciliation is reported
      once via `cli::cli_alert_info()` naming which scales were adjusted and which were not.
- [ ] **AC7.** `vignettes/pid5_scoring.Rmd`, `pid5sf_scoring.Rmd`, and `pid5bf_scoring.Rmd`
      each gain a norming section that executes against the package's example data;
      `norm_pid5()` is listed in `_pkgdown.yml`'s reference index; NEWS.md records it; and
      `devtools::document()` leaves the tree clean while `devtools::check()` reports 0
      errors and 0 warnings, with every NOTE justified in this file's Review section.

## Coverage

- AC1 → T1, T2, T6
- AC2 → T3
- AC3 → T3, T6
- AC4 → T2, T6
- AC5 → T5, T6
- AC6 → T4, T6
- AC7 → T7

## Tasks

- [ ] **T1.** File the Review Brief via `/milestone-brief` covering AC1's three rules;
      ingest the resulting report and record its rules in this file's Decisions section.
      Set the **Driving RR** header slot if the report carries Binding criteria.
- [ ] **T2.** Implement the conversion primitives over `pid_norms` — T → raw, raw → T, raw
      → percentile — applying the report's rules.
- [ ] **T3.** Implement `norm_pid5()`: signature, per-scale `_t`/`_ptl` columns, and the
      unnormed-scale message.
- [ ] **T4.** Implement the `srange` reconciliation per D-020 — option-count check, the
      four per-metric rescale branches, and the `cli_alert_info()` report.
- [ ] **T5.** Implement capping at both ends across both output columns, with per-end
      warning counts.
- [ ] **T6.** Tests: every printed row round-tripped per AC4, capping at both ends,
      unnormed scales, both `srange` branches, and the R edge cases the profile's
      test-doctrine names.
- [ ] **T7.** Roxygen with the cited rules, three vignette norming sections, the
      `_pkgdown.yml` reference entry, NEWS; run `document()` / `test()` / `check()`.

## Work log

- 2026-07-30: created by /milestone-plan, split from the original M26 (whose BF-total half is now M26).
- 2026-07-30: plan gate chose per-scale `srange` reconciliation including the validity scales (D-020) over rescaling only the mean-metric domains, because a silently wrong validity percentile is worse than an absent one and the four metrics are each derivable; falsified by any of the four per-scale formulas lacking a defensible derivation at T4.
- 2026-07-30: plan gate chose to escalate the lookup, tie, and above-ceiling rules to a written review brief over pinning a default rule in the criteria, because the tables print 29 rows no real score can reach and a guessed rule would be unfalsifiable against them; falsified by the book stating all three rules outright, which would make the brief redundant.
- 2026-07-30: plan chose `cli::cli_alert_info()` for the reconciliation report over the silent rescale the legacy-M16 note proposed, because GP1 requires deviations to be loud and it changes the numbers a user reads; falsified by the message proving noisy enough on ordinary data that users suppress it.
- 2026-07-30: plan absorbed the `Norm-ready response ranges (legacy M16)` candidate row into AC6 rather than planning it separately, because the reconciliation has no home outside a norming lookup (D-012 deferred it here explicitly).
- 2026-07-30: blocked on RB02 (T1) — AC1's three lookup rules escalated; the brief's falsification check confirmed the book prints no lookup rule (Appendix pp. 113-115, Ch. 7), and probing `pid_norms` established that the 16 T-carrying tables are exactly linear, every tie is a raw-0.00 floor clip, percentile is empirical rather than a transform of T, and only 39 of 305 printed BF domain raws are attainable.

## Decisions

## Review
