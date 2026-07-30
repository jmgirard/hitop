# M27: PID-5 norming and score conversion (raw ↔ T ↔ percentile)

- **Status:** planned
- **Priority:** normal
- **Depends on:** M26
- **Driving RR:** RR02
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

- [ ] **AC1.** The AC8–AC17 rules are reproduced in `norm_pid5()`'s `@details` with their
      source (RR02, GP1's no-published-rule branch) and covered by tests whose expected
      values come from the report or the book, never from the function's own output (IP2).
      *(RB tripwire: no-oracle — discharged by RB02/RR02, archived.)*
- [ ] **AC2.** `norm_pid5(data, scores, version, srange, prefix, append = TRUE)` names its
      score columns via `scores` (mirroring the scoring family's `items`) and returns a
      tibble with a `_ptl` column for every named column `pid_norms` covers for that
      `version` and a `_t` column for every covered column whose rows carry a non-`NA`
      `tscore` — the domains and the BF `total`. `INC`/`INCS`/`ORS`/`PRD` are
      score→percentile only and get no `_t` column.
- [ ] **AC3.** A named score column `pid_norms` does not cover for that version (for
      example any of the 25 facets) yields both a `_t` and a `_ptl` column, present and
      filled with `NA`, plus one `cli` message naming those scales — never an error, and
      never a silently absent column.
- [ ] **AC4.** Conversion is verified against the shipped tables rather than the function's
      own output, over **every** scale in `pid_norms` — the five domains per version, the
      BF `total`, and the four validity scales; the round-trip and printed-row identity
      requirements are AC16's (BC9), which amends this criterion's original clause.
- [ ] **AC5.** Out-of-table raws are capped at both ends on both output columns: above the
      highest printed row returns that row's T (where the scale has one) and its
      percentile rather than an extrapolation; below the lowest printed row returns the
      values of the row AC10 (BC3) selects for the lowest printed raw; one `cli` warning
      names how many observations were capped at each end. The validity scales, whose rows
      carry no T, are capped on percentile alone.
- [ ] **AC6.** An `srange` implying an option count other than four returns `NA` in every
      conversion column and warns naming the mismatch. A shifted four-option coding is
      reconciled per scale per D-020 — item means by `low`, `PRD` by `low × nItems`, `ORS`
      re-derived against the shifted maximum, `INC`/`INCS` unchanged as already
      coding-invariant — the formulas documented in `@details` on their merits (GP1) and
      reported once via `cli::cli_alert_info()` naming which scales were adjusted and
      which were not.
- [ ] **AC7.** The three PID vignettes each gain a norming section executing against the
      package's example data; `norm_pid5()` is in `_pkgdown.yml`'s reference index;
      NEWS.md records it; `devtools::document()` leaves the tree clean and
      `devtools::check()` reports 0 errors and 0 warnings, every NOTE justified in the
      Review section.
- [ ] **AC8** (BC1): For every conversion, the returned `_t` and `_ptl` for one
      observation are the `tscore` and `percentile` of **one** printed row of
      `pid_norms` for that version/scale, selected by arithmetic on printed cells only.
      No returned value is interpolated, and no constant derived by fitting `pid_norms`
      (M, SD, slope, intercept, or equivalent) ships in package code, package data, or
      documentation-stated formulas.
- [ ] **AC9** (BC2): The selected row minimizes `|observed − raw|` over the scale's
      printed rows. Verified: for every printed row outside a tie run, converting that
      row's raw returns that row's T and percentile exactly (0 tolerance on the returned
      values; input comparison per BC8).
- [ ] **AC10** (BC3): When two or more rows remain candidates — an observed value equal
      to a raw printed in multiple rows, or equidistant from two adjacent printed raws
      within the BC8 tolerance — the candidate whose T is nearest 50 is selected. Fixed
      consequences (exact expected values): SF psychoticism 0.00 → T=42, ptl 0.31; BF
      psychoticism 0.00 → T=42, ptl 0.00; BF total 0.00 → T=37, ptl 0.00; BF detachment
      0.20 → T=43, ptl 0.35; BF total 0.76 → T=54, ptl 0.71.
- [ ] **AC11** (BC4): raw → T and raw → percentile are monotone nondecreasing in the
      observed value on every version/scale, verified over a grid no coarser than 0.01
      across each table's raw range.
- [ ] **AC12** (BC5): Every returned percentile equals a printed `percentile` cell of
      the selected scale exactly; no returned percentile has more than 2 decimal places
      of information.
- [ ] **AC13** (BC6): For `INC`, `INCS`, `ORS`, `PRD`: every integer within the printed
      range returns that row's percentile exactly; attainable above-table integers
      (`PRD` 56–66) return the last printed row's percentile under AC5's capping and are
      counted in its warning; no `_t` column is produced (AC2, restated for
      completeness).
- [ ] **AC14** (BC7): The 29 above-ceiling rows remain in `pid_norms` and in the lookup
      domain (this report directs no row edit, addition, or removal). Fixed
      consequences: BF negativeAffectivity 3.00 → T=84, ptl 1.00; BF detachment 3.00 →
      T=87, ptl 1.00; BF disinhibition 3.00 → T=93, ptl 1.00; FULL negativeAffectivity
      3.00 → T=87, ptl 1.00; SF negativeAffectivity 3.00 → T=85, ptl 1.00. No runtime
      message fires for in-table scores; the five effective T-maxima above are stated in
      `norm_pid5()`'s `@details`.
- [ ] **AC15** (BC8): All lookup comparisons (exact match, nearest distance,
      equidistance) use a documented absolute tolerance of 1e-8, so that binary
      representation of decimal and twelfth-grid raws never changes a selection. The
      tolerance appears in `@details`.
- [ ] **AC16** (BC9): M27 AC4 is amended as follows: T → raw reproduces every printed
      row exactly and raw → T → raw is the identity on every printed row, both including
      tie runs and above-ceiling rows; the raw → percentile identity holds for every
      printed row *outside* tie runs, and within a tie run the shared raw converts to
      the BC3-selected row's percentile.
- [ ] **AC17** (BC10): The test suite contains, at minimum: (i) the full printed-row
      identity checks of BC9 against `pid_norms`; (ii) hand-computed fixtures whose
      expected values are stated with their bracketing printed cells cited in the test
      source, covering: one generic (non-midpoint) between-rows case per version, both
      BC3 midpoint fixtures (one resolving up, one down), both zero-tie flavors (SF and
      BF psychoticism), the five BC7 attainable-maximum cases (at least one asserted),
      capping at both table ends, and `PRD` = 60; (iii) an independently coded naive
      scalar lookup agreeing exactly with the shipped primitive over the full attainable
      grid of every BF and SF scale and a 0.01-step grid for FULL scales; (iv) BC4's
      monotonicity checks. Optional (consider): a test-time fitted-line inversion
      cross-check agreeing with the implementation within ±1 T over the attainable
      grids; its fitted constants are derived inside the test and never committed.

### Deviations from RR02

| Criterion | Departure | Reason |
|---|---|---|
| AC12 (BC5) | Second clause ("no returned percentile has more than 2 decimal places") is not implemented; returned percentiles carry the printed precision — 2 dp on the domain/total tables, 3 dp on `ORS`, `PRD`, `INCS`. | False against `pid_norms`: 71 of 105 validity cells carry 3 dp (`PRD` 0 → 0.007, `INCS` 0 → 0.078, `ORS` 0 → 0.889), so the clause contradicts BC5's own first clause and AC13. Verified 2026-07-30. |
| AC10 (BC3) | The T-nearest-50 tie-break governs the 16 T-carrying scales only. On `INC`/`INCS`/`ORS`/`PRD` a tie resolves to the candidate whose percentile is nearest 0.50. | `tscore` is `NA` on all 105 validity rows, so the stated key is unevaluable there; 101 half-integer inputs had no defined answer. Maintainer decision, 2026-07-30. |
| AC13 (BC6) | "(`PRD` 56–66)" is read as an example, not an enumeration: `INC` 24–60, `INCS` 16–30, and `ORS` 9–10 are equally attainable above their printed tables and are capped and counted the same way. | All four scales have attainable above-table integers (20 INC pairs, 10 INCS pairs, 10 ORS items, 22 PRD items). Verified 2026-07-30. |

## Coverage

- AC1 → T1, T2, T6, T7
- AC2 → T3
- AC3 → T3, T6
- AC4 → T2, T6
- AC5 → T5, T6
- AC6 → T4, T6
- AC7 → T7
- AC8 → T2, T6
- AC9 → T2, T6
- AC10 → T2, T6
- AC11 → T6
- AC12 → T2, T6
- AC13 → T3, T5, T6
- AC14 → T2, T6
- AC15 → T2, T7
- AC16 → T2, T6
- AC17 → T6

## Tasks

- [x] **T1.** File the Review Brief via `/milestone-brief` covering AC1's three rules;
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
- 2026-07-30: RR02 received (Fable, spawned at the RB02 gate) — checkpoint only, ingest pending; its ten binding criteria are with a fresh-context [O] auditor and all ten of its fixture values reproduced exactly against `pid_norms` on an independently coded lookup.
- 2026-07-30: ingested RR02 (T1 done) — ten binding criteria in verbatim as AC8–AC17, Driving RR set, three departures recorded in the Deviations table, decisions M27-D1..D6 written, D-022 promoted as cross-cutting; RB02/RR02 archived; status back to `planned`.
- 2026-07-30: the pre-ingest criteria audit ([O], fresh context) returned three defects — AC12 (BC5) false against the shipped data, AC10 (BC3) undefined on the four validity scales, AC13 (BC6) enumerating one of four above-table cases — plus an AC5 low-end collision and an unspecified `NA` input; every factual claim was re-verified against `pid_norms` here before the gate, and all five were disposed of at it rather than softened.
- 2026-07-30: ingest chose to take RR02's criteria verbatim with a Deviations table over sending the report back for a second Fable pass, because the defects are three narrow wording faults in an otherwise independently reproduced rule set; falsified by a fourth defect turning up in implementation that the audit's two questions should have caught.
- 2026-07-30: blocked on RB02 (T1) — AC1's three lookup rules escalated; the brief's falsification check confirmed the book prints no lookup rule (Appendix pp. 113-115, Ch. 7), and probing `pid_norms` established that the 16 T-carrying tables are exactly linear, every tie is a raw-0.00 floor clip, percentile is empirical rather than a transform of T, and only 39 of 305 printed BF domain raws are attainable.

## Decisions

**2026-07-30 — M27-D1: the selection rule (RR02 Q1–Q3).** Every conversion is one-row
selection over the printed rows of `pid_norms`: pick the row whose printed raw is nearest
the observed value; where more than one candidate remains, pick the candidate whose T is
nearest 50; return that one row's printed T and printed percentile. One row supplies both
output columns, so a user never receives a (T, percentile) pair that appears in no
published row. Rejected with reasons recorded in RR02: floor lookup (downward bias
nothing in the source motivates), interpolation to a non-integer T or a finer percentile
(manufactures resolution the tables do not print, and no oracle can check it — IP2), and
inversion of the fitted line (ships unpublished constants, and contradicts six printed
cells at rounding boundaries so it cannot reproduce the table exactly).

**2026-07-30 — M27-D2: the 29 above-ceiling rows are retained (RR02 Q4).** They stay in
`pid_norms` and in the lookup domain; no row is edited, added, or removed. Under the
selection rule they are simply never reached by an attainable score, so a maxed BF
`negativeAffectivity` correctly returns T=84. No runtime message fires — nothing about
the user's data is anomalous and no value was capped. The five effective T-maxima are
stated in `@details` as a mechanical fact about the published tables (IP4-clean).
Rejected: remapping the attainable maximum to the top printed T, an 11-point fabrication
contradicting the printed cells with no source behind it (GP1).

**2026-07-30 — M27-D3: printed cells only; the fitted line is test-side (RR02 Q5).**
Promoted to D-022 as cross-cutting — it binds every future norming function, not only
PID-5.

**2026-07-30 — M27-D4: the oracle set (RR02 Q6).** Printed-row identity against
`pid_norms` (itself verified cell-for-cell against the book by
`data-raw/verify_norms_against_book.R`), hand-computed between-rows fixtures citing their
bracketing printed cells in the test source, and an independently coded naive scalar
lookup agreeing with the shipped vectorized primitive over the attainable grids — two
independent oracle types, neither asserting the function's own output. The APA-referenced
computerized scoring system at `https://pid5-us-en.pegasopoint.it` (markon2024, p. 115) is
**rejected** as an oracle: nothing citable states which tables or rounding it uses, an
unversioned web service can change silently, and captured outputs could not be committed
as fixtures with clean provenance. A one-time manual spot check is welcome as informal
corroboration in this log, never as a test.

**2026-07-30 — M27-D5: three departures from RR02's binding criteria.** A fresh-context
[O] audit of the criteria set before ingest found one criterion false against the shipped
data, one undefined on four scales, and one whose enumeration was incomplete; each was
verified independently against `pid_norms` and raised at the 2026-07-30 gate rather than
softened in place. The criteria are ingested verbatim and the departures are recorded in
the "Deviations from RR02" table under the acceptance criteria: AC12's second clause is
not implemented (71 of 105 validity percentile cells carry 3 dp), AC10's tie-break is
scoped to the 16 T-carrying scales with validity ties resolving to the percentile nearest
0.50 (maintainer decision — `tscore` is `NA` on every validity row, so the stated key is
unevaluable there), and AC13's "(`PRD` 56–66)" reads as an example because `INC` 24–60,
`INCS` 16–30, and `ORS` 9–10 are equally attainable above their printed tables.

**2026-07-30 — M27-D6: AC5's low end reconciled, and `NA` input pinned.** The audit found
AC5's original "returns the lowest row's values" contradicting the selection rule on the
15 scales with a zero tie run (T=30/ptl 0.00 against T=42/ptl 0.31 for SF psychoticism).
AC5 now returns the values of the row AC10 selects for the lowest printed raw, so a
below-table value and an observed 0.00 agree; the case is reachable only from mis-scaled
input, since 0 is the attainable minimum of every metric. Separately, an `NA` input raw
returns `NA` in both output columns and is not counted in the capping warning — the R
convention, and the reachable state D-021 creates when a BF `total` prorates to `NA`
beside reported domains.

## Review
