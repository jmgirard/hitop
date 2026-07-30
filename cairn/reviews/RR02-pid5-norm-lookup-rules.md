# RR02: PID-5 raw ↔ T ↔ percentile lookup rules (M27)

- **Date:** 2026-07-30
- **Answers:** `cairn/reviews/RB02-pid5-norm-lookup-rules.md`
- **Reviewer:** independent Fable session (no prior conversation context)

All six established facts (F1–F6) were re-derived from `data/pid_norms.rda`
before answering; the book's Appendix and Chapter 7 prose were re-searched in
`cairn/references/sources/markon2024.epub` and confirmed to state no lookup,
rounding, or interpolation rule (the only "round" hit in the Appendix is the
substring inside "BACKGROUND"). One count in F1 is slightly off as printed in
the brief — see Beyond the brief, item 1 — with no effect on any conclusion.

**The one-sentence summary of this report.** All conversions are a single-row
selection over the printed rows: *pick the row whose printed raw is nearest the
observed value; when that leaves more than one candidate, pick the candidate
whose T is nearest 50; return that one row's printed T and printed percentile.*
One rule answers Q1, Q2, and Q3 at once, uses printed cells only (Q5), and every
consequence is checkable by hand against named cells (Q6).

## Answers

### Q1 — Between-rows raw → T: (a) nearest printed row

**Rule (assertion form).** For each T-carrying version/scale, `raw → T` returns
the `tscore` of the printed row minimizing `|observed − raw|`. Consequently:
(i) restricted to printed raws outside tie runs, `raw → T` is the identity on
the table; (ii) for any observed value strictly between two printed raws, it
returns the T of the closer row; (iii) it is monotone nondecreasing in the
observed value. All three are checkable against the printed rows and are
covered by BC2/BC10.

**Tie-break at an exact midpoint.** An observed value equidistant (within the
BC8 tolerance) from two adjacent printed raws selects the candidate whose T is
nearer 50 — the less deviant of the two rows. Adjacent integer T's are never
equidistant from 50, so the pick is always unique, and because the pick is one
of the two bracketing rows, monotonicity survives. This case is not
hypothetical: printed 2-dp raws step by 0.04–0.07 while attainable scores fall
on coarse grids (F5), and exact midpoints occur at common values — BF
detachment 0.20 (the second-lowest attainable value) is exactly midway between
T=42 (raw 0.17) and T=43 (raw 0.23) and resolves to **T=43**; BF total 0.76 is
exactly midway between T=54 (raw 0.74) and T=55 (raw 0.78) and resolves to
**T=54**. The same toward-50 principle also decides Q3, which is the argument
for it over "always the lower T": always-lower is asymmetric (it *inflates*
deviance on the low tail) and cannot be unified with the floor-tie rule, which
must pick the *higher* T at the low edge.

**What each rejected option gets wrong.**

- **(b) floor** biases T downward by ~0.5 points on average and by a full point
  for any observation just under a printed raw; on the low tail it maps every
  positive observation below the first positive row onto the tie run's floor.
  Nothing in the source motivates an asymmetric rule.
- **(c) linear interpolation to non-integer T** manufactures resolution the
  published table does not print, has no oracle anywhere (IP2: no worked
  example exists, so a fractional T can never be checked against anything), and
  is incoherent with the percentile column, which is empirical and non-linear
  in T (F3) and therefore cannot be interpolated on the same justification.
- **(d) inversion of the fitted line** ships constants (M, SD) the book does
  not print — barred under Q5's reading of IP2/IP3 — and is also *numerically*
  disqualified: the fitted line contradicts six printed cells at rounding
  boundaries (Beyond the brief, item 1), so inversion cannot reproduce the
  table exactly and would fail AC4. Nearest-row is, to within the 2-dp rounding
  of the printed raws, *equivalent* to inverting the line and rounding T to the
  nearest integer — F1 makes the printed raws an evenly spaced (±0.01) sampling
  of the line at integer T — so option (a) captures essentially all of (d)'s
  accuracy while touching only published cells.

### Q2 — Between-rows raw → percentile: same rule, same selected row, printed resolution

**Same rule, and necessarily so.** The `_t` and `_ptl` returned for one
observation must come from **one** selected printed row; otherwise a user can
receive a (T, percentile) pair that appears in no published row and is
internally inconsistent. So percentile follows the Q1 selection: the nearest
row's printed percentile, tie-broken identically.

**No interpolation; printed 2-dp values only.** The percentile function is
empirical, non-linear in T, and saturating (F3); between printed rows its shape
is unknown — the weighted sample's mass sits at attainable grid points the
table does not index, so linear interpolation between printed percentiles is a
guess presented at false precision, with no oracle to check it against (IP2).
The returned percentile is always exactly a printed cell value. The error of
nearest-row relative to the unprinted truth is bounded by the adjacent-row
percentile gap, which in the shipped tables is at most 0.44 at the extreme low
edge (BF psychoticism's first step) and ≤ 0.07 through the mid-range — honest
to source resolution.

**Validity scales (F6): same rule, degenerate case.** Observed `INC`/`INCS`/
`ORS`/`PRD` values are always integers and the tables are contiguous one-row-
per-integer, so every in-range observation is an exact unique match and the
between-rows branch never fires. No special case is needed: the same selection
rule reduces to exact lookup. Out-of-table integers (attainable `PRD` 56–66
above the table's last row of 55) are AC5's fixed capping behavior, not this
rule's. If a user supplies a non-integer validity score (impossible from
`validity_pid5()`), the same nearest rule applies rather than a special error —
one rule, no carve-outs.

### Q3 — Floor ties: the tied row with T nearest 50 (equivalently, the interior-adjacent row)

**Rule (general form).** When the observed value exactly equals a raw printed
in more than one row, all tied rows are nearest-row candidates and the Q1
tie-break decides: return the candidate whose T is nearest 50. Every tie in the
shipped tables is a contiguous run of raw 0.00 starting at the table's lowest T
(verified across all 16 scales), so this always selects the **highest-T tied
row** — the row adjacent to the un-tied interior. The rule as stated also
covers any future tie: a ceiling run would resolve to its lowest-T row, and a
run can only sit at an edge (off the floor, printed raws are strictly
increasing), so the toward-50 candidate is always the interior-adjacent row and
always unique.

**Why the interior-adjacent row is the right row, not merely a permitted one.**
The tie runs are floor-censoring artifacts of the F1 line: rows where the line
predicts a negative raw print 0.00. The interior-adjacent row is the one row of
the run where the line's value is at (or within rounding of) zero — for SF
psychoticism, the line crosses zero at T≈42.0, so T=42 is the genuine rendering
of raw 0.00 and T=30–41 are renderings of unattainable negative values. Its
percentile is correspondingly the only tied percentile that can carry
information about an attainable score. And the choice preserves monotonicity
with the neighboring rows (0.00 → 42, then 0.05 → 43), where choosing the low
end of the run (T=30) would open a 13-point gap.

**Concrete values.** SF psychoticism observed 0.00 → **T=42, percentile 0.31**;
BF psychoticism observed 0.00 → **T=42, percentile 0.00**; BF total observed
0.00 → **T=37, percentile 0.00**. Note the asymmetry between the first two: it
is a property of the printed table, not of the rule (Beyond the brief, item 2
shows the book's percentiles are consistent with evaluation at the *unrounded*
line value, so some zero-runs top out at a positive percentile and others at
0.00). Because 0.00 is a common score on 5-item BF domains, the alternative —
returning percentile 0.00 from a clipped row when the table *does* print a
positive percentile for that raw (SF psychoticism's 0.31) — would mislabel
roughly a third of ordinary respondents; the selected rule returns the most
informative value the published table contains for that observation, and
nothing beyond it.

### Q4 — The 29 above-ceiling rows: (a) retain, document, no runtime message

**Retain (a).** The rows stay in `pid_norms` (frozen; this report recommends no
edit, addition, or removal of any row) and stay in the lookup domain. They are
required by AC4's T → raw direction, and under the Q1 rule they are simply
never selected by any attainable score: the published table asserts that BF
negativeAffectivity T=85 corresponds to raw 3.05, so an attainable 3.00 is
genuinely *below* that point on the published metric and maps to T=84 (printed
raw 2.98, distance 0.02 vs 0.05).

**Against (b).** Remapping the attainable maximum to the top printed T would
turn a published 84 into a 95 — an 11-point fabrication contradicting the
printed cells, breaking monotone consistency with T → raw, and asserting a rule
no source states (GP1). A bounded scale that cannot reach the top of a linear T
range is a known, unremarkable property of linear T norms; the norming sample's
location and spread simply put the item-ceiling at ~3.4 SD for these five
scales. Nothing is lost on the percentile side (F4: the last attainable row
already carries 1.00).

**User-facing behavior.** A maxed BF negativeAffectivity returning T=84 is
correct and should not warn: nothing about the user's data is anomalous, no
value was capped (AC5 does not fire — 3.00 is inside the table), and a message
on every maxed score would be noise. The right surface is `@details`: one
paragraph stating that linear T norms on a bounded scale leave the top of some
T ranges unattainable, with the five affected scales and their effective maxima
— BF negativeAffectivity **T=84**, BF detachment **T=87** (raw 3.00 printed
exactly), BF disinhibition **T=93**, FULL negativeAffectivity **T=87**, SF
negativeAffectivity **T=85** (raw 3.00 printed exactly) — all at percentile
1.00. That is a mechanical fact about the published tables, not interpretation
(IP4-clean). If a user feeds a raw *above* the attainable ceiling but inside
the table (only possible with mis-scaled input), the same nearest rule applies
with no special case; values above the last printed row are AC5's cap.

### Q5 — Table authority: printed rows only; the fitted line is barred from shipping but licensed as a test-side oracle

**Reading of IP3/IP2.** The published artifact is the table of rows. The linear
form behind it (F1) is real but its constants are not printed anywhere in the
book; fitting them here and shipping them — as code constants, as `sysdata`, or
implicitly by returning values only derivable from them — would make returned
numbers trace to a regression performed in this repo rather than to a cited
authority. That fails IP2's "every shipped numeric constant traces to a cited
authority" directly, and fails IP3 in spirit: the norms the package applies
would no longer be the published tables but a reconstruction of their
generator. So: **every returned T and percentile must be a value printed in a
row of `pid_norms`, selected by arithmetic on printed cells only.** The Q1–Q3
rules satisfy this by construction — nearest-distance comparison and the
toward-50 tie-break consume printed raws and printed T's and return printed
cells untouched.

**The compromise where inversion is numerically better.** It essentially
isn't: nearest-row equals rounded inversion to within the table's own 2-dp
rounding (Q1), and where the two can diverge — knife-edge midpoints and the six
rounding-boundary cells — the printed table is the *more* authoritative of the
two, not less. The one legitimate use for the fitted line is verification: a
test may fit the line *at test time from the shipped rows* and assert that the
implementation's raw → T agrees with rounded inversion within ±1 T point over
the attainable grid. Constants derived inside a test from the shipped table are
IP2's "independent recomputation", not a shipped constant; they never touch
returned values. This resolves the Q5 tension without weakening either
principle, and Q1–Q3 are consistent with it: no rule above needs anything the
printed rows do not contain.

### Q6 — Oracles: printed-row identity + hand-computed fixtures + independent recomputation; exclude Pegaso

The book prints no worked example, so the evidence set must make the *rules*
checkable, not just the code. The minimum set (all IP2-compliant, none
asserting the function's own output):

1. **Printed-row identity (AC4, amended per BC9).** Every row of `pid_norms`
   round-trips: T → raw exact on all rows; raw → T → raw identity on all rows;
   raw → percentile returns the row's printed percentile, with tie runs
   targeting the run's selected row. Oracle: the shipped table, itself verified
   cell-for-cell against the book by `data-raw/verify_norms_against_book.R`.
2. **Hand-computed between-rows fixtures** — expected values derived by hand
   from named printed cells quoted beside the assertion, per this report
   (BC10 lists the required minimum: generic between-rows, both midpoint
   directions, both zero-tie flavors, above-ceiling maximum, caps at both ends,
   PRD above-table). The authority chain is: printed cells (book) + selection
   rule (this RR, cited in `@details` under GP1's no-published-rule branch).
3. **Independent recomputation.** A naive, separately coded scalar lookup
   (loop + `which.min`, written in the test file) must agree exactly with the
   shipped vectorized primitive over the full attainable grid of every scale —
   two independent implementations of the same stated rule. Plus structural
   invariants: `_t` and `_ptl` monotone nondecreasing in raw over a dense grid;
   every returned value present in the scale's printed rows.
4. **Optional cross-check (consider):** the test-time fitted-line inversion of
   Q5, tolerance ±1 T, over attainable grids.

**Pegaso (`https://pid5-us-en.pegasopoint.it`): exclude as an oracle.** The
Appendix (p. 115) offers it as an administration-and-scoring service, not as an
algorithm statement. Against it: (i) *provenance* — nothing citable states
whether it uses these tables, unrounded parameters, different rounding, or
updated/stratified norms, so a disagreement would be undiagnosable and an
agreement uninformative; (ii) *version drift* — an unversioned web service can
change silently, so a fixture captured from it would trace to "a website on a
date", below IP2's citation bar; (iii) *access and redistribution* — it sits
behind the publisher's platform, and captured outputs could not be committed as
test fixtures with clean provenance; (iv) it plausibly returns T from the
continuous formula, which would produce false failures against a table-derived
rule. A one-time informal spot check by the maintainer, recorded in M27's work
log as corroboration (never as a test), is worthwhile if access is convenient —
recommended below as *consider*.

## Beyond the brief

1. **F1's counts are slightly off; its claim stands.** Rerunning the brief's
   own script yields **850** non-zero printed rows, **844** exact 2-dp matches,
   and **6** exceptions (not 863/856/7): BF antagonism T=85 (printed 2.11,
   line 2.115), FULL detachment T=38 (0.12 / 0.125) and T=41 (0.28 / 0.285),
   FULL negativeAffectivity T=54 (1.04 / 1.045), SF negativeAffectivity T=60
   (1.40 / 1.395), FULL psychoticism T=63 (1.16 / 1.155). Every exception is a
   half-way (x.xx5) rounding-boundary cell, exactly as the brief characterizes
   them. The substantive conclusions (linearity; inversion cannot reproduce the
   table exactly) are unaffected.
2. **The percentile column's convention is identifiable, and it explains the
   zero-run asymmetry.** Across all 16 scales, the top row of a zero-run
   carries a positive percentile *exactly when* the fitted line's unrounded raw
   at that T is positive (e.g., SF psychoticism T=42: line +0.0015, ptl 0.31;
   BF psychoticism T=42: line −0.0210, ptl 0.00; 16/16 consistent). The book's
   percentiles thus behave as the weighted empirical CDF evaluated at the
   *unrounded* linear raw, before 2-dp printing. Consequence: for some scales
   the printed table contains a positive percentile for raw 0.00 and for others
   it does not, so under any printed-cells-only rule, identical observed zeros
   legitimately receive different percentile treatment across scales (Q3's
   concrete values). This is a property of the frozen source data; it should be
   stated in `@details`, not repaired — any repair would require unpublished
   inference (and editing frozen rows, which this report does not recommend).
3. **AC4 as currently worded is unsatisfiable on tie runs** — "each printed raw
   converts to that row's printed percentile" cannot hold for a raw printed in
   13 rows spanning two percentiles. BC9 states the required amendment; the
   T → raw direction and the raw → T → raw identity are unaffected and remain
   exact for every row, tie runs and above-ceiling rows included.
4. **Percentile is monotone nondecreasing in raw within every shipped
   version/scale** (verified) — usable as a standing test invariant (BC10).
5. **Floating-point care is load-bearing, not cosmetic.** Attainable SF raws
   are twelfths (non-terminating in binary) and printed raws are 2-dp decimals,
   so exact `==` comparisons will misclassify exact matches and midpoints; a
   documented absolute tolerance is required (BC8).

## Recommendations

1. **Apply** — implement raw → T and raw → percentile as one-row selection:
   nearest printed raw, all ambiguity (exact-match tie runs and equidistant
   midpoints alike) resolved to the candidate row with T nearest 50; return
   that row's printed T and percentile (BC1–BC5).
2. **Apply** — validity scales use the same rule (degenerate exact lookup), no
   special cases beyond AC5's fixed capping (BC6).
3. **Apply** — retain the 29 above-ceiling rows in the lookup domain; document
   the five effective T-maxima in `@details`; no runtime message (BC7).
4. **Apply** — ship no fitted constants; permit the fitted line only as a
   test-time independent-recomputation oracle (BC1, BC10).
5. **Apply** — amend M27 AC4's percentile-identity clause per BC9 (via the
   milestone's "Deviations from RR02" table or a direct AC edit at ingest,
   whichever the tracking rules prescribe).
6. **Apply** — document in `@details`: the selection rule with this RR cited
   (GP1 no-published-rule branch), the zero-run percentile asymmetry (Beyond
   the brief 2), and the comparison tolerance (BC8).
7. **Consider** — the test-time fitted-line ±1 T cross-check over attainable
   grids (BC10's optional clause).
8. **Consider** — a one-time manual Pegaso spot check recorded in M27's work
   log as informal corroboration only.
9. **Reject** — Pegaso as a test oracle or cited authority: unverifiable
   provenance, version drift, access/redistribution constraints (Q6).
10. **Reject** — floor lookup, linear interpolation, shipped-line inversion,
    and attainable-maximum remapping to the top printed T, for the reasons
    given under Q1 and Q4.

## Binding criteria

- **BC1.** For every conversion, the returned `_t` and `_ptl` for one
  observation are the `tscore` and `percentile` of **one** printed row of
  `pid_norms` for that version/scale, selected by arithmetic on printed cells
  only. No returned value is interpolated, and no constant derived by fitting
  `pid_norms` (M, SD, slope, intercept, or equivalent) ships in package code,
  package data, or documentation-stated formulas.
- **BC2.** The selected row minimizes `|observed − raw|` over the scale's
  printed rows. Verified: for every printed row outside a tie run, converting
  that row's raw returns that row's T and percentile exactly (0 tolerance on
  the returned values; input comparison per BC8).
- **BC3.** When two or more rows remain candidates — an observed value equal to
  a raw printed in multiple rows, or equidistant from two adjacent printed raws
  within the BC8 tolerance — the candidate whose T is nearest 50 is selected.
  Fixed consequences (exact expected values): SF psychoticism 0.00 → T=42,
  ptl 0.31; BF psychoticism 0.00 → T=42, ptl 0.00; BF total 0.00 → T=37,
  ptl 0.00; BF detachment 0.20 → T=43, ptl 0.35; BF total 0.76 → T=54,
  ptl 0.71.
- **BC4.** raw → T and raw → percentile are monotone nondecreasing in the
  observed value on every version/scale, verified over a grid no coarser than
  0.01 across each table's raw range.
- **BC5.** Every returned percentile equals a printed `percentile` cell of the
  selected scale exactly; no returned percentile has more than 2 decimal
  places of information.
- **BC6.** For `INC`, `INCS`, `ORS`, `PRD`: every integer within the printed
  range returns that row's percentile exactly; attainable above-table integers
  (`PRD` 56–66) return the last printed row's percentile under AC5's capping
  and are counted in its warning; no `_t` column is produced (AC2, restated
  for completeness).
- **BC7.** The 29 above-ceiling rows remain in `pid_norms` and in the lookup
  domain (this report directs no row edit, addition, or removal). Fixed
  consequences: BF negativeAffectivity 3.00 → T=84, ptl 1.00; BF detachment
  3.00 → T=87, ptl 1.00; BF disinhibition 3.00 → T=93, ptl 1.00; FULL
  negativeAffectivity 3.00 → T=87, ptl 1.00; SF negativeAffectivity 3.00 →
  T=85, ptl 1.00. No runtime message fires for in-table scores; the five
  effective T-maxima above are stated in `norm_pid5()`'s `@details`.
- **BC8.** All lookup comparisons (exact match, nearest distance, equidistance)
  use a documented absolute tolerance of 1e-8, so that binary representation of
  decimal and twelfth-grid raws never changes a selection. The tolerance
  appears in `@details`.
- **BC9.** M27 AC4 is amended as follows: T → raw reproduces every printed row
  exactly and raw → T → raw is the identity on every printed row, both
  including tie runs and above-ceiling rows; the raw → percentile identity
  holds for every printed row *outside* tie runs, and within a tie run the
  shared raw converts to the BC3-selected row's percentile.
- **BC10.** The test suite contains, at minimum: (i) the full printed-row
  identity checks of BC9 against `pid_norms`; (ii) hand-computed fixtures whose
  expected values are stated with their bracketing printed cells cited in the
  test source, covering: one generic (non-midpoint) between-rows case per
  version, both BC3 midpoint fixtures (one resolving up, one down), both
  zero-tie flavors (SF and BF psychoticism), the five BC7 attainable-maximum
  cases (at least one asserted), capping at both table ends, and `PRD` = 60;
  (iii) an independently coded naive scalar lookup agreeing exactly with the
  shipped primitive over the full attainable grid of every BF and SF scale and
  a 0.01-step grid for FULL scales; (iv) BC4's monotonicity checks. Optional
  (consider): a test-time fitted-line inversion cross-check agreeing with the
  implementation within ±1 T over the attainable grids; its fitted constants
  are derived inside the test and never committed.
