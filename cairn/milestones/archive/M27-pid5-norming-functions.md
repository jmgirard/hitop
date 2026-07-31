# M27: PID-5 raw → T / percentile conversion (`norm_pid5()` on the official coding)

**Status:** done (2026-07-30, PR #30 https://github.com/jmgirard/hitop/pull/30)

**Goal:** Ship `norm_pid5()`, converting scored PID-5 / SF / BF columns to normative T scores
and percentiles from `pid_norms` by RR02's selection rule, on the official 0–3 coding.

**Outcome:** `norm_pid5(data, scores, version, srange, prefix, append)` returns a `_t` column
per T-carrying scale and a `_ptl` per covered scale. Internal `R/norm_engine.R`: `norm_select()`
picks the printed row nearest the observed value, breaking ties toward T=50 (toward percentile
0.50 on the four T-less validity tables) after clamping to the printed range; `norm_t_to_raw()`
inverts by exact lookup; `norm_capped()` flags out-of-range observations per end. Uncovered
scales and `NA` inputs return `NA` with a message; any `srange` but `c(0, 3)` is refused
pending M28. `pid_norms` itself was not touched.

**Decisions:** M27-D1 the selection rule; M27-D2 the 29 above-ceiling rows retained with five
effective T-maxima documented; M27-D4 the oracle set (Pegaso rejected); M27-D5 three departures
from RR02's binding criteria; M27-D6 AC5's low end reconciled, `NA` input pinned. Cross-cutting:
D-022 (published cells only) and D-023 (`ORS` is coding-invariant).

**Review:** Three fresh-context lenses; blame-history and prior-review found nothing, the diff-bug
lens 15. Five scored ≥80, all fixed: infinite/enormous scores tied every row and landed near T=50
instead of capping (88); `@details` described the low-end behavior AC5 abandoned (82); capping
counted observation×scale pairs (85); two above-table test gaps (80, 82). Ten logged below
threshold, four became a candidate row. All ten RR02 projections reproduced exactly.
