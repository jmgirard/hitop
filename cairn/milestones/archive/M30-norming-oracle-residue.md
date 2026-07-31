# M30: Norming-family test oracles and internal consistency

**Status:** done (2026-07-31, PR #33 https://github.com/jmgirard/hitop/pull/33)

**Goal:** Replace the PID-5 norming tests that transcribe the implementation's own
constants with a measured oracle, and make the family internally consistent.

**Outcome:** `observed_shift()` (`test-norm_pid5.R`) scores a dataset and its
`data + low` copy on matching `srange` and differences per scale, so `norm_shift()`
is asserted against a measured quantity rather than against `norm_engine.R`'s three
partition vectors (IP2); the per-scale label expectations are gone. Those vectors are
now asserted pairwise disjoint — `norm_metric()` assigns in write order, so an overlap
resolved silently. It also takes `call`, so its abort names `norm_pid5()`. Both aborts
pluralize off `cli::qty()` and interpolate `version` alike; the non-numeric abort emits
one `{.cls}` bullet per offending column (`<ordered/factor>`, not `ordered`). DESIGN.md
gained `strip_prefix()` and D-024/D-025's alert-convention carve-out. No returned value
moved: 9/9 characterization configs `identical()`.

**Decisions:** None milestone-local; two plan-gate approach choices are in the work
log. D-022's test-time fitting licence permits the differential oracle.

**Review:** Blame-history and prior-review lenses found no regression; diff-bug returned
16 and confirmed the oracle's arithmetic commutes with the shift. None of the 17 scored
≥80, so nothing was actioned; the two highest (78, 75) became a candidate row. One
return: AC1's file-header clause was unmet.
