# M033: PID-5 facet-level norms

**Status:** done (2026-07-31, PR #36 https://github.com/jmgirard/hitop/pull/36)

**Goal:** Ship the book's facet-level normative tables so `norm_pid5()` converts
all 25 PID-5 facet scores for the full and short forms.

**Outcome:** markon2024 A–6 (p. 124) and A–8 (p. 151) ingested; `pid_norms` 1,056
→ 4,606 rows, 3,550 new over 50 (version, scale) pairs at T = 30–100. New
`data-raw/extract_facet_norms.R` writes the facet CSVs; `verify_norms_against_book.R`
gains an independent block reader (T-restart cuts, name-keyed compare), spans 9
tables, `stop()`s on discrepancy. `norms_pid5.R` maps book captions onto
`pid_scales$Facet` behind a `setequal()` guard; the 25 stems join `norm_mean_scales`.
Tests: `lm` linearity → exact minimax bound 0.005; ceiling-run tests; 50 hand-read
anchors at T = 65; fixtures → `SDTD`/`PNA`; 5 facet mutations in the check script.

**Decisions:** [D-027](../../DECISIONS.md) — facets normed for FULL/SF, and the
book's unattainable printed rows (42 of 50 columns above raw 3.00, 19 repeating
4.00) ship verbatim rather than truncated.

**Review:** 3-lens fan-out; blame-history and prior-review 0, diff-bug 21 scored
8–88. 5 actioned and fixed — F1 (88) row-count claim, F7 (85) NA-blind compare,
F3/F15 stale `@details`, F12 percentile-displacement gap (remainder → candidate);
scorer surfaced a 6th, `grid_for()` shadowing BF. CI 7/7; `check()` 0/0/0.
