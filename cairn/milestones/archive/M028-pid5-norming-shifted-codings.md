# M028: PID-5 norming under shifted response codings, and the vignette norming sections

**Status:** done (2026-07-30, PR #31 https://github.com/jmgirard/hitop/pull/31)

**Goal:** Let `norm_pid5()` convert scores collected on a non-official four-option coding by
reconciling each scale to the official 0–3 range (D-020, D-023), and show the norming workflow in
the three PID vignettes.

**Outcome:** `norm_pid5()` accepts any four-option `srange`. Internal `norm_metric()` classifies
each scale — `"mean"` (domains, BF total), `"sum"` (`PRD`), `"invariant"` (`INC`, `INCS`, `ORS`) —
and `norm_shift()` subtracts what a coding starting at `low` adds: `low` per item mean,
`low × nItems` for `PRD` (count read from `pid_items`), 0 otherwise, before the untouched M027
lookup. A non-four option count returns `NA` everywhere with one `cli_warn()`; a shifted coding
warns once naming adjusted vs invariant scales. `@details` covers all five treatments plus the
unreconciled `validity_pid5()` cut scores (Known issue #3); three vignettes gained norming sections.

**Decisions:** D-024 (shifted-coding reports are `cli_warn()` conditions, annotating D-020's
`cli_alert_info()` clause); none milestone-local.

**Review:** Three lenses; prior-review found nothing and confirmed M027's five fixes intact,
blame-history confirmed per-observation capping survived the guard rename, diff-bug raised 16 and
built a shift-every-item oracle confirming the arithmetic exactly (+1 per mean, 0 `INC`/`ORS`, +22
`PRD`). Actioned: F8 (88) the FULL vignette claimed four normed validity scales where `pid_norms`
has three; F2 (80) `@details` overstated when the warning fires; F3 (78) fixed alongside; 13 logged
below threshold. At the gate the maintainer took AC3's line pointer into `@details` over amending.
