# M26: PID-5-BF total score across scoring, reliability, and the BF paper forms

**Status:** done (2026-07-30, PR #29 https://github.com/jmgirard/hitop/pull/29)

**Goal:** Ship the PID-5-BF total score that `pid_norms` already norms, across every
surface a `pid_scales` entry reaches.

**Outcome:** `score_pid5(version = "BF")` returns a `total` column — the item-level mean
over all 25 items (Markon et al. 2024, p. 23; traced in `SOURCES.md`), settling the rule
D-017 left open against the mean-of-domain-means alternative. Built as a
`pid_scales[["BF"]]` row, so it needs no new code path and reaches that table's other
consumers: `reliability_pid5(version = "BF")` returns six rows, and `pid5bf_{US,A4}.docx`
print a Total row (new `hitop_artifacts` rows; BF Qualtrics/REDCap stay byte-identical).
`data-raw/artifacts.R` gained validated `rebuild_stems`/`rebuild_formats` filters so one
instrument rebuilds without churning all 19 checksums. Three falsified prose claims fixed.

**Decisions:** [D-021](../../DECISIONS.md) the total prorates independently of the domains
under `missing = "apa"` (up to 3 `NA` domains beside a reported total; the converse is
unreachable) — promoted from M26-D1 at the merge gate, with M26-D2 recording the
promotion. [D-019](../../DECISIONS.md), authored at plan time, carried the ripple.

**Review:** 3 lenses + scorer; 16 findings, **none ≥ 80**, all logged. Fixed anyway: a
tautological DOCX assertion (75) and a stale `@examples` comment (62); the prior-review
lens caught a third stale copy of a claim M25's own review had fixed. Maintainer declined
the first merge chip pending F1/F9; both fixed. Suite 9976, check 0/0/0, CI green.
