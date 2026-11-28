# M034: A second spot-value anchor per normed PID-5 column

**Status:** done (2026-07-31, PR #37 https://github.com/jmgirard/hitop/pull/37)

**Goal:** Give every T-scored column in `pid_norms` a second hand-read anchor,
placed to catch a percentile column displaced down one row and a swap of two
columns whose anchors coincide.

**Outcome:** 63 anchors added as `second_spot` in `test-norms.R` (`tscored_spot`
133 rows, all 66 T-scored columns). New `data-raw/select_norm_anchors.R` assigns
each a T from a frozen order (44, 64, 63, 45, 46) — shared values, so the read
was five row scans not 63 lookups. Three adequacy tests (>=2 distinct-T anchors,
step placement, distinctness over 2,145 pairs), all red without `second_spot`.
`swap_columns()` adds two swap mutations; all 13 now CAUGHT — M033's two NOT
CAUGHT included — each with the book test among the failures. Values hand-read by
a fresh reader barred from `pid_norms`, the CSVs and the test file; 63/63 matched.
Upward displacement open (32 columns need a third anchor) → candidate.

**Decisions:** none milestone-local, none promoted; approach choices in the work log.

**Review:** 3-lens fan-out; blame-history and prior-PR-comments 0 (no inline PR
comments exist here — archived `## Review` sections were the evidence base),
diff-bug 9 scored 25–90. One actioned: F1 (90), a comment claiming 41 columns
would need a third anchor when the post-M034 figure is 32; fixed in the comment
and the ROADMAP row. CI 7/7; `check()` 0/0/0. Two lessons captured, none retired.
