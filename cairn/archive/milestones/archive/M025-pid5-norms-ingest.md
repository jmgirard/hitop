# M025: PID-5 normative tables — verification and ingest

**Status:** done (2026-07-30, PR #28 https://github.com/jmgirard/hitop/pull/28)

**Goal:** Ship the maintainer-transcribed PID-5 / PID-5-SF / PID-5-BF normative tables
as one verified, source-cited, documented dataset with oracle tests.

**Outcome:** Exported `pid_norms` — 1,056 rows over `version`/`scale`/`tscore`/`raw`/
`percentile`, built by `data-raw/norms_pid5.R` from the seven `norms_*.csv` tables: raw
and percentile at each T for the FULL/SF/BF domains and the BF `total`, percentile at
each raw score for `INC`, `INCS`, `ORS`, `PRD`. `scale` holds the
`score_pid5()`/`validity_pid5()` column stems, so a lookup joins with no crosswalk.
`data-raw/verify_norms_against_book.R` diffs all seven against the source's markup;
`tests/testthat/test-norms.R` holds three structural invariants, 34 page-cited spot
values, and an anchor-per-scale rule, re-run against corruption by `mutate_norms_check.R`.

**Decisions:** M025-D1 a committed extractor replaces the planned fresh-reader
transcription. M025-D2 the 18-cell correction ledger with IP1 sign-off. M025-D3/D4 keep
`INC` ([D-018](../../DECISIONS.md)) and record PID-5-PRD, not "PIM-RD". M025-D5 fixes the
column names, `total` for the BF total, and no book-table column.

**Review:** 3 lenses, 19 findings (all from [O] diff-bug; both [S] lenses clean). Fixed
at ≥ 80: F1 (92) BF `disinhibition` had no spot anchor, closed as a class; F4 (85) the
`raw` metric was mis-described for FULL/SF. Two 78s fixed at the maintainer's direction;
15 logged. One lesson captured, none retired.
