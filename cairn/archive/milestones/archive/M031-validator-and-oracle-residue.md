# M031: Argument-validation consistency and a harder norming oracle

**Status:** done (2026-07-31, PR #34 https://github.com/jmgirard/hitop/pull/34)

**Goal:** Put every argument check in `R/` on the one cli validator mechanism, and
make the norming differential oracle fail under regressions it tolerated.

**Outcome:** `validate_string(x, arg, allow_null)`/`validate_flag(x, arg)`/
`validate_count(x, arg, max)` (`R/util.R`) replace all 22 `stopifnot()` calls in 8
files on the `arg`/`call` convention, so an abort names the argument and blames the
exported function; plus `validate_scales()`'s supplied-type bullet, `call` into
`warn_item_order()`, `dir` on `rlang::arg_match()` (factor coerced first), and
`score_engine()`'s unreachable `missing` check dropped. The `norm_shift()` test gained
a missing-PRD-item fixture, the whole covered set via `expect_setequal()` replacing
`any(keep)`, and a negative `low`. `devel/characterize_m31.R` (33 configs) and
`devel/acceptance_probe_m31.R` (187 accept/reject pairs vs the base ref) show no change.

**Decisions:** None milestone-local. Two gated amendments: `missing`/`version` keep
`match.arg()` (only `dir` moved); AC4's false "enables partial matching" clause
corrected — `arg_match()` requires an exact match.

**Review:** Blame-history and prior-review lenses found nothing; diff-bug returned 12.
One scored 92 and was actioned — `arg_match()` rejected a factor `dir` the old check
accepted, falsifying AC4 — causing one return, fixed and re-verified by the acceptance
probe. Two sub-threshold (72, 70) fixed as factual errors this milestone wrote; 10 logged, top two graduated to a candidate row.
