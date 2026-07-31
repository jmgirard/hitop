# M29: `norm_pid5()` hygiene and robustness

- **Status:** in-progress
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** GP2, GP3
- **Branch/PR:** `m29-norm-pid5-hygiene`

## Goal

Close the nine sub-threshold review findings M27 and M28 left on the PID-5
norming family, so `norm_pid5()` fails loudly on bad input, never returns a
silently wrong number, and is silenced by one suppression call.

## Scope

**In:** `R/norm_pid5.R`, `R/norm_engine.R`, the two shared validators in
`R/util.R` that `norm_pid5()` calls, `R/rank_scales.R`'s matching `prefix`
bug, and `tests/testthat/test-norm_pid5.R`. Input validation, loud failure on
an unclassifiable scale, one report mechanism, and the test-hygiene gaps.
Also the user-facing prose describing the two reports being reclassified:
`NEWS.md` and the norming paragraphs of the three PID-5 scoring vignettes
(`vignettes/pid5_scoring.Rmd`, `pid5sf_scoring.Rmd`, `pid5bf_scoring.Rmd`),
which today call them "messages".

**Out:** any change to a converted value on a valid call — the lookup, tie,
capping and shift arithmetic M27 and M28 verified stay untouched.
`validity_pid5()`'s deferred cut-score reconciliation → still deferred
(D-020). Facet-level norms → the `pid_norms` extension candidate. Profile
plots → the norm-referenced plotting candidate.

## Acceptance criteria

- [ ] AC1. `norm_pid5()` and `rank_scales()` both strip `prefix` from a column
      name by literal match, never as a regex: `prefix = "pid(_"` on a column
      named `pid(_detachment` returns that column's `_t`/`_ptl` values with no
      regex-compilation error, and `prefix = "p.d_"` on `pXd_detachment`
      leaves the name unstripped — both conversion columns `NA`, the column
      named in the uncovered-scale report. `rank_scales()`'s roxygen
      (`R/rank_scales.R:15-17, 42`) no longer promises regex semantics. NEWS
      records the change (GP2).
- [ ] AC2. `norm_pid5()` validates `scores` before any conversion, and every
      error it raises about that argument names `scores` — never `items` or
      `scales`: a duplicated entry aborts naming the duplicates (as
      `score_pid5()` does), and a factor or character score column aborts
      naming the column instead of being coerced through `as.numeric()` (a
      logical column is still accepted). Errors about `data` and `srange`
      keep naming those arguments, and the scoring and validity families'
      validator messages are unchanged, evidenced by their current tests
      passing unedited. NEWS records both aborts (GP2).
- [ ] AC3. `norm_metric()` takes `version` alongside `scale` and aborts when
      asked to classify a scale `pid_norms` covers for that version but its
      metric partition does not name, so a `PRDS` or `SDTD` row added to
      `pid_norms` fails loudly rather than silently taking the item-mean
      formula. The abort is exercised by a test injecting such a scale through
      a mocked `norm_covers()` (`pid_norms` is lazy data and cannot be rebound
      by `local_mocked_bindings()`). A scale `pid_norms` does not cover — the
      25 facets — still classifies without error, and
      `tests/testthat/test-norm_pid5.R:249` passes unedited.
- [ ] AC4. `norm_shift()` names the `validity_pid5()` `rowSums()`-without-
      `na.rm` dependency in a comment citing `R/validity_pid5.R:172`, and
      `tests/testthat/test-norm_pid5.R` carries a test asserting the norming
      consequence — a `PRD` with a missing item is `NA` both before and after
      the `low × nItems` correction, so the correction is never applied to a
      partial sum — rather than re-asserting `test-validity_pid5.R:44`, which
      already turns red on `na.rm = TRUE` today.
- [ ] AC5. One `suppressWarnings()` call silences `norm_pid5()` entirely: all
      four reports — the option-count refusal, the reconciliation report, the
      uncovered-scale report, and the capping report — are `cli::cli_warn()`
      conditions. Tested in the refusal case (`srange = c(0, 4)`, which emits
      the refusal alone, the other three being gated off) and in the shifted
      case (one call emitting reconciliation, coverage and capping together),
      with no console output remaining in either.
- [ ] AC6. `tests/testthat/test-norm_pid5.R` no longer defines a helper named
      `capture_warnings`, so nothing shadows the `testthat` export of that
      name, and two previously untested interaction paths are covered: a
      negative `low` (`srange = c(-1, 2)`) reconciles an item mean by adding
      1, and a coding both shifted and of the wrong option count
      (`srange = c(1, 5)`) reports only the option-count refusal.
- [ ] AC7. The profile gate is clean: `devtools::document()` produces no diff,
      `devtools::test()` passes, and `devtools::check()` reports 0 errors and
      0 warnings.

## Coverage

- AC1 → T1
- AC2 → T2, T3
- AC3 → T4
- AC4 → T5
- AC5 → T6
- AC6 → T7
- AC7 → T8

## Tasks

- [x] T1. Replace the regex strip at `R/norm_pid5.R:161` and
      `R/rank_scales.R:106` with a literal prefix match; rewrite
      `rank_scales()`'s roxygen promise; test both failure modes.
- [x] T2. Add `validate_item_uniqueness(scores)` and a numeric-type guard
      ahead of the `as.numeric()` at `R/norm_pid5.R:205`; test both aborts.
- [x] T3. Give `validate_scales()` / `validate_items_present()` an
      argument-name parameter in `R/util.R` (default preserving today's
      wording), thread `"scores"` from `norm_pid5()`, and confirm the scoring
      and validity test files pass unedited.
- [ ] T4. Add `version` to `norm_metric()`, abort on a covered-but-
      unclassified scale, update the call site at `R/norm_pid5.R:193`, and
      test through a mocked `norm_covers()`.
- [ ] T5. Comment the `na.rm` coupling at `norm_shift()`; add the
      missing-item `PRD` norming test.
- [ ] T6. Convert the coverage and capping reports (`R/norm_pid5.R:258-278`)
      to `cli::cli_warn()`; add whole-function silence tests for both coding
      cases.
- [ ] T7. Rename the `capture_warnings()` helper at
      `tests/testthat/test-norm_pid5.R:271`; add the negative-`low` and
      shifted-plus-wrong-count interaction tests.
- [ ] T8. Sync `@details` for the new aborts and report classes, write the
      NEWS entries, and run the full gate.

## Work log

- 2026-07-31: created by /milestone-plan.
- 2026-07-31: criteria audit ran — a fresh-context [O] reader returned problems on four of seven drafted criteria (AC3 over-broad, AC4 unreachable with no test seam and an unstated signature change, AC5 vacuous, AC6 under-specified and conflicting with D-024) plus four gate flags; all four rewrites adopted, three flags taken to the gate, the GP2 flag folded into AC1/AC2.
- 2026-07-31: plan gate chose unifying all four reports onto `cli_warn()` over adding a `quiet` argument and over documenting the split, because D-024 already licensed the unification and the capping report carries none of the `dplyr::filter()` remedy wording its convention clause was written for; falsified by a user needing the four reports suppressible independently, or by a second norming function whose count reporting the alert convention plainly fits.
- 2026-07-31: plan gate chose fixing `prefix` in both `norm_pid5()` and `rank_scales()` over `norm_pid5()` alone, because one argument name meaning literal in one exported function and regex in another is the divergence a later milestone would have to close anyway; falsified by a user relying on `rank_scales()`'s documented regex `prefix`.
- 2026-07-31: plan gate chose aborting on factor and character score columns over also aborting on logical, because `as.numeric(TRUE)` is well defined and a 0/1 column converts correctly today; falsified by a logical column reaching the lookup and producing a number the caller did not intend.
- 2026-07-31: amendment (implement gate) — Scope grew to include `NEWS.md` and the three PID-5 vignettes' norming paragraphs, which describe the coverage and capping reports as "messages" that AC5 turns into warnings; chosen over a candidate row for the vignettes because a doc line contradicting the shipped condition class is the GP2 mismatch this milestone exists to close.
- 2026-07-31: implement gate chose amending the unreleased 0.2.0 `norm_pid5()` NEWS bullet in place, plus new bullets for `rank_scales()`'s literal `prefix` and `norm_pid5()`'s new aborts, over narrating a messages-to-warnings transition no released version ever exposed (`v0.1.0` is the only tag).
- 2026-07-31: T1 — `strip_prefix()` in `R/util.R` replaces the regex strip in both `norm_pid5()` and `rank_scales()`; `rank_scales()`'s roxygen now documents a literal match; NEWS bullet added; two new tests (metacharacter prefix that matches, `.`-bearing prefix that must not); `devtools::test()` clean.
- 2026-07-31: T2 — `norm_pid5()` now calls `validate_item_uniqueness()` and aborts naming any factor or character score column, both ahead of every report and conversion; a logical column still converts. Three new tests; `devtools::test()` clean.
- 2026-07-31: T3 — `validate_scales()`, `validate_items_present()` and `validate_item_uniqueness()` take an `arg` name (defaults reproduce today's wording; uniqueness also takes `unit` so the default sentence stays byte-identical for the scoring family), and `norm_pid5()` threads `"scores"` through all three. `tests/testthat/test-score_pid5.R`, `test-validity_pid5.R` and `test-validate.R` pass unedited (`git diff` empty). NEWS bullet added for the two new aborts.

## Decisions

## Review
