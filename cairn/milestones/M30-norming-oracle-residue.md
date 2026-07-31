# M30: Norming-family test oracles and internal consistency

- **Status:** in-progress
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** IP2, GP2
- **Branch/PR:** `m30-norming-oracle-residue`

## Goal

Close M29's review residue on the PID-5 norming family: replace the test
expectations that transcribe the implementation's own constants with a measured
oracle, and make the family's partition, error attribution, and messages
internally consistent.

## Scope

**In:** `tests/testthat/test-norm_pid5.R` — the metric/shift expectations that
copy `norm_engine.R`'s three partition vectors, the logical-column expectation
computed with `norm_convert()`, the tie-boundary fixture at 0.20, the vestigial
`suppressMessages()`, and the refusal half of the silence test that would have
passed pre-M29. `R/norm_engine.R` — a pairwise-disjointness assertion over the
three vectors, `call` threading so `norm_metric()`'s abort blames the exported
caller, and its `{version}` interpolation. `R/norm_pid5.R` — the non-numeric
abort's per-column class markup and pluralization, the two unreachable
`strip_prefix()` guards, the `prefix = ""` documentation gap, and the redundant
coverage scans. `cairn/DESIGN.md` and `CLAUDE.md` — `strip_prefix()` and
D-024/D-025's carve-out from the alert convention.

**Out:** the `validate_*()` helpers in `R/util.R` shared with `score_pid5()`,
`validity_pid5()`, and `rank_scales()` — their `.internal = TRUE` idiom, raw
glue interpolation of the internal `unit`/`arg` literals, and the pre-existing
`stopifnot()` on `prefix` → a `candidate` row, since a change there ripples
across the scoring family and past this milestone's characterization evidence.
Any change to a returned T score or percentile → nothing plans it; AC6 forbids
it here.

## Acceptance criteria

- [ ] AC1. The tests asserting how much `detachment`, `total`, `PRD`, `INC`,
      `INCS`, and `ORS` move under a four-option coding shifted off 0-3 derive
      their expected values from a fixture dataset **and its shifted copy**
      (`data + low`), each scored on its own matching `srange` through
      `score_pid5()`/`validity_pid5()`, asserting the observed per-scale
      difference against `norm_shift()`'s output. No expected value in these
      tests is copied from `norm_mean_scales`, `norm_sum_scales`,
      `norm_invariant_scales`, or `norm_metric()`, and the per-scale
      `"mean"`/`"sum"`/`"invariant"` label expectations at
      `test-norm_pid5.R:218-221` and `:234` are gone; what remains of those
      tests asserts that a covered scale classifies without aborting. The
      existing PRD test (`:410-425`), which already asserts against
      `pid_items` keying and a printed `pid_norms` cell, keeps its oracle and
      the new differential test lands beside it. The file's IP2 header states
      the differential oracle and why it is not self-reference.
- [ ] AC2. A test asserts `norm_mean_scales`, `norm_sum_scales`, and
      `norm_invariant_scales` are pairwise disjoint, and fails when any scale
      name is added to a second vector.
- [ ] AC3. Every test added or rewritten under AC1, AC2 and AC5, plus the
      refusal half of the one-`suppressWarnings()` test, is shown to fail under
      a named mutation of the code it locks; the work log records each mutation
      and that it was reverted.
- [ ] AC4. With `norm_covers()` mocked `TRUE`, the abort `norm_pid5()` raises on
      a covered-but-unclassified scale carries a `conditionCall()` of
      `norm_pid5()` rather than `norm_metric()`, matching the `call`-threading
      convention DESIGN.md records for the validators.
- [ ] AC5. Two aborts are fixed and each is locked by a test on its rendered
      text: `norm_metric()`'s interpolates `version` bare, as its only sibling
      report in `norm_pid5()` does; and `norm_pid5()`'s non-numeric abort emits
      **one bullet per offending column**, each labelling that column's class
      with cli's `{.cls}`, with a headline that interpolates the offending count
      before pluralizing (a bare `{?s}` aborts at runtime — LESSONS 2026-07-30).
      The existing both-offenders-in-one-abort test is updated to the new shape.
- [ ] AC6. No returned value changes and the profile gate is clean: a
      characterization harness scores `sim_pid5` (FULL), `sim_pid5sf` (SF), and
      `sim_pid5bf` (BF), each on its own version, runs `norm_pid5()` over
      `srange` in `{c(0,3), c(1,4), c(0,4)}`, and reports `identical()` for
      every configuration before and after the branch; `devtools::document()`
      produces no diff, `devtools::test()` 0 failures / 0 warnings, and
      `devtools::check()` 0 errors / 0 warnings / 0 notes (M29's review is the
      baseline for the absolute form).
- [ ] AC7. `DESIGN.md`'s internal-utilities paragraph lists `strip_prefix()`,
      its "User communication" convention records D-024/D-025's carve-out
      (reports a caller is expected to catch or suppress are `cli::cli_warn()`;
      count-of-flagged-observations screening output stays `cli_alert_*`), and
      `CLAUDE.md`'s messaging convention points at the carve-out in one clause.

## Coverage

- AC1 → T1, T2
- AC2 → T3
- AC3 → T1, T2, T3, T5, T6
- AC4 → T4
- AC5 → T5
- AC6 → T7, T8
- AC7 → T9

## Tasks

- [x] T1. Build the differential fixture: one PID-5 dataset and its `data + low`
      copy, scored through `score_pid5()`/`validity_pid5()` on `c(0, 3)` and the
      shifted `srange`, exposing an observed per-scale difference.
- [x] T2. Rewrite the metric/shift expectations on that fixture; drop the
      per-scale label expectations at `test-norm_pid5.R:218-221` and `:234`,
      keeping the no-abort substance; keep `:410-425`'s published-cell oracle
      and add the differential PRD assertion beside it; restate the file's IP2
      header.
- [x] T3. Add the pairwise-disjointness assertion over the three vectors in
      `R/norm_engine.R:114-123`.
- [x] T4. Thread `call` from `norm_pid5()` into `norm_metric()`
      (`R/norm_engine.R:125-153`) and test the attribution.
- [x] T5. Fix the two aborts per AC5 — bare `{version}` in `norm_metric()`;
      one bullet per bad column with `{.cls}` and a counted headline in
      `R/norm_pid5.R:195-207` — and update the both-offenders test.
- [x] T6. Test hygiene: replace the logical-column expectation computed with
      `norm_convert()`, move the 0.20 prefix fixture off the tie boundary, drop
      the vestigial `suppressMessages()`, and make the refusal half of the
      silence test fail pre-M29's condition class.
- [ ] T7. Write the characterization harness, capture the pre-branch baseline
      before any code change, and re-run it at the end.
- [ ] T8. Cheap polish in files already touched: the two unreachable
      `strip_prefix()` guards, the `prefix = ""` documentation gap, and the
      redundant coverage scans in `norm_pid5()`.
- [ ] T9. DESIGN.md + CLAUDE.md entries for `strip_prefix()` and the
      alert-convention carve-out.

## Work log

- 2026-07-31: created by /milestone-plan.
- 2026-07-31: criteria audit ([O], fresh context) returned 12 findings across 7 drafted criteria; 7 had one clear right answer and were applied before the gate (shifted-copy fixture, AC1 scoped to enumerated tests, PRD test kept alongside, AC2 falsifiability clause dropped, counted pluralization, AC5 scoped to two named aborts, AC6 matrix pinned to per-dataset versions), 3 became gate questions, 1 was overridden (check() "0 notes" is evidenced by M29's Review section, which the auditor could not see post-archive), 1 was informational.
- 2026-07-31: plan gate chose asserting the measured shift magnitude over asserting the `"mean"`/`"sum"`/`"invariant"` labels because mapping a measured difference back to a label re-transcribes `norm_shift()` and relocates the IP2 violation rather than closing it; falsified by a mutation that changes a scale's label without changing its shift magnitude.
- 2026-07-31: plan gate chose one abort bullet per offending column over `{.cls}` for the single-offender case only because cli's `{.cls}` collapses several classes into one union label and cannot reproduce the per-column pairing; falsified by a rendered-message test showing the multi-column abort still carrying a hand-rolled class string.
- 2026-07-31: plan gate chose substantive findings plus cheap polish over the full 24 because the polish items in `R/util.R`'s shared validators ripple into the scoring family and past this milestone's characterization evidence; falsified by that ripple turning out to be confined to the norming family.
- 2026-07-31: branch `m30-norming-oracle-residue` cut from main at 12b7887; status in-progress.
- 2026-07-31: T7's baseline capture reordered ahead of T1 (minor amendment) — the harness must read the pre-change code; 9 configurations captured to a scratch RDS.
- 2026-07-31: T1-T3 done — `observed_shift()` scores a dataset and its `data + low` copy on matching `srange` and differences per scale; `norm_shift()` is asserted against the measured shift for FULL/SF/BF x low 1,2; the per-scale label expectations at the old :218-221 and :234 are gone; the three vectors are asserted pairwise disjoint. `test_file()` 406 pass / 0 fail.
- 2026-07-31: AC3 mutations for T1-T3, each observed red and reverted — "PRD" added to `norm_mean_scales` (disjointness 1 failure, differential 2); `out[metric == "mean"] <- low + 1` (differential 6 failures); "ORS" moved from invariant to mean (4 failures).
- 2026-07-31: T4-T6 done — `norm_metric()` takes `call = rlang::caller_env()` and the abort now reads `norm_pid5()`; the abort names the version bare and pluralizes off the unclassified count; the non-numeric abort emits one `{.cls}` bullet per offending column (an ordered factor reads `<ordered/factor>`); logical-column and `prefix` fixtures read printed cells instead of `norm_convert()`, the prefix fixture moved off the 0.20 tie, the vestigial `suppressMessages()` dropped, and the refusal half now also asserts `suppressMessages()` does not reach it. NEWS updated.
- 2026-07-31: cli takes a plural marker's quantity from the last value interpolated before it, so `{cli::qty(n)}` at the head of a string is cancelled by an intervening `{version}` — qty moved adjacent to the marker.
- 2026-07-31: AC3 mutations for T4-T6, each observed red and reverted — dropping `call = call` (attribution test, 2 failures); `{.val {version}}` restored (consistency test, 2); single hand-rolled class bullet (consistency test, 2); the refusal reverted to `cli_alert_warning()` (silence test 2, plus 5 elsewhere).
- 2026-07-31: `devtools::document()` no diff; `devtools::test()` 10400 pass / 0 fail / 0 warn / 1 pre-existing skip.

## Decisions

## Review
