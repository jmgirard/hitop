# M084: The `label_*()` family's unpadded-item report

- **Status:** in-progress
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** GP3
- **Resolves:** —
- **Branch/PR:** `m084-unpadded-item-report`

## Goal

The three `label_*()` helpers report a prefixed item column they could not label
whether or not any other column matched, and describe what they found.

## Scope

Surface tier: **user-facing** — three exported functions' warning behavior and
their documentation.

**In:** `unpadded_item_cols()` and `warn_unpadded_items()` (`R/util.R:590`) and
their three call sites in `R/label_pid5.R`, `R/label_hitopsr.R` and
`R/label_hitopbr.R`; the three defects and the test gap M083 filed (F1, F2, F3,
F9). F1: each helper returns early on "no columns matched" before the report
runs, so a frame whose item columns are *all* mis-padded escapes
`hitop_unpadded_items`. F2: a column padded to the form's width but naming no
item of the form is called unpadded, and the hint hands back the offending name.
F3: the third plural marker keys on `{width}`, so a one-column report reads
"they were not labelled". F9: every existing unpadded test leaves padded
neighbours in the frame, so none can reach F1. Tests, roxygen, `man/` and a
`NEWS.md` bullet.

**Out:** the `rename_*_items()` reports, `rename_hitopsr_items()`'s classless
no-match warning, labelling `validity_pid5()` and `_se` columns, and inferring
`version` — all on the standing "four remainders M082 and M083 left" candidate
row. A new condition class for out-of-range columns → declined at the plan
gate; the same class carries both sentences.

## Acceptance criteria

- [ ] AC1: On a frame whose every prefixed item column is mis-padded so that no
      column matches an expected item name — at most five such columns, cli
      truncating longer lists — each of `label_pid5()` (FULL, SF and BF),
      `label_hitopsr()` and `label_hitopbr()` raises its existing no-match
      warning *and* a warning of class `hitop_unpadded_items` naming every
      mis-padded column. The SR probe uses a wider spelling (`hsr_0001`), since
      `hsr_1`..`hsr_405` still matches items 100 and up.
- [ ] AC2: A column carrying the prefix and a number padded to the form's width
      but naming no item of the form (`pid5_221`, `pid5sf_101`, `pid5bf_26`,
      `hsr_406`, `hbr_46`) is reported under class `hitop_unpadded_items` in its
      own sentence, which states the form's item-number range, not in the
      not-zero-padded sentence; the report's "expected as" hint never repeats an
      out-of-range column's own name. A frame carrying one out-of-range and one
      mis-padded column reports each in its own sentence, on each instrument.
- [ ] AC3: Every plural marker in the report keys on the number of columns the
      sentence carrying it reports: a one-column sentence reads "1 column is …
      was not labelled" and a two-column sentence "2 columns are … were not
      labelled", at both instrument widths the helpers pass (2 and 3).
- [ ] AC4: Each of `man/label_pid5.Rd`, `man/label_hitopsr.Rd` and
      `man/label_hitopbr.Rd` states that the report is raised whether or not any
      column matched, and that a padded number outside the form's range is
      reported as out of range; `grep -RF "That warning is raised only when at
      least one column did match" R/ man/` returns nothing.
- [ ] AC5: Correctly padded frames stay silent — the shipped `sim_*` and `ku_*`
      item frames, a module subset (`ku_hitopsr[, c("participant", "hsr_001",
      "hsr_002")]`), and a frame with no prefixed column (`data.frame(a = 1)`)
      raise no `hitop_unpadded_items` warning, the last raising only the
      no-match warning.
- [ ] AC6: `NEWS.md` carries a bullet for the development version describing the
      changed report.
- [ ] AC7: `Rscript -e 'devtools::test()'` is clean, `Rscript -e
      'devtools::document()'` produces no diff, and `Rscript -e
      'devtools::check()'` reports no error, warning or note absent from the
      baseline recorded in this file's work log at implementation start.

## Coverage

- AC1 → T2, T3
- AC2 → T4, T5
- AC3 → T4, T5
- AC4 → T7
- AC5 → T6
- AC6 → T7
- AC7 → T1, T8

## Tasks

- [x] T1: Record the pre-work `devtools::check()` and `devtools::test()` results
      as one work-log line — the baseline AC7 measures against.
- [x] T2: Add the all-mispadded probes: `test-label_pid5.R` for FULL, SF and BF;
      `test-label_scales.R` for SR (wider spelling) and BR. ≤5 columns per
      probe, asserting the class and every named column. Run each against the
      current `R/` and record it red.
- [x] T3: Move `warn_unpadded_items()` ahead of the no-match early return in
      `R/label_pid5.R:83`, `R/label_hitopsr.R:45` and `R/label_hitopbr.R:46` so
      both warnings fire. T2 green; the existing no-match assertions
      (`test-label_pid5.R:185`, `test-label_scales.R:206,220`) still pass. Sweep
      every test that merely *calls* a label helper for a newly raised warning
      (LESSONS M029).
- [x] T4: Split `unpadded_item_cols()` (`R/util.R:590`) into mis-padded and
      out-of-range groups; rewrite `warn_unpadded_items()` (`R/util.R:594`) to
      give each group its own sentence, state the form's item-number range in
      the out-of-range sentence itself (AC2's wording; a separate `i` hint would
      only repeat it), and place `cli::qty()` immediately before each
      plural marker (LESSONS M030). Update the three call sites' arguments.
- [x] T5: Tests for T4 — out-of-range probes on all five forms, a mixed
      out-of-range + mis-padded frame per instrument, and one- versus two-column
      wording at both widths. Show each red by reverting T4's change before
      trusting its green.
- [x] T6: Silence-regression tests for AC5, capturing conditions with
      `withCallingHandlers`, never `expect_no_warning(message = )`, which takes
      `message` as a regexp selector and passes on a raised warning (LESSONS
      M032).
- [ ] T7: Update the three helpers' `@param prefix` and `@return` roxygen, run
      `devtools::document()`, add the `NEWS.md` bullet.
- [ ] T8: Run `devtools::test()` and `devtools::check()`; compare against T1's
      baseline and record the comparison.

## Work log

- 2026-09-02: created by /milestone-plan, promoting the standing candidate row
  (lineage M077, M083 findings 1, 2, 3, 9).
- 2026-09-02: plan gate chose raising both the no-match and the padding warning
  over replacing the no-match warning because callers catching the existing
  warning keep working and one `suppressWarnings()` still silences both;
  falsified by a report that the paired warnings read as contradictory.
- 2026-09-02: plan gate chose an own-sentence out-of-range report under the
  existing class over a silent skip and over a new condition class because a
  typo'd item number stays visible and no new public name is minted; falsified
  by a caller needing to catch out-of-range columns apart from mis-padded ones.
- 2026-09-02: plan gate chose a baseline-relative check bar over `check()`
  0/0/0 because a pre-existing note would otherwise block work that did not
  cause it; falsified by the baseline hiding a note this milestone introduced.
- 2026-09-02: T1 baseline at the branch point (6f75befc's parent): `devtools::test()` FAIL 0 / WARN 0 / SKIP 9 / PASS 17043; `devtools::check()` Status OK, 0 errors / 0 warnings / 0 notes.
- 2026-09-02: implementation gate chose reporting a column that is both mis-padded and out of range under out-of-range alone over the padding sentence or both, because the padding hint would otherwise name a column the form does not carry either; and chose the no-match warning ahead of the padding report, matching the order a partly-matching frame already reads in.
- 2026-09-02: T2 added five all-mis-padded probes (`label_pid5()` FULL/SF/BF, `label_hitopsr()` at `hsr_0001`, `label_hitopbr()` at `hbr_1`) plus the `collect_warnings()`/`warning_text()` test helpers; all five red against the current `R/`, each raising 1 warning where 2 are expected.
- 2026-09-02: T3 ran the report after the no-match warning instead of behind an early return in all three helpers; the five T2 probes green, and the full suite FAIL 0 / WARN 0 / SKIP 9 / PASS 17088 with no other test newly raising a warning.
- 2026-09-02: T4 split `unpadded_item_cols()` into `mispadded` and `out_of_range` and gave `warn_unpadded_items()` the signature `(cols, prefix, expected, max_n, instrument)`, folding the split in; the three call sites pass `max_n` instead of a digit width. Minor task edit: T4 now says the range is stated in the out-of-range sentence rather than in a separate `i` hint, which would only repeat it.
- 2026-09-02: T5 added out-of-range, mixed and one-versus-two-column probes on all five forms. Discrimination: planting the pre-split classification (every unmatched column called mis-padded) reddened 20 assertions across both files; planting the missing `cli::qty()` reddened the 6 pluralization assertions. Full suite FAIL 0 / WARN 0 / SKIP 9 / PASS 17159.
- 2026-09-02: T6 added the silence regression over the eight shipped item frames, two module subsets and a frame with no prefixed column, capturing with `withCallingHandlers`; each shipped frame is also shown to label something, so the domain cannot silently empty. Discrimination: reporting every prefixed column regardless of `expected` reddened it. Full suite PASS 17186.
- 2026-09-02: criteria audit ran in full mode ([O] fresh reader, user-facing
  tier); it returned findings on AC1 (unsatisfiable — cli truncates lists past
  five, and `hsr_1`..`hsr_405` does not reach the no-match path), AC2 (no class
  or text pinned; FULL-only probe), AC3 (one of three markers pinned), AC4
  (unbounded negative, vacuous for two of three files), AC6-as-drafted (a
  property of the checker's report, no baseline) and two additions (silence
  regression; the two-report contract). All were disposed at the gate — five
  fixed in the wording above, the two-report contract and the check bar posed
  as questions.

## Decisions

## Review
