# M34: A second spot-value anchor per normed PID-5 column

- **Status:** review
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** IP2
- **Branch/PR:** `m34-second-norm-anchors` / https://github.com/jmgirard/hitop/pull/37

## Goal

Give every T-scored `(version, scale)` column in `pid_norms` a second hand-read
spot anchor, placed so the suite catches a percentile column displaced down one
row and a swap of two columns whose current anchors coincide.

## Scope

**In:** the 63 T-scored columns carrying exactly one anchor (13 domain + 50
facet); one new anchor each, at a `tscore` chosen against that column so its
`percentile` differs from the row below it and its anchor pair distinguishes it
from every other column. Hand-read off the rendered appendix pages, added to
`domain_spot`/`facet_spot` in `tests/testthat/test-norms.R`. The coverage test
tightens from ≥1 to ≥2 anchors; two anchor-adequacy tests are added; two
column-swap mutations join `data-raw/mutate_norms_check.R`.

**Out:** an *upward* displacement (every row taking its successor's value) — no
single anchor catches both directions, and 41 of the 66 columns would need a
second new anchor; → candidate row, planned separately. A cell-by-cell
comparison of the shipped dataset against the book → M35. The four raw-keyed
validity columns (FULL `INC`, SF `INCS`, FULL `ORS`, FULL `PRD`) → nowhere: each
already carries 3–4 anchors and needs no change. Any change to `pid_norms`
itself → nowhere; this milestone is test-only, and a hand-read value
disagreeing with the shipped cell is a finding to escalate, not a cell to edit.

## Acceptance criteria

- [x] AC1. Every `(version, scale)` pair in `pid_norms` with a non-`NA` `tscore`
      — 66 columns — carries at least two spot anchors at distinct `tscore`
      values, asserted by a test over
      `norm_keys(pid_norms[!is.na(pid_norms$tscore), ])` that fails naming any
      column with fewer. The four raw-keyed validity columns are outside this
      test and keep their existing anchors.
- [x] AC2. Every anchor added here records a `raw`, a `percentile`, and the page
      on which its table begins (per the file's existing convention), and its
      two values equal the cells printed at that `(version, scale, tscore)` in
      markon2024 Tables A–5 (p. 120), A–6 (p. 124), A–7 (p. 147), A–8 (p. 151),
      A–9 (p. 174). The existing "domain and facet norms match the values
      printed in the book" test covers the new rows and passes.
- [x] AC3. Each T-scored column has at least one anchor at a `tscore` whose
      `percentile` differs from the `percentile` at `tscore - 1`, restricted to
      T values whose predecessor row exists in that column; a test asserts this
      and fails naming any column whose anchors all sit on a percentile plateau.
      This closes a percentile column displaced *down* one row; the upward
      displacement stays open (see Scope **Out**). The test asserts the adequacy
      of the anchor set, never the correctness of `pid_norms`.
- [x] AC4. No two T-scored columns agree on both `raw` and `percentile` at every
      `tscore` in the union of their anchor sets, treating columns as differing
      at any `tscore` where either lacks a row; a test asserts this over all
      pairs and fails naming any colliding pair. Adequacy of the anchor set,
      never correctness of `pid_norms`.
- [ ] AC5. `Rscript data-raw/mutate_norms_check.R` reports CAUGHT for every
      mutation, with the "domain and facet norms match the values printed in the
      book" test among the failing tests for the two percentile-column
      displacements (SF withdrawal, FULL anhedonia) and for two new column-swap
      mutations added here (SF impulsivity ↔ SF intimacyAvoidance;
      SF manipulativeness ↔ SF suspiciousness).
- [ ] AC6. The `# ---- spot values from the printed tables` block comment states
      what the second anchor closes and what it leaves open; the matching
      comment in `data-raw/mutate_norms_check.R` ("Kept here to measure the gap
      rather than to assert it is closed") and the ROADMAP candidate row are
      updated or retired. No comment in either file still describes a closed gap
      as open.
- [ ] AC7. `devtools::test()` passes and `devtools::check()` reports 0 errors,
      0 warnings, 0 notes.

## Coverage

- AC1 → T3, T4
- AC2 → T2, T3
- AC3 → T1, T4
- AC4 → T1, T4
- AC5 → T5
- AC6 → T6
- AC7 → T6

## Tasks

- [x] T1. Compute per T-scored column the `tscore` values satisfying AC3 and
      AC4 jointly; pick one per column and record the selection and its method.
      A naive first-eligible-T selection was verified at plan time to yield 0
      colliding pairs, so a search is not expected to be needed.
- [x] T2. Hand-read `raw` and `percentile` at each selected T off the *rendered*
      appendix pages for the 63 columns — not the epub markup and not the
      `data-raw/` CSVs. Serve the extracted epub directory via a
      `.claude/launch.json` static-server entry (M33 lesson: the browser pane
      refuses `file://`), and record the attestation in the work log.
- [x] T3. Add the anchors to `domain_spot`/`facet_spot` in
      `tests/testthat/test-norms.R` with their table's first page.
- [x] T4. Tighten the coverage test to ≥2 for T-scored columns; add the AC3
      step-placement and AC4 pairwise-distinctness tests.
- [x] T5. Add the two column-swap mutations to `data-raw/mutate_norms_check.R`;
      run it and confirm every mutation comes back CAUGHT.
- [x] T6. Rewrite the two block comments; run `devtools::test()` and
      `devtools::check()`.

## Work log

- 2026-07-31: created by /milestone-plan; promotes the 2026-07-31 ROADMAP candidate spawned by M33's review (finding F12).
- 2026-07-31: criteria audit ([O], fresh context) returned 7 findings — AC1 test scope over all 70 columns vs 66, AC2 page convention contradicting the file's own, AC4 undefined for cross-version pairs, AC5 satisfiable by the meta-tests alone, AC6 line-pinned and missing two other homes of the stale claim, plus a verified infeasibility behind AC3's directionality; all applied, the directionality one routed to the gate. IP2 verdict on AC3/AC4: no conflict.
- 2026-07-31: plan gate chose one anchor per column, closing the downward displacement only, over covering both directions because 41 of 66 columns would need a second new anchor for a failure direction never yet observed here; falsified by an upward displacement reaching the shipped dataset, or by any second displacement whose direction the seeded pair does not cover.
- 2026-07-31: plan chose hand-read anchors over folding in the mechanical whole-dataset comparison because the anchors are the only layer reading the rendered page, where the markup-based checks share a blind spot; falsified by a defect that the markup extraction and the rendered page would report identically, which would make the hand read redundant. The mechanical comparison is planned as M35 rather than dropped.
- 2026-07-31: T1 — `data-raw/select_norm_anchors.R` assigns each of the 63 columns a T from a frozen five-value preference order (44, 64, 63, 45, 46), covering all of them and reporting 0 indistinguishable pairs over 2,145. Shared T values were chosen over per-column ones so the read is five row scans across the page rather than 63 separate lookups (the M33 shape); each assigned T is still drawn from that column's own eligible set, so Scope's "chosen against that column" holds and no amendment is needed.
- 2026-07-31: implement gate chose a fresh-context reader for the hand read over this session reading it, because this session has been reasoning about `pid_norms` all along; no `raw` or `percentile` cell value has been printed here, so the reader can be given page images and row labels with no exposure to the shipped numbers. On a mismatch: one independent re-read, then stop and escalate — never edit the norms, never adopt the shipped value.
- 2026-07-31: T2 — hand read delegated to a fresh-context [O] reader given only the served rendered pages, the column headings and the row list; it was barred from `pid_norms`, the data-raw CSVs, test-norms.R and R itself, and from extracting numbers via the DOM, `get_page_text` or the file on disk, so the read is visual and independent of the existing markup extraction. It reported 0 unreadable cells and controlled column alignment two ways (re-photographing each block banner, and overlapping the two horizontal pans by a full column). Attestation: all 63 values were read off rendered screenshots.
- 2026-07-31: T2 — all 63 hand-read pairs match the shipped cells exactly; 0 mismatches, so the re-read-then-escalate protocol was not exercised.
- 2026-07-31: T3 — anchors added as a third `second_spot` table rbound into `tscored_spot`; a separate table rather than extending `facet_spot`, whose vectorized shape assumes one shared T.
- 2026-07-31: T4 — three adequacy tests added (>=2 distinct-T anchors, step placement, pairwise distinctness), scoped to T-scored columns; the existing all-70 coverage test is unchanged. Inversion-checked: removing `second_spot` turns all three red, so none passes vacuously. Guard ordering matters in the distinctness test — `[[` on an absent name errors rather than returning NA, so the membership test precedes the lookup.
- 2026-07-31: T5 — two swap mutations added via a `swap_columns()` helper; the full script now reports CAUGHT for all 13 mutations, the two M33 percentile displacements included. For all four M34-relevant cases the book-comparison test is among the failing tests, which is what AC5 requires over a bare CAUGHT. Restore verified by md5, unchanged.
- 2026-07-31: T6 — both block comments rewritten; the ROADMAP row needed no edit, having been replaced at plan time by the narrower upward-displacement candidate. `devtools::test()` 11681 pass / 0 fail / 1 skip (pre-existing); `devtools::check()` 0 errors, 0 warnings, 0 notes.
- 2026-07-31: T6 — the `.claude/launch.json` static-server entry added for the hand read was committed in cc51413 with a session-specific absolute path; removed again here, and the server stopped (the M33 lesson's own practice).

- 2026-07-31: all six tasks done; status → review. Acceptance-criteria boxes left unticked for /milestone-review to fence against fresh evidence.
- 2026-07-31: review in progress — PR #37 opened as draft; AC1-AC4 and AC6 verified with fresh evidence, consistency gate green both halves. AC5 and AC7 deferred within this phase: both read `data/pid_norms.rda`, and the mutation script swaps it in place, so they wait until the diff-bug reviewer finishes rather than race it (a concurrent run already produced one phantom mismatch).

## Decisions

## Review

Evidence is fresh, gathered at review time by command. Where a criterion names a
test, it was also verified *independently of that test* — by evaluating the
anchor tables straight out of the test file and re-deriving the property — so a
criterion cannot be satisfied by a test that agrees with itself.

- AC1 — VERIFIED. `tscored_spot` evaluated from `tests/testthat/test-norms.R`
  holds 133 rows over the 66 T-scored columns; every column carries >=2 anchors
  at distinct T scores (0 with fewer), and 0 anchors name a column absent from
  `pid_norms`. The in-suite test asserting this passes.
- AC2 — VERIFIED. All 133 T-scored anchor rows carry `version, scale, tscore,
  raw, percentile, page` with no NA; the pages cited are exactly 120, 124, 147,
  151, 174 (the five tables' first pages, the file's existing convention). All
  133 match their shipped cell exactly — 0 value mismatches. Provenance of the
  63 new values is the fresh-context rendered-page read logged under T2.
- AC3 — VERIFIED. Re-derived from `pid_norms`: 0 of the 66 T-scored columns are
  anchored only on a percentile plateau. The in-suite test passes and was
  inversion-checked at T4 (red when `second_spot` is removed).
- AC4 — VERIFIED. Re-derived over all 2,145 column pairs: 0 indistinguishable
  pairs. The in-suite test passes and was inversion-checked at T4.

**Consistency gate.** Universal cairn-file checks: `cairn_validate` exit 0, all
16 PASS including `coverage complete`; the 20 `dangling id tokens` advisories are
the standing pre-migration references in DESIGN.md/SOURCES.md, unchanged by this
branch. No principle changed, so `cairn_impact` is not run. Toolchain checks per
the `r-package` profile's `consistency-gate` slot: `devtools::document()`
produces no diff; `pkgdown::check_pkgdown()` reports no problems; README.Rmd
untouched; the new `data-raw/select_norm_anchors.R` sits inside the already
`.Rbuildignore`d `data-raw/`, and `check()` reports 0 notes. No NEWS.md entry is
owed — the diff touches only tracking files, `data-raw/` and `tests/`, with no
`R/`, `data/`, `man/` or `NAMESPACE` change, so nothing user-visible changed.

**A note on evidence hygiene.** A first pass at AC2 reported one value mismatch
that did not reproduce. Cause: `data-raw/mutate_norms_check.R` swaps
`data/pid_norms.rda` in place, and the blame-history reviewer was running it
concurrently, so that read saw a deliberately mutated dataset. Re-checked
serially: 0 mismatches, and `data/pid_norms.rda` md5-matches its committed
object with a clean `git status`. The script's own restore-and-hash-check is what
made this recoverable rather than silent.

