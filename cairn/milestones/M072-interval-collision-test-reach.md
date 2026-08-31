# M072: The HiTOP-BR interval tests and the collision-ordering sweep fail on the defects they claim to catch

- **Status:** in-progress
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** IP2, GP2
- **Branch/PR:** `m072-interval-collision-test-reach`

## Goal

Close the four test-reach gaps M068's and M069's reviews left open, so each guard
goes red on the defect its comment says it catches.

## Scope

Surface tier: **internal** — every deliverable is a test file or a `data-raw/`
maintainer script, and no external consumer of the repo relies on either.

**In:** (a) a multi-column oracle in `test-interval_hitopbr.R`, closing the
per-column mis-indexing hole the single-column loop leaves at
`R/interval_engine.R:124`; (b) a per-row `scale` <-> `camelCase` pairing check on
`hitopbr_devstats`, which is asserted only as sets today; (c) the three
`score_*()` exports added to `test-append-collision.R`'s warnings-before-abort
coverage, which `R/score_engine.R:123-127`'s own comment claims and no test
exercises; (d) `data-raw/characterize_calc_se.R`'s condition channel deleted —
all 48 cells record zero conditions, so it can catch an added condition but never
a removed one, and the header claims otherwise.

**Out:** driving the warnings-before-abort coverage from `append_exports()`
rather than by name → stays as it is, chosen at this gate. Making the
characterization harness's condition channel discriminating → rejected at this
gate, not deferred. The D-045(d) superseding entry → its candidate row stands;
this milestone appends no `DECISIONS.md` entry. `hitopsr_devstats`'s pairing →
already checked in `test-interval_hitopsr.R`.

## Acceptance criteria

- [ ] AC1: `tests/testthat/test-interval_hitopbr.R` carries a `test_that()` block
      calling `interval_hitopbr()` once with at least two score columns whose
      reference means, SDs and reliabilities differ, each column's expected
      `_est`, `_lo` and `_hi` derived from Table 1's printed constants for that
      scale rather than from `hitopbr_devstats`. With `R/interval_engine.R`'s
      `ref <- refstats[hit[[i]], ]` changed to `refstats[hit[[1]], ]`, that block
      reports at least one failure; on the unmutated tree it reports none.
- [ ] AC2: `tests/testthat/test-interval_hitopbr.R` checks `hitopbr_devstats`'s
      per-row `scale` <-> `camelCase` pairing against `hitopbr_scales` through a
      helper that returns the names of the rows whose pairing disagrees. A second
      `test_that()` block runs that helper over a copy of `hitopbr_devstats` with
      two rows' `camelCase` values exchanged and asserts it returns both affected
      scale names; run over the shipped table the helper returns `character(0)`.
- [ ] AC3: `tests/testthat/test-append-collision.R`'s warnings-before-abort
      coverage includes `score_pid5()`, `score_hitopsr()` and `score_hitopbr()`
      called with `calc_se = TRUE`, each paired with a non-colliding control
      asserting the same call does raise `hitop_deprecated_calc_se`. With
      `R/score_engine.R`'s `deprecate_calc_se()` call moved ahead of the
      append-collision guard, each of the three added cases reports at least one
      failure; on the unmutated tree each reports none.
- [ ] AC4: `data-raw/characterize_calc_se.R` captures no conditions — a run of
      `Rscript data-raw/characterize_calc_se.R . <out.rds>` on the current tree
      exits 0 and writes 48 entries, none of which holds a `conditions` element.
- [ ] AC5: `Rscript -e 'devtools::test()'` is clean and `Rscript -e
      'devtools::check()'` reports 0 errors and 0 warnings (NOTEs justified).

## Coverage

- AC1 → T1
- AC2 → T2
- AC3 → T3
- AC4 → T4
- AC5 → T5

## Tasks

- [x] T1: Add the multi-column oracle to `test-interval_hitopbr.R`, reusing the
      `br_reference` constants already transcribed there (two scales whose `r`
      differs most, so the halves differ visibly). Prove it red under the
      `hit[[1]]` mutation and green without, recording both runs.
- [ ] T2: Factor the pairing check into a returning helper beside the existing
      `test_that()` at `test-interval_hitopbr.R:16`, in the shape
      `test-interval_hitopsr.R`'s `join_residue()` family uses, and add the
      exchanged-`camelCase` probe block.
- [ ] T3: Extend `test-append-collision.R:345`'s block with the three `score_*()`
      cases and their `calc_se = TRUE` controls. Prove each red under the
      reordered `deprecate_calc_se()` plant and green without.
- [ ] T4: Delete `capture_call()`'s condition capture and the `conditions`
      element from `data-raw/characterize_calc_se.R`; rewrite its header and
      usage block to compare values only, and say in the header that the
      condition channel was removed because it recorded nothing on any of the
      matrix's 48 cells. Re-run and confirm 48 value-only entries.
- [ ] T5: `devtools::document()` (no diff expected), `devtools::test()`,
      `devtools::check()`.

## Work log

- 2026-08-30: created by /milestone-plan.
- 2026-08-30: plan-gate criteria audit ran in **reduced** mode (internal tier, no RB-tripwire tag) in a fresh-context [O] reader; it returned seven findings, five fixed here and reported in chat, two taken to the user's gate as questions, and the gate-changed AC4 was re-read against the reduced audit's three questions before being written.
- 2026-08-30: the five audit findings fixed at the gate — AC1 and AC3 stated their mutation kill over a whole filtered `devtools::test()` run, so a pre-existing test could supply the failure (narrowed to the added blocks); AC2 pinned its helper's form to `test-interval_hitopsr.R:81-83`, a line-numbered address that drifts (restated behaviorally); AC2's two halves named incompatible shapes, lines 81-83 being inline `expect_identical()` calls that return nothing while the plant test needs a helper naming the offending scales (repaired to the returning-helper shape); AC4 bound the script's header prose, satisfiable by writing a comment (moved to T4); AC4's rationale clause overclaimed sensitivity across the whole condition channel from one signalling cell (narrowed).
- 2026-08-30: plan gate chose hand-listing the three `score_*()` exports in `test-append-collision.R` over driving that block from the file's own `append_exports()` sweep, because the sweep-driven form needs a per-export "does its output path warn" registry and the internal-tier criteria standard warns against exemption registries; falsified by an eighth appending export landing with a warning output path and going uncovered.
- 2026-08-30: T1 — `test-interval_hitopbr.R` gained a two-column block (antagonism r = .82, internalizing r = .90, requested in the order opposite to `data`), expectations from Table 1's printed constants; under `refstats[hit[[1]], ]` that block alone reports 4 failures and the file's other 16 blocks report none, and it is green on the unmutated tree.
- 2026-08-30: plan gate chose deleting `characterize_calc_se.R`'s condition channel over adding a matrix cell that signals, because the scope sits near the checker-regress shape — hardening a maintainer harness M069 already shipped — and the channel's stated promise is false as it stands (verified at the gate: 48 cells, 0 conditions); falsified by a `calc_se` change whose only visible effect is a condition the package stops signalling.

## Decisions

## Review
