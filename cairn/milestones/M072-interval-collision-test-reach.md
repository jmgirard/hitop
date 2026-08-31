# M072: The HiTOP-BR interval tests and the collision-ordering sweep fail on the defects they claim to catch

- **Status:** review
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** IP2, GP2
- **Branch/PR:** `m072-interval-collision-test-reach` / https://github.com/jmgirard/hitop/pull/78

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

- [x] AC1: `tests/testthat/test-interval_hitopbr.R` carries a `test_that()` block
      calling `interval_hitopbr()` once with at least two score columns whose
      reference means, SDs and reliabilities differ, each column's expected
      `_est`, `_lo` and `_hi` derived from Table 1's printed constants for that
      scale rather than from `hitopbr_devstats`. With `R/interval_engine.R`'s
      `ref <- refstats[hit[[i]], ]` changed to `refstats[hit[[1]], ]`, that block
      reports at least one failure; on the unmutated tree it reports none.
- [x] AC2: `tests/testthat/test-interval_hitopbr.R` checks `hitopbr_devstats`'s
      per-row `scale` <-> `camelCase` pairing against `hitopbr_scales` through a
      helper that returns the names of the rows whose pairing disagrees. A second
      `test_that()` block runs that helper over a copy of `hitopbr_devstats` with
      two rows' `camelCase` values exchanged and asserts it returns both affected
      scale names; run over the shipped table the helper returns `character(0)`.
- [x] AC3: `tests/testthat/test-append-collision.R`'s warnings-before-abort
      coverage includes `score_pid5()`, `score_hitopsr()` and `score_hitopbr()`
      called with `calc_se = TRUE`, each paired with a non-colliding control
      asserting the same call does raise `hitop_deprecated_calc_se`. With
      `R/score_engine.R`'s `deprecate_calc_se()` call moved ahead of the
      append-collision guard, each of the three added cases reports at least one
      failure; on the unmutated tree each reports none.
- [x] AC4: `data-raw/characterize_calc_se.R` captures no conditions — a run of
      `Rscript data-raw/characterize_calc_se.R . <out.rds>` on the current tree
      exits 0 and writes 48 entries, none of which holds a `conditions` element.
- [x] AC5: `Rscript -e 'devtools::test()'` is clean and `Rscript -e
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
- [x] T2: Factor the pairing check into a returning helper beside the existing
      `test_that()` at `test-interval_hitopbr.R:16`, in the shape
      `test-interval_hitopsr.R`'s `join_residue()` family uses, and add the
      exchanged-`camelCase` probe block.
- [x] T3: Extend `test-append-collision.R:345`'s block with the three `score_*()`
      cases and their `calc_se = TRUE` controls. Prove each red under the
      reordered `deprecate_calc_se()` plant and green without.
- [x] T4: Delete `capture_call()`'s condition capture and the `conditions`
      element from `data-raw/characterize_calc_se.R`; rewrite its header and
      usage block to compare values only, and say in the header that the
      condition channel was removed because it recorded nothing on any of the
      matrix's 48 cells. Re-run and confirm 48 value-only entries.
- [x] T5: `devtools::document()` (no diff expected), `devtools::test()`,
      `devtools::check()`.

## Work log

- 2026-08-30: created by /milestone-plan.
- 2026-08-30: plan-gate criteria audit ran in **reduced** mode (internal tier, no RB-tripwire tag) in a fresh-context [O] reader; it returned seven findings, five fixed here and reported in chat, two taken to the user's gate as questions, and the gate-changed AC4 was re-read against the reduced audit's three questions before being written.
- 2026-08-30: the five audit findings fixed at the gate — AC1 and AC3 stated their mutation kill over a whole filtered `devtools::test()` run, so a pre-existing test could supply the failure (narrowed to the added blocks); AC2 pinned its helper's form to `test-interval_hitopsr.R:81-83`, a line-numbered address that drifts (restated behaviorally); AC2's two halves named incompatible shapes, lines 81-83 being inline `expect_identical()` calls that return nothing while the plant test needs a helper naming the offending scales (repaired to the returning-helper shape); AC4 bound the script's header prose, satisfiable by writing a comment (moved to T4); AC4's rationale clause overclaimed sensitivity across the whole condition channel from one signalling cell (narrowed).
- 2026-08-30: plan gate chose hand-listing the three `score_*()` exports in `test-append-collision.R` over driving that block from the file's own `append_exports()` sweep, because the sweep-driven form needs a per-export "does its output path warn" registry and the internal-tier criteria standard warns against exemption registries; falsified by an eighth appending export landing with a warning output path and going uncovered.
- 2026-08-30: T1 — `test-interval_hitopbr.R` gained a two-column block (antagonism r = .82, internalizing r = .90, requested in the order opposite to `data`), expectations from Table 1's printed constants; under `refstats[hit[[1]], ]` that block alone reports 4 failures and the file's other 16 blocks report none, and it is green on the unmutated tree.
- 2026-08-30: T2 — `pairing_residue(devstats, scales)` returns the printed names whose `camelCase` is not the stem `hitopbr_scales` pairs with that name; the shape block asserts `character(0)` and a new block trades two rows' stems and asserts both names come back, after asserting the trade leaves every set comparison over the two columns unchanged. Its code landed in T1's commit — both edits are in the same file and were made before that commit.
- 2026-08-30: plan gate chose deleting `characterize_calc_se.R`'s condition channel over adding a matrix cell that signals, because the scope sits near the checker-regress shape — hardening a maintainer harness M069 already shipped — and the channel's stated promise is false as it stands (verified at the gate: 48 cells, 0 conditions); falsified by a `calc_se` change whose only visible effect is a condition the package stops signalling.
- 2026-08-30: T3 — `test-append-collision.R` gained one block per scoring export (`score_pid5`, `score_hitopsr`, `score_hitopbr`), each asserting a `calc_se = TRUE` colliding call aborts with `hitop_append_collision` naming the collided column and signals no warning, paired with a non-colliding control raising `hitop_deprecated_calc_se`. With `deprecate_calc_se()` moved ahead of the collision guard, those three blocks fail with one failure each and the file's other 13 stay green; all 16 are green unmutated.
- 2026-08-30: T4 — `characterize_calc_se.R`'s `capture_call()` now returns the call's value and muffles the deprecation instead of recording a condition set; header and usage block say the comparison is of values, and why the channel went. The pre-change script run on this tree captured 48 cells, 0 of them with a non-empty condition set; the rewritten script exits 0, writes 48 entries, none holding a `conditions` element, and all 48 values are `identical()` to the pre-change run's.
- 2026-08-30: T5 — `devtools::document()` no diff; `devtools::test()` clean (4 pre-existing skips: the OQ-1 keying question and three merge-base-dependent scale-name blocks); `devtools::check()` Status: OK, 0 errors / 0 warnings / 0 notes, tests 196s.
- 2026-08-30: the first `devtools::check()` run reported 1 ERROR — its test process was SIGTERMed at 304s while three other R jobs of this session were running on the machine. Re-run with nothing else running, the same tree gives Status: OK. No code changed between the two runs.
- 2026-08-30: review — every criterion executed with fresh evidence (recorded in the Review section); consistency gate clean (`cairn_validate` exit 0, `document()` no diff, `check_pkgdown()` clean, `check()` 0/0/0). Three fresh-context reviewers returned eleven findings: four fixed on the branch at this gate, six rejected with reasons, one routed to a Known issues entry; none met the return floor.

## Decisions

## Review

Verified 2026-08-30 on `m072-interval-collision-test-reach` at 61566679 plus the
gate fixes below; PR https://github.com/jmgirard/hitop/pull/78. `main` had not
moved since the branch was cut (`git rev-list --count HEAD..origin/main` = 0),
so no merge was needed before gathering evidence. Every mutation run below was
made in a scratch copy of the tree, never in the checkout, so the fresh-context
reviewers read an unmutated working tree.

**AC1 — met.** `test_that("two scales converted in one call each use their own
reference row")` at `tests/testthat/test-interval_hitopbr.R:526` calls
`interval_hitopbr()` once for `hbr_antagonism` (M 1.42, SD 0.45, r .82) and
`hbr_internalizing` (M 1.85, SD 0.77, r .90), requested in the order opposite to
`data`'s, with every expected `_est`/`_lo`/`_hi` computed from the `br_reference`
Table 1 constants transcribed at `:135`, never from `hitopbr_devstats`. On the
unmutated tree the file's 18 blocks report 0 failures and 0 errors. With
`R/interval_engine.R:124`'s `refstats[hit[[i]], ]` changed to
`refstats[hit[[1]], ]`, that block alone reports 4 failures and the other 17
report none.

**AC2 — met.** `pairing_residue(devstats, scales)` at
`tests/testthat/test-interval_hitopbr.R:17` returns the printed `scale` names
whose `camelCase` is not the stem `hitopbr_scales` pairs with that name. Run
directly against the shipped table it returns `character(0)`; over a copy with
`Detachment` and `Internalizing` trading stems it returns
`c("Detachment", "Internalizing")`, while `setequal()` over the two `camelCase`
columns still reports TRUE — the exchange is invisible to the set comparison the
shape block already made, which is the reach this adds. Both facts are asserted
in the file: the shape block at `:66` asserts `character(0)`, and
`test_that("the pairing checker names both rows of an exchanged stem")` at `:75`
builds the exchange and asserts both names come back.

**AC3 — met, after a gate fix.** `tests/testthat/test-append-collision.R` now
generates one block per scoring export, each asserting that a colliding
`calc_se = TRUE` call aborts with `hitop_append_collision` naming the collided
column and signals no warning. The control was repaired at this gate: it had
passed `append = FALSE` as well as clean `data`, so it ran a different branch
rather than "the same call" the criterion asks for; it now differs from the
colliding call only in that `data` does not collide. On the unmutated tree the
file's 17 blocks report 0 failures and 0 errors. With `R/score_engine.R`'s
`deprecate_calc_se()` call moved from its site inside the `calc_se` block to
ahead of the append-collision guard, exactly the three added blocks fail, one
failure each, and the other 14 stay green.

**AC4 — met.** `Rscript data-raw/characterize_calc_se.R . <out.rds>` run on this
tree exits 0 and prints `configs: 48`. Reading the written RDS back gives
`length(a)` 48, and no element carries a `conditions` member — each is now the
scored tibble the call returned (`class` `tbl_df`).

**AC5 — met.** `Rscript -e 'devtools::test()'`: FAIL 0, WARN 0, SKIP 4, PASS
16191 (the four skips are the pre-existing OQ-1 keying question and three
merge-base-dependent scale-name blocks). `Rscript -e 'devtools::check()'`:
Status OK, 0 errors / 0 warnings / 0 notes, 5m 7s, tests 227s.

### Consistency gate

`cairn_validate.py` exits 0, all 16 checks PASS including `coverage complete`,
`weight caps` and `binding criteria`; 22 advisories, all pre-existing (dangling
legacy D-001..D-012 tokens, and `schmukle2026.md`'s missing extraction status).
`release window` did not fire. No DESIGN.md principle changed, so `cairn_impact`
was not run. Toolchain slot: `devtools::document()` produces no diff and leaves
`NAMESPACE`/`man/` untouched; `pkgdown::check_pkgdown()` reports no problems;
README.md is in sync as committed; no new top-level files; `devtools::check()`
clean as above. No NEWS entry is owed — the diff is tests, one `data-raw/`
maintainer script and tracking files, with no user-visible change.

### Independent review

Three fresh-context reviewers, none having seen the implementation, on distinct
evidence bases: [O] diff-bug, [S] blame-history, [S] prior-PR-comments. Eleven
candidate findings; four actioned as gate fixes, six rejected, one routed to a
Known issues entry. None demonstrated an acceptance criterion failing inside the
domain of the procedure that criterion names, so none met the return floor.

Fixed at the gate:

1. [O] `cairn/ORACLES.md`'s line anchors went stale: O-005 pointed at
   `test-interval_hitopbr.R:133` and O-006 at `:16`, while the diff moved those
   blocks to `:170` and `:29`; the new multi-column block had no record at all,
   though `DESIGN.md` names ORACLES.md as the declared oracle register. Both
   anchors corrected and O-005's provenance extended to name its companion block
   at `:526` and what that block adds.
2. [O] The new `calc_se` sweep had no domain assertion, so an emptied
   `calc_se_cases()` would have generated no blocks and left the file green —
   the very silence the file's own header forbids and its two sibling sweeps
   guard against. A block now derives the domain from the `calc_se` formal over
   `append_exports()` and asserts the table equals it; dropping the
   `score_hitopbr` case turns that block red, block count 17 to 16.
3. [O] The controls were not "the same call without the collision" AC3 asks for:
   they changed clean `data` and `append = FALSE` together, exercising the
   non-append branch rather than the one the colliding call aborted from. The
   append setting now stays as the colliding call made it.
4. [O] The AC1 block's comment claimed the reversed request order made the
   engine's row lookup run on "a selection that is not the column order", but
   `R/interval_engine.R:64`'s `score_cols <- data[scores]` means the engine never
   sees `data`'s order. Comment rewritten to state what the reversal actually
   exercises.

Routed:

5. [S blame] Deleting the condition channel is a real, consciously accepted
   reduction in the M069 characterization harness: a future `score_engine.R`
   change that silently stops signalling a warning is no longer caught there.
   The plan gate rejected making the channel discriminating, so this is a
   limitation to live with rather than a defect — it goes to `DESIGN.md`'s Known
   issues at the hygiene pass.

Rejected:

6. [O] The plan gate's recorded rationale for hand-listing the three exports —
   that a sweep "cannot say which exports warn on their output path" — is
   falsifiable, since the `calc_se` formal is an exact self-maintaining domain.
   The hand-listing itself is what the Scope put out of bounds at the plan gate,
   and fix 2 above adopts the formal-derived domain as the assertion, which
   closes the falsifier the work log recorded without reopening the choice.
7. [O] `results[[key]] <- capture_call(...)` would delete a cell if the call ever
   returned NULL. Hypothetical — the scoring exports always return a tibble — and
   the script's own `configs:` count prints the surviving length, so a dropped
   cell shows.
8. [O] A pre-M072 baseline RDS compared against a new one prints 0 of 48 with no
   diagnostic. The documented workflow runs the current script against both
   checkouts, so it takes a stale file to bite.
9. [O] `pairing_residue()` checks `hitopbr_devstats` against its sibling keying
   table rather than an independent source. That is what AC2 specifies; a
   pairing error shared by both transcriptions is out of this criterion's reach
   by construction.
10. [O] `expect_setequal(named_columns(conditionMessage(got$error)), collide)`
    would throw rather than fail under a mutation that removes the guard
    entirely. Not the mutation AC3 names, and a test error is still red.
11. [O] `calc_se_cases()` restates entries `collision_probes()$extra` carries and
    is re-evaluated per call. Style and cost, no behavior.

[S prior-PR-comments] reported no regression: it traced M068's findings 6 and 7
and M069's findings 3 and 4 — the four filings this milestone was created to
absorb — to the four blocks the diff adds, and the GitHub inline-comment probe
returned empty, as M069's and M070's reviews had already recorded.
