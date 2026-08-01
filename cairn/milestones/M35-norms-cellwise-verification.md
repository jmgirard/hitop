# M35: Cell-by-cell verification of the shipped norms against the book

- **Status:** review
- **Priority:** normal
- **Depends on:** M34
- **Driving RR:** —
- **Principles touched:** IP2
- **Branch/PR:** `m35-norms-cellwise-verification`

## Goal

Extend `data-raw/verify_norms_against_book.R` to diff every cell of the shipped
`pid_norms` against its independent extraction of the book's markup, closing the
assembly stage that today only scattered anchors observe.

## Scope

**In:** the script currently diffs the `data-raw/norms_*.csv` transcriptions
against an independent reshaping of the epub's table markup, and stops there —
so `data-raw/norms_pid5.R`'s long-format assembly, which turns those CSVs into
the shipped `pid_norms`, has no exhaustive check behind it. This milestone adds
a third comparison over the loaded `pid_norms`, name-keyed by
`(version, scale, tscore | raw)`, covering all 70 columns including the four
raw-keyed validity ones; it reports missing rows, extra rows, and value
mismatches separately, and `stop()`s on any. Mutation evidence proves the new
comparison catches the displacement and swap classes. The maintainer-run
boundary — the check needs the gitignored shelf epub and so cannot run in CI —
is stated in the script header and in `tests/testthat/test-norms.R`'s block
comment, beside what the in-suite layers do cover.

**Out:** committing the CSVs or the extraction as test fixtures so the
comparison could run in CI → candidate row; it would ship or duplicate ~42 KB
already present in `pid_norms` and needs its own decision. Any change to
`pid_norms`, the CSVs, or the extraction logic → nowhere; a discrepancy this
milestone surfaces is a finding to escalate to the maintainer under IP1/IP2, not
a cell to correct. Retiring any hand-read anchor → nowhere: the anchors read the
rendered page, which this markup-based check cannot, so the two layers stay.

## Acceptance criteria

- [ ] AC1. `Rscript data-raw/verify_norms_against_book.R` compares every cell of
      the shipped `pid_norms` against the extracted markup, keyed by name rather
      than position, and covers all 70 `(version, scale)` columns — asserted by
      the script printing a per-column comparison count that a reader can check
      sums to the dataset's row count, with no column silently skipped.
- [ ] AC2. The comparison reports rows present in `pid_norms` but absent from
      the extraction, rows absent from `pid_norms` but present in the
      extraction, and cells present in both whose `raw` or `percentile` differ,
      as three distinct categories; each is NA-aware, so a value failing to
      parse on either side is reported rather than passing as equal (M33
      lesson).
- [ ] AC3. The script exits non-zero via `stop()` when any of the three
      categories is non-empty, and exits 0 on the unmodified dataset.
- [ ] AC4. Mutation evidence: with each of the four M34 mutation cases applied
      in turn to `pid_norms` — the SF withdrawal and FULL anhedonia
      percentile-column displacements and the two column swaps — the new
      comparison reports the mutation and `stop()`s. Recorded as a runnable
      check, not a one-off transcript.
- [ ] AC5. The script header and the `# ---- spot values from the printed
      tables` comment in `tests/testthat/test-norms.R` state which layer covers
      what: that this check is exhaustive but maintainer-run and markup-based,
      that the hand-read anchors are the only rendered-page layer, and that CI
      therefore sees the anchors and structural invariants only.
- [ ] AC6. `devtools::test()` passes and `devtools::check()` reports 0 errors,
      0 warnings, 0 notes; no package dependency is added (`xml2`/`readr` stay
      maintainer-local, per the M18 lesson).

## Coverage

- AC1 → T1, T2
- AC2 → T2
- AC3 → T2
- AC4 → T3
- AC5 → T4
- AC6 → T4

## Tasks

- [x] T1. Map the extraction's per-table matrices onto `pid_norms`'s long
      format: the version/scale naming, the T-scored vs raw-keyed split, and
      the unattainable printed rows D-027 ships verbatim.
- [x] T2. Add the third comparison to `data-raw/verify_norms_against_book.R` —
      name-keyed, NA-aware, three categories, per-column counts, `stop()` on
      any discrepancy.
- [x] T3. Add a runnable mutation check over the four M34 cases (reuse
      `data-raw/mutate_norms_check.R`'s save/restore-by-hash pattern rather than
      re-inventing it) and confirm each is reported.
- [x] T4. Update the script header and the `test-norms.R` block comment with the
      layer map; run `devtools::test()` and `devtools::check()`.

## Work log

- 2026-07-31: created by /milestone-plan alongside M34; the exhaustive-check half of the same gap, split out at the plan gate at the user's choice rather than folded in.
- 2026-07-31: plan gate chose extending the maintainer script over committing the CSVs as test fixtures because a fixture duplicates ~42 KB already in `pid_norms` and creates a second copy that can drift, at the cost of the check never running in CI; falsified by a norms defect reaching a release through a path where the maintainer never re-ran the script — which would show the CI-blind boundary is the binding one.
- 2026-07-31: `Depends on: M34` is ordering, not necessity — both touch the same two block comments, and M34's rewrite of them lands first so M35 amends one settled text rather than racing it.

- 2026-07-31: T1+T2 — the assembly comparison reads all nine tables into `pid_norms`'s long format and diffs 4,606 rows over all 70 columns, clean; the domain and facet column names come from the tables' own banner rows and the validity ones from a spec whose table index is now checked against each `<caption>`.
- 2026-07-31: question gate — the book-wording crosswalk is written independently of `data-raw/norms_pid5.R` (facets by a case/`&` normalizing rule against `pid_scales$Facet`, domains by a five-entry banner map against `pid_domains`), so a mislabelled column is compared against the right one rather than against itself; and the mutation definitions move to a shared `data-raw/norms_mutations.R` sourced by both harnesses.
- 2026-07-31: T3 — the 13 seeded corruptions move to `data-raw/norms_mutations.R` with stable ids and the shared save/restore-by-hash wrapper; the new `data-raw/mutate_norms_book_check.R` runs all of them against the book comparison and all 13 are CAUGHT, the four M34 cases included, with the restore hash unchanged. `data-raw/mutate_norms_check.R` re-run over the shared list: 0 NOT CAUGHT.
- 2026-07-31: the book harness asserts every seeded corruption, not only AC4's four — an exhaustive cell-by-cell diff that missed any of them would be a finding, so it `stop()`s on an unreported one rather than merely printing it.
- 2026-07-31: T4 — the layer map is stated in the script header and in `test-norms.R`'s block comment, and the comment's now-false claim that the script compares the CSVs rather than `pid_norms` is corrected. `devtools::test()` 11681 pass / 0 fail; `devtools::check()` 0 errors, 0 warnings, 0 notes; DESCRIPTION and NAMESPACE untouched.

## Decisions

## Review
