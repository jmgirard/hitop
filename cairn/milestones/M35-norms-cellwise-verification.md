# M35: Cell-by-cell verification of the shipped norms against the book

- **Status:** review
- **Priority:** normal
- **Depends on:** M34
- **Driving RR:** —
- **Principles touched:** IP2
- **Branch/PR:** `m35-norms-cellwise-verification` — https://github.com/jmgirard/hitop/pull/38

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

- [x] AC1. `Rscript data-raw/verify_norms_against_book.R` compares every cell of
      the shipped `pid_norms` against the extracted markup, keyed by name rather
      than position, and covers all 70 `(version, scale)` columns — asserted by
      the script printing a per-column comparison count that a reader can check
      sums to the dataset's row count, with no column silently skipped.
- [x] AC2. The comparison reports rows present in `pid_norms` but absent from
      the extraction, rows absent from `pid_norms` but present in the
      extraction, and cells present in both whose `raw` or `percentile` differ,
      as three distinct categories; each is NA-aware, so a value failing to
      parse on either side is reported rather than passing as equal (M33
      lesson).
- [x] AC3. The script exits non-zero via `stop()` when any of the three
      categories is non-empty, and exits 0 on the unmodified dataset.
- [x] AC4. Mutation evidence: with each of the four M34 mutation cases applied
      in turn to `pid_norms` — the SF withdrawal and FULL anhedonia
      percentile-column displacements and the two column swaps — the new
      comparison reports the mutation and `stop()`s. Recorded as a runnable
      check, not a one-off transcript.
- [x] AC5. The script header and the `# ---- spot values from the printed
      tables` comment in `tests/testthat/test-norms.R` state which layer covers
      what: that this check is exhaustive but maintainer-run and markup-based,
      that the hand-read anchors are the only rendered-page layer, and that CI
      therefore sees the anchors and structural invariants only.
- [x] AC6. `devtools::test()` passes and `devtools::check()` reports 0 errors,
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

Reviewed 2026-07-31 on `m35-norms-cellwise-verification` at 86b9cfc, PR #38.

### Acceptance-criteria evidence

- AC1. `Rscript data-raw/verify_norms_against_book.R` exits 0 and prints a
  per-column count for 70 `(version, scale)` columns summing to 4,606, which is
  `nrow(pid_norms)` exactly — no column skipped, none double-counted. Column
  identity is read from each table's own banner row (domains, facets) or from a
  spec entry whose table index is checked against that table's `<caption>`
  (validity), never from a position.
- AC2. All three categories fired on demand. Differing values: 12 of the 13
  seeded mutations. Rows in the book only: the anxiousness ceiling-cut mutation,
  reported as 11 book-only rows. Rows in `pid_norms` only: a review probe adding
  a FULL anhedonia row at T = 101 — reported as
  `pid_norms has a row the book does not print -- FULL anhedonia T = 101`.
  NA-awareness: a review probe setting FULL detachment raw at T = 65 to `NA`
  was reported as a differing value (`book raw 1.57 ... pid_norms raw NA`),
  not passed over as equal. Both probes restored the dataset, hash unchanged.
- AC3. Exit status 1 on each of the 15 corrupted datasets exercised (13 seeded
  mutations plus the 2 review probes); exit status 0 on the unmodified dataset.
- AC4. `Rscript data-raw/mutate_norms_book_check.R` is the runnable check: it
  reports every seeded corruption CAUGHT — 13 of 13, including AC4's four named
  M34 cases (SF withdrawal and FULL anhedonia percentile displacements, the two
  column swaps) — and `stop()`s on any unreported one, so it is a check rather
  than a transcript. Restore hash unchanged.
- AC5. Stated in both places. `data-raw/verify_norms_against_book.R`'s header
  carries a "Which layer covers what" block: exhaustive but markup-based and
  maintainer-run, the anchors as the only rendered-page layer, CI seeing the
  anchors and structural invariants only. `tests/testthat/test-norms.R`'s
  `# ---- spot values from the printed tables` comment states the same division,
  and its now-false claim that the script compares the CSVs rather than
  `pid_norms` was corrected.
- AC6. `devtools::test()` 11,681 pass / 0 fail / 1 skip; `devtools::check()`
  0 errors, 0 warnings, 0 notes (4m35s). No dependency added — `DESCRIPTION`
  and `NAMESPACE` are byte-identical to the default branch, and `xml2`/`readr`
  stay maintainer-local.

### Consistency gate

- `cairn_validate` exit 0, all checks pass; the 20 `dangling id tokens`
  advisories are the standing pre-migration references in DESIGN.md/SOURCES.md.
- No IP/GP principle changed, so `cairn_impact` does not apply (the header's
  `Principles touched: IP2` records what the milestone serves, not a change).
- Toolchain (`r-package` profile): `devtools::document()` produces no diff;

  no generated file hand-edited; README.Rmd/README.md in sync; `pkgdown::check_pkgdown()`
  reports no problems; `devtools::check()` clean; no new top-level file, so no
  `.Rbuildignore` entry is owed. No NEWS entry: the milestone changes maintainer
  tooling and comments only, with no user-visible behavior change.

### Independent review

Three fresh-context reviewers with distinct evidence bases, then a Sonnet
scorer that did not generate the findings.

- [O] diff-bug (Opus, the full diff against criteria/DESIGN/DECISIONS): 14
  findings. It separately confirmed on the unmodified dataset that the script
  exits 0 over 4,606 rows and 70 columns; that the name-keying is genuinely
  name-based and independent of `norms_pid5.R`'s maps; that all 13 seeded
  corruptions are caught with the restore hash unchanged; and, by injecting a
  `stop()` into the harness body, that the restore fires on the error path.
- [S] blame-history (Sonnet, `git log`/`blame` on the touched lines): no
  findings. It enumerated all 13 mutations before and after the refactor and
  found the bodies and their `stopifnot()` guards byte-identical modulo the
  added `id`, and found no claim dropped from either rewritten comment.
- [S] prior-review record (Sonnet, archived `## Review` sections; the GitHub
  inline-comment probe returned empty, so no PR-thread walk): 1 finding. It
  separately confirmed the new comparison does not regress M33's NA-blind-compare
  finding.

**Nothing scored at or above the 80 threshold, so the actioned list is empty.**
All 15 findings are logged here, highest first, with the scorer's confidence:

- 72 — the M34 lesson about swapping the tracked `data/pid_norms.rda` in place
  names only `mutate_norms_check.R`; two scripts now share that pattern and the
  new one's window is minutes rather than seconds. Handled: the lesson is
  corrected in place at hygiene (current knowledge, D-045), not in code.
- 70 — a duplicated `(version, scale, key)` in `pid_norms` aborts on the bare
  `stopifnot(!anyDuplicated(...))` before the three categories are computed, so a
  crosswalk defect that relabels a whole block crashes opaquely instead of
  being reported.
- 68 — the NA-awareness is one-sided: an unparseable cell on the *book* side
  aborts in `book_long()`/`facet_book_columns()` rather than reaching `differs()`.
- 65 — `mutate_norms_book_check.R`'s `stopped && reported` test cannot tell a
  crashed verification run from a clean one, so an unrelated crash is reported
  as "the comparison missed it".
- 55 — on the four validity scales `raw` is the match key, so a `raw` mismatch
  surfaces as a paired extra+missing row rather than as a value difference.
- 55 — the subprocess call passes neither `--vanilla` nor a quoted path.
- 45 — no `length(cols) == 25` assertion on the facet side (pre-existing M33
  code; the CSV layer flags the same defect independently).
- 45 — the new harness's working-tree window is ~2 minutes (the plan directed
  reusing the existing pattern rather than redesigning isolation).
- 35 — the per-column coverage is printed for a reader, not asserted; AC1 asks
  only that a reader can check it.
- 35 — `res$lines[[1]]` is unguarded against a future message-format change.
- 30 — the new `<caption>` assertion is a beneficial guard that no seeded
  corruption exercises.
- 25 — `norm_key` is not NA-safe in `raw` (reachable only from a hand-corrupted
  `.rda`).
- 20 — `with_pristine_norms()` discards its `file.copy()` return values
  (pre-existing on the default branch; T3 directed reusing the pattern).
- 20 — the validity key stringifies a double (harmless while every validity raw
  score is a small integer).
- 18 — the per-column listing's sort order is locale-dependent.

The three strongest code findings (70/68/65) are one theme — how the script
*reports* a failure it does detect, on a maintainer-run tool — rather than a
wrong answer on the shipped data, so they graduate to a candidate row rather
than blocking the merge.
