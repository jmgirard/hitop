# M058: The HiTOP-SR's NSSI scale is named in full

- **Status:** planned
- **Priority:** normal
- **Depends on:** M057
- **Driving RR:** —
- **Principles touched:** IP1, IP2, GP2
- **Branch/PR:** —

## Goal

The HiTOP-SR scale the package abbreviates `NSSI` carries its full name wherever the
package prints or returns it, so the picker, the forms and the scored output agree.

## Scope

Surface tier: **user-facing** — the deliverable renames a scored output column, a
scale name printed on four distributed questionnaires, and the keying tables those
derive from.

**In:** The rename in the keying source `data-raw/hitopsr_items.csv`, sourced against
a cited authority (RB tripwire: ip-touching); the regenerated `hitopsr_items`,
`hitopsr_scales`, `hitopsr_subscales` and `hitopsr_definitions`; the derived
`camelCase` stem and therefore the scored output column `hsr_nssi` and its `_se`
sibling; the four rebuilt `inst/extdata/hitopsr_*` artifacts, their
`pkgdown/assets/downloads/` copies and `hitop_artifacts` rows; NEWS, a `DECISIONS.md`
entry, and every doc surface the AC2 sweep reaches.

**Gate condition, not an acceptance criterion:** if no citable source printing the
scale's full name can be obtained — the Society's published HiTOP-SR form and the
measure's publication are the documents to check, and `hitopsr_definitions`'s own
spelling is not one of them, having been authored in-repo — this milestone does not
proceed. It returns to the plan gate, and the `NSSI` / `Non-suicidal Self-injury`
discrepancy is instead recorded as a visible open question, which is what IP1
requires of a discrepancy that cannot be sourced. The criteria below all assume the
sourced branch.

**Out:**
- A `lifecycle` deprecation shim for the old scored column name — the package is
  pre-1.0 and GP2 lets a signature break with NEWS → candidate row if a user is
  caught by it.
- Renaming any other scale, on this or any instrument → candidate row.
- M057's tooltip work, which joins on the stem and is correct either way.

## Acceptance criteria

- [ ] AC1 The adopted name is sourced, not chosen: `cairn/SOURCES.md` gains a line
      naming the HiTOP-SR source that prints the scale's full name and where in it,
      and the string written into `data-raw/hitopsr_items.csv` is compared character
      for character against that source document read at verification time — never
      against a transcription kept in this file, which would compare one author's
      transcription against itself.
- [ ] AC2 The rename reaches every place the old name lived and moves nothing else.
      No stale spelling survives: a check enumerates every dataset the package
      exports programmatically rather than by hand-list, sweeps every character
      column of each, and greps the working tree, asserting no case-insensitive match
      for `nssi` outside an allow-list written into the check itself (`NEWS.md`,
      `cairn/`, where the old name is history). And the four keying tables are
      compared against the same objects loaded from a `git worktree` of the
      merge-base of this branch and the default branch, asserted identical except the
      cells that held the old name. The base-commit reference serves invariance
      claims only; it never certifies a value correct.
- [ ] AC3 Scored output renames once and nothing else moves:
      `score_hitopsr(sim_hitopsr, calc_se = TRUE)` is compared against the
      merge-base build's result over the whole returned tibble and asserted equal
      after renaming the two affected columns. The expected new column names are
      written literally in the test, never re-derived by `snakecase::to_any_case()`,
      which is the function the deliverable itself used.
- [ ] AC4 Each of the four rebuilt HiTOP-SR artifacts carries the new name, read back
      by parsing the file: both DOCX variants, the Qualtrics `.txt`, and the REDCap
      `.zip`'s data dictionary. Each file's `pkgdown/assets/downloads/` copy is
      byte-identical to the freshly built `inst/extdata/` file. Text extracted from
      each rebuilt DOCX differs from the merge-base build's only in the scale-name
      lines and the build-date stamp. Re-locking `test-artifacts.R` to fresh
      checksums is a required step (T5), not evidence — its expected values are
      regenerated from the files just built.
- [ ] AC5 No other distributed file moves: every file under `inst/extdata/` and
      `pkgdown/assets/downloads/`, enumerated by listing both directories rather than
      by the manifest, is byte-identical to the merge-base outside the four rebuilt
      files and their copies. `hitop_artifacts`'s set of files, and its rows for every
      file but those four, are unchanged from the merge-base — without which a
      manifest that lost rows would shrink the sweep and pass vacuously.
- [ ] AC6 The sign-off is recorded before the keying edit lands: `cairn/DECISIONS.md`
      carries an entry citing the source line AC1 adds, naming the adopted name and
      the scored-column rename, committed no later than the commit that edits
      `data-raw/hitopsr_items.csv`. NEWS records the rename as a breaking
      column-name change.
- [ ] AC7 `devtools::document()` no diff; `devtools::test()` clean with zero skips in
      `test-artifacts.R` and the HiTOP-SR keying tests, since those skip themselves
      when Suggests are absent and are what AC4 leans on; `devtools::check()` 0
      errors, 0 warnings, every remaining NOTE listed in this file with a reason.

## Coverage

- AC1 → T1
- AC2 → T2, T3
- AC3 → T2, T4
- AC4 → T5, T6
- AC5 → T7
- AC6 → T1, T8
- AC7 → T9

## Tasks

- [ ] T1 Obtain the citable source, add its `cairn/SOURCES.md` line, and land the
      `DECISIONS.md` sign-off entry — all before any keying edit (RB tripwire:
      ip-touching; escalation is on the table here if the sources disagree).
- [ ] T2 Write the AC2 sweep, the AC2 merge-base invariance check and the AC3 scored
      comparison first; confirm the sweep red against today's tables.
- [ ] T3 Rename in `data-raw/hitopsr_items.csv` and regenerate the four tables via
      `data-raw/hitopsr_info.R`; confirm the sweep and invariance check green.
- [ ] T4 Run the AC3 comparison against a merge-base worktree build.
- [ ] T5 Regenerate the four artifacts, their `pkgdown/assets/downloads/` copies and
      the `hitop_artifacts` rows via `data-raw/artifacts.R`; re-lock the checksums.
- [ ] T6 The AC4 parse-backs for all four formats and the bounded DOCX text diff.
- [ ] T7 The AC5 directory-listing sweep and the manifest file-set anchor.
- [ ] T8 NEWS; fix every doc surface the AC2 sweep flags (`?score_hitopsr`,
      `R/data.R`, `README.Rmd`, `_pkgdown.yml`, the vignettes); `build_readme()`.
- [ ] T9 `document()`/`test()`/`check()`; open the PR.

## Work log

- 2026-08-26: created by /milestone-plan, from the M057 plan gate's choice to rename separately rather than inside the tooltip milestone.
- 2026-08-26: criteria audit ran in full mode (user-facing tier), fresh-context [O] reader, on a seven-criterion draft. Findings on all seven; all had one clear right answer and were fixed before this file was written. The load-bearing one: the draft put a "stop if no source is obtainable" branch inside AC1, so a milestone that never happened would have passed AC1 while AC6 became unsatisfiable and five others unevaluable — the branch is now a Scope gate condition carrying IP1's open-question obligation. Also fixed: AC1 compared the CSV against this file's own transcription of the source (one author checking themself); the stale-spelling sweep was a hand-list of four tables and missed `R/sysdata.rda`, `man/`, `README.Rmd` and the lowercase stem; the invariance check listed three columns and left item text free; "the base commit" named no ref or loading mechanism; the scored comparison pinned one column where GP2 governs the whole tibble and omitted `calc_se`; the artifact parse-back named two formats of four and left the REDCap dictionary unverified; the MD5 re-lock was stated as verification when its expected values are regenerated from the files just built; and `devtools::test()` clean was vacuous because `test-artifacts.R` skips itself without officer. No finding went to the gate.
- 2026-08-26: plan gate chose to rename separately from M057 over doing it inside that milestone and over shortening the definitions row to `NSSI`, because the rename touches keying content, renames a scored column for existing users and regenerates four checksum-locked artifacts, none of which belongs in a UI change; falsified by the source search returning nothing citable, which sends this back to the gate as an open question and leaves M057's stem join standing on its own.

## Decisions

## Review
