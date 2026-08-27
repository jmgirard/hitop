# M058: The HiTOP-SR's NSSI scale is named in full

- **Status:** blocked
- **Priority:** normal
- **Depends on:** M057
- **Driving RR:** —
- **Principles touched:** IP1, IP2, GP2
- **Branch/PR:** `m058-nssi-scale-name`

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
- 2026-08-27: /milestone-implement opened on branch `m058-nssi-scale-name`. T1 source search, before any keying edit. The Scope gate condition's sourced branch holds: the HiTOP Measures Development Workgroup deck *Measurement Workgroup Presentation December 2023* (Leonard Simms; PDF CreationDate 2023-12-01; sha256 `903e277334924d42286bbf261857ddea539b87e00eff5563784686711614681a`), slide 34, table "Internalizing : Distress Scales", prints the row `Non-suicidal Self-Injury (NSSI)  6  0.83` — plain ASCII hyphens, capital `I` in `Injury`. Served from the same 3P Lab page as `HiTOP-SR-Final.xlsx` (<https://ubwp.buffalo.edu/3plab/wp-content/uploads/sites/251/2026/07/MeasurementWorkgroupPresentationDecember2023.pdf>); copy on the gitignored shelf as `cairn/references/sources/simms2023_hitop_measurement_workgroup.pdf`. Corroboration: 75 of the package's 76 `hitopsr_items$Scale` values appear as verbatim lines in that deck and `NSSI` is the only one that does not; the deck's item count for the scale (6) matches the package's 6 NSSI items. No source disagreement — `HiTOP-SR-Final.xlsx` prints `NSSI` in all five sheets that mention it and no full name anywhere, so the workbook is silent rather than competing, and `hitopsr_definitions.csv`'s `Non-suicidal Self-injury` is in-repo authoring, not a source. Both casings convert to the same stem, so the scored column would be `hsr_nonSuicidalSelfInjury` either way and only the four printed questionnaires differ.
- 2026-08-27: question gate. On T1's `(RB tripwire: ip-touching)` question — adopt the deck's `Non-suicidal Self-Injury` — Jeff chose **Escalate via `/milestone-brief`** over adopting it here and over the definitions CSV's unsourced `Non-suicidal Self-injury`. No keying edit was made; the milestone stops at T1 pending the RR. On the second gate question Jeff chose, for after the escalation returns: align `data-raw/hitopsr_definitions.csv`'s `Scale` cell to whatever name is adopted and delete the then-inert `definition_scale_labels` map and its comment from `data-raw/hitopsr_info.R`, keeping the `stopifnot` stem check as the guard that catches future drift. That choice is banked, not executed.
- 2026-08-27: blocked on RB04 (`cairn/reviews/RB04-nssi-scale-name-authority.md`), which puts to an independent reviewer whether the December 2023 Workgroup deck clears IP1's bar for a HiTOP-SR scale name, which document is primary for that content type under D-018's per-content-type rule, whether the workbook's `NSSI` is a competing name or an absence of one, whether to wait for the introduction paper D-032 ranks above a development workbook (M041's T1 source, not in hand, whose Table 1 is known to carry four labels that do not join the package's scale names), the exact string and the `(NSSI)` parenthetical, whether the derived stem `hsr_nonSuicidalSelfInjury` is proportionate, and — if the rename should not proceed — where IP1's open question is recorded. Second-escalation sweep of `cairn/reviews/` and its archive: RB01 (PID-5 validity-scale naming), RB02 (PID-5 norm lookup rules), RB03 (builder Word numbering control); none names this subject, and the subject is a one-off source adjudication rather than a standing mechanism, so the removal option does not apply.
- 2026-08-27: ingested RR04 (advisory; RB04/RR04 moved to `cairn/reviews/archive/`). Triage of its eight recommendations. **R1 apply** — request the final introduction paper from Len before adopting any string; it is already M041's T1 need, and obtaining it collapses every risk at once (the D-entry would cite the primary source, AC1 would run against the real authority, and no reconciliation debt is created). The milestone stays `blocked`, now on that request rather than on RB04. **R2 apply, conditional** — if the window Jeff sets closes empty, proceed on the deck with `Non-suicidal Self-Injury`, the D-entry carrying a reconciliation commitment: at M041 T1 the adopted string is diffed against the final Table 1, a casing-only difference triggers a DOCX rebuild and a SOURCES amendment without re-litigation, a stem-changing difference reopens the decision. **R3 apply** — derived stem kept, already what Scope says; no change. **R4 apply, deferred to the amendment gate** — AC4 and T5 must be reworded for the fact that only the two DOCX artifacts print scale names, and AC1 for comparing the name span before the `(NSSI)` gloss; acceptance-criterion wording is Substantive, so it goes through `/milestone-implement` step 6 with a fresh-context [O] criteria audit, not through this ingestion. **R5 apply** — `NSSI` survives as an also-known-as doc sentence and in NEWS; the banked gate choice on `hitopsr_definitions.csv` and `definition_scale_labels` is confirmed correct and executes with whichever string is adopted. **R6 consider** — OQ-3 in `cairn/SOURCES.md` plus a `cairn/DESIGN.md` known-issues mirror, held in reserve for the case where neither the paper nor a decision lands. **R7 reject** (the reviewer's own reject, concurred) — adopting the definitions CSV's `Non-suicidal Self-injury` because it happens to match the draft would write an unsourced string into the keying table, the precise thing IP1 forbids. **R8 reject** (likewise) — carrying `(NSSI)` into the `Scale` cell.
- 2026-08-27: RR04's three corrections to RB04 were verified independently before ingestion rather than taken on the reviewer's word. M041 T3 does enumerate the four non-joining Table 1 labels and one is this scale, printed `Non-suicidal Self-injury`; `snakecase::to_any_case("Nonsuicidal Self-Injury", case = "lower_camel")` returns `nonsuicidalSelfInjury`, so RB04's claim that every variant converges on one stem is false; and `inst/extdata/hitopsr_qualtrics.txt` and the REDCap `instrument.csv` return zero matches for `nssi` and for the control name `Aloofness`, while both DOCX files carry exactly one `NSSI` and one `Social Aloofness` — so AC4's promise over four artifacts cannot be met by two of them. No `DECISIONS.md` entry is written by this ingestion: no decision has been taken, and AC6 already requires the sign-off entry to land with the adopted name.

## Decisions

- 2026-08-27 (RR04, advisory — ingested, no keying decision taken): the reviewer's holdings, recorded here because the adoption decision itself is Jeff's and its `DECISIONS.md` entry lands with AC6, when a string is adopted.
  - **Naming authority for a HiTOP-SR scale, per D-018's per-content-type rule: introduction paper > Workgroup deck > development workbook.** The workbook is primary for item-to-scale membership, reverse flags and item text — the content the keying was built from — and never for what a scale is called.
  - **The December 2023 Measurement Workgroup deck clears IP1's bar under the Society-sanction prong, as the weakest admissible form of it**, on a four-link chain: authored by the HiTOP-SR's lead developer and workgroup chair, served by the developing lab's page, linked from the Society's measures page as the measure's introduction slideshow, and describing the final instrument (405 items, 76 scales, 17 subscales, and the NSSI row's 6 items all match the package). Not an APA key, and "cited publication" is not stretched to a slide deck. Its authority is best-available, not final: the introduction paper outranks it the moment that is in hand.
  - **The workbook's `NSSI` is not a competing name, but "silent" was the wrong framing** and the implement-session work log's use of that word is corrected here. The workbook writes `NSSI` in name-position in three sheets where the other scales appear as `Anxious Worry`, `Angry Hostility`, `Social Aloofness`, `Suicidality`. What defeats a competing-name reading is the deck's own `Non-suicidal Self-Injury (NSSI)` gloss from the same author: one authority at two levels of contraction, not two authorities with two names. D-018's decline-and-document branch is therefore not triggered by the workbook.
  - **The only real inter-source discrepancy is casing, and it runs against the deck.** The introduction paper's 2026-03-24 tracked-changes draft prints `Non-suicidal Self-injury` (lowercase `i`) — recorded in M041's T3 as one of four Table 1 labels that do not join. Both hyphenated casings yield the same stem `nonSuicidalSelfInjury`, so a casing-only reconciliation costs a DOCX rebuild and a SOURCES amendment, not a column rename. Only an unhyphenated `Nonsuicidal Self-Injury` would change the stem, to `nonsuicidalSelfInjury`; neither document in hand uses it.
  - **If the rename proceeds from the deck the string is `Non-suicidal Self-Injury`, with `(NSSI)` dropped from the `Scale` cell.** The parenthetical is an abbreviation gloss — no other deck scale row carries one and `NSSI` occurs exactly once in the whole deck — so carrying it in would restyle this one scale against the other 75 on the printed forms and put parentheses into the cell every stem derives from. `NSSI` survives only as an also-known-as documentation sentence (D-018's established pattern) and in NEWS as "formerly `hsr_nssi`".
  - **The stem stays mechanically derived; the name-to-stem exception is rejected.** The length objection is empirically hollow: `hsr_difficultiesReachingOrgasm` (26) and `hsr_sexRelatedSubstanceUse` (22) already exceed `nonSuicidalSelfInjury` (21), verified against `hitopsr_scales`. An exception would break the invariant that `camelCase` names the `itemNumbers`, the scored columns, the definitions join and M057's tooltip join alike, and would move `definition_scale_labels`' special case out of a build script and into shipped code. The abbreviation-column convention (`pid_INC`, `pid_PNA`) is for validity scales; NSSI is a content scale.

## Review
