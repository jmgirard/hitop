# M058: The HiTOP-SR's NSSI scale is named in full

- **Status:** in-progress
- **Priority:** normal
- **Depends on:** M057
- **Driving RR:** —
- **Principles touched:** IP1, IP2, GP2
- **Branch/PR:** `m058-nssi-scale-name` / https://github.com/jmgirard/hitop/pull/64

## Goal

The HiTOP-SR scale the package abbreviates `NSSI` carries its full name wherever the
package prints or returns it, so the picker, the forms and the scored output agree.

## Scope

Surface tier: **user-facing** — the deliverable renames a scored output column, a
scale name printed on the two distributed Word questionnaires, and the keying tables
those derive from.

**In:** The rename in the keying source `data-raw/hitopsr_items.csv`, sourced against
a cited authority (RB tripwire: ip-touching); the regenerated `hitopsr_items`,
`hitopsr_scales`, `hitopsr_subscales` and `hitopsr_definitions`; the derived
`camelCase` stem and therefore the scored output column `hsr_nssi` and its `_se`
sibling; the two rebuilt `inst/extdata/hitopsr_*.docx` artifacts, their
`pkgdown/assets/downloads/` copies and `hitop_artifacts` rows; NEWS, a `DECISIONS.md`
entry, and every doc surface the AC2 sweep reaches.

**Gate condition, discharged 2026-08-27:** this milestone proceeds only against a
citable source printing the scale's full name. The source obtained is the HiTOP-SR
introduction paper (*Assessment* submission `ASMNT-26-0390`, shelved as
`cairn/references/sources/ASMNT-26-0390_Proof_hi.pdf`), whose Tables 1-3 print
`Non-suicidal Self-injury`. Jeff accepted it on 2026-08-27 as the source while it is
still under peer review, undertaking to re-shelve the accepted version once it
exists; the AC6 `DECISIONS.md` entry carries that reconciliation commitment and the
D-032 reasoning. `hitopsr_definitions`'s spelling is no part of the sourcing, and for
a sharper reason than this file first gave: those definitions are Jeff's own text,
supplied to the paper's authors and published as its Appendix A, so the appendix
descends from this package. Tables 1-3 predate that supply and stand independent of
it. Had no source been obtainable, this milestone would have returned to the plan
gate with the discrepancy recorded as a visible open question, which is what IP1
requires.

**Out:**
- A `lifecycle` deprecation shim for the old scored column name — the package is
  pre-1.0 and GP2 lets a signature break with NEWS → candidate row if a user is
  caught by it.
- Renaming any other scale, on this or any instrument → candidate row.
- M057's tooltip work, which joins on the stem and is correct either way.

## Acceptance criteria

- [x] AC1 The adopted name is sourced, not chosen. Which source, and why it clears
      D-032's draft bar while under peer review and outranks the December 2023
      Workgroup deck under D-018, are settled in the AC6 `DECISIONS.md` entry, not
      here. The string in `data-raw/hitopsr_items.csv` is compared character for
      character against the whole cell in Table 1 of
      `cairn/references/sources/ASMNT-26-0390_Proof_hi.pdf` (sha256 `1c211219…4681a`),
      read at verification time on a machine carrying that gitignored shelf — never
      against any transcription held in this repo. Table 1's cell carries the bare
      name with no `(NSSI)` gloss; a cell that carries a parenthetical stops the
      milestone and returns it to the question gate rather than being stripped.
      `cairn/SOURCES.md` gains a numbered `OQ-n` in OQ-1's form recording the
      source's disagreement with itself and with the deck, its per-variant occurrence
      counts produced by a re-runnable search of the PDF rather than copied from this
      file, and — attributed to the maintainer, dated — that Appendix A is
      `hitopsr_definitions`'s own text supplied after Tables 1-3 existed, so those
      tables stand independent of this package and the appendix does not.
      Instrument: `data-raw/verify_hitopsr_scale_name.R`.
- [x] AC2 The rename reaches every place the old name lived and moves nothing else.
      A check enumerates the package's exported datasets programmatically, sweeps
      every character leaf including a list's own names, and sweeps every tracked
      file (`git ls-files`) — opening each format in its own terms, never as bytes,
      since `.rda` is gzip and `.docx` a zip and a byte grep passes either whatever
      it contains — asserting no case-insensitive `nssi` outside an allow-list
      written into the check and no other path: `NEWS.md` and `cairn/`, where the old
      name is history; the two files whose job is to name the forbidden spelling
      (`data-raw/verify_hitopsr_*.R`, `tests/testthat/test-scale-name-hitopsr.R`);
      and `R/data.R` with its generated `man/hitopsr_items.Rd`, which carry the
      also-known-as note D-041 keeps for a name the literature will go on using.
      The four keying tables are compared against the same objects from a
      `git worktree` of the merge-base, rows matched by identity, identical except
      cells matching the renamed name in either spelling — blanked on both sides,
      since the definitions table already carried the new spelling and blanking each
      side by its own would report an artifact. The renamed row moves
      (`dplyr::arrange(Scale)` sorts in the C locale); the move is reported, not
      failed, but two things are asserted: every other row keeps its relative order,
      checked by dropping the renamed row from each side and requiring the key
      sequences identical element for element — key-matching absorbs any permutation,
      so the comparison alone passes a genuine reordering, verified by planting one —
      and the new position equals where the adopted name sorts under
      `sort(method = "radix")` over the merge-base's names with the old one replaced,
      recomputed rather than read off `arrange()`, which is the thing under test.
      The base-commit reference serves invariance only; it never certifies a value.
- [x] AC3 Scored output renames once and nothing else moves:
      `score_hitopsr(sim_hitopsr, items = 1:405, calc_se = TRUE)` against the
      merge-base build over the whole tibble, after renaming the two affected
      columns: the column set equal and every column identical in value. The renamed
      column's position moves with `hitopsr_scales`' order — which AC2 pins, so no
      second oracle here — and is reported, not failed, while any reordering of the
      others fails. Its from and to positions are recorded in NEWS beside the rename,
      positional selection being a break GP2 requires be visible. Expected new column
      names are written literally, never re-derived by `snakecase::to_any_case()`,
      the function the deliverable itself used.
- [x] AC4 Exactly two files are rebuilt: `data-raw/artifacts.R` with
      `rebuild_stems <- "hitopsr"` and `rebuild_formats <- "docx"`, producing
      `hitopsr_US.docx` and `hitopsr_A4.docx`. The REDCap zip is deliberately not
      rebuilt — `zip::zip` stores the intermediate CSV's mtime, so it is not
      byte-reproducible and a rebuild would churn a checksum and record a D-016
      revision that is not one; the Qualtrics `.txt` is reproducible but prints no
      scale name. All four artifacts are then read back by parsing: the two DOCX
      carry the new name and no `NSSI`; `hitopsr_qualtrics.txt` and the REDCap
      `instrument.csv` carry neither, which is what makes their unchanged bytes a
      result rather than an oversight. Each rebuilt DOCX's
      `pkgdown/assets/downloads/` copy is byte-identical to the file just built.
      Text via `officer::docx_summary()`, plus the footer read separately, differs
      from the merge-base build only at the scale-name row — in place or moved, the
      scoring table sorting on `Scale` (`R/generate_docx.R:491`) under the running
      locale — and in the footer's `Generated` date, which must have changed.
- [x] AC5 No other distributed file moves: the listings of `inst/extdata/` and
      `pkgdown/assets/downloads/`, enumerated by listing both directories rather than
      by the manifest, are identical to the merge-base's — nothing added, renamed or
      removed — and every file in them is byte-identical to the merge-base outside
      the two rebuilt DOCX and their copies, the Qualtrics and REDCap files included,
      since a byte change in either would mean the rebuild moved something
      unintended. `hitop_artifacts` gains exactly two rows, one per rebuilt DOCX,
      each with the rebuild's build date and a `changes` note naming the rename, and
      every pre-existing row is unchanged — without which a manifest that lost rows
      would shrink the sweep and pass vacuously, or a rebuild land with no version
      bump against D-016.
- [x] AC6 The sign-off is recorded before the keying edit lands: `cairn/DECISIONS.md`
      carries an entry citing the source line AC1 adds, naming the adopted name and
      the scored-column rename, committed no later than the commit that edits
      `data-raw/hitopsr_items.csv`. NEWS records the rename as a breaking
      column-name change.
- [x] AC7 `devtools::document()` no diff; `devtools::test()` clean with zero skips in
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

- [x] T1 Obtain the citable source, add its `cairn/SOURCES.md` line, and land the
      `DECISIONS.md` sign-off entry — all before any keying edit (RB tripwire:
      ip-touching; escalation is on the table here if the sources disagree).
- [x] T2 Write the AC2 sweep, the AC2 merge-base invariance check and the AC3 scored
      comparison first; confirm the sweep red against today's tables.
- [x] T3 Rename in `data-raw/hitopsr_items.csv` and regenerate the four tables via
      `data-raw/hitopsr_info.R`; confirm the sweep and invariance check green.
- [x] T4 Run the AC3 comparison against a merge-base worktree build.
- [x] T5 Rebuild the two DOCX artifacts and their `pkgdown/assets/downloads/` copies
      via `data-raw/artifacts.R` with `rebuild_stems <- "hitopsr"` and
      `rebuild_formats <- "docx"`; confirm it appended exactly two `hitop_artifacts`
      rows and left the REDCap and Qualtrics files untouched.
- [x] T6 The AC4 parse-backs for all four formats — the new name and no `NSSI` in
      both DOCX, neither name in the Qualtrics `.txt` and the REDCap dictionary —
      and the bounded DOCX text-plus-footer diff.
- [x] T7 The AC5 directory-listing sweep and the manifest file-set anchor.
- [x] T8 NEWS; fix every doc surface the AC2 sweep flags (`?score_hitopsr`,
      `R/data.R`, `README.Rmd`, `_pkgdown.yml`, the vignettes); `build_readme()`.
- [x] T9 `document()`/`test()`/`check()`; open the PR.

## Work log

- 2026-08-26: created by /milestone-plan, from the M057 plan gate's choice to rename separately rather than inside the tooltip milestone.
- 2026-08-26: criteria audit ran in full mode (user-facing tier), fresh-context [O] reader, on a seven-criterion draft. Findings on all seven; all had one clear right answer and were fixed before this file was written. The load-bearing one: the draft put a "stop if no source is obtainable" branch inside AC1, so a milestone that never happened would have passed AC1 while AC6 became unsatisfiable and five others unevaluable — the branch is now a Scope gate condition carrying IP1's open-question obligation. Also fixed: AC1 compared the CSV against this file's own transcription of the source (one author checking themself); the stale-spelling sweep was a hand-list of four tables and missed `R/sysdata.rda`, `man/`, `README.Rmd` and the lowercase stem; the invariance check listed three columns and left item text free; "the base commit" named no ref or loading mechanism; the scored comparison pinned one column where GP2 governs the whole tibble and omitted `calc_se`; the artifact parse-back named two formats of four and left the REDCap dictionary unverified; the MD5 re-lock was stated as verification when its expected values are regenerated from the files just built; and `devtools::test()` clean was vacuous because `test-artifacts.R` skips itself without officer. No finding went to the gate.
- 2026-08-26: plan gate chose to rename separately from M057 over doing it inside that milestone and over shortening the definitions row to `NSSI`, because the rename touches keying content, renames a scored column for existing users and regenerates four checksum-locked artifacts, none of which belongs in a UI change; falsified by the source search returning nothing citable, which sends this back to the gate as an open question and leaves M057's stem join standing on its own.
- 2026-08-27: /milestone-implement opened on branch `m058-nssi-scale-name`. T1 source search, before any keying edit. The Scope gate condition's sourced branch holds: the HiTOP Measures Development Workgroup deck *Measurement Workgroup Presentation December 2023* (Leonard Simms; PDF CreationDate 2023-12-01; sha256 `903e277334924d42286bbf261857ddea539b87e00eff5563784686711614681a`), slide 34, table "Internalizing : Distress Scales", prints the row `Non-suicidal Self-Injury (NSSI)  6  0.83` — plain ASCII hyphens, capital `I` in `Injury`. Served from the same 3P Lab page as `HiTOP-SR-Final.xlsx` (<https://ubwp.buffalo.edu/3plab/wp-content/uploads/sites/251/2026/07/MeasurementWorkgroupPresentationDecember2023.pdf>); copy on the gitignored shelf as `cairn/references/sources/simms2023_hitop_measurement_workgroup.pdf`. Corroboration: 75 of the package's 76 `hitopsr_items$Scale` values appear as verbatim lines in that deck and `NSSI` is the only one that does not; the deck's item count for the scale (6) matches the package's 6 NSSI items. No source disagreement — `HiTOP-SR-Final.xlsx` prints `NSSI` in all five sheets that mention it and no full name anywhere, so the workbook is silent rather than competing, and `hitopsr_definitions.csv`'s `Non-suicidal Self-injury` is in-repo authoring, not a source. Both casings convert to the same stem, so the scored column would be `hsr_nonSuicidalSelfInjury` either way and only the four printed questionnaires differ.
- 2026-08-27: question gate. On T1's `(RB tripwire: ip-touching)` question — adopt the deck's `Non-suicidal Self-Injury` — Jeff chose **Escalate via `/milestone-brief`** over adopting it here and over the definitions CSV's unsourced `Non-suicidal Self-injury`. No keying edit was made; the milestone stops at T1 pending the RR. On the second gate question Jeff chose, for after the escalation returns: align `data-raw/hitopsr_definitions.csv`'s `Scale` cell to whatever name is adopted and delete the then-inert `definition_scale_labels` map and its comment from `data-raw/hitopsr_info.R`, keeping the `stopifnot` stem check as the guard that catches future drift. That choice is banked, not executed.
- 2026-08-27: blocked on RB04 (`cairn/reviews/RB04-nssi-scale-name-authority.md`), which puts to an independent reviewer whether the December 2023 Workgroup deck clears IP1's bar for a HiTOP-SR scale name, which document is primary for that content type under D-018's per-content-type rule, whether the workbook's `NSSI` is a competing name or an absence of one, whether to wait for the introduction paper D-032 ranks above a development workbook (M041's T1 source, not in hand, whose Table 1 is known to carry four labels that do not join the package's scale names), the exact string and the `(NSSI)` parenthetical, whether the derived stem `hsr_nonSuicidalSelfInjury` is proportionate, and — if the rename should not proceed — where IP1's open question is recorded. Second-escalation sweep of `cairn/reviews/` and its archive: RB01 (PID-5 validity-scale naming), RB02 (PID-5 norm lookup rules), RB03 (builder Word numbering control); none names this subject, and the subject is a one-off source adjudication rather than a standing mechanism, so the removal option does not apply.
- 2026-08-27: ingested RR04 (advisory; RB04/RR04 moved to `cairn/reviews/archive/`). Triage of its eight recommendations. **R1 apply** — request the final introduction paper from Len before adopting any string; it is already M041's T1 need, and obtaining it collapses every risk at once (the D-entry would cite the primary source, AC1 would run against the real authority, and no reconciliation debt is created). The milestone stays `blocked`, now on that request rather than on RB04. **R2 apply, conditional** — if the window Jeff sets closes empty, proceed on the deck with `Non-suicidal Self-Injury`, the D-entry carrying a reconciliation commitment: at M041 T1 the adopted string is diffed against the final Table 1, a casing-only difference triggers a DOCX rebuild and a SOURCES amendment without re-litigation, a stem-changing difference reopens the decision. **R3 apply** — derived stem kept, already what Scope says; no change. **R4 apply, deferred to the amendment gate** — AC4 and T5 must be reworded for the fact that only the two DOCX artifacts print scale names, and AC1 for comparing the name span before the `(NSSI)` gloss; acceptance-criterion wording is Substantive, so it goes through `/milestone-implement` step 6 with a fresh-context [O] criteria audit, not through this ingestion. **R5 apply** — `NSSI` survives as an also-known-as doc sentence and in NEWS; the banked gate choice on `hitopsr_definitions.csv` and `definition_scale_labels` is confirmed correct and executes with whichever string is adopted. **R6 consider** — OQ-3 in `cairn/SOURCES.md` plus a `cairn/DESIGN.md` known-issues mirror, held in reserve for the case where neither the paper nor a decision lands. **R7 reject** (the reviewer's own reject, concurred) — adopting the definitions CSV's `Non-suicidal Self-injury` because it happens to match the draft would write an unsourced string into the keying table, the precise thing IP1 forbids. **R8 reject** (likewise) — carrying `(NSSI)` into the `Scale` cell.
- 2026-08-27: RR04's three corrections to RB04 were verified independently before ingestion rather than taken on the reviewer's word. M041 T3 does enumerate the four non-joining Table 1 labels and one is this scale, printed `Non-suicidal Self-injury`; `snakecase::to_any_case("Nonsuicidal Self-Injury", case = "lower_camel")` returns `nonsuicidalSelfInjury`, so RB04's claim that every variant converges on one stem is false; and `inst/extdata/hitopsr_qualtrics.txt` and the REDCap `instrument.csv` return zero matches for `nssi` and for the control name `Aloofness`, while both DOCX files carry exactly one `NSSI` and one `Social Aloofness` — so AC4's promise over four artifacts cannot be met by two of them. No `DECISIONS.md` entry is written by this ingestion: no decision has been taken, and AC6 already requires the sign-off entry to land with the adopted name.
- 2026-08-27: Jeff shelved `cairn/references/sources/Prolific data HiTOP-SR.sav` (7.8 MB, SPSS, the Prolific dataset the introduction paper is based on). Checked against the naming question and it does not answer it, so the block stands. The file carries 1,362 variables, 1,265 of them labelled; the labels are item text and survey metadata. All 93 scale and subscale score variables are among the 97 *unlabelled* ones, and every one of them is a compressed working abbreviation rather than a name — `SocAloofness`, `DiffReachingOrgasm`, `AffLability`, `RestrictedAffect`, `Anehdonia` (sic) — with `NSSI` sitting in that set as one abbreviation among 93. Of the package's 76 full scale names, three (`Gambling`, `Nightmares`, `Restlessness`) occur inside some variable label, all as incidental words in item text; no label anywhere contains `injury` or `suicidal` except a consent paragraph and the item "I exercised when I was injured." So the dataset asserts nothing about what the scale is called, and unlike the development workbook it does not even use full names for the other scales. It also does not substitute for the paper on M041: D-032 makes the paper's published Table 1 the ingestion source, and computing alpha, mean and SD ourselves from the raw responses would be a re-derivation, not a published citable table (IP3). It would serve as an independent check on Table 1's cells once the paper is in hand.
- 2026-08-27: Jeff shelved `cairn/references/sources/ASMNT-26-0390_Proof_hi.pdf` (5.7 MB; sha256 `1c211219b7fe13f8ed172f9210c152a642a9be77d790e08d795843c25da8e425`; PDF CreationDate 2026-06-03), downloaded from Manuscript Central. RB04's blocker is discharged: this is the introduction paper. **What the file is**, stated because it bears on D-032: the ScholarOne submission PDF of *Assessment of the Hierarchical Taxonomy Of Psychopathology (HiTOP): Introducing the HiTOP Self-Report (HiTOP-SR) and Brief Report (HiTOP-BR)*, journal *Assessment*, Manuscript ID `ASMNT-26-0390` with no revision suffix, 1,107 pages, carrying the rotated "For Peer Review" watermark on essentially every page. It is a manuscript under review, not a publisher's typeset proof and not an accepted or in-press version — so whether it satisfies D-032's "nothing ships from a draft: the final submitted paper is the ingestion source" is a live gate question, not something this session settles. **What it prints.** Four occurrences, all in tables, all `Non-suicidal Self-injury`: Table 1 (descriptive statistics and internal consistencies for all primary scales and subscales), Table 2 (descriptives by biological sex), Table 3 (mean correlations), and Appendix A (definitions and client-facing descriptions). Two occurrences in prose, both capitalising the S: `Non-Suicidal Self-Injury (NSSI)` at first mention, where the abbreviation is introduced, and `Non-Suicidal Self-injury` later in the results. The paper therefore disagrees with itself across three renderings, which is D-018's "not a reliable naming authority even about itself" pattern arising inside a single document. **The lowercase form is idiosyncratic to this scale, not a house style:** the paper Title-Cases every other multi-word scale name, with zero sentence-case occurrences across six probes (`Social Aloofness` 5, `Anxious Worry` 5, `Angry Hostility` 4, `Manic Energy` 9, `Restricted Affectivity` 4, `Trauma Reactions` 6; each lowercase variant 0). **Corroboration of the deck:** Table 1's row for the scale gives 6 items and alpha 0.83, identical to slide 34 of the December 2023 Workgroup deck, so the deck's table and Table 1 are the same content at two dates.
- 2026-08-27: the provenance of `hitopsr_definitions.csv` runs the other way, corrected by Jeff on 2026-08-27 after this session first recorded it backwards. Jeff wrote the clinician and client-facing definitions for the Society, sent them to Len, and they were then published as the paper's Appendix A — so the paper's appendix descends from the repo's text, not the repo's text from the paper. RB04's "in-repo authored" characterization was right and the milestone's Scope sentence saying so needs no correction; RR04's Beyond-the-brief finding 4, which read the matching spelling as evidence the CSV's author had followed the draft, is wrong in direction. What the correction leaves open is an IP2 question this session did not anticipate: the paper's four tables print `Non-suicidal Self-injury`, the same string Jeff supplied, so adopting it may be adopting the package's own text round-tripped through a publication rather than an independent naming decision — where the two renderings demonstrably not traceable to Jeff's text, the December 2023 deck's `Non-suicidal Self-Injury` and the paper's prose `Non-Suicidal Self-Injury`, both capitalise `Injury`.
- 2026-08-27: amendment gate. Jeff adopted the four-part amendment package (Scope's gate condition, AC1, AC4, AC5) at the mini gate; the amended AC wording then went to a fresh-context [O] criteria audit in full mode (user-facing tier), which returned 14 findings and pronounced AC5's manifest anchor and AC4's DOCX parse-back sound. Three findings changed substance rather than wording, and each was verified in this session before being applied rather than taken on the reader's word. **Only two artifacts are rebuilt, not four:** `zip::zip` stores the intermediate CSV's mtime in `hitopsr_redcap.zip` (confirmed — the committed archive carries a `07-16-2026 22:33` member date), so the zip is not byte-reproducible and rebuilding it would churn a checksum and record a D-016 manifest revision that is not one, while `hitopsr_qualtrics.txt` is byte-reproducible but prints no scale name; `data-raw/artifacts.R` already takes `rebuild_stems`/`rebuild_formats` filters to express this. Scope's "In" clause, its surface-tier sentence, AC4, AC5 and T5 all moved from four to two. **Nothing required the manifest to gain rows:** AC5 froze pre-existing rows and AC4 demoted the re-lock to a step, so a rebuild landing with no version bump would have passed; AC5 now requires exactly two new rows with a `changes` note. **AC4 named an instrument that does not exist:** `tests/testthat/test-artifacts.R` holds no checksums to re-lock — it recomputes `tools::md5sum()` against the latest `hitop_artifacts` row (confirmed at `test-artifacts.R:43`), so the manifest is what changes. Also applied: AC1 now defers the D-032 and D-018 source reasoning to the AC6 entry instead of settling it silently, names the shelf path, sha256 and Table 1 as the compared cell, requires a numbered `OQ-n` in `cairn/SOURCES.md` rather than "a line", bars comparison against any transcription held in this repo rather than only against this file, fixes the rule for a glossed cell, and attributes the Appendix A provenance to the maintainer with its date; AC4 names `officer::docx_summary()` plus a separate footer read as the extractor and allows the scale-name row to move, since the DOCX scoring table sorts on `Scale` (`R/generate_docx.R:491`) — verified that `NSSI` and `Non-suicidal Self-injury` both land at index 46 under `en_US` collation, so a move would be a diff to explain rather than an expected outcome; AC5 asserts the two directory listings are identical to the merge-base's, closing the deleted-file hole. Plan-owned body is 148 lines against the 150 cap; no compression needed.
- 2026-08-27: adopted string settled — `Non-suicidal Self-injury`, as printed in Tables 1-3 of `ASMNT-26-0390`. Jeff settled the two questions the source raised: to proceed on the peer-review-stage submission now and re-shelve the accepted version later, and that the casing did not warrant further deliberation given both hyphenated forms derive the same stem. The IP2 circularity this session raised against that string is resolved rather than merely recorded: Jeff confirmed on 2026-08-27 that Tables 1-3 existed before he sent the paper's authors the definitions CSV, so their spelling is the authors' own and independent of this package, and only Appendix A descends from it.
- 2026-08-27: T1 part 1 — `data-raw/verify_hitopsr_scale_name.R` written and `cairn/SOURCES.md` given a "HiTOP-SR scale names" provenance section and OQ-3. The script is AC1's instrument: it checks the shelf PDF's sha256, inventories every rendering with the page of each, and compares the committed string against Table 1's cell read at run time, never against a transcription — it reads the name from `data-raw/hitopsr_items.csv` rather than carrying a copy, so the comparison cannot go circular. Run 2026-08-27: sha256 matches; `Non-suicidal Self-injury` 3 occurrences on pages 49, 52, 55 (Tables 1-3), `Non-Suicidal Self-injury` 1 on page 23, `Non-Suicidal Self-Injury` 1 on page 18 (both prose), plus a wrapped occurrence on page 69 (Appendix A) reported rather than counted; cell read as `Non-suicidal Self-injury` against the committed `NSSI`, so it exits non-zero. That red is the expected pre-rename state and is what AC1 will read green after T3. The SOURCES section also records what is *not* corroboration: Appendix A is `hitopsr_definitions`'s own text supplied by Jeff to the paper's authors, and Tables 1-3 are admissible only because they predate that supply. The OQ preamble's "Both are encoded as `skip()`-ed tests" was corrected, since OQ-3 carries no such test — it records a disagreement among renderings rather than a falsifiable keying assertion.
- 2026-08-27: T1 complete — [D-041](../DECISIONS.md) landed, citing the SOURCES.md provenance section and OQ-3 that AC1 requires, naming `Non-suicidal Self-injury` and the `hsr_nonSuicidalSelfInjury` rename, and carrying two bindings: the narrowing of D-032's draft bar for scale *names* only (verifiable character for character against one cell, a correction costing two Word rebuilds rather than a re-derivation of shipped numbers) with the reconciliation against the accepted version fixed in advance, and the ruling that `hitopsr_definitions` is not evidence in either direction because its text is the maintainer's own and became the paper's Appendix A.
- 2026-08-27: T2 complete, red in the right places before any keying edit. `tests/testthat/test-scale-name-hitopsr.R` covers the shipped-object half and runs everywhere; `data-raw/verify_hitopsr_rename.R` covers the working-tree sweep and the two merge-base comparisons, which need git and a second build. Two holes were found and closed while writing them, both of the kind AC2 exists to catch. The dataset sweep first read values only, so `names(hitopsr_scales$itemNumbers)` — a list keyed by scale stem, holding `nssi` with every value clean — passed it; the sweep now treats a list's own names as a swept surface, and reports that entry as a fifth offender. And the working-tree sweep first read every file as raw bytes, which is inert against compression: `.rda` is gzip and `.docx` a zip container, so `data/*.rda`, `R/sysdata.rda` and the four Word forms were all being scanned as noise — precisely the surfaces the plan-gate criteria audit had named. Each format is now opened in its own terms, and the sweep went from 2 hits to 9: `data-raw/hitopsr_info.R`, `data-raw/hitopsr_items.csv`, three `data/*.rda`, and the four DOCX under `inst/extdata/` and `pkgdown/assets/downloads/`. `R/sysdata.rda` is clean, its administration text naming no scale. Merge-base is `df14f3c`; `hitopsr_subscales` already compares identical, the scale having no subscales.
- 2026-08-27: T3/T4 — renamed the six `NSSI` cells in `data-raw/hitopsr_items.csv` to `Non-suicidal Self-injury`, deleted the now-inert `definition_scale_labels` map and rewrote its comment in `data-raw/hitopsr_info.R` (the `stopifnot` stem guards stay and passed on the rebuild), and regenerated the four tables. `verify_hitopsr_scale_name.R` is green: the committed string matches Table 1's cell character for character. `test-scale-name-hitopsr.R` is green, 12 passing, 0 skipped. `verify_hitopsr_rename.R` has all four keying tables identical outside the renamed cells and every scored column identical in value; its only remaining hits are the four Word forms, which are T5's. `data/hitopsr_subscales.rda` did not change at all, the scale having no subscales.
- 2026-08-27: two consequences of the rename that nobody anticipated at planning, both sort-order effects, both verified content-neutral. **`hitopsr_scales`' renamed row moves from position 43 to 46**: the tables are built with `dplyr::arrange(Scale)`, which sorts in the C locale, so the uppercase `NSSI` sorted among the uppercase names while `Non-suicidal Self-injury` sorts after `Non-planfulness`. Every other scale keeps its relative order (checked directly). **The scored tibble's column order follows it** — `hsr_nonSuicidalSelfInjury` moves from position 448 to 451 — while every column, that one included, is identical in value to the merge-base build, and no other column reorders. Worth noting that this is locale-dependent in a way the Word forms are not: `make_scoring_table()` sorts with base `order()` under the running locale's collation, where both spellings land at index 46, so the printed forms do not reorder. Both moves need an AC2/AC3 amendment, since those criteria say "identical except the cells that held the old name" and "asserted equal", which a row or column move violates literally; the checks themselves were strengthened rather than loosened to see this — rows are now matched by identity and columns compared by name, so a value change still fails while a position change is reported.
- 2026-08-27: three defects found in this session's own verification code while running it, each of which would have passed the milestone vacuously. `blank_renamed()` blanked the base side by the old spelling and the current side by the new one, so `hitopsr_definitions` — which already carried the new spelling, being the maintainer's own text — appeared to differ when it had not changed; both sides now blank on one combined pattern. The same function returned a bare list for a data frame, so the row-keyed comparison could not subset it. And the working-tree sweep read every file as raw bytes, which is inert against gzip and zip, so `data/*.rda`, `R/sysdata.rda` and the four Word forms were being scanned as noise; opening each format in its own terms took the sweep from 2 hits to 9 before the rename.
- 2026-08-27: T5/T6 — rebuilt exactly the two Word forms via `data-raw/artifacts.R` with `rebuild_stems <- "hitopsr"` and `rebuild_formats <- "docx"`, which is the mechanism the script's own header already prescribed for this case ("because DOCX/zip rebuilds are not byte-deterministic, a needless rebuild churns a checksum and records a manifest revision that isn't one"). Six files moved and no others: the two DOCX, their two `pkgdown/assets/downloads/` copies, `data-raw/artifacts.R` and the manifest. `hitop_artifacts` gained exactly two rows, both dated 2026-08-27, and now holds 35. `test-artifacts.R` passes 121, 0 skipped. AC4's parse-backs all hold: both DOCX carry `Non-suicidal Self-injury` and no `NSSI`, each `pkgdown` copy is byte-identical to the file just built, and `hitopsr_qualtrics.txt` and the REDCap `instrument.csv` carry neither spelling — their bytes are untouched, which is the result AC4 asks for rather than an oversight. The bounded diff was added to `verify_hitopsr_rename.R` as a fourth step: against the merge-base build each Word form differs in exactly one paragraph, row 2064, `NSSI` -> `Non-suicidal Self-injury`, with the footer build stamp changed and asserted to have changed. The paragraph index does not move, because `make_scoring_table()` sorts with base `order()` under the running locale's collation where both spellings land at the same position — so the printed forms do not reorder even though `hitopsr_scales` does.
- 2026-08-27: open at T7 — the working-tree sweep now reports two hits that are neither leftovers nor artifacts: `data-raw/artifacts.R`'s `build_notes` and the `changes` column of `data/hitop_artifacts.rda`, both carrying the sentence "The scale abbreviated NSSI is renamed to its full name, Non-suicidal Self-injury, on the scoring page." That note is the artifact version history, rendered on the pkgdown download pages, so naming the superseded spelling there is the same deliberate history AC2 already allow-lists `NEWS.md` for, and rewording it to avoid the old name would make the version history less informative than the change it records. Whether the allow-list gains the manifest is an AC2 wording question, held open pending the fresh-context audit of the amended AC2/AC3, which was asked to check whether AC2's quoted allow-list is already false about its own instrument — `verify_hitopsr_rename.R` also exempts `data-raw/verify_hitopsr_*` and the sweep's own test file, which AC2's text does not mention.
- 2026-08-27: the fresh-context [O] audit of the amended AC2/AC3 returned 8 findings and pronounced four clauses sound. One was a live defect in this session's own check, and it was reproduced before being fixed rather than taken on the reader's word: **matching rows by key absorbs any permutation**, so the merge-base comparison could not distinguish the renamed row's move from a second, genuine reordering of unrelated scales, and the "expected move" message fired on any key-sequence difference. Planting a clean two-scale swap (`Panic`/`Purging`, with the `itemNumbers` names carried along) confirmed it passed. `verify_hitopsr_rename.R` now asserts the relative order of every other row directly, and asserts the renamed row's new position against `sort(method = "radix")` recomputed from the merge-base's own names — verified to reproduce `dplyr::arrange()`'s C-locale order exactly — rather than printing a position read off the output under test. Also applied: AC2's allow-list text was false about its own instrument, naming two patterns where the check exempts four; AC2 understated the blanking, which is by pattern across both sides and every column, so the exemption is now stated; AC2's "greps the working tree" invited the very byte-grep defect this milestone already hit, so it now names the per-format opening; **AC3's literal call `score_hitopsr(sim_hitopsr, calc_se = TRUE)` errors**, `items` having no default (confirmed), and is corrected to `items = 1:405`; both criteria now name their instrument; and AC3 now requires NEWS to record the column's from/to positions, since GP2 makes a positional break user-visible. The audit's suggestion to give AC3 its own independent position oracle was declined with reason: the scored column order follows `hitopsr_scales`' row order, which AC2 already pins against a recomputed sort, so a second oracle would restate one fact rather than test a second.
- 2026-08-27: the build note was reworded rather than allow-listed. `data-raw/artifacts.R`'s `build_notes` named the old abbreviation, which put it and the manifest's `changes` column — rendered as version history on the pkgdown download pages — into the sweep. Allow-listing the manifest was the alternative and was declined: the sweep's exemption set is the whole point of AC2, and the note reads as well without the superseded spelling, while `NEWS.md`, which is allow-listed, is where a user actually needs the literal old column name for migration. To keep the branch recording one rebuild rather than two, the four artifact files and the manifest were reset to their pre-T5 state and rebuilt once with the corrected note; `hitop_artifacts` holds 35 rows with exactly two dated 2026-08-27. `verify_hitopsr_rename.R` is now fully green on all four steps, and `test-artifacts.R` passes 121 with 0 skips.
- 2026-08-27: plan-owned body is 161 lines against the 150-line cap. The heaviest plan-owned section, Acceptance criteria, was compressed in one pass from 100 lines to 88 — no checkable clause dropped, only prose tightened — and the body is still 11 over. Recorded rather than nibbled at further, on the precedent of the ROADMAP byte-budget overage Jeff accepted on 2026-08-24. Most of the residue is Scope's discharged gate condition, which is now history rather than plan and would be the natural thing to move at archive time.
- 2026-08-27: T7 — the AC5 sweep added to `verify_hitopsr_rename.R` as a fifth step. Both distribution directories hold 24 files, their listings are identical to the merge-base's (so a deleted or added file would be caught, which a loop over the current tree alone cannot see), and exactly the two DOCX differ in each. The manifest anchor caught a wrong assumption of my own first: `hitop_artifacts` is kept sorted rather than appended to — the new rows landed at indices 11 and 15, not at the end — so comparing the first `nrow(base)` rows positionally reported the pre-existing rows as changed when none had. Rows are now matched by content: 33 unchanged, exactly 2 added, one per rebuilt DOCX. All five verifier steps green.
- 2026-08-27: T8 — NEWS entry written, recording the rename as breaking, naming both old and new column names, and per AC3's amendment stating the position move from 448 to 451 so positional selection is warned about rather than left to surprise; it also records that no score changed and that the Qualtrics and REDCap exports are unaffected. The AC2 sweep flagged no doc surface: `?score_hitopsr`, `R/data.R`, `README.Rmd`, `_pkgdown.yml` and the vignettes carried no occurrence of the old name, so there was nothing to fix there, and `README.md` needed no re-knit. What T8 did add is the also-known-as note D-041 committed to, in `hitopsr_items`'s `@details`: it names `NSSI` as the literature's abbreviation for the scale, says the package used it before 0.2.0, and gives the derived column name. That note is itself an occurrence of the old spelling, so the sweep flagged `R/data.R` and its generated `man/hitopsr_items.Rd` — a third touch on AC2's allow-list, put to Jeff rather than edited in, since AC2 had already had its gate and its audit. He chose to amend the allow-list over dropping the note (which would reverse D-041) or writing a note that never spells the name a reader would search for. Both paths added, with the reason recorded beside them in the check.
- 2026-08-27: T9 — `devtools::document()` produces no diff (`man/` and `NAMESPACE` unchanged after a fresh run). `devtools::test()` is FAIL 0 | WARN 0 | SKIP 1 | PASS 14598; the single skip is `test-keying.R:102`, the pre-existing OQ-1 placeholder for the disputed PID-5 SD-TD item 38, which this milestone does not touch — AC7's clause names `test-artifacts.R` and the HiTOP-SR keying tests, and both were run directly at 0 skips (121 and 12 passing). `devtools::check()` is Status: OK, 0 errors, 0 warnings, 0 notes, so AC7's "every remaining NOTE listed in this file with a reason" has an empty obligation. A duplicate `devtools::test()` process from an earlier invocation had to be killed part-way: two runs were contending for the machine and neither was progressing, which is why the first suite log came back empty.
- 2026-08-27: all nine tasks checked; status `review`. Branch pushed and PR opened: https://github.com/jmgirard/hitop/pull/64.
- 2026-08-27: /milestone-review ran. All seven acceptance criteria verified with fresh evidence (recorded in the Review section) and every toolchain consistency check is clean, but the universal consistency gate FAILED: `cairn_validate.py` exits 1 on `weight caps` (this file, 172 plan-owned lines against the <150 cap, shed >=23; heaviest Acceptance criteria 90, Scope 36) and on `iso date format` (line 186, the non-ISO `07-16-2026` zip member mtime quoted in a work-log entry). Status returns to `in-progress`; defect return 1 for this milestone. The three-lens review ran and its 13 findings (all from the [O] diff lens; the two [S] lenses found none) are recorded undisposed in the Review section for triage at the next review's gate.

## Decisions

- 2026-08-27 (RR04, advisory — ingested, no keying decision taken): the reviewer's holdings, recorded here because the adoption decision itself is Jeff's and its `DECISIONS.md` entry lands with AC6, when a string is adopted.
  - **Naming authority for a HiTOP-SR scale, per D-018's per-content-type rule: introduction paper > Workgroup deck > development workbook.** The workbook is primary for item-to-scale membership, reverse flags and item text — the content the keying was built from — and never for what a scale is called.
  - **The December 2023 Measurement Workgroup deck clears IP1's bar under the Society-sanction prong, as the weakest admissible form of it**, on a four-link chain: authored by the HiTOP-SR's lead developer and workgroup chair, served by the developing lab's page, linked from the Society's measures page as the measure's introduction slideshow, and describing the final instrument (405 items, 76 scales, 17 subscales, and the NSSI row's 6 items all match the package). Not an APA key, and "cited publication" is not stretched to a slide deck. Its authority is best-available, not final: the introduction paper outranks it the moment that is in hand.
  - **The workbook's `NSSI` is not a competing name, but "silent" was the wrong framing** and the implement-session work log's use of that word is corrected here. The workbook writes `NSSI` in name-position in three sheets where the other scales appear as `Anxious Worry`, `Angry Hostility`, `Social Aloofness`, `Suicidality`. What defeats a competing-name reading is the deck's own `Non-suicidal Self-Injury (NSSI)` gloss from the same author: one authority at two levels of contraction, not two authorities with two names. D-018's decline-and-document branch is therefore not triggered by the workbook.
  - **The only real inter-source discrepancy is casing, and it runs against the deck.** The introduction paper's 2026-03-24 tracked-changes draft prints `Non-suicidal Self-injury` (lowercase `i`) — recorded in M041's T3 as one of four Table 1 labels that do not join. Both hyphenated casings yield the same stem `nonSuicidalSelfInjury`, so a casing-only reconciliation costs a DOCX rebuild and a SOURCES amendment, not a column rename. Only an unhyphenated `Nonsuicidal Self-Injury` would change the stem, to `nonsuicidalSelfInjury`; neither document in hand uses it.
  - **If the rename proceeds from the deck the string is `Non-suicidal Self-Injury`, with `(NSSI)` dropped from the `Scale` cell.** The parenthetical is an abbreviation gloss — no other deck scale row carries one and `NSSI` occurs exactly once in the whole deck — so carrying it in would restyle this one scale against the other 75 on the printed forms and put parentheses into the cell every stem derives from. `NSSI` survives only as an also-known-as documentation sentence (D-018's established pattern) and in NEWS as "formerly `hsr_nssi`".
  - **The stem stays mechanically derived; the name-to-stem exception is rejected.** The length objection is empirically hollow: `hsr_difficultiesReachingOrgasm` (26) and `hsr_sexRelatedSubstanceUse` (22) already exceed `nonSuicidalSelfInjury` (21), verified against `hitopsr_scales`. An exception would break the invariant that `camelCase` names the `itemNumbers`, the scored columns, the definitions join and M057's tooltip join alike, and would move `definition_scale_labels`' special case out of a build script and into shipped code. The abbreviation-column convention (`pid_INC`, `pid_PNA`) is for validity scales; NSSI is a content scale.

## Review

Reviewed 2026-08-27 against branch `m058-nssi-scale-name` at `f92f49f`, merge-base
`df14f3c` (`origin/main` had not moved since the branch was cut, so no merge was
needed). All seven criteria verified with fresh evidence; the consistency gate then
failed on two `cairn_validate` checks, so the milestone returns to `in-progress`.

### Acceptance-criterion evidence

- **AC1 — met.** `Rscript data-raw/verify_hitopsr_scale_name.R` run fresh, exit 0:
  the shelf PDF's sha256 matches `1c211219…8e425`; the rendering inventory is
  `Non-suicidal Self-injury` 3 (pp. 49, 52, 55 — Tables 1-3), `Non-Suicidal
  Self-injury` 1 (p. 23), `Non-Suicidal Self-Injury` 1 (p. 18), plus the reported
  wrapped occurrence on p. 69; the table cell reads `Non-suicidal Self-injury` and
  matches the string committed in `data-raw/hitopsr_items.csv` character for
  character. The cell carries no parenthetical, so AC1's stop condition did not
  fire. The script reads the committed string out of the CSV rather than carrying a
  copy, so the comparison is not circular, and it scans every page rather than pages
  chosen by the committed string. `cairn/SOURCES.md` carries the "HiTOP-SR scale
  names" provenance section and a numbered **OQ-3** in OQ-1's form, whose per-variant
  counts are this script's own output and whose Appendix A note is attributed to Jeff
  and dated 2026-08-27.
- **AC2 — met.** `Rscript data-raw/verify_hitopsr_rename.R` step 1: the sweep runs
  over 289 tracked files (71 allow-listed) and is clean; each format is opened in its
  own terms rather than as bytes. Step 2: all four keying tables identical to the
  merge-base outside the renamed cells; every other row keeps its relative order,
  asserted directly; the renamed `hitopsr_scales` row moves 43 → 46, equal to where
  the adopted name sorts under a recomputed `sort(method = "radix")` over the
  merge-base's names. The allow-list in the check matches AC2's enumeration
  (`NEWS.md`, `cairn/`, `data-raw/verify_hitopsr_*`, the sweep's test file, `R/data.R`
  and `man/hitopsr_items.Rd`).
- **AC3 — met.** Step 3 of the same run: against the merge-base build,
  `score_hitopsr(sim_hitopsr, items = 1:405, calc_se = TRUE)` returns the same column
  set once the two columns are renamed, and every column is identical in value. The
  renamed column moves 448 → 451, following `hitopsr_scales`' order; no other column
  reorders. NEWS records the position move.
- **AC4 — met.** Exactly the two DOCX were rebuilt. Verified independently of the
  script: each `pkgdown/assets/downloads/` copy is byte-identical to its
  `inst/extdata/` file (sha256 `c6f2b537…6f1a` for US, `2b586012…ad968` for A4); both
  DOCX carry `Non-suicidal Self-injury` once and `NSSI` zero times;
  `hitopsr_qualtrics.txt` and the REDCap `instrument.csv` carry neither spelling.
  Step 4 of the verifier: each Word form's `officer::docx_summary()` text differs from
  the merge-base build in exactly one paragraph (row 2064, `NSSI` →
  `Non-suicidal Self-injury`), and the separately read footer is asserted to have
  changed.
- **AC5 — met.** Step 5: `inst/extdata/` and `pkgdown/assets/downloads/` each hold 24
  files, both listings identical to the merge-base's, and in each directory exactly
  the two DOCX differ. `hitop_artifacts`: 33 rows unchanged, exactly 2 added, one per
  rebuilt DOCX.
- **AC6 — met.** `cairn/DECISIONS.md` D-041 landed in `93106a8`; the keying edit to
  `data-raw/hitopsr_items.csv` landed in `891e7f1`, the next commit — so the sign-off
  precedes the edit as AC6 requires. D-041 cites the `SOURCES.md` provenance section
  and OQ-3, names the adopted string and the `hsr_nssi` → `hsr_nonSuicidalSelfInjury`
  rename. NEWS records the rename as breaking, naming both old and new column names.
- **AC7 — met.** `devtools::document()` leaves the tree clean (`git status` empty
  after a fresh run). `devtools::test()`: FAIL 0 | WARN 0 | SKIP 1 | PASS 14598; the
  single skip is `test-keying.R:102`, the pre-existing OQ-1 PID-5 placeholder, which
  this milestone does not touch — `test-artifacts.R` and
  `test-scale-name-hitopsr.R` each ran at 0 skips. `devtools::check()`: Status OK,
  0 errors, 0 warnings, 0 notes, so AC7's NOTE-listing obligation is empty.

### Consistency gate — FAILED

`cairn_validate.py` exits 1 on two checks, both in this milestone's own tracking file:

- **weight caps:** `cairn/milestones/M058-nssi-scale-name.md` has 172 plan-owned
  lines against the <150 cap (shed ≥23); heaviest first, Acceptance criteria 90 ·
  Scope 36 · Tasks 22 · Coverage 10 · Goal 5. The 2026-08-27 work-log entry recorded
  an overage of 11 lines as accepted after one compression pass; the file has since
  grown to 22 over, and the cap is a validator FAIL rather than a judgment call.
- **iso date format:** line 186 carries the non-ISO date `07-16-2026`, a zip member
  mtime quoted inside a work-log entry.

Advisories (not gate failures): 20 dangling id tokens, all pre-existing D-001–D-012
legacy references in `DESIGN.md`/`SOURCES.md`. `cairn_impact.py` was skipped —
`cairn/DESIGN.md` is not in the diff, so no IP/GP principle changed.

Toolchain checks (`r-package` profile `consistency-gate` slot), all clean:
`devtools::document()` no diff; no hand-edit to `NAMESPACE`/`man/`/`data/*.rda`
(all regenerate); `README.Rmd`/`README.md` untouched by the branch and in sync;
`pkgdown::check_pkgdown()` "No problems found"; NEWS carries the user-visible entry
with no milestone number; no new top-level files, so no `.Rbuildignore` entry owed;
`devtools::check()` clean.

### Independent review — three lenses, 13 findings

Full three-lens fan-out (the diff touches executable surface). Triage is **deferred**:
the gate failed before the step-7 approval gate, which is the triage surface, so every
finding is recorded here undisposed and goes to Jeff at the next review.

- **[S] blame-history: no findings.** Confirmed the `definition_scale_labels` map
  deleted from `data-raw/hitopsr_info.R` was added (in `7e545ba`) precisely to paper
  over the spelling mismatch this rename removes, and that the `stopifnot` added
  alongside it is untouched; confirmed the manifest rows and `.rda` regeneration match
  what NEWS and D-041 describe.
- **[S] prior-PR-comments: no findings.** Archived `## Review` sections on the touched
  files (M057 on `hitopsr_info.R`, M042 on `artifacts.R`, M026's roxygen lesson,
  M035's `NA`-comparison lesson) are all clean against this diff. The GitHub probe
  `gh api repos/jmgirard/hitop/pulls/comments?per_page=1` returned `[]`, so the
  per-PR walk was skipped.
- **[O] diff-bug: 13 findings**, ranked as the reviewer ranked them. Findings 1, 2, 3
  and 5 were re-verified in this session against the implementation rather than taken
  on the reviewer's word.
  1. `data-raw/verify_hitopsr_rename.R` leaks a `git worktree` on every run: the
     `on.exit()` cleanup never fires in `Rscript` at top level, and the `dir.exists()`
     guard cannot fire because `tempdir()` differs each run. **Verified** — 15 stale
     `m058-base-df14f3c8` worktrees were registered in the primary checkout. Pruned
     during this review (`git worktree prune`); the script defect stands.
  2. `R/data.R:133-139` and its generated `man/hitopsr_items.Rd` tell users "Scale
     names are those printed in the HiTOP-SR introduction paper's Table 1", but only
     this one scale's name was sourced there — `cairn/SOURCES.md` names the
     development workbook as primary for the other 75, and M041 records four Table 1
     labels that do not join the package's names at all. **Verified** in the diff. A
     user-facing provenance claim the repo's own record contradicts.
  3. The comment replacing `definition_scale_labels` claims the `stopifnot` "is what
     catches a genuine drift", but that check compares stems, and
     `snakecase::to_any_case()` maps every casing of the name to one stem — so the
     casing divergence D-041's reconciliation clause plans for would pass silently.
     **Verified** against `data-raw/hitopsr_info.R:74-84`.
  4. The break reaches beyond the scored columns: `hitop_module("hitopsr", "NSSI")`
     now aborts, as does `read_module()` on any descriptor recording `"NSSI"`. NEWS
     and D-041 mention only `score_hitopsr()`'s columns. Mitigating: modules ship
     first in this same unreleased 0.2.0.
  5. `data-raw/artifacts.R` merges with `rebuild_stems <- "hitopsr"` /
     `rebuild_formats <- "docx"` and the M058 build note still loaded. **Checked
     against the file's own header**, which states the convention: "Both settings are
     left as the last build ran them, so the script records what it last did."
  6. AC5's per-row manifest requirements are printed, not asserted: step 5 asserts the
     added-row count and file names, then `cat`s the `changes` string, so a stale date
     or empty note would pass.
  7. Step 2's position oracle assumes unique `Scale` values; on `hitopsr_items` (6
     matches) and `hitopsr_definitions` (2) it would return a vector and fail
     spuriously. Unreached because neither table's rows move — a false-fail risk, so
     the oracle is exercised on only one of four tables.
  8. `blank_renamed()` blanks a whole cell on substring match, so a real change
     co-located with the name in one cell would be invisible. Checked: no such cell
     exists today.
  9. AC4's "carry neither" is half-verified for the two unrebuilt formats — the sweep
     proves no `nssi`, but nothing asserts they carry no `Non-suicidal Self-injury`.
  10. Scope's "Out" bullet promising a candidate row for renaming any other scale is
      undischarged; no such row is in `cairn/ROADMAP.md`.
  11. The manifest `changes` note was reworded to pass the sweep rather than
      allow-listed, so the pkgdown version history now says "One scale … by an
      abbreviation" without naming which scale.
  12. NEWS gives the position move for `hsr_nonSuicidalSelfInjury` (448 → 451) but not
      for `hsr_nonSuicidalSelfInjury_se` (524 → 527), which breaks positional
      selection identically.
  13. `verify_hitopsr_scale_name.R:35` identifies the committed name by grepping item
      text, coupling the source check to wording IP1 could change; guarded by a
      `length == 1` `stopifnot`, so it fails loudly rather than silently.
