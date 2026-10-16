# M145: HiTOP-DAT Qualtrics import file

- **Status:** in-progress
- **Priority:** normal
- **Depends on:** M143, M144
- **Driving RR:** —
- **Principles touched:** IP1, IP2
- **Resolves:** —
- **Surface tier:** user-facing — a new exported generator and its download
- **Branch/PR:** m145-hitopdat-qualtrics

## Goal

Researchers can import the HiTOP-DAT into Qualtrics from a file that the package builds from the M143 tables.

## Scope

**In:** `generate_qualtrics_hitopdat()`, which writes a Qualtrics import .txt file. The shared `build_qualtrics_txt()` takes one answer set for each instrument, so it gains an answer set for each item. The prebuilt file ships with a manifest row, linked from the M144 download page.

**Out:**
- The shared `HiTOP-DAT.qsf` is not shipped. It holds clinic screens, patient fields and unsourced scoring (M143 plan gate).
- Scoring embedded in Qualtrics goes to the HiTOP-DAT scoring candidate row.
- The clinic screens and the "Skip" answer stay out, as in M143.

## Acceptance criteria

- [ ] AC1: `generate_qualtrics_hitopdat()` writes a Qualtrics import .txt file. Its parsed questions are exactly the `hitopdat_items` rows in battery order, with IDs `[[ID:hdat_001]]` to `[[ID:hdat_382]]`. Each parsed question carries the item text and the `[[Choice:value]]` values and labels of its answer set, equal to `hitopdat_choices` and in its order. Its parsed text-only questions are exactly the six instruction texts, each once, before its measure's first item.
- [ ] AC2: `?generate_qualtrics_hitopdat` states that each answer carries its scoring value from `hitopdat_choices` as its Qualtrics recode value. AC1's value check is the test of that statement. The statement makes no claim about a live Qualtrics import, which no test here can run.
- [ ] AC3: The .txt file ships in `inst/extdata/` and `pkgdown/assets/downloads/`, byte-identical to the file its `hitop_artifacts` row records. The HiTOP-DAT download page links it.
- [ ] AC4: The generator refuses each bad input in this list with the named check. `file` is checked by no generator today, so the DAT generator adds its check. A test fires each refusal and asserts its condition.
  - `file`, `block_name` and `id_prefix`: a value that is not one string (`validate_string()`).
  - `include_instructions`: a value that is not one `TRUE` or `FALSE` (`validate_flag()`).
  - `breaks`: a value that is not a count of 0 or more, or `NULL` (`validate_count()`).
- [ ] AC5: NEWS.md and the `_pkgdown.yml` reference index list the generator. `pkgdown::check_pkgdown()` passes. `devtools::check()` reports 0 errors and 0 warnings.

## Coverage

- AC1 → T1, T3
- AC2 → T2
- AC3 → T4
- AC4 → T1, T3
- AC5 → T5

## Tasks

- [x] T1: Let `build_qualtrics_txt()` in `R/generate_qualtrics.R` take one answer set per item and one instruction per measure. Keep the output of the other instruments byte-identical, and prove it against their committed `*_qualtrics.txt` files. Add `generate_qualtrics_hitopdat()`.
- [x] T2: Document in the roxygen block of the generator that each answer carries its scoring value as its recode value (AC2).
- [x] T3: Write the parse-back test (AC1) and the per-argument refusal test (AC4).
- [x] T4: Add the row to `data-raw/artifacts.R`. Build the file with LF line endings through a binary connection (LESSONS M020). Stage the pkgdown copy, and link it from `download-hitopdat.Rmd`.
- [ ] T5: Add a NEWS entry and the `_pkgdown.yml` reference row. Run `devtools::document()`, `devtools::test()`, `devtools::check()` and `pkgdown::check_pkgdown()`.

## Work log

- 2026-09-29: created by /milestone-plan, together with M143 and M144.
- 2026-09-29: plan chose a .txt import file built from the tables over shipping a cleaned copy of the shared .qsf. The other instruments use the .txt route, and the .qsf carries unsourced scoring. Falsified by a Qualtrics .txt import that cannot hold the DAT's answer sets.
- 2026-10-06: blocked. Jeff is waiting for confirmation of the HiTOP-DAT items and questions. After that confirmation arrives, the milestone is workable again.
- 2026-10-16: unblocked, back to planned (M173 plan gate). Katherine Jonas's email of 2026-10-15 confirms the 382 items. It still runs after M144.
- 2026-10-16: implement started on branch m145-hitopdat-qualtrics. Six untracked files predate it (the M172 draft and five `devel/hitopdat_*` files); they stay unstaged.
- 2026-10-16: T1 done. `build_qualtrics_txt()` takes `choices` (one data frame per item) and `sections` (row, id, text); NULL for both writes the old output, and a new test shows the nine other committed `.txt` files rebuild byte-identical. Choices made: `id_prefix` defaults to `hdat`, matching the REDCap names; instruction IDs are `<id_prefix>_<measure>_instructions`; each measure after the first starts a page and `breaks` counts within a measure, as in the REDCap file; a break inside a measure does not repeat the instruction, so each text appears once (AC1); the multi-line DUDIT instruction is written as its lines, untested against a live import.
- 2026-10-16: correction to the T1 line: its new byte test duplicated the existing lock in `test-response-value-no-move.R`, so T3 removes it; that lock covers the nine other `.txt` files.
- 2026-10-16: checkpoint, T2 to T5 edits written (help statement, parse-back and refusal tests, DAT added to the hand-kept test lists, artifact built and staged, download page, README row, NEWS, reference index); the full suite is still running, so no task is ticked yet. The untracked M172 draft stays unstaged.
- 2026-10-16: T2, T3, T4 done. Full `devtools::test()`: 0 failed, 0 errors, 15 skipped (merge-base checks); `document()` left no diff. T3 parses with a new `read_qualtrics_questions()` helper (per-question choices, multi-line text); the help text names the `[[Choice:<value>]]` tag and claims nothing about a live import. T4 rebuilt only the DAT `.txt` (`rebuild_formats = "qualtrics"`); the file is LF, and its staged copy is byte-identical. The README capability row gained Qualtrics.
- 2026-10-16: claim audit: 37 claims read, 6 corrected — R/generate_qualtrics.R (instruction sentence names `include_instructions` and drops the CAPE "Its ID" slip), NEWS.md and the build note (name the `[[Choice:<value>]]` tag; the note says "where it has one"), two test comments. The help's recode sentence stays as AC2 requires; it names the tag. The manifest row was regenerated from `main`'s manifest so its `changes` text carries the corrected note; the `.txt` bytes did not move.

## Decisions

## Review
