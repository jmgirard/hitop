# M145: HiTOP-DAT Qualtrics import file

- **Status:** review
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

- [x] AC1: `generate_qualtrics_hitopdat()` writes a Qualtrics import .txt file. Its parsed questions are exactly the `hitopdat_items` rows in battery order, with IDs `[[ID:hdat_001]]` to `[[ID:hdat_382]]`. Each parsed question carries the item text and the `[[Choice:value]]` values and labels of its answer set, equal to `hitopdat_choices` and in its order. Its parsed text-only questions are exactly the six instruction texts, each once, before its measure's first item.
- [x] AC2: `?generate_qualtrics_hitopdat` states that each answer carries its scoring value from `hitopdat_choices` as its Qualtrics recode value. AC1's value check is the test of that statement. The statement makes no claim about a live Qualtrics import, which no test here can run.
- [x] AC3: The .txt file ships in `inst/extdata/` and `pkgdown/assets/downloads/`, byte-identical to the file its `hitop_artifacts` row records. The HiTOP-DAT download page links it.
- [x] AC4: The generator refuses each bad input in this list with the named check. `file` is checked by no generator today, so the DAT generator adds its check. A test fires each refusal and asserts its condition.
  - `file`, `block_name` and `id_prefix`: a value that is not one string (`validate_string()`).
  - `include_instructions`: a value that is not one `TRUE` or `FALSE` (`validate_flag()`).
  - `breaks`: a value that is not a count of 0 or more, or `NULL` (`validate_count()`).
- [x] AC5: NEWS.md and the `_pkgdown.yml` reference index list the generator. `pkgdown::check_pkgdown()` passes. `devtools::check()` reports 0 errors and 0 warnings.

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
- [x] T5: Add a NEWS entry and the `_pkgdown.yml` reference row. Run `devtools::document()`, `devtools::test()`, `devtools::check()` and `pkgdown::check_pkgdown()`.

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
- 2026-10-16: T5 done; status review. On the final tree: `devtools::test()` 0 failed, 0 errors, 15 skipped; `devtools::check()` 0 errors, 0 warnings, 0 notes; `pkgdown::check_pkgdown()` no problems (`_pkgdown.yml` unchanged since). Open point for review: unlike the REDCap file, a page break inside a measure does not repeat the instruction, because AC1 binds each text to appear once.

## Decisions

## Review

- 2026-10-16 AC1 evidence: a fresh `generate_qualtrics_hitopdat()` build parsed with `read_qualtrics_questions()`. First line is `[[AdvancedFormat]]`. The 382 MC IDs equal `sprintf("hdat_%03d", 1:382)`. The texts equal `hitopdat_items$Text` in `Item` order. All 382 choice blocks equal their set's `hitopdat_choices` values and labels, in order. There are 6 DB questions, whose texts equal the 6 non-NA `hitopdat_instructions$start` in battery order, and each is followed by its measure's first item. `test-generate-hitopdat.R` Qualtrics tests: 0 failed.
- 2026-10-16 AC2 evidence: the rendered `man/generate_qualtrics_hitopdat.Rd` reads "Each answer carries its scoring value from `hitopdat_choices$Value` as its Qualtrics recode value, written in the file as the answer's [[Choice:<value>]] tag." It makes no statement about the result of a live import. AC1's per-item value check is the test that backs it.
- 2026-10-16 AC3 evidence: `hitop_artifacts` has 1 row for `hitopdat_qualtrics.txt` (HiTOP-DAT, qualtrics). Its md5 equals the md5 of `inst/extdata/`, of `pkgdown/assets/downloads/` and of a fresh default build. The file has 0 CR bytes. `download-hitopdat.Rmd:33` links `../downloads/hitopdat_qualtrics.txt`.
- 2026-10-16 AC4 evidence: "generate_qualtrics_hitopdat refuses a bad value of each argument" ran with 16 expectations and 0 failed. Its map equals the generator's formals, and each of the 5 arguments is refused with its validator's message, with no file written. The arguments and values are `file` c("a.txt", "b.txt"), `block_name` 1, `id_prefix` NA, `include_instructions` NA and `breaks` -1, which gives "out of range". `breaks = 1.5` is refused as "not whole", and 0 and NULL are accepted (3 expectations, 0 failed). `file` and `id_prefix` are checked by `validate_string()` in the generator, and the others by the builder's existing validators.
- 2026-10-16 gate: `cairn_validate.py` exits 1 in the working tree only on "roadmap<->disk orphans" for the untracked M172 draft, which is not on this branch. On a clean HEAD copy it exits 0, with 37 advisories. `devtools::document()` leaves no diff. `pkgdown::check_pkgdown()` reports no problems. NEWS has the entry, and README.md was rebuilt from README.Rmd. No new top-level files. No principle changed, so `cairn_impact` is skipped.
- spawned: diff-bug, blame-history, prior-review
- diff-bug #1: the recode claim is untested by a live import, and the 0, 2, 4 harm sets would show a position-based recode — follow-up, row "DAT online file gaps" (AC2's wording is the bound statement and claims nothing about a live import).
- diff-bug #2: the 10-line DUDIT instruction is unconfirmed in a live import — follow-up, row "DAT online file gaps".
- diff-bug #3: `import-instructions.Rmd` leaves the HiTOP-DAT out of its `.txt` list — fix now, fixed a7ebf397.
- diff-bug #4: the help's "each of the seven measures that has instruction text" can read as seven questions — fix now (now "six of the seven measures, all but the CAPE"), fixed a7ebf397.
- diff-bug #5: `page_header` combined with `sections` would number headers by item — fix now (a `stopifnot()` refuses the combination, as in `build_redcap_zip()`), fixed 1486f18d.
- diff-bug #6: a first section not at row 1, or unsorted rows, misplaces breaks — fix now (same `stopifnot()`), fixed 1486f18d.
- diff-bug #7: `choices` of the wrong length or type fails with no argument named — fix now (`stopifnot()` on length and type), fixed a7ebf397.
- diff-bug #8: a misnamed measure would silently drop its instruction — reject, false as a defect: the AC1 test asserts the six texts and fails on that mistake.
- diff-bug #9: the Qualtrics file has no source notice, unlike the Word footer — follow-up, row "DAT online file gaps" (the REDCap file has none either).
- diff-bug #10: the `expect_setequal()` type assertion is weak — reject, false: `expect_setequal()` fails on any third question type, which is what its comment states.
- diff-bug #11: the "module = NULL … byte-identical" test title no longer describes the DAT case — fix now (retitled to the width check it makes), fixed a7ebf397.
- diff-bug #12: `artifacts.R`'s filter and note were overwritten — reject, planned change: the script's own header says it records the last build it ran.
- blame-history #1: a page break inside a measure does not repeat the instruction, unlike REDCap (M144) and the IRF `page_header` (M160) — follow-up, row "DAT online file gaps"; AC1 binds one copy of each text, so repeating them changes the plan, which is the user's decision at the merge question.
- blame-history #2: no guard on `sections` as `build_redcap_zip()` has — fix now (same fix as diff-bug #5 and #6), fixed 1486f18d.
- blame-history #3: `test-download-pages.R` comments still say the DAT has no Qualtrics file — fix now, fixed 1486f18d.
- blame-history #4: `cairn/DESIGN.md` lines 14 and 47 still say the DAT has no Qualtrics file — fix now, fixed 1486f18d.
- blame-history #5: the ID-width loop's comment says it locks every existing generator — fix now (same fix as diff-bug #11), fixed a7ebf397.
- blame-history #6: `rebuild_formats` was narrowed to `qualtrics` — reject, planned change (the script's rule to rebuild only the format that changed).
- blame-history #7: the log says T3 removed a duplicate byte test the diff does not show — reject, false: T1's commit added it and T3 removed it, so the net diff holds none, as the log states.
- prior-review #1: no instruction repeated after an in-measure break, regressing the M144 and M160 fixes — follow-up, row "DAT online file gaps" (same finding as blame-history #1).
- prior-review #2: DESIGN.md is stale — fix now (same as blame-history #4), fixed 1486f18d.
- prior-review #3: `generate_docx_hitopdat()`'s `@seealso` does not name the Qualtrics generator — fix now (the Word and Qualtrics help pages now cross-link), fixed 1486f18d.
- 2026-10-16 AC5 evidence: on the tree after both fix commits, `devtools::check()` reports 0 errors, 0 warnings and 0 notes. `devtools::test()` has 0 failed, 0 errors and 15 skipped, all merge-base checks. `pkgdown::check_pkgdown()` reports no problems. NEWS.md has the "HiTOP-DAT Qualtrics import file" entry, and `_pkgdown.yml` lists `generate_qualtrics_hitopdat`.
