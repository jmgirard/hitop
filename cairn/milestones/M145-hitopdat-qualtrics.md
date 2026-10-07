# M145: HiTOP-DAT Qualtrics import file

- **Status:** blocked
- **Priority:** normal
- **Depends on:** M143, M144
- **Driving RR:** —
- **Principles touched:** IP1, IP2
- **Resolves:** —
- **Surface tier:** user-facing — a new exported generator and its download
- **Branch/PR:** —

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

- [ ] T1: Let `build_qualtrics_txt()` in `R/generate_qualtrics.R` take one answer set per item and one instruction per measure. Keep the output of the other instruments byte-identical, and prove it against their committed `*_qualtrics.txt` files. Add `generate_qualtrics_hitopdat()`.
- [ ] T2: Document in the roxygen block of the generator that each answer carries its scoring value as its recode value (AC2).
- [ ] T3: Write the parse-back test (AC1) and the per-argument refusal test (AC4).
- [ ] T4: Add the row to `data-raw/artifacts.R`. Build the file with LF line endings through a binary connection (LESSONS M020). Stage the pkgdown copy, and link it from `download-hitopdat.Rmd`.
- [ ] T5: Add a NEWS entry and the `_pkgdown.yml` reference row. Run `devtools::document()`, `devtools::test()`, `devtools::check()` and `pkgdown::check_pkgdown()`.

## Work log

- 2026-09-29: created by /milestone-plan, together with M143 and M144.
- 2026-09-29: plan chose a .txt import file built from the tables over shipping a cleaned copy of the shared .qsf. The other instruments use the .txt route, and the .qsf carries unsourced scoring. Falsified by a Qualtrics .txt import that cannot hold the DAT's answer sets.
- 2026-10-06: blocked. Jeff is waiting for confirmation of the HiTOP-DAT items and questions. After that confirmation arrives, the milestone is workable again.

## Decisions

## Review
