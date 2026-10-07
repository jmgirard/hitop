# M144: HiTOP-DAT Word forms and REDCap dictionary

- **Status:** blocked
- **Priority:** normal
- **Depends on:** M143
- **Driving RR:** —
- **Principles touched:** IP1, IP2
- **Resolves:** —
- **Surface tier:** user-facing — two new exported generators and their downloads
- **Branch/PR:** —

## Goal

Researchers can build and download HiTOP-DAT paper forms and a REDCap data dictionary made from the M143 tables.

## Scope

**In:** `generate_docx_hitopdat()` (US and A4) and `generate_redcap_hitopdat()`. The shared Word and REDCap builders take one answer set for each instrument. Each builder gains an answer set for each item, as the HiTOP-HSUM REDCap builder already has. The prebuilt files ship with manifest rows and a new download page.

**Out:**
- The Qualtrics export goes to M145.
- The Word forms carry no scoring page, because no key ships (see the HiTOP-DAT scoring candidate row).
- A module (subset) of the DAT is not requested, so it gets no row.
- The clinic screens and the "Skip" answer stay out, as in M143.

## Acceptance criteria

- [ ] AC1: `generate_docx_hitopdat()` writes US and A4 Word forms. The parsed item rows of each form are exactly the `hitopdat_items` rows in battery order. Each row shows its battery number, its text and the values of its answer set. Each measure's legend shows the value and label pairs of its answer sets from `hitopdat_choices`. The parsed instruction passages are exactly the six instruction texts of `hitopdat_instructions`, each once, before its measure's first item. No other item or instruction appears.
- [ ] AC2: `generate_redcap_hitopdat()` writes a REDCap data-dictionary zip. Its item fields are exactly `hdat_001` to `hdat_382` in battery order. Each item field carries the item text and the values and labels of its answer set from `hitopdat_choices`. Its descriptive fields are exactly the six instruction texts, each once, before its measure's first item field.
- [ ] AC3: Both Word forms and the REDCap zip ship in `inst/extdata/` and `pkgdown/assets/downloads/`. Each is byte-identical to the file that its `hitop_artifacts` row records. A new HiTOP-DAT download page links each one, so the site has 7 download pages. Each Word form's footer names the source of each of the seven measures as SOURCES.md records it. SOURCES.md records the redistribution permission Jeff confirmed.
- [ ] AC4: The two generators refuse each bad input in this list with the named check. Where no generator checks an argument today, the DAT generator adds that check. A test fires each refusal and asserts its condition.
  - `file`, `title` and `font_family`: a value that is not one string (`validate_string()`).
  - `papersize`: a value outside `"us"` and `"a4"` (`match.arg()`).
  - `font_size`: a value that is not one positive number (`validate_count()` or a like check).
  - `form_name`, `required` and `breaks`: the `validate_*()` checks that `build_redcap_zip()` runs.
- [ ] AC5: NEWS.md and the `_pkgdown.yml` reference index list both generators. `pkgdown::check_pkgdown()` passes. `devtools::check()` reports 0 errors and 0 warnings.

## Coverage

- AC1 → T1, T4
- AC2 → T2, T4
- AC3 → T3, T5
- AC4 → T1, T2, T4
- AC5 → T6

## Tasks

- [ ] T1: Let the Word builder (`make_items_table()` and its callers in `R/generate_docx.R`) take one answer set per item, and one instruction and legend per measure. Keep the output of the other instruments unchanged, and prove it with a parse of their current forms. Add `generate_docx_hitopdat()` with no `include_scoring`, and any missing argument checks that AC4 names.
- [ ] T2: Let `build_redcap_zip()` in `R/generate_redcap.R` take one answer set per item, by the `Choice_Set` route of the HiTOP-HSUM builder, and one instruction field per measure. Add `generate_redcap_hitopdat()` with `instrument = "HDAT"`. Keep the dictionaries of the other instruments unchanged.
- [ ] T3: Write the footer notice for the DAT forms. `test-artifacts.R` needs "Copyright" in every footer. The notice names the source of each of the seven measures as SOURCES.md records it. Record in SOURCES.md the redistribution permission Jeff confirmed at the M143 plan gate.
- [ ] T4: Write the parse-back tests (AC1, AC2) and the per-argument refusal tests (AC4).
- [ ] T5: Add the DAT rows to `data-raw/artifacts.R`, build the artifacts and stage the pkgdown copies. Write `vignettes/articles/download-hitopdat.Rmd`, add it to the navbar, and change the page count of 6 in `test-artifacts.R` to 7.
- [ ] T6: Add a NEWS entry and the `_pkgdown.yml` reference rows. Run `devtools::document()`, `devtools::test()`, `devtools::check()` and `pkgdown::check_pkgdown()`.

## Work log

- 2026-09-29: created by /milestone-plan, together with M143 and M145. This plan carries five fixes from M143's criteria audit. The parse checks run both ways and cover the instructions. The item number is named as the battery number. AC3 binds the shipped files, not the md5 test. The refusals are enumerated from `formals()`. T3 settles the footer text.
- 2026-09-29: plan chose to extend the shared builders with an answer set per item over separate DAT-only builders. The HiTOP-HSUM REDCap builder already carries per-set choices. Falsified by an extension that changes the output of another instrument's form or dictionary.
- 2026-10-06: blocked. Jeff is waiting for confirmation of the HiTOP-DAT items and questions. After that confirmation arrives, the milestone is workable again.

## Decisions

## Review
