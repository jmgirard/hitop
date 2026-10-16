# M144: HiTOP-DAT Word forms and REDCap dictionary

- **Status:** in-progress
- **Priority:** normal
- **Depends on:** M143, M173
- **Driving RR:** —
- **Principles touched:** IP1, IP2
- **Resolves:** —
- **Surface tier:** user-facing — two new exported generators and their downloads
- **Branch/PR:** m144-hitopdat-word-redcap

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
- [ ] AC3: Both Word forms and the REDCap zip ship in `inst/extdata/` and `pkgdown/assets/downloads/`. Each copy has the md5 that the file's latest `hitop_artifacts` row records. A new HiTOP-DAT download page links each one's `../downloads/` copy, so `vignettes/articles/` holds 11 `download-*.Rmd` pages. Each Word form's footer names each of the seven measures with the author and year that its heading in the revised HiTOP-DAT manual's "Measures and reference data" section prints, except that the WHODAS is credited to the World Health Organization with no year.
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

- [x] T1: Let the Word builder (`make_items_table()` and its callers in `R/generate_docx.R`) take one answer set per item, and one instruction and legend per measure. Keep the output of the other instruments unchanged, and prove it with a parse of their current forms. Add `generate_docx_hitopdat()` with no `include_scoring`, and any missing argument checks that AC4 names.
- [x] T2: Let `build_redcap_zip()` in `R/generate_redcap.R` take one answer set per item, by the `Choice_Set` route of the HiTOP-HSUM builder, and one instruction field per measure. Add `generate_redcap_hitopdat()` with `instrument = "HDAT"`. Keep the dictionaries of the other instruments unchanged.
- [x] T3: Write the footer notice for the DAT forms. `test-artifacts.R` needs "Copyright" in every footer. The notice names each measure as AC3 says. Record in SOURCES.md the seven citations, the manual's WHODAS year as an open question, and the redistribution permission Jeff confirmed at the M143 plan gate.
- [ ] T4: Write the parse-back tests (AC1, AC2) and the per-argument refusal tests (AC4).
- [ ] T5: Add the DAT rows to `data-raw/artifacts.R`, build the artifacts and stage the pkgdown copies. Write `vignettes/articles/download-hitopdat.Rmd`, add it to the navbar, and change the page counts of 10 in `test-artifacts.R` and `test-download-pages.R` to 11. Until the Qualtrics file ships, the DAT page has two cards, so the three-card and Qualtrics-link checks except it. DESIGN.md's count of download articles follows.
- [ ] T6: Add a NEWS entry and the `_pkgdown.yml` reference rows. Run `devtools::document()`, `devtools::test()`, `devtools::check()` and `pkgdown::check_pkgdown()`.

## Work log

- 2026-09-29: created by /milestone-plan, together with M143 and M145. This plan carries five fixes from M143's criteria audit. The parse checks run both ways and cover the instructions. The item number is named as the battery number. AC3 binds the shipped files, not the md5 test. The refusals are enumerated from `formals()`. T3 settles the footer text.
- 2026-09-29: plan chose to extend the shared builders with an answer set per item over separate DAT-only builders. The HiTOP-HSUM REDCap builder already carries per-set choices. Falsified by an extension that changes the output of another instrument's form or dictionary.
- 2026-10-06: blocked. Jeff is waiting for confirmation of the HiTOP-DAT items and questions. After that confirmation arrives, the milestone is workable again.
- 2026-10-16: unblocked, back to planned (M173 plan gate). Katherine Jonas's email of 2026-10-15 confirms the 382 items and says every scale is public. M173 records it in SOURCES.md, which T3 cites with the M143 permission. Now depends on M173, since both edit SOURCES.md "HiTOP-DAT".
- 2026-10-16: implement started. Branch `m144-hitopdat-word-redcap` cut from `main` at `1c30667d`. The untracked M172 draft and `devel/hitopdat_*` files are not this milestone's and stay unstaged.
- 2026-10-16: AC3's "7 download pages" is stale: the PID-5 forms brought the site to 10, so the new page makes 11. Amendment of the count sent to a fresh reader before writing.
- 2026-10-16: re-audit: AC3 (full) — 2 findings: tie each copy to the md5 of the file's latest manifest row; the footer clause bound a record T3 itself writes, and the SOURCES.md permission sentence binds a recording act. Fixed in the draft: latest-row md5, footer cites the revised manual's "Measures and reference data" section, permission recording moved to T3.
- 2026-10-16: re-audit: AC3 (full) — 3 findings: that section gives author-year citations in its headings only, and the WHODAS heading cites "World Health Organization & others, 1988" for the 12-item short form; "11 download pages" names no count method; "links each one" names no copy. Second re-audit on AC3, so the wording goes to Jeff (stop). AC3 in this file is still the plan's text.
- 2026-10-16: substantive amendment: AC3 rewritten at Jeff's stop selection ("WHO, no year"): copies tied to the latest manifest row's md5, 11 `download-*.Rmd` pages, links to the `../downloads/` copies, and footer citations from the manual's headings with the WHODAS credited to WHO without the printed 1988. The deliverable is unchanged. No further reader: AC3 has its two re-audit lines. T3 and T5 wording follow (minor).
- 2026-10-16: T1 in progress: `build_hitop_doc()` takes a list of sections (instruction plus tables), and `instruction_fpar()` turns "\n" into line breaks. The document, header and footer parts of all 35 other Word forms (US and A4) and the 10 REDCap dictionaries match `main` byte for byte, the footer date aside.

## Decisions

## Review
- 2026-10-16: T1 done. `generate_docx_hitopdat()` prints one section per measure: its instruction (none for the CAPE), then one items table per run of items sharing an answer set, each with its own legend and option columns. `make_items_table()` is unchanged; its callers pass the run's answer set. Choices made: a legend over 80 characters prints two options per line; tables are kept apart by a small blank paragraph, since Word joins adjacent tables; the title is "HiTOP-DAT"; `font_size` is checked by a new `validate_positive_number()` (a size such as 10.5 is valid, so `validate_count()` would refuse a good value). The footer notice is T3.
- 2026-10-16: T2 done. `build_redcap_zip()` takes one choices string per item (`choices`, built per answer set as the HSUM builder does) and per-measure instruction fields (`sections`). `generate_redcap_hitopdat()` writes 388 rows: `hdat_001` to `hdat_382` and six `hdat_<measure>_instructions` fields. Choice made: each measure after the first starts a new page, and `breaks` counts from each measure's start. Other dictionaries match `main` byte for byte. One commit for T1 and T2, because the regenerated NAMESPACE carries both exports.
- 2026-10-16: T3 done. `hitopdat_instructions$notice` (`data-raw/sysdata.R`, rebuilt `R/sysdata.rda`; no other object changed) is the footer line "Copyright and sources:" plus the seven measures with their manual-heading author and year, the WHODAS without its year. SOURCES.md "HiTOP-DAT" gains DAT-16 (the WHODAS year) and a section with the permission and the seven citations. The simple-english hook flags 227 older SOURCES.md sites, left alone as outside this milestone.
