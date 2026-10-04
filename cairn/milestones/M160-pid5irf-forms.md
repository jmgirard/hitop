# M160: PID-5 informant form (IRF) Word, Qualtrics and REDCap forms

- **Status:** in-progress
- **Priority:** normal
- **Depends on:** M159
- **Driving RR:** —
- **Principles touched:** IP1, IP2
- **Resolves:** —
- **Surface tier:** user-facing — three new exported generators and their downloads
- **Branch/PR:** m160-pid5irf-forms

## Goal

Researchers can build and download PID-5 IRF paper forms, a Qualtrics import file and a REDCap data dictionary.

## Scope

**In:** Word (US and A4), Qualtrics and REDCap generators for the IRF, built on the existing PID-5 builders. They use M159's informant text, keying and instructions. The prebuilt files ship with manifest rows and a download page without an online-form strip.

**Out:**
- Module Builder, hitop-form and Study Link Builder support stay on the downstream candidate row.
- An informant brief or short form is not published, so it gets no row.

## Acceptance criteria

- [ ] AC1: The IRF Word generator writes US and A4 forms. The parsed item rows are exactly the 218 IRF items in APA order. Each row shows its number, its informant text and the 0 to 3 values. The parsed instructions and legend are the informant instructions and response labels that M159 stores. The parsed scoring table lists each facet's IRF item numbers and R marks from M159's IRF keying, not the self-report numbers. The footer carries the APA notice that the key prints. No other item appears.
- [ ] AC2: The IRF Qualtrics file and REDCap zip each hold exactly the 218 IRF items in APA order. Each item carries its informant text and the response values and labels. REDCap field names follow D-055 (`<stem>_001`, with the stem M159's gate chose). Qualtrics IDs follow the existing uppercase `<STEM>_001` convention.
- [ ] AC3: The four files (Word US, Word A4, Qualtrics, REDCap) ship in `inst/extdata/` and `pkgdown/assets/downloads/`. Each is byte-identical to the file that its `hitop_artifacts` row records. A new download page links each one and carries no online-form strip. The page counts in `test-artifacts.R` and `test-download-pages.R` and the strip test's exemption admit the new page.
- [ ] AC4: NEWS.md and the `_pkgdown.yml` reference index list the new generators. `pkgdown::check_pkgdown()` passes. `devtools::check()` reports 0 errors and 0 warnings.

## Coverage

- AC1 → T1, T3
- AC2 → T2, T3
- AC3 → T4
- AC4 → T5

## Tasks

- [x] T1: Add the IRF Word generator in `R/generate_docx.R`, following `generate_docx_pid5()`. The footer carries the APA notice that the key prints.
- [x] T2: Add the Qualtrics and REDCap generators, following the FULL ones.
- [x] T3: Write the parse-back tests (AC1, AC2) in the D-010 style.
- [x] T4: Add the rows to `data-raw/artifacts.R`, build the artifacts and stage the pkgdown copies. Write the download page and add it to the navbar. Update the page counts in `test-artifacts.R` and `test-download-pages.R`, and exempt the page from the online-strip test.
- [x] T5: Add NEWS and reference rows. Run `devtools::document()`, `devtools::test()`, `devtools::check()` and `pkgdown::check_pkgdown()`.

## Work log

- 2026-10-03: created by /milestone-plan, together with M157 to M159 and M161.
- 2026-10-03: criteria audit (full mode, fresh Opus reader) returned 3 clear fixes and 1 judgment finding, all applied. AC1 binds the scoring table to IRF numbers and the footer notice. AC2 separates REDCap names from Qualtrics IDs. AC3 counts four files and exempts the page from the online-strip test.
- 2026-10-04: implement started on branch `m160-pid5irf-forms`, cut from main at `79b9c052` after M159 merged. The untracked `devel/hitopdat_*` files predate the branch and stay out of every commit. Read with D-089, D-090 and the M159 review's deferred note: the generators read `pid_items$Text`, so the IRF ones must use `TextIRF` (row "PID-5 norms not yet shipped").
- 2026-10-04: implement choices: a shared `pid_irf_form()` gives all three generators the 218 IRF rows with `Text` replaced by `TextIRF`. The instruction text is `pid_irf_instructions` start, prompt and stem joined, and items print as stored, with no leading ellipsis. The Word scoring page mirrors the FULL form's (facets only, no domain table). The default title is "PID-5-IRF (Informant Form)".
- 2026-10-04: implement choice: the APA footer notice is a new `notice` element of `pid_irf_instructions` (sysdata), taken from the form, and `check_pid_irf_text.R` now matches it to the PDF (PASS). `build_docx_footer()` and `build_hitop_doc()` gained a `notice`/`footer_notice` argument; NULL keeps the Society line, so the other forms do not change. The other sysdata objects are identical.
- 2026-10-04: T1 to T3 done. `generate_docx_pid5irf()`, `generate_qualtrics_pid5irf()` and `generate_redcap_pid5irf()` added. `test-generate-pid5irf.R` (7 tests, 491 expectations) parses all three back against `pid_items`, `pid_irf_instructions` and the key tables typed in `helper-fixtures.R`. Planted self-report text, the Society footer and FULL scoring numbers each failed their test. The generators joined the shared smoke lists, the legend lock and the Qualtrics ID-width loop (LESSONS M158).
- 2026-10-04: T4 done. `data-raw/artifacts.R` built the four files under instrument "PID-5-IRF" (manifest 48 to 52 rows) and staged 37 download copies. New page `download-pid5irf.Rmd` without an online strip, navbar entry "PID-5 Informant Form (IRF)". Page counts 7 to 8 in `test-artifacts.R` and `test-download-pages.R`, `pid5irf` added to `no_strip_stems`, manifest-generator counts 13 to 15 in `test-export-padding-width.R`, and two builders in `test-response-value-no-move.R`. T1 to T4 ticked: full suite 27592 passes, 0 failures.
- 2026-10-04: T5 done. NEWS entry "New PID-5 Informant Form forms"; three `_pkgdown.yml` reference rows (added in T4); README's export row ticked; DESIGN.md's goal line updated. `document()` no diff, `check_pkgdown()` no problems. The first `devtools::check()` warned on a literal © in `R/generate_docx.R` (the default footer string), written by an edit in place of the `©` escape; restored, and the rerun gives 0 errors, 0 warnings, 0 notes.
- 2026-10-04: claim audit: 96 claims read, 6 corrected — vignettes/articles/download-pid5irf.Rmd, tests/testthat/test-generate-pid5irf.R, R/generate_docx.R, R/generate_qualtrics.R, R/generate_redcap.R
- 2026-10-04: the corrections: the download page's customization sentence names what the generators take; the Society-footer test loops over FULL, SF, BF and BF+M; one test comment reworded and another backed by a rename assertion; the stem quoted with its ellipsis in three help pages; the response-options source comment; and a stale `check_pid_irf_text.R` comment. `test-generate-pid5irf.R` 498 passes, 0 failures. Re-read by the same reader pending.

## Decisions

## Review
