# M160: PID-5 informant form (IRF) Word, Qualtrics and REDCap forms

- **Status:** planned
- **Priority:** normal
- **Depends on:** M159
- **Driving RR:** —
- **Principles touched:** IP1, IP2
- **Resolves:** —
- **Surface tier:** user-facing — three new exported generators and their downloads
- **Branch/PR:** —

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

- [ ] T1: Add the IRF Word generator in `R/generate_docx.R`, following `generate_docx_pid5()`. The footer carries the APA notice that the key prints.
- [ ] T2: Add the Qualtrics and REDCap generators, following the FULL ones.
- [ ] T3: Write the parse-back tests (AC1, AC2) in the D-010 style.
- [ ] T4: Add the rows to `data-raw/artifacts.R`, build the artifacts and stage the pkgdown copies. Write the download page and add it to the navbar. Update the page counts in `test-artifacts.R` and `test-download-pages.R`, and exempt the page from the online-strip test.
- [ ] T5: Add NEWS and reference rows. Run `devtools::document()`, `devtools::test()`, `devtools::check()` and `pkgdown::check_pkgdown()`.

## Work log

- 2026-10-03: created by /milestone-plan, together with M157 to M159 and M161.
- 2026-10-03: criteria audit (full mode, fresh Opus reader) returned 3 clear fixes and 1 judgment finding, all applied. AC1 binds the scoring table to IRF numbers and the footer notice. AC2 separates REDCap names from Qualtrics IDs. AC3 counts four files and exempts the page from the online-strip test.

## Decisions

## Review
