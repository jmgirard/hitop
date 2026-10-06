# M163: PID-5 forensic form (FFBF) English forms

- **Status:** blocked
- **Priority:** normal
- **Depends on:** M162
- **Driving RR:** —
- **Principles touched:** IP1, GP2
- **Resolves:** —
- **Surface tier:** user-facing — six new exported generators and their downloads
- **Branch/PR:** —

## Goal

Researchers can build and download English PID-5-FFBF self-report and informant forms as Word files, a Qualtrics import file and a REDCap data dictionary.

## Scope

**In:**
- Six generators: `generate_{docx,qualtrics,redcap}_pid5ffbf()` for the self-report form and `generate_{docx,qualtrics,redcap}_pid5ffbfirf()` for the informant form. They are built on the existing PID-5 builders and use M162's `pid_ffbf_items` English text and keying.
- Table S3 prints no instructions. So the self-report forms use the APA instructions and response labels in `pid_instructions`, and the informant forms use `pid_irf_instructions`. The paper's scale (0 "very false" to 3 "very true", p. 33) has the same end points.
- REDCap fields are `pid5ffbf_001` to `pid5ffbf_100` and `pid5ffbfirf_001` to `pid5ffbfirf_100`. Qualtrics IDs are the uppercase forms. With two stems, one REDCap project can hold a prisoner's self-report and the informant reports, as the paper's design needs.
- The prebuilt files ship with manifest rows and a download page without an online-form strip.

**Out:**
- German forms go to the "Instruments awaiting materials" candidate row. No source on the shelf prints the German labels for 1 and 2.
- Module Builder, hitop-form and Study Link Builder support go to the same row.

## Acceptance criteria

- [ ] AC1: The two FFBF Word generators each write US and A4 forms. The parsed item rows of each form are exactly the 100 items in item-number order. Each row shows its number, its English text for that form (self or informant) and the 0 to 3 values. The parsed instructions and legend are the `start`, `continue`, `prompt` and `options` fields of `pid_instructions` (self) or `pid_irf_instructions` (informant). The informant rows follow the stored `stem`, as the IRF forms do. The parsed scoring table lists each of the 25 facets with its 4 FFBF item numbers. R marks sit on the reverse items of `pid_ffbf_items`. The table also lists the 7 domains with their facets from `pid_ffbf_domains`. The footer carries Table S3's copyright notice verbatim and cites Niemeyer et al. (2022), in place of the stored APA `notice`. No other item appears.
- [ ] AC2: The two Qualtrics files and the two REDCap zips each hold exactly the 100 items in item-number order. Each item carries its English text for that form and the response values and labels. REDCap field names are `pid5ffbf_001` (self) and `pid5ffbfirf_001` (informant) to `_100`. Qualtrics IDs are `PID5FFBF_001` and `PID5FFBFIRF_001` to `_100`.
- [ ] AC3: The eight files ship in `inst/extdata/` and `pkgdown/assets/downloads/`. These are Word US, Word A4, Qualtrics and REDCap for each form. Each is byte-identical to the file that its `hitop_artifacts` row records. A new download page links each one and carries no online-form strip. The page counts in `test-artifacts.R` and `test-download-pages.R` and the strip test's exemption admit the new page.
- [ ] AC4: SOURCES.md records the basis for redistributing the FFBF item text. The record names who grants it (the rights holders or the authors), its date, and Jeff's confirmation.
- [ ] AC5: NEWS.md and the `_pkgdown.yml` reference index list the six generators. `pkgdown::check_pkgdown()` passes. `devtools::check()` reports 0 errors and 0 warnings.
- [ ] AC6: A test scores the 100 informant columns of the REDCap export with `score_pid5(version = "FFBF", items = )`. It gets the same values as the same answers under self-report names. The informant generators' help pages show that call, and state that `label_pid5()` labels with the self-report text.

## Coverage

- AC1 → T1, T3
- AC2 → T2, T3
- AC3 → T4
- AC4 → T5
- AC5 → T5
- AC6 → T3, T5

## Tasks

- [ ] T1: Add the two Word generators in `R/generate_docx.R`, following `generate_docx_pid5irf()`. The scoring table reads FFBF item numbers from `pid_scales$FFBF`.
- [ ] T2: Add the four Qualtrics and REDCap generators, following the IRF ones.
- [ ] T3: Write the parse-back tests (AC1, AC2) in the D-010 style, and the informant scoring test (AC6). Add each new generator to the hand-kept test lists that the M158 lesson names.
- [ ] T4: Add the rows to `data-raw/artifacts.R`, build the artifacts and stage the pkgdown copies. Write the download page and add it to the navbar. Update the page counts in `test-artifacts.R` and `test-download-pages.R`, and exempt the page from the online-strip test.
- [ ] T5: Record the redistribution basis in SOURCES.md (AC4). This milestone does not merge without it. Add NEWS and reference rows, and the informant help text (AC6). Run `devtools::document()`, `devtools::test()`, `devtools::check()` and `pkgdown::check_pkgdown()`.

## Work log

- 2026-10-04: created by /milestone-plan, together with M162.
- 2026-10-04: question set: German forms? Answer: English forms only. Text permission? Answer: Jeff gives it before this milestone merges.
- 2026-10-04: plan chose the APA instructions and labels over the codebook's English instructions. The codebook is a 2017 draft whose English item text differs from Table S3. The BF+M forms reuse `pid_instructions` on the same grounds (SOURCES.md, M158). Falsified by a shelved FFBF questionnaire that prints its own English instructions.
- 2026-10-04: plan chose a separate informant stem over D-091's shared stem. D-091 shared stems because the child items equal the adult items. FFBF informant items differ in text, and the paper collects self and informant reports on the same prisoner. Falsified by users who need informant columns named as self-report columns for `rename_pid5_items()`.
- 2026-10-04: criteria audit (full mode, fresh Opus reader) returned 4 findings on M163, all applied. AC1 and AC2 use item-number order. AC1 names the instruction fields and the 7 domains on the scoring page. AC4 records who grants redistribution, not only Jeff's word. The new AC6 covers informant columns, which M162's defaults name and label as self-report items.
- 2026-10-04: audit judgment on the footer: the English forms carry Table S3's notice verbatim (IP1), because it is the source's notice for this text, although it names the German version.
- 2026-10-04: from RR08 (M162's keying review): Table S3 prints no English informant stem (its cells start with an ellipsis and use they/them, item 48 uses his/her) and no response labels beyond the end points. This plan already takes the stem and labels from the APA forms. Also, SOURCES.md OQ-7 lists five English typos that the forms will print unless Jeff signs off a correction.

## Decisions

## Review
