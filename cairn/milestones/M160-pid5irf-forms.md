# M160: PID-5 informant form (IRF) Word, Qualtrics and REDCap forms

- **Status:** review
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

- [x] AC1: The IRF Word generator writes US and A4 forms. The parsed item rows are exactly the 218 IRF items in APA order. Each row shows its number, its informant text and the 0 to 3 values. The parsed instructions and legend are the informant instructions and response labels that M159 stores. The parsed scoring table lists each facet's IRF item numbers and R marks from M159's IRF keying, not the self-report numbers. The footer carries the APA notice that the key prints. No other item appears.
- [x] AC2: The IRF Qualtrics file and REDCap zip each hold exactly the 218 IRF items in APA order. Each item carries its informant text and the response values and labels. REDCap field names follow D-055 (`<stem>_001`, with the stem M159's gate chose). Qualtrics IDs follow the existing uppercase `<STEM>_001` convention.
- [x] AC3: The four files (Word US, Word A4, Qualtrics, REDCap) ship in `inst/extdata/` and `pkgdown/assets/downloads/`. Each is byte-identical to the file that its `hitop_artifacts` row records. A new download page links each one and carries no online-form strip. The page counts in `test-artifacts.R` and `test-download-pages.R` and the strip test's exemption admit the new page.
- [x] AC4: NEWS.md and the `_pkgdown.yml` reference index list the new generators. `pkgdown::check_pkgdown()` passes. `devtools::check()` reports 0 errors and 0 warnings.

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
- 2026-10-04: the same reader re-read all 7 corrected claims once and found each accurate, with the test file passing. Its two notes on the check-script comment (check order, line length) were fixed.
- 2026-10-04: implement done. Status set to `review`.

## Decisions

- 2026-10-04 (T1, IP1, sign-off asked at the merge gate): the PID-5 Informant Form's forms print its own text from `pid_irf_instructions`, all matched to the APA PDF. Word prints the opening paragraph, then the rating prompt and "He or she…" stem as a repeated header row of the item table. Qualtrics and REDCap print the opening paragraph, prompt and stem as the first block, and restate the prompt and stem at the top of each later page. Items print as `TextIRF` (no stem, ellipsis or final period). The Word footer carries the form's APA copyright and permission notice in place of the Society line. The form's name, informant and relationship fields and its "Clinician Use" column are not printed, as on the other PID-5 forms.

## Review

- Evidence AC1 (2026-10-04, head `ebe0952a`): `test-generate-pid5irf.R` 0 failures, 498 passes. Its Word tests check, for US and A4, 218 parsed item rows numbered 1 to 218 with `pid_items$TextIRF` text and no self-report wording, each followed by 0 to 3. They check the instruction run equal to the stored start, prompt and stem, the legend equal to `pid_irf_instructions$options`, and 25 facet rows equal to the key's Facet Table typed in `helper-fixtures.R`, with "(R)" on its 14 items. The footer carries `pid_irf_instructions$notice`, which `check_pid_irf_text.R` matches to the form's printed notice, and no Society line. The committed US and A4 files parse to the same 218 rows, 25 facets, 14 R marks and APA footer.
- Evidence AC2 (2026-10-04, head `ebe0952a`): the same file's Qualtrics test finds 218 questions with IDs `PID5IRF_001` to `PID5IRF_218` in IRF order, `TextIRF` text, the 4 stored choice pairs on every question and the stored instruction text. Its REDCap test finds 219 rows, the instruction row then fields `pid5irf_001` to `pid5irf_218` (D-055 stem from D-089), with `TextIRF` labels and the same choice string, and `rename_pid5_items(version = "IRF")` gives those names. The committed Qualtrics and REDCap files parse to the same IDs, names and text.
- Evidence AC3 (2026-10-04, head `ebe0952a`): the four `pid5irf_*` files exist in `inst/extdata/` and `pkgdown/assets/downloads/`, `cmp` finds each pair identical, and their md5 sums equal the 4 new `hitop_artifacts` rows (instrument "PID-5-IRF", 52 rows in all). `test-artifacts.R` 0 failures, 171 passes; `test-download-pages.R` 0 failures, 136 passes. These cover the 8-page counts, `pid5irf` in `no_strip_stems` with no strip rendered on the new page, and the page linking exactly its 4 manifest files.
- Evidence AC4 (2026-10-04, head `ebe0952a`): NEWS.md has the "New PID-5 Informant Form forms" entry naming the three generators, and `_pkgdown.yml` lists them in the reference index (plus the navbar page). `pkgdown::check_pkgdown()` finds no problems. `devtools::check()` gives 0 errors, 0 warnings and 0 notes; it runs the full suite.
- Consistency gate (2026-10-04): `cairn_validate.py` passes (advisories only: 32 legacy dangling ids, 1 references staleness). `devtools::document()` makes no diff. `check_pkgdown()` no problems. README.Rmd's export row is ticked and README.md matches. NEWS.md has the entry. No new top-level files. No DESIGN principle changed, so `cairn_impact` is skipped.
- spawned: diff-bug, blame-history, prior-review
- diff-bug #1: `import-instructions.Rmd` listed the `.txt` Qualtrics files without the BF+M or the IRF, though the IRF page links there — fix now, fixed 5494cc29.
- diff-bug #2: `overview.Rmd` said every page but the HSUM's offers an online form — fix now (names the five pages that do), fixed 5494cc29.
- diff-bug #3: DESIGN.md and the multi-language row said seven download pages, and the informant-gaps row still listed the builders' `TextIRF` routing as open — fix now, fixed 5494cc29.
- diff-bug #4: the Qualtrics and REDCap exports printed the prompt and stem once, so later pages showed bare phrases with no subject — fix now: `build_qualtrics_txt()` and `build_redcap_zip()` gained `page_header` (a block or section header after each break), and the Word item table gained a repeated header row with the prompt and stem; the other exports are unchanged (tested), and the four IRF files were rebuilt; fixed 5494cc29.
- diff-bug #5: the Word form prints no name, informant or relationship fields — follow-up, row "PID-5 norms not yet shipped" (informant gaps).
- diff-bug #6: the APA rights page bars modifying the measure without written permission — follow-up, same row, and put to Jeff at the merge question.
- diff-bug #7: the FULL, SF and BF Word footers still credit the Society while the IRF prints the APA notice — follow-up (pre-existing), in the "Form text awaiting a source and sign-off" row, whose wording now excepts the IRF.
- diff-bug #8: footer checks ran on US paper only and the Society loop skipped HiTOP-SR and HiTOP-BR — fix now (both paper sizes; SR and BR added), fixed 5494cc29.
- diff-bug #9: option and order assertions cannot tell IRF from self-report sources — reject, false: the options are a copy by construction and IRF numbers rise with FULL numbers (D-089 mapping), so no reachable state separates them.
- diff-bug #10: README's "Add Instrument Export Functions" box stayed open with all children ticked — fix now, fixed 5494cc29.
- diff-bug #11: `test-export-arg-guards.R:114` counts eleven generators — follow-up (pre-existing), informant-gaps row.
- blame-history #1: the "Form text" row said every PID-5 footer credits the Society, and the 4 IRF manifest rows record "1.0" — row wording fixed now, fixed 5494cc29; the "1.0" rows are a follow-up in the informant-gaps row.
- blame-history #2: new participant-facing text with no recorded IP1 sign-off or SOURCES row — fix now (SOURCES.md row; milestone Decisions entry; sign-off asked at the merge question), fixed 5494cc29.
- blame-history #3: footer tests covered only some forms — fix now, same as diff-bug #8, fixed 5494cc29.
- blame-history #4: the informant-gaps row still said M160 must route the builders through `TextIRF` — fix now, same as diff-bug #3, fixed 5494cc29.
- blame-history #5: the IRF no-move builders compare a fresh build with a file built moments earlier — reject, planned: D-054 states the lock is no-regression, not an oracle.
- blame-history #6: `rebuild_stems` reset to "pid5irf" — reject, planned: the script header says the settings record the last build.
- blame-history #7: check-script coverage changes — reject, false: the reviewer found no guard dropped.
- blame-history #8: the "37 staged" comment — reject, false: the count is correct.
- prior-review #1: no milestone Decisions entry for the new form text (M158 blame-history #3) — fix now, fixed 5494cc29.
- prior-review #2: the notice is checked only by substring and the test reads the generator's own object (LESSONS M159) — fix now (a notice typed from the PDF, asserted equal to the stored one and found in both footers), fixed 5494cc29.
- prior-review #3: the "Form text" row's footer and version facts — fix now, same as blame-history #1, fixed 5494cc29.
- prior-review #4: no `\value` on the three new help pages — follow-up, already in the "Clinical reporting & release" row's family-wide note.
- prior-review #5: short wrapped lines in a test header comment — reject, style.
- Fix verification (2026-10-04, head `5494cc29`): `test-generate-pid5irf.R` 547 passes, 0 failures; the four IRF files rebuilt from main's manifest (52 rows, 4 IRF); `devtools::check()` 0 errors, 0 warnings, 0 notes (full suite); `check_pkgdown()` no problems; `cairn_validate.py` passes.
- Evidence refresh AC1 to AC3 (2026-10-04, head `928c34ba`, after the fix-now rebuild): `test-generate-pid5irf.R` 8 tests, 547 passes, 0 failures. The Word instruction paragraph is now the stored `start`, and the stored prompt and stem form a repeated header row of the item table (asserted). The Qualtrics and REDCap files restate prompt and stem at each of the 14 later pages (asserted). The rebuilt US and A4 files parse to 218 rows, 25 facets, 14 R marks and the APA footer. The Qualtrics file has 218 `PID5IRF_` IDs with `TextIRF` text, and the REDCap file has 219 rows with `pid5irf_` fields. The 4 manifest rows (52 in all) match the rebuilt files' md5 sums, and the staged copies are identical. `test-artifacts.R` 171 and `test-download-pages.R` 136 passes, 0 failures.
