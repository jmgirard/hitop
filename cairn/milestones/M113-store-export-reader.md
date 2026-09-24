# M113: The package reads a store's CSV export and an article walks the Google Sheet route end to end

- **Status:** review
- **Priority:** normal
- **Depends on:** M111, M112
- **Driving RR:** —
- **Principles touched:** GP3, IP4
- **Resolves:** —
- **Surface tier:** user-facing — an exported reader's behavior and a published article
- **Branch/PR:** `m113-store-export-reader`

## Goal

Make `read_form_responses()` read a file holding many participants' rows, and publish an article that walks a researcher from a study link through a Google Sheet to scored data.

## Scope

**In:** multi-row files in `read_form_responses()`, with the header-only refusal kept. The two store exports from M111 and M112 as fixtures, one of them also an installed example. A new site article under Tutorials, and the two existing hitop-form sections rewritten to match the page's behavior. The article's data-holding section states facts and characterizes no store as compliant (IP4).

**Out:** reading a Google Sheet by URL or a Supabase table by API → not planned; the CSV download is the retrieval. Condition classes for the reader's unclassed refusals → unchanged (D-064 stands). In-browser encryption → its candidate row.

## Acceptance criteria

- [ ] AC1: `read_form_responses()` reads a file holding N data rows as N rows of the result, N at least 1, in file order, and binds files in path order. A file with a header and no data row is refused with a message naming the file. Tests: a two-row file the test writes from one fixture's row twice with `participant` changed, the same file with LF line endings and no final newline, a header-only file, and a directory holding a one-row and a two-row file that yields three rows in path then file order.
- [ ] AC2: `inst/examples/responses-sheet-hitopbr.csv`, copied as LF from hitop-form's `tests/fixtures/sheet-hitopbr.csv` (M111) with a provenance row in `inst/examples/README.md`, reads into two rows: `participant` holds `=1+1` and `007` as character, `submitted` is a UTC date-time, and every item column is integer. `score_hitopbr()` on it equals means recomputed per row from the file's text with `hitopbr_items$Reverse` and `hitopbr_scales$itemNumbers`. hitop-form's `tests/fixtures/supabase-hitopbr.csv` (M112), copied under `tests/testthat/fixtures/` with a provenance row in `tests/testthat/fixtures/README.md`, reads into two rows the same way: `participant` holds `p001` and `p002` as character, `submitted` is a UTC date-time, and every item column is integer, and `score_hitopbr()` on it equals means recomputed per row the same way. One test per file.
- [ ] AC3: A new article `vignettes/articles/online-collection.Rmd` is listed under Tutorials in `_pkgdown.yml`. Its steps run in the order a researcher follows them: deploy the Apps Script from the hitop-form README, make the link, download the sheet as CSV, read it with `read_form_responses()`, score it. One section points to the Supabase route in the hitop-form README. One section states who holds the data: the store vendor holds the rows; the page's host receives the study link (study, participant code, store address and key) with each page load but no answers; participant codes are whatever the link or the participant supplies, so the researcher should issue pseudonymous codes in the link; agreements such as a BAA or DPA are the institution's. It cites no law and calls no store compliant. Its R chunks run on the installed package and read the example through `system.file()`. In the "Collecting Responses Online with hitop-form" section of `vignettes/articles/modules-hitopsr.Rmd` and of `vignettes/pid5_scoring.Rmd`, no sentence says the page always saves a file, that nothing is sent, or that the reader returns one row per file, and each section points to the article in one sentence.
- [ ] AC4: `?read_form_responses` documents multi-row files, and NEWS has an entry under "New features". `devtools::test()` is clean, `devtools::document()` makes no diff, `pkgdown::check_pkgdown()` is clean, and `devtools::check()` has 0 errors and 0 warnings.

## Coverage

- AC1 → T1
- AC2 → T2
- AC3 → T3
- AC4 → T4

## Tasks

- [x] T1: Replace the one-row check at `R/read_form_responses.R:190` with multi-row binding and the header-only refusal; the four tests of AC1.
- [x] T2: Copy the two store exports (LF), add the provenance rows, and write the two reading and scoring tests of AC2.
- [x] T3: Write the article, add its `_pkgdown.yml` row, rewrite the two existing sections and their pointers.
- [x] T4: Roxygen for `?read_form_responses`, the NEWS entry, `document()`, `test()`, `check_pkgdown()`, `check()`.

## Work log

- 2026-09-23: created by /milestone-plan. Promoted from the online-form candidate row (lineage M093, M096, M099); planned with M111 and M112.
- 2026-09-23: criteria audit ran in full mode on a fresh [O] reader (shared with M111): findings on this file were the two-row file that no two fixtures can form, the missing LF and no-final-newline variant, the article's step order, "code chunks" covering JavaScript, and the unnamed vignette sentences. All fixed in the wording; none posed as a question.
- 2026-09-23: plan gate chose a new site article over an installed vignette or extending the two existing sections because the route spans three tools and both existing sections are instrument-specific; falsified by readers asking where the walkthrough is from the installed vignettes.
- 2026-09-23: plan chose multi-row reading inside `read_form_responses()` over a second reader because a store export has the page's columns and differs only in row count; falsified by an export whose columns differ from the page's.
- 2026-09-23: re-audit by the same fresh [O] reader: findings on this file were line-numbered sentence ranges that miss three further sentences and drift, and two false statements in the data-holding section (the host receives the link contents; a participant-typed code is not a pseudonym). Both fixed in the wording after the plan commit.
- 2026-09-23: /milestone-implement started; branch `m113-store-export-reader` cut from `main` at `3d7a0fd7`; question gate skipped, no plan choice open. T1 done: `read_form_response_file()` refuses a header-only file and reads N rows, checks run down each column; the old two-row refusal test replaced by the four AC1 tests plus a bad-value-on-row-2 test; suite 19759 passing.

- 2026-09-23: amendment (substantive, mini gate): AC2 said the Supabase export reads into one row; the file hitop-form committed holds two (p001, p002). Jeff chose amending AC2 to two rows over truncating the copy. Wording written after the re-audit below; the Coverage map is unchanged.
- 2026-09-23: re-audit: AC2 (full) — two findings on the gate's wording, both fixed: the two scoring sentences made symmetric ("per row", the two tables), and the Supabase provenance file named.
- 2026-09-23: re-audit: AC2 (full) — two findings on the fixed wording, not applied under the two-line stop, for Jeff: "`score_hitopbr()` on it" omits the `items` and `append = FALSE` the equality depends on; "One test per file" binds the test layout, not the deliverable.

- 2026-09-23: T2 done: both exports copied from hitop-form `c15e149` with CR bytes removed and nothing else changed, provenance rows in both READMEs, one test per file through a shared `expect_hitopbr_export()` helper recomputing means per row; reader tests 175 passing, line-ending check clean.
- 2026-09-23: T3 done: `vignettes/articles/online-collection.Rmd` written in the researcher's step order with the Supabase and data-holding sections, listed under Tutorials after the import article; both existing sections rewritten and pointed at it; the installed vignette links by the site's absolute URL as `hitopsr_scoring.Rmd` does. `check_pkgdown()` clean; the three pages render on the installed package.

- 2026-09-23: T4 done: roxygen and a stale comment updated for multi-row files, NEWS bullet under "New features"; `document()` no diff, `test()` 19787 passing, `check_pkgdown()` clean, `check()` 0 errors 0 warnings 0 notes (4m 9s).
- 2026-09-23: claim audit: 46 claims read, 4 corrected — R/read_form_responses.R, NEWS.md, vignettes/articles/online-collection.Rmd, vignettes/articles/modules-hitopsr.Rmd, vignettes/pid5_scoring.Rmd, inst/examples/README.md, tests/testthat/fixtures/README.md. Corrections: the Apps Script's key cap and formula reason, the descriptor as an addition to the instrument choice, "one or more" rows in NEWS, the Supabase table's SQL pinned to hitop-form `1e472ba`. Two gaps the reader noted and left were closed by the author: the fallback file and the link's instrument and module in the data-holding section.
- 2026-09-23: all tasks checked; status set to review. The T3 tick landed in the T2 checkpoint commit by the author's oversight; the code for T3 is in its own commit.
- 2026-09-23: resume by /milestone-implement: `main` had moved (M114 merged), so it was merged into the branch at `36fce0a9`, the ROADMAP conflict resolved to M113 review and M114 done. Re-verify on the merged tree: `document()` no diff, line-ending check clean, `test()` 0 failures. Status stays review.

## Decisions

## Review
