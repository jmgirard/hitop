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

- [x] AC1: `read_form_responses()` reads a file holding N data rows as N rows of the result, N at least 1, in file order, and binds files in path order. A file with a header and no data row is refused with a message naming the file. Tests: a two-row file the test writes from one fixture's row twice with `participant` changed, the same file with LF line endings and no final newline, a header-only file, and a directory holding a one-row and a two-row file that yields three rows in path then file order.
- [x] AC2: `inst/examples/responses-sheet-hitopbr.csv`, copied as LF from hitop-form's `tests/fixtures/sheet-hitopbr.csv` (M111) with a provenance row in `inst/examples/README.md`, reads into two rows: `participant` holds `=1+1` and `007` as character, `submitted` is a UTC date-time, and every item column is integer. `score_hitopbr()` on it equals means recomputed per row from the file's text with `hitopbr_items$Reverse` and `hitopbr_scales$itemNumbers`. hitop-form's `tests/fixtures/supabase-hitopbr.csv` (M112), copied under `tests/testthat/fixtures/` with a provenance row in `tests/testthat/fixtures/README.md`, reads into two rows the same way: `participant` holds `p001` and `p002` as character, `submitted` is a UTC date-time, and every item column is integer, and `score_hitopbr()` on it equals means recomputed per row the same way. One test per file.
- [x] AC3: A new article `vignettes/articles/online-collection.Rmd` is listed under Tutorials in `_pkgdown.yml`. Its steps run in the order a researcher follows them: deploy the Apps Script from the hitop-form README, make the link, download the sheet as CSV, read it with `read_form_responses()`, score it. One section points to the Supabase route in the hitop-form README. One section states who holds the data: the store vendor holds the rows; the page's host receives the study link (study, participant code, store address and key) with each page load but no answers; participant codes are whatever the link or the participant supplies, so the researcher should issue pseudonymous codes in the link; agreements such as a BAA or DPA are the institution's. It cites no law and calls no store compliant. Its R chunks run on the installed package and read the example through `system.file()`. In the "Collecting Responses Online with hitop-form" section of `vignettes/articles/modules-hitopsr.Rmd` and of `vignettes/pid5_scoring.Rmd`, no sentence says the page always saves a file, that nothing is sent, or that the reader returns one row per file, and each section points to the article in one sentence.
- [x] AC4: `?read_form_responses` documents multi-row files, and NEWS has an entry under "New features". `devtools::test()` is clean, `devtools::document()` makes no diff, `pkgdown::check_pkgdown()` is clean, and `devtools::check()` has 0 errors and 0 warnings.

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

Evidence gathered 2026-09-23 on branch head `f3f3416c` (main `f12b54cd` merged in).

- AC1: `devtools::test(filter = "read_form_responses")` passes, 0 failures. The four named tests exist and ran: "a file holding two response rows reads as two rows, in file order" (test-read_form_responses.R:347, two rows from one fixture row with `participant` changed, p002 before p001 as written), "a two-row file with LF endings and no final newline reads the same" (:370, `eol = "\n"`, `final = FALSE`, no warning), "a header-only file is refused by name" (:283, message names `empty.csv` and "no response row", neither public class), "a directory of a one-row and a two-row file yields three rows in path then file order" (:385, p003, p001, p002). Evidence recorded; box ticked.
- AC2: `cmp` shows `inst/examples/responses-sheet-hitopbr.csv` equals hitop-form's `sheet-hitopbr.csv` with CR bytes removed, and `tests/testthat/fixtures/supabase-hitopbr.csv` equals `supabase-hitopbr.csv` the same way; `file` reports both as LF text. One provenance row each in `inst/examples/README.md` and `tests/testthat/fixtures/README.md`. One test per file (:567, :576) through `expect_hitopbr_export()`, which asserts two rows, `participant` character (`=1+1`, `007`; `p001`, `p002`), `submitted` POSIXct in UTC, every item column integer, and `score_hitopbr()` equal to per-row means recomputed from the file's text with `hitopbr_items$Reverse` and `hitopbr_scales$itemNumbers`. Both pass in the run above. Box ticked.
- AC3: `vignettes/articles/online-collection.Rmd` is at `_pkgdown.yml:47` under Tutorials. Its headings run 1 Deploy the sheet's script, 2 Make the study link, 3 Download the sheet as CSV, 4 Read the file, 5 Score it, then The Supabase route (links the hitop-form README section) and Who holds the data (vendor holds the rows; host receives the link with instrument, module, study, code, address and key, no answers; codes are whatever the link or participant supplies, issue pseudonymous codes; BAA or DPA is the institution's). `grep -i 'compliant|HIPAA|GDPR|FERPA|law'` on the article returns nothing. Its two R chunks read the example through `system.file()`; `R CMD INSTALL` of the branch then `rmarkdown::render()` in a scratch directory produced the page with the `hbr_*` score columns. In both rewritten sections, `grep -i 'always saves|nothing is sent|no answer is sent|one row per file|sent anywhere'` returns nothing, and each section points to the article in one sentence (modules-hitopsr.Rmd:411, pid5_scoring.Rmd:198). `pkgdown::check_pkgdown()`: no problems. Box ticked.
- AC4: `man/read_form_responses.Rd` lines 17, 25, 31-32 document the store download and one row per response row. NEWS.md:23 is under "## New features". `devtools::document()` produced no diff. `devtools::check()`: 0 errors, 0 warnings, 0 notes, tests 204 s OK, duration 4 m 33 s. `devtools::test()` on this tree: 0 failures. Box ticked.

**Consistency gate.** `cairn_validate.py`: all checks passed, exit 0, 24 advisory warnings (pre-existing D-entry references and one references-staleness line). No DESIGN principle changed, so `cairn_impact` was skipped. `document()` no diff; README.Rmd and README.md unchanged on the branch; no new top-level file; `check()` 0 notes; NEWS entry present.

**Independent review** (three fresh-context lenses, 2026-09-23). Findings ranked as reported, with the disposition proposed at the gate:

- [O]1 online-collection.Rmd:45-46, 75-77 — the article does not say to deploy one sheet per form; the Apps Script adds keys the header lacks, so two forms sent to one sheet give one merged download that the reader returns with no error, the other form's items `NA`. Verified by the reviewer against hitop-form `5702a3a:README.md` and a mixed hitopbr/pid5bf file. Proposed: fix now (one sentence in step 1).
- [O]2 R/read_form_responses.R:160-168 — `read.csv`'s `fill = TRUE` pads a short data row with blanks, which read as `NA` items with no error. Pre-existing; the multi-row change is where it starts to matter. Proposed: follow-up candidate row.
- [O]3 R/read_form_responses.R:160-168, 254-265 — a row with an extra field wraps onto a new row, and the refusal names `form_build` with `Got ""` rather than the real problem. Proposed: the same candidate row.
- [O]4 R/read_form_responses.R:219-242, 256-262 — item and stamp refusals name the column or value but not the row. Proposed: the same candidate row.
- [O]5 online-collection.Rmd:133-134 — "and nowhere else" leaves out the default route (no store, a file saved) and conflicts with line 69, where a late row lands in both. Proposed: fix now.
- [O]6 R/read_form_responses.R:160 — a zero-byte `.csv` gives base R's "no lines available in input" without the file name. Same on main. Proposed: the same candidate row.
- [O]7 test-read_form_responses.R:295-315 — the later-row test does not assert the file name in the message, and no test puts a bad `form_build` or an out-of-range value on row 2 (the reviewer checked by hand that a bad `form_build` on row 2 is refused). Proposed: fix now.
- [O]8 the multi-row tests — every row repeats the fixture's items with only `participant` changed, so a bug that recycled item values between rows would pass. AC1 requires this construction. Proposed: reject, a limit of the test the criterion set.
- [O]9 pid5_scoring.Rmd:204 — "one row per participant row" is awkward. Proposed: fix now.
- [O]10 and [S-blame]1 R/read_form_responses.R:18, 29 — two roxygen lines run past the block's wrap width. Cosmetic. Proposed: fix now.
- [S-prior]: no regression of a prior finding (archives M096, M099, M111, M112 read; the PR-comment probe returned an empty array).
