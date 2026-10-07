<!-- Section ownership + write-modes: see tracking-rules.md "Milestone-file
     section ownership". A phase skill never rewrites another phase's section.
     Per-section owners are tagged below. The one size check that can fail is
     cairn_validate's <150 over the plan-owned body. -->
# M169: Spreadsheet formula caveat for response files

- **Status:** in-progress
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** —
- **Resolves:** —
- **Surface tier:** user-facing — researcher-facing setup docs in hitop-form's README and the package article
- **Branch/PR:** m169-formula-caveat; companion: /Users/jmgirard/github/hitop-form m169-formula-caveat

## Goal

A researcher who uses hitop-form learns two facts: a spreadsheet program can run a participant's text in a response file as a formula, and `read_form_responses()` keeps it as text.

## Scope

**In:** The formula caveat in hitop-form's `README.md` for the saved file, the Google Sheet download and the Supabase export. The same caveat in `vignettes/articles/online-collection.Rmd`. Each sentence that says a value is not read as a formula is limited to the cell in the sheet or table itself. Tests that `read_form_responses()` returns such text unchanged from both export types.

**Out:**
- An escape that the page writes and the reader removes: rejected at this plan gate, because anyone with a study link can post a row directly (work log).
- A caveat for question text in the questions file, which is the researcher's own text: left out at the plan gate. It stays in the "hitop-form question gaps" candidate row.
- The other gaps in that row (size limits, screen-reader state, digits, Excel re-save, locale, quick loads): they stay in the row.

## Acceptance criteria

- [ ] AC1: In hitop-form's `README.md`, three sections each state two facts. The sections are "Where the file lands" (the file the page saves with no store), "Send responses to a Google Sheet" (the `webhook` kind, for the sheet's CSV download) and "Send responses to Supabase" (the `supabase` kind, for the Table Editor export). These are no store plus the two kinds that `STORE_KINDS` in `form.js` lists. The first fact: a participant identifier or text answer that starts with `=`, such as `=1+1`, can be read as a formula when the file is opened in a spreadsheet program. The second fact: `read_form_responses()` returns it as text.
- [ ] AC2: `vignettes/articles/online-collection.Rmd` states the same two facts in "3. Download the sheet as CSV" and in "The Supabase route".
- [ ] AC3: In hitop-form's `README.md` and in `vignettes/articles/online-collection.Rmd`, every sentence outside fenced code that contains `formula` or `=1+1` (case-insensitive) and says a value stays text or is not read as a formula limits that claim to the cell in the sheet or table itself.
- [ ] AC4: `read_form_responses()` returns a participant identifier of `=1+1` and a `q_` answer of `=1+1` as the character value `"=1+1"`, from a Google Sheet download and from a Supabase export. A test asserts each of the four cases.

## Coverage

- AC1 → T3
- AC2 → T4
- AC3 → T3, T4, T5
- AC4 → T2

## Tasks

- [x] T1: Cut `m169-formula-caveat` in hitop and in the hitop-form checkout (`/Users/jmgirard/github/hitop-form`). Record a milestone-local decision: the M095 review's rejection of formula escaping (O15) stands, with the added reason that a direct post bypasses any page-side escape.
- [x] T2: In `tests/testthat/test-read_form_responses.R`, add the four AC4 cases. Build the files from `inst/examples/responses-sheet-hitopbr.csv` and `tests/testthat/fixtures/supabase-hitopbr.csv`. Add a `q_` column to each, as the article's chunk near `online-collection.Rmd:184` does. Assert with `expect_identical()` against the typed literal. Plant a reader change that alters a leading `=` and see the new tests go red, then restore it.
- [x] T3: In hitop-form's `README.md`, write the two facts in the three AC1 sections. If the Supabase section (near line 1076) already states both, log it as a no-op. Limit the Google Sheet sentence near line 910 to the sheet itself (AC3).
- [ ] T4: In the article, write the two facts in "3. Download the sheet as CSV" and "The Supabase route". Limit the sentence in "1. Deploy the sheet's script" (near line 50) to the sheet itself, and judge the sentence near line 147 (AC3).
- [ ] T5: Run `grep -n -i -E 'formula|=1\+1'` over both files. Judge each whole sentence outside fenced code and record the ledger in the work log (AC3). Run `devtools::test(filter = "read_form_responses")` and `pkgdown::check_pkgdown()`. Render the article after `devtools::load_all()`.

## Work log

- 2026-10-06: created by /milestone-plan. Lineage: the formula clause of the "hitop-form question gaps" candidate row (M137, M138 reviews), promoted at the 2026-10-06 triage. The row graduates only in part at post-merge hygiene, and its other gaps stay.
- 2026-10-06: collision sweep. The M095 review rejected O15, "CSV formula injection through a study or participant string", because "the file is read by R (M096), and a spreadsheet's cell execution is the spreadsheet's setting". This plan keeps that stance. M111 added the sheet script's apostrophe, and M112 added the Supabase caveat. No D-entry rejects a caveat. Inbox: 1 open issue (#87, no overlap), no outside PRs.
- 2026-10-06: question set: how to handle answers that start with "=" — Docs and tests. A caveat for the questions file — Leave it out.
- 2026-10-06: plan gate chose docs and tests over a page-side escape that the reader removes. Anyone with a study link can post a row directly, so the escape is bypassable. It also changes the file format in both repos. Falsified by a store that accepts rows only from the page, or by a report of an honest participant's answer that ran as a formula.
- 2026-10-06: criteria audit (full mode, fresh Opus reader) returned 5 findings, all applied. AC1 is narrowed to identifiers and text answers, because D-064 refuses item cells that are not whole numbers. The test reference moved from AC1 to T2. AC3 judges whole sentences that contain `formula` or `=1+1`, not lines that contain `formula`. AC4 is new, because only the participant cases had tests for the two exports. The Supabase section can already satisfy AC1 (T3 logs a no-op).
- 2026-10-06: lessons applied: M111 (a sheet's text format alone does not stop a formula, the apostrophe does) limits the sheet claim. M096 (an article renders against the installed package) puts `load_all()` before the render in T5. M116 gives the companion merge spelling. No NEWS entry, because no exported behavior changes.
- 2026-10-06: implement started. Branch `m169-formula-caveat` cut in hitop and in the hitop-form checkout, both from a synced main. Five untracked `devel/hitopdat_*` files in hitop belong to the blocked HiTOP-DAT work and stay unstaged.
- 2026-10-06: T1 done: decision M169-D1 recorded.
- 2026-10-06: T2 done. `formula_copy()` and one test in `test-read_form_responses.R` set row 1's participant to `=1+1` and add a `q_note` of `=1+1` to the sheet download and the Supabase export. A planted `sub("^=", "'=", ...)` over the reader's character columns failed the new test on all 4 cases, and also 2 older tests. Restored. Suite: 1157 tests, 0 failed, 15 skipped.
- 2026-10-06: T3 done (hitop-form d2f4dbe). The Supabase section named only a participant code and not `read_form_responses()`, so it was edited, not logged as a no-op. Before writing the page-file claim, I observed `read_form_responses()` on a copy of `tests/fixtures/responses-hitopbr-questions.csv` with participant and `q_note` set to `=1+1`: both came back `"=1+1"`. No hitop-form spec reads the changed prose. The local Playwright run is skipped because only README prose changed, and hitop-form CI runs at review.

## Decisions

- M169-D1 (2026-10-06): The M095 review's rejection of formula escaping (finding O15) stands. Its reason was that R reads the file and that a spreadsheet's cell execution is the spreadsheet's setting. This milestone adds a second reason. Anyone with a study link can post a row directly to the sheet script or the Supabase table. An escape written by the page therefore does not stop a planted formula. The page keeps writing answers unchanged, and the docs carry the caveat.

## Review
