<!-- Section ownership + write-modes: see tracking-rules.md "Milestone-file
     section ownership". A phase skill never rewrites another phase's section.
     Per-section owners are tagged below. The one size check that can fail is
     cairn_validate's <150 over the plan-owned body. -->
# M169: Spreadsheet formula caveat for response files

- **Status:** review
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

- [x] AC1: In hitop-form's `README.md`, three sections each state two facts. The sections are "Where the file lands" (the file the page saves with no store), "Send responses to a Google Sheet" (the `webhook` kind, for the sheet's CSV download) and "Send responses to Supabase" (the `supabase` kind, for the Table Editor export). These are no store plus the two kinds that `STORE_KINDS` in `form.js` lists. The first fact: a participant identifier or text answer that starts with `=`, such as `=1+1`, can be read as a formula when the file is opened in a spreadsheet program. The second fact: `read_form_responses()` returns it as text.
- [x] AC2: `vignettes/articles/online-collection.Rmd` states the same two facts in "3. Download the sheet as CSV" and in "The Supabase route".
- [x] AC3: In hitop-form's `README.md` and in `vignettes/articles/online-collection.Rmd`, every sentence outside fenced code that contains `formula` or `=1+1` (case-insensitive) and says a value stays text or is not read as a formula limits that claim to the cell in the sheet or table itself.
- [x] AC4: `read_form_responses()` returns a participant identifier of `=1+1` and a `q_` answer of `=1+1` as the character value `"=1+1"`, from a Google Sheet download and from a Supabase export. A test asserts each of the four cases.

## Coverage

- AC1 → T3
- AC2 → T4
- AC3 → T3, T4, T5
- AC4 → T2

## Tasks

- [x] T1: Cut `m169-formula-caveat` in hitop and in the hitop-form checkout (`/Users/jmgirard/github/hitop-form`). Record a milestone-local decision: the M095 review's rejection of formula escaping (O15) stands, with the added reason that a direct post bypasses any page-side escape.
- [x] T2: In `tests/testthat/test-read_form_responses.R`, add the four AC4 cases. Build the files from `inst/examples/responses-sheet-hitopbr.csv` and `tests/testthat/fixtures/supabase-hitopbr.csv`. Add a `q_` column to each, as the article's chunk near `online-collection.Rmd:184` does. Assert with `expect_identical()` against the typed literal. Plant a reader change that alters a leading `=` and see the new tests go red, then restore it.
- [x] T3: In hitop-form's `README.md`, write the two facts in the three AC1 sections. If the Supabase section (near line 1076) already states both, log it as a no-op. Limit the Google Sheet sentence near line 910 to the sheet itself (AC3).
- [x] T4: In the article, write the two facts in "3. Download the sheet as CSV" and "The Supabase route". Limit the sentence in "1. Deploy the sheet's script" (near line 50) to the sheet itself, and judge the sentence near line 147 (AC3).
- [x] T5: Run `grep -n -i -E 'formula|=1\+1'` over both files. Judge each whole sentence outside fenced code and record the ledger in the work log (AC3). Run `devtools::test(filter = "read_form_responses")` and `pkgdown::check_pkgdown()`. Render the article after `devtools::load_all()`.
- [x] T6: Add a NEWS.md entry under "Documentation and website" for the article's formula caveat (review return 1).

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
- 2026-10-06: T4 done. The article gained the caveat after "3. Download the sheet as CSV" and in "The Supabase route". The deploy-step sentence now says "in the sheet itself", and the sentence in "4. Read the file" now says the codes "come back as text, as the sheet itself stored them".
- 2026-10-06: T5 done. Sweep ledger (awk over lines outside fenced code that match `formula` or `=1+1`). README 864-867 caveat, 915-917 limited to the sheet itself, 1012-1015 limited to the sheet, 1087-1092 limited to the table itself. Article 49-51 limited to the sheet itself, 133-136 limited to the sheet, 152 names the sheet itself, 300-302 limited to the table itself. Article 51-54 ("put a formula in it") is about header keys, not values, so it is outside AC3. `check_pkgdown()`: no problems. The article rendered after `load_all()`, with one sheet caveat and one table caveat in the HTML.
- 2026-10-06: claim audit: 25 claims read, 1 corrected — tests/testthat/test-read_form_responses.R, vignettes/articles/online-collection.Rmd, hitop-form README.md
- 2026-10-06: the corrected claim, "You can also open the file in the spreadsheet as text" (README, Supabase section), was spreadsheet behavior no record covers. It was deleted (hitop-form 8d2af62), so no re-read was owed. Status set to review. The suite last ran clean after T2, and no R or test file changed after that.
- 2026-10-06: review return 1: consistency gate failed on NEWS. The profile requires a NEWS entry for user-visible changes, and NEWS.md has a "Documentation and website" section in the development version. The plan's "no NEWS entry" missed the article change. Add an entry there for the article's formula caveat.
- 2026-10-06: minor amendment: T6 added for review return 1. Done: NEWS.md gained an entry as the first bullet of the development version's "Documentation and website" section. Its `read_form_responses()` claim is held by the AC4 test. Status set to review.

## Decisions

- M169-D1 (2026-10-06): The M095 review's rejection of formula escaping (finding O15) stands. Its reason was that R reads the file and that a spreadsheet's cell execution is the spreadsheet's setting. This milestone adds a second reason. Anyone with a study link can post a row directly to the sheet script or the Supabase table. An escape written by the page therefore does not stop a planted formula. The page keeps writing answers unchanged, and the docs carry the caveat.

## Review

Pass 1 (2026-10-06), stopped at the step-4 gate before the reviewers ran. Criterion evidence passed but is not ticked, because the return re-runs it.
- AC1 to AC4: section greps found both facts in each of the 3 README and 2 article sections. The sweep found every value claim limited to the sheet or table itself. The AC4 test had 4 expectations and 0 failures (261 tests in the file, 0 failed).
- Gate: `cairn_validate` exit 0. `document()` no diff. README.md current. `check_pkgdown()` no problems. `check()` 0 errors, 0 warnings, 0 notes. NEWS: no entry for the article change, so this fails (review return 1).

Pass 2 (2026-10-06). hitop and hitop-form are both 0 commits behind origin/main.
- AC1: in hitop-form README.md, outside fenced code, each of "Where the file lands", "Send responses to a Google Sheet" and "Send responses to Supabase" has 1 match each for "participant identifier or a text answer", "can … read such a cell as a formula" and "`read_form_responses()` returns … as text". `STORE_KINDS` is `['webhook', 'supabase']` (form.js:1071). Pass.
- AC2: in the article, "3. Download the sheet as CSV" and "The Supabase route" each have 1 match for each of the same three phrases. Pass.
- AC3: the awk sweep (outside fenced code, `formula` or `=1+1`, case-insensitive) hit README lines 865, 866, 917, 1013, 1015, 1088 and 1090, and article lines 51, 54, 134, 136, 152, 301 and 302. Read as whole sentences, each value claim says "in the sheet itself", "in the sheet", "in the table itself" or "as the sheet itself stored them". The other hits are the caveat. Article 51-54 "put a formula in it" is about header keys, not values. Pass.
- AC4: `test_file("test-read_form_responses.R")` ran 261 tests with 0 failed. The test "a participant and an answer of =1+1 read as text from both store exports" ran 4 expectations (participant and `q_note`, for the sheet download and the Supabase export), with 0 failed. At implement, a planted reader change failed all 4 (T2 work-log line). Pass.
- Gate: `cairn_validate` exit 0. `document()` no diff. README.md current. `check_pkgdown()` no problems. NEWS.md has the entry (T6). `check()` 0 errors, 0 warnings, 0 notes. No principle changed, so `cairn_impact` is skipped.
- spawned: diff-bug, blame-history, prior-review
- diff-bug #1: the sheet half of the participant test changes nothing, because row 1 of the sheet download already holds `=1+1` — fix now. `formula_copy()` now sets row 2, which holds `007` and `p002`. Fixed 0c4d2889.
- diff-bug #2: "the sheet's protection" names nothing in the article — fix now. Both files now name the apostrophe, and the download "does not keep the apostrophe". Fixed 0c4d2889, hitop-form bd37f7a.
- diff-bug #3: the sheet sentences credit "as text" alone for stopping a formula, against the M111 lesson — fix now. Each now says the cell is written behind a leading apostrophe and formatted as text. Fixed 0c4d2889, hitop-form bd37f7a.
- diff-bug #4: "read the file in R before you open it in a spreadsheet" implies a later open is safe — fix now. It now says "rather than in a spreadsheet". Fixed hitop-form bd37f7a.
- diff-bug #5: the saved-file claim had only a one-off check — fix now. The test "a participant of =1+1 in the page's saved file reads as text" was added. Fixed 0c4d2889.
- diff-bug #6: the Supabase closing sentence repeats the R route — reject, style.
- diff-bug #7: two rewrapped article lines run past 80 columns — fix now, rewrapped. Fixed 0c4d2889.
- diff-bug #8: `study` and option labels can also start with `=` — reject, planned change. Both are researcher-set, and the plan scoped participant text.
- diff-bug #9: the NEWS entry fits the file — reject, false (no defect reported).
- blame-history #1: the dropped "open it in the spreadsheet as text" advice was M112's on purpose — follow-up, the new "Formula caveat reach" row. The claim audit found no record that checks it.
- blame-history #2: "in the sheet itself" narrows an earned claim accurately — reject, false (no defect reported).
- blame-history #3: long rewrapped lines — fix now, the same fix as diff-bug #7. Fixed 0c4d2889.
- blame-history #4: the Supabase paragraph agrees with "exactly as the sheet's download does" — reject, false (no defect reported).
- prior-review #1: M111's review lists `+`, `-` and `@` as formula prefixes, and the caveat names only `=` — follow-up, the new "Formula caveat reach" row. No run in this repo shows which prefixes each spreadsheet program runs.
- prior-review #2: the dropped open-as-text advice (M112 F18) — follow-up, the same row.
- prior-review #3: the apostrophe goes unnamed — fix now, the same fix as diff-bug #2. Fixed 0c4d2889, hitop-form bd37f7a.
- After the fixes: a re-planted reader change failed the export test on all 4 cases and the new page-file test on both. Suite: 1158 tests, 0 failed. `check_pkgdown()` no problems. The article rendered. The AC1 and AC2 phrase counts are unchanged (1 each in all 5 sections). The AC3 sweep lines all still limit the claim to the sheet or table. The step-6 checkpoint landed after the fix commits, not before them. That changes no content.
