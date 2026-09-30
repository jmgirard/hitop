# M146: Study Link Builder: required choices first, optional ones folded away

- **Status:** review
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** GP3, IP1
- **Resolves:** —
- **Surface tier:** user-facing — the researcher page that makes study links, and the package tutorials that describe it
- **Branch/PR:** m146-study-link-builder-new-user, companion: /Users/jmgirard/github/hitop-form m146-study-link-builder-new-user

## Goal

A researcher who opens the Study Link Builder for the first time sees the required choices first. The optional features sit in named sections, the hints are short, and the finished link comes with its next step.

## Scope

**In:** In hitop-form `link.html`, the milestone changes four things. The page gets a required block and five closed optional sections. The instrument-row and question-group buttons get correct states. The result region is rebuilt. Every hint, intro and refusal text takes the D-083 names. Long hint detail moves to hitop-form `README.md` sections that the hints link to. The `form.js` messages that the Study Link Builder shows also take the D-083 names. In hitop, the D-083 names reach `vignettes/articles/online-collection.Rmd`, `vignettes/articles/modules-hitopsr.Rmd`, `README.Rmd` and the `_pkgdown.yml` Instruments menu. Instrument text, item counts and instrument names stay as they are (IP1).

**Out:**
- M147 takes the Module Builder, the hand-off between the pages, and the module file picker.
- M148 takes the participant form page.
- Focus on the refused field for refusals outside the AC1 set joins the `z` link gaps candidate row.
- The R help pages and the `descriptor` argument keep their names (D-083).
- The `z` link gaps candidate row keeps its other gaps. Its do-nothing Move buttons and their accessible names come in here as AC3.

## Acceptance criteria

- [x] AC1: Open `link.html` with no address parameters. The top-level parts come in this order: instruments, study name, where responses go, the five optional sections, and the "Make the link" button. The optional sections are "Participants and recruiting site", "Item order and HiTOP-SR module", "Consent", "When the participant finishes" and "Your own questions". Each is a closed `<details>` element. Its summary line reads "Not used", or it lists the visible labels of its fields that hold a value, joined by ", ". The test makes one refusal in each optional section. Each time, "Make the link" opens that section and moves focus to the refused field. The page is checked at 375px and at 1280px wide with every section open. No element is wider than the page. A Playwright test asserts each fact.
- [x] AC2: Open `link.html` with a study link that sets one optional field. The section that holds the field opens, and its summary lists that field's label. The other optional sections stay closed. A Playwright test runs this once for each optional field alone. It uses a `z` link for the consent fields and the questions. A link that sets no optional field leaves every optional section closed.
- [x] AC3: This criterion covers the instrument rows and the question groups. "Move up" is disabled on the first row, and "Move down" is disabled on the last row. On a single instrument row, all three buttons are disabled. The accessible name of each button begins with its visible text. A Playwright test asserts the state at each position for 1, 2 and 3 instrument rows. It does the same for 1, 2 and 3 question groups. It asserts the state again after a move, a removal, a prefill from a study link, and a questions file load.
- [x] AC4: After a successful build, a region headed "Your study link" shows the link in a box that scrolls. A "Copy the link" button sits beside the box. Below it, one next-step sentence fits the choices made. With a recruiting site, the sentence says to paste the link into that site's study page. With a Supabase table, it says to run the SQL first. Otherwise it says to open the link once to test it, and then to give it to each participant. Focus moves to the region's heading. A Playwright test asserts each part for each recruiting-site option and each where-responses-go option. It also builds a `z` link over 5,000 characters, and asserts that the box stays under 16rem high.
- [x] AC5: A Playwright test opens every optional section and adds one question of each type. It then chooses each recruiting-site option and each where-responses-go option in turn, and makes one Supabase build. In each of these states, it checks every `.hint` and `.site-hint` element in the page, shown or not. Each holds at most 40 words. The intro is all text between the `<h1>` and the first form part. It holds at most 60 words. The page's text, its `placeholder` values and its `aria-label` values hold none of the D-083 retired terms. The text of a built link is exempt. The test counts words by splitting on whitespace.
- [x] AC6: A case-insensitive grep runs for each D-083 retired-term pattern. In hitop-form it reads `link.html`, `form.js` and `README.md`. In hitop it reads the two articles, `README.Rmd` and `_pkgdown.yml`. Each remaining hit is one of these: a code identifier, a comment, R code, an R function or argument name, or a literal URL or URL example. The Instruments menu names the two pages "Module Builder" and "Study Link Builder".
- [ ] AC7: The hitop-form Playwright suite passes locally and on its PR's CI. In hitop, `devtools::check()` gives 0 errors and 0 warnings, and `pkgdown::check_pkgdown()` passes. Both articles build with `pkgdown::build_article()`. The navbar test passes. `README.md` equals a fresh `devtools::build_readme()`.

## Coverage

- AC1 → T1, T5
- AC2 → T1, T5
- AC3 → T2, T5
- AC4 → T3, T5, T13
- AC5 → T4, T5, T15
- AC6 → T4, T6, T7, T8
- AC7 → T5, T6, T7, T11

## Tasks

- [x] T1: In `link.html` (fields at lines 105-213), move participant, recruiting site and module into their sections. Put the completion and decline addresses with their fields. Wrap each optional section in `<details>`, with a summary that updates on input. If `prefill()` (lines 646-781) gives a field a value, open the field's section. In the build handler (lines 790-1040), open the section and focus the field for a refused optional field.
- [x] T2: Set the disabled state of Move up, Move down and Remove in `renumber()` (lines 292-369) and in the question groups (lines 433-509). Update it after every change to the rows. Make each accessible name begin with the visible text.
- [x] T3: Build the "Your study link" region (lines 213-227, 1041-1069). It holds the heading, the scrolling box, "Copy the link" beside the box, the SQL block and the next-step sentence. Focus moves to the heading.
- [x] T4: Rewrite the intro, step list, labels, hints, placeholders and refusal texts in `link.html` under the D-083 names and the word limits. Do the same for the `form.js` messages that the page shows. Move the cut detail into `README.md` sections, and link each shortened hint to its section.
- [x] T5: Write the Playwright tests for AC1 to AC5 in a new spec file. Update the existing specs that assert old text. Run the suite locally and on the PR.
- [x] T6: In hitop, apply the D-083 names to the two articles, `README.Rmd` and the `_pkgdown.yml` menu. Rebuild `README.md` and build both articles. Run `check()` and `check_pkgdown()`.
- [x] T7: Run the AC6 greps, and classify each remaining hit in a work-log ledger for review to re-run. Take before and after screenshots at 375px and 1280px, with the sections closed and open, for Jeff's look at the merge gate.
- [x] T8: Rename hitop-form `tests/instruments-store.spec.js` to `tests/instruments-supabase.spec.js`, and update the README row and every other reference.
- [x] T9: Put back the facts the rewrite dropped, each asserted by a test. They are the host note in the intro, and the SONA token and `XXXX` sentence. They are also the declined-text, decline-URL and saved-file URL rules, and the Prolific URL-parameters caveat.
- [x] T10: Make the smaller fixes chosen at the return gate:
  - "the online form" where a hint means the participant's page, "unpack" in the builder-shown refusals, and the README opening name
  - an anchored Supabase focus pattern, the same trim in the summary and the build, and a result region that a field change hides
  - README hint links that open a new tab
  - the README refusal-focus claim, the Supabase next-step sentence with no site, "HiTOP" in the tab title, and the Google Sheet wording in `modules-hitopsr.Rmd`
- [x] T11: Add the NEWS.md entry for the menu names, the reworked Study Link Builder and the article names.
- [x] T12: Fix the quote in DESIGN Known issue 11. Add a candidate row for old names outside the scope, and add the test-reach gaps to the `z` link gaps row.
- [x] T13: Make the next step with Supabase and no site one sentence. Make S7 check the sentence count by its own rule, not by a copy of the page's text.
- [x] T14: If a field changes while a build waits, the build does not show its link and SQL. Test it with an edit during a slowed export fetch.
- [x] T15: Put back the hint facts of pass-2 findings 3, 4, 6, 9, 10 and 11, each asserted by S9 and each hint within 40 words.
- [x] T16: Make the small fixes of pass-2 findings 14, 15, 18, 19 and 23. File findings 7, 12, 13, 16, 17, 21, 24 and 25 in their rows.

## Work log

- 2026-09-29: created by /milestone-plan, with M147 and M148. The criteria audit ran in full mode with a fresh [O] reader. It returned about 25 findings over M146 and M147, and each was repaired as suggested. The hint limit now counts `.site-hint` and question-group hints. The greps use word-boundary patterns and allow URL examples. The next-step sentence follows the site and the destination. The section name avoids the questions menu's "After the form".
- 2026-09-29: plan gate chose two linked pages with a better hand-off (M147) over a scale list inside the Study Link Builder. That list needs scale membership in the JSON export, a format change under D-063. Falsified by researchers who still paste or lose the module file after M147 ships.
- 2026-09-29: plan gate chose two linked pages over one merged page, for M125's reasons. With one page, every link waits on the R load, and the other four instruments move into a HiTOP-SR page. Falsified by a report that the pages still read as disjointed after M146 and M147.
- 2026-09-29: plan gate chose automated checks, screenshots and Jeff's look at the merge gate over a fresh new-user walkthrough at review. Falsified by a new-user problem that Jeff finds after merge and that the checks passed.
- 2026-09-29: implement started. Branches cut in hitop and hitop-form. Question gate: the questions section's summary lists each question's legend ("Question 1, Question 2"), as AC1 reads, with no amendment. Hitop-form baseline: 720 passed.
- 2026-09-29: T1 done in hitop-form. The page has a required block and five closed sections with live summaries. Prefill opens a filled section, and a field refusal opens its section and focuses the field. New `tests/link-sections.spec.js` (22 tests) covers AC1 and AC2. Four planted defects turned 20 of them red. Old link specs open every section through a new `openBuilderSections()` helper. Full suite: 736 of 742 passed, and 6 timeouts at one moment passed on re-run.
- 2026-09-29: T2 done. Move up and Move down are disabled at the ends, Remove on a single instrument row, and names read "Move up instrument 2". If the pressed move button becomes disabled, focus moves to the other one. Two AC3 tests added, and two planted defects turned both red. Full suite: 744 passed.
- 2026-09-29: T3 done. After a build, a "Your study link" region shows the link in a scrolling box. "Copy the link" sits beside it, and one next-step sentence sits below it. Focus moves to its heading. Sixteen AC4 tests added (5 sites by 3 destinations, and one long `z` link). Three planted defects (no focus, no box height, one sentence for all) each turned tests red. Full suite: 760 passed.
- 2026-09-29: T4 done. Hints, intro and refusals in `link.html` and the messages in `form.js` take the D-083 names. The steps list is gone, and cut detail moved to new README sections that the hints link to. Two AC5 tests added (word limits and retired terms, and README anchors). Five planted defects turned them red. A Sonnet subagent updated the old message strings in 9 test files, and its diff was checked here. Full suite: 762 passed.
- 2026-09-29: T5 done. `tests/link-sections.spec.js` (42 tests, AC1 to AC5) grew with T1 to T4, and the old specs were updated. The local suite passed with 762. The PR's CI run comes at review, once `/milestone-review` opens the PR.
- 2026-09-29: T6 done. A Sonnet subagent renamed the retired terms in the two articles (46 prose sites), and its diff was checked here. The Instruments menu now reads "Module Builder" and "Study Link Builder", with the navbar test updated first and seen red. `check()`: 0 errors, 0 warnings, 0 notes. `check_pkgdown()` passes, both articles build, and `README.md` equals a fresh build.
- 2026-09-29: T7 wording amended (minor): the grep ledger goes in the work log, because the Review section is review's alone.
- 2026-09-29: T7 grep ledger. `link.html` 33 lines: 20 comments, 13 code identifiers (`store` names and variables, `storeSql`). `form.js` 69 lines: 33 comments, 36 code identifiers (`store`, `checkStore`, `STORE_KINDS`, the `'no-store'` fetch option). hitop-form `README.md` 6 lines: 5 URL examples (`link.html?c=…`, `link.html?z=…`) and the file name `instruments-store.spec.js`. `modules-hitopsr.Rmd` 9 lines: 3 `descriptor` argument names and 6 R code lines. `online-collection.Rmd`, `README.Rmd` and `_pkgdown.yml`: none.
- 2026-09-29: T7 screenshots in hitop-form `playwright-report/m146-screens/` (ignored by git). They show the page before at 375px and 1280px, and after with sections closed, open and after a Prolific build at both widths. The shots showed disabled buttons looking enabled, so a disabled style was added. Full suite after it: 761 of 762 passed. N7 in `network.spec.js` timed out under full-suite load and passed 5 of 5 alone.
- claim audit: 175 claims read, 4 corrected — hitop-form link.html, hitop-form README.md, vignettes/articles/online-collection.Rmd
- 2026-09-29: the audit also found that a Supabase refusal quoting a URL with "table" or "key" in it focused the wrong field. Fixed test-first (4 tests, 2 red before the fix). Full suite: 766 passed. Doubtful items left for review: an instruments refusal focuses the first menu, not the row at fault, and "Copy the link" hides without `navigator.clipboard`.
- 2026-09-30: review pass 1 returned the milestone to in-progress, defect return 1. The consistency gate failed because NEWS.md has no entry for this milestone. AC6 failed because hitop-form `README.md:892` keeps "store" in the file name `tests/instruments-store.spec.js`, which is none of AC6's five kinds. The repair is to rename the file, or to amend AC6 through the gate. AC1 to AC5 are met. AC7 is met locally, and its PR CI part waits for the push. The 26 reviewer findings in the Review section are untriaged, for the implement question gate.
- 2026-09-30: implement resumed. Question gate (Jeff chose each recommended option): AC6 is repaired by renaming the spec file (T8), with no amendment. Findings 1, 2 and 4 to 6 are restored (T9).
- 2026-09-30: question gate, continued. Findings 8 to 12, 16, 17, 23 to 25, the title part of 20 and the README opening name are fixed (T10, T12). Finding 7 gets a candidate row. Findings 14 and 18 join the `z` link gaps row.
- 2026-09-30: question gate, rejections. Finding 3: a revert of the online-form messages puts retired terms back into `form.js` strings and fails AC6, and M148 reviews that page's text. Finding 13: the site is https-only, and the copy fallback is the same as on main.
- 2026-09-30: question gate, rejections continued. Findings 19, 21, 22 and 26 are speculative or cosmetic. The rest of finding 20: the section grouping carries "Optional", and the README states the blank-line rule.
- 2026-09-30: tasks T8 to T12 added (minor amendment), and Coverage updated (AC6 → T8, AC7 → T11).
- 2026-09-30: T8 done. The spec is now `tests/instruments-supabase.spec.js`, and the README row and `tests/fixtures/README.md` name it. Its 6 tests pass. The AC6 grep over hitop-form `README.md` now finds 5 lines, all URL examples.
- 2026-09-30: T9 done. The intro says an opened study link puts its setup in GitHub Pages' logs, and links the README section on it. The Prolific, SONA, declined-text, decline-URL and saved-file URL hints have their facts back, each within 40 words. New test S9 asserts each fact, and it failed on the old text first. The SONA hint is at 40 words. The two builder specs pass, 154 tests.
- 2026-09-30: T10 done. Eight hints and two refusals say "the online form" where they meant the participant's page. The `z` refusals say "does not unpack" and "unpacks to more than". The Supabase focus pattern reads only the message's opening. The consent and declined boxes count any value in the summary.
- 2026-09-30: T10, continued. A field change hides the built link. Links in the intro, the hints and the SQL block open a new tab. The Supabase-only next step is two sentences. The tab title is "HiTOP Study Link Builder". The README names the page and qualifies the focus claim, and `modules-hitopsr.Rmd` fixes the Google Sheet sentence.
- 2026-09-30: T10 tests. The new tests (2 focus probes, a whitespace summary, the stale-link and title test, and new-tab links) and the updated sentence, unpack and refusal assertions were run first: 14 failed. After the fix, the full hitop-form suite passed 770 tests. `modules-hitopsr` builds.
- 2026-09-30: T11 done. NEWS.md opens New features with the Study Link Builder entry. It covers the required parts first, the five sections and their summaries, refusal focus, disabled buttons, the result region, shorter hints, the menu names and the article names.
- 2026-09-30: T12 done. DESIGN Known issue 11 quotes the hint as "HiTOP-SR only." (corrected M146). ROADMAP is at its 59-line cap, so the old names outside scope joined the "Module" naming row, now "User-facing names". The `z` link row gained the test-reach gaps and the instruments focus, and lost the SQL-beside-error gap that T3 closed. ROADMAP is 23,940 bytes.
- claim audit: 80 claims read, 2 corrected — hitop-form link.html, NEWS.md
- 2026-09-30: the audit found that Add, Move and Remove left a built link on screen. Both renumber functions now hide it. The stale-link test covers the six presses, and it failed with the call removed. The NEWS entry now says the articles use "Study Link Builder", since neither names the Module Builder. The SONA hint says "Paste the link" again, still at 40 words. Full hitop-form suite: 770 passed.
- 2026-09-30: T8 to T12 done, and status set to review. `devtools::check()` gives 0 errors, 0 warnings and 0 notes, and `check_pkgdown()` finds no problems. The hitop-form suite passed 770 tests.
- 2026-09-30: review pass 2 returned the milestone to in-progress, defect return 2. AC4 fails: with Supabase and no site, `nextStep()` gives two sentences where AC4 promises one, and S7 copies them. AC1 to AC3, AC5 and AC6 are met, and AC7 is met locally. The 25 findings in the Review section are untriaged, for the implement question gate.
- 2026-09-30: for that gate, finding 5 (a prefilled intro over 60 words) falls outside AC5's named states. It is a possible AC5 amendment, not a defect. A third defect return makes descope or park the recommended option.
- 2026-09-30: implement resumed. Question gate (Jeff chose each recommended option): AC4 is repaired by one sentence, with no amendment (T13). Findings 2, 3, 4, 6, 9, 10, 11, 14, 15, 18, 19 and 23 are fixed now (T14 to T16).
- 2026-09-30: question gate, continued. AC5 stays as written, and finding 5 is rejected: the notice lists the addresses to check, and no word limit can bound it. Finding 7 goes to M147. Findings 12, 13, 21, 24 and 25 join the `z` link gaps row. Findings 16 and 17 get a follow-up row.
- 2026-09-30: question gate, rejections. Finding 8 keeps the pass-1 rejection of finding 3. Finding 20 is wording that reads correctly in context, and finding 22 names an anchor that no link uses.
- 2026-09-30: tasks T13 to T16 added (minor amendment), and Coverage updated (AC4 → T13, AC5 → T15).
- 2026-09-30: T13 done. With Supabase and no site, the next step is one sentence: "Run the SQL below once in your Supabase project's SQL editor, then open the link once to test it and give it to each participant." S7 now also counts sentence ends in the page's text, and that count gives two on the old text. The Supabase test failed before the fix, and all 16 S7 tests pass after it.
- 2026-09-30: T14 done. `hideResult()` counts each call, and `build()` ends with nothing shown if the count moved during the export fetch or the link encoding. A typed edit was already covered, because the focus move to the heading fires its `change`. A menu change was not. The new test changes the recruiting site during a held fetch, and it failed before the fix. The first fix declared the count after setup had called `hideResult()`, which stopped the page's script. The count now sits beside `result`. Five builder specs pass, 234 tests.
- 2026-09-30: T15 done. The intro says the page keeps and sends nothing you type, and it drops "with your choices" to stay within 60 words. The destination hint names an Apps Script web app and one JSON row. The table hint says a Supabase build downloads each instrument from the hitop site.
- 2026-09-30: T15, continued. The options hint says blank lines are skipped. The decline URL hint says `{participant}` is empty unless the link or a site gives it. The shuffle and Prolific hints say "the responses". The SONA hint names `id=%SURVEY_CODE%`, and the completion hint names the arrival screen.
- 2026-09-30: T15 tests. S9 asserts each fact, and it failed on the old page. Three old assertions in `link.spec.js` take the new text. Four builder specs pass, 214 tests, with S8's word limits among them.
- 2026-09-30: T16 changes made, not yet ticked. The README and two test comments drop "three" from "required parts". The S3 empty-key pattern is anchored. The top-bar link opens a new tab, tested, and that test failed on the old page. A README row says "unpack". The ROADMAP question-gaps row loses the fixed load gap.
- 2026-09-30: T16 filing. The `z` link gaps row takes findings 12, 13, 16, 17, 21, 24 and 25, with a label-less control as a promote trigger. M147's work log takes finding 7. The modularization row is compressed, so ROADMAP stays at 23,984 bytes. The full suite and the claim audit are running.
- 2026-09-30: T16 done. The full hitop-form suite passed 771 of 771.
- claim audit: 35 claims read, 1 corrected — hitop-form tests/link.spec.js
- 2026-09-30: the corrected claim was two L19 comments, which said the intro names the required parts. They now say it asks for them, and the reader's re-read found both hold. The audit covered the return-2 lines in hitop-form, because hitop has no code change since the pass-2 `check()`.
- 2026-09-30: T13 to T16 done, and status set to review. hitop code is unchanged since pass 2, where `devtools::check()` gave 0 errors, 0 warnings and 0 notes.

- 2026-09-30: review pass 3. AC1 to AC6 met, and AC4 now passes. AC7 is met locally, and its CI part waits for the push. The 19 reviewer findings are untriaged, for the step-7 gate.

## Decisions

## Review

Pass 1, 2026-09-29 to 2026-09-30. Both branches were level with `origin/main`, so no merge was needed.

**Evidence.** The hitop-form suite ran in full: 766 of 766 passed, in 3.0 minutes. The criteria map to `tests/link-sections.spec.js` as follows.
- AC1: S1 asserts the part order and five closed sections reading "Not used". S2 asserts the label lists. S3 makes one refusal per section and asserts the message, the section open, the field focused and the others closed. S4 asserts that no element overflows at 375px and 1280px with every section open. Met.
- AC2: S5 covers 12 one-field links, using `z` for the consent fields and the questions. This is every optional field `parseLink()` accepts. A no-field link leaves all sections closed. Met.
- AC3: S6 computes button states from the rule for 1, 2 and 3 instrument rows and question groups. It asserts the exact accessible names. It checks again after a move and a removal, and for instrument rows after a prefill. For question groups it also checks after a `z` prefill and a questions file load. Met.
- AC4: 15 tests cover the 5 sites by 3 destinations. They assert the heading text and its focus, and a box with `overflow-y` auto. They assert "Copy the link" to the right of the box, and the exact next-step sentence below it. A `z` link over 5,000 characters stays under 16rem and scrolls. Met.
- AC5: S8 checks every `.hint` and `.site-hint`, guarded at more than 20, and the intro range. It checks the page's text, placeholders and aria-labels for all 12 retired terms. It does this with sections open and four question types, across 5 sites, 3 destinations and a Supabase build. Met.
- AC6: re-run with `perl` over the same 12 patterns. The counts match the ledger: `link.html` 33, `form.js` 69, hitop-form `README.md` 6, `modules-hitopsr.Rmd` 9, and 0 in the other three files.
  - `link.html`: 19 comments and 14 code identifiers, `name="store"` at line 133 among them. The ledger said 20 and 13.
  - `form.js`: every hit is a comment or a code identifier.
  - `modules-hitopsr.Rmd`: 3 `descriptor` argument names and 6 R code lines.
  - hitop-form `README.md`: 5 URL examples. The sixth hit, line 892, is the file name `tests/instruments-store.spec.js` in the test table. A file name is none of AC6's five kinds, so **AC6 fails as written**.
  - The menu reads "Module Builder" and "Study Link Builder".
- AC7, local parts only:
  - The Playwright suite passes (above).
  - `devtools::check()` gives 0 errors, 0 warnings and 0 notes. The first background run was killed with no output and was re-run in the foreground.
  - `pkgdown::check_pkgdown()` finds no problems.
  - The download-pages navbar test passes.
  - `build_readme()` leaves `README.md` unchanged.
  - Both articles build with `build_article()` against the branch installed in a scratch library. The installed hitop 0.2.0 predates D-080, so `online-collection` fails against it.
  - Not yet run: the PR's CI, which starts only once the step-8 push happens.

**Consistency gate.**
- `cairn_validate` passes, with only advisory warnings.
- `document()` leaves no diff.
- No principle changed, so `cairn_impact` was skipped.
- **Fail:** NEWS.md has no entry for this milestone. The menu renames and the reworked Study Link Builder are user-visible. Every earlier link-builder change and the menu's earlier entry got one (M125, M133, M134).

**Findings.** Three fresh reviewers ran: an Opus diff reviewer, a Sonnet history reviewer and a Sonnet prior-review reviewer. Their findings are merged and ranked most severe first. "Seen" means this session read the code and found the defect. None is triaged yet, because the step-7 gate was not reached.
1. The intro drops M128 AC6's sentence that an edit link's `c` or `z` goes to the page's host. It now says "This page keeps nothing and sends nothing you type". No test guards this. (Seen)
2. The SONA hint says participants read the credit token "in SONA's completion URL". The token sits in the study link. The hint also drops the `{participant}`-for-`XXXX` instruction, and both test assertions were deleted. (Seen)
3. `form.js` messages that participants see on the online form, which M148 owns, were reworded. These include the c-and-z refusal, the `checkStore` prefix and the send reasons, and support can no longer tell the faults apart.
4. The decline URL hint lost its `{participant}` note. The saved-file URL hint lost the rule that a send the store accepts still goes to the completion URL. The tests that asserted it were dropped.
5. The declined-text hint gives one sentence for an empty field. With no decline URL, the page shows a second one, "You can close this page." (`form.js:1665`). (Seen)
6. The warning about Prolific's URL-parameters option left the page. A test comment points to the wrong README section.
7. Old names remain outside the files AC6 lists:
   - `vignettes/pid5_scoring.Rmd:198-202` ("link builder", "store")
   - `vignettes/articles/_download-helpers.R:179-186` ("form page", and the button "Make a study link" on six download pages)
   - `overview.Rmd:69` ("store")
   - hitop-form `README.md:16` ("Make a study link")
8. "The page" means the builder in the intro but the online form in 11 hints and 2 refusals.
9. `inflateConfig` refusals that the builder shows still say "decompress", where the README and page say "unpack".
10. The Supabase focus regex is not anchored, so a URL quoting ": its table" focuses the table field.
11. The result region stays on screen after a field change or file load, and its next-step sentence can then disagree with the fields.
12. Nine hint links to the README open in the same tab mid-form, and leaving the page can lose input. No one tested this in a browser.
13. With no `navigator.clipboard`, "Copy the link" is hidden. A copy failure writes to `#err`, far from the button, and a later copy does not clear it.
14. An instruments refusal focuses the first menu, not the row at fault.
15. For whitespace-only consent or declined text, the summary trims but the build does not.
16. The README says a refusal focuses its field, but instruments, fetch and encode refusals do not.
17. DESIGN Known issue 11 quotes the old hint "Optional, HiTOP-SR only."
18. Test reach:
    - S4 measures only the default site and destination.
    - "Beside the box" is checked at 1280px only.
    - S5 has no Connect case.
    - The README slug test counts `#` lines inside code fences.
    - S1 ignores unknown top-level children.
19. `showHeld()` and `labelText()` run outside the prefill try/catch. This is speculative, because no control outside a label exists today.
20. Small losses:
    - The options hint no longer says "Blank lines are skipped".
    - Per-field "Optional." is gone.
    - `<title>` lost "HiTOP".
21. A click test on the first row's Move up was replaced by a disabled-state check. The row guard is still there but no longer driven by a click.
22. Hint links inside `<label>` lengthen the controls' accessible names.
23. The intro says "three required parts", but where responses go has a default.
24. With Supabase and no site, the sentence reads "before you open the link once to test it, and then give it…".
25. `modules-hitopsr.Rmd` (~409) calls a Google Sheet a web address.
26. "The builder" is still used as shorthand in the articles and the README.

Pass 2, 2026-09-30. Both branches were level with `origin/main` after a fetch, so no merge was needed.

**Evidence.** The full hitop-form suite ran once: 769 of 770 passed, in 3.8 minutes. The one failure, `tests/instruments-row.spec.js:212`, was a 10-second connect timeout on the fetch of an instrument export from GitHub (`tests/helpers.mjs:30`). That spec is not in the diff. Run alone three times, it passed 33 of 33. All 50 tests in `tests/link-sections.spec.js` passed.
- AC1: S1 asserts the part order and five closed sections that read "Not used". S2 asserts the label lists. S3 makes one refusal per section, and the Supabase focus probes pass too. S4 finds no overflow at 375px and 1280px. All passed. Met.
- AC2: S5 runs one test per optional field, with `z` links for consent and questions. It passed, and the no-field test passed. Met.
- AC3: S6 passed for instrument rows and question groups. It covers 1 to 3 rows, a move, a removal, a prefill and a questions file load. Met.
- AC4: the S7 tests over 5 sites by 3 destinations passed. The long `z` link test passed. **AC4 fails as written** (corrected in pass 2, after the diff reviewer's finding). With Supabase and no recruiting site, `nextStep()` returns two sentences (`link.html:1211`), and AC4 promises one next-step sentence. S7 passes because `expectedNext()` copies the same two sentences (`tests/link-sections.spec.js:426`).
- AC5: S8 checks word limits and retired terms over the site, destination and Supabase states. It passed, and the README heading test passed. Met.
- AC6: the 12 D-083 patterns were re-run with `perl`.
  - `link.html` 33 lines: comments and code identifiers (`name="store"`, `store` variables, `storeSql`).
  - `form.js` 69 lines: comments and code identifiers (`store`, `checkStore`, `STORE_KINDS`, the `'no-store'` fetch option).
  - hitop-form `README.md` 5 lines: all URL examples (`link.html?c=…`, `link.html?z=…`).
  - `modules-hitopsr.Rmd` 9 lines: 3 `descriptor` argument names and 6 R code lines.
  - `online-collection.Rmd`, `README.Rmd` and `_pkgdown.yml`: 0.
  - The menu reads "Module Builder" and "Study Link Builder". Met.
- AC7, local parts only:
  - The Playwright suite passes, with the one network timeout above.
  - `devtools::check()` gives 0 errors, 0 warnings and 0 notes, in 7.6 minutes.
  - `pkgdown::check_pkgdown()` finds no problems.
  - The download-pages navbar test passes.
  - `build_readme()` leaves `README.md` unchanged.
  - Both articles build with `build_article()` against the branch installed in a scratch library.
  - Not yet run: the PR's CI, which starts only after the step-8 push.

**Consistency gate.**
- `cairn_validate` passes, with only advisory warnings.
- `document()` leaves no diff.
- NEWS.md opens New features with the Study Link Builder entry.
- No principle changed, so `cairn_impact` was skipped.

**Findings.** Three fresh reviewers ran: an Opus diff reviewer, a Sonnet history reviewer and a Sonnet prior-review reviewer. Both GitHub probes for inline PR comments returned none. The findings are merged and ranked most severe first. "Seen" means this session read the code and found the defect. The reviewers found the T8 rename in code. They also found the fixes for pass-1 findings 1, 2, 4, 5, 8 to 12, 15 to 17, and 23 to 25.
1. AC4 fails: with Supabase and no site, the next step is two sentences, and S7 copies them (see AC4 above). The README "Your study link" section also says one sentence. (Seen)
2. A Supabase build waits on the export fetch. An edit made during that wait hides the result. The build then shows the link and SQL for the old values (`link.html:1168` to `1200`). The T10 test covers only edits after a build. (Seen)
3. The options hint lost "Blank lines are skipped". The pass-1 rejection of finding 20 said the README states the rule, but `README.md:313` states it for consent text only. (Seen)
4. The destination hint says "a Google Sheet's web app" (`link.html:124`), the error T10 fixed in `modules-hitopsr.Rmd`. The old "one JSON row" rule is gone. (Seen)
5. On a prefilled page, the text between `<h1>` and the form holds 61 words before the notice's list items (`link.html:105` to `110`). AC5's named states do not include a prefilled page. (Seen)
6. The decline-URL hint says "`{participant}` works here too". But with no identifier on the page yet, `decline()` fills it with nothing (`form.js:1658` to `1662`).
7. The intro no longer links the Module Builder. The module route sits only in the closed "Item order and HiTOP-SR module" section.
8. Participant-facing `form.js` strings in `decodeLink` and `sendResponses` were reworded, though `link.html` never shows them. This is pass-1 finding 3. The history reviewer disputes its rejection for these strings only. (Seen)
9. The intro dropped "sends none of what you type anywhere" and the sentence that a Supabase build downloads each instrument from the hitop site.
10. Four hints lost table and column-order facts (shuffle, Prolific, instruments, SQL). Two say "the file" where a row or table also holds the data.
11. The SONA hint lost the `id=%SURVEY_CODE%` sentence. The completion URL hint lost "once the sent screen is drawn".
12. Pressing "Make the link" before prefill ends still submits a native GET. The `z` link gaps row names an edit to the prefill or build handler as the time to fix it.
13. `showHeld()` and `labelText()` run outside the prefill try/catch. This is pass-1 finding 19, rejected as speculative.
14. The ROADMAP question-gaps row still says a questions file load leaves an old built link on screen, which T10 fixed.
15. hitop-form `README.md:28` still says "three required parts", and a comment in `tests/link.spec.js` says the same.
16. The articles do not say that the participant, module, consent and questions fields sit in closed sections, or name "Your study link".
17. `modules-hitopsr.Rmd` (about line 405) lists the forms the online form shows without the PID-5 forms. The line predates the branch, but the branch rewrote it.
18. The S3 empty-key probe's pattern has an unanchored second branch (`tests/link-sections.spec.js:244`).
19. The "hitop package documentation" link opens in the same tab (`link.html:103`).
20. In the Prolific hint, "it reads the filled ones" can read as the Prolific option.
21. The first-row Move up guard is no longer driven by a click. This is pass-1 finding 21.
22. The README anchor `#make-a-study-link` is gone. No link in either repo uses it.
23. The README test row for `link-consent.spec.js` says "decompress" (`README.md:885`).
24. The old specs open every section through `openBuilderSections()`, so their refusals and prefills never run with sections closed.
25. `showHeld()` clones each held label, textarea value included, on every keystroke.

Pass 3, 2026-09-30. Both branches were level with `origin/main` after a fetch, so no merge was needed. No PR exists yet in either repo.

**Evidence.** The full hitop-form suite ran once: 771 of 771 passed, in 1.9 minutes. All 51 tests in `tests/link-sections.spec.js` passed.
- AC1: S1 (part order, five closed sections reading "Not used"), S2 (label lists), S3 (one refusal per section, plus the Supabase focus probes) and S4 (no overflow at 375px and 1280px) passed. Met.
- AC2: S5 (one test per optional field, `z` links for consent and questions) and the no-field test passed. Met.
- AC3: S6 passed for instrument rows and question groups over 1 to 3 rows, a move, a removal, a prefill and a questions file load. Met.
- AC4: the 15 S7 tests (5 sites by 3 destinations) passed. Each asserts the heading and its focus, the scrolling box, "Copy the link" beside it, and the sentence below it. Each also counts sentence ends in the page's text and requires exactly one. `nextStep()` (`link.html:1224`) returns one sentence in every branch, the Supabase-with-no-site branch included. The long `z` link test passed. Met.
- AC5: S8 checks every `.hint` and `.site-hint` against 40 words and the intro against 60, and checks the page's text, placeholders and aria-labels for the 12 retired terms. It does so with every section open and four question types, across 5 sites, 3 destinations and after a Supabase build. It passed, and the README heading test passed. Met.
- AC6: the 12 D-083 patterns were re-run with `perl`.
  - `link.html` 33 lines: comments ("form page") and code identifiers (`name="store"`, `store` variables, `storeSql`).
  - `form.js` 69 lines: comments and code identifiers. The non-comment hits are `store`, `checkStore`, `checkStoreUrl`, `STORE_KINDS`, `store.kind` inside message templates, and the `'no-store'` fetch option.
  - hitop-form `README.md` 5 lines: all URL examples (`link.html?c=…`, `link.html?z=…`).
  - `modules-hitopsr.Rmd` 9 lines: `descriptor` argument names and R code.
  - `online-collection.Rmd`, `README.Rmd` and `_pkgdown.yml`: 0.
  - The menu reads "Module Builder" (`_pkgdown.yml:39`) and "Study Link Builder" (`_pkgdown.yml:41`). Met.

- AC7, local parts only:
  - The Playwright suite passes, 771 of 771.
  - `devtools::check()` gives 0 errors, 0 warnings and 0 notes. A background run wrote no output, and the foreground re-run gave this result.
  - `pkgdown::check_pkgdown()` finds no problems.
  - The download-pages navbar test passes, 93 expectations.
  - `build_readme()` leaves `README.md` unchanged.
  - Both articles build with `build_article()` against the branch installed in a scratch library.
  - Not yet run: the PR's CI in hitop-form, which starts only after the step-8 push. AC7 stays unticked until it passes.

**Consistency gate.**
- `cairn_validate` passes. Its warnings are advisory: 16 tasks over the sizing tripwire, and old dangling D-ids.
- `document()` leaves no diff.
- NEWS.md opens New features with the Study Link Builder entry.
- No principle changed, so `cairn_impact` was skipped.

**Findings.** Three fresh reviewers ran: an Opus diff reviewer, a Sonnet history reviewer and a Sonnet prior-review reviewer. Both GitHub probes for inline PR comments returned none, and the archives hold no `## Review` findings on these files. None found a criterion failing. They found the fixes for pass-2 findings 1 to 3, 6, 9, 11, 14, 15, 18, 19 and 23 in code. The findings are merged and ranked most severe first. "Seen" means this session read the code and found the defect.
1. Pass-2 finding 10 is half fixed, though the work log records it fixed. The instruments hint (`link.html:114`) and the SQL hint (`link.html:247`) lost the column facts: one group of item columns per instrument in list order, and an `item_order` column and two Prolific columns when chosen. Nothing on the page says that a changed choice needs a new table. A researcher who runs the SQL "once", then ticks the random order or adds a question, gets every row refused and every participant falls back to a downloaded file. Only `README.md:411-413` states the rule. (Seen)
2. A stale build ends in silence (`link.html:1180-1200`). If a field changes during the export fetch, the page shows no link, no message, and moves no focus. A screen-reader user hears nothing.
3. The SONA hint (`link.html:168`) no longer says the survey code becomes the participant identifier. "For completion" does not name the Completion URL field, which sits in another closed section.
4. The destination hint says a Supabase table "gets one JSON row per participant" (`link.html:124`), but the table holds one column per item. S9 asserts this wording. (Seen)
5. The decline-URL hint dropped that `{participant}` goes after the `?` or `#`, and "code" from the Prolific no-consent completion URL. The build refuses the token elsewhere, and its refusal explains.
6. The saved-file URL hint dropped its two triggers (no destination, or a send not confirmed). The completion hint lost "For Prolific, the completion URL shown on the study's page". The module hint lost that, with several instruments, the module applies to the HiTOP-SR among them.
7. S7's `expectedNext()` (`tests/link-sections.spec.js:422-430`) still copies `nextStep()` branch for branch. Only the sentence count is independent of the code, and it misses two sentences joined by ";".
8. The T14 guard's stale-after-encode branch and its fetch-fails-while-stale branch are untested, and so is a typed edit during the wait. The T14 test reaches the real network through `route.continue()`.
9. `showHeld()` and `labelText()` still run outside the prefill try/catch. The history reviewer argues that T14's own setup-order crash is new evidence against the pass-1 finding 19 and pass-2 finding 13 rejections.
10. The intro, the table hint and a prefill refusal still call the builder "this page" (`link.html:106`, `146`, `844`). It is no retired pattern. "Decline URL" and the field "Completion URL after a decline" name one field two ways.
11. The intro no longer says that an opened edit link sends the Supabase key to the host. Only README lines 219 to 236 say it.
12. On the participant page, the `checkStore` prefix no longer says the fault is in the study link, and `link.html:1164` matches that prefix as literal text. M148 owns that page's text.
13. The "unpack" wording changes messages participants see (`form.js:204`, `210`). The pass-1 gate accepted it under finding 9. A note only.
14. `const sql` in `nextStep()` (`link.html:1225`) hides the page's `sql` textarea.
15. D-074 still records the old menu labels, and no entry annotates it. D-083(c) covers the menu.
16. The README test-table row for `tests/link-sections.spec.js` does not name the return-2 tests (S9, the stale-result tests).
17. The top-bar "← hitop package documentation" link, a back arrow, now opens a new tab. Cosmetic.
18. The articles do not mention the closed sections or "Your study link". This is pass-2 finding 16, already filed.
19. In a browser that does not fire `change` before an Enter submit, the heading focus could fire `change` and hide the link just shown. Chromium fires it first. Unverified.
