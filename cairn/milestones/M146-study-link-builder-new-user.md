# M146: Study Link Builder: required choices first, optional ones folded away

- **Status:** in-progress
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
- [ ] AC6: A case-insensitive grep runs for each D-083 retired-term pattern. In hitop-form it reads `link.html`, `form.js` and `README.md`. In hitop it reads the two articles, `README.Rmd` and `_pkgdown.yml`. Each remaining hit is one of these: a code identifier, a comment, R code, an R function or argument name, or a literal URL or URL example. The Instruments menu names the two pages "Module Builder" and "Study Link Builder".
- [ ] AC7: The hitop-form Playwright suite passes locally and on its PR's CI. In hitop, `devtools::check()` gives 0 errors and 0 warnings, and `pkgdown::check_pkgdown()` passes. Both articles build with `pkgdown::build_article()`. The navbar test passes. `README.md` equals a fresh `devtools::build_readme()`.

## Coverage

- AC1 → T1, T5
- AC2 → T1, T5
- AC3 → T2, T5
- AC4 → T3, T5
- AC5 → T4, T5
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
- [ ] T10: Make the smaller fixes chosen at the return gate:
  - "the online form" where a hint means the participant's page, "unpack" in the builder-shown refusals, and the README opening name
  - an anchored Supabase focus pattern, the same trim in the summary and the build, and a result region that a field change hides
  - README hint links that open a new tab
  - the README refusal-focus claim, the Supabase next-step sentence with no site, "HiTOP" in the tab title, and the Google Sheet wording in `modules-hitopsr.Rmd`
- [ ] T11: Add the NEWS.md entry for the menu names, the reworked Study Link Builder and the article names.
- [ ] T12: Fix the quote in DESIGN Known issue 11. Add a candidate row for old names outside the scope, and add the test-reach gaps to the `z` link gaps row.

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
