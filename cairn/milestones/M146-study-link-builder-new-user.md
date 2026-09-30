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

- [ ] AC1: Open `link.html` with no address parameters. The top-level parts come in this order: instruments, study name, where responses go, the five optional sections, and the "Make the link" button. The optional sections are "Participants and recruiting site", "Item order and HiTOP-SR module", "Consent", "When the participant finishes" and "Your own questions". Each is a closed `<details>` element. Its summary line reads "Not used", or it lists the visible labels of its fields that hold a value, joined by ", ". The test makes one refusal in each optional section. Each time, "Make the link" opens that section and moves focus to the refused field. The page is checked at 375px and at 1280px wide with every section open. No element is wider than the page. A Playwright test asserts each fact.
- [ ] AC2: Open `link.html` with a study link that sets one optional field. The section that holds the field opens, and its summary lists that field's label. The other optional sections stay closed. A Playwright test runs this once for each optional field alone. It uses a `z` link for the consent fields and the questions. A link that sets no optional field leaves every optional section closed.
- [ ] AC3: This criterion covers the instrument rows and the question groups. "Move up" is disabled on the first row, and "Move down" is disabled on the last row. On a single instrument row, all three buttons are disabled. The accessible name of each button begins with its visible text. A Playwright test asserts the state at each position for 1, 2 and 3 instrument rows. It does the same for 1, 2 and 3 question groups. It asserts the state again after a move, a removal, a prefill from a study link, and a questions file load.
- [ ] AC4: After a successful build, a region headed "Your study link" shows the link in a box that scrolls. A "Copy the link" button sits beside the box. Below it, one next-step sentence fits the choices made. With a recruiting site, the sentence says to paste the link into that site's study page. With a Supabase table, it says to run the SQL first. Otherwise it says to open the link once to test it, and then to give it to each participant. Focus moves to the region's heading. A Playwright test asserts each part for each recruiting-site option and each where-responses-go option. It also builds a `z` link over 5,000 characters, and asserts that the box stays under 16rem high.
- [ ] AC5: A Playwright test opens every optional section and adds one question of each type. It then chooses each recruiting-site option and each where-responses-go option in turn, and makes one Supabase build. In each of these states, it checks every `.hint` and `.site-hint` element in the page, shown or not. Each holds at most 40 words. The intro is all text between the `<h1>` and the first form part. It holds at most 60 words. The page's text, its `placeholder` values and its `aria-label` values hold none of the D-083 retired terms. The text of a built link is exempt. The test counts words by splitting on whitespace.
- [ ] AC6: A case-insensitive grep runs for each D-083 retired-term pattern. In hitop-form it reads `link.html`, `form.js` and `README.md`. In hitop it reads the two articles, `README.Rmd` and `_pkgdown.yml`. Each remaining hit is one of these: a code identifier, a comment, R code, an R function or argument name, or a literal URL or URL example. The Instruments menu names the two pages "Module Builder" and "Study Link Builder".
- [ ] AC7: The hitop-form Playwright suite passes locally and on its PR's CI. In hitop, `devtools::check()` gives 0 errors and 0 warnings, and `pkgdown::check_pkgdown()` passes. Both articles build with `pkgdown::build_article()`. The navbar test passes. `README.md` equals a fresh `devtools::build_readme()`.

## Coverage

- AC1 → T1, T5
- AC2 → T1, T5
- AC3 → T2, T5
- AC4 → T3, T5
- AC5 → T4, T5
- AC6 → T4, T6, T7
- AC7 → T5, T6, T7

## Tasks

- [x] T1: In `link.html` (fields at lines 105-213), move participant, recruiting site and module into their sections. Put the completion and decline addresses with their fields. Wrap each optional section in `<details>`, with a summary that updates on input. If `prefill()` (lines 646-781) gives a field a value, open the field's section. In the build handler (lines 790-1040), open the section and focus the field for a refused optional field.
- [x] T2: Set the disabled state of Move up, Move down and Remove in `renumber()` (lines 292-369) and in the question groups (lines 433-509). Update it after every change to the rows. Make each accessible name begin with the visible text.
- [x] T3: Build the "Your study link" region (lines 213-227, 1041-1069). It holds the heading, the scrolling box, "Copy the link" beside the box, the SQL block and the next-step sentence. Focus moves to the heading.
- [x] T4: Rewrite the intro, step list, labels, hints, placeholders and refusal texts in `link.html` under the D-083 names and the word limits. Do the same for the `form.js` messages that the page shows. Move the cut detail into `README.md` sections, and link each shortened hint to its section.
- [ ] T5: Write the Playwright tests for AC1 to AC5 in a new spec file. Update the existing specs that assert old text. Run the suite locally and on the PR.
- [ ] T6: In hitop, apply the D-083 names to the two articles, `README.Rmd` and the `_pkgdown.yml` menu. Rebuild `README.md` and build both articles. Run `check()` and `check_pkgdown()`.
- [ ] T7: Run the AC6 greps, and classify each remaining hit in the Review section. Take before and after screenshots at 375px and 1280px, with the sections closed and open, for Jeff's look at the merge gate.

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

## Decisions

## Review
