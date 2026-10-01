# M153: Questions limited by size, and a long-link warning

- **Status:** review
- **Priority:** high
- **Depends on:** M152
- **Driving RR:** —
- **Principles touched:** IP2
- **Resolves:** —
- **Surface tier:** user-facing — changes what a study link may hold and what the Study Link Builder shows
- **Branch/PR:** m153-question-limit-link-length; companion: /Users/jmgirard/github/hitop-form m153-question-limit-link-length

## Goal

A study link can hold as many of the researcher's own questions as fit in the 100,000-byte setup, and the Study Link Builder warns when a link grows long enough that some sites may cut it.

## Scope

**In:** hitop-form companion work. Remove `QUESTIONS_MAX`, apply the 100,000-byte limit to `c` setups too, refuse a Supabase table over PostgreSQL's column limit, and show a link-length warning. README, the hitop article's questions section, NEWS, and source notes for RFC 9110 and the PostgreSQL limits.

**Out:**
- SONA's own Study URL limit stays in the hitop-form researcher-content candidate row until SONA documents one.
- Text-answer length limits stay in the hitop-form question-gaps candidate row.

## Acceptance criteria

- [ ] AC1: `QUESTIONS_MAX` is removed from form.js, and `checkQuestions()` counts no questions. A setup with 51 questions and one with 200 short questions pass `parseLink()` and build in link.html. The 100,000-byte limit applies to a `c` setup too. `encodeLink()` refuses to write one over it with "This link's setup is N bytes, more than the 100,000 bytes the online form reads.", and `decodeLink()` refuses a `c` whose decoded bytes exceed it with a message naming the size and the limit. Tests that pinned 51 questions as refused are replaced by these.
- [ ] AC2: For a Supabase destination, link.html refuses a setup whose table would have more than 1,600 columns, PostgreSQL's limit per table (PostgreSQL documentation, Appendix K), counting the columns `storeSql()` would create, and names the count. Tests build one setup at 1,600 columns and one at 1,601.
- [ ] AC3: When the finished link is longer than 8,000 characters, the result region shows "This link is N characters long. Some sites and mail programs cut long links. You can keep the setup in a file you host instead." At 8,000 characters or fewer it shows nothing. 8,000 is the minimum URI length RFC 9110 (section 4.1) recommends that senders and recipients support. Tests pad the study name and the participant-parameter name at run time to build links of exactly 8,000 and 8,001 characters.
- [ ] AC4: A case-insensitive search of each paragraph holding `question` and `50`, `51` or `fifty`, in hitop-form's README.md, link.html, index.html and form.js and in hitop's `vignettes/`, finds none that states a limit on the number of questions. hitop's NEWS.md keeps its released entries as history. The README and the hitop article's questions sections state the 100,000-byte limit, the Supabase column limit and that PostgreSQL also limits a row's size.
- [ ] AC5: hitop's NEWS.md says the 50-question limit is gone and names the link-length warning. hitop-form's Playwright suite passes. In hitop, `devtools::check()` reports 0 errors and 0 warnings, and `pkgdown::build_article("articles/online-collection")` renders without error.

## Coverage

- AC1 → T1, T2
- AC2 → T3
- AC3 → T4
- AC4 → T5
- AC5 → T2, T3, T4, T6

## Tasks

- [x] T1: In `form.js`, remove `QUESTIONS_MAX` from `checkQuestions()`, and add the `c` size checks to `encodeLink()` and `decodeLink()`.
- [x] T2: Replace the 51-question tests in `questions.spec.js`, `link-questions.spec.js` and `link-questions-file.spec.js`, and add the `c` size tests.
- [x] T3: Add the Supabase column check to `link.html`'s build, with tests. Write a hitop source note on PostgreSQL's Appendix K limits.
- [x] T4: Add the link-length warning to the result region, with the padded-length tests. Write a hitop source note on RFC 9110 section 4.1.
- [x] T5: Update the link.html hint, the README's questions section and test rows, and the hitop article. Run AC4's paragraph search and read each hit.
- [x] T6: hitop NEWS line, `pkgdown::build_article()`, `devtools::check()`, and the companion PR.

## Work log

- 2026-10-01: created by /milestone-plan with M152 and M154. The 50-question cap came from M137's plan with no recorded reason; the measured limit is the 100,000-byte setup. Audits are recorded in M152's work log.
- 2026-10-01: plan chose a warning at 8,000 characters, the length RFC 9110 recommends supporting, over a 2,000-character warning, which has no current standard behind it. Falsified by a recruiting site or mail program that cuts links shorter than 8,000 characters.
- 2026-10-01: plan chose a 100,000-byte limit on `c` setups, to match `z` and hosted files, over leaving `c` unlimited. Falsified by a researcher whose `c` link over 100,000 bytes worked before.
- 2026-10-01: implement started. Branch m153-question-limit-link-length in hitop and in the hitop-form companion. Question gate skipped: nothing open. The test page address is 23 characters, so a `c` link of 8,000 and of 8,001 characters is reachable.
- 2026-10-01: T1+T2 done. `QUESTIONS_MAX` and its count check removed. `encodeLink()` checks the size of every setup, with the "Shorten" sentence only for consent text or questions. `decodeLink()` refuses a `c` over 100,000 decoded bytes. The 51-question refusals became 51 and 200 accepted in questions, link-questions and link-questions-file specs. S12's no-question probe removed: the editor has no such fault now. Z6 and an LQ5 test cover `c` at 100,000 and 100,001 bytes. The test page server takes headers up to 256 KiB, since Node refuses a request line over 16 KiB. A plant that disabled both checks turned 3 tests red. Full suite 954 passed, 1 layout failure (pid5 legend) that passed on a rerun of its spec.
- 2026-10-01: T3 done. form.js gains `POSTGRES_COLUMNS_MAX` (1,600) and `storeColumns()`, which `storeSql()` now uses. link.html refuses a Supabase table over the limit, naming the count, after the export fetch. LQ7 builds 1,600 columns (its SQL counted) and refuses 1,601. S12 fires the new refusal. Plants at 1,601 and 1,599 each turned the matching tests red. Source note `postgresql2026limits.md` added. 157 passed in the three affected specs.
- 2026-10-01: T4 done. If the link passes `LINK_WARN_LENGTH` (8,000), link.html shows `#long` in the result region. Each build clears it. L37 pads the study name, with a 64-character participant parameter, to exact lengths of 8,000 and 8,001. It also checks that a shorter link clears the warning. Plants at 7,999, at 8,001 and with no clearing each turned one test red. Source note `rfc9110.md` added. The L37 block was appended with a shell heredoc, not the Edit tool.
- 2026-10-01: T5 done. The link.html hint reads "No limit but the 100,000-byte setup." (40 words, the page's limit). The README's questions section and the article's state the 100,000-byte limit, the 1,600-column limit and the row-size limit. The README's study-link section names the long-link warning and the `c` refusal, its Supabase steps point to the column limit, and 7 test rows are updated. The article's setup-file section names the warning. AC4 search over 22 paths and 851 paragraphs: 2 hits, none a limit on the number of questions. The same search finds the old sentence in main's README, link.html and article.
- 2026-10-01: T6 done. NEWS gains a bullet on the removed limit, the `c` limit, the column refusal and the long-link warning. The unreleased questions entry loses "up to 50 questions". The installed hitop predated the multi-instrument reader, so the article failed at its two-instrument chunk. After `devtools::install()` of the branch, `pkgdown::build_article()` rendered. `devtools::check()`: 0 errors, 0 warnings, 0 notes. Playwright: 961 passed. The companion PR waits for review, which opens PRs after approval (tracking rules).
- 2026-10-01: claim audit: 46 claims read, 4 corrected — hitop-form tests/link.spec.js, link.html, README.md; hitop NEWS.md, vignettes/articles/online-collection.Rmd. L37 now pads the parameter name at run time, the hint states the byte limit alone, the long-answer sentence says "can be kept outside", and the column wording names the link, since "Download the setup file" has no column check. The reader's re-read found all 4 hold. Playwright after the fixes: 961 passed.
- 2026-10-01: implement done, status review. Article re-rendered after the audit fixes.

## Decisions

## Review
