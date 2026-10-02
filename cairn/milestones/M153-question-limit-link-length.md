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

**In:** hitop-form companion work. Remove `QUESTIONS_MAX`, apply the 100,000-byte limit to `c` setups too, refuse a Supabase table over PostgreSQL's column limit, show a link-length warning, and refuse a link longer than the online form's host accepts. README, the hitop article's questions section, NEWS, and source notes for RFC 9110, the PostgreSQL limits and the host's URL limit.

**Out:**
- SONA's own Study URL limit stays in the hitop-form researcher-content candidate row until SONA documents one.
- Text-answer length limits stay in the hitop-form question-gaps candidate row.

## Acceptance criteria

- [x] AC1: `QUESTIONS_MAX` is removed from form.js, and `checkQuestions()` counts no questions. A setup with 51 questions and one with 200 short questions pass `parseLink()` and build in link.html. The 100,000-byte limit applies to a `c` setup too. `encodeLink()` refuses to write one over it with "This link's setup is N bytes, more than the 100,000 bytes the online form reads.", and `decodeLink()` refuses a `c` whose decoded bytes exceed it with a message naming the size and the limit. Tests that pinned 51 questions as refused are replaced by these.
- [x] AC2: For a Supabase destination, link.html refuses a setup whose table would have more than 1,600 columns, PostgreSQL's limit per table (PostgreSQL documentation, Appendix K), counting the columns `storeSql()` would create, and names the count. Tests build one setup at 1,600 columns and one at 1,601.
- [x] AC3: When the finished link is longer than 8,000 characters, the result region shows "This link is N characters long. Some sites and mail programs cut long links. You can keep the setup in a file you host instead." At 8,000 characters or fewer it shows nothing. 8,000 is the minimum URI length RFC 9110 (section 4.1) recommends that senders and recipients support. Tests pad the study name and the participant-parameter name at run time to build links of exactly 8,000 and 8,001 characters.
- [x] AC4: A case-insensitive search of each paragraph holding `question` and `50`, `51` or `fifty`, in hitop-form's README.md, link.html, index.html and form.js and in hitop's `vignettes/`, finds none that states a limit on the number of questions. hitop's NEWS.md keeps its released entries as history. The README and the hitop article's questions sections state the 100,000-byte limit, the Supabase column limit and that PostgreSQL also limits a row's size.
- [x] AC5: hitop's NEWS.md says the 50-question limit is gone and names the link-length warning. hitop-form's Playwright suite passes. In hitop, `devtools::check()` reports 0 errors and 0 warnings, and `pkgdown::build_article("articles/online-collection")` renders without error.
- [x] AC6: link.html refuses a link whose path and query are longer than 8,192 characters, counting each Prolific placeholder as the 24-character ID Prolific puts in its place. The refusal reads `This link is N characters long, longer than the online form's host accepts. Choose "In a file I host" under "Where the setup is kept".`, N being the length of the link as written, and for a link to a setup file it ends after "accepts.". `#result` stays hidden. A source note in `cairn/references/` traces 8,192 to Fastly's URL size limit and a 2026-10-01 measurement of GitHub Pages. A setup whose link counts 8,192 characters builds, and one that counts 8,193 is refused.

## Coverage

- AC1 → T1, T2
- AC2 → T3
- AC3 → T4
- AC4 → T5
- AC5 → T2, T3, T4, T6, T8
- AC6 → T7

## Tasks

- [x] T1: In `form.js`, remove `QUESTIONS_MAX` from `checkQuestions()`, and add the `c` size checks to `encodeLink()` and `decodeLink()`.
- [x] T2: Replace the 51-question tests in `questions.spec.js`, `link-questions.spec.js` and `link-questions-file.spec.js`, and add the `c` size tests.
- [x] T3: Add the Supabase column check to `link.html`'s build, with tests. Write a hitop source note on PostgreSQL's Appendix K limits.
- [x] T4: Add the link-length warning to the result region, with the padded-length tests. Write a hitop source note on RFC 9110 section 4.1.
- [x] T5: Update the link.html hint, the README's questions section and test rows, and the hitop article. Run AC4's paragraph search and read each hit.
- [x] T6: hitop NEWS line, `pkgdown::build_article()`, `devtools::check()`, and the companion PR.
- [x] T7: Add the host-length refusal to `link.html`'s build. Tests build both boundary lengths with no site ending and with SONA's, so each length is reachable on either server, and a Prolific case. Plants that drop the site ending from the count and that move the limit to 8,191 each turn a test red. Write the source note `fastly2026limits.md`, and record in `prolific2026help.md` that the study and session IDs are assumed to be 24 characters.
- [x] T8: State the refusal in README, the article and NEWS, and correct their claim that a link holds what fits in 100,000 bytes. Fix the row-size wording (review finding 10). Re-run AC4's search, the Playwright suite, `devtools::check()` and `pkgdown::build_article()`.

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
- 2026-10-01: review pass 1 stopped at the consistency gate, status in-progress (defect return 1). `cairn_validate` FAIL `references index<->disk`: `rfc9110.md` and `postgresql2026limits.md` provenance names no ingested date ("Read 2026-10-01", not "Ingested 2026-10-01"). The reviewers also measured GitHub Pages refusing the form's address over about 8,200 characters (HTTP 414), recorded in Review. The fix for that is a design decision for the implement question gate.
- 2026-10-01: implement resumed. Gate: Jeff chose to refuse a link longer than the host accepts, over naming the host in the warning (recommended). This widens the criteria after a defect return: AC6 is added, AC3 is unchanged. He also chose to answer long test addresses in the browser and to fix review findings 3, 6, 7, 8 and 10 now. curl on 2026-10-01: GitHub Pages, served by Fastly, answered 8,192 characters of path and query and refused 8,193 with 414. Fastly documents an 8 KB URL size limit.
- re-audit: AC6 (full) — six points, each fixed in the wording. The placeholders and N were undefined, and characters and bytes were mixed. A setup-file link was told to choose the file option, and "no link" was not named. The figure 8,192 rested on the curl measurement, not on Fastly's page. A refused 8,193 link cannot be built.
- 2026-10-01: source notes `rfc9110.md` and `postgresql2026limits.md` name their ingested date, and `cairn_validate` passes. Z6 opens its long `c` links through `gotoLong()`, which answers the request in the browser. serve.mjs is back to Node's default limit, and with the route removed the local server refuses the link. On the deployed page the 100,000-byte test passes, and the 100,001 refusal waits for this branch's deploy.
- re-audit: AC6 (full) — Prolific fills each placeholder with an ID of 24 characters. So a Prolific link that counts 8,192 as written is longer at the host and gets the 414. The test, Fastly and measurement clauses describe evidence, not link.html. This is AC6's second line, so the wording goes to Jeff.
- 2026-10-01: review findings 3, 6, 7 and 8 fixed. link.html refuses to fill from an opened `c` over 100,000 bytes, naming its size. `#long` is a status, and the line beside "Open the link" writes the length with a comma. The `readQuestions()` comment no longer describes a fault the editor cannot make. A test covers each code change, and a plant turned each test red.
- 2026-10-01: amendment at Jeff's selection, a widening after a defect return. AC6 is added. Scope's In line names the refusal and its source note. T7 and T8 are added, and AC5 maps to T8 too. Jeff chose the AC6 wording that counts each Prolific placeholder as 24 characters.
- 2026-10-01: T7 done. link.html gains `HOST_PATH_MAX` (8,192) and `PROLIFIC_ID_LENGTH` (24) and refuses a longer link after it is written. A setup-file link's refusal ends after "accepts.". L38 builds 8,192 and 8,193 with no site, SONA and Prolific, and LF9 does so with a setup file, with and without Prolific. Three plants each turned tests red: the ending dropped from the count (L38 and LF9 Prolific), Prolific counted as written (LF9 Prolific), and the limit at 8,191 (all three). LQ5's 100,000-byte `c` test now expects the length refusal. L37's shorter-link test pads 6,000, and S12 fires the new refusal. Source note `fastly2026limits.md` added, and `prolific2026help.md` records the assumed ID lengths. The LF9 block was appended with a shell heredoc. Playwright: 966 passed.
- 2026-10-01: T8 done. README, the article and NEWS state the refusal at 8,192 characters after the host name. They say that a setup holds what fits in 100,000 bytes, and that a link carrying its setup must also fit the host. The row-size wording says one page of 8,192 bytes, and that a shorter answer stays whole in the row. Five README test rows are updated. AC4 search over 21 paths and 852 paragraphs: the same 2 hits, neither a limit on the number of questions. Playwright 966 passed. `pkgdown::build_article()` rendered. `devtools::check()`: 0 errors, 0 warnings, 0 notes.
- claim audit: 104 claims read, 4 corrected — hitop-form README.md, link.html, tests/link.spec.js, tests/link-setupfile.spec.js, tests/link-sections.spec.js. The Prolific count now names 24 as the participant ID's length, with the other two assumed. The L38 row says a site is tried only where it can reach a length. The questions hint reads "As many as the setup's size allows." The S12 header names the host refusal. The reader's re-read found all 4 hold. A fifth point, that the warning advises a hosted file even for a setup-file link, is AC3's fixed text and stays review finding 11. Playwright after the fixes: 966 passed.
- 2026-10-01: implement done, status review.
- 2026-10-01: review pass 2 checkpoint, half done. AC1-AC4 and AC6 verified and ticked. AC5 waits on `devtools::check()`, and the diff-bug reviewer is still running.
- 2026-10-01: review pass 2 pre-gate checkpoint. All six criteria ticked against pass-2 evidence, the consistency gate passed, and 18 reviewer findings are recorded for triage. None shows a criterion failing. Defect returns so far: 1.
- step-7 approval: m153-question-limit-link-length approved for merge, with the companion /Users/jmgirard/github/hitop-form m153-question-limit-link-length, companion first. D-087 written for P8.

## Decisions

## Review

Pass 1, 2026-10-01. Both branches were current with `origin/main`. The pass stopped at the consistency gate. The criterion boxes stay unticked, because the gate failed and the re-review runs every criterion again.

Evidence gathered before the stop:

- AC1: `QUESTIONS_MAX` is in no file of hitop-form. `checkQuestions()` reads list lengths only to refuse an empty list. The full Playwright suite passed locally (961 of 961). It includes Q2, which sends 51 and 200 questions through `parseLink()`, and the editor and file tests at 51 and 200. It also includes Z6 and LQ5's `c` test at 100,000 and 100,001 bytes. The `encodeLink()` message matches AC1's text.
- AC2: LQ7 and the S12 probe passed. `storeSql()` builds its columns from `storeColumns()`, and the refusal counts the same list.
- AC3: both L37 tests and the clearing test passed. The warning text matches AC3's text.
- AC4: a paragraph search of 21 files and 851 paragraphs found 2 hits. One is the README test table, which says 51 and 200 questions are accepted. The other is the HiTOP-HSUM page's 650 items. Neither states a limit on the number of questions. The README and the article's questions sections state the 100,000-byte limit, the 1,600-column limit and the row-size limit.
- AC5: NEWS has the entry. Playwright passed locally. `devtools::check()` gave 0 errors, 0 warnings and 0 notes. `pkgdown::build_article()` was not run in this pass.

Consistency gate: `cairn_validate` exited 1 with FAIL `references index<->disk`. The provenance of `rfc9110.md` and `postgresql2026limits.md` says "Read 2026-10-01", and the check needs an ingested date. `document()` left no diff. `pkgdown::check_pkgdown()` found no problems. README.Rmd did not change.

This session and the diff-bug reviewer each measured the host. GitHub Pages hosts the online form, and it returns HTTP 414 for a request line over about 8,192 bytes. With `https://jmgirard.github.io/hitop-form/?c=`, a payload of 8,171 characters loads and one of 8,187 is refused. At 70,000 characters the connection drops.

Findings from the three reviewers, most severe first. None is triaged yet. The re-review gate triages them.

1. (diff-bug 1, prior-review 1 and 2) The form's host refuses a link over about 8,200 characters. So a link the builder makes under the 100,000-byte limit can fail to open for every participant. The warning blames "some sites and mail programs", and README, NEWS and the article say a link holds what fits in 100,000 bytes. Repair is a design choice. Three examples: a refusal at the host limit, a warning that names the host, or a setup carried after `#`. A browser does not send the part after `#` to the server.
2. (diff-bug 2, prior-review 1) Z6 and LQ5's `c` test open a `?c=` of about 133,000 characters. The scheduled and manual CI runs use the deployed page, so these 3 tests fail there. `tests/serve.mjs` raises the local header limit to 256 KiB, which hides what the host does. LESSONS.md (M125) records that a page host refuses long request lines.
3. (diff-bug 3, blame 1, prior-review 3) `link.html:934` reads an opened `c` link with `decodeConfig()`, with no 100,000-byte check. `decodeLink()` refuses the same link, so the builder and the form disagree.
4. (diff-bug 4) "Download the setup file" has no column check, and it measures indented JSON while `encodeLink()` measures compact JSON.
5. (blame 2) A questions CSV of any size now builds one editor group per row. The 50-question cap was the only bound on that load.
6. (diff-bug 5, prior-review 5) `#long` has no live-region role, so a screen reader does not announce the warning.
7. (diff-bug 6, prior-review 4) The line above the warning shows "(8001 characters)", and the warning shows "8,001".
8. (blame 3) A comment in `readQuestions()` and a `group !== undefined` guard still describe a fault with no question, which the editor can no longer make.
9. (diff-bug 9, prior-review 2) To find both lengths, L37 needs a base address whose length fits its mod-4 rule. It holds for the local server and for GitHub Pages.
10. (diff-bug 8) The prose says a row must fit in 8,192 bytes, which is the page size. The usable space is a little less, and the pressure comes from the 18-byte pointers of many text columns.
11. (blame 6) For a link that is already a `setup=` link, the warning also suggests a hosted file. Such a link is short, so this is unlikely.
12. (prior-review 6) New article sentences call the online form "the page", which the open wording candidate row covers.
13. (blame 4) S12's probe of a refusal with no question control is gone. The column and fetch probes still cover a refusal with no focus.
14. (blame 7) No D-entry records the removed limit or the two new refusals.
15. (diff-bug 7) A `z` refusal adds "Shorten the consent text or the questions." after AC1's quoted sentence.
16. (diff-bug 10) The T5 work-log line quotes a hint that the claim audit later changed.

Pass 2, 2026-10-01. Both branches were current with `origin/main`, and no PR existed for either. The full Playwright suite passed locally, 966 of 966.

- AC1: no file in hitop-form's form.js, link.html, index.html, README.md or tests/ holds `QUESTIONS_MAX`. `checkQuestions()` reads list lengths only to refuse an empty list. `encodeLink()` measures every setup and throws AC1's sentence over 100,000 bytes. `decodeLink()` refuses a `c` over 100,000 decoded bytes, naming the size and the limit. Q2 (51 and 200 questions through `parseLink()`), the editor and file tests at 51 and 200, Z6 and LQ5's `c` tests passed in the 966.
- AC2: link.html counts `storeColumns()`, the list `storeSql()` builds from, and refuses over `POSTGRES_COLUMNS_MAX` (1,600), naming the count. LQ7 (1,600 built, its SQL counted, and 1,601 refused) and the S12 probe passed in the 966.
- AC3: if the link passes `LINK_WARN_LENGTH` (8,000), link.html shows `#long` with AC3's sentence. Each build empties and hides it. L37 pads the study name and a 64-character parameter name at run time. It builds links of exactly 8,000 characters (no warning) and 8,001 (the sentence, with "8,001"). Both passed in the 966, with the test that a shorter link clears the warning.
- AC4: a fresh paragraph search (split on blank lines) of 21 files and 852 paragraphs found 2 hits. One is the README test table, which says 51 and 200 questions are accepted. The other is the HiTOP-HSUM page's 650 items. Neither states a limit on the number of questions. NEWS.md keeps its released entries. The README's questions section and the article's state the 100,000-byte limit, the 1,600-column limit and the row-size limit.
- AC6 (recorded before AC5, which waited on `devtools::check()`): link.html counts the path, the query and the site ending, each Prolific placeholder as `PROLIFIC_ID_LENGTH` (24). Over `HOST_PATH_MAX` (8,192) it refuses with AC6's sentence, N being `href.length`, and hides `#result`. For a setup-file link the refusal ends after "accepts.". L38 (no site, SONA and Prolific) and LF9 (setup file, with and without Prolific) build 8,192 and refuse 8,193, and passed in the 966. `fastly2026limits.md` traces 8,192 to Fastly's 8 KB limit and the 2026-10-01 curl. A fresh curl the same day gave 200 at 8,192 characters five times and 414 at 8,193. One earlier try at 8,192 gave a single 400.
- AC5: NEWS.md's new bullet says the 50-question limit is gone and names the long-link warning. hitop-form's Playwright suite passed, 966 of 966. `devtools::check()` gave 0 errors, 0 warnings and 0 notes. `pkgdown::build_article("articles/online-collection")` rendered and exited 0.

Consistency gate, pass 2: `cairn_validate` exited 0, with 24 advisory warnings that predate this milestone. `devtools::document()` left no diff. `pkgdown::check_pkgdown()` found no problems. README.Rmd did not change, and the branch adds no top-level file. No DESIGN.md principle changed, so `cairn_impact` was skipped.

Pass 2 findings, from the three reviewers, most severe first. Each was read against the code. Pass 1 findings 1, 2, 3, 6 and 7 are fixed, and 8 is fixed in its comment.

- P1. (diff-bug 1) The host count leaves out parameters a recruiting site adds. Prolific's "I'll use URL parameters" option appends its three IDs again, about 108 more characters (README, Prolific section). No source says whether Connect appends its own. So a Prolific link the builder makes at 8,085 to 8,192 characters can get a 414 for every participant.
- P2. (diff-bug 2, pass 1 finding 4) "In a file I host" can lead nowhere. "Download the setup file" measures JSON with 2-space indents, which the reviewer measured at 150,479 bytes for a setup of 84,451 compact bytes. A setup refused for its length can be refused again at the download. That refusal says "Shorten the consent text or the questions" even for a setup with neither. README, the article and NEWS imply the file holds the full 100,000-byte setup.
- P3. (diff-bug 3, blame 5) The 24 characters for Prolific's study and session IDs have no source, and the tests repeat the same 24. The 8,192 rests on Fastly's "8 KB" and one day's measurement, with no planned re-check.
- P4. (blame 2, diff-bug 5, pass 1 finding 11) The warning now shows only from 8,001 characters to the refusal at 8,192 after the origin. Its text tells a setup-file link to use a hosted file, and LF9 builds such a link at about 8,190 characters.
- P5. (diff-bug 4) SONA's `%SURVEY_CODE%` is counted as its 13 characters, and SONA puts a code of 2 to 7 in its place. A SONA link up to 11 characters under the limit is refused, on the safe side.
- P6. (blame 6) The refusal's N is the whole link, origin included, and the limit is counted after the origin. A reader sees a link under 8,192 refused for being over a limit stated as 8,192. This is AC6's wording.
- P7. (diff-bug 6) Z6's 100,001-byte test and link.spec's `c` 100,001 test fail against the deployed page until this branch deploys, because `gotoLong()` serves the deployed page.
- P8. (blame 4, pass 1 finding 14) No D-entry records the removed limit, the column refusal or the host refusal. D-079(b) and D-086(d) say a link that carries its whole setup stays the default, and the host refusal now limits what that link can carry.
- P9. (prior-review 1, diff-bug 10, LESSONS M148) `#long` is filled and shown in one step, while `#result` opens and focus moves to its heading. A screen reader can skip announcing it.
- P10. (blame 3, pass 1 finding 5) A questions file of any number of rows builds one editor group per row. Nothing bounds that load.
- P11. (diff-bug 8) NEWS and the article do not say that a Prolific link counts each ID as 24 characters. The README does.
- P12. (diff-bug 7) `HOST_PATH_MAX` applies wherever link.html is served, a fork on another host included.
- P13. (diff-bug 9) The builder's size check on an opened `c` skips a value with `=` padding, and gives a size message for a malformed length. `decodeLink()` still refuses both.
- P14. (diff-bug 11, prior-review 3, pass 1 finding 8) The `group !== undefined` guard in `readQuestions()` can no longer be false.
- P15. (prior-review 2, pass 1 finding 12) New README and article sentences call the online form "the page".
- P16. (diff-bug 12, pass 1 finding 9) L37 finds its lengths only from some base addresses, and fails with a message where it cannot.
- P17. (blame 7) The 100,000-byte `c` tests answer the long address in the browser, so they test no real host.
- P18. (diff-bug, pass 1 finding 10) README, the article and form.js:1292 still say a row must fit in one page of 8,192 bytes. The usable space is a little less.
- Pass 1 findings 13, 15 and 16 stand as recorded.

Pass 2 triage, Jeff at the merge gate, 2026-10-01. Follow-up: P1, P3, P4, P5 and P9 go to a new high-priority candidate row, and P2 joins the "Hosted setup file gaps" row. Fix now: P8, D-087 written on the branch. Noted: P7 clears when main deploys, P10 is in the question-gaps row, P15 is in the "the page" row, P16 fails with a message, and P17 follows the M125 lesson. Rejected: P6 is AC6's chosen wording, P11 is stated in the README, P12 is outside the hosted form, P13 is refused by `decodeLink()`, P14 is harmless, and P18 is precise enough for a researcher. Pass 1 findings 13, 15 and 16 are rejected: other probes cover 13, AC1's sentence starts 15's message, and 16 is history.
