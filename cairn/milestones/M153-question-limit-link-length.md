# M153: Questions limited by size, and a long-link warning

- **Status:** planned
- **Priority:** high
- **Depends on:** M152
- **Driving RR:** —
- **Principles touched:** IP2
- **Resolves:** —
- **Surface tier:** user-facing — changes what a study link may hold and what the Study Link Builder shows
- **Branch/PR:** —

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

- [ ] T1: In `form.js`, remove `QUESTIONS_MAX` from `checkQuestions()`, and add the `c` size checks to `encodeLink()` and `decodeLink()`.
- [ ] T2: Replace the 51-question tests in `questions.spec.js`, `link-questions.spec.js` and `link-questions-file.spec.js`, and add the `c` size tests.
- [ ] T3: Add the Supabase column check to `link.html`'s build, with tests. Write a hitop source note on PostgreSQL's Appendix K limits.
- [ ] T4: Add the link-length warning to the result region, with the padded-length tests. Write a hitop source note on RFC 9110 section 4.1.
- [ ] T5: Update the link.html hint, the README's questions section and test rows, and the hitop article. Run AC4's paragraph search and read each hit.
- [ ] T6: hitop NEWS line, `pkgdown::build_article()`, `devtools::check()`, and the companion PR.

## Work log

- 2026-10-01: created by /milestone-plan with M152 and M154. The 50-question cap came from M137's plan with no recorded reason; the measured limit is the 100,000-byte setup. Audits are recorded in M152's work log.
- 2026-10-01: plan chose a warning at 8,000 characters, the length RFC 9110 recommends supporting, over a 2,000-character warning, which has no current standard behind it. Falsified by a recruiting site or mail program that cuts links shorter than 8,000 characters.
- 2026-10-01: plan chose a 100,000-byte limit on `c` setups, to match `z` and hosted files, over leaving `c` unlimited. Falsified by a researcher whose `c` link over 100,000 bytes worked before.

## Decisions

## Review
