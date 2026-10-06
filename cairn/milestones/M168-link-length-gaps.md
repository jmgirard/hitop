# M168: Study link length-check gaps

- **Status:** in-progress
- **Priority:** high
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** IP2, GP3
- **Resolves:** —
- **Surface tier:** user-facing — the Study Link Builder's refusals, warning and documentation that researchers read
- **Branch/PR:** m168-link-length-gaps; companion: /Users/jmgirard/github/hitop-form m168-link-length-gaps

## Goal

The Study Link Builder tells each researcher the true length of a study link, from sourced counts, in a line a screen reader announces.

## Scope

**In:** the five M153 review findings in the "Study link length-check gaps" candidate row. The work is in hitop-form `link.html` and its tests, with the matching README, article and NEWS text. P1: a Prolific link's count adds the three IDs that Prolific's `I'll use URL parameters` option appends. P3: a source for the 24 characters of each Prolific ID, and a weekly re-check of the host's 8,192. P4: the long-link line stops telling a setup-file link to use a hosted file. P5: SONA's `%SURVEY_CODE%` counts as the 7 characters SONA's documentation gives as its longest code. P9: the long-link line is filled after the result section opens, so a screen reader announces it.

**Out:**
- P2 (the setup-file download measures indented JSON) stays in the "Hosted setup file gaps" row.
- Whether CloudResearch Connect appends parameters has no source. It stays the open question in `cairn/references/fastly2026limits.md`.
- The other screen-reader items of the Study Link Builder stay in the "Study Link Builder failure-path gaps" row.
- The warning's 8,000 and the refusal's 8,192 do not move, except where the re-measurement in T5 differs (D-087's reopening evidence).

## Acceptance criteria

- [ ] AC1: For a Prolific link, the builder counts the path and query, each of the three printed placeholders as 24 characters, and 108 characters more. The 108 is an upper bound for Prolific's `I'll use URL parameters` option appending all three parameters, each as `&NAME=` and 24 characters. Article 445178 says that the option appends URL parameters, but not which or with what separator (`prolific2026help`, open questions). The refusal message names the counted length. A test counts apart from the builder. It makes a Prolific setup-file link counting `HOST_PATH_MAX`, and is refused one counting `HOST_PATH_MAX` + 1.
- [ ] AC2: For a SONA link, the builder counts `%SURVEY_CODE%` as 7 characters, the longest code SONA's documentation gives (`sona2026help`, *Using the SURVEY CODE Feature*). A test counts apart from the builder. It makes a SONA setup-file link counting `HOST_PATH_MAX`, and is refused one counting `HOST_PATH_MAX` + 1.
- [ ] AC3: `cairn/references/prolific2026help.md` quotes three example IDs from Prolific's API reference page "get submission", with the date read: a submission, a study and a participant ID. It also quotes the page's sentence that the submission ID is the ID passed as `%SESSION_ID%`. Each example is 24 characters. Three places cite that page for all three IDs: the comment beside `PROLIFIC_ID_LENGTH` in `link.html`, the hitop-form README and hitop's `vignettes/articles/online-collection.Rmd`. Each of the three says that the Prolific pages the note lists state no maximum length.
- [ ] AC4: `cairn/references/fastly2026limits.md` records a re-measurement of the deployed online form made during this milestone, with its date: the path-and-query lengths tried and the status each got. The lengths tried include `HOST_PATH_MAX`, answered 200, and `HOST_PATH_MAX` + 1, answered 414. Every shorter length tried got 200, and every longer one got 414.
- [ ] AC5: For a link that names a setup file, the long-link line gives the length and says that the setup file's address makes the link long. It does not say to keep the setup in a file. For a link that carries its setup, the line is as before. A test reads the line's text for each of the two kinds.
- [ ] AC6: When "Make the link" shows a link over 8,000 characters, `#long` is unhidden and empty in the same animation frame in which `#result` opens. Its text is written at least two frames later. A test uses a frame counter and a `MutationObserver` to record two frames: the one in which `#result` opens and the one in which `#long` gets its text. It asserts the gap. A field changed one frame after `#result` opens leaves `#long` without the earlier link's text, and a test makes that change.
- [ ] AC7: The hitop-form README, hitop's `online-collection.Rmd` and hitop's NEWS.md state the Prolific count of AC1 and the SONA count of AC2. hitop-form's `npx playwright test` passes with no failed test. hitop's `pkgdown::check_pkgdown()` passes, and `devtools::check()` reports 0 errors and 0 warnings.

## Coverage

- AC1 → T2
- AC2 → T2
- AC3 → T1, T6
- AC4 → T5
- AC5 → T3
- AC6 → T4
- AC7 → T6, T7

## Tasks

- [x] T1: Read Prolific's API reference page "get submission" (https://docs.prolific.com/api-reference/submissions/get-submission) and update `cairn/references/prolific2026help.md`: the three example IDs, the `%SESSION_ID%` sentence, the date read, and the open question narrowed to "no listed page states a maximum length".
- [x] T2: Tests first in `tests/link-setupfile.spec.js`: Prolific and SONA setup-file links at `HOST_PATH_MAX` and `HOST_PATH_MAX` + 1, each count made apart from the builder as LF9 does. Then change the count at `link.html:1563-1565` to take each site's added characters. Prolific's placeholders count at `PROLIFIC_ID_LENGTH` plus the appended 108, and SONA's placeholder at a new constant of 7. Each constant's comment cites its source. The refusal names the counted length. Update L38, LF9 and the other tests whose lengths move.
- [ ] T3: Test first: the long-link line's text for a setup-file link and for a link carrying its setup. Then branch the text at `link.html:1579-1582`.
- [ ] T4: Test first, with a frame counter and a `MutationObserver` log (LESSONS M148), and plant a same-frame write to see it fail. Then unhide `#long` empty in the frame that opens `#result`, and write its text two frames later. A later build or `hideResult()` cancels a pending write. Test the cancel with a field change one frame after `#result` opens.
- [ ] T5: Re-measure the deployed host with the `curl` loop `fastly2026limits.md` describes (8,185 to 8,215) and record it there. Add a test that repeats the 8,192 and 8,193 requests in the weekly run. It runs only on a run whose `FORM_TARGET` names the deployed page. Plant 8,191 as the expected limit to see it fail.
- [ ] T6: Update the hitop-form README's length paragraphs (`README.md:211-225`, `:252-256`), hitop's `online-collection.Rmd` and NEWS.md, and the `link.html` comments. Write a D-entry annotating D-087(d) with the new Prolific and SONA counts.
- [ ] T7: Run hitop-form's full Playwright suite, and hitop's `pkgdown::check_pkgdown()` and `devtools::check()`.

## Work log

- 2026-10-06: created by /milestone-plan, promoting the "[high] Study link length-check gaps" candidate row (lineage M153 review P1, P3 to P5, P9). Collision sweep: the "Hosted setup file gaps" and "failure-path gaps" rows stay distinct (Out). D-087(d) is annotated, not superseded (T6). Inbox: 1 open issue (jmgirard/hitop#87), no overlap. No PR is open.
- 2026-10-06: question set: what to plan, with M144 already workable. Jeff chose "Study link length gaps" over HiTOP-DAT Titanium, HiTOP-DAT scoring and issue #87. No permission, file or access was granted or needed.
- 2026-10-06: criteria audit (full mode, fresh Opus reader) returned 12 points, all disposed. AC1 now calls 108 an upper bound, because article 445178 names neither the parameters nor the separator. AC1 and AC2 test setup-file links at `HOST_PATH_MAX` and one more, because a `c` link cannot reach every length and T5 can move the limit. AC1 makes the refusal name the counted length. AC4 states the measured lengths in order. AC6 adds the cancel of a pending write. AC7 names hitop's NEWS.md. The weekly deployed-host test stays task T5 with its plant. It is not a criterion, because it checks the deliverable (the narrower promise). AC2, AC3 and AC5 had no finding. SOURCES.md is unchanged, because the three reference notes are the repo's records for web sources.
- 2026-10-06: plan gate chose to count Prolific's appended IDs on every Prolific link over a checkbox that asks whether the option is on, because a researcher does not always know the setting and the refusal errs on the safe side by 108 characters; falsified by a researcher refused a link that Prolific's real URL carries.
- 2026-10-06: plan gate chose a deployed-only test in the existing weekly run over a new scheduled workflow, because the weekly run already targets the deployed page; falsified by the weekly run being dropped or no longer targeting the deployed page.
- 2026-10-06: implement started. Branch `m168-link-length-gaps` cut in hitop and in the hitop-form companion. Untracked `devel/hitopdat_*` files stay unstaged (not this milestone's).
- 2026-10-06: T1 done. `prolific2026help.md` adds the *Get submission* example IDs (24 characters each, no stated maximum) and article 445133, with the open questions corrected in place.
- 2026-10-06: T2 done (hitop-form a922ac6). `hostCount()` and `hostRefusal()` moved to `tests/helpers.mjs`. LF9 now runs for no site, SONA and Prolific, and failed first for the expected reasons: the old message, SONA refused at 8,192, Prolific made at 8,193. `link.html` adds `PROLIFIC_APPENDED_LENGTH` (derived from `PROLIFIC_PARAMS`, 108) and `SONA_CODE_LENGTH` (7). The refusal now reads "This link counts N characters after the host name…", which also answers M153's rejected P6. The four builder test files pass (353).

## Decisions

## Review
