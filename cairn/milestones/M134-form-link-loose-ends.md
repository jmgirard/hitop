# M134: A link made under "Another site" reloads as that site, and the form page refuses a broken identifier before the form starts

- **Status:** review
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** GP3, IP1
- **Resolves:** —
- **Surface tier:** user-facing — the deployed form page and its link builder
- **Branch/PR:** `m134-form-link-loose-ends`; companion: /Users/jmgirard/github/hitop-form m134-form-link-loose-ends

## Goal

Under "Another site" the builder refuses `id` and `participantId`, and the form page refuses a broken identifier before the form starts.

## Scope

**In:** hitop-form: the builder refusal of `id` and `participantId` under "Another site" in link.html. A check in form.js that refuses an identifier with an unpaired surrogate, from the link's `participant` field and from the start screen. The test gaps from the M133 review: L27 does not check the rebuilt link's ending, and P13 checks only the saved screen. The README, and the stale line at `tests/fixtures/README.md:13`. hitop: one NEWS entry.

**Out:** A limit on SONA's Study URL length for a long `c`. SONA's help pages state no limit, so it stays a candidate row. A site field in the link and a replacement of the broken character were rejected at the plan gate (work log). The reader, `read_form_responses()`, is unchanged, because no column changes. Instrument content (IP1) is untouched. The refusals are page copy, on the reading of D-072(c).

## Acceptance criteria

- [x] AC1: link.html refuses "Another site" with the address parameter `id`, `participantId` or either name padded with spaces, and prints no link. For `id` the message is `The address parameter could not be used: "id" is the name SONA fills. For a SONA study choose SONA as the recruiting site.` For `participantId` it names `"participantId"` and CloudResearch Connect in the same form. The form page still opens a link with `participantParam: "id"`. Tests: `id`, `participantId` and `" id "`, each with the full message and an empty link.
- [x] AC2: A link loaded on link.html through `?c=` for each of the five recruiting-site choices is rebuilt through "Make the link". The printed link's text after the `c` value is the ending of that choice. Prolific ends in its three placeholders and SONA in `&id=%SURVEY_CODE%`. None, CloudResearch Connect and another site end in nothing. Tested in L27.
- [x] AC3: Under `participantParam`, a completion address without `{participant}` is used as the link check returns it, which is the parsed address's `href`. This holds at the navigation after a confirmed send, at the sent screen's link and at the saved screen's link. A `completeSaved` without the token is linked the same way on the saved screen. Tested in P13, each use against the exact expected address, with one address that the parse changes (`https://Example.org/done?x=1`).
- [x] AC4: The page refuses a link whose `participant` holds an unpaired surrogate, and shows no Begin button. An unpaired surrogate is a UTF-16 code unit in U+D800 to U+DFFF without its partner. The message is `The study link carries a participant identifier with a character that cannot be written.` Tests: a lone high and a lone low surrogate, each alone and at the start, middle and end of an identifier. Also a low before a high (`\udc00\ud800`) and a high before a valid pair (`\ud800` then U+1F600). Each test checks the full message and the absent Begin button. Control: an identifier with U+1F600 is accepted, and its completion address holds `%F0%9F%98%80`.
- [x] AC5: The start screen refuses an entered identifier with an unpaired surrogate, and the form does not start. The message is `Your participant identifier holds a character this page cannot read. Please type it again.` Tests: a lone high, a lone low and `\udc00\ud800`, each by the full message and by no first item page. An address value `%ED%A0%80` under `participantParam` reaches the page as three U+FFFD characters. The sent screen then draws, and its link holds `%EF%BF%BD` three times.
- [x] AC6: The hitop-form README states the builder refusal of AC1 and the identifier refusals of AC4 and AC5. `git grep -n "Recruit through Prolific"` in hitop-form returns no line. hitop NEWS.md has one entry that names the builder refusal and the identifier refusal.

## Coverage

- AC1 → T1
- AC2 → T2
- AC3 → T3
- AC4 → T4
- AC5 → T4
- AC6 → T5

## Tasks

- [x] T1: In link.html's "Another site" branch (link.html:390-411), refuse `id` and `participantId` after the trim, before `checkParticipantParam()`. Keep the refusal out of `checkParticipantParam()`, because form.js:137 will then refuse SONA's own links. Add the AC1 tests beside L26 in `tests/link.spec.js`, plus one page open of a `participantParam: "id"` link.
- [x] T2: In L27 (`tests/link.spec.js:959`), assert the printed link's text after the `c` value for each choice (AC2).
- [x] T3: Extend P13 (`tests/recruit.spec.js:310`) to a confirmed send, for the navigation and the sent screen's link, and to a `completeSaved` without the token. Use `https://Example.org/done?x=1` as one address (AC3).
- [x] T4: Add one check in form.js (`isWritableIdentifier()`, which tries `encodeURIComponent()`) that `parseLink()` (form.js:104-110) and the start screen (form.js:858) both call. Add the AC4 tests in `tests/guard.spec.js` and the AC5 tests in `tests/recruit.spec.js`. Set the start-screen field through `page.evaluate`, because Playwright's `fill()` replaces a surrogate with U+FFFD. Read `.value` back before Begin (LESSONS, M128). `readProlific()` uses the same URLSearchParams reading as `readParticipantParam()`, so the `participantParam` address case stands for both.
- [x] T5: The hitop-form README: the builder section, the SONA and Connect section and the test table. `tests/fixtures/README.md:13`: name Prolific as the recruiting site. hitop NEWS.md: one entry (AC6).
- [x] T6: Run the full hitop-form Playwright suite and `cairn_validate` in hitop. Before a plant is restored with `git checkout`, `git add` the fix (LESSONS, M118).

## Work log

- 2026-09-28: created by /milestone-plan, from the M133 review's loose-ends candidate row.
- 2026-09-28: criteria audit (full mode, fresh [O] reader): no principle conflict, and the three identifier sources are complete. Five findings, all fixed before the gate. AC1 gained a padded name, and AC3 the parsed `href` and `completeSaved`. AC4 and AC5 gained the wrong-order forms, pinned messages and `page.evaluate`. AC6 names the refusals NEWS states.
- 2026-09-28: plan gate chose a builder refusal of `id` and `participantId` under "Another site" over a site field in the link. A new field changes the link format for a case a menu choice covers. Falsified by a researcher on a site other than SONA that needs `id` without the SONA ending.
- 2026-09-28: plan gate chose to refuse an identifier with an unpaired surrogate at intake over a replacement in the completion address. A replacement lets the saved row and the completion address hold different identifiers. Falsified by a participant whose real identifier the refusal blocks.
- 2026-09-28: T1 done (hitop-form). link.html refuses `id` and `participantId` under "Another site" after the trim, with the site's label from `SITES`. New L32 (3 tests) was red with an empty message before the fix. `tests/link.spec.js` 103 passed. The page open of an `id` link is L28, already on main, so no new open test was added.
- 2026-09-28: T2 done (hitop-form). L27 asserts the printed link equals its bare part plus each case's ending. A plant of an empty SONA ending turned the SONA case red, 5 others green. 6 passed after the restore.
- 2026-09-28: T3 done (hitop-form). P13 is now 4 tests: a confirmed send (navigation and the sent screen's link at the held request), and the saved screen's link to `complete`, to `complete` as `https://Example.org/done?x=1`, and to `completeSaved`. A plant appending the identifier to an address without the token turned all 4 red, and P14 too. `tests/recruit.spec.js` 23 passed after the restore.
- 2026-09-28: T4 done (hitop-form). `isWritableIdentifier()` tries `encodeURIComponent()`, and `parseLink()` and the start screen call it. Minor task edit: this replaces `isWellFormed`, so a browser without that method does not break, and the check refuses exactly what the fill throws on. New G17 (10 refusals, 1 control), P15 (3) and P16 (1). The 13 refusal tests were red before the fix, and the control and P16 passed. A count-only plant (as many high as low) turned exactly the 2 low-before-high cases red.
- 2026-09-28: re-audit: AC4 (full) — nothing. The reader noted T4's "before Start", fixed as a minor task edit.
- 2026-09-28: amendment (mini gate, Jeff): AC4's "Start button" became "Begin button" in both places, because the page's button is labelled Begin and no Start button exists.
- 2026-09-28: T5 done. hitop-form README: the "Another site" paragraph, an identifier paragraph under "What the participant sees", and the link, guard and recruit rows of the test table. `tests/fixtures/README.md:13` names Prolific as the recruiting site, and `git grep -n "Recruit through Prolific"` in hitop-form returns no line. hitop: one NEWS entry under "Improvements and fixes", and one sentence in the online-collection article's "Another site" paragraph (added beyond the task, so the article states the refusal too).
- 2026-09-28: claim audit: 60 claims read, 5 corrected — hitop-form README.md, form.js, tests/recruit.spec.js. Corrected: the Another-site sentence's cause, the U+FFFD count ("one or more"), two unsupported "paste" claims, and the P13 header and README row (two uppercase hosts, saved screen after a walk with no store). Re-read once by the same reader: all right.
- 2026-09-28: T6 done. hitop-form full Playwright suite 364 passed (after T5). hitop `devtools::test()` no failures, 15 skips, all merge-base skips. `cairn_validate` all checks passed, 24 advisory warnings as on main. Status set to review.

## Decisions

## Review

Sync 2026-09-28: both branches contain their `origin/main` (hitop and hitop-form, fetched), so no merge. hitop-form full Playwright suite: 364 passed (1.3m).

- AC1: L32's three tests pass (`"id"`, `" id "`, `"participantId"`), each asserting the full pinned message with `toBe` and an empty `#out`. L28 passes: the SONA link, whose `c` carries `participantParam: "id"`, opens on the form page to the identifier question. Code read: the refusal runs after the trim and before `checkParticipantParam()` (link.html:397-401).
- AC2: L27's six cases pass: no site, Prolific, SONA, Connect, another site, and a non-text `participantParam`. Each asserts that the printed link equals `bare(printed)`, which is the origin, path and `?c=` value, plus the case's ending. Prolific's ending is `PLACEHOLDERS` (its three placeholders), SONA's is `&id=%SURVEY_CODE%`, and the rest are empty.
- AC3: P13's four tests pass. After a confirmed send with `complete: https://Example.org/done?x=1`, the held navigation request and the sent screen's link are both `https://example.org/done?x=1`, written out in the test. The saved screen links to `COMPLETE_URL` unchanged. It links to `https://Example.org/done?x=1` and to a `completeSaved` without the token in their lowercased forms.
- AC4: G17's eleven tests pass. Eight refusals put a lone high or a lone low surrogate alone and at the start, middle and end. Two more are `\udc00\ud800` and `\ud800` before U+1F600. Each asserts the full pinned message and zero Begin buttons. The control, `p` plus U+1F600, starts the form, and its saved screen links to `https://example.org/done?code=p%F0%9F%98%80`.
- AC5: P15's three tests pass (a lone high, a lone low, `\udc00\ud800`). Each sets the field through the page, reads the code points back, presses Begin, and asserts the full pinned message and no item card. P16 passes: `&id=%ED%A0%80` draws no identifier field, and the sent screen links to the address with `%EF%BF%BD` three times.
- AC6: The hitop-form README's "Another site" paragraph states the `id` and `participantId` refusal. A new paragraph under "What the participant sees" states both identifier refusals, the link's and the start screen's. `git grep -n "Recruit through Prolific"` in hitop-form exits 1 with no line. hitop NEWS.md has one new entry, under the development heading's "Improvements and fixes", that names both refusals.

Consistency gate 2026-09-28: `cairn_validate` all checks passed, with 24 advisories as on main. `devtools::document()` gives no diff. `pkgdown::check_pkgdown()` finds no problems. `devtools::check()` gives 0 errors, 0 warnings and 0 notes. No R code, README, DESCRIPTION or top-level file changed. The NEWS entry is present. No principle text changed, so `cairn_impact` was skipped.

Independent review: three fresh reviewers, an [O] diff reviewer, an [S] history reviewer and an [S] prior-review reviewer. The prior-review reviewer found no regression of an archived finding or a lesson, and both GitHub comment probes were empty. The findings, most severe first, with the dispositions proposed at the gate:

- F1 (diff): the builder does not check its own participant field. A hand-made `c` whose `participant` holds a lone surrogate prefills the field. `utf8ToBase64url()` uses `TextEncoder`, which writes U+FFFD, so "Make the link" prints a link the form page accepts. This is the silent replacement the plan gate rejected. Verified at form.js:51. Proposed: fix now.
- F2 (diff): the builder refusal matches exact names, so `ID` or `participantid` under "Another site" is accepted. Proposed: reject, because another site can use such a name and the round trip stays consistent.
- F3 (diff): NEWS describes the old failure only with a completion URL. A link with a broken identifier and no completion URL used to save a file and is now refused. Proposed: fix now, one NEWS sentence.
- F4 (diff and history): link.html:400 reads and trims `participantParam` for SONA and Connect too. If a later edit disables the field, that read throws. Proposed: fix now, reading it only under "Another site".
- F5 (diff): the P13 send test and P16 copy `serveCompletion()`'s route-and-hold code for `example.org`. Proposed: reject, a test-style point.
- F6 (diff): P16 does not assert the page's address after `release()`, where P8 and P13 do. Proposed: fix now, one line.
- F7 (diff): P13 checks the saved screen only after a walk with no store, not after an unconfirmed send. Proposed: reject, because both paths call `showSaved()` and AC3 is met.
- F8 (history): the builder refusal has no D-entry. Proposed: reject, because the plan gate's work-log line records the choice and Scope records the D-072(c) reading.
