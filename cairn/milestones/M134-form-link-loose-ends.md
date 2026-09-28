# M134: A link made under "Another site" reloads as that site, and the form page refuses a broken identifier before the form starts

- **Status:** in-progress
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

- [ ] AC1: link.html refuses "Another site" with the address parameter `id`, `participantId` or either name padded with spaces, and prints no link. For `id` the message is `The address parameter could not be used: "id" is the name SONA fills. For a SONA study choose SONA as the recruiting site.` For `participantId` it names `"participantId"` and CloudResearch Connect in the same form. The form page still opens a link with `participantParam: "id"`. Tests: `id`, `participantId` and `" id "`, each with the full message and an empty link.
- [ ] AC2: A link loaded on link.html through `?c=` for each of the five recruiting-site choices is rebuilt through "Make the link". The printed link's text after the `c` value is the ending of that choice. Prolific ends in its three placeholders and SONA in `&id=%SURVEY_CODE%`. None, CloudResearch Connect and another site end in nothing. Tested in L27.
- [ ] AC3: Under `participantParam`, a completion address without `{participant}` is used as the link check returns it, which is the parsed address's `href`. This holds at the navigation after a confirmed send, at the sent screen's link and at the saved screen's link. A `completeSaved` without the token is linked the same way on the saved screen. Tested in P13, each use against the exact expected address, with one address that the parse changes (`https://Example.org/done?x=1`).
- [ ] AC4: The page refuses a link whose `participant` holds an unpaired surrogate, and shows no Start button. An unpaired surrogate is a UTF-16 code unit in U+D800 to U+DFFF without its partner. The message is `The study link carries a participant identifier with a character that cannot be written.` Tests: a lone high and a lone low surrogate, each alone and at the start, middle and end of an identifier. Also a low before a high (`\udc00\ud800`) and a high before a valid pair (`\ud800` then U+1F600). Each test checks the full message and the absent Start button. Control: an identifier with U+1F600 is accepted, and its completion address holds `%F0%9F%98%80`.
- [ ] AC5: The start screen refuses an entered identifier with an unpaired surrogate, and the form does not start. The message is `Your participant identifier holds a character this page cannot read. Please type it again.` Tests: a lone high, a lone low and `\udc00\ud800`, each by the full message and by no first item page. An address value `%ED%A0%80` under `participantParam` reaches the page as three U+FFFD characters. The sent screen then draws, and its link holds `%EF%BF%BD` three times.
- [ ] AC6: The hitop-form README states the builder refusal of AC1 and the identifier refusals of AC4 and AC5. `git grep -n "Recruit through Prolific"` in hitop-form returns no line. hitop NEWS.md has one entry that names the builder refusal and the identifier refusal.

## Coverage

- AC1 → T1
- AC2 → T2
- AC3 → T3
- AC4 → T4
- AC5 → T4
- AC6 → T5

## Tasks

- [x] T1: In link.html's "Another site" branch (link.html:390-411), refuse `id` and `participantId` after the trim, before `checkParticipantParam()`. Keep the refusal out of `checkParticipantParam()`, because form.js:137 will then refuse SONA's own links. Add the AC1 tests beside L26 in `tests/link.spec.js`, plus one page open of a `participantParam: "id"` link.
- [ ] T2: In L27 (`tests/link.spec.js:959`), assert the printed link's text after the `c` value for each choice (AC2).
- [ ] T3: Extend P13 (`tests/recruit.spec.js:310`) to a confirmed send, for the navigation and the sent screen's link, and to a `completeSaved` without the token. Use `https://Example.org/done?x=1` as one address (AC3).
- [ ] T4: Add one check in form.js (`String.prototype.isWellFormed`) that `parseLink()` (form.js:104-110) and the start screen (form.js:858) both call. Add the AC4 tests in `tests/guard.spec.js` and the AC5 tests in `tests/recruit.spec.js`. Set the start-screen field through `page.evaluate`, because Playwright's `fill()` replaces a surrogate with U+FFFD. Read `.value` back before Start (LESSONS, M128). `readProlific()` uses the same URLSearchParams reading as `readParticipantParam()`, so the `participantParam` address case stands for both.
- [ ] T5: The hitop-form README: the builder section, the SONA and Connect section and the test table. `tests/fixtures/README.md:13`: name Prolific as the recruiting site. hitop NEWS.md: one entry (AC6).
- [ ] T6: Run the full hitop-form Playwright suite and `cairn_validate` in hitop. Before a plant is restored with `git checkout`, `git add` the fix (LESSONS, M118).

## Work log

- 2026-09-28: created by /milestone-plan, from the M133 review's loose-ends candidate row.
- 2026-09-28: criteria audit (full mode, fresh [O] reader): no principle conflict, and the three identifier sources are complete. Five findings, all fixed before the gate. AC1 gained a padded name, and AC3 the parsed `href` and `completeSaved`. AC4 and AC5 gained the wrong-order forms, pinned messages and `page.evaluate`. AC6 names the refusals NEWS states.
- 2026-09-28: plan gate chose a builder refusal of `id` and `participantId` under "Another site" over a site field in the link. A new field changes the link format for a case a menu choice covers. Falsified by a researcher on a site other than SONA that needs `id` without the SONA ending.
- 2026-09-28: plan gate chose to refuse an identifier with an unpaired surrogate at intake over a replacement in the completion address. A replacement lets the saved row and the completion address hold different identifiers. Falsified by a participant whose real identifier the refusal blocks.
- 2026-09-28: T1 done (hitop-form). link.html refuses `id` and `participantId` under "Another site" after the trim, with the site's label from `SITES`. New L32 (3 tests) was red with an empty message before the fix. `tests/link.spec.js` 103 passed. The page open of an `id` link is L28, already on main, so no new open test was added.

## Decisions

## Review
