# M133: A study link can take the participant's identifier from a named address parameter and put it into the completion address, so hitop-form fits SONA and CloudResearch Connect

- **Status:** in-progress
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** GP3, IP1
- **Resolves:** —
- **Surface tier:** user-facing — the deployed form page, its link builder and the package's article
- **Branch/PR:** `m133-form-recruiter-param`; companion: /Users/jmgirard/github/hitop-form m133-form-recruiter-param

## Goal

A study link's `participantParam` field names the address parameter holding the participant's identifier, and a `{participant}` token in `complete` or `completeSaved` is replaced by that identifier, so a study recruited through SONA or CloudResearch Connect runs on hitop-form as a Prolific study does.

## Scope

**In:** hitop-form: the `participantParam` link field in `parseLink()` and its refusals; the page reading the named parameter; the `{participant}` token in both completion addresses; link.html's "Recruiting site" choice (none, Prolific, SONA, CloudResearch Connect, another site), which writes the fields and prints the suffix a site needs, with M125's prefill round trip; README section; Playwright tests. hitop: source notes `sona2026help.md` and `cloudresearch2026help.md` with their INDEX lines; the online-collection article's section on the two sites; NEWS. D-077 (annotates D-071(b), recorded at this plan) governs.

**Out:** saving a site's further parameters as columns (Connect's `assignmentId` and `projectId`, MTurk's `assignmentId` and `hitId`) and SONA's server-side credit grant, which needs a relay the static page cannot hold → the narrowed recruiter candidate row. The reader, `read_form_responses()`: unchanged, since no column is added. Instrument content (IP1): untouched.

## Acceptance criteria

- [ ] AC1: `parseLink()` accepts `participantParam` as a string of 1 to 64 characters from `A-Z a-z 0-9 _ . -`, other than `c`, `PROLIFIC_PID`, `STUDY_ID` and `SESSION_ID`. It refuses by name, showing the value: a non-string, an empty string, a string with any other character, a string over 64 characters, `c` (the page's own parameter), one of the three Prolific names (pointing to `prolific: true`), a link that also names a non-blank `participant`, and a link that also carries `prolific: true`. link.html refuses before it builds a link the forms its controls can produce: empty, another character, over 64, `c`, a Prolific name, and beside a participant. Its prefill skips a non-string `participantParam` as it skips other mistyped fields. Tests assert each refusal's message on each side where it can occur.
- [ ] AC2: Under `participantParam`, the page takes the participant identifier from the first value of that parameter in its address that is not blank, not of the form `%…%` and not of the form `{{…}}`. With no such value, the start screen asks for the identifier as it does without the field. A link with neither `participantParam` nor `prolific: true` reads no address parameter for the identifier. Tests walk: a filled value, a lone `%SURVEY_CODE%`, a doubled parameter whose first value is `%SURVEY_CODE%`, a blank value, a `{{…}}` value, an absent parameter, and a parameter name holding `.` and `-`.
- [ ] AC3: With shuffle off and with shuffle on, the row, the saved file and the builder's Supabase SQL under `participantParam` have the same columns in the same order as under the same link without it. Tests compare each pair.
- [ ] AC4: In `complete` and `completeSaved`, each `{participant}` in the query or fragment is replaced by `encodeURIComponent()` of the participant identifier wherever the page uses the address: the navigation after a confirmed send, the sent screen's link, and the saved-file screens' link. `checkCompleteUrl()` accepts the token in the query or fragment and refuses by name a token in the host or path. Tests assert the exact address for a SONA-shaped URL (`…/webstudy_credit.aspx?experiment_id=123&credit_token=abc&survey_code={participant}`) in each use, the saved-file link under `complete` alone and under `completeSaved`. The identifier `a&b c` comes from the address and from the start screen, and `12345` from the link's `participant` field and from `PROLIFIC_PID`. An address without the token is left unchanged.
- [ ] AC5: link.html's "Recruiting site" choice writes: none, no field; Prolific, `prolific: true` and its placeholder suffix as on main; SONA, `participantParam: "id"` and the suffix `&id=%SURVEY_CODE%`; CloudResearch Connect, `participantParam: "participantId"` and no suffix; another site, the name typed into its field and no suffix. The Connect clause is provisional: if T1's pages state that the study URL needs a placeholder, it returns to the gate. A link loaded through `?c=` selects Prolific for `prolific: true`, SONA for `id`, Connect for `participantId`, another site with the name filled for any other value, and none otherwise. The SONA link opened unfilled through "Open the link" shows the start screen's identifier question. Tests: one per choice, one prefill round trip per choice, and the unfilled SONA open.
- [ ] AC6: The README section and the article section each state, citing the vendor page that `cairn/references/sona2026help.md` or `cloudresearch2026help.md` records: SONA fills `%SURVEY_CODE%` into the study URL, as `id` in its Qualtrics guide; SONA's client-side completion address carries the code as `survey_code`; Connect passes the participant's ID as `participantId`; Connect gives each study one completion redirect address. Both say that SONA's client-side completion address carries its credit token inside the study link, where a participant can read it, and that SONA's server-side address needs a server. NEWS names the field and the token.
- [ ] AC7: The Prolific link fields, suffix, columns, SQL and prefill behave as on main, and builder tests that drove the Prolific box change only the control they operate. The hitop-form suite passes in its CI; hitop's `devtools::check()` gives 0 errors, 0 warnings and 0 notes; `cairn_validate` exits 0.

## Coverage

- AC1 → T2, T5
- AC2 → T3
- AC3 → T3, T5
- AC4 → T4
- AC5 → T1, T5
- AC6 → T1, T6
- AC7 → T5, T7

## Tasks

- [x] T1: Write `cairn/references/sona2026help.md` (the External Study Credit Granting page and the Qualtrics and SoSci Survey help pages) and `cloudresearch2026help.md` (the Connect articles "How to Integrate your Survey with Connect" and "Project Link"), quoting what each states about the parameter and the completion address, with INDEX lines. Settle from them whether Connect appends `participantId` itself; if no page says so, AC5's Connect suffix returns to the gate.
- [x] T2: `participantParam` in `parseLink()` (form.js), with the page-side refusals AC1 lists and their tests. (The link.html submit check and prefill skip moved to T5, where the builder's field is made.)
- [x] T3: Read the named parameter in `runForm()`'s identifier choice beside `readProlific()`, the placeholder forms read as absent, the start-screen fallback; the row and file comparisons with shuffle off and on.
- [ ] T4: The `{participant}` substitution in `finish()`, the sent screen and `showSaved()`; `checkCompleteUrl()` accepting the token in the query or fragment and refusing it in the host or path, whose `new URL()` form is `%7Bparticipant%7D`; the address tests (read the target at the held request, per the M119 lesson).
- [ ] T5: The "Recruiting site" choice in link.html in place of the Prolific box, the suffixes, the other-site name field, `prefill()` for the new field and its skip of a non-string value; the builder-side refusals AC1 lists; the SQL comparison; builder, prefill and unfilled-SONA tests; the Prolific builder tests moved to the new control (tests/link.spec.js drives `input[name="prolific"]` today).
- [ ] T6: README "Recruit through SONA or CloudResearch Connect"; the article section after "The Prolific route"; NEWS.
- [ ] T7: hitop-form CI green on its PR; hitop `check()`; `cairn_validate`.

## Work log

- 2026-09-27: created by /milestone-plan. Promoted from the candidate row "hitop-form under another recruiter's own parameters (SONA, CloudResearch)" (lineage M118); the row is narrowed to its remainder in the same commit.
- 2026-09-27: criteria audit ran in full mode (user-facing tier) on a fresh [O] reader: 14 findings, all with one clear repair and fixed before the gate (D-077 to annotate D-071(b); AC2's clause excluding `prolific: true`; AC1 builder-side forms narrowed to what the controls produce, ASCII set, `c` and the Prolific names refused; the token held to query or fragment since `new URL()` encodes braces in a path; AC4's uses and identifier sources enumerated; AC5 prefill mapping, the `&` suffix, the unfilled SONA open, the Connect clause provisional on T1; AC7 promise moved from tests to Prolific behavior; AC3 with shuffle both ways and mapped to T5; AC6's four cited statements named; AC2 probes widened). None posed as a question.
- 2026-09-27: plan gate chose a named address parameter plus a completion token over one switch per site like `prolific: true` because SONA's parameter name is the researcher's choice and a switch per site adds columns and reader changes; falsified by a site needing more than the identifier to return its participant.
- 2026-09-27: plan gate chose one "Recruiting site" menu over keeping the Prolific checkbox beside a free-text parameter field because the menu writes SONA's name and `%SURVEY_CODE%` ending for the researcher; falsified by researchers of a site outside the menu finding "another site" harder than a plain field.
- 2026-09-27: plan gate chose the identifier alone over saving a site's further IDs as columns because neither SONA nor Connect requires them and columns change the reader's contract; falsified by a study that must keep Connect's `assignmentId` or `projectId`.
- 2026-09-27: plan gate chose tests against the documented URL shapes over a live SONA run because a live run needs a SONA study set up and blocks the milestone on it; falsified by a SONA credit grant failing on an address the tests accept.
- 2026-09-27: implement started. Branch `m133-form-recruiter-param` cut in hitop and hitop-form from their pushed default branches; the step-3 gate was skipped, nothing being open.
- 2026-09-27: T1 notes written: `sona2026help.md` (five SONA pages) and `cloudresearch2026help.md` (two Connect pages, read in the browser pane after a plain fetch got HTTP 403), with INDEX lines. SONA removes `%SURVEY_CODE%` when the researcher views the URL, so the page sees a blank value there. No Connect page says in those words that Connect adds `participantId` to the project URL, so AC5's Connect clause goes to a mini gate, as T1 states.
- 2026-09-27: mini gate: Jeff kept AC5's Connect clause (no suffix), on the pages' instruction to read `participantId` from the address and their after-launch capture; the start screen asks when it is missing. AC5 unchanged. T1 done.
- 2026-09-27: T2 done (hitop-form). `checkParticipantParam()` in form.js, called by `parseLink()` with the two conflict refusals; G14 in tests/guard.spec.js, 21 tests. A plant returning the name at once turned all 13 refusal probes red. Minor amendment: T2's link.html half moved to T5, which makes the builder field; Coverage AC1 → T2, T5.
- 2026-09-27: T3 done (hitop-form). `readParticipantParam()` in form.js (`%…%` and `{{…}}` read as absent), passed by `boot()` to `runForm()` beside the Prolific read; tests/recruit.spec.js P1 to P7, 13 tests, the header and row-key comparisons with shuffle off and on. Two plants red: ignoring the address value (P1, P3, P4, P7) and counting `%…%` as filled (P2, P3, P6).

## Decisions

## Review
