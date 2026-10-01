# M151: Study Link Builder test reach

- **Status:** review
- **Priority:** normal
- **Depends on:** M149
- **Driving RR:** —
- **Principles touched:** —
- **Resolves:** —
- **Surface tier:** internal — hitop-form's Playwright specs and test helpers, which no user runs
- **Branch/PR:** m151-link-builder-test-reach · companion: /Users/jmgirard/github/hitop-form m151-link-builder-test-reach

## Goal

The Study Link Builder's tests state their expectations apart from the page's code and reach the states the reviews found untested.

## Scope

**In:** In hitop-form `tests/`, the next-step test states each sentence as text. The anchor check skips fenced code. The section tests fulfil the instrument exports from local copies. The result region is checked at two widths with a short and a long link. The one-field table gets a CloudResearch Connect case. The unreachable Move guards are driven. Six untested `z` refusals are fired on the builder. The helper that opens every section before a test is replaced by one that opens the section of each field used.

**Out:**
- M149 takes the page changes and the stale-build refusal tests. M150 takes the hints.
- The Module Builder test gaps stay in their own candidate row.

## Acceptance criteria

- [x] AC1: In `tests/link-sections.spec.js`, the next-step test compares `#next` with a table in the spec. The table holds the full expected text for each of the 15 pairs of recruiting site and destination. The spec holds no function that builds that text. Its sentence count counts `.`, `;`, `!` and `?` ends. The README anchor check skips lines inside fenced code blocks: a test gives it a README text with a `# x` line inside a fence and asserts that no `x` anchor results.
- [x] AC2: The `beforeEach` of `tests/link-sections.spec.js` adds a listener to the page's `requestfinished` and `requestfailed` events. The listener records each request whose URL does not start with the test target's base URL and that no route of the test fulfilled or aborted. An `afterEach` fails the test when that record is not empty. The spec passes with the listener in place. No route in the spec calls `route.continue()`, and the export routes fulfill from files under `tests/fixtures/exports/`.
- [x] AC3: The result-region test builds a short link and a `z` link over 5,000 characters, each at 375px and at 1280px wide. In each of the four states, "Copy the link" sits to the right of the box, and the box is under 16rem high. For the long link, the box's `scrollHeight` exceeds its `clientHeight`.
- [x] AC4: The one-field table in `tests/link-sections.spec.js` has a CloudResearch Connect entry. For instrument rows and question groups, a test removes `disabled` from the first Move up and the last Move down, and clicks each. The order does not change, and focus moves to the other move button of that row.
- [x] AC5: A builder test fires six refusals through a `z` link on `link.html`. For each, it asserts the message. It also asserts that the values of `f.elements`, the instrument rows and the question list equal a snapshot from a load with no link. The six refusals are these:
  - a browser with no `DecompressionStream`
  - text that is not base64url
  - a stream that unpacks to more than 100,000 bytes
  - bytes that are not UTF-8
  - text that is not JSON
  - JSON that holds no form
- [x] AC6: A grep for `openBuilderSections` in `tests/` returns no hit. The specs that `git grep -l openBuilderSections a2d74e3 -- tests/` lists open each optional section they use by a click on its summary, through one helper.
- [ ] AC7: The hitop-form Playwright suite passes locally and on its PR's CI.

## Coverage

- AC1 → T1, T7
- AC2 → T2, T7
- AC3 → T3, T7
- AC4 → T4, T7
- AC5 → T5, T7
- AC6 → T6, T7, T8
- AC7 → T7, T8

## Tasks

- [x] T1: Replace `expectedNext()` (`tests/link-sections.spec.js:423-433`) and its tables from `:414` with a literal table, and widen the sentence count. Make the anchor parser (`:616-617`) skip fences, with its plant test.
- [x] T2: Add the request listener to the spec's `beforeEach`, and fulfil the export route from local copies (the `exportJson` shape of `openForm` at `tests/helpers.mjs:215-224`). Commit the copies under `tests/fixtures/exports/` with a fixtures README row. Plant a route that calls `route.continue()` and see the test fail.
- [x] T3: Extend `expectRegion` (`:445-465`) and the long-link test (`:500-523`) to the four states.
- [x] T4: Add the Connect entry to the one-field table (about `:288`). Write the Move guard test for the guards at `link.html:389`, `394`, `602` and `607`.
- [x] T5: Write the six `z` refusal tests, in the style of the `BAD_C` table (`tests/link.spec.js:665-690`).
- [x] T6: Replace `openBuilderSections()` (`tests/helpers.mjs:139-149`) with a helper that opens the section of a named field, and move its nine calling specs to it. A Sonnet subagent can do the move, and its diff is checked here.
- [x] T7: Update the README test-table rows of the changed specs. Run the full suite locally and on the PR.
- [x] T8: In `tests/link-consent.spec.js`, open the Consent section through `openSectionOf` before a test writes `consentText` or `declinedText`. Plant a Consent summary that does not open, and see the spec fail. Run the full suite.

## Work log

- 2026-09-30: created by /milestone-plan from the "Study Link Builder follow-ups" row, with M149 and M150.
- 2026-09-30: the criteria audit ran in reduced mode with a fresh Opus reader. It returned 6 findings, and each was repaired as suggested. AC4 expected focus to stay, but the page moves it to the other move button. AC5 names its field snapshot, and AC6 fixes the list of specs at `a2d74e3`. Three task line numbers were corrected.
- 2026-09-30: implement started. Branches cut in hitop and hitop-form from their pushed main.
- 2026-09-30: amendment (gate): AC2 reworded at the user's choice. The old text was impossible to pass, because a route that answers an export request from a local copy still emits a request to jmgirard.github.io. T2 gained the copies and a plant.
- re-audit: AC2 (reduced) — the first rewording counted a proxy domain, and under FORM_TARGET the target shares the exports' host. It was narrowed to the listener's two events and a base-URL prefix.
- re-audit: AC2 (reduced) — nothing.
- 2026-09-30: T1 done. `NEXT` holds the 15 sentences in full, and a test checks it has each pair once. The count takes `;`. `readmeSlugs()` skips fences, and its plant test went red with the skip removed.
- 2026-09-30: T2 done. The spec's routes mark what they fulfill or abort, and `afterEach` fails on any other off-target request. The two exports are copies from hitop `318629fe`. Two plants went red, each naming the export URL: a `route.continue()` in `holdExports`, and no export route. The spec passed, 93 of 93.
- 2026-09-30: T3 done. The long-link test became four: a short `c` link and a long `z` link at 375px and 1280px. Two plants in `link.html` went red: a `.link-row` set to `display: block` and an `#out` with no `max-height`.
- 2026-09-30: T4 done. `ONE_FIELD` has a Connect entry (`participantId`) that also checks the menu reads `connect`. The forced-press test covers instrument rows and question groups. Removing each of the four `if` guards in `link.html` failed that guard's test.
- 2026-09-30: T5 done. L36 in `link.spec.js` fires the six refusals as `BAD_Z` and compares `builderValues()` with a load with no link. A planted fill of the first instrument row before the refusal failed it.
- 2026-09-30: T6 done. `openSectionOf(page, control)` in `helpers.mjs` opens the section that holds a field name or selector, by a click on its summary. A Sonnet subagent moved the nine specs, 48 calls in eight of them; `network.spec.js` needed none. The diff removed no `expect`. The nine specs passed, 326 of 326. A grep for `openBuilderSections` in `tests/` finds nothing.
- 2026-09-30: T7 done. The README rows of `link.spec.js` and `link-sections.spec.js` name the new cases. The full suite passed locally, 845 of 845 (M150 had 831). The PR's CI run belongs to `/milestone-review`, which opens the PR.
- claim audit: not owed — internal tier
- 2026-09-30: status review.
- 2026-09-30: review return 1 (defect). AC6 failed: `link-consent.spec.js` sets `consentText` and `declinedText` by `evaluate()` in `make()` and K2 and never opens the Consent section. AC1-AC5 passed with evidence. AC7 has local evidence only. F2-F23 wait for triage at the next gate. Status in-progress.
- 2026-09-30: implement resumed. Minor amendment: T8 added for the AC6 return, and the Coverage lines for AC6 and AC7 name it.
- 2026-09-30: T8 done. `setBox()` in `link-consent.spec.js` calls `openSectionOf` before it writes, which covers `make()` and K2. A plant of `onclick="return false"` on the Consent summary failed 13 of 17 tests at the open check in `openSectionOf`. The spec passed 17 of 17, and the full suite passed 845 of 845.
- 2026-09-30: status review.
- 2026-09-30: review pass 2 passed AC1-AC6. Gate triage: G1, G3, F4 and F8 fixed in hitop-form `7604936`, seven findings to a candidate row, 18 rejected.
- step-7 approval: m151-link-builder-test-reach approved for merge

## Decisions

- D1 (2026-09-30): The spec's export copies are committed files under `tests/fixtures/exports/`, copied from hitop's `pkgdown/assets/downloads/`, rather than fetched once per run. The spec then needs no network. The copies can fall behind the site, and the fixtures README names the source commit.

## Review

Pass 1, 2026-09-30. Both branches were current with their `main`. Full hitop-form suite at the branch head: run 1 passed 844 of 845, with N7 in `network.spec.js` timed out. Run 2 passed 845 of 845.

- AC1: pass. `NEXT` in `link-sections.spec.js` holds the 15 sentences as literals, and S7 compares `#next` with each by `toHaveText`. No function in the spec builds the text (`expectedNext` and `SITE_TEXT` are gone). The count regex is `/[.;!?](\s|$)/g`. `readmeSlugs()` toggles on fence lines, and the test "a "#" line inside a fenced code block gives no README anchor" passed in run 2.
- AC2: pass. The `beforeEach` (`link-sections.spec.js:110-119`) adds one listener to `requestfinished` and `requestfailed`. It records a request whose URL does not start with `base()` and that is not in `answered`, the set that `fulfillExport` and `abortRequest` fill. The `afterEach` expects that record to be empty. The spec has four `page.route` calls and no `route.continue()`. Every export route ends in `fulfillExport`, which reads `tests/fixtures/exports/<name>`, or in `abortRequest`. The spec passed in run 2.
- AC3: pass. The loop at `link-sections.spec.js:631-662` runs four tests: a short `c` link and a `z` link over 5,000 characters, at 375px and at 1280px. Each calls `expectRegion`, which asserts that the copy button's left edge is at or past the box's right edge. Each asserts a box height under 16rem. The long cases assert `scrollHeight > clientHeight`. All four passed in run 2.
- AC4: pass. `ONE_FIELD` (`link-sections.spec.js:365`) has the entry "participantParam of CloudResearch Connect". Two forced-press tests (`:516-531`) cover instrument rows and question rows. Each sets `disabled = false` on Move up of row 1 and Move down of row 3, and clicks each. They assert the order is unchanged and focus is on the row's other move button. Both passed in run 2.
- AC5: pass. `BAD_Z` in `link.spec.js:767-779` lists the six refusals that AC5 names. Test L36 loads `link.html` with no link, takes `builderValues()`, then loads each bad `z` link. It asserts the full message and that `builderValues()` equals the first snapshot. `builderValues()` reads the values of `f.elements`, the instrument-row menus and the question names. All six passed in run 2.
- AC6: fail. A grep for `openBuilderSections` in `tests/` returns no hit, and `git grep -l` at `a2d74e3` lists `helpers.mjs` and nine specs. `link-consent.spec.js` uses the Consent section but never opens it. Its `make()` (`:53-54`) and K2 (`:99`) set `consentText` and `declinedText` through `setBox()`. That helper is an `evaluate()`, which writes a hidden field. No call opens `secConsent`. K1, K2, the 20,000-character test and the refused probes therefore run with that section closed. The other eight specs open each section they write: `link-module-file` and `link-questions-file` in `openBuilder`, `link-questions` before each `#addQuestion` use, and `network.spec.js` uses no section.
- AC7: not yet met. The suite passed locally in run 2, 845 of 845. The PR's CI run has not happened. N7 fails at random: 1 of 15 repeats on the branch, and 3 of 30 on an export of `main`, so the flake is older than this milestone.

Consistency gate: `cairn_validate.py` exit 0, with advisory warnings only. In hitop, `devtools::document()` made no change, `pkgdown::check_pkgdown()` found no problems, and `devtools::check()` gave 0 errors, 0 warnings, 0 notes. The hitop branch changes only `cairn/` files, so no NEWS entry and no README rebuild are owed.

Independent review: three fresh reviewers (Opus diff-bug, Sonnet blame-history, Sonnet prior-review). The prior-review probes found no GitHub review comments in either repo. Findings, most severe first, merged across lenses. F1 is the return. The others wait for triage at the next gate.

- F1 (Opus): `link-consent.spec.js` writes the Consent fields without opening the section. This is the AC6 failure above.
- F2 (Opus): at the time of `afterEach`, the listener has not seen a request still in flight. It also does not see requests from other pages in the context. The README row claims more than this.
- F3 (Opus): `fulfillExport` marks a request only after `await readFile`, so a request cancelled in that gap is recorded as a stray. This is a risk of a false red.
- F4 (Opus, blame, prior-review): the weekly run against the deployed page now gives `link-sections.spec.js` the committed copies, not the live exports. The comment at `helpers.mjs:28` ("not from a copy that could drift") and the README sentence on the weekly run were not updated. D1 accepts the drift but does not state this effect.
- F5 (Opus): the sentence count runs after an exact `toHaveText`, so it can only fail on the `NEXT` literal, not on the page.
- F6 (Opus): `link.spec.js:459-461`, `553-556`, `804` and `997` read hints inside closed sections. `main` read the same text, so no coverage was lost.
- F7 (Opus, prior-review): the four region tests take their sentence as `NEXT[0][2]`, by position.
- F8 (Opus, prior-review): `readmeSlugs()` misses indented fences, such as `README.md:672`. It also lets a `~~~` line close a backtick fence. The current README does not trigger either case.
- F9 (Opus): only the Connect entry of `ONE_FIELD` checks the site menu's value. The SONA and other-site entries do not.
- F10 (Opus, blame): two section-opening helpers now exist, `openSectionOf` and the spec's own `openSection`.
- F11 (Opus): the S1-S12 header of `link-sections.spec.js` does not name the request rule or the fence rule.
- F12 (Opus): the exemption is the prefix `startsWith(base())`. For a target at a host root, every export URL matches it. The workflow's target is not a host root.
- F13 (Opus): T7 is ticked, but its "and on the PR" half has not run.
- F14 (Opus): the fixtures README says the copies matched the site byte for byte, with no recorded command. The reviewer found them byte-identical to hitop `318629fe` and `main`.
- F15 (Opus): the `beforeEach` opens a page for the two tests that need none.
- F16 (blame): the `BAD_Z` snapshot does not read section open state or summary text. AC5 does not ask for them.
- F17 (blame): any route-handled request is exempt from the listener, also one a future route aborts on purpose.
- F18 (blame): a PID-5 export has no copy. A future PID-5 test in this spec fails, and it needs a new copy file first.
- F19 (prior-review): S4 still checks overflow in one site and destination state, and S1 still ignores unknown children. These items were in the follow-ups row but not in M151's scope.
- F20 (prior-review): the other specs still fetch the live exports, which caused past network flakes.
- F21 (prior-review): `README.md:640` says "Eleven example files", and two export copies were added. Those copies are not example response files.
- F22 (orchestrator): N7 in `network.spec.js` fails at random, also on `main` (see AC7).
- F23 (blame): `BAD_Z` case 1 runs in Chromium only. DESIGN Known issue 13 already records this.

Pass 2, 2026-09-30, after T8. Both branches were current with their `main`. The full hitop-form suite passed 845 of 845 at `7e10236`. Since pass 1, only `setBox()` in `link-consent.spec.js` changed, so the AC1-AC5 evidence above stands on this run.

- AC6: pass. A grep for `openBuilderSections` in `tests/` returns no hit. `setBox()` now calls `openSectionOf` before its write, which covers `make()`, K1, K2, K3, the nine K5 probes and the 20,000-character test. The T8 plant failed 13 of 17 tests in that spec. All three pass-2 reviewers checked each write in the nine specs against the five sections of `link.html`. Each write follows an `openSectionOf` call for its section, or a `c` or `z` load whose prefill opens it. Only F6's hint reads touch a closed section.
- Consistency gate: `cairn_validate.py` exit 0, with advisory warnings only. In hitop, `devtools::document()` made no change, `pkgdown::check_pkgdown()` found no problems, and `devtools::check()` gave 0 errors, 0 warnings, 0 notes.

Pass-2 findings, most severe first. Each lens found F1 closed and no F2-F23 entry wrong.

- G1 (Opus): at `link-questions.spec.js:297-298`, `openSectionOf` runs straight after a `z` load. The `z` prefill opens Questions after an async unpack (`link.html:875`, `:976`). `openSectionOf` can read `open` as false, the prefill then opens the section, and the click closes it. The test then fails at random. Line 207 waits for 51 groups first, so it is safe.
- G2 (Opus, prior-review): the `openSectionOf` calls after a prefill at `link.spec.js:1090` and `link-questions.spec.js:207` and `:298` hide a prefill that does not open its section. S5 in `link-sections.spec.js` covers that case.
- G3 (blame, prior-review): `setBox()` writes `.value` and fires no `input` event, so the Consent summary still reads "Not used". The new comment says the section opens "as a researcher opens it", which claims more than the test does.
- G4 (blame): the K5 probes now open Consent before the refusal, so they cannot catch a `refuseAt` that does not open it. `main` opened every section, so this is not new, and `link-sections.spec.js` covers it.
- G5 (blame): K4 reads prefilled values with Consent opened by the prefill, not by a click. `toHaveValue` reads a closed field.
- G6 (blame): the T8 work-log line names `make()` and K2, and the plant also reached the K5 probes and the 20,000-character test.
- G7 (prior-review): the header of `link-consent.spec.js` and its README row do not say that the spec opens its sections.

Triage at the gate, 2026-09-30. The user chose the recommended triage.

- Fixed now, in hitop-form `7604936`. G1: the 100,000-byte test waits for group 9's prefilled text before `openSectionOf`. G3: the `setBox()` comment says the write fires no input event. F4: the `fetchExport` comment and the README paragraph on the weekly run name `link-sections.spec.js` as the spec that uses the copies. F8: `readmeSlugs()` takes indented fences and closes a fence only on the same character at the same or greater length. A new test covers the three cases, and it failed with the old rule put back. The full suite passed 846 of 846.
- Follow-up, as the ROADMAP candidate row "Study Link Builder test follow-ups (M151 review)": F2, F3, F9, F19, F20, F22 and G2.
- Rejected. F5: the count's job under AC1 is the sentence rule, and the exact text check covers the page. F6: `main` read the same hints, so no coverage was lost. F7: a reordered `NEXT` fails, it does not pass. F10: AC6 names one helper for the nine specs only. F11: the comment at `:84-90` covers the listener. F12: the workflow's target is not a host root. F13: AC7 stays unticked until the PR's CI is green. F14: the reviewer found the copies byte-identical. F15: the cost is two page launches. F16: AC5 does not ask for section state, and M149's S11 tests the reset. F17: no route aborts a stray today. F18: the failure names the missing export. F21: the README sentence counts example response files, and the export copies are not ones. F23: DESIGN Known issue 13 records it. G4: `main` opened every section, and `link-sections.spec.js` tests `refuseAt`. G5: AC6 is about the sections a test writes, and a prefill opens K4's. G6: the work log is append-only, and this Review section states the full reach. G7: documentation only, and the README row describes what the tests check.
