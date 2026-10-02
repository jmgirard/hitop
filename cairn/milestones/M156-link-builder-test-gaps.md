# M156: Study Link Builder test gaps

- **Status:** review
- **Priority:** high
- **Depends on:** M155
- **Driving RR:** —
- **Principles touched:** —
- **Resolves:** —
- **Surface tier:** internal — hitop-form's Playwright specs, helpers, fixtures and test workflow, which no user runs
- **Branch/PR:** m156-link-builder-test-gaps, companion: /Users/jmgirard/github/hitop-form m156-link-builder-test-gaps

## Goal

hitop-form's tests catch the stray requests, closed sections and wrong site choices they now miss, and give the same result on every run.

## Scope

**In:** These changes are in hitop-form `tests/`:
- the stray-request check in `link-sections.spec.js`
- the site menu in `ONE_FIELD`, S1's unknown children and S4's states
- `openSectionOf` after a prefill
- export requests answered from committed copies
- the two-channel read that makes N7 fail about 1 run in 10

**Out:**
- The page changes of the same candidate row go to M155.
- Module Builder test gaps stay in their own candidate row.

## Acceptance criteria

- [x] AC1: In `tests/link-sections.spec.js`, the stray-request check records each request of the test's browser context when it starts. It judges the record in `afterEach`. Each export route marks its request before its first `await`. A stray request is a request to another address that no route answers. The check fails a test whose stray request starts on a second page of the context. It fails a test whose stray request is still open at the test's end. It does not fail a test whose export request is cancelled while its copy is read.
- [x] AC2: In `tests/link-sections.spec.js`, every `ONE_FIELD` entry states the site menu's value, and the test asserts it. S1 fails when `#f` gains a child of a kind it does not name. S4 checks overflow at both widths for each recruiting site with each destination.
- [x] AC3: `openSectionOf()` in `tests/helpers.mjs` takes an option that fails the test when the section is closed. The search `git grep -n "openSectionOf" tests/` lists the calls. Each listed call that follows a `c`, `z` or setup-file load in the same test uses that option or follows an assertion of the section's state. A setup-file prefill test asserts which sections are open.
- [x] AC4: With `FORM_TARGET` empty, no page in the full suite requests `EXPORT_BASE` unanswered. A context-level check in the shared test fixture fails a test on such a request, and the full suite passes with it. The search `git grep "from '@playwright/test'" tests/*.spec.js` finds no spec that bypasses the fixture. The search `git grep -n "fetch(" tests/` finds no Node-side fetch of an export outside the `FORM_TARGET` branch of `fetchExport()`. `tests/fixtures/exports/` holds copies of the five exports. With `FORM_TARGET` set, the exports are fetched live, and a test fails when a copy differs from its live file.
- [x] AC5: The search `git grep -n "states.at(-1)" tests/` lists the tests that read a page state beside a request. Each listed test reads the page state and the request from one ordered source, or waits for the page state it asserts. N7 passes 100 of 100 runs under `--repeat-each=100`.
- [ ] AC6: The README test-table rows of the changed specs say what each now checks. The full hitop-form suite passes locally and on its PR's CI.

## Coverage

- AC1 → T1, T7
- AC2 → T2, T7
- AC3 → T3, T7
- AC4 → T4, T5, T7
- AC5 → T6, T7
- AC6 → T7

## Tasks

- [x] T1: Move the stray listener to the `context` fixture's `request` event, as `network.spec.js:64` does, and judge in `afterEach`. In `fulfillExport`, mark before `readFile` and unmark in its `catch`. Mark before the holds at about `:915` and `:1170`. Plant a second-page request, a held request, and a cancel during the read. See each give the result AC1 states.
- [x] T2: Add `site` to the `ONE_FIELD` entries (about `:374-379`). Make S1's map name an unknown child (about `:180-187`). Loop S4 (about `:348-369`) over the site and destination tables. Plant a wrong site and an extra child, and see each go red.
- [x] T3: Add the option to `openSectionOf()` (`tests/helpers.mjs:207-213`). Use it at each call that the AC3 search finds after a prefill. At planning time these are `link.spec.js:1129` and `link-questions.spec.js:211` and `:304`. Add the section-state check to the setup-file prefill test (`link-setupfile.spec.js:279-296`).
- [x] T4: Commit copies of the PID-5, PID-5-SF and PID-5-BF exports beside the two in `tests/fixtures/exports/`. Make `fetchExport()` read the copy unless `FORM_TARGET` is set. Add a shared fixture for when `FORM_TARGET` is empty. It routes `EXPORT_BASE` on every context to the copies, and fails on an unanswered request. A Sonnet subagent moves the specs' imports to it, and its diff is read here.
- [x] T5: Add the copy-against-live test that runs only with `FORM_TARGET` set. Update the `fetchExport` comment and the README paragraph on the weekly run.
- [x] T6: Find the race in N7 with a repeat run that logs `states` on failure. Fix N7 and the other tests that the AC5 search lists. Run N7 under `--repeat-each=100`, and put that command in the README beside N7's row.
- [x] T7: Update the README test-table rows. Run the full suite locally and on the PR.

## Work log

- 2026-10-01: created by /milestone-plan.
- 2026-10-01: criteria audit, reduced mode, fresh Opus reader. AC4 claimed Node-side requests that the fixture check cannot see, and was narrowed to page requests plus a search. A second audit added the search for specs that bypass the shared fixture. AC1 to AC3, AC5 and AC6 returned no finding.
- 2026-10-01: plan gate chose committed copies of all five exports on PR and push runs, with live fetches and a copy-against-live test on the weekly run, over live fetches everywhere. Live fetches make a run fail on a network fault, and copies alone never see a new export. Falsified by a drifted copy that the weekly run misses, or a red PR run caused by a stale copy.
- 2026-10-01: plan chose a stray check on the context's `request` event, judged in `afterEach`, over waiting for in-flight requests to settle, as `network.spec.js:64` already does. Falsified by a stray request that starts after `afterEach` reads the record.
- 2026-10-02: implement started. Branches cut in hitop and hitop-form. Baseline hitop-form suite: 1028 passed. Question gate chose one shared mark helper for every route that answers an export, one ordered page channel for N7's state and its navigation, and `helpers.mjs` as the shared fixture's home.
- 2026-10-02: T1 done. `markAnswered()`, `isAnswered()` and `fulfillExport()` moved to `helpers.mjs`, and the stray record is the context's `request` event, judged in `afterEach`. Plants: a second-page request and a held request each failed by name, an export cancelled during its hold passed with the mark first and failed with the mark after the hold. `network.spec.js:64` is a page listener, not a context one. Spec: 122 passed.
- 2026-10-02: T2 done. S1 now names `#setupBlock` and `#err`, which it skipped before. This settles the hosted-setup row's "S1 does not pin the new fieldset". Plants went red: a wrong Connect site, an extra `aside`, and a 2,000px Connect hint on its three pairs at each width. Spec: 122 passed.
- 2026-10-02: T3 done. `openSectionOf()` takes `wasOpen`, the state a prefill leaves. A run-time log found four call sites after a prefill. They are `link-questions.spec.js:213` and `:309`, `link-setupfile.spec.js:424` (closed), and `link.spec.js:1148`. Each now passes `wasOpen`. LF5 asserts all five sections. With the page's prefill opening no section, the seven open-expecting calls and LF5 failed. Three specs: 220 passed.
- 2026-10-02: T4 delegation. A Sonnet subagent moved the 27 specs' `test`/`expect` imports to `helpers.mjs`. Its diff, read here, dropped a space after `useTarget,` in all 27, and it fixed that on return.
- 2026-10-02: T4 done. The PID-5 copies come from hitop `d4f43238`, and all five equaled the site on 2026-10-02. `exportCopies` routes the context, and `routeExport()` marks a spec's own export routes. W1's `route.continue()` became `route.fallback()`.
- 2026-10-02: T4 finding. With any context route set, the store saw no CORS preflight, and four Supabase send tests failed. Send T2's "no OPTIONS" check was then blind. `begin()` now waits for Begin and takes the copies route off, and `openForm()` puts it back. Plants: an export sent on with `route.continue()` and one with no copy each failed by name.
- 2026-10-02: T5 done. `tests/exports.spec.js` E1 runs only with `FORM_TARGET` set, through `fetchExport(…, { text: true })`. A one-byte drift in a copy failed it. The README, fixtures README, workflow and config comments now describe copies on PR runs. Full suite: 1028 passed, 5 skipped (E1).
- 2026-10-02: T6 finding. N7 failed 2 of 100 before the fix, both in the Finish click. The click waited on the held navigation for 15 seconds. The state read was not the failure seen.
- 2026-10-02: T6 done. `observeUntilLeave()` reports each change and the `navigate` event through one exposed function. The six tests the AC5 search lists use it. The tests that hold a navigation press Finish with `noWaitAfter`. A plant that navigated before drawing the sent screen failed N7 5 of 5. N7: 100 of 100. The six: 20 runs each, 480 passed. Full suite: 1028 passed, 5 skipped.
- 2026-10-02: T7 done locally. README rows for `link-sections`, `link-setupfile`, `network` and `exports` updated. With `FORM_TARGET` set to the deployed page: 1028 passed and 5 failed on `net::ERR_TIMED_OUT` loading the page, and those 5 passed on rerun. The PR's CI run falls to `/milestone-review`.
- 2026-10-02: claim audit: not owed — internal tier.
- 2026-10-02: implement done, status `review`. The hitop branch changes only `cairn/`, so no R check is owed.
- 2026-10-02: review started. Both branches contain their `origin/main`. No PR exists for either branch.
- 2026-10-02: review return 1 (defect, step 3): AC6 fails as written. T3 added two checks that the README rows do not state. `link.spec.js` checks that a `c` selecting a site leaves the participants section open. `link-questions.spec.js` checks that a `z` leaves the questions section open. Fix: add those checks to the two rows. AC1 to AC5 evidence is recorded in Review. Status `in-progress`.
- 2026-10-02: implement resumed for return 1. Both branches still contain their `origin/main`. The `link.spec.js` row now says a `c` that selects a site leaves the menu's section open and one that selects none leaves it closed. The `link-questions.spec.js` row now says a `z` link that holds questions leaves the questions section open. Both read from L27 and the two `wasOpen: true` calls. Full suite: 1028 passed, 5 skipped.
- 2026-10-02: claim audit: not owed — internal tier.
- 2026-10-02: implement done again, status `review`.
- 2026-10-02: review pass 2 started. No PR exists for either branch. AC1 to AC5 and the gate passed again, and AC6's local half passed. Three reviewers spawned over the hitop-form diff.

## Review

- AC1: plants appended to `link-sections.spec.js`, then removed. A fetch to another address from a second page of the context failed by name. A routed fetch to another address held open at the end failed by name. An export request marked first and cancelled during its hold passed. The same request marked after its hold failed. Every export route in the spec (`:133`, `:931`, `:1314`, `:1598`) marks before its first await. Each uses `fulfillExport()`, `abortRequest()` or an explicit `markAnswered()`.
- AC2: all 12 `ONE_FIELD` entries carry `site`, asserted at `:418`. Plants went red: `prolific` given site `sona` (expected "sona", received "prolific"), an `<aside>` added to `#f` (S1 lists "unknown: aside"), and a 2,000px `#connectHint` (S4 fails its three Connect pairs at both widths). `SITES` and `KINDS` match the menus' options in `link.html`.
- AC3: the AC3 search lists 52 calls. A fresh Sonnet audit found 4 after a `c`, `z` or setup-file load, and all 4 pass `wasOpen`. They are `link.spec.js:1148`, `link-questions.spec.js:212` and `:308`, and `link-setupfile.spec.js:430`. LF5 asserts all five sections at `link-setupfile.spec.js:309-311`. A plant that stops the prefill opening sections failed 8 tests: the four site cases, the three questions-editor cases and LF5.
- AC4: the import search finds no spec. The `fetch(` search finds `helpers.mjs:40`, inside the `FORM_TARGET` branch of `fetchExport()`. It also finds `:436`, a `route.fetch` of the page in `gotoLong()`, not an export. All five copies exist. Full suite with `FORM_TARGET` empty: 1028 passed, 5 skipped (E1). E1 with `FORM_TARGET` set: 5 passed against the site. A one-byte drift in `pid5sf.json` failed E1 for that copy. With `pid5bf.json` moved away, 5 tests failed naming its request.
- AC5: the AC5 search lists 6 lines in `consent`, `network`, `recruit` (3) and `send`. Each test reads its states through `observeUntilLeave()`, directly or through a spec helper. N7 under `--repeat-each=100`: 100 passed.
- AC6: FAILS. Local suite green (AC4 line). The branch changes rows for `link-sections`, `link-setupfile`, `network` and `exports`. The `link.spec.js` row says "the site a `c` selects and its round trip" and not that its section is open. The `link-questions.spec.js` row does not mention the open questions section after a `z`. Those are the checks T3 added. PR CI not reached.
- Gate: `cairn_validate` passed (24 advisory warnings, none new). `document()` left no diff. `check_pkgdown()` found no problems. README.Rmd and NEWS are untouched, as the hitop diff is `cairn/` only. `devtools::check()` and the independent review were not run because review stopped at AC6.

Pass 2 (2026-10-02, after return 1). Since pass 1 the hitop-form branch changed only `README.md` (`f2a98b8`), and `origin/main` did not move in either repo.
- AC1, AC2: no test file changed since the pass-1 plants. Full suite at `f2a98b8` with `FORM_TARGET` empty: 1028 passed, 5 skipped (E1). The 12 `ONE_FIELD` entries still carry `site` (`link-sections.spec.js:390-407`).
- AC3: the four `wasOpen` calls are the same four, at the same lines. LF5 is unchanged.
- AC4: the import search finds no spec. The `fetch(` search adds `setupfile.spec.js:67` to pass 1's list, a comment and not a fetch. The five copies exist.
- AC5: the AC5 search lists the same 6 test lines and one comment line in `helpers.mjs:721`. N7 under `--repeat-each=100`: 100 passed.
- AC6: the `link.spec.js` row now says a `c` that selects a site leaves the menu's section open. A `c` that selects none leaves it closed. L27's `wasOpen: c.site !== ''` asserts both. The `link-questions.spec.js` row now says a `z` link that holds questions leaves the questions section open, which `:212` and `:308` assert. The rows for `link-sections`, `link-setupfile`, `network` and `exports` are as pass 1 read them. Local suite green (AC1 line). PR CI is read at the merge step.
- Gate: `cairn_validate` passed (24 advisory warnings, none new). `document()` left no diff. `check_pkgdown()` found no problems. `devtools::check()`: 0 errors, 0 warnings, 0 notes. The hitop diff is `cairn/` only, so README.Rmd, NEWS and `.Rbuildignore` owe nothing.
