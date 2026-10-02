# M156: Study Link Builder test gaps

- **Status:** planned
- **Priority:** high
- **Depends on:** M155
- **Driving RR:** —
- **Principles touched:** —
- **Resolves:** —
- **Surface tier:** internal — hitop-form's Playwright specs, helpers, fixtures and test workflow, which no user runs
- **Branch/PR:** —

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

- [ ] AC1: In `tests/link-sections.spec.js`, the stray-request check records each request of the test's browser context when it starts. It judges the record in `afterEach`. Each export route marks its request before its first `await`. A stray request is a request to another address that no route answers. The check fails a test whose stray request starts on a second page of the context. It fails a test whose stray request is still open at the test's end. It does not fail a test whose export request is cancelled while its copy is read.
- [ ] AC2: In `tests/link-sections.spec.js`, every `ONE_FIELD` entry states the site menu's value, and the test asserts it. S1 fails when `#f` gains a child of a kind it does not name. S4 checks overflow at both widths for each recruiting site with each destination.
- [ ] AC3: `openSectionOf()` in `tests/helpers.mjs` takes an option that fails the test when the section is closed. The search `git grep -n "openSectionOf" tests/` lists the calls. Each listed call that follows a `c`, `z` or setup-file load in the same test uses that option or follows an assertion of the section's state. A setup-file prefill test asserts which sections are open.
- [ ] AC4: With `FORM_TARGET` empty, no page in the full suite requests `EXPORT_BASE` unanswered. A context-level check in the shared test fixture fails a test on such a request, and the full suite passes with it. The search `git grep "from '@playwright/test'" tests/*.spec.js` finds no spec that bypasses the fixture. The search `git grep -n "fetch(" tests/` finds no Node-side fetch of an export outside the `FORM_TARGET` branch of `fetchExport()`. `tests/fixtures/exports/` holds copies of the five exports. With `FORM_TARGET` set, the exports are fetched live, and a test fails when a copy differs from its live file.
- [ ] AC5: The search `git grep -n "states.at(-1)" tests/` lists the tests that read a page state beside a request. Each listed test reads the page state and the request from one ordered source, or waits for the page state it asserts. N7 passes 100 of 100 runs under `--repeat-each=100`.
- [ ] AC6: The README test-table rows of the changed specs say what each now checks. The full hitop-form suite passes locally and on its PR's CI.

## Coverage

- AC1 → T1, T7
- AC2 → T2, T7
- AC3 → T3, T7
- AC4 → T4, T5, T7
- AC5 → T6, T7
- AC6 → T7

## Tasks

- [ ] T1: Move the stray listener to the `context` fixture's `request` event, as `network.spec.js:64` does, and judge in `afterEach`. In `fulfillExport`, mark before `readFile` and unmark in its `catch`. Mark before the holds at about `:915` and `:1170`. Plant a second-page request, a held request, and a cancel during the read. See each give the result AC1 states.
- [ ] T2: Add `site` to the `ONE_FIELD` entries (about `:374-379`). Make S1's map name an unknown child (about `:180-187`). Loop S4 (about `:348-369`) over the site and destination tables. Plant a wrong site and an extra child, and see each go red.
- [ ] T3: Add the option to `openSectionOf()` (`tests/helpers.mjs:207-213`). Use it at each call that the AC3 search finds after a prefill. At planning time these are `link.spec.js:1129` and `link-questions.spec.js:211` and `:304`. Add the section-state check to the setup-file prefill test (`link-setupfile.spec.js:279-296`).
- [ ] T4: Commit copies of the PID-5, PID-5-SF and PID-5-BF exports beside the two in `tests/fixtures/exports/`. Make `fetchExport()` read the copy unless `FORM_TARGET` is set. Add a shared fixture for when `FORM_TARGET` is empty. It routes `EXPORT_BASE` on every context to the copies, and fails on an unanswered request. A Sonnet subagent moves the specs' imports to it, and its diff is read here.
- [ ] T5: Add the copy-against-live test that runs only with `FORM_TARGET` set. Update the `fetchExport` comment and the README paragraph on the weekly run.
- [ ] T6: Find the race in N7 with a repeat run that logs `states` on failure. Fix N7 and the other tests that the AC5 search lists. Run N7 under `--repeat-each=100`, and put that command in the README beside N7's row.
- [ ] T7: Update the README test-table rows. Run the full suite locally and on the PR.

## Work log

- 2026-10-01: created by /milestone-plan.
- 2026-10-01: criteria audit, reduced mode, fresh Opus reader. AC4 claimed Node-side requests that the fixture check cannot see, and was narrowed to page requests plus a search. A second audit added the search for specs that bypass the shared fixture. AC1 to AC3, AC5 and AC6 returned no finding.
- 2026-10-01: plan gate chose committed copies of all five exports on PR and push runs, with live fetches and a copy-against-live test on the weekly run, over live fetches everywhere. Live fetches make a run fail on a network fault, and copies alone never see a new export. Falsified by a drifted copy that the weekly run misses, or a red PR run caused by a stale copy.
- 2026-10-01: plan chose a stray check on the context's `request` event, judged in `afterEach`, over waiting for in-flight requests to settle, as `network.spec.js:64` already does. Falsified by a stray request that starts after `afterEach` reads the record.
