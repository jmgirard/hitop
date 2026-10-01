# M151: Study Link Builder test reach

- **Status:** in-progress
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

- [ ] AC1: In `tests/link-sections.spec.js`, the next-step test compares `#next` with a table in the spec. The table holds the full expected text for each of the 15 pairs of recruiting site and destination. The spec holds no function that builds that text. Its sentence count counts `.`, `;`, `!` and `?` ends. The README anchor check skips lines inside fenced code blocks: a test gives it a README text with a `# x` line inside a fence and asserts that no `x` anchor results.
- [ ] AC2: The `beforeEach` of `tests/link-sections.spec.js` adds a listener to the page's `requestfinished` and `requestfailed` events. The listener records each request whose URL does not start with the test target's base URL and that no route of the test fulfilled or aborted. An `afterEach` fails the test when that record is not empty. The spec passes with the listener in place. No route in the spec calls `route.continue()`, and the export routes fulfill from files under `tests/fixtures/exports/`.
- [ ] AC3: The result-region test builds a short link and a `z` link over 5,000 characters, each at 375px and at 1280px wide. In each of the four states, "Copy the link" sits to the right of the box, and the box is under 16rem high. For the long link, the box's `scrollHeight` exceeds its `clientHeight`.
- [ ] AC4: The one-field table in `tests/link-sections.spec.js` has a CloudResearch Connect entry. For instrument rows and question groups, a test removes `disabled` from the first Move up and the last Move down, and clicks each. The order does not change, and focus moves to the other move button of that row.
- [ ] AC5: A builder test fires six refusals through a `z` link on `link.html`. For each, it asserts the message. It also asserts that the values of `f.elements`, the instrument rows and the question list equal a snapshot from a load with no link. The six refusals are these:
  - a browser with no `DecompressionStream`
  - text that is not base64url
  - a stream that unpacks to more than 100,000 bytes
  - bytes that are not UTF-8
  - text that is not JSON
  - JSON that holds no form
- [ ] AC6: A grep for `openBuilderSections` in `tests/` returns no hit. The specs that `git grep -l openBuilderSections a2d74e3 -- tests/` lists open each optional section they use by a click on its summary, through one helper.
- [ ] AC7: The hitop-form Playwright suite passes locally and on its PR's CI.

## Coverage

- AC1 → T1, T7
- AC2 → T2, T7
- AC3 → T3, T7
- AC4 → T4, T7
- AC5 → T5, T7
- AC6 → T6, T7
- AC7 → T7

## Tasks

- [x] T1: Replace `expectedNext()` (`tests/link-sections.spec.js:423-433`) and its tables from `:414` with a literal table, and widen the sentence count. Make the anchor parser (`:616-617`) skip fences, with its plant test.
- [ ] T2: Add the request listener to the spec's `beforeEach`, and fulfil the export route from local copies (the `exportJson` shape of `openForm` at `tests/helpers.mjs:215-224`). Commit the copies under `tests/fixtures/exports/` with a fixtures README row. Plant a route that calls `route.continue()` and see the test fail.
- [ ] T3: Extend `expectRegion` (`:445-465`) and the long-link test (`:500-523`) to the four states.
- [ ] T4: Add the Connect entry to the one-field table (about `:288`). Write the Move guard test for the guards at `link.html:389`, `394`, `602` and `607`.
- [ ] T5: Write the six `z` refusal tests, in the style of the `BAD_C` table (`tests/link.spec.js:665-690`).
- [ ] T6: Replace `openBuilderSections()` (`tests/helpers.mjs:139-149`) with a helper that opens the section of a named field, and move its nine calling specs to it. A Sonnet subagent can do the move, and its diff is checked here.
- [ ] T7: Update the README test-table rows of the changed specs. Run the full suite locally and on the PR.

## Work log

- 2026-09-30: created by /milestone-plan from the "Study Link Builder follow-ups" row, with M149 and M150.
- 2026-09-30: the criteria audit ran in reduced mode with a fresh Opus reader. It returned 6 findings, and each was repaired as suggested. AC4 expected focus to stay, but the page moves it to the other move button. AC5 names its field snapshot, and AC6 fixes the list of specs at `a2d74e3`. Three task line numbers were corrected.
- 2026-09-30: implement started. Branches cut in hitop and hitop-form from their pushed main.
- 2026-09-30: amendment (gate): AC2 reworded at the user's choice. The old text was impossible to pass, because a route that answers an export request from a local copy still emits a request to jmgirard.github.io. T2 gained the copies and a plant.
- re-audit: AC2 (reduced) — the first rewording counted a proxy domain, and under FORM_TARGET the target shares the exports' host. It was narrowed to the listener's two events and a base-URL prefix.
- re-audit: AC2 (reduced) — nothing.
- 2026-09-30: T1 done. `NEXT` holds the 15 sentences in full, and a test checks it has each pair once. The count takes `;`. `readmeSlugs()` skips fences, and its plant test went red with the skip removed.

## Decisions

- D1 (2026-09-30): The spec's export copies are committed files under `tests/fixtures/exports/`, copied from hitop's `pkgdown/assets/downloads/`, rather than fetched once per run. The spec then needs no network. The copies can fall behind the site, and the fixtures README names the source commit.
