# M103: One build at a time in the builder, with its settings fixed at the click

- **Status:** in-progress
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** —
- **Resolves:** —
- **Surface tier:** user-facing — the public builder page that researchers use to download forms
- **Branch/PR:** `m103-builder-build-lock` (hitop and jmgirard/hitop-builder)

## Goal

A builder build runs alone, on the settings it started with, so each saved bundle holds only the files of that build.

## Scope

**In:** the following changes to `index.html` in jmgirard/hitop-builder.

- A page-level flag marks a build as running. `download()` sets it at entry and clears it in its `finally` block.
- `refreshTally()` keeps `.downloads button` disabled while the flag is set.
- While the flag is set, a call to `download()` returns at once.
- `download()` reads every control value before its first `await`. Today the paper size radio, `namingValue()` for three names, and `el('required')` come after it.

The milestone also adds a smoke-test assertion for a tick during a build, and a plant that turns that assertion red. If the new behavior contradicts a sentence in `#downloadHint` or README.md about when the button is on or off, the milestone rewrites that sentence. In this repo, the milestone changes tracking files only.

**Out:** The zip-reader gaps in the smoke test stay in their candidate row: a corrupt archive, an unknown method, and entry order. The milestone adds a note to that row, because M103 edits the harness but not the reader. The writer-guard gaps in `tests/prose.mjs` stay in their candidate row. The milestone does not give each build its own scratch directory, and the work log records the reason.

## Acceptance criteria

- [ ] AC1: While a build runs, no code path in `index.html` enables the download button. The domain is every write to a `disabled` state, found by `grep -n "disabled" index.html`. At the end of a build, with at least one scale ticked and boot not abandoned, the button is enabled again. A headless run does four actions during a build: it ticks a scale, unticks a scale, presses Select all, and presses Clear all. After each action, the button's `disabled` property reads `true`.
- [ ] AC2: A call to `download()` during a build returns and starts no second build. A headless run starts a Word build, ticks the shuffle box, and calls `download()` again during the build. The run records one download event and one log line that starts with `> generate_`. The bundle name, the questionnaire entry name and the `.json` entry name carry the stem of the first call, which is not shuffled.
- [ ] AC3: A build uses the settings that were current when it started. No read of a form control's `.value` or `.checked` state, however the control is found, comes after the first `await` in `download()`, except inside the `refreshTally()` call in its `finally` block, which redraws the page after the save and passes nothing to the build. The domain is the body of `download()` and each page function that it calls directly or transitively. Headless runs change one setting during a build and find the starting value in the bundle, for three formats:
  - Word: the paper size, in the `w:pgSz` element of the `.docx`.
  - Qualtrics: the block name, in the `[[Block:` line of the `.txt`.
  - REDCap: the required box, in the required column of the dictionary.
- [ ] AC4: Each sentence in `index.html` and README.md about when the download button is on or off agrees with the behavior that AC1 and AC2 verify. The domain is the paragraphs that `grep -n -i -E "button|turns? (on|off)|is off|off (until|while)" index.html README.md` matches, each read whole.
- [ ] AC5: The builder's smoke suite (`npm run smoke`) passes locally and on the CI of the hitop-builder pull request.

## Coverage

- AC1 → T2, T3, T6
- AC2 → T3, T6
- AC3 → T4, T6
- AC4 → T7
- AC5 → T2, T5, T8

## Tasks

- [x] T1: Make sure that hitop-builder PR #17, which holds the M101 code, is merged. Jeff approved it at the M101 gate and again at this plan gate. If it is still open, merge it from a session whose working directory is inside hitop-builder. Then cut `m103-builder-build-lock` from the updated builder `main` and from hitop `main`.
- [x] T2: Write the test first. Add assertion A8 to `tests/smoke.spec.js`. After the Word build click, the test ticks one more scale and asserts that the download button is disabled. Add A8 to the assertion list in the file header. Run the test on the unfixed page and see it fail.
- [x] T3: Add the flag. Set it at the entry of `download()`, after the return for an empty selection. Clear it in `finally`, before the call to `refreshTally()`. Add it to the disabled expression in `refreshTally()`. While the flag is set, make `download()` return at once.
- [ ] T4: Move each control read in `download()` and its callees above the first `await`. Today these are `papersize`, the three `namingValue()` calls and `el('required')`. In the work log, list each read with its line number and the line number of the first `await`.
- [ ] T5: Add a plant to `tests/plants.mjs` that removes the flag from `refreshTally()`. Run `npm run plants`. Make sure that each plant is red on its named assertion, and that the new plant fails A8.
- [ ] T6: Run headless probes on the served branch. For AC1, do the four selection actions. For AC2, make the double call with shuffle ticked between the calls. For AC3, do the three formats. Write one work-log line for each criterion.
- [ ] T7: Run the AC4 grep and read each matched paragraph. Rewrite each sentence that the T6 runs contradict, for example `#downloadHint` at `index.html:612` and README.md §What the page shows. In the work log, record the matched paragraphs and a verdict for each.
- [ ] T8: Run `npm run smoke` locally. At review, open the hitop-builder PR and wait for its smoke run.

## Work log

- 2026-09-21: created by /milestone-plan from the M089 F1/F2 candidate row. The criteria audit ran in full mode with an [O] reader and returned 12 findings, all fixed before the gate. AC2 now looks for a `> generate_` log line, because the page prints no `hitop::` prefix. The README entry carries no stem. The second call needs a different stem. AC1 allows the case where the page abandoned boot. The AC1 domain moved from `refreshTally` call sites to writes of `disabled`. AC3 states its callee domain and adds a REDCap probe. The AC4 grep is wider. Three clauses about instruments moved to tasks: A8, the list of reads, and the Review quotations.
- 2026-09-21: the plan gate chose to run one build at a time with a page-level flag, not to give each build its own scratch directory. With separate directories, two saves can still interleave, and the promise about the button stays false. A case where a visitor needs two builds at once falsifies this choice.
- 2026-09-21: the plan gate put the settings leak (reads after the first `await`) in this milestone, not in a new candidate row. It kept the zip-reader row separate.
- 2026-09-21: the plan gate chose to merge hitop-builder PR #17 now. The merge guard refused the merge from the hitop session, because the guard finds the repo from the working directory of the session. The merge moved to T1.
- 2026-09-21: T1 done. Jeff confirmed the merge again in this session, and the session moved into hitop-builder. PR #17 was squash-merged as `bd24032`, and `m103-builder-build-lock` was cut from it in both repos.
- 2026-09-21: implement gate. AC3 amended (substantive, narrowing): `refreshTally()` in `finally` reads controls after the first `await` to redraw the page, so AC3 now excepts that one call. Its domain is now value/checked reads, found in direct and transitive callees. A second call to `download()` during a build returns silently.
- re-audit: AC3 (full) — the first reader returned 3 wording faults, all fixed in the written text. `status()` calls `el(` after the await. The exception sat outside the no-read sentence. The callee domain read as direct calls only. It also noted that the probes skip idPrefix and formName, which is proportionate.
- re-audit: AC3 (full) — the second reader returned nothing blocking. It noted that a Word probe can pass on the unfixed page, so T6 also runs each probe against `main`.
- 2026-09-21: T2 done. A8 in `tests/smoke.spec.js` goes back to step one after the Word click, ticks a second scale, and reads `isDisabled()` once. On the unfixed page it fails with `Received: false`, and A4 to A6 pass.
- 2026-09-21: T3 done. `let building` sits beside `bootAbandoned`. `download()` returns at its first line while the flag is set, sets it after the empty-selection return, and clears it first in `finally`. `refreshTally()` ORs it into the disabled expression. `npm run smoke` passes (A8 now green), and `tests/prose.mjs` reports 18 of 18 writer sites in the ledger.

## Decisions

## Review
