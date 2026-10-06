# M167: Module Builder test reach

- **Status:** planned
- **Priority:** high
- **Depends on:** M166
- **Driving RR:** —
- **Principles touched:** IP2
- **Resolves:** —
- **Surface tier:** internal — the builder repo's smoke test, zip reader and prose ledger, which no researcher runs
- **Branch/PR:** —

## Goal

The Module Builder's smoke test and prose ledger catch the defects that the M076 to M147 reviews found they can miss.

## Scope

**In:** Changes in hitop-builder `tests/` and `smoke.yml`:
- bundle-entry checks for the Qualtrics and REDCap builds that the smoke test already runs,
- a `zipEntries()` that refuses a malformed archive,
- an A4 message that agrees with its assertion,
- A28's status reads and A20's wait,
- a focus-stays step,
- the write forms of `prose.mjs` and the exit of its `--ref` option,
- a dark-scheme read of A10's card look.

This repo gets tracking only.

**Out:**
- The failure behavior of the page belongs to M166.
- Headed and non-Chromium runs stay out. CI runs headless Chromium only (DESIGN Known issues 13 and 14).
- A run against the deployed page still goes red on a webR or r-universe outage. The header of `smoke.yml` gives the reason.

## Acceptance criteria

- [ ] AC1: The smoke test runs a Word, a Qualtrics and a REDCap build. For each, it asserts that the sorted entry names of the bundle equal a sorted list stated in the spec file, not read off the page. It also asserts that the questionnaire entry holds more than zero bytes. Each message says that the assertion checks which entries the bundle holds, not their order.
- [ ] AC2: A browser-free test in the spec file makes four defective copies of one zip. The defects are a central-directory record with a wrong signature, fewer records than the end record counts, a compression method other than 0 or 8, and an entry whose data runs past the end of the buffer. `zipEntries()` returns an empty map for each of the four copies, and the full entry list for the unaltered zip. The zip is built inside the test or committed in `tests/`, never read from a file that a browser test saved.
- [ ] AC3: A28 records the Word and Online build statuses through an observer set before the press, as it does for Qualtrics and REDCap. It disconnects each observer after the status of that build returns to "Ready.". A20 waits on the status prefix "Starting R" and asserts the exact loading text in its soft read.
- [ ] AC4: During a build, a smoke step moves focus to an enabled control other than the download button. After the build ends, the step asserts that focus is still on that control.
- [ ] AC5: `tests/prose.mjs` counts the sites in `index.html` of the write forms in one list in the file. The list holds `textContent`, `innerHTML`, `setAttribute`, `innerText`, `outerHTML`, `insertAdjacentText`, `insertAdjacentHTML`, `append(`, `prepend(`, `replaceChildren(` and `createTextNode(`. The script refuses to run when that count differs from the number of `WRITERS` rows, as it does today for the three forms. The comment in `smoke.yml` names the same list. Under `--ref`, a body-text floor failure or a writer-count difference alone warns on stderr and sets no nonzero exit. The retired-name and `--compare` exits stay as they are.
- [ ] AC6: During one build, the smoke run emulates `prefers-color-scheme: dark`. It asserts the A10 card-look comparison against the disabled download button in that scheme.
- [ ] AC7: The smoke test passes locally and on the CI of the hitop-builder pull request. `npm run prose` exits 0.

## Coverage

- AC1 → T2
- AC2 → T1
- AC3 → T3
- AC4 → T4
- AC5 → T5
- AC6 → T6
- AC7 → T7

## Tasks

- [ ] T1: Make `zipEntries()` (`tests/smoke.spec.js:71`) refuse the four AC2 defects. Add the browser-free test, which builds each defective copy from a zip made in the test or committed in `tests/`.
- [ ] T2: Add the Qualtrics and REDCap entry checks to the A28 builds (`smoke.spec.js:1032`). Change A4 to sorted membership. State the expected names beside `FORMAT_NAMES`.
- [ ] T3: Set the Word and Online status observers before their presses. Disconnect every A28 observer at "Ready.". Change the A20 wait to the prefix (LESSONS M127).
- [ ] T4: Add the focus-stays step to a build other than the Word build, because A11 needs focus on the body at the end of that build. Use a control that stays enabled during a build, for example the "Technical details" summary.
- [ ] T5: Widen the writer grep of `prose.mjs` (`prose.mjs:241`) to the AC5 list. Classify any new sites in `WRITERS`. Under `--ref`, turn the floor failure and the writer-count throw into warnings. Update the comment in `smoke.yml`.
- [ ] T6: Add the dark-scheme read to one A28 build with `page.emulateMedia({ colorScheme: 'dark' })`. Restore light after it.
- [ ] T7: Add plants to `tests/plants.mjs`, so that one plant fails each new or changed assertion. Update the header list and the README file table. Run `npm run smoke`, `npm run plants` and `npm run prose` with nothing else running on the machine. The companion PR opens at review.

## Work log

- 2026-10-06: created by /milestone-plan, together with M166, from the "[high] Module Builder test reach" candidate row. The plan commit retires the row. M166, M167 or the Out list of this file holds each of its items.
- 2026-10-06: plan chose sorted membership for A4 over an order assertion, because no researcher sees the entry order of a zip. Falsified by a tool that reads a bundle in entry order.
- 2026-10-06: plan chose a dark read of A10 inside the run over a second full run in the dark scheme. A second run boots webR again and breaks the 25-minute CI budget in `playwright.config.js`. Falsified by a dark-only defect outside the card look.
- 2026-10-06: question set: part-level plants for A23's counts and A28's card and button — none, declined by Jeff. Outage runs — keep red, declined by Jeff. Dark scheme — one dark read during a build.
- 2026-10-06: criteria audit (reduced mode, fresh Opus reader) returned 4 findings, all fixed. AC2 promised every defective buffer: it now names the four copies the test makes, from a zip built in the test or committed, not one a browser test saved. AC5 promised a per-site check that the count-based `checkWriters()` does not make: it now promises the count, as today, and names the forms. The `--ref` clause clashed with the writer-count throw and the other exits: only the floor and the count now warn. T4 put the step in A11's Word build: it now uses another build.

## Decisions

## Review
