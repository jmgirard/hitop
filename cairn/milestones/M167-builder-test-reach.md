# M167: Module Builder test reach

- **Status:** review
- **Priority:** high
- **Depends on:** M166
- **Driving RR:** —
- **Principles touched:** IP2
- **Resolves:** —
- **Surface tier:** internal — the builder repo's smoke test, zip reader and prose ledger, which no researcher runs
- **Branch/PR:** m167-builder-test-reach, companion: /Users/jmgirard/github/hitop-builder m167-builder-test-reach

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

- [x] AC1: The smoke test runs a Word, a Qualtrics and a REDCap build. For each, it asserts that the sorted entry names of the bundle equal a sorted list stated in the spec file, not read off the page. It also asserts that the questionnaire entry holds more than zero bytes. Each message says that the assertion checks which entries the bundle holds, not their order.
- [x] AC2: A browser-free test in the spec file makes four defective copies of one zip. The defects are a central-directory record with a wrong signature, fewer records than the end record counts, a compression method other than 0 or 8, and an entry whose data runs past the end of the buffer. `zipEntries()` returns an empty map for each of the four copies, and the full entry list for the unaltered zip. The zip is built inside the test or committed in `tests/`, never read from a file that a browser test saved.
- [x] AC3: A28 records the Word and Online build statuses through an observer set before the press, as it does for Qualtrics and REDCap. It disconnects each observer after the status of that build returns to "Ready.". A20 waits on the status prefix "Starting R" and asserts the exact loading text in its soft read.
- [x] AC4: During a build, a smoke step moves focus to an enabled control other than the download button. After the build ends, the step asserts that focus is still on that control.
- [x] AC5: `tests/prose.mjs` counts the sites in `index.html` of the write forms in one list in the file. The list holds `textContent`, `innerHTML`, `setAttribute`, `innerText`, `outerHTML`, `insertAdjacentText`, `insertAdjacentHTML`, `append(`, `prepend(`, `replaceChildren(` and `createTextNode(`. The script refuses to run when that count differs from the number of `WRITERS` rows, as it does today for the three forms. The comment in `smoke.yml` names the same list. Under `--ref`, a body-text floor failure or a writer-count difference alone warns on stderr and sets no nonzero exit. The retired-name and `--compare` exits stay as they are.
- [x] AC6: During one build, the smoke run emulates `prefers-color-scheme: dark`. It asserts the A10 card-look comparison against the disabled download button in that scheme.
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

- [x] T1: Make `zipEntries()` (`tests/smoke.spec.js:71`) refuse the four AC2 defects. Add the browser-free test, which builds each defective copy from a zip made in the test or committed in `tests/`.
- [x] T2: Add the Qualtrics and REDCap entry checks to the A28 builds (`smoke.spec.js:1032`). Change A4 to sorted membership. State the expected names beside `FORMAT_NAMES`.
- [x] T3: Set the Word and Online status observers before their presses. Disconnect every A28 observer at "Ready.". Change the A20 wait to the prefix (LESSONS M127).
- [x] T4: Add the focus-stays step to a build other than the Word build, because A11 needs focus on the body at the end of that build. Use a control that stays enabled during a build, for example the "Technical details" summary.
- [x] T5: Widen the writer grep of `prose.mjs` (`prose.mjs:241`) to the AC5 list. Classify any new sites in `WRITERS`. Under `--ref`, turn the floor failure and the writer-count throw into warnings. Update the comment in `smoke.yml`.
- [x] T6: Add the dark-scheme read to one A28 build with `page.emulateMedia({ colorScheme: 'dark' })`. Restore light after it.
- [x] T7: Add plants to `tests/plants.mjs`, so that one plant fails each new or changed assertion. Update the header list and the README file table. Run `npm run smoke`, `npm run plants` and `npm run prose` with nothing else running on the machine. The companion PR opens at review.

## Work log

- 2026-10-06: created by /milestone-plan, together with M166, from the "[high] Module Builder test reach" candidate row. The plan commit retires the row. M166, M167 or the Out list of this file holds each of its items.
- 2026-10-06: plan chose sorted membership for A4 over an order assertion, because no researcher sees the entry order of a zip. Falsified by a tool that reads a bundle in entry order.
- 2026-10-06: plan chose a dark read of A10 inside the run over a second full run in the dark scheme. A second run boots webR again and breaks the 25-minute CI budget in `playwright.config.js`. Falsified by a dark-only defect outside the card look.
- 2026-10-06: question set: part-level plants for A23's counts and A28's card and button — none, declined by Jeff. Outage runs — keep red, declined by Jeff. Dark scheme — one dark read during a build.
- 2026-10-06: criteria audit (reduced mode, fresh Opus reader) returned 4 findings, all fixed. AC2 promised every defective buffer: it now names the four copies the test makes, from a zip built in the test or committed, not one a browser test saved. AC5 promised a per-site check that the count-based `checkWriters()` does not make: it now promises the count, as today, and names the forms. The `--ref` clause clashed with the writer-count throw and the other exits: only the floor and the count now warn. T4 put the step in A11's Word build: it now uses another build.
- 2026-10-06: implement started. Branches cut in hitop and hitop-builder from main after M166 (hitop-builder 0b7626a). The untracked `devel/hitopdat_*` files in hitop stay unstaged. The R verify slot does not apply, because no R code changes; the companion's smoke run is the verify step.
- 2026-10-06: T1 done (hitop-builder b846ba4). `makeZip()` in the spec writes a three-entry zip byte by byte. The new test made four damaged copies; the old `zipEntries()` returned entries for all four, and the new one returns an empty map for each and all three entries with their bytes for the whole zip. The reader also refuses a local header with a wrong signature and a failed inflate.
- 2026-10-06: T2 and T3 done (hitop-builder 9147e1b). `BUNDLE_ENTRIES` beside `FORMAT_NAMES` states each bundle's three names. A4 compares sorted names, and new step A34 checks the Qualtrics and REDCap bundles' sorted names and a non-empty questionnaire entry. `watchStatus()` and `takeStatuses()` record every status a build writes from before its press and disconnect the observer at "Ready."; the Word, Online, Qualtrics and REDCap reads use them. A20 waits on `/^Starting R/`. Local smoke: 4 passed (31.7s).
- 2026-10-06: T4 done (hitop-builder b63da1d). A35 moves focus to the "Technical details" summary during the Qualtrics build, reads in the same call that the download button is still off, and after "Ready." reads focus still on the summary. Local smoke: 4 passed (32.4s).
- 2026-10-06: T5 done (hitop-builder 24fa8e4). `WRITE_FORMS` holds the eleven AC5 forms, and the grep built from it found 27 sites: the 21 before and 6 `.append(` calls, now classified in `WRITERS`. `ledgerProblem()` warns under `--ref` and throws otherwise. Probes on two throwaway commits: an extra `innerText` write and a stray body text node each exited 0 under `--ref` with a warning. `--ref 059a06f` (before M147) also lacked `scaleRowText()` and `buildStatus()`. A missing extraction site now warns under `--ref` too, beyond AC5, and that ref exits 1 on 47 retired names, which AC5 keeps. `smoke.yml` names the list. `npm run prose` exits 0.
- 2026-10-06: T6 done (hitop-builder 9ff70d7). The dark read went into the Word build, not an A28 build as T6 said, because M105 records a Qualtrics build ending before an unheld read and A10 already reads reliably in the Word build (minor deviation). `readCardLook()` serves A10 and new step A36. A36 emulates the dark scheme, reads the cards against the disabled button, and requires `matchMedia` dark and a button background different from the light read, then restores light. Local smoke: 4 passed (32.5s).
- 2026-10-06: T7 code (hitop-builder 97c3981, 8a74e7b). Plants ax (Qualtrics questionnaire named `.qsf`, for A34), ay (focus taken back wherever it is, for A35) and az (a solid card border in the dark block only, for A36). Changed assertions A4, A20 and A28 keep the plants that failed them before. README's file table and the spec header name the four tests and the new steps. Smoke and the plant matrix are running.
- 2026-10-06: the first plant run on 8a74e7b failed: plants aj and at no longer turned the test red. T3's `watchStatus()` seeded the record with the status on show before the press, "Ready. <format> chosen.", which names the format, so A28 passed a build status without the name. The record now starts empty (hitop-builder 42fa366). By hand on planted copies, aj and at each failed A28 alone. A stray command imported `plants.mjs` and started a second matrix; it was stopped within two minutes, and no process was left. The full matrix reruns on 42fa366.
- 2026-10-06: T7 done. On hitop-builder 42fa366, local smoke passed (4 passed, 31.4s), and `npm run plants` ran alone: 36 enumerated assertions, the unplanted copy passed, all 53 plants red, every assertion covered. Plant ax failed A34 alone, ay A35 alone, az A36 alone, and aj and at A28. `npm run prose` exits 0. Status set to review.
- 2026-10-06: claim audit: not owed — internal tier

## Decisions

## Review

- AC1: met. Fresh local smoke on hitop-builder 42fa366: 4 passed (32.3s). A4 compares the Word bundle's sorted names with `bundleNames('docx')`, and A34 the Qualtrics and REDCap bundles' with `BUNDLE_ENTRIES`, a list in the spec, with a non-empty questionnaire entry. The Word questionnaire's size is A6's (more than 10,000 bytes). The A4 and A34 messages say "whatever their order". Plant ax (a `.qsf` name) failed A34 alone, and plants d and g failed A4 and A34.
- AC2: met. The browser-free test "the zip reader refuses a damaged archive" passed in 2ms. It builds the zip with `makeZip()` in the spec, and four damaged copies (wrong record signature, end-record count one higher, method 12, compressed size past the end) each read as an empty map, the whole zip as its three entries with their bytes. Before the fix, the old reader returned entries for all four (T1 work log).
- AC3: met. A28's Word, Online, Qualtrics and REDCap status reads go through `watchStatus()` before the press and `takeStatuses()` after "Ready.", which disconnects the observer. A20 waits on `/^Starting R/` and its soft read holds the exact loading text. Smoke passed, and plants aj and at failed A28 (plant run on 42fa366).
- AC4: met. A35 passed in the Qualtrics build: focus moved to the "Technical details" summary while the download button was off, and was still there after "Ready.". Plant ay failed A35 alone.
- AC5: met. `WRITE_FORMS` lists the eleven forms, and `npm run prose` exited 0 with 27 sites and 27 rows. The `smoke.yml` comment names the same forms. Working tree: an added `innerText` write gave exit 1 (28 sites, 27 rows). Under `--ref` on two throwaway commits, the same write and a stray body text node each gave exit 0 with a warning. `--ref 059a06f` still exits 1, on 47 retired names, a check AC5 keeps.
- AC6: met. A36 passed during the Word build: with `prefers-color-scheme: dark` emulated, the four cards matched the disabled button on all six properties, `matchMedia` read dark, and the button's background differed from the light read. Plant az (a dark-only solid border) failed A36 alone.

- Gate: `cairn_validate` exit 0. No file outside `cairn/` changed in hitop, so `devtools::document()` and `check()` have no new input. M166's pass gave 0/0/0 on the same package files. NEWS: none, because this repo carries tracking only.
- spawned: diff-bug, blame-history, prior-review
- diff-bug #1: A35 reads the button's state after the Qualtrics press with nothing holding the build, so a fast build gives a false red — fix now (the press, the focus move and the read happen in one call).
- diff-bug #2: A36's reads run before A11's blur, which widens A11's false-red window — fix now (A36 moves after the blur).
- diff-bug #3: `zipEntries()` trusts the end record's count and returns a subset when it is too low — fix now (the reader must end where the central directory ends).
- diff-bug #4: duplicate entry names collapse in the map, so four entries with two README.txt read as three — fix now (a repeated name is refused).
- diff-bug #5: `--ref` also relaxes anchor, missing-function and README failures, beyond AC5 — fix now. The missing-function and README relaxations are reverted. Pre-M147 refs exit 1 on retired names anyway, so they bought nothing. The anchor check stays a warning as part of the writer ledger.
- diff-bug #6: "body-text floor" can mean the empty-body check, which still exits 1 under `--ref` — fix now (both body checks warn under `--ref`).
- diff-bug #7: prose.mjs header names only two `--ref` warnings, with one unwrapped line — fix now.
- diff-bug #8: README prose.mjs row's `--ref` list is incomplete — fix now (matches the code after #5).
- diff-bug #9: `WRITE_FORM_RE` skips `??=`, bracket and spaced forms, and counts a non-DOM `.append(` — noted. AC5 binds the named list, and every `.append(` site today is classified.
- diff-bug #10: the zip-reader test has no A-number, so no plant proves it can fail — noted. Its damaged copies are its plants, and T1 showed the old reader failing it.
- diff-bug #11: the light read's scheme is not asserted — noted. `schemeChanged` covers it.
- diff-bug #12: A35 does not read focus on the summary during the build — fix now, with #1.
- diff-bug #13: light restored explicitly, not to the default — noted. Equivalent under the config.
- blame-history #1: `--ref` warnings can let a half-read baseline exit 0 — fix now, as diff-bug #5.
- blame-history #2: A4 no longer pins entry order — reject, planned change (plan work log: no researcher sees entry order).
- blame-history #3: `playwright.config.js` comment calls the boot test the first and counts "the other two tests" — fix now.
- blame-history #4: the ledger skips other DOM write forms — noted, as diff-bug #9.
- blame-history #5: A36 lengthens the Word build's reads — fix now, as diff-bug #2. The `stillBuilding` false red stays possible and is a false red only.
- blame-history #6: the Word build's status record spans A8 to A11 — noted. No status naming "Word form" is written there.
- blame-history #7: the Online record also holds the "Ready." from `removeNextStep()` — noted, `statusNaming()` picks the right text.
- blame-history #8: prose.mjs header splice — fix now, as diff-bug #7.
- blame-history #9: zip-reader test outside the plant list — noted, as diff-bug #10.
- prior-review #1: A35 race — fix now, as diff-bug #1.
- prior-review #2: `--ref` drops passages and so narrows the retired-name scan — fix now, as diff-bug #5.
- prior-review #3: no plant for the zip-reader test — noted, as diff-bug #10.
- prior-review #4: duplicate names and a low count — fix now, as diff-bug #3 and #4.
- prior-review #5: `--ref` header and README case lists — fix now, as diff-bug #7 and #8.
- prior-review #6: a timeout inside A36's poll leaves the page dark — noted. The test has already failed then, and the page ends with it.
