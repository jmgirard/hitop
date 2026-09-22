# M104: The builder's format cards are off during a build

- **Status:** review
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** —
- **Resolves:** —
- **Surface tier:** user-facing — the public builder page that researchers use to download forms
- **Branch/PR:** `m104-builder-format-cards-lock` in both hitop and jmgirard/hitop-builder

## Goal

While a builder build runs, the page keeps naming the format it is building.

## Scope

**In:** the following changes to `index.html` in jmgirard/hitop-builder.

- `download()` disables the three format cards (`[data-choose]`) inside its `try`, where it disables `.downloads button`. Its `finally` turns them back on.
- The comment at `let building` and the card-handler comment in `main()` say that the cards are off during a build.

- The step-two hint and README.md §What the page shows each gain a clause that the cards are off while a build runs.

The milestone also adds a smoke-test assertion that presses a card during a build, and a plant that turns that assertion red. If the new behavior contradicts another sentence in `index.html` or README.md about the cards, the milestone rewrites that sentence. In this repo, the milestone changes tracking files only.

**Out:** The zip-reader gaps in the smoke test stay in their candidate row, which M103 already annotated. The writer-guard gaps in `tests/prose.mjs` stay in their candidate row, because this milestone adds no text writer. The settings panels' own controls stay on during a build, because `download()` reads them at the click (M103). Deferring a card press until the build ends is not built, and the work log records the reason.

## Acceptance criteria

- [x] AC1: While a build runs, the three format cards are disabled. A press on any card leaves the step's format state as it was at the build click. The format state is the text of `#downloadBtn`, the `.fmtpanel` that is not hidden, the card with `aria-current`, and which `.fmtpanel details` are open. A headless run opens the Word settings disclosure and starts a Word build. During the build, it first reads each card's `disabled` property as true and `#downloadBtn` as disabled. It then presses each of the three cards with `locator.click({ force: true })`. A second run opens the Qualtrics settings disclosure, starts a Qualtrics build, and presses the Word card the same way.
- [x] AC2: When a build ends by saving a bundle or by failing, the three cards are enabled again. A headless run reads each card's `disabled` property after a saved Word build. A second run does the same on a served copy of the page whose generator call is replaced by an R `stop()`. After each ending, the run presses the Qualtrics card, and `#downloadBtn` names Qualtrics.
- [x] AC3: No write to a card's `disabled` state exists in `index.html` outside `download()`. The domain is every line that `grep -n "disabled" index.html` lists. Each line is read, with a note on whether its selector matches a `[data-choose]` button.
- [x] AC4: The step-two hint and the comment at `let building` in `index.html` say that the cards are off while a build runs. README.md §What the page shows says the same. Each other sentence about a card press agrees with the behavior that AC1 and AC2 verify. The domain is the paragraphs that `grep -n -i -E "card|data-choose|choos|format button" index.html README.md` matches, each read whole.
- [ ] AC5: The builder's smoke suite (`npm run smoke`) passes locally and on the CI of the hitop-builder pull request.

## Coverage

- AC1 → T2, T3, T5
- AC2 → T3, T5
- AC3 → T3, T5
- AC4 → T6
- AC5 → T2, T4, T7

## Tasks

- [x] T1: Cut `m104-builder-format-cards-lock` from the updated builder `main` and from hitop `main`.
- [x] T2: Write the test first. Add assertion A9 to `tests/smoke.spec.js`, after A8 and during the same build. The test presses the Qualtrics card and asserts that `#downloadBtn` still names Word and the Word card still carries `aria-current`. Add A9 to the assertion list in the file header. Run the test on the unfixed page and see it fail.
- [x] T3: In `download()`, collect the `[data-choose]` buttons next to `buttons`. Disable them after `building = true`. Enable them in `finally`, after `building = false`.
- [x] T4: Add a plant to `tests/plants.mjs` that removes the cards' disable from `download()`. Run `npm run plants`. Make sure that each plant is red on its named assertion, and that the new plant fails A9 alone.
- [x] T5: Run headless probes on the served branch. Do the AC1 presses, the AC2 reads after a saved and a failed build, and the AC3 grep. Write one work-log line for each criterion.
- [x] T6: Add the clause to the step-two hint (`index.html:486`), to the comment at `let building` (`index.html:701`), and to README.md §What the page shows (line 84). Run the AC4 grep and read each matched paragraph. Rewrite each sentence that the T5 runs contradict. In the work log, record the matched paragraphs and a verdict for each.
- [x] T7: Run `npm run smoke` locally. At review, open the hitop-builder PR and wait for its smoke run.

## Work log

- 2026-09-21: created by /milestone-plan from the format-card candidate row (M103 review finding D2). The criteria audit ran in full mode with an [O] reader and returned 9 findings. Six were fixed before the gate. AC1 drops `currentFormat`, which is private to the module script. AC1 also presses all three cards and reads the open disclosure. AC2 names the forced failure and is bounded to a save or a failure. AC3 records each hit's selector. AC4 has a wider grep. One went to the gate (AC4 wording), and two needed no change (AC5, the plant in T4).
- 2026-09-21: the plan gate chose to disable the cards during a build. It rejected a press applied after the build ends and a press ignored silently. A deferred press changes the page with no visible cause, and an ignored press looks like a broken page. A visitor report that the disabled cards are confusing falsifies this choice.
- 2026-09-21: the plan gate chose to state in the step-two hint and README.md that the cards are off during a build. It rejected a page that relies on the greyed look alone. AC4 now requires both sentences. A re-audit of the changed AC1 to AC4 by the same [O] reader returned 3 findings, all fixed before the commit: the second AC1 run opens the Qualtrics disclosure, AC1 reads `disabled` before the presses, and AC4 covers the `let building` comment.
- 2026-09-21: implement started. T1: both branches cut from `main` (hitop at a2b3b2ee, builder at 840dace), both already in sync with origin. No question gate, because the plan left nothing open. The `finally` sets the cards to `bootAbandoned`, as it does for the download button.
- 2026-09-21: T2: A9 added to `tests/smoke.spec.js` (forced press on the Qualtrics card after A8, read once against the button text read before the build). On the unfixed page the smoke run failed on A9 alone: the button read "Download the Qualtrics file (.zip bundle)" and the Word card had no `aria-current`. A8 passed in the same run, so the press landed during the build.
- 2026-09-21: T3: `download()` collects `[data-choose]` as `cards`, disables them after `building = true`, and sets them to `bootAbandoned` in `finally`. `refreshTally()` writes `.downloads button` only, so it cannot turn a card on. Smoke run on the fixed page: 1 passed.
- 2026-09-21: T4: plant (j) removes the `cards.forEach((b) => (b.disabled = true));` line. `npm run plants` exited 0: the unplanted copy passed, and plants a to j each failed their named assertions (j on A9 alone, i on A8 alone, the other eight as before).
- 2026-09-21: T5 AC1: a scratchpad Playwright probe on the served branch (builder at 1ac41d7). Word build with the Word disclosure open: during the build all three cards read `disabled` true and `#downloadBtn` disabled. Forced presses on docx, qualtrics and redcap left the state equal to the click state: Word button text, panel docx, `aria-current` docx, open docx. After the presses the button was still disabled and the status read "Building the DOCX file…". In the Qualtrics build with its disclosure open, a Word card press left all four parts on Qualtrics, and the status read "Building the TXT file…". Both builds saved a bundle. 2 passed.
- 2026-09-21: T5 AC2: the failed run served a copy whose generator line is `stop("M104 planted generator failure")`. Its log read "FAILED: Error in `eval(expr, env)`: M104 planted generator failure". After that failure, and after a saved Word build, all three cards read `disabled` false. A Qualtrics card press then set `#downloadBtn` to "Download the Qualtrics file (.zip bundle)" in both runs. 2 passed.
- 2026-09-21: T5 AC3: `grep -n "disabled" index.html` lists 22 lines. `.formats` at 490 closes before `.downloads` at 621, so `.downloads button` never matches a card. Writes to a `disabled` state: 717 and 934 (`.downloads button`, not a card), 1128 (`#selectAll`, not a card), 1280 and 1405 (`buttons` = `.downloads button`, not a card), 1281 and 1406 (`cards` = `[data-choose]`, inside `download()`, which spans 1259 to 1409). Line 622 is the `disabled` attribute on `#downloadBtn` in the markup, not a card. The other 14 lines are CSS (146, 147, 150, 151, 158, 159, 160, 267, 415, and the variables at 19 and 50) or comments (12, 709, 836, 1494). Line 267 names the cards but only reads `:disabled` for a hover style. No card write exists outside `download()`.
- 2026-09-21: T6: clause added to the step-two hint (index.html:489), the `let building` comment (704 to 706), and README.md §What the page shows (96 to 97: "The button and the three cards are off while a build is running."). Two sentences rewritten. The `main()` card-handler comment (1659 to 1666) said the status write waits for an idle page so that a build's progress is not written over. It now says the write waits for "Ready." and that no press lands during a build. README.md:112 to 114 said a press "while the page is idle" names the format. After a failed build the page is idle and the press left "The DOCX build failed. The log below says why." in place, so the sentence now says that. Both rewrites rest on a re-run of the T5 probes on the edited page (4 passed, status read after each press). Smoke: 1 passed.
- 2026-09-21: T6 AC4 ledger, after the edits. Agree, no change: index.html 252 to 253 (CSS, a card selects its format in place), 270 (CSS, the current card's mark), 476 (step comment), 505 and 537 (disclosure closes on a card press), 779 (`currentFormat` changes on a card press), 841 to 845 (`setFormat`), 857 to 863 (`markFormatChoice`), 1269 to 1283 and 1408 (the T3 lines). README.md 82 to 90 (the cards, a press switches the format, the disclosure closes on a press). Changed: index.html 487 to 489 (hint), 704 to 706 (`let building`), 1659 to 1671 (card handler), and README.md 96 to 97 and 112 to 114. Not about a card press: README.md 4, 68, 73, 130 and 251, and index.html 428, 456, 457, 462, 483 and 491 to 499 (the words "choose" or "chooses", headings, and the card markup).
- 2026-09-21: T7: `npm run smoke` 1 passed locally on builder d676de1. The PR and its CI smoke run are left to review.
- 2026-09-21: claim audit: 20 claims read, 2 corrected — hitop-builder index.html, README.md, tests/smoke.spec.js, tests/plants.mjs. The audit read the builder branch diff, because hitop's diff outside `cairn/` adds no lines. The [O] reader corrected the card-handler comment ("reads" became "begins with", to match `startsWith('Ready.')`) and the forced-click comment (without `force`, Playwright waits up to the 30-second action timeout, and a build that ends inside it gives a false red on A9). The same reader re-read both as true. Builder d676de1, smoke 1 passed.
- 2026-09-21: implement complete, status set to review.

## Decisions

## Review

Sync: hitop branch contains origin/main (a2b3b2ee); builder branch contains origin/main (840dace). Evidence below is on builder d676de1.

- AC1: a Playwright probe (the T5 probe, copied to this session's scratchpad and read before the run) served the branch checkout. Word build with the Word disclosure open: during the build all three cards read `disabled` true and `#downloadBtn` read disabled. Forced clicks on docx, qualtrics and redcap left the state equal to the click state: Word button text, panel docx, `aria-current` docx, open docx. The button was still disabled after the presses, and the status read "Building the DOCX file…". Qualtrics build with its disclosure open: the same `disabled` reads, and a forced Word press left all four parts on Qualtrics, with status "Building the TXT file…". Both builds saved a bundle. 2 passed. Pass.
- AC2: after a saved Word build, all three cards read `disabled` false. A Qualtrics press set `#downloadBtn` to "Download the Qualtrics file (.zip bundle)" and the status to "Ready. Qualtrics file chosen.". The failed run served a copy whose generator call is `stop("M104 planted generator failure")`. Its log read "FAILED: Error in `eval(expr, env)`: M104 planted generator failure". The cards then read `disabled` false, and a Qualtrics press set the same button text. The status kept "The DOCX build failed. The log below says why.". 2 passed. Pass.

- AC3: `grep -n "disabled" index.html` lists 22 lines. Lines 719 (`abandonBoot`) and 936 (`refreshTally`) write `.downloads button`. Line 1130 writes `#selectAll`. Lines 1282 and 1407 write `buttons` (`.downloads button`, set at 1268). Lines 1283 and 1408 write `cards` (`[data-choose]`, set at 1272), inside `download()` (1261 to 1411). The three `[data-choose]` buttons (491, 495, 499) sit in `.formats`, which closes at 503. `.downloads` opens at 621 and holds only `#downloadBtn`, so no `.downloads button` write reaches a card. The other 15 lines are CSS (19, 50, 146, 147, 150, 151, 158, 159, 160, 267, 415), comments (12, 711, 838, 1496) and the markup attribute on `#downloadBtn` (622). Pass.
- AC4: the grep matched 50 lines (37 in index.html, 13 in README.md), each paragraph read whole. The step-two hint has the clause (index.html:489, "The cards are off while a build runs."). So do the `let building` comment (704 to 706) and README.md §What the page shows (96 to 97). These card-press sentences agree with the AC1 and AC2 runs: index.html 252 to 253, 270 to 272, 476 to 481, 504 to 509, 536 to 539, 777 to 779, 841 to 846, 857 to 860, 1269 to 1271 and 1659 to 1666. In 1659 to 1666, the status write needs a status that begins with "Ready.", as the AC2 status reads show. README.md 82 to 90 and 111 to 114 also agree. A press after a failure leaves the failure message, as the AC2 failed run read. Not about a card press: README.md 4, 68, 73, 130, 251 and index.html 428, 456, 457, 462, 483, 491, 495, 499 (the words "choose" or "chooses", headings, markup), plus the code lines 862 to 863, 1272, 1283, 1408, 1667 to 1671. No contradicting sentence. Pass.
- AC5 (local half): `npm run smoke` 1 passed on d676de1. `npm run plants` exit 0: the unplanted copy passed, and plants a to j each failed their named assertions (j on A9 alone, i on A8 alone). The PR CI half waits for the PR, which opens only after the merge approval. The box stays unticked until that run is green.

Consistency gate: `cairn_validate` exit 0 (24 advisory warnings, all older than this branch). No principle changed, so `cairn_impact` was skipped. `devtools::document()` left no diff. README.Rmd, README.md, NEWS.md and `_pkgdown.yml` are unchanged on the branch, and this repo ships no user-visible change, so no NEWS entry is owed. `pkgdown::check_pkgdown()`: no problems. `devtools::check()`: 0 errors, 0 warnings, 0 notes.

Independent review: three-lens fan-out (user-facing tier). Blame-history lens: no finding beyond B1. Prior-review lens: no prior-review evidence reintroduced or contradicted. Both PR-comment probes returned no comments. Its one note is B1. Diff-bug lens: no correctness bug, eight findings, ranked:

- D1: A9 cannot tell a disabled card from a card whose handler ignores presses. It reads only the button text and `aria-current`, so a page built on the rejected "ignore the press" design passes it (smoke.spec.js:205 to 216).
- D2: A9 does not record whether the build still ran at the press. A build that ends first gives a false red whose message blames the lock (smoke.spec.js:205 to 207).
- D3: the current card barely looks disabled. `.formats button` comes after `button:disabled` at equal specificity and overrides its background and border colour, and `[aria-current=true]` restores the solid accent border (index.html:253 to 281). Confirmed by reading the CSS.
- D4: the card-handler comment gives an incomplete reason. The `startsWith('Ready.')` guard already stops a press from writing over "Building…", so the disabled cards are a second guard (index.html:1665 to 1666).
- D5: the new `download()` comment leaves out that `setFormat()` also closes the open disclosure (index.html:1269 to 1271).
- D6: in Safari and Firefox on macOS a mouse click does not focus a button, so a keyboard-focused card that goes disabled drops focus to the body. `#downloadBtn` has the same loss since M103 (index.html:1283).
- D7: a hung build now also keeps the cards off until a reload, which widens M103's rejected D1 (index.html:1405 to 1408).
- B1 (all three lenses): README.md:97 was not rewrapped. It is 93 characters against 65 to 77 around it.
