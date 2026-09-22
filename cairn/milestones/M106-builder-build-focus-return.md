# M106: Keyboard focus comes back when a builder build ends

- **Status:** review
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** —
- **Resolves:** —
- **Surface tier:** user-facing — the public builder page that researchers use to download forms
- **Branch/PR:** `m106-builder-build-focus-return` in both repos

## Goal

When a builder build ends, keyboard focus is back on the control that had it at the build click.

## Scope

**In:** the following changes in jmgirard/hitop-builder.

- `download()` in `index.html` records `document.activeElement` at the build click. After `finally` turns the controls back on, it checks where focus is. If focus is on the body and the recorded control is enabled and shown, it calls `focus({ preventScroll: true })` on that control. Otherwise it leaves focus where it is.
- The defect: `download()` disables `#downloadBtn` and the three cards during a build. A focused button that goes disabled drops focus to the body, and nothing puts it back. In Chromium the click focuses `#downloadBtn`. In Safari and Firefox on macOS a mouse click does not focus a button, so a card that took focus from the keyboard keeps it until the build disables it.
- The comment in `download()` and README.md §What the page shows say that focus comes back when a build ends.
- A smoke-test assertion reads the focus after the build, and a plant turns it red.

In this repo, the milestone changes tracking files only.

**Out:** The look of the disabled cards → M105. A move of focus to the status line or the log is not built. The settings controls stay on during a build (M103). The zip-reader gaps and the `tests/prose.mjs` writer gaps stay in their candidate rows.

## Acceptance criteria

- [x] AC1: If focus is on the body when a build ends with a saved bundle, and the control that had focus at the build click is enabled and shown, focus returns to that control. Two headless runs test this. In the first run, `#downloadBtn` takes focus from the Playwright `click()`. In the second run, `.focus()` puts focus on the Qualtrics card while Word is the current format. The Word build then starts with `dispatchEvent('click')` on `#downloadBtn`, which moves no focus. During each build, the run reads `document.activeElement` as the body. After the save, the run polls `document.activeElement` for up to 5 seconds and reads it as the control that had focus at the click.
- [x] AC2: The same holds when the build fails. The two AC1 runs are repeated on a served copy of the page, where an R `stop()` replaces the generator call. The run polls after the failure status appears.
- [x] AC3: If the control that had focus at the click is disabled or hidden when the build ends, focus stays on the body. There are two runs, each starting a Word build with the Playwright `click()` on `#downloadBtn`. In the first run, the probe unticks every ticked scale with `dispatchEvent('click')` during the build. Step one and its checkboxes are hidden at that time. So `#downloadBtn` is disabled at the end. In the second run, the probe presses the step bar's first-step button, which hides step two, and then calls `blur()` on the focused heading. After the build ends, each run polls for 5 seconds and reads `document.activeElement` as the body throughout.
- [x] AC4: If focus is on an element other than the body when a build ends, focus stays there. There are two runs. In the first run, the probe opens the Word settings disclosure during the build and focuses `#shuffle`. In the second run, the probe presses the step bar's first-step button during the build, which focuses that step's heading. After the build ends, each run polls for 5 seconds and reads `document.activeElement` as `#shuffle` or as that heading throughout.
- [x] AC5: The return of focus does not scroll the page. In both AC1 runs, the viewport is set to 1280 by 400, and the probe calls `scrollIntoView()` on `#logSection` during the build. It then confirms that the control that had focus at the click lies outside the viewport, and it reads `window.scrollY`. After focus returns, `window.scrollY` equals that value.
- [ ] AC6: The comment in `download()` and README.md §What the page shows say that focus comes back to the control that had it when a build ends. The domain is the comment blocks in `index.html` and the paragraphs in README.md that `grep -n -i -E "focus|keyboard|tab key" index.html README.md` matches, each read whole. Each sentence about focus during or after a build agrees with what AC1 to AC5 verify.
- [ ] AC7: The builder's smoke suite (`npm run smoke`) passes locally and on the CI of the hitop-builder pull request.

## Coverage

- AC1 → T2, T3, T5
- AC2 → T3, T5
- AC3 → T3, T5
- AC4 → T3, T5
- AC5 → T3, T5
- AC6 → T6, T8
- AC7 → T2, T4, T7

## Tasks

- [x] T1: Cut `m106-builder-build-focus-return` from the updated builder `main` and from hitop `main`.
- [x] T2: Write the test first. On the unfixed page, confirm in a probe that focus is on the body during a build after the Playwright `click()`. If it is not, stop and raise it at an amendment gate, because the assertion below cannot then fail. The smoke build presses the step bar, which leaves focus on a step heading (`index.html:912`). So after the A9 and A10 reads and before the download wait, the test calls `blur()` on the focused element, and a comment says that this stands in for a focus lost to the disabled button. Add assertion A11 to `tests/smoke.spec.js` after the save. A11 polls `document.activeElement` and asserts that it is `#downloadBtn`. Add A11 to the assertion list in the file header. Run the test on the unfixed page and see it fail on A11.
- [x] T3: In `download()`, record `document.activeElement` before the controls go off. After `finally` turns them back on and `refreshTally()` runs, return focus as the Scope says. Write the comment that says why.
- [x] T4: Add plant (l) to `tests/plants.mjs`, which removes the focus return. Run `npm run plants`. Make sure that each plant is red on its named assertion, and that plant (l) fails A11 alone.
- [x] T5: Run headless probes on the served branch for AC1 to AC5, with the forced-failure copy for AC2. Write one work-log line for each criterion.
- [x] T6: Write the README.md sentence. Run the AC6 grep and read each matched block. Rewrite each sentence that the T5 runs contradict. In the work log, record the matched blocks and a verdict for each.
- [x] T7: Run `npm run smoke` locally. At review, open the hitop-builder PR and wait for its smoke run.
- [x] T8: Rewrite README.md:102 and the matching clause in the `download()` comment so that they say that focus on another element at the build's end stays there. Run the AC6 grep again and read each matched block.

## Work log

- 2026-09-21: created by /milestone-plan from the disabled-controls candidate row (M104 review), split from M105 (see its work log). The criteria audit ran in full mode with an [O] reader. Six findings were M106 findings, and five were fixed before the gate. They were a card focus on the current card only, polls for a deferred focus, `#shuffle` inside a closed disclosure, a scroll probe that passes on any code, and a narrow grep. The sixth went to the gate: a control that is off or hidden at the build's end.
- 2026-09-21: the plan gate chose to return focus only to a control that is enabled and shown. It rejected a return to `#downloadBtn` in every case, which moves focus to a control the visitor never used. A visitor report that focus lands in the wrong place after a build falsifies this choice.
- 2026-09-21: the plan gate chose a focus return with no scroll. It rejected a plain `focus()`, which moves the page away from the log that a visitor is reading. A keyboard visitor report that focus is lost from view after a build falsifies this choice.
- 2026-09-21: the plan gate chose a smoke assertion plus a plant over one-off probes only, for the reason in M105's work log.
- 2026-09-21: a re-audit of the gate-changed criteria by the same [O] reader (full mode) returned 4 findings, 3 on M106, all fixed after the plan commit. A11 was unable to pass, because the smoke build's step-bar presses leave focus on a heading, so T2 now blurs it first. AC5 now sets a 400 px viewport. AC3 now says that the unticked checkboxes are hidden.
- 2026-09-21: implement started. Both branches cut from their updated `main` (builder at `6ac767e`). The question gate was skipped: the one open choice, how to test that a control is shown, uses `getClientRects().length > 0`.
- 2026-09-21: T2 done. A probe on the unfixed page read focus on the body during a build after the Playwright `click()` and after it ended. A11 and the blur are in `tests/smoke.spec.js`, and `npm run smoke` on the unfixed page failed on A11 alone.
- 2026-09-21: T3 done. `download()` keeps `document.activeElement` before the controls go off. After `refreshTally()` in the `finally`, it checks the focus. If focus is on the body and the kept control is connected, on, and has client rects, it focuses that control with `preventScroll`. `npm run smoke` passed.
- 2026-09-21: T4 done. Plant (l) removes the `focus()` call. `npm run plants` passed: the unplanted copy passed, all 12 plants were red, and plant (l) failed A11 alone.
- 2026-09-21: T5 AC1. Scratch probes on the served branch held webR's worker messages during the build. The `click()` run read the body during the build and `#downloadBtn` after the save. The card run read the Qualtrics card before the dispatch and the body during the build. After the save it read the card. On main's page the card run read the body after the save.
- 2026-09-21: T5 AC2. The same two runs on a copy with `stop()` in place of the generator call reached the status "The DOCX build failed". They read the body during the build and `#downloadBtn` or the Qualtrics card after the failure.
- 2026-09-21: T5 AC3. The untick run unticked 1 hidden checkbox, left 0 ticked, and read the body for 5 s with `#downloadBtn` disabled. The step run read the step-one heading, then the body after `blur()`. It read the body for 5 s after the build.
- 2026-09-21: T5 AC4. The `#shuffle` run and the step-one heading run each read that element for 5 s after the build.
- 2026-09-21: T5 AC5. In both AC1 runs at 1280 by 400, the control was outside the viewport at `scrollY` 1248. After focus returned, `scrollY` was 1248. A copy with a plain `focus()` read 899.
- 2026-09-21: T6 done. README.md step 2 gained four sentences on focus after a build. The AC6 grep matched README.md 76 (Tab popup). In index.html it matched 25/56 (tokens), 96-101 (ring), 377-380 (heading ring), and 917-927 (step heading). It also matched 1019 and 1062 (popup), 1289-1293 and 1431-1447 (build focus), 1699 (card press), and 1727 (first paint). Verdict: the two build-focus blocks and the new README sentences agree with T5. The other blocks say nothing about focus during or after a build. Nothing was rewritten.
- 2026-09-21: T7 local half done. `npm run smoke` passed on the builder branch head. The PR and its smoke run belong to `/milestone-review`.
- 2026-09-21: claim audit: 22 claims read, 3 corrected — hitop-builder index.html, tests/smoke.spec.js. The [O] reader ran on the builder branch diff, because this repo's diff adds no lines outside cairn/. It flagged a Chromium-only focus drop, an unobserved Safari card case, and an unobserved heading focus. The same reader re-read all three as true. It also flagged README claim 18, which stays as written because the next README sentence covers moved focus. Smoke passed after the edits.
- 2026-09-21: implement done, status set to review.
- 2026-09-22: review return 1 (defect): AC6 fails. README.md:102 in hitop-builder says "If you moved focus during the build, it stays where you put it." A visitor who moves focus to a step-bar button and then clicks a spot in the log that takes no focus drops focus to the body, and `download()` returns it to the control from the click. The fix rewrites that sentence so that it agrees with AC1 and AC4, then re-runs the AC6 read. The other nine [O] findings in the Review section wait for triage at the next review gate. Status set to in-progress.
- 2026-09-22: resume after review return 1. Both repos level with `origin/main`. Minor amendment: T8 added for the AC6 fix, Coverage AC6 → T6, T8.
- 2026-09-22: T8 done (builder `f31dd36`). README.md 102-105 now says that focus on another part of the page when the build ends stays there, for example a control moved to during the build. The `download()` comment at index.html 1435-1437 says the same. The AC6 grep matched README.md 76, 99, 102 and 104. The step-2 paragraph (97-107), read whole, now agrees with AC1 to AC5. `npm run smoke` passed.
- 2026-09-22: claim audit: 6 claims read, 1 corrected — hitop-builder README.md. The [O] reader found that "another part of the page" covers a click on the log, after which focus comes back. README.md now says that another element with focus keeps it, and that a click on plain text or the log takes no focus. The reader's other five claims were true. `npm run smoke` passed on `2f4d5a3`.
- 2026-09-22: claim audit re-read of the corrected claim: the log and plain text take no focus, but a click on a step heading (`tabindex="-1"`, index.html 477 and 498) focuses it. README.md now says "the log or text other than a step heading" (builder `f8ee9ed`), and `npm run smoke` passed. No second pass, per the stopping rule.
- 2026-09-22: status set to review.

## Decisions

## Review

Fresh evidence from 2026-09-21, on builder branch head `7e01c50`. Both repos were level with `origin/main`, so no merge was needed. The probes reuse the T5 harness, which holds webR's worker messages to keep a build running. They ran on four fresh copies of the page: the branch page, a copy with `stop()` in place of the generator call, main's page, and a copy with a plain `focus()`.

- AC1: the `click()` run read the body during the build and `#downloadBtn` in every poll read after the save. The card run read the Qualtrics card before the dispatch. It read the body after the dispatch and during the build, and the Qualtrics card in every poll read after the save. As a control, the card run on main's page read the body after the save.
- AC2: the same two runs on the `stop()` copy reached the status "The DOCX build failed. The log below says why." Each read the body during the build. After the failure status, every poll read was `#downloadBtn` in the first run and the Qualtrics card in the second.
- AC3: the untick run unticked 1 checkbox, which was hidden, and left 0 ticked. After the save, `#downloadBtn` was disabled and all poll reads for 5 s were the body. The step run read the step-one heading after the step-bar press and the body after `blur()`. After the save, all poll reads for 5 s were the body.
- AC4: the `#shuffle` run read `#shuffle` after the probe opened the Word settings and focused it. All poll reads for 5 s after the save were `#shuffle`. The step run read the step-one heading after the press, and all poll reads for 5 s after the save were that heading.
- AC5: both AC1 runs used a 1280 by 400 viewport. After `scrollIntoView()` on `#logSection`, the control lay outside the viewport at `scrollY` 1248. After focus returned, `scrollY` was 1248 in both runs. As a control, the `click()` run on the plain `focus()` copy read 899 after focus returned.
- AC6: the grep matched README.md 76 and 99-103. In index.html it matched 25, 56, 96-101, 377-380, 917-927, 1019, 1062-1064, 1289-1295, 1433-1449, 1701 and 1729. Each block was read whole. The README step-2 sentences and the two `download()` blocks agree with AC1 to AC5. They say that focus comes back after a save and after a failure, with no scroll. They also say that it comes back only from the body, and only to a control that is on and shown. The other blocks cover the focus ring, step headings, the scale popup, the card press and the first paint. They say nothing about focus during or after a build.
- AC6 correction after review finding [O]7: the line above is wrong about one sentence. README.md:102 says "If you moved focus during the build, it stays where you put it." A visitor can move focus to a step-bar button and then click a spot in the log that takes no focus. Focus then drops to the body, and `download()` (index.html:1442-1449) returns it to the control from the click. That disagrees with what AC1 verifies, so AC6 fails as written. AC6 is unticked.
- AC7 (local half): `npm run smoke` passed on `7e01c50` (1 test, 10.2 s). `npm run plants` passed: the unplanted copy passed, all 12 plants were red, and plant (l) failed A11 alone. The CI half waits for the builder PR.

Consistency gate: `cairn_validate.py` exit 0, with 24 advisory warnings that predate M106. In hitop, `devtools::document()` left no diff, `pkgdown::check_pkgdown()` found no problems, and `devtools::check()` gave 0 errors, 0 warnings and 0 notes. README.Rmd and README.md are in step. NEWS.md needs no entry, because this repo's diff touches `cairn/` only and the builder has no changelog.

Independent review, three fresh reviewers on the builder diff. The [S] history reader and the [S] prior-review reader found nothing. The [O] diff reader found no bug that breaks the change and reported 10 findings, ranked:

1. A11 guards only the basic return. The suite stays green without the body guard, without `preventScroll`, or with a return to `#downloadBtn` in every case. AC3 to AC5 rest on one-off probes.
2. The focus move runs in the same task as the write to `#status`, a polite live region. A screen reader can then announce the button in place of "Ready." or the failure message. Not tested.
3. A11 can go red on a correct page if the build ends between the A10 read and the blur. That is a false red, and the A11 comment does not say so as A8 to A10 do.
4. The blur means A11 tests focus that the test put on the body, not the drop from the disabled button.
5. The smoke comment at tests/smoke.spec.js:290-291 says the forced card press can leave focus off the body. A click on a disabled button does not move focus.
6. The comment at index.html:1289-1294 names Safari only, where the Scope names Safari and Firefox.
7. README.md:102 overstates (see the AC6 correction above).
8. The shown test `getClientRects().length > 0` is true for `visibility: hidden` and `inert` elements. `focus()` then does nothing, so no failure today.
9. The comment phrase at index.html:1436-1437 "on a step that is not on show" reads as if the button could sit on another step.
10. An older bug outside M106: an `abandonBoot()` during a build is overwritten by the build-failed status. The focus code handles that path.

Dispositions are set at the gate.
