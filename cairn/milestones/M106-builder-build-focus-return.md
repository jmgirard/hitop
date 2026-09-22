# M106: Keyboard focus comes back when a builder build ends

- **Status:** in-progress
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

- [ ] AC1: If focus is on the body when a build ends with a saved bundle, and the control that had focus at the build click is enabled and shown, focus returns to that control. Two headless runs test this. In the first run, `#downloadBtn` takes focus from the Playwright `click()`. In the second run, `.focus()` puts focus on the Qualtrics card while Word is the current format. The Word build then starts with `dispatchEvent('click')` on `#downloadBtn`, which moves no focus. During each build, the run reads `document.activeElement` as the body. After the save, the run polls `document.activeElement` for up to 5 seconds and reads it as the control that had focus at the click.
- [ ] AC2: The same holds when the build fails. The two AC1 runs are repeated on a served copy of the page, where an R `stop()` replaces the generator call. The run polls after the failure status appears.
- [ ] AC3: If the control that had focus at the click is disabled or hidden when the build ends, focus stays on the body. There are two runs, each starting a Word build with the Playwright `click()` on `#downloadBtn`. In the first run, the probe unticks every ticked scale with `dispatchEvent('click')` during the build. Step one and its checkboxes are hidden at that time. So `#downloadBtn` is disabled at the end. In the second run, the probe presses the step bar's first-step button, which hides step two, and then calls `blur()` on the focused heading. After the build ends, each run polls for 5 seconds and reads `document.activeElement` as the body throughout.
- [ ] AC4: If focus is on an element other than the body when a build ends, focus stays there. There are two runs. In the first run, the probe opens the Word settings disclosure during the build and focuses `#shuffle`. In the second run, the probe presses the step bar's first-step button during the build, which focuses that step's heading. After the build ends, each run polls for 5 seconds and reads `document.activeElement` as `#shuffle` or as that heading throughout.
- [ ] AC5: The return of focus does not scroll the page. In both AC1 runs, the viewport is set to 1280 by 400, and the probe calls `scrollIntoView()` on `#logSection` during the build. It then confirms that the control that had focus at the click lies outside the viewport, and it reads `window.scrollY`. After focus returns, `window.scrollY` equals that value.
- [ ] AC6: The comment in `download()` and README.md §What the page shows say that focus comes back to the control that had it when a build ends. The domain is the comment blocks in `index.html` and the paragraphs in README.md that `grep -n -i -E "focus|keyboard|tab key" index.html README.md` matches, each read whole. Each sentence about focus during or after a build agrees with what AC1 to AC5 verify.
- [ ] AC7: The builder's smoke suite (`npm run smoke`) passes locally and on the CI of the hitop-builder pull request.

## Coverage

- AC1 → T2, T3, T5
- AC2 → T3, T5
- AC3 → T3, T5
- AC4 → T3, T5
- AC5 → T3, T5
- AC6 → T6
- AC7 → T2, T4, T7

## Tasks

- [x] T1: Cut `m106-builder-build-focus-return` from the updated builder `main` and from hitop `main`.
- [x] T2: Write the test first. On the unfixed page, confirm in a probe that focus is on the body during a build after the Playwright `click()`. If it is not, stop and raise it at an amendment gate, because the assertion below cannot then fail. The smoke build presses the step bar, which leaves focus on a step heading (`index.html:912`). So after the A9 and A10 reads and before the download wait, the test calls `blur()` on the focused element, and a comment says that this stands in for a focus lost to the disabled button. Add assertion A11 to `tests/smoke.spec.js` after the save. A11 polls `document.activeElement` and asserts that it is `#downloadBtn`. Add A11 to the assertion list in the file header. Run the test on the unfixed page and see it fail on A11.
- [x] T3: In `download()`, record `document.activeElement` before the controls go off. After `finally` turns them back on and `refreshTally()` runs, return focus as the Scope says. Write the comment that says why.
- [ ] T4: Add plant (l) to `tests/plants.mjs`, which removes the focus return. Run `npm run plants`. Make sure that each plant is red on its named assertion, and that plant (l) fails A11 alone.
- [ ] T5: Run headless probes on the served branch for AC1 to AC5, with the forced-failure copy for AC2. Write one work-log line for each criterion.
- [ ] T6: Write the README.md sentence. Run the AC6 grep and read each matched block. Rewrite each sentence that the T5 runs contradict. In the work log, record the matched blocks and a verdict for each.
- [ ] T7: Run `npm run smoke` locally. At review, open the hitop-builder PR and wait for its smoke run.

## Work log

- 2026-09-21: created by /milestone-plan from the disabled-controls candidate row (M104 review), split from M105 (see its work log). The criteria audit ran in full mode with an [O] reader. Six findings were M106 findings, and five were fixed before the gate. They were a card focus on the current card only, polls for a deferred focus, `#shuffle` inside a closed disclosure, a scroll probe that passes on any code, and a narrow grep. The sixth went to the gate: a control that is off or hidden at the build's end.
- 2026-09-21: the plan gate chose to return focus only to a control that is enabled and shown. It rejected a return to `#downloadBtn` in every case, which moves focus to a control the visitor never used. A visitor report that focus lands in the wrong place after a build falsifies this choice.
- 2026-09-21: the plan gate chose a focus return with no scroll. It rejected a plain `focus()`, which moves the page away from the log that a visitor is reading. A keyboard visitor report that focus is lost from view after a build falsifies this choice.
- 2026-09-21: the plan gate chose a smoke assertion plus a plant over one-off probes only, for the reason in M105's work log.
- 2026-09-21: a re-audit of the gate-changed criteria by the same [O] reader (full mode) returned 4 findings, 3 on M106, all fixed after the plan commit. A11 was unable to pass, because the smoke build's step-bar presses leave focus on a heading, so T2 now blurs it first. AC5 now sets a 400 px viewport. AC3 now says that the unticked checkboxes are hidden.
- 2026-09-21: implement started. Both branches cut from their updated `main` (builder at `6ac767e`). The question gate was skipped: the one open choice, how to test that a control is shown, uses `getClientRects().length > 0`.
- 2026-09-21: T2 done. A probe on the unfixed page read focus on the body during a build after the Playwright `click()` and after it ended. A11 and the blur are in `tests/smoke.spec.js`, and `npm run smoke` on the unfixed page failed on A11 alone.
- 2026-09-21: T3 done. `download()` keeps `document.activeElement` before the controls go off. After `refreshTally()` in the `finally`, it checks the focus. If focus is on the body and the kept control is connected, on, and has client rects, it focuses that control with `preventScroll`. `npm run smoke` passed.

## Decisions

## Review
