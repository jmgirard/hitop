# M104: The builder's format cards are off during a build

- **Status:** in-progress
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

- [ ] AC1: While a build runs, the three format cards are disabled. A press on any card leaves the step's format state as it was at the build click. The format state is the text of `#downloadBtn`, the `.fmtpanel` that is not hidden, the card with `aria-current`, and which `.fmtpanel details` are open. A headless run opens the Word settings disclosure and starts a Word build. During the build, it first reads each card's `disabled` property as true and `#downloadBtn` as disabled. It then presses each of the three cards with `locator.click({ force: true })`. A second run opens the Qualtrics settings disclosure, starts a Qualtrics build, and presses the Word card the same way.
- [ ] AC2: When a build ends by saving a bundle or by failing, the three cards are enabled again. A headless run reads each card's `disabled` property after a saved Word build. A second run does the same on a served copy of the page whose generator call is replaced by an R `stop()`. After each ending, the run presses the Qualtrics card, and `#downloadBtn` names Qualtrics.
- [ ] AC3: No write to a card's `disabled` state exists in `index.html` outside `download()`. The domain is every line that `grep -n "disabled" index.html` lists. Each line is read, with a note on whether its selector matches a `[data-choose]` button.
- [ ] AC4: The step-two hint and the comment at `let building` in `index.html` say that the cards are off while a build runs. README.md §What the page shows says the same. Each other sentence about a card press agrees with the behavior that AC1 and AC2 verify. The domain is the paragraphs that `grep -n -i -E "card|data-choose|choos|format button" index.html README.md` matches, each read whole.
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
- [ ] T4: Add a plant to `tests/plants.mjs` that removes the cards' disable from `download()`. Run `npm run plants`. Make sure that each plant is red on its named assertion, and that the new plant fails A9 alone.
- [ ] T5: Run headless probes on the served branch. Do the AC1 presses, the AC2 reads after a saved and a failed build, and the AC3 grep. Write one work-log line for each criterion.
- [ ] T6: Add the clause to the step-two hint (`index.html:486`), to the comment at `let building` (`index.html:701`), and to README.md §What the page shows (line 84). Run the AC4 grep and read each matched paragraph. Rewrite each sentence that the T5 runs contradict. In the work log, record the matched paragraphs and a verdict for each.
- [ ] T7: Run `npm run smoke` locally. At review, open the hitop-builder PR and wait for its smoke run.

## Work log

- 2026-09-21: created by /milestone-plan from the format-card candidate row (M103 review finding D2). The criteria audit ran in full mode with an [O] reader and returned 9 findings. Six were fixed before the gate. AC1 drops `currentFormat`, which is private to the module script. AC1 also presses all three cards and reads the open disclosure. AC2 names the forced failure and is bounded to a save or a failure. AC3 records each hit's selector. AC4 has a wider grep. One went to the gate (AC4 wording), and two needed no change (AC5, the plant in T4).
- 2026-09-21: the plan gate chose to disable the cards during a build. It rejected a press applied after the build ends and a press ignored silently. A deferred press changes the page with no visible cause, and an ignored press looks like a broken page. A visitor report that the disabled cards are confusing falsifies this choice.
- 2026-09-21: the plan gate chose to state in the step-two hint and README.md that the cards are off during a build. It rejected a page that relies on the greyed look alone. AC4 now requires both sentences. A re-audit of the changed AC1 to AC4 by the same [O] reader returned 3 findings, all fixed before the commit: the second AC1 run opens the Qualtrics disclosure, AC1 reads `disabled` before the presses, and AC4 covers the `let building` comment.
- 2026-09-21: implement started. T1: both branches cut from `main` (hitop at a2b3b2ee, builder at 840dace), both already in sync with origin. No question gate, because the plan left nothing open. The `finally` sets the cards to `bootAbandoned`, as it does for the download button.
- 2026-09-21: T2: A9 added to `tests/smoke.spec.js` (forced press on the Qualtrics card after A8, read once against the button text read before the build). On the unfixed page the smoke run failed on A9 alone: the button read "Download the Qualtrics file (.zip bundle)" and the Word card had no `aria-current`. A8 passed in the same run, so the press landed during the build.
- 2026-09-21: T3: `download()` collects `[data-choose]` as `cards`, disables them after `building = true`, and sets them to `bootAbandoned` in `finally`. `refreshTally()` writes `.downloads button` only, so it cannot turn a card on. Smoke run on the fixed page: 1 passed.

## Decisions

## Review
