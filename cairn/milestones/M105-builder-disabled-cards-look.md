# M105: The builder's format cards look off while a build runs

- **Status:** in-progress
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** —
- **Resolves:** —
- **Surface tier:** user-facing — the public builder page that researchers use to download forms
- **Branch/PR:** `m105-builder-disabled-cards-look` in both hitop and jmgirard/hitop-builder

## Goal

While a build runs, the builder's format cards look disabled but the current card keeps its check.

## Scope

**In:** the following changes in jmgirard/hitop-builder.

- A `.formats button:disabled` rule in `index.html` paints the page's disabled pair and a dashed border on every card, the card with `aria-current` included. Today `.formats button` (`index.html:258`) comes after `button:disabled` (`index.html:150`) at equal specificity and puts back the card background and border colour. `.formats button[aria-current=true]` (`index.html:278`) puts back the blue fill and the solid border.
- The filled check (`::after` and `::before`) stays on the current card during a build, so the page still shows which format is building.
- A comment above the new rule says what it is for. README.md §What the page shows says that the cards look grey while a build runs.
- A smoke-test assertion reads the cards' look during the build, and a plant turns it red.

In this repo, the milestone changes tracking files only.

**Out:** Focus that drops to the body during a build → M106. The settings panels' controls stay on during a build (M103). The zip-reader gaps and the `tests/prose.mjs` writer gaps stay in their candidate rows. A theme control stays in its candidate row.

## Acceptance criteria

- [ ] AC1: While a build runs, each format card paints the page's disabled look, the current card included. A headless run reads each card during a build. It parks the mouse outside the cards and waits until `getAnimations()` on the card is empty. The card's computed `background-color` equals the computed `background-color` of `#downloadBtn` in the same read. The card's computed `border-top-style` is `dashed`, its `border-top-color` equals that of `#downloadBtn`, and its `box-shadow` is `none`. The computed `color` of its `.fmtname` and its `.fmtwhat` equals the computed `color` of a probe element styled `color: var(--disabled-fg)`. There are two runs: a Word build under `page.emulateMedia({ colorScheme: 'light' })`, and a Qualtrics build under `'dark'`.
- [ ] AC2: While a build runs, the card with `aria-current` still draws its filled check, and the other two cards do not. In the two AC1 runs, the current card's `::after` computed `background-color` equals the computed `background-color` of a probe element styled `background: var(--accent)`. Its `::before` computed `content` is not `none`. On each other card, the `::after` background equals that of a probe styled `background: var(--bg)`, and the `::before` `content` is `none`.
- [ ] AC3: Each text colour on a disabled card clears 4.5:1 against the computed background of that card. The two AC1 runs compute the WCAG 2 contrast ratio from computed colours for the `.fmtname` and the `.fmtwhat` of each card. That gives 12 cells: 3 cards by 2 texts by 2 schemes.
- [ ] AC4: After a build ends with a saved bundle, each card looks as it did before the build click. In the two AC1 runs, with the mouse parked and `getAnimations()` empty, the run reads each card's computed `background-color`, `border-top-style`, `border-top-color` and `box-shadow`, and the `color` of its `.fmtname` and its `.fmtwhat`. Each value equals the value read on that card before the build click.
- [ ] AC5: The comment above each card rule that the milestone adds or edits says what the rule is for. README.md §What the page shows says that the cards look grey while a build runs. The domain is the comment blocks in `index.html` and the paragraphs in README.md that `grep -n -i -E "disabled|gr[ae]y|dashed|check|card" index.html README.md` matches, each read whole. Each sentence about the look of a card agrees with what AC1 and AC2 verify.
- [ ] AC6: The builder's smoke suite (`npm run smoke`) passes locally and on the CI of the hitop-builder pull request.

## Coverage

- AC1 → T2, T3, T5
- AC2 → T3, T5
- AC3 → T3, T5
- AC4 → T3, T5
- AC5 → T6
- AC6 → T2, T4, T7

## Tasks

- [x] T1: Cut `m105-builder-disabled-cards-look` from the updated builder `main` and from hitop `main`.
- [x] T2: Write the test first. Add assertion A10 to `tests/smoke.spec.js`, in the in-build read that A9 takes. A10 asserts that each card's computed `border-top-style` is `dashed` and that its `background-color` equals `#downloadBtn`'s. Wait for the card transitions to end before the read. Add A10 to the assertion list in the file header. Run the test on the unfixed page and see it fail on A10.
- [x] T3: Add the `.formats button:disabled` rule after the `aria-current` rules in `index.html`. It sets the background, the border colour and style, the inset shadow, and the colour of `.fmtname` and `.fmtwhat`. It leaves the `::after` and `::before` check on the current card.
- [x] T4: Add plant (k) to `tests/plants.mjs`, which removes the new rule. Run `npm run plants`. Make sure that each plant is red on its named assertion, and that plant (k) fails A10 alone.
- [x] T5: Run headless probes on the served branch for AC1 to AC4, in both schemes. Write one work-log line for each criterion.
- [x] T6: Write the comment above the new rule and the README.md sentence. Run the AC5 grep and read each matched block. Rewrite each sentence that the T5 runs contradict. In the work log, record the matched blocks and a verdict for each.
- [ ] T7: Run `npm run smoke` locally. At review, open the hitop-builder PR and wait for its smoke run.

## Work log

- 2026-09-21: created by /milestone-plan from the disabled-controls candidate row (M104 review). The user chose this row with neither promotion trigger fired. The row's two parts split into M105 (card look) and M106 (focus), because the goal needed "and" and the parts ship apart. The criteria audit ran in full mode with an [O] reader and returned 14 findings across both files. Six were M105 findings, all fixed before the gate. They were raw-token colour compares, reads during the 0.15s transition, and the current card left out. The others were one format only, the hover state, and a narrow grep. Test placement went to the gate.
- 2026-09-21: the plan gate chose to grey all three cards and keep the filled check on the current card. It rejected keeping the current card blue, which looks pressable, and greying the check too, which leaves only the button text naming the format. A visitor report that the build's format is unclear during a build falsifies this choice.
- 2026-09-21: the plan gate chose a smoke assertion plus a plant over one-off probes only, because probes alone leave nothing red in CI when the look breaks. A10 rides the existing build, so it adds no webR boot. A smoke run that exceeds smoke.yml's budget falsifies this choice.
- 2026-09-21: a re-audit of the gate-changed criteria by the same [O] reader (full mode) returned 4 findings, 1 on M105, fixed after the plan commit. AC1 now also reads the border colour and a `box-shadow` of `none`, and AC4 compares `box-shadow` too.
- 2026-09-21: /milestone-implement started. T1 cut both branches from main at c7fbbb47 (hitop) and c56a9fb (builder). The question gate was skipped because the plan leaves no choice open.
- 2026-09-21: T2 added A10 to `tests/smoke.spec.js` (mouse parked at 0,0, a poll named A10 waits for `getAnimations({ subtree: true })` to empty, then reads each card's `border-top-style` and whether its background equals `#downloadBtn`'s). On the unfixed page the run failed on A10 alone: the Word card read solid, and no card's background matched the button.
- 2026-09-21: T3 added the `.formats button:disabled` rule and its `.fmtname`/`.fmtwhat` colour rule after the `aria-current` rules. The pseudo-element rules are untouched. `npm run smoke` passed. The comment waits for T6.
- 2026-09-21: T4 added plant (k), which removes the card rule. `npm run plants` exited 0. The unplanted copy passed and plant (k) failed on A10 alone. Plants (i) and (j) also failed A10 beside A8 and A9. Plant (i) fails A10 because it turns the download button on, and A10 uses that button as its reference. A10's comment now says so.
- 2026-09-21: T5 AC1: a headless probe (full Chromium, served branch) read all 3 cards during a Word build in light and a Qualtrics build in dark. Every card matched the disabled button's background and border colour, read dashed with no shadow, and both texts matched the `--disabled-fg` probe. On the unfixed page (c56a9fb) the same probe failed all 6 card reads.
- 2026-09-21: T5 AC2: in both runs, only the current card's `::after` matched the `--accent` probe and had `::before` content. The other two matched the `--bg` probe with `::before` content `none`.
- 2026-09-21: T5 AC3: all 12 cells cleared 4.5:1. Light reads 4.93:1 for every cell, and dark reads 5.42:1.
- 2026-09-21: T5 AC4: both bundles saved (Word 17014 bytes, Qualtrics 1370 bytes). After each build, all six values on all three cards equalled the pre-click read. AC2 to AC4 also pass on the unfixed page, which keeps the check and restores the look already.
- 2026-09-21: T6 wrote the comment above the new rule and a README.md step-2 sentence ("all three cards look grey with a dashed border, and the current card keeps the check in its corner"). The AC5 grep matched 100 lines.
- 2026-09-21: T6 verdicts, blocks with a card-look sentence. index.html tokens comment (4.5:1 for disabled pairs): agrees. `button:disabled` comment: agrees. Current-card comment ("is filled"): contradicted during a build, so it now says the disabled rule greys the fill and leaves the check. New rule comment: agrees. Hint text and `markFormatChoice` comment: agree. README step 2: agrees.
- 2026-09-21: T6 verdicts, other matched blocks. They describe behavior, focus, checkboxes or the boot-abandon form, with no card-look sentence. The README file table said "eight planted defects", stale at 10 before M105. It now names the `PLANTS` list and gives no count.

## Decisions

## Review
