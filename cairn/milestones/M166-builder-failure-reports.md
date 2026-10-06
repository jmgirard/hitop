# M166: Module Builder failure reports

- **Status:** in-progress
- **Priority:** high
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** GP3
- **Resolves:** —
- **Surface tier:** user-facing — the failure status and scroll position of the Module Builder page
- **Branch/PR:** m166-builder-failure-reports, companion: /Users/jmgirard/github/hitop-builder m166-builder-failure-reports

## Goal

A Module Builder failure shows the researcher a true status, with the opened log in view.

## Scope

**In:** Two changes in hitop-builder `index.html`. `showFailure()` brings the opened "Technical details" section into view. The top-level `main().catch` (`index.html:1940`) stops reporting "R did not start." for a throw after R starts. The milestone adds smoke steps for both, keeps the plant matrix whole, and updates the README sentences that state the failure behavior. This repo gets tracking only.

**Out:**
- The rest of the "Module Builder test reach" candidate goes to M167. This commit plans M167, which depends on M166.
- The other failure statuses (the `abandonBoot()` calls at `index.html:1705` to `1824`) keep their wording.
- Firefox and Safari scroll behavior stays unchecked, as in DESIGN Known issue 14.

## Acceptance criteria

- [x] AC1: After a build fails, the "Technical details" section is open and its `<summary>` element's bounding box lies wholly inside the viewport. This holds for a build that started while the summary's box lay outside the viewport. The check is a smoke step on the forced `URL.createObjectURL` throw that A30 uses. The step sets a viewport size or scroll position that puts the summary outside the viewport, and asserts that it is outside before the failure.
- [x] AC2: The page's start-up throws after `webR.init()` resolves. The status line then reads "R started, but the page did not finish setting up.", followed by the existing pointer to "Technical details". The section is open, and `#controls` is hidden. The check is a smoke step that forces such a throw. A refused `webr.mjs` still gets a status that begins "R did not load."
- [x] AC3: In hitop-builder `README.md`, the "Technical details" passage and the `tests/smoke.spec.js` file-table row state the behavior that the AC1 and AC2 smoke steps assert. `npm run prose` exits 0.
- [ ] AC4: The smoke test passes locally and on the CI of the hitop-builder pull request.

## Coverage

- AC1 → T1, T2
- AC2 → T1, T2
- AC3 → T3
- AC4 → T2, T3

## Tasks

- [x] T1: In `index.html`, make `showFailure()` (`index.html:817`) scroll the "Technical details" summary into view with no smooth scroll. Send the top-level catch through `abandonBoot()` with the AC2 status. Update the comments that name "R did not start." as the catch-all.
- [x] T2: In `tests/smoke.spec.js`, add the AC1 step to the A30 build-failure path. Add a new test that forces a throw after R starts. Throw from one site outside every `try`, never from `el()` as a whole, because `abandonBoot()` and `showFailure()` read `status`, `controls` and `techDetails`. Prefer a site before the package install, so the test skips the install. Update the time budget comment in `playwright.config.js` for the extra boot. Give each step a header line. Add one plant per new step to `tests/plants.mjs`: one removes the scroll call, one restores the old catch message. Run `npm run smoke` and `npm run plants` with nothing else running on the machine.
- [x] T3: Update the "Technical details" passage (`README.md:125` to `131`) and the smoke-test file row. Run `npm run prose`. The companion PR opens at review.

## Work log

- 2026-10-06: created by /milestone-plan, together with M167, from the "[high] Module Builder test reach" candidate row. The row's two page-behavior items come here. Its test items go to M167.
- 2026-10-06: question set: what to plan — the Module Builder test reach row. Run on into implement after the plan — yes.
- 2026-10-06: plan split the row into M166 (page, user-facing) and M167 (tests, internal), because its ten items need more than seven criteria. M167 depends on M166, because both edit `smoke.spec.js` and `plants.mjs`.
- 2026-10-06: plan chose to scroll the summary into view over a focus move to it. The status line is the announcing region, and a focus move interrupts a keyboard user. Falsified by a screen-reader report that the failure goes unheard.
- 2026-10-06: criteria audit (full mode, fresh Opus reader) returned 7 findings, all fixed. AC1 had no off-screen start, so an unchanged page passed it. A precondition now puts the summary outside the viewport. AC2's sample throw broke the failure path itself: T2 now names a safe site and the budget comment. The hidden-controls check cannot tell the old catch from the new one: it stays as a fact, not as evidence of the change. "A smoke step asserts", "A22 is unchanged" and "written against an observed run" bound test or record properties: AC1 to AC3 now state page and README facts. `npm run prose` checks no README claim, so AC3's truth rests on the review read.
- 2026-10-06: implement started. Branches cut in hitop and hitop-builder. The untracked `devel/hitopdat_*` files in hitop are not this milestone's and stay unstaged. The R verify slot (`devtools::test()`) does not apply, because no R code changes. The companion's smoke run is the verify step.
- 2026-10-06: T1 done (hitop-builder f40e761). `showFailure()` calls `scrollIntoView({ block: 'nearest' })` on the whole section, so a section that fits shows its log too, and one taller than the window shows its summary. `abandonBoot()` now hides the controls before it shows the failure, so the scroll measures the page without them. The top-level catch goes through `abandonBoot()`, and the latch comment counts nine sites. `npm run prose` exits 0 with 9 `abandonBoot` passages.
- 2026-10-06: T2 code done (hitop-builder 10aead5). A31 scrolls the download button to the window's foot with `block: 'end'`, reads the summary below the window, then reads it wholly inside after the A30 failure. A32 is a new second test. An init script makes the `textContent` setter throw only for the status write "Downloading the hitop package…", so R boots and no package downloads. Plants au (no scroll) and av (old catch) added. The `playwright.config.js` budget comment names the two short tests. Local smoke: 3 passed (35.8s).
- 2026-10-06: the first plant run stopped at plant ad, whose target text the new scroll line changed. Plant ad now removes the `open = true` line before the scroll. Before it stopped, the unplanted copy passed and plants a to ac each went red. During that run a `git stash` of the companion's uncommitted test files lasted about one second, so one run in that window can have read the old spec. The matrix reruns whole on ec57570.
- 2026-10-06: T3 done (hitop-builder ec57570). README's "Technical details" passage, its developer note on `showFailure()` and the smoke-test file row state the A31 and A32 behavior. `npm run prose` exits 0. README lint rose from 1 to 5 long or trailing sentences, so the review read checks those sentences.
- 2026-10-06: claim audit: 32 claims read, 7 corrected — index.html, tests/smoke.spec.js, playwright.config.js, README.md
- 2026-10-06: T2 done. `npm run plants` ran alone on hitop-builder ec57570. The unplanted copy passed, all 49 plants went red, and all 32 enumerated assertions were covered. Plant au failed A31 alone, plant av failed A32 alone, and plant ad failed A22, A30 and A32. Status set to review.
- 2026-10-06: review return 1: diff-bug #1, a load failure now scrolls, which can push the status line above a short window. Fix-now work rides the return: diff-bug #2, #3, #4, #6, #7, #9, #10, #11 and prior-review #1 to #3 (Review section).

## Decisions

## Review

- AC1: met. Fresh local smoke on hitop-builder ec57570: 3 passed (32.6s). A31 passed inside the first test. It scrolled `#downloadBtn` to the window's foot, read the summary's top at or below the window height, and after the forced `URL.createObjectURL` failure read the summary's box wholly inside the window. A30 passed on the same failure, with the section open. The plant run on ec57570 showed plant au (scroll removed) failing A31 alone.
- AC2: met. The same run passed A32 in the second test: the status began "R started, but the page did not finish setting up." and named "Technical details", the section was open and rendered, and `#controls` was hidden. A22 passed in the third test on a status that begins "R did not load.". Plant av (old catch) failed A32 alone.
- AC3: met. Read of `git diff main -- README.md` on hitop-builder: the "Technical details" passage says a build that fails with the section below the window scrolls it into view, and gives the "R started, but…" status. The developer note on `showFailure()` and the smoke-test row state the same two behaviors and the new second test. `npm run prose` exited 0 (21 writer sites, 184 passages, 95 body text nodes, no retired names).
- Gate (pass 1): `cairn_validate` exit 1 on `mirror agreement` (M163: ROADMAP=blocked, file=planned). The cause is the `main` commit ffe29b46, not this branch. The fix goes to `main` as a docs-only commit. `devtools::document()` changed no file. `devtools::check()` is still running.
- spawned: diff-bug, blame-history, prior-review
- diff-bug #1: every load failure now scrolls through `abandonBoot()`, and on a short window `nearest` can push the status line above the window — fix now (floor return: a user-visible regression outside AC1's build-failure case). The scroll moves to the build-failure path alone.
- diff-bug #2: the top-level catch overwrites a specific message after a latched failure such as the lost connection — fix now (the catch keeps the first message when `bootAbandoned` is set).
- diff-bug #3: plant av's "and no latch" claims more than A32 checks — fix now (description narrowed).
- diff-bug #4: A31's off-screen start depends on page height at 1280×720 — fix now (A31 sets a shorter viewport first).
- diff-bug #5: after a failed build the focused download button can sit off-screen — follow-up, to the "Module Builder page requests" candidate row.
- diff-bug #6: the catch comment says every throw that reaches it comes after R started, but three lines before the race and a throw inside `abandonBoot()` reach it too — fix now (wording narrowed).
- diff-bug #7: README "an error that has no message of its own" reads as an empty message — fix now.
- diff-bug #8: README visitor passage omits the load-failure scroll — fix now by #1, after which no load failure scrolls.
- diff-bug #9: README line 442 not wrapped — fix now.
- diff-bug #10: `playwright.config.js` timing reads as if the A32 test downloads nothing — fix now (it downloads R).
- diff-bug #11: plant ad's comment names A22 alone, and it fails A22, A30 and A32 — fix now.
- diff-bug #12: textContent override reach — noted, no defect.
- diff-bug #13: `abandonBoot()` reorder — noted, no defect.
- blame-history #1: the top-level catch now latches, and the M076 comment chose not to — reject, planned change (T1 routes the catch through `abandonBoot()`). After the controls show, only `showStep(0)` and `status('Ready.')` run, and neither throws in practice.
- blame-history #2: catch overwrites a latched message — fix now, as diff-bug #2.
- blame-history #3: "comes after R started" not strictly true — fix now, as diff-bug #6.
- blame-history #4: message shown after the controls hide, so a throw while hiding loses it — reject, planned change (the scroll measures the page without the controls). Fixing #1 removes the scroll from this path, so the order goes back to message first.
- blame-history #5: budget comment loose on the A32 boot — fix now, as diff-bug #10.
- blame-history #6: plant ad now matches across two lines — reject, false as a defect: `replaceOnce` stops the matrix loudly if the text moves.
- blame-history #7: README wrap — fix now, as diff-bug #9.
- blame-history #8: focused button off-screen — follow-up, as diff-bug #5.
- prior-review #1: `index.html:1650` and `README.md:115` still say the page does not scroll at a build's end, false for a failed build — fix now.
- prior-review #2: README line 442 not wrapped, and short lines in the catch comment — fix now.
- prior-review #3: the "or only its top" taller-window case rests on a spec reading with no probe — fix now (README and comment drop the clause).
