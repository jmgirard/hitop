# M127: The module builder removes the link-builder link on a format change, announces the saved file through the status line, and repairs A16, plant (o) and the README pointer

- **Status:** review
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** —
- **Resolves:** —
- **Surface tier:** user-facing — the builder page at jmgirard.github.io/hitop-builder is a public site, and two items change what a visitor sees or hears
- **Branch/PR:** m127-online-card-loose-ends (this repo, tracking); jmgirard/hitop-builder branch m127-online-card-loose-ends, PR #23 https://github.com/jmgirard/hitop-builder/pull/23 (code)

## Goal

Close the six loose ends M126's review deferred on the builder's Online form card: two page fixes a visitor meets, and three checker repairs plus one README pointer beside them.

## Scope

**In:** jmgirard/hitop-builder only (`index.html`, `tests/smoke.spec.js`, `tests/plants.mjs`, `README.md`); this repo takes tracking only, as M126 did. (1) A press on another format's card after an online save removes the link and hides `#onlineNext`; the Online card pressed again keeps it. (2) The online save that puts the link in says so through `#status`, the page's one live region, with a text that begins with "Ready." so the card handler's prefix check (`index.html:1855`) still announces the chosen format; a removal that follows a tick resets the status. (3) A16 re-aimed at the start of a second online save begun with the first link present. (4) Plant (o) parses the JSON instead of reading one line. (5) README: the pointer to "Ticking every scale", the card press among the link's removal triggers, the status text. (6) The two markup comments that list the removal triggers (`index.html:668-674`, `849-852`).

**Out:** visibility reads of `#onlineNext` after the save and the tick, and a notice-and-hint swap assertion → declined at the plan gate (the fixes' own assertions cover the loose end); the focus move that follows the status write → the M106 focus-return candidate row; a second live region for the link → rejected at planning (the page's design names `#status` its only announced region, `index.html:685-689`); the M125 link-builder loose ends → their own candidate row.

## Acceptance criteria

- [x] AC1: After an online save, pressing the Word, Qualtrics or REDCap card leaves no "Continue to the link builder" anchor in the document and sets the `hidden` attribute on `#onlineNext`; pressing the Online card again leaves the link in place. Shown by smoke assertion A18, which presses the Word card after a save and reads the anchor count, the attribute and the status text, then saves again, presses the Online card and reads a count of one; and by two plants, one dropping the format-change removal and one dropping its guard so every card press removes the link, each failing the A18 read that sees it.
- [x] AC2: An online save that puts the link in ends with `#status` holding "Ready. The scoring file is saved. The link to the link builder is under the button."; the guarded save (a tick during the build), the boot and the bundle builds end with "Ready."; a tick that removes a link present at the tick returns the status to "Ready.", and a tick with no link present leaves the status alone. Shown by the smoke test's `saveOnline()` waiting for a status that begins with "Ready." and A19 reading the exact text after the first save, after the A17 save, and after the A15 untick, and by two plants, one dropping the saved text and one dropping the reset, each failing the read that sees it.
- [x] AC3: Smoke assertion A16 reads the anchor count and the download button's disabled state together, right after the button is pressed for a second online save with the first save's link present, and asserts zero anchors while the build runs; its message and the spec's enumeration say so; plant (u) fails it.
- [x] AC4: Plant (o) reverses the saved file's items by parsing the JSON text and serialising it again, reading no layout of `write_module()`'s output; (o) fails A13 and A15.
- [x] AC5: README.md's "The online form" says the every-scale name rests on the probe "Ticking every scale" describes and links that section; its sentence on when the link is removed names a press on another format's card; and it describes the status line's saved-file text. `node tests/prose.mjs --compare` against a baseline written from main lists only the `md:The online form` passage and entries of the status group (the group pairs by position, so the inserted status string shifts the later ones); the reworded sentences are verified by reading the README and the `--text` output, since the compare reads fact tokens only.
- [x] AC6: The builder's checks are clean: `npm run smoke` green locally, the plant matrix green (the unplanted copy passes, every plant fails, every enumerated assertion is failed by at least one plant), `npm run prose` runs, and the builder PR's CI is green.

## Coverage

- AC1 → T1, T2, T3
- AC2 → T1, T2, T3
- AC3 → T2, T3
- AC4 → T3
- AC5 → T4
- AC6 → T5

## Tasks

- [x] T1: `index.html`: `setFormat()` (`:948`) calls `removeLinkBuilder()` only when `format !== currentFormat`, before it stores the new format; the online branch (`:1458-1461`) writes the saved-file status inside the `selected().join() === chosen.join()` guard and "Ready." otherwise; `removeLinkBuilder()` (`:853`) resets `#status` to "Ready." only when it removed an anchor and the status holds the saved-file text (a reset on every tick would overwrite "Building the DOCX file…" at the A8 tick and fail A9); the comments at `:668-674` and `:849-852` name the card press among the removal triggers. Write the baseline first: `node tests/prose.mjs --json <scratch>/baseline.json` on main.
- [x] T2: `tests/smoke.spec.js`: `saveOnline()` (`:161`) waits for `/^Ready\./` and returns the status text; A19 reads the saved-file text after the first save; A16 (`:381`, `:511`) moves to a second online save pressed right after the first with the link present, reading the count and the button's disabled state in one `evaluate` right after the click (as A17 does at `:307-313`), then waiting for the saved-file text; the A17 wait (`:319`) stays "Ready."; A15's `afterTick` read (`:296`) adds the status text; A18 presses the Word card on step 2 after a save, reads the count, `#onlineNext`'s `hidden` attribute and the status, saves again, presses Online and reads a count of one; the header enumeration (`:17-26`) lists A18 and A19 and restates A16. Per LESSONS M119, show each absence selector matches somewhere first.
- [x] T3: `tests/plants.mjs`: (x) drops the format-change removal (fails A18's Word read); (x2) drops the `format !== currentFormat` guard so every press removes the link (fails A18's Online read); (y) replaces the saved-file status with "Ready." (fails A19); (z) drops the reset in `removeLinkBuilder()` (fails A15); (o) (`:156-163`) parses the JSON, reverses `items`, serialises; (u) (`:198-205`) keeps failing A16 at its new site. Per LESSONS M118, `git add` the fix before planting.
- [x] T4: `README.md` "The online form" (`:223-258`): the pointer to "Ticking every scale", the card press in the removal sentence, the status text; `node tests/prose.mjs --compare <scratch>/baseline.json` names only the changed passages.
- [x] T5: `npm run smoke`, `npm run plants`, `npm run prose`; the builder PR on a `m127-online-card-loose-ends` branch, CI green; Jeff merges it from his terminal (LESSONS M116); this repo's PR carries the tracking.

## Work log

- 2026-09-25: created by /milestone-plan; promoted from the candidate row "Six online-card loose ends" (lineage M126 review O2 to O6, O10).
- 2026-09-25: criteria audit ran in full mode (user-facing tier), two passes by a fresh-context reader: pass one returned nine findings (A16 unsatisfiable once the card press removes the link; the status text stale after a tick and breaking the card handler's "Ready." prefix check; the guarded save leaving no link; a visibility read vacuous on step 1; `npm run prose` proving nothing about README.md; stale README and comment triggers; the Online re-press unspecified; one card probed for three; one reversal plant), six fixed as one-clear-answer, three posed at the gate; pass two, over the gate-settled wording, returned five (a hard wait in `saveOnline()` stopping the run before A19; `--compare` pairing by position and reading fact tokens only; the Online-keeps-it half unprobed and its order ambiguous; a late A16 read failing a correct page; a reset on every tick breaking A9), all fixed as one-clear-answer before writing.
- 2026-09-25: plan gate chose a status-line announcement over a second live region on `#onlineNext` because the page's design names `#status` its only announced region; falsified by a screen reader that does not speak the status text after the save.
- 2026-09-25: plan gate chose keeping the link on an Online re-press over removing it on any card press because a valid link should not need a second save; falsified by a visitor report of a stale link after a re-press.
- 2026-09-25: plan gate chose re-aiming A16 at a second online save over deleting A16 and plant (u) because the build-start removal would otherwise go untested; falsified by the re-aimed read passing on a page with that removal dropped.
- 2026-09-25: plan gate chose assertions only for the two fixes over full visibility coverage because the fixes' reads close the loose end at less matrix time; falsified by a hidden-attribute regression after the save or the tick that A18 does not see.
- 2026-09-25: plan chose one card pressed in A18 for three because all three share `setFormat()`; falsified by a per-card handler diverging from `setFormat()`.
- 2026-09-25: implement started by /milestone-implement. No question gate: the plan left nothing open. Branches cut from the pushed default branch in both repos. Prose baseline written from main before any edit.
- 2026-09-25: T1 done, hitop-builder commit 1541425. Minor amendment to T1's reset condition: `removeLinkBuilder()` resets the status whenever it removed an anchor, with no read of the status text, because the ticks made during a build (A8, A17) find no anchor and so leave "Building…" alone, and a link is present only under a status that begins with "Ready.". The prose compare after the page edit names status#2 and status#3 (the inserted saved-file string shifts the group) and status#6 (new); the reset's "Ready." is a pinned string and adds no passage.
- 2026-09-25: T2 done, hitop-builder commit 85c0f74. `npm run smoke` green locally in 11 s (webR cached), 19 assertions enumerated. `readLinkState()` reads the anchor count, the paragraph's hidden attribute and the status in one call; A16 and A18 read it before the press too, which shows the count finds the anchor (LESSONS M119). A16's wait takes any "Ready." so plant (y) reaches A19 rather than stopping at A16.
- 2026-09-25: T4 done, hitop-builder commit 2b94440. `--compare` against the baseline names status#2, status#3, status#6 and `md:The online form#1` (gained `id=ticking-every-scale`), nothing else. The reworded sentences read in the README and in the `--text` output. The simple-english lint counts on README.md are the same before and after the edit (five long sentences, one trailing condition, all pre-existing).
- 2026-09-25: claim audit: 45 claims read, 7 corrected — hitop-builder README.md, index.html, tests/smoke.spec.js, tests/plants.mjs (a fresh-context [O] reader over `git diff main` in that repo; this repo's diff outside `cairn/` adds no lines, the prose all lives there). Corrected: the README's status-after-removal sentence (a card press names the chosen format, a tick returns "Ready."), the `removeLinkBuilder()` comment's caller list (showLinkBuilder() added), the `saveOnline()` comment on which read catches a wrong status, the `readLinkState()` comment on how the two anchor reads match, two smoke comments naming "the second save" where a later save is meant, and plant (w)'s comment naming the first save's link where the A16 save's is there. The re-read found one clause over-reaching ("as every card press does", false after a failed build) and it was trimmed. One stale comment outside the added lines (the mid-tick save's link "the one the Word build starts with") was corrected in the same commit. hitop-builder commit b24118f (plant (w)'s comment goes with T3's commit).
- 2026-09-25: T3 done, hitop-builder commit 9dbdd99. Plant matrix: the unplanted copy passed, 27 of 27 plants red, every one of the 19 assertions failed by at least one plant. Per plant as planned: (o) A13, A15; (u) A16; (v) A17; (w) A15, A19; (x) A18; (x2) A18; (y) A18, A19; (z) A19. The matrix ran on scratch copies, so no restore was needed (LESSONS M118 checked). The run was started by an import in a check script rather than `npm run plants`, on the same files; nothing else differs.
- 2026-09-25: T5 done: branch pushed, hitop-builder PR #23 opened, its smoke job green in 57 s. `npm run prose` runs (18 writer sites, 166 passages). This repo changes tracking only, so the r-package verify slot has no code to run against. Status set to review.
- 2026-09-25: review: every criterion evidenced fresh and ticked; gate green; three lenses returned 13 findings, 11 fixed now in hitop-builder, one noted, one rejected (Review section). Correction to T3's text: plant (z) fails A19, not A15, as the matrix shows and the plant's comment says.
- 2026-09-25: step-7 approval: m127-online-card-loose-ends approved for merge

## Decisions

## Review

Evidence gathered 2026-09-25 by /milestone-review on hitop-builder commit 9dbdd99 (PR #23 head), with neither default branch moved since the branches were cut.

- AC1: `npm run smoke` green (A18 in the run, 19 assertions enumerated). Matrix: plant (x) (removal in `setFormat()` dropped) failed A18; plant (x2) (its guard dropped) failed A18; the unplanted copy passed. A18 reads the anchor count, `#onlineNext`'s hidden attribute and the status before and after the Word card press and after the Online re-press.
- AC2: A19 in the green smoke run reads the saved-file text after the first save, "Ready." after the A17 save, "Ready." after the untick. `saveOnline()` waits on `/^Ready\./`. Matrix: plant (y) (saved text replaced by "Ready.") failed A18, A19; plant (z) (reset dropped) failed A19. A scratch Playwright probe (not committed) read "Ready." at boot, "Ready." after a tick with no link present, "Ready. Word form chosen." after the card press, and "Ready." after a Word bundle build. The A8 tick during the Word build leaves "Building…" standing (A9 and A10 read it in the green run).
- AC3: A16 at `tests/smoke.spec.js:330-337` reads the anchor count and the button's disabled state in one `evaluate` right after the click that starts a second online save with the first link present, expecting zero anchors and the button off; its message and the header enumeration (line 25) say so. Matrix: plant (u) failed A16 alone.
- AC4: plant (o) (`tests/plants.mjs`) replaces the read of the file with `JSON.parse`, `items.reverse()`, `JSON.stringify`; no regular expression over the text remains. Matrix: (o) failed A13, A15.
- AC5: README "The online form" read in full: it says the every-scale name rests on the probe the linked "Ticking every scale" section describes, names a press on another format's card among the removal triggers, and quotes the status text. `node tests/prose.mjs --compare` against a baseline written with `--ref main` lists status#2, status#3 (shifted), status#6 (new) and `md:The online form#1` (gained `id=ticking-every-scale`), nothing else; the `--text` output carries the saved-file status passage.
- AC6: smoke green locally (10.2 s); matrix OK, unplanted passed, 27 of 27 plants red, every enumerated assertion covered; `npm run prose` runs (18 writer sites, 166 passages); PR #23's smoke check passed (57 s) on head 9dbdd99.

Consistency gate: `cairn_validate.py` all checks passed, 24 advisory warnings, none new. No principle changed, so no impact report. r-package slot: `devtools::document()` no diff; README.Rmd untouched; `pkgdown::check_pkgdown()` not applicable, no export changed; NEWS.md: no entry, this repo ships no user-visible change (the builder is a separate site with its own README); `devtools::check()` 0 errors, 0 warnings, 0 notes in 4m 7s.

Independent review, three lenses over the hitop-builder diff. Prior-review lens: no prior-review evidence regressed, the PR-comment probe returned an empty list, 0 findings. Blame lens: 2 findings. Diff-bug lens: 11 findings. Triage at the 2026-09-25 gate, the recommended triage accepted:

- F1 (diff-bug, first): the A17 wait on the exact "Ready." made A19's mid-tick read unfalsifiable and plant (v) fail only by a 240 s timeout. Fix now: the wait takes `/^Ready\./`, (v) then fails A17's anchor read and A19.
- F2: with a prefix wait, the status before the click already matches, so `saveOnline()` leaned on download() writing "Building…" synchronously. Fix now: the save's download event is awaited before the status wait, in `saveOnline()` and the A16 save.
- F3: the "Ticking every scale" section, now pointed at, counted "the other two formats" and never named the online file. Fix now in part: "the other formats". The file-name half rejected: a code token there would add the passage to the compare list AC5 pins, and the online form section already names the file.
- F4: A18's before-press reads assert `hidden: false`, a visibility read after a save, which Scope "Out" declined. Noted, kept: AC1 requires the attribute read after the press, and the before-read shows the selector finds the paragraph (LESSONS M119).
- F5: T3's text says plant (z) fails A15; the matrix shows A19. Fix now as a work-log correction; the task line stands as written at planning.
- F6: A16's comment said download() takes the link out "before anything else". Fix now: "synchronously, before its first await, right after it turns the controls off".
- F7: the README omitted that an Online re-press writes "Ready. Online form chosen." over the saved-file status. Fix now.
- F8: the README's "Verified 2026-09-25" sentence followed the new sentences and read as covering them. Fix now: moved up beside the link-builder sentences; the new paragraph says the smoke test checks it.
- F9: A16's inline evaluate duplicated `readLinkState()`'s anchor filter. Fix now: `readLinkState()` returns the button state too and serves both A16 reads; A18's expectations carry `building: false`.
- F10: plant (o)'s comment said "on every save". Fix now: "on every online save".
- F11: the spec header said the test reads no page internals. Fix now: "reads only the page's document, no script state".
- F12 (blame, first): the read that no anchor remains after a completed Word build was deleted with nothing replacing it. Rejected: the plan gate chose re-aiming A16 over that read; `showLinkBuilder()` runs only in the online branch and A18 shows the card press removes the link.
- F13 (blame): the comment before the build-start `removeLinkBuilder()` did not say the card-press removal now does the work for every format but online. Fix now: a clause added.

Re-verification after the fixes: recorded below.
