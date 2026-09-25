# M127: The module builder removes the link-builder link on a format change, announces the saved file through the status line, and repairs A16, plant (o) and the README pointer

- **Status:** planned
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** —
- **Resolves:** —
- **Surface tier:** user-facing — the builder page at jmgirard.github.io/hitop-builder is a public site, and two items change what a visitor sees or hears
- **Branch/PR:** —

## Goal

Close the six loose ends M126's review deferred on the builder's Online form card: two page fixes a visitor meets, and three checker repairs plus one README pointer beside them.

## Scope

**In:** jmgirard/hitop-builder only (`index.html`, `tests/smoke.spec.js`, `tests/plants.mjs`, `README.md`); this repo takes tracking only, as M126 did. (1) A press on another format's card after an online save removes the link and hides `#onlineNext`; the Online card pressed again keeps it. (2) The online save that puts the link in says so through `#status`, the page's one live region, with a text that begins with "Ready." so the card handler's prefix check (`index.html:1855`) still announces the chosen format; a removal that follows a tick resets the status. (3) A16 re-aimed at the start of a second online save begun with the first link present. (4) Plant (o) parses the JSON instead of reading one line. (5) README: the pointer to "Ticking every scale", the card press among the link's removal triggers, the status text. (6) The two markup comments that list the removal triggers (`index.html:668-674`, `849-852`).

**Out:** visibility reads of `#onlineNext` after the save and the tick, and a notice-and-hint swap assertion → declined at the plan gate (the fixes' own assertions cover the loose end); the focus move that follows the status write → the M106 focus-return candidate row; a second live region for the link → rejected at planning (the page's design names `#status` its only announced region, `index.html:685-689`); the M125 link-builder loose ends → their own candidate row.

## Acceptance criteria

- [ ] AC1: After an online save, pressing the Word, Qualtrics or REDCap card leaves no "Continue to the link builder" anchor in the document and sets the `hidden` attribute on `#onlineNext`; pressing the Online card again leaves the link in place. Shown by smoke assertion A18, which presses the Word card after a save and reads the anchor count, the attribute and the status text, then saves again, presses the Online card and reads a count of one; and by two plants, one dropping the format-change removal and one dropping its guard so every card press removes the link, each failing the A18 read that sees it.
- [ ] AC2: An online save that puts the link in ends with `#status` holding "Ready. The scoring file is saved. The link to the link builder is under the button."; the guarded save (a tick during the build), the boot and the bundle builds end with "Ready."; a tick that removes a link present at the tick returns the status to "Ready.", and a tick with no link present leaves the status alone. Shown by the smoke test's `saveOnline()` waiting for a status that begins with "Ready." and A19 reading the exact text after the first save, after the A17 save, and after the A15 untick, and by two plants, one dropping the saved text and one dropping the reset, each failing the read that sees it.
- [ ] AC3: Smoke assertion A16 reads the anchor count and the download button's disabled state together, right after the button is pressed for a second online save with the first save's link present, and asserts zero anchors while the build runs; its message and the spec's enumeration say so; plant (u) fails it.
- [ ] AC4: Plant (o) reverses the saved file's items by parsing the JSON text and serialising it again, reading no layout of `write_module()`'s output; (o) fails A13 and A15.
- [ ] AC5: README.md's "The online form" says the every-scale name rests on the probe "Ticking every scale" describes and links that section; its sentence on when the link is removed names a press on another format's card; and it describes the status line's saved-file text. `node tests/prose.mjs --compare` against a baseline written from main lists only the `md:The online form` passage and entries of the status group (the group pairs by position, so the inserted status string shifts the later ones); the reworded sentences are verified by reading the README and the `--text` output, since the compare reads fact tokens only.
- [ ] AC6: The builder's checks are clean: `npm run smoke` green locally, the plant matrix green (the unplanted copy passes, every plant fails, every enumerated assertion is failed by at least one plant), `npm run prose` runs, and the builder PR's CI is green.

## Coverage

- AC1 → T1, T2, T3
- AC2 → T1, T2, T3
- AC3 → T2, T3
- AC4 → T3
- AC5 → T4
- AC6 → T5

## Tasks

- [ ] T1: `index.html`: `setFormat()` (`:948`) calls `removeLinkBuilder()` only when `format !== currentFormat`, before it stores the new format; the online branch (`:1458-1461`) writes the saved-file status inside the `selected().join() === chosen.join()` guard and "Ready." otherwise; `removeLinkBuilder()` (`:853`) resets `#status` to "Ready." only when it removed an anchor and the status holds the saved-file text (a reset on every tick would overwrite "Building the DOCX file…" at the A8 tick and fail A9); the comments at `:668-674` and `:849-852` name the card press among the removal triggers. Write the baseline first: `node tests/prose.mjs --json <scratch>/baseline.json` on main.
- [ ] T2: `tests/smoke.spec.js`: `saveOnline()` (`:161`) waits for `/^Ready\./` and returns the status text; A19 reads the saved-file text after the first save; A16 (`:381`, `:511`) moves to a second online save pressed right after the first with the link present, reading the count and the button's disabled state in one `evaluate` right after the click (as A17 does at `:307-313`), then waiting for the saved-file text; the A17 wait (`:319`) stays "Ready."; A15's `afterTick` read (`:296`) adds the status text; A18 presses the Word card on step 2 after a save, reads the count, `#onlineNext`'s `hidden` attribute and the status, saves again, presses Online and reads a count of one; the header enumeration (`:17-26`) lists A18 and A19 and restates A16. Per LESSONS M119, show each absence selector matches somewhere first.
- [ ] T3: `tests/plants.mjs`: (x) drops the format-change removal (fails A18's Word read); (x2) drops the `format !== currentFormat` guard so every press removes the link (fails A18's Online read); (y) replaces the saved-file status with "Ready." (fails A19); (z) drops the reset in `removeLinkBuilder()` (fails A15); (o) (`:156-163`) parses the JSON, reverses `items`, serialises; (u) (`:198-205`) keeps failing A16 at its new site. Per LESSONS M118, `git add` the fix before planting.
- [ ] T4: `README.md` "The online form" (`:223-258`): the pointer to "Ticking every scale", the card press in the removal sentence, the status text; `node tests/prose.mjs --compare <scratch>/baseline.json` names only the changed passages.
- [ ] T5: `npm run smoke`, `npm run plants`, `npm run prose`; the builder PR on a `m127-online-card-loose-ends` branch, CI green; Jeff merges it from his terminal (LESSONS M116); this repo's PR carries the tracking.

## Work log

- 2026-09-25: created by /milestone-plan; promoted from the candidate row "Six online-card loose ends" (lineage M126 review O2 to O6, O10).
- 2026-09-25: criteria audit ran in full mode (user-facing tier), two passes by a fresh-context reader: pass one returned nine findings (A16 unsatisfiable once the card press removes the link; the status text stale after a tick and breaking the card handler's "Ready." prefix check; the guarded save leaving no link; a visibility read vacuous on step 1; `npm run prose` proving nothing about README.md; stale README and comment triggers; the Online re-press unspecified; one card probed for three; one reversal plant), six fixed as one-clear-answer, three posed at the gate; pass two, over the gate-settled wording, returned five (a hard wait in `saveOnline()` stopping the run before A19; `--compare` pairing by position and reading fact tokens only; the Online-keeps-it half unprobed and its order ambiguous; a late A16 read failing a correct page; a reset on every tick breaking A9), all fixed as one-clear-answer before writing.
- 2026-09-25: plan gate chose a status-line announcement over a second live region on `#onlineNext` because the page's design names `#status` its only announced region; falsified by a screen reader that does not speak the status text after the save.
- 2026-09-25: plan gate chose keeping the link on an Online re-press over removing it on any card press because a valid link should not need a second save; falsified by a visitor report of a stale link after a re-press.
- 2026-09-25: plan gate chose re-aiming A16 at a second online save over deleting A16 and plant (u) because the build-start removal would otherwise go untested; falsified by the re-aimed read passing on a page with that removal dropped.
- 2026-09-25: plan gate chose assertions only for the two fixes over full visibility coverage because the fixes' reads close the loose end at less matrix time; falsified by a hidden-attribute regression after the save or the tick that A18 does not see.
- 2026-09-25: plan chose one card pressed in A18 for three because all three share `setFormat()`; falsified by a per-card handler diverging from `setFormat()`.

## Decisions

## Review
