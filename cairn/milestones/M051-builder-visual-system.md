# M051: A visual system and tightened copy for the browser module builder

- **Status:** review
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** IP1
- **Branch/PR:** `m051-builder-visual-system` (tracking, jmgirard/hitop) · `m051-visual-system` (code, jmgirard/hitop-builder)

## Goal

The HiTOP-SR Module Builder page carries a stated type, spacing and colour system in place of default browser styling, and copy rewritten to the same standard, with every file it builds unchanged.

## Scope

Surface tier: **user-facing** — the deliverable is a public web page researchers visit. The page's own copy is researcher-facing and outside IP1 (D-038); what the generated documents contain is not, which is what AC1 fences.

The work lands in the sibling repo `jmgirard/hitop-builder` (single file `index.html`, no build step, no backend), on a branch and PR there; this repo carries only the tracking record.

**In:** the page's `<style>` block and the markup it styles; the page's own words — headings, control labels, group titles, hints, notices, status and log lines; `README.md` where a renamed control makes it wrong.

**Out:** the page's element order and interaction model → M052. Any change to what the `hitop` package generates, or to which generator arguments the page passes (the unexposed `title`/`font_size`/`font_family` and `include_instructions`/`breaks` stay with their candidate rows). Recovering a shuffled whole-instrument's printed order → its candidate row (DESIGN known issue 8). Distinguishing shuffled from unshuffled download names → its candidate row. Tests, CI or a package version pin for the builder repo → its candidate row. Any change in this repo's R package.

## Acceptance criteria

- [ ] AC1: For each of the three formats, a file built from the branch page has the same content as the file the deployed page builds from the same selections and settings, across this matrix: {a named two-scale selection, every scale} × {Word: US Letter and A4, numbering 1-to-n and original, shuffle off} × {Qualtrics and REDCap: the package defaults, and one named non-default naming set}. The Qualtrics `.txt` is compared byte for byte, the REDCap archive's `instrument.csv` line for line, and the Word file has the same header text and the same printed item rows, parsed back out of it (a DOCX is not byte-reproducible). Both pages report the same `hitop` version, which each prints on load; a version that moves between captures is re-baselined.
- [ ] AC2: With one scale ticked and the shuffle box ticked, every focusable control the page renders — enumerated by querying the rendered DOM for the focusable set in that state — is reachable by keyboard and paints a focus indicator that is a non-transparent outline or box-shadow of non-zero width. Querying the DOM for elements carrying `aria-live` or a role with an implicit live value returns exactly the status line (polite) and the log pane (`off`).
- [ ] AC3: In both the light and the dark colour scheme, at viewport widths of 360, 768 and 1280 CSS pixels, the page's document scrolls vertically only (`documentElement.scrollWidth <= clientWidth`). Every element bearing text over a resolved opaque background at effective opacity 1, enumerated by a script walking the rendered DOM and computing the ratio from `getComputedStyle`, meets the contrast WCAG 2.2 SC 1.4.3 (Level AA) requires — 4.5:1, or 3:1 at 24px, or 18.66px bold, and above. The cases that walk excludes — disabled buttons, muted and hint text, link colour, the log pane — are each measured against their own rendered backdrop and meet the same figures.
- [ ] AC4: Each of the four numbering × selection combinations shows a shuffle notice whose crosswalk sentence matches what the Word file built under that same combination actually contains, verified by building the file in each of the four and reading its scoring page back.
- [ ] AC5: The page's rendered text names `webr.r-wasm.org`, `jmgirard.r-universe.dev` and `repo.r-wasm.org`. Over one full page load plus one build of each of the three formats, every network request the browser records goes to the page's own origin or to a host the page's rendered text names.
- [ ] AC6: Every control-group title the page renders, read out of the elements that carry those titles, appears verbatim in `README.md`, and the README names in italics no control group the page does not render.

## Coverage

- AC1 → T1, T6
- AC2 → T2, T5
- AC3 → T2, T3, T5
- AC4 → T4, T6
- AC5 → T4, T6
- AC6 → T4, T7

## Tasks

- [x] T1: Capture the AC1 baseline from the deployed page — each matrix cell's file, plus the reported `hitop` version — by patching `URL.createObjectURL` to keep the blob and returning early from the anchor's click, driving the controls through `read_page` refs and `form_input` (never screenshot coordinates). Record the current control-group titles, live-region arrangement and the four crosswalk sentences alongside.
- [x] T2: Author the visual system in the page's `<style>` block: type scale, spacing scale, colour tokens for both schemes, surface and border treatment, button/input/checkbox states, and a focus ring meeting AC2's non-zero-width, non-transparent bar.
- [x] T3: Apply the system to the existing markup without moving elements — the notice, the scale grid, the four control groups, the download row, the status line and the log pane — and make each fluid at 360px.
- [x] T4: Rewrite the page's copy — headings, labels, group titles, hints, the privacy notice (adding `repo.r-wasm.org`), the shuffle notice and its three crosswalk sentences, the status and log strings — keeping every factual claim each one makes.
- [x] T5: Verify presentation: the keyboard tab-through and focus-indicator comparison, the live-region re-read, the contrast sweep including the excluded cases, and the horizontal-overflow check at all three widths in both schemes; screenshots at each as evidence.
- [x] T6: Verify behaviour: rebuild every AC1 matrix cell from the branch page and compare against the T1 baseline; drive the four crosswalk combinations and read each built file back; record the hosts of every network request over a load plus three builds.
- [ ] T7: Update `README.md` wherever a renamed control group or heading makes it wrong.

## Work log

- 2026-08-24: created by /milestone-plan.
- 2026-08-24: implementation started; branches cut in both repos.
- 2026-08-24: implementation gate — own token palette in the blue family over copying the pkgdown site's stock Bootstrap or going near-monochrome; the four control-group titles keep their current wording, so the copy pass works on the lede, notices, hints and status strings and the README's italicized names stay correct; no light/dark toggle, the page keeps following the operating system, recorded as a ROADMAP candidate for the stepped-flow rework.
- 2026-08-24: ROADMAP hygiene paid for that candidate row — a stray blank line in the candidates list dropped, the hygiene stamp restated, and the builder version-pin row's lineage parenthetical compressed; 59 lines, 23,974 bytes, both under cap.
- 2026-08-24: T1 — AC1 baseline captured from the deployed page (hitop 0.2.0, 76 scales) over all 16 matrix cells, with `URL.createObjectURL` patched to keep the blob and the anchor's click suppressed; controls driven through read_page refs and form_input. Two-scale selection is agoraphobia + bingeEating (8 items); the named set is block "Wave 2 Screening", prefix "W2SCR", form "wave2_screening", required unticked. A DOCX proved not byte-reproducible as the criterion says — the same selection built twice gave different raw bytes and identical extracted text — so the comparison key is a SHA-256 over the extracted content: the .txt itself, the archive's instrument.csv, or word/document.xml's paragraph text plus the header and footer strings. Baseline, page copy and the replay harness in the session scratchpad under `m051/`.
- 2026-08-24: criteria audit ran in FULL mode (user-facing tier), fresh-context [O] reader. Nine findings across AC1, AC2, AC3, AC5, AC6 plus one coverage gap on AC2; all had one clear right answer and were fixed before the criteria were written — AC1 narrowed to what its procedure compares and given a version-parity clause, AC2 re-stated over a non-empty focusable set with a rendering focus indicator and a named live-region query, AC3 scoped to opaque full-opacity backdrops with the excluded cases enumerated and its screenshot clause moved to T5, AC5 corrected against `repo.r-wasm.org` which the notice omits today, AC6 restated as containment against the README's italicized group names, and T5 given the keyboard and live-region sweep AC2 had no task for. AC4 passed all six questions.
- 2026-08-24: plan gate chose a restyle holding the existing element order over a stepped rework in one milestone because the user's stepped-flow answer trips the sizing tripwires (new interaction model plus visual system plus copy pass); the rework is M052, depending on this. Falsified by the restyle proving unshippable without the reflow — a control group that cannot be made legible at 360px in the current order.
- 2026-08-24: plan gate chose a self-contained page with no new outside host over a hosted web font because the page's privacy notice enumerates where it connects and a font server adds a fourth host to disclose. Falsified by the system fonts proving unable to carry the type scale legibly in the AC3 sweep.

- 2026-08-24: T2/T3/T4 — visual system authored and applied, copy rewritten (hitop-builder 5a89d70). Colour tokens for both schemes with every text pair computed against its surface before any CSS was written; disabled buttons carry stated colours rather than `opacity: .5`, which lands under 4.5:1 on every surface here; `color-scheme` added so the browser paints checkboxes, radios and scrollbars in the page's scheme; `fieldset > label` stacks each group's choices alike. Privacy notice now names `repo.r-wasm.org`, the third host the page fetches from and the one it omitted. Control-group titles unchanged per the gate. First audit run (dark, 400px): 191 text elements walked, none skipped, no failures, lowest ratio 5.42:1, no horizontal overflow.

- 2026-08-24: AC5 amended at a mini gate — the user judged the privacy callout to answer a question the page never raises, there being no way to give the page anyone's data, and chose a plain host line over removing the mention entirely. The callout's reassurance clauses leave both the page and the criterion; the host-naming and the network check stay. Criteria audit ran in FULL mode on the amended wording and returned one finding with a clear right answer, fixed before it was written: the draft promised both "every host it fetches from" and a three-host enumeration, so a fourth host would have satisfied the enumeration while falsifying the universal — the first sentence narrowed to the enumeration, the second sentence carrying the universal over a domain it names. Deviation: the audit ran inline rather than in a fresh-context [O] reader, this session being configured not to spawn agents (the M050 precedent).
- 2026-08-24: T5/T6 verification restarted from the amended page — the AC2 focusable-set and AC3 contrast sweeps taken before the amendment are superseded by the re-runs below.

- 2026-08-24: T5 — presentation verified on the amended page. AC2: 95 focusable elements queried from the rendered DOM; 100 real Tab presses reached every one but the two unselected radios, which a radio group makes arrow-key stops and which were then reached with ArrowDown; all 104 focus events painted one indicator, `outline: solid 3px` at offset 2px, non-transparent in both schemes, none missing. The live-region query returns exactly `div#status` (polite) and `pre#log` (off). AC3: six cells (360/768/1280 × light/dark), 195 text elements walked in each, none skipped, zero failures, lowest ratio 6.52:1 light and 7.25:1 dark, `scrollWidth == clientWidth` at every width; the named cases measured against their own backdrops, the tightest being the disabled buttons at 4.93:1 light. The audit was shown able to fail first — a planted `#b9c2cb` tally reported at 1.8:1, a planted 0.5 opacity moved four elements to skipped, and undoing both returned it silent over the same 195-element domain. Evidence in the session scratchpad, `m051/t5-presentation.md`.

- 2026-08-24: T6 — behaviour verified (hitop-builder 3c56d57). Both pages reported hitop 0.2.0, so no re-baselining. AC1: all 16 matrix cells rebuilt through the same ref and form_input driving and compared on file name, non-blank line count, content digest and DOCX header — 16 of 16 identical, 0 mismatches; raw byte counts differ on some DOCX cells, which is the non-reproducibility the criterion anticipates, and the comparison was shown able to fail on a flipped digest, a swapped header and a changed line count. AC4: each of the four numbering × selection combinations matched its file, scanning the whole document text rather than its tail — the 1-to-n module form carries `Item Number Crosswalk (printed number → original HiTOP-SR number)` with 8 pairs, the other three carry no crosswalk line and no arrow pairs.
- 2026-08-24: T6 finding, fixed on the branch — the page named three hosts but fetches from four. Neither the browser's request recorder nor the page's performance timeline sees webR's worker-side fetches, so traffic was constrained instead: a copy of the page under an enforced Content-Security-Policy naming only its own origin and the hosts its text names. That run failed on the package download, and `curl -L` showed `jmgirard.r-universe.dev` answers 302 with `location: https://r2.ropensci.org/<sha256>`; `repo.r-wasm.org` and `webr.r-wasm.org` answer 200 with no redirect. The page and README now name `r2.ropensci.org`. With all four named, one load plus one build of each format ran clean with zero policy violations; dropping `repo.r-wasm.org` from the policy fails on the missing `rlang`, so the policy binds the worker rather than passing everything. The only edit after the AC1 and AC4 runs is that one sentence, touching no CSS and no script line. Evidence in the session scratchpad, `m051/t6-behaviour.md`.

- 2026-08-24: all seven tasks done; `devtools::test()` clean on the branch (0 failures, 0 warnings, 1 skip, 13897 passing — the R package is untouched by this milestone, so this confirms the branch rather than exercising it). AC6 re-checked after the README edits: all nine rendered group titles appear verbatim in `README.md`, and every italicized name in the README is a group the page renders. Both branches pushed. Status to review.

## Decisions

## Review
