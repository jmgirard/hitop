# M052: A stepped flow for the browser module builder

- **Status:** in-progress
- **Priority:** normal
- **Depends on:** M051
- **Driving RR:** —
- **Principles touched:** IP1
- **Branch/PR:** `m052-builder-stepped-flow` (this repo) · `m052-stepped-flow` in `jmgirard/hitop-builder`

## Goal

The HiTOP-SR Module Builder presents its work as a progression — choose scales, set options, download — instead of one long column, with every file it builds unchanged.

## Scope

Surface tier: **user-facing** — the deliverable is a public web page researchers visit. The interaction model is page behaviour, outside IP1 (D-038); what the generated documents contain is not, which is what AC1 fences.

The work lands in the sibling repo `jmgirard/hitop-builder` (single file `index.html`), on a branch and PR there; this repo carries only the tracking record. It builds on M051's visual system rather than restating it.

**In:** the page's element order and the markup and script that drive a stepped progression; whatever indication of progress and current selection the steps themselves need; the wide-screen and narrow-screen layouts of each step; `README.md` where the reorganization makes it wrong.

**Out:** a separate persistent selection summary, a light/dark switch, and pinned download buttons — each declined at the 2026-08-24 plan gate. Any new control exposing a generator argument the page does not expose today → the standing candidate rows. Any change to what the `hitop` package generates. Tests, CI or a package version pin for the builder repo → its candidate row. Any change in this repo's R package.

## Acceptance criteria

- [ ] AC1: For each of the three formats, a file built from the branch page has the same content as the file the M051 page builds from the same selections and settings, across this matrix: {a named two-scale selection, every scale} × {Word: US Letter and A4, numbering 1-to-n and original, shuffle off} × {Qualtrics and REDCap: the package defaults, and one named non-default naming set}. The Qualtrics `.txt` is compared byte for byte, the REDCap archive's `instrument.csv` line for line, and the Word file has the same header text and the same printed item rows, parsed back out of it (a DOCX is not byte-reproducible). Both pages report the same `hitop` version, which each prints on load; a version that moves between captures is re-baselined.
- [ ] AC2: Every step the page defines is reachable and leaveable by keyboard alone from the step before it, and each step's controls — enumerated by querying the rendered DOM for the focusable set while that step is shown — paint the focus indicator M051 established. Querying the DOM for elements carrying `aria-live` or a role with an implicit live value returns exactly the status line (polite) and the log pane (`off`), whichever step is shown.
- [ ] AC3: At viewport widths of 360, 768 and 1280 CSS pixels, in both colour schemes, every step renders its own controls unclipped — each control's bounding rectangle lies within its container's — and the page's document scrolls vertically only (`documentElement.scrollWidth <= clientWidth`).
- [ ] AC4: Each notice the page can show appears on the step carrying the control it qualifies, and each of the four numbering × selection combinations shows a shuffle notice whose crosswalk sentence matches what the Word file built under that same combination actually contains, verified by building the file in each of the four and reading its scoring page back.
- [ ] AC5: Reaching the download controls and building each of the three formats is possible from a first load with no scale ticked, by keyboard alone, and the download controls stay unavailable while no scale is ticked.
- [ ] AC6: Every control-group title the page renders, read out of the elements that carry those titles, appears verbatim in `README.md`, and the README describes the page's steps in the order the page presents them.

## Coverage

- AC1 → T1, T5
- AC2 → T2, T3, T4
- AC3 → T3, T4
- AC4 → T2, T5
- AC5 → T2, T4
- AC6 → T6

## Tasks

- [ ] T1: Capture the AC1 baseline from the M051 page — each matrix cell's file plus the reported `hitop` version — by the blob-capture and `read_page`/`form_input` route M051's T1 used.
- [x] T2: Restructure the markup into steps — choose scales, set options, download — moving the existing control groups and notices under the step each belongs to, and keeping every notice's trigger logic intact.
- [x] T3: Wire step navigation: showing and hiding, the progress indication the steps need, keyboard reach between steps, and focus placement on entering one.
- [ ] T4: Lay out each step at 360, 768 and 1280 CSS pixels in both schemes; verify unclipped rendering, the horizontal-overflow check, the keyboard walk through every step, and the live-region re-read.
- [ ] T5: Verify behaviour: rebuild every AC1 matrix cell from the branch page and compare against the T1 baseline; drive the four crosswalk combinations and read each built file back.
- [ ] T6: Rewrite `README.md`'s control-group sections to describe the steps in the order the page presents them.

## Work log

- 2026-08-24: created by /milestone-plan, split from M051 at the plan gate.
- 2026-08-24: criteria audit ran in FULL mode (user-facing tier) over M051's drafted criteria, which these were derived from after its nine findings were fixed; the derived criteria carry the same repairs — procedure-bounded comparison, a non-empty focusable set, a named live-region query, page properties rather than screenshot existence. No separate reader ran over this file's wording.
- 2026-08-24: plan gate chose a stepped progression over a two-column reflow and over leaving the element order alone, on the user's answer at the 2026-08-24 gate. Falsified by a step boundary that cannot be drawn without splitting a control group whose parts depend on each other.
- 2026-08-24: plan gate declined a persistent selection summary, a light/dark switch and pinned download buttons, on the user's answer at the same gate; whatever progress and selection indication the steps themselves need is in scope. Falsified by a visitor unable to tell from a step which scales are currently chosen.
- 2026-08-24: implementation gate chose one step on screen at a time with a jumpable step bar, a count-and-items recap line on the later steps, the status line and log page-level, and both opening paragraphs above the steps.
- 2026-08-24: the builder repo has no DESCRIPTION and no test suite, so the inferred profile is generic and its verify slot is empty; the page checks in T4 and T5 are this milestone's verification, as at M051.
- 2026-08-24: T2, T3 — `index.html` restructured into three steps with a step bar, Back/Continue pairs, a recap line and heading focus on arrival (hitop-builder `ad5a001`).

## Decisions

## Review
