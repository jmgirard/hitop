# M053: A format-first flow for the browser module builder

- **Status:** in-progress
- **Priority:** normal
- **Depends on:** M052
- **Driving RR:** —
- **Principles touched:** —
- **Branch/PR:** `m053-builder-format-first-flow` (tracking); builder repo `m053-format-first-flow`

## Goal

The HiTOP-SR Module Builder asks which output format the visitor wants before it
asks anything about that format, so each visitor configures and downloads one
format and never reads another's settings.

## Scope

The deliverable is user-facing: a public web page researchers use. The work
lands in the sibling repo `jmgirard/hitop-builder` (single file `index.html`,
plus its `README.md`), on a branch and PR there; this repo carries only the
tracking record. It re-cuts the three steps M052 shipped and adds no control
that reaches a generator argument the page does not already pass.

**In:** three steps — choose scales, choose a format, then set that format's
options and download it from the same screen. The four control groups M052
placed on one options step are re-sorted per format, which splits the
*Qualtrics and REDCap naming* fieldset (a block name and an ID prefix that
reach only Qualtrics; a form name and a required checkbox that reach only
REDCap) into one group per format. From a completed download the page offers a
way back to the format choice with the scale selection kept. The step bar, the
selection recap, the shuffle notice and the disabled-until-a-scale-is-ticked
rule carry over.

**Out:** a light/dark switch of the page's own → the standing ROADMAP candidate,
declined a third time at this plan gate. Any change to what a generator prints
for a given set of arguments, and any generator argument the page does not
already pass → the maintainer sign-off gate. Naming a shuffled download
distinctly from an unshuffled one, and logging a shuffled form's printed order →
their own ROADMAP candidate rows. The R package → untouched.

## Acceptance criteria

- [ ] AC1: For each of the three formats, a file built from the branch page has
      the same content as the file the deployed M052 page builds from the same
      scale selection and the same option values, across this matrix: {a named
      two-scale selection, every scale} × {Word: US Letter and A4, numbering
      1-to-n and original, shuffle off} × {Qualtrics and REDCap: the package
      defaults, and one named non-default naming set}. The Qualtrics `.txt` is
      compared byte for byte, the REDCap archive's `instrument.csv` line for
      line and by entry list, and the Word file by its header text, page size
      and printed item rows parsed back out of it (a DOCX is not
      byte-reproducible); the download file name is compared in every cell.
      Both pages report the same `hitop` version, which each prints on load; a
      version that moves between captures is re-baselined.
- [ ] AC2: On the third step, the focusable form controls the page renders
      under each format — enumerated by walking the shown step with real Tab
      presses, once per format — are exactly: for Word, the paper-size radios,
      the numbering radios and the shuffle checkbox; for Qualtrics, the block
      name and question ID prefix boxes; for REDCap, the form name box and the
      required checkbox. No control in any of the three enumerations sets a
      value another format's generator call reads.
- [ ] AC3: From a completed download, a second format is built for the same
      scales without re-ticking any scale: driving that path returns a file
      whose scale content matches the first file's, verified by reading both
      files back.
- [ ] AC4: Every step boundary the page defines is crossable by keyboard alone
      in both directions where the page offers both, each arrived-at step's
      heading takes focus and paints the focus indicator M051 established, and
      the download control stays unavailable while no scale is ticked, with a
      rendered hint saying why. Querying the rendered DOM for elements carrying
      `aria-live` or a role with an implicit live value returns exactly the
      status line (polite) and the log pane (`off`), whichever step is shown.
- [ ] AC5: The status line, the log pane, the two opening paragraphs and the
      back-link to the package documentation render outside the steps, and each
      one's rendered text is unchanged from the deployed M052 page.
- [ ] AC6: At viewport widths of 360, 768 and 1280 CSS pixels, in both colour
      schemes, every step renders its own controls unclipped — each control's
      bounding rectangle lies within its container's — and the page's document
      scrolls vertically only (`documentElement.scrollWidth <= clientWidth`).
- [ ] AC7: Every control-group title the page renders, read out of the elements
      that carry those titles, appears verbatim in `README.md`, and the README
      describes the page's steps in the order the page presents them.

## Coverage

- AC1 → T1, T5
- AC2 → T2, T5
- AC3 → T3, T5
- AC4 → T3, T4
- AC5 → T2, T4
- AC6 → T4
- AC7 → T6

## Tasks

- [x] T1: Capture the AC1 baseline from the deployed M052 page — each matrix
      cell's file plus the reported `hitop` version — by the blob-capture and
      `read_page`/`form_input` route M052's T1 used; confirm by digest that the
      deployed page is the builder repo's `origin/main` `index.html`.
- [x] T2: Re-cut the markup into the three steps, sorting the four existing
      control groups per format and splitting the shared naming fieldset;
      keep the shuffle notice with the shuffle control and leave the status
      line, log, opening paragraphs and back-link page-level.
- [x] T3: Wire the format choice and the download step: which options show,
      the single download button, the return-to-the-format-choice control that
      keeps the selection, step-bar labels, keyboard reach and focus placement.
- [ ] T4: Lay out each step at 360, 768 and 1280 CSS pixels in both schemes;
      run the keyboard walk across every boundary, the focus-indicator check,
      the live-region re-read and the horizontal-overflow check.
- [ ] T5: Verify behaviour: rebuild every AC1 matrix cell from the branch page
      and compare against the T1 baseline; run the per-format control
      enumeration; drive the two-format path and read both files back. Show
      each detector able to fail on a planted defect before trusting a clean
      result.
- [ ] T6: Rewrite `README.md`'s *What the page shows* section to walk the new
      steps in page order and to name each format's own control group.

## Work log

- 2026-08-24: created by /milestone-plan.
- 2026-08-24: criteria audit ran inline in full mode, not in a fresh-context subagent (this session is configured not to spawn agents, as at M052). Two findings, both fixed before the gate: a drafted "no other page behavior moves" criterion claimed a universal over a domain no named procedure enumerates, narrowed to AC5's four named regions compared by rendered text; and a drafted per-format criterion mixed absence from the DOM with absence from the accessibility tree, narrowed to AC2's Tab-walk enumeration.
- 2026-08-24: plan gate chose three steps with options and download merged over a four-step flow separating them because each format carries two or three options, so the merged step stays short and the button sits beside the settings it uses; falsified by a download step that grows past one screen at 360 CSS pixels.
- 2026-08-24: plan gate chose an explicit return-to-the-format-choice control over leaving the two-format case to the step bar, and over keeping all three download buttons on the last step, because the step bar advertises nothing and three buttons undo what choosing a format buys; falsified by a visitor reporting the second format harder to reach than on the M052 page.
- 2026-08-24: plan gate chose the full 16-cell AC1 matrix over a reduced six-cell one because the harness and the baseline method already exist from M052, so the cost is running it; falsified by the matrix run exceeding a working session.
- 2026-08-24: implementation gate chose three format buttons that advance on click over radios plus Continue; group titles dropping the format prefix ("Paper size", "Item numbering", "Item order") with the shared naming fieldset split into "Block and question naming" and "Form name and required items"; a standing "Choose a different format" button at the foot of the last step over an extra post-download line; step-bar labels "Choose scales", "Choose a format", "Options and download".

- 2026-08-24: T1 — AC1 baseline captured from the deployed page, confirmed byte-identical to the builder repo's `origin/main` `index.html` (sha256 `5ebe25f0…`); `hitop` 0.2.0, 76 scales; all 16 matrix cells built and summarized (Qualtrics whole-file digest; REDCap entry list, per-line digests and whole-`instrument.csv` digest; Word header text, `w:pgSz` and printed table rows parsed from `word/document.xml`); two-scale selection is Agoraphobia + Social Anxiety, non-default naming set block `Wave 2 Screening` / prefix `W2SCR` / form `wave2_screening` with required unticked. A first parse run was discarded: the `<w:t>` regex also matched `<w:tbl`/`<w:tc`/`<w:tr`, so the extracted "text" was markup; re-run after anchoring the tag boundary.

- 2026-08-24: T2 and T3 landed in one commit to `index.html` (builder repo `3dd1357`) — the markup re-cut and its wiring are one edit to one file. Step two is a format choice of three cards, each recording the choice and moving to step three; step three shows one format panel (Word: Paper size / Item numbering / Item order; Qualtrics: Block and question naming; REDCap: Form name and required items), one download button named for the format, and a standing "Choose a different format" button at its foot. `currentFormat` starts at Word so a step-bar jump straight to step three is coherent.

## Decisions

## Review
