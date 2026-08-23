# M049: An original-numbering toggle for the browser module builder

- **Status:** blocked
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —  <!-- RB03 open; advisory, no binding criteria requested -->
- **Principles touched:** IP1
- **Branch/PR:** `m049-builder-numbering-toggle`

## Goal

The browser module builder lets the person building a Word module form keep the
HiTOP-SR's own item numbers instead of the default `1..n`, so a paper form's
printed numbers line up with the column names the same module's Qualtrics and
REDCap exports produce.

## Scope

This milestone's deliverable is **user-facing**: a control on a public web page
and the copy around it. The code lives in `jmgirard/hitop-builder`; the hitop
branch carries the milestone record, and the builder commit is pushed at merge
(the M048 pattern).

**In:** a *Word item numbering* control in the builder's `index.html`, passing
`renumber` to `generate_docx_hitopsr()` and to no other call; a rewrite of the
existing shuffle notice, which today asserts unconditionally that every
shuffled Word file carries a crosswalk — false once numbering can be set to
original, because the package prints no crosswalk when `renumber = FALSE`
(`R/generate_docx.R:110-120`); a README section for the new control.

**Out:** `include_scoring` — dropped at the 2026-08-23 plan gate, so the M045
candidate row stands unchanged. The Qualtrics and REDCap controls and their
package-side validation → M050. Naming a download from the options that
produced it → the standing M048 candidate row. `title`, `font_size`,
`font_family` → a new candidate row. `include_subscales` → nowhere: the
package errors when it is combined with `module`, which the app always
supplies (`R/generate_docx.R:191-199`), so it is not exposable at all.

## Acceptance criteria

- [ ] AC1. In a browser loading the modified page, two named modules — one
      whose scales sit low in the instrument and one whose scales sit high, so
      the gapped-number case is exercised — each build a Word file in both
      numbering states. With the control set to *original*, the printed item
      numbers read out of each built `.docx` are exactly that module's
      HiTOP-SR numbers taken from `hitopsr_items` independently of the app;
      with the control at its default they are `1..n`. Evidence: all four
      number lists quoted.
- [ ] AC2. `renumber` reaches the Word call alone. For one module, the
      Qualtrics `.txt` downloaded with numbering set to *original* is
      byte-identical to the one downloaded at the default, and the two REDCap
      archives carry an identical `instrument.csv` (member-content, not
      archive bytes — DOCX and REDCap zips are not byte-deterministic).
      Evidence: the four downloads and the comparison output.
- [ ] AC3. The page's shuffle notice is conditional on the numbering state and
      matches the built document in both. With shuffle ticked and numbering at
      *original* the visible notice says the file carries no crosswalk and the
      printed numbers are the instrument's own, and the built `.docx` contains
      no crosswalk; with shuffle ticked and numbering at its default the
      notice says the crosswalk is present, and it is. Evidence: the notice
      text quoted in each state beside whether the crosswalk appears.
- [ ] AC4. The new control is a `fieldset`/`legend` group whose input carries
      an associated label, on the same pattern as the existing `papersize` and
      `shuffle` groups, and it appears as such in the page's accessibility
      tree. These named existing controls still behave after the change: the
      scale filter, Select all, Clear all, the item tally, and each of the
      three download buttons. Evidence: the accessibility tree for the new
      group, and a run through each named control.
- [ ] AC5. The builder `README.md` gains a section for the numbering control
      stating what it does, that it applies to the Word file only, and what
      happens when it is combined with shuffle — matching the behavior AC1–AC3
      verified — carrying the verification date. Evidence: the section quoted.

## Coverage

- AC1 → T1, T3
- AC2 → T1, T3
- AC3 → T2, T3
- AC4 → T1, T4
- AC5 → T5

## Tasks

- [ ] T1. In `jmgirard/hitop-builder` on a branch: add the *Word item
      numbering* fieldset (default = renumbered) beside the existing
      `papersize` and `shuffle` groups in `index.html`; bind its value and
      pass `renumber` into the DOCX call only, mirroring how `randomize` is
      passed today (`index.html:296-306`).
- [ ] T2. Rewrite the shuffle notice so its text is conditional on the
      numbering state, and wire it to update when either control changes.
- [ ] T3. Serve the modified page locally and verify AC1–AC3: build both
      modules in both numbering states, parse each `.docx` for its printed
      numbers and for the crosswalk, and compare the Qualtrics and REDCap
      downloads across states.
- [ ] T4. Verify AC4: read the page's accessibility tree for the new group and
      drive each named existing control.
- [ ] T5. Add the README section (AC5) with the verification date; open the
      hitop-side PR carrying the milestone record; push the builder commit at
      merge.

## Work log

- 2026-08-23: created by /milestone-plan.
- 2026-08-23: criteria audit ran in FULL mode (user-facing tier), inline rather than in a fresh-context subagent (session configured not to spawn agents). One finding: AC1 as first drafted probed a single module, standing one exemplar in for the family the promise covers — fixed here by requiring two modules chosen to differ on whether the instrument's own numbers are gapped. No other finding.
- 2026-08-23: plan gate chose exposing `renumber` as a two-state control over leaving the app to always renumber and documenting the limitation; the latter lost because a researcher fielding paper and online forms of one module gets two numbering schemes from the app with no way to align them. Falsified by a report that the original-numbering form is never wanted in practice.
- 2026-08-23: plan gate chose a conditional shuffle notice over leaving the existing unconditional wording; the latter lost because `renumber = FALSE` makes its crosswalk claim false. Falsified by the package printing a crosswalk under `renumber = FALSE`, which `R/generate_docx.R:110-120` says it does not.
- 2026-08-23: implementation gate Q1 asked whether the numbering control still ships, given the maintainer's steer that a paper form being scoreable from what it prints outranks aligning its numbers to the online exports — the goal's stated reason. The maintainer chose escalation to a review brief over the three offered options. M049 is marked as touching IP1, so this question sat on an escalation tripwire the chip should have carried an option for; it did not, and the maintainer supplied one.
- 2026-08-23: implementation gate Q2 chose having the page treat an all-scales tick as the full instrument (no module passed) over leaving it a module or fixing only the header. Verified on main the same day: an all-scales module is exactly items 1-405, so renumbering is already the identity there; what differs is the Word header (`HiTOP-SR Module (v1.0)` against `HiTOP-SR (v1.0)`) and the 405-pair crosswalk a shuffled all-scales module prints and a whole-instrument call declines; the Qualtrics `.txt` and the REDCap `instrument.csv` are byte-identical between the two calls. The scope and criteria amendment this needs is deferred until the escalated Q1 resolves, since the shape of the control it attaches to is what is under review.
- 2026-08-23: blocked on RB03 (`cairn/reviews/RB03-builder-word-numbering-control.md`), which asks whether the Word item-numbering control belongs on the page at all under the maintainer's self-containment priority, and what the goal should rest on if it does. The brief and this record are committed on the milestone branch rather than the default branch, because the branch already carries M049's status mirror and file; putting the pair on the default branch would split the record.

## Decisions

## Review
