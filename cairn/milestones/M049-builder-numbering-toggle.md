# M049: An original-numbering toggle for the browser module builder

- **Status:** planned
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** RR03 (advisory; no binding criteria requested)
- **Principles touched:** IP1
- **Branch/PR:** — (the `m049-builder-numbering-toggle` branch carried tracking only and was folded back; a re-cut milestone cuts a fresh one)

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
- 2026-08-23: RB03 reviewed by a Fable subagent; RR03 ingested and the pair archived. All eight recommendations triaged below in the Decisions section: five applied, two accepted rejections, one deferred to a gate. One departure from the report, logged at M049-D1: its recommendation that the Goal be re-cut through /milestone-plan is put to the maintainer rather than taken, because the replacement rationale the report supplies is the alignment the Goal's existing sentence already states.
- 2026-08-23: amendment gate returned the Goal to planning, choosing RR03's recommendation over the session's contrary reading; status back to `planned` and no code written. The same gate settled the four inputs the re-cut inherits, recorded at M049-D5. No implementation work was done on the branch at any point — every commit was tracking.

## Decisions

**M049-D1 (2026-08-23): the numbering control ships, on the reading of its
purpose RR03 supplies.** The maintainer's priority — a paper form being
scoreable from what it prints, over its numbers mapping back to the instrument
— does not rule the control out, because the two never trade off: all four
combinations of numbering and shuffle print a scoring key built from the same
printed-order map as the items table, so every form the control can produce is
scoreable from the paper alone. What the priority does rule out is justifying
the control mainly as a codebook convenience. The purpose that survives is
narrower and concrete: the printed number is the name a person uses to move a
response off the page, and with the control set to original those names are the
field names the same page's REDCap and Qualtrics exports give the collected
variables, so hand entry needs no translation. *Departure from RR03:* the
report also recommends the Goal be re-cut through `/milestone-plan`. That is
put to the maintainer rather than taken, because the Goal's existing sentence
already states the alignment the report's replacement rationale rests on; what
the report changes is why that alignment matters, which is recorded here.

**M049-D2 (2026-08-23): original numbering combined with shuffle stays
available, with no block and no package-side warning.** Such a form prints the
instrument's own numbers in scrambled order and a scoring key listing those
same numbers, so it is internally consistent and scoreable from the page; the
crosswalk the package omits there would read "42 → 42" on every row. The page's
conditional shuffle notice is the right instrument, and it is researcher-facing
copy, so IP1 is not engaged. Blocking the combination package-side would be a
behavior change to a signed-off generator with no defect behind it.

**M049-D3 (2026-08-23): the all-scales reconciliation lives in the page, not
the package.** What is being acted on is a visitor's intent — every box ticked
means the whole instrument — which only the page can observe; the package sees
an item set. The page carries a comment recording the assumption this rests on,
that the HiTOP-SR's scales tile its items exactly, so a future multi-instrument
builder trips over it deliberately. Rejected with RR03: collapsing an
all-scales module package-side, in `hitop_module()`, `apply_module()`, or by
coverage in `generate_docx_hitopsr()`. Each would change the header token and
the crosswalk's presence for existing R callers, which is participant-facing
printed text needing a fresh gate and a partial supersession of D-037, to
encode a judgment about intent the package cannot see. An R caller passing an
all-scales module therefore still gets module framing, on purpose.

**M049-D5 (2026-08-23): the Goal returns to planning, and the four inputs the
re-cut inherits.** RR03 recommended the Goal be re-cut because the reason it
gives for the control — aligning a paper form's numbers with the online
exports' column names — is not the reason that survives the maintainer's
priority. The session read the existing sentence as already stating the
alignment RR03's replacement rationale rests on and recommended carrying on;
the maintainer chose the re-cut, so the Goal returns to `/milestone-plan` and
this session wrote no code. The re-cut carries forward, all settled at the same
gate:

1. *The purpose the Goal should state.* The printed number is the name a person
   uses to move a response off the page. With the control set to original,
   those names are the field names the page's own REDCap and Qualtrics exports
   give the collected variables, so hand-entering paper responses into a
   project built from the same page needs no translation. Both numbering states
   produce a form scoreable from the paper alone, so the control never trades
   away self-containment — the subordinate clause RR03 asks the Goal to keep.
2. *The all-scales behavior is in scope, with its own criterion and task.* The
   page passes no `module` when every scale is ticked, so the Word header reads
   `HiTOP-SR (v1.0)` and a shuffled all-scales build carries no crosswalk; the
   page carries a comment recording that this rests on the HiTOP-SR's 76 scales
   tiling its 405 items exactly. Its criterion pins both headers and holds the
   Qualtrics `.txt` and REDCap `instrument.csv` unchanged.
3. *The shuffle-notice criterion is two-dimensional.* It conditions on the
   numbering control and on whether every scale is ticked, and covers three
   states: original-plus-shuffle (no crosswalk, printed numbers are the
   instrument's own), default-plus-shuffle on a module (crosswalk present), and
   default-plus-shuffle with every scale ticked (no crosswalk claimed). Without
   the third the page ships a notice that is false in exactly the configuration
   the maintainer flagged.
4. *Three smaller items, all taken into the milestone rather than the backlog.*
   The printed-numbers criterion pins ascending order as well as the number set;
   an all-scales download is named for what it contains rather than
   `hitopsr-module.docx`, which narrows the standing M048 candidate row rather
   than duplicating it; and `hitop_module()`'s help page gains a sentence
   noting that a module naming every scale still receives module framing from
   the Word generator, so an R caller wanting full-instrument framing passes no
   module. The third makes the milestone touch the package, not the page alone.

**M049-D4 (2026-08-23): triage of RR03's eight recommendations.** Applied: 1
(ship the control, per M049-D1), 2 (leave original-plus-shuffle available, per
M049-D2), 3 (make the shuffle notice conditional on the all-scales state as
well as the numbering state — without it the page ships a notice that is false
in exactly the configuration the maintainer flagged), 4 (place the all-scales
reconciliation in the page with the tiling comment, per M049-D3). Accepted as
rejections: 7 (blocking or warning original-plus-shuffle) and 8 (package-side
collapse of all-scales modules), both reasoned above. Considered, deferred to
the maintainer at the amendment gate: 5 (name an all-scales download something
other than `hitopsr-module.docx`, which the standing M048 candidate row already
covers) and 6 (a sentence in `hitop_module()`'s documentation on the module
framing an all-scales module still receives). Also from the report's Beyond the
brief: AC1 is satisfiable by the right numbers in the wrong order and could pin
ascending order — likewise deferred to the amendment gate. Promoted to
`cairn/DECISIONS.md` as D-038: that a builder control reaching an
already-signed-off generator argument is page behavior rather than an
artifact-touching change under IP1.

## Review
