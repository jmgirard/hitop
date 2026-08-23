# M049: An original-numbering toggle for the browser module builder

- **Status:** in-progress
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** RR03 (advisory; no binding criteria requested)
- **Principles touched:** IP1
- **Branch/PR:** `m049-builder-numbering-toggle` (the earlier branch of the same name carried tracking only and was folded back; this re-cut cut a fresh one)

## Goal

The browser module builder lets the person building a Word module form choose
which numbers it prints: the default `1..n`, or the HiTOP-SR's own item
numbers. The printed number is the name a person uses to move a response off
the page, and at *original* those names are the field names the same page's
REDCap and Qualtrics exports give the collected variables, so hand-entering
paper responses into a project built from that page needs no translation. Both
settings produce a form scoreable from the paper alone, so the control never
trades away self-containment.

## Scope

The deliverable is **user-facing**: a control and its copy on a public web
page, plus one sentence on a package help page. Most of the code lives in
`jmgirard/hitop-builder`; the hitop branch carries the milestone record and the
help-page change, and the builder commit is pushed at merge (the M048 pattern).

**In:** a two-option *Word item numbering* group in the builder's `index.html`,
passing `renumber` to `generate_docx_hitopsr()` and to no other call; treating
an all-scales tick as the whole instrument — no `module` on any of the three
downloads, gated on an R-side check that the chosen items really are `1` to `N`
with no gaps, beside a source comment recording the assumption; naming an
all-scales download for the instrument rather than a module; a shuffle notice
conditional on both the numbering and the all-scales state, replacing today's
unconditional crosswalk claim (`index.html:121-126`), which
`R/generate_docx.R:290` falsifies in three of the four shuffled states; the
same correction plus a numbering section in the builder `README.md`; and one
sentence in `hitop_module()`'s help page on the module framing an all-scales
module still receives.

**Out:** naming a *shuffled* download distinctly → the standing M048 candidate
row, where the M048 gate put it; `include_scoring` → the standing M045 row;
the Qualtrics and REDCap naming controls and their guards → M050; `title`,
`font_size`, `font_family` → the standing candidate row; `include_subscales` →
nowhere, since the package errors when it is combined with `module`
(`R/generate_docx.R:191-199`). Blocking or warning original-plus-shuffle, and
any package-side collapse of an all-scales module → rejected at M049-D2/D3.

## Acceptance criteria

- [ ] AC1. In a browser loading the modified page, two named modules — one
      whose scales sit low in the instrument's numbering and one whose sit
      high, so the gapped case is exercised — each build a Word file in both
      numbering states with shuffle off. The numbers parsed out of each `.docx`
      are, at *original*, exactly that module's HiTOP-SR numbers ascending, and
      at the default `1` to `n` ascending; the expected sets come from
      `hitopsr_items` independently of the app, never back through the page's
      own scale-to-item mapping. Evidence: the four parsed lists and that
      expectation.
- [ ] AC2. `renumber` reaches the Word call alone. For one module, the
      Qualtrics `.txt` downloaded at *original* is byte-identical to the one at
      the default, and the two REDCap archives carry an identical
      `instrument.csv` (member content, not archive bytes — DOCX and REDCap
      zips are not byte-deterministic). Evidence: the four downloads compared.
- [ ] AC3. With shuffle ticked, the notice the page shows matches what the
      built Word file contains in each of the four combinations of the
      numbering control (default / *original*) and the selection (a module /
      every scale ticked). In the one combination that produces a crosswalk —
      default numbering on a module — the notice says the file carries one and
      the `.docx` contains it; in the other three the notice says it does not
      and the `.docx` does not, and wherever numbering is *original* the notice
      also says the printed numbers are the instrument's own and responses
      should be entered by item number. Evidence: the notice quoted in each
      state beside whether the crosswalk appears in that state's file.
- [ ] AC4. With every scale ticked the page passes no `module` on any of the
      three downloads: the Word header reads `HiTOP-SR (v1.0)` and the download
      is named for the instrument; with any scale unticked the header reads
      `HiTOP-SR Module (v1.0)` and the download is named for a module. The
      all-scales Qualtrics `.txt` and REDCap `instrument.csv` are identical to
      those the page produced before the change. The omission is gated on an
      R-side check that the chosen items are exactly `1` to `N` with no gaps —
      so a selection failing it still passes its module — beside a comment
      recording what that encodes, the 76 scales covering exactly items 1–405,
      and the date verified. Evidence: both headers parsed from their files,
      both download names, the two online comparisons, the gate's predicate
      returning `FALSE` on a module whose items are not `1..N`, the comment.
- [ ] AC5. The new control is a `fieldset`/`legend` group whose two inputs each
      carry an associated label, on the pattern of the existing `papersize` and
      `shuffle` groups, and appears as such in the page's accessibility tree.
      These named existing controls still behave after the change: the scale
      filter, Select all, Clear all, the item tally, the shuffle box and its
      notice, and each of the three download buttons. Evidence: the
      accessibility tree for the new group, and a run through each control named.
- [ ] AC6. `hitop_module()`'s help page gains a sentence saying that a module
      naming every scale is item-identical to the full instrument but still
      receives module framing from the Word generator — the `HiTOP-SR Module`
      header, and a crosswalk when shuffled — so an R caller wanting
      full-instrument framing passes no module. The claim is derived: a module
      naming all 76 scales is built with `randomize = TRUE` and its header and
      crosswalk read back out. Evidence: the rendered text and those readings.
- [ ] AC7. The builder `README.md` gains a section for the numbering control
      stating what it does, that it applies to the Word file only, and what
      happens when it is combined with shuffle; and its existing shuffle
      section's unconditional claim that a shuffled Word file carries a
      crosswalk is corrected to match. Both sections match what AC1–AC4
      verified and carry the verification date. Evidence: both sections quoted.

## Coverage

- AC1 → T1, T4
- AC2 → T1, T4
- AC3 → T3, T5
- AC4 → T2, T5
- AC5 → T1, T6
- AC6 → T7
- AC7 → T8

## Tasks

- [x] T1. In `jmgirard/hitop-builder` on a branch: add the *Word item
      numbering* fieldset (two radios, default = numbered `1..n`) beside the
      existing `papersize` and `shuffle` groups in `index.html`; bind its value
      and interpolate `renumber` as a literal `TRUE`/`FALSE` into the DOCX call
      only, mirroring how `randomize` is passed today (`index.html:296-306`).
- [x] T2. Treat an all-scales tick as the whole instrument: build the module
      object, ask R whether its items are exactly `1` to `N`, and on both
      conditions omit `module` from all three calls, from the log line, and
      from the download name (`hitopsr.<ext>` against `hitopsr-module.<ext>`).
      Carry the tiling comment at the check.
- [x] T3. Rewrite the shuffle notice so its text is conditional on the
      numbering state and on whether every scale is ticked, and wire it to
      update when either input or any scale checkbox changes.
- [x] T4. Verify AC1–AC2: serve the modified page locally, build both modules
      in both numbering states, parse each `.docx` for its printed numbers, and
      compare the Qualtrics and REDCap downloads across states.
- [x] T5. Verify AC3–AC4: the four shuffled states; the all-scales header,
      crosswalk and download name against a module's; the two online exports
      against the pre-change page; and the gate's predicate on a module that is
      not `1..N`.
- [x] T6. Verify AC5: read the page's accessibility tree for the new group and
      drive each named existing control.
- [x] T7. In `hitop`: add the `hitop_module()` help sentence (AC6) against a
      built document, run `devtools::document()`, and run the profile's checks.
- [ ] T8. Add the builder README's numbering section and the shuffle-section
      correction (AC7) with the verification date; open the hitop-side PR
      carrying the milestone record; push the builder commit at merge.

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
- 2026-08-23: implementation started; branch `m049-builder-numbering-toggle` cut fresh from `main`. Builder work on `jmgirard/hitop-builder` branch `m049-word-item-numbering`, pushed at merge (the M048 pattern).
- 2026-08-23: goal re-cut through /milestone-plan; every plan-owned section rewritten from the four inputs M049-D5 settled plus this gate's four answers. No code written, no branch cut.
- 2026-08-23: criteria audit ran in FULL mode (user-facing tier), inline rather than in a fresh-context subagent (session configured not to spawn agents). Three findings, all fixed before the criteria were written: a criterion had bundled `devtools::document()`/`check()`, which the profile's consistency-gate slot already owns (trimmed to the help-page text, verified against a built document); the builder README's existing shuffle section asserts unconditionally that a shuffled Word file carries a crosswalk, which the all-scales change makes false in three of the four shuffled states (pulled into AC7's scope); and AC3's domain is the four shuffle-ticked control combinations, not the three the amendment gate named (all four now enumerated).
- 2026-08-23: plan gate chose omitting `module` on all three downloads for an all-scales tick over omitting it on the Word call only; Word-only lost because it leaves two code paths and a download name saying "module" for one file and not another built from the same tick. Falsified by an online export differing between an all-scales module call and a no-module call, which the 2026-08-23 verification says it does not.
- 2026-08-23: plan gate chose an R-side check that the chosen items are exactly `1..N` over RR03 recommendation 4's source comment alone (the comment is kept beside the check); comment-only lost because the failure it guards against is silent and a comment can be skimmed past. Falsified by the check proving unaffordable in the page's webR round-trip, or by an instrument whose full selection is legitimately not `1..N`.
- 2026-08-23: plan gate chose naming a download by module-versus-whole-instrument only over also folding in the shuffled-versus-unshuffled naming now; the wider option lost because the M048 gate declined that same widening and the standing candidate row still holds it. Falsified by two forms being confused on disk, which is that row's own promotion condition.
- 2026-08-23: plan gate chose a two-radio *Word item numbering* group over a single tick box; the tick box lost because it names only one of the two numbering schemes and leaves the default unlabelled. Falsified by the radio pair failing the accessibility reading AC5 requires.

- 2026-08-23: T1-T3 done in `jmgirard/hitop-builder` (commit `aabfdf7`, unpushed): the *Word item numbering* fieldset with its hint line, `renumber` interpolated into the DOCX call only, the all-scales path dropping `module` behind an R-side tiling check asked once at load, and the shuffle notice's crosswalk sentence written per state.
- 2026-08-23: implementation gate settled three open items, all at the recommended option: the radio wording plus a hint line under the group; the notice stays tied to the shuffle box, so no criterion widens; and an all-scales tick that fails the tiling check falls back to passing its module and logs one line saying why.
- 2026-08-23: T7 done - `hitop_module()`'s help page gains the all-scales framing sentence, derived from a built document (all-76-scale module with `randomize = TRUE`: header `HiTOP-SR Module (v1.0)`, 405 crosswalk rows; no-module: header `HiTOP-SR (v1.0)`, 0 crosswalk rows; Qualtrics bytes and REDCap `instrument.csv` identical between the two calls). `devtools::document()` and `devtools::test()` clean (FAIL 0, WARN 0, SKIP 1, PASS 13794).
- 2026-08-23: T4 done - AC1/AC2 verified by driving the locally served modified page. Printed numbers: Difficulties Reaching Orgasm at default `1, 2, 3` and at original `82, 124, 151`; Binge Eating + Sexual Pain at default `1..6` and at original `238, 275, 344, 358, 392, 398`; both original lists strictly ascending and equal to the expectation taken from `hitopsr_items$Scale`, not from the page's own mapping. The same module's Qualtrics `.txt` came out byte-identical across the two numbering states and its REDCap `instrument.csv` identical.
- 2026-08-23: added a `hitop-builder` entry to `.claude/launch.json` beside the existing `pkgdown-docs` one, so the builder page can be served locally for verification here and at M050.

- 2026-08-23: T5 done - AC3/AC4 verified on the served page. Of the four shuffle-ticked states, only module-plus-default numbering produced a crosswalk (6 rows for the two-scale module; 0 rows in the other three), matching the notice shown in each. All-scales files: header `HiTOP-SR (v1.0)`, printed numbers `1..405` ascending, log line and download name carrying no module (`hitopsr.docx`/`.txt`/`.zip`); a module's header stayed `HiTOP-SR Module (v1.0)` with `hitopsr-module.<ext>`. The all-scales Qualtrics `.txt` came out byte-identical to the pre-change page's and the REDCap `instrument.csv` identical (406 rows each), captured by serving the pre-change `index.html` beside it. The tiling predicate returned `TRUE` on all 76 scales and `FALSE` on the two-scale module.
- 2026-08-23: T6 done - AC5 verified. The accessibility tree renders the new group as `generic "Word item numbering"` over `label "Number the items 1 to n"` / `radio "renumber"` and `label "Keep the HiTOP-SR's own item numbers"` / `radio "original"`, the shape the `papersize` group takes. Drove each named existing control: the filter (`Select all` retitled `Select all 1 shown`), Select all (76 of 76), Clear all (0 of 76, all three buttons disabled), the tally, the shuffle box and its notice, and each download button. A real click on the original-numbering radio switched the notice sentence.

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
