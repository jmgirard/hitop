# RR03: A Word item-numbering control in the browser module builder (M049)

- **Date:** 2026-08-23
- **Brief:** `cairn/reviews/RB03-builder-word-numbering-control.md`
- **Status:** advisory; no binding criteria requested

Materials read in the order the brief lists them: the M049 milestone file;
`R/generate_docx.R:73-312`; `R/module.R`; `/Users/jmgirard/github/hitop-builder/index.html`
in full; D-036 and D-037 in `cairn/DECISIONS.md`; IP1 in `cairn/DESIGN.md`. I
additionally confirmed in `R/generate_redcap.R:233-238` that the REDCap
dictionary names each field `hitopsr_` plus the zero-padded **original** item
number, which several answers below lean on.

## 1. Does the control belong on the page at all?

**Recommendation: (a), ship the planned two-state control, defaulting to
renumbered — on a re-cut Goal.** The maintainer's priority weakens the
milestone's stated rationale enough that the Goal must return to planning, but
it does not kill the control, for one decisive reason: **the two goals are not
in conflict for any form this control can produce.** Self-containment is a
property of what the document prints beside its own scoring key, and the key is
built from the same printed-order map as the items table
(`R/generate_docx.R:212-219`, the single `printed`/`printed_of` map), so:

- `renumber = TRUE`, no shuffle: printed `1..n`, key lists `1..n`. Self-contained.
- `renumber = FALSE`, no shuffle: printed original numbers, key lists original
  numbers. Self-contained.
- `renumber = TRUE`, shuffled: crosswalk printed. Self-contained by design.
- `renumber = FALSE`, shuffled: printed original numbers in scrambled order, key
  lists those same numbers; a scorer finds item 312 wherever it printed. Self-
  contained (see question 3).

A priority ranking only bites when the ranked things trade off. Here choosing
original numbering costs nothing in scoreability-from-the-paper, so "self-
containment outranks mapping back" cannot rule the control out; what it rules
out is *justifying* the control primarily as a mapping-back convenience — which
is exactly what M049's Goal does. So: the priority **kills the Goal's "so"
clause as the load-bearing rationale, and merely weakens the control**, which
stands on a different foot.

The reasoning the Goal should rest on instead: **the printed number on a paper
module form is the name a human uses to move a response off the page, and the
control lets the researcher choose which naming scheme that is.** Concretely:
the same page's REDCap export names every collected field by the original item
number (`R/generate_redcap.R:238`), and Qualtrics likewise. A researcher who
builds a REDCap project and a paper form for the same module from this page,
then hand-enters paper responses into that project, today reads "item 14" off
the paper and must map it to `hitopsr_213` with no aid the page gives them.
With the control at *original*, entry is a direct read-off. That is not
"mapping back to the instrument" for its own sake; it is the paper form and
the data-entry target speaking one language, which serves the maintainer's
actual concern — data moving off the paper correctly — rather than opposing
it. The default stays renumbered, preserving "renumbering is only there to
improve the UX of modules" as the common case.

Against the alternatives:

- **(b) drop the control, re-cut M049 around the all-scales behavior.** This
  discards a two-state package argument that D-036's gate already signed off
  and shipped, reachable by nobody the builder serves (the builder's audience
  is precisely people not at an R console). Note also that D-036's rejection
  of "original numbers behind an opt-in argument" was a rejection of that as
  the *default posture*, on the ground that the browser builder would need its
  own change before anyone saw the renumbering fix. M049 *is* that builder
  change, with the default unmoved — so D-036's rejection reasoning has
  nothing left to say against it; if anything it anticipated this milestone.
- **(c) drop the control from the page, keep `renumber` R-side.** This is the
  status quo (the argument already exists). It leaves the hand-entry use in 1
  unserved for the page's audience and serves nobody better than (a) does.
- **(d) a fourth option, named for completeness:** ship the control but have
  the page couple it to shuffle (disable or warn on the *original* + shuffle
  combination). I considered and rejected the disabling half — question 3
  finds the combination coherent — but the warning half is just M049's own
  conditional notice, already planned as T2/AC3.

One honest caveat: if the maintainer's position is stronger than the quoted
priority — not "self-containment outranks alignment" but "nobody will ever
want original numbers on paper" — then (b) is right and the control is
speculative surface area. The plan gate's own falsifier ("a report that the
original-numbering form is never wanted in practice", work log 2026-08-23)
already names this. The maintainer's quote does not go that far; it ranks, it
does not zero out. On the quote as given, (a) stands.

## 2. Real uses for original numbering that survive the priority

- **Hand-entering paper responses into a REDCap project built from the same
  page.** Grounded, and the strongest use. The package's own REDCap generator
  names each field by the original number (`R/generate_redcap.R:233-238`), and
  its documentation gives that as the reason the online exports never renumber
  (`R/generate_redcap.R:66`). Hand entry from paper into REDCap forms is a
  routine workflow; with a renumbered form every keyed item passes through a
  1..n → original translation the page currently provides no aid for. This use
  is entirely about data leaving the paper correctly, so it survives — indeed
  serves — the maintainer's priority, and it does not involve scoring paper
  data with the package's functions at all.

- **Fielding the same module on paper and online with one codebook.**
  Grounded in the package's documented numbering contrast (`renumber` docs,
  `R/generate_docx.R:103-109`) and recorded as the plan-gate rationale
  (M049 work log, 2026-08-23). Frequency is speculative — the maintainer
  doubts many people collect on paper — but the use is real for whoever does,
  and it is operationally the same fact as the hand-entry use: one variable
  naming scheme across modes.

- **Longitudinal comparison against a pre-module full-instrument
  administration.** Mostly speculative as a *printed-number* use: the
  alignment that matters there is at the data-column level, which the online
  exports and `score_hitopsr()`'s ascending-original `module$items` contract
  (`R/module.R:170-186`) already preserve regardless of what any paper form
  printed. The printed numbers matter only at entry time, so this use reduces
  to the hand-entry case above rather than adding to it.

- **Uses the brief did not name.** (i) Verification of a fielded form against
  the canonical instrument — an IRB, a translator, or a Society reviewer
  checking item-by-number that the module form matches HiTOP-SR v1.0;
  original numbers make that a line-by-line read. Speculative but plausible,
  and adjacent to IP1's own concern that instrument content be checkable
  against its source. (ii) Item-level reporting in a paper ("HSR item 213")
  citable against the instrument. Speculative. Neither carries the milestone
  alone; both come free with it.

## 3. Original numbering combined with shuffle

**The combination is self-contained and scoreable from what's included, by the
maintainer's stated standard.** The scoring key lists original numbers
(`printed_of` maps through the same `printed` vector, which under
`renumber = FALSE` *is* `item_order`, `R/generate_docx.R:218`), and every item
on the page carries its original number. A human scorer needs nothing outside
the document: find the numbers the key names, average them. The absent
crosswalk is not a gap — every row of it would read "42 → 42", which is the
code comment's own reasoning (`R/generate_docx.R:282-284`). Arguably this
configuration is the *most* self-contained shuffled form the package makes,
because even the printed order is recoverable from the page itself (read the
numbers top to bottom), whereas a renumbered shuffled form depends on its
crosswalk paragraph surviving.

The residual hazard is not scoring from the paper but **column order at data
entry**: someone who types responses into an unnamed spreadsheet in printed
order gets non-ascending columns, and `score_hitopsr()` addresses positions in
ascending `module$items`. That hazard is identical for the renumbered shuffled
form (the package's own warning at `R/generate_docx.R:121-126` covers it) and
is *smaller* here, since the natural entry target — fields named by original
number — self-corrects.

Therefore: **do not block the combination anywhere, and add no package-side
warning.** The package already documents the behavior precisely; a runtime
warning would nag a coherent, documented configuration. The right instrument
is the one M049 already plans: the page's conditional shuffle notice (T2/AC3)
saying, in the *original* state, that the file carries no crosswalk because
the printed numbers are the instrument's own, and that responses should be
entered by item number. None of this crosses into instrument content: the
notice and README are researcher-facing page copy, not participant-facing text
and not a distributed artifact, so IP1 is not engaged. Blocking the
combination *package-side* would be a behavior change to a signed-off
generator and would need its own gate — a further reason not to.

## 4. Where the all-scales reconciliation belongs

**The page.** The datum being acted on is a user gesture — "I ticked every
box, meaning the whole instrument" — and only the page can see intent; the
package sees an item set. `index.html` comparing selected count against
`scales.length` and omitting `module` from the call is a faithful encoding of
the maintainer's statement, which was about the page's users ("if someone does
check all boxes...").

Failure modes each placement leaves open, stated per the brief:

- **Page-side** (recommended): the page hardcodes an equivalence — all 76
  scales = all 405 items — that is an empirical fact of today's `hitopsr`
  tables (verified on main 2026-08-23), enforced nowhere. It holds for the
  HiTOP-SR by construction, but if the builder ever gains an instrument whose
  scales do not tile its items (overlap, or items outside every scale), "all
  scales ticked" would silently stop meaning "full instrument", and passing no
  module would field items the user never selected. Mitigation: a comment at
  the comparison recording the assumption and the date it was verified, so the
  multi-instrument builder (the standing generalization candidate) trips over
  it deliberately. Second, smaller mode: the page and an R caller now diverge
  for the same selection — an R user passing an all-scales module still gets
  the `Module` header and, shuffled, the 405-pair crosswalk. That divergence
  is acceptable (the R caller asked for module framing explicitly and gets
  it), but it is a fact worth one documentation sentence (see Consider below).

- **`hitop_module()`/`apply_module()` recognizing full coverage** (e.g.
  returning `NULL` or a flagged object): changes the semantics of an explicit
  descriptor — `print.hitop_module()` would describe an object the generators
  then ignore, and `score_hitopsr()`'s module path would either diverge from
  the generators' or change too. It alters output for existing R callers who
  pass an all-scales module today (header token, crosswalk presence), which is
  a participant-facing printed-text change requiring a fresh IP1 gate and a
  D-entry superseding part of D-037's `is.null(module)` resolution. High cost,
  and it moves a UI-intent judgment into a data structure.

- **`generate_docx_hitopsr()` resolving header/crosswalk by item coverage**:
  the most defensible package-side variant ("a form containing all 405 items
  *is* the instrument" is a reading of D-037's 'resolves by what the form
  contains'), but it has the same existing-caller output change and the same
  gate requirement, plus it couples the generator to the tiling property of
  the tables. It also leaves the online exports' behavior untouched while
  changing the Word form's, adding a second cross-format asymmetry to explain.

- **Failure mode the page placement must still handle** (flagged under
  question 6 and Beyond the brief): once the page passes no module for an
  all-scales tick, a *shuffled* all-scales build prints no crosswalk — so the
  conditional shuffle notice must condition on this state too, not only on the
  numbering control.

## 5. Fresh IP1 sign-off

- **The numbering control itself: no fresh sign-off needed, and I say so
  plainly — it changes no printed token in any distributed artifact.** Every
  document the control can produce is one `generate_docx_hitopsr()` already
  produces under arguments D-036's gate created and signed off; the page
  merely reaches an existing, documented, gated argument. Nothing under
  `inst/extdata/` changes (every shipped artifact is the full instrument and
  the page passes no `renumber` distinct from the default for that case after
  the all-scales change). "The next artifact-touching change returns to the
  gate" is not triggered because M049, cut as planned plus the page-side
  all-scales change, touches no artifact and no participant-facing text —
  the shuffle notice and README are researcher-facing web copy.
- **The re-cut Goal**: returns to planning under the tracking rules, and the
  plan-gate re-approval is itself the maintainer decision that resolves this
  brief's question 1. That is process sign-off, not IP1 sign-off.
- **The all-scales reconciliation, placed page-side**: page behavior only; the
  full-instrument document it produces is byte-for-byte the package's existing
  signed-off no-module output. No gate. Had it been placed package-side
  (either variant in question 4), it **would** change the header token and
  crosswalk presence on existing callers' output and would return to the gate
  with a new D-entry; that is a further reason the page placement is right.
- **The Consider item below touching `hitop_module()` documentation**: R help
  text for researchers, not participant-facing, no printed token in any
  artifact. No gate.

## 6. Beyond the brief

1. **The shuffle notice's crosswalk claim is falsified by the all-scales
   decision alone, independently of the numbering control.** Once the page
   stops passing a module when every scale is ticked (decided, not under
   review), a shuffled all-scales build prints no crosswalk
   (`R/generate_docx.R:290`, the `!is.null(module)` gate) — but the current
   notice (`index.html:121-122`) says "Every shuffled Word file carries a
   crosswalk", and M049's AC3 as written conditions the notice only on the
   numbering state. The notice's conditionality is two-dimensional: numbering
   × all-scales. Whatever amendment the deferred all-scales scope change takes,
   AC3 (or its successor) must cover the shuffled-all-scales state, or the page
   ships a notice that is false in exactly the configuration the maintainer
   flagged at the gate.

2. **The download filename and log line misdescribe the all-scales build.**
   After the decided change, an all-scales download still saves as
   `hitopsr-module.docx` (`index.html:316`) and logs
   `module = <76 scales>` (`index.html:303`) while the built document is
   headed `HiTOP-SR (v1.0)` and the call passes no module. Small, but it is
   the same class of mismatch D-037 fixed in the header. Fits the standing
   M048 candidate row (naming a download from the options that produced it)
   or the all-scales amendment itself.

3. **AC1 could pin printed order, not just the number set.** "the printed item
   numbers ... are exactly that module's HiTOP-SR numbers" is satisfiable by
   the right numbers in the wrong order; with shuffle off they must also be
   ascending. One word ("in ascending order") closes it. Advisory — I am not
   imposing criteria.

4. **`R/generate_docx.R` docs and code agree on the crosswalk gates.** I
   checked the three documented conditions (lines 110-119) against the code
   (line 290: `randomize && renumber && !is.null(module)`); they match, and
   the scoring table provably uses printed numbers in both numbering states
   via the single `printed_of` map. No defect found in the package materials
   the brief directed me to.

5. **Injection surface of the planned T1 wiring: none, if it truly mirrors
   `randomize`.** The existing pattern interpolates only a JS-side boolean
   into the R string (`index.html:300-302`) and binds user-influenced values
   (`selected_scales`, `paper_size`, `out_path`) through `globalEnv.bind`.
   The `renumber` value should likewise be interpolated only as a literal
   `TRUE`/`FALSE` derived from the checkbox, never as free text. T1's
   "mirroring how `randomize` is passed" already says this; keep it that way.

## Recommendations

1. **Apply.** Ship the two-state Word item-numbering control, default
   renumbered (option a), with M049's Goal re-cut at a plan-gate pass to rest
   on the printed number as the name a human uses to move a response off the
   page: with the control at *original*, the paper form's numbers are the same
   names the page's own REDCap and Qualtrics exports give the collected
   variables, so hand entry and mixed-mode codebooks need no translation.
   Subordinate clause worth keeping in the Goal: both numbering states produce
   a form scoreable from the paper alone, so the control never trades away
   self-containment.

2. **Apply.** Leave the *original* + shuffle combination available; cover it
   with the conditional shuffle notice M049 already plans (no crosswalk, the
   printed numbers are the instrument's own, enter responses by item number).
   No package change, no blocking.

3. **Apply.** Make the shuffle notice conditional on the all-scales state as
   well as the numbering state, and ensure the deferred all-scales scope
   amendment (or AC3) covers the shuffled-all-scales / no-crosswalk case.
   Without this the page ships a false notice in a decided configuration.

4. **Apply.** Place the all-scales reconciliation in the page (selected count
   vs total, omit `module`), with a source comment recording the tiling
   assumption — all 76 scales are exactly items 1–405, verified 2026-08-23 —
   so the future multi-instrument builder revisits it deliberately.

5. **Consider.** Rename the download and its log line when no module is passed
   (e.g. `hitopsr.docx`, and a log line without `module =`), here or in the
   standing M048 candidate row.

6. **Consider.** One sentence in `hitop_module()`'s documentation noting that
   a module naming every scale is item-identical to the full instrument but
   still receives module framing from the Word generator (Module header;
   crosswalk when shuffled), so R callers who want full-instrument framing
   pass no module. Documentation only; no output change; no gate.

7. **Reject: blocking or package-warning the *original* + shuffle
   combination.** The form is self-contained and internally consistent; the
   package documents the behavior; a block removes a coherent configuration
   and the package-side variant would be a gated behavior change with no
   defect behind it.

8. **Reject: package-side collapse of all-scales modules (in
   `hitop_module()`, `apply_module()`, or coverage-based resolution in
   `generate_docx_hitopsr()`).** It changes the header token and crosswalk
   presence for existing R callers' output, which is participant-facing
   printed text requiring a fresh IP1 gate and a partial supersession of
   D-037, to encode a judgment about user intent that only the page can
   actually observe.
