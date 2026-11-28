# RB03: A Word item-numbering control in the browser module builder (M049)

- **Date:** 2026-08-23
- **Output required:** write findings to `cairn/reviews/RR03-builder-word-numbering-control.md`
- **Binding criteria:** not requested

You are performing an independent expert review. This brief is fully
self-contained — do not assume any conversation context. Read only what this
brief directs you to read, answer the numbered questions, and write your
findings to the output path above using the same numbering.

## Background

**The package.** `hitop` is an R package that scores and distributes
questionnaire instruments for the HiTOP Society. One of them is the HiTOP-SR,
a 405-item self-report instrument whose items are grouped into 76 scales.
Three generator functions turn the instrument — or a chosen subset of its
scales, which this package calls a **module** — into a fieldable artifact:
`generate_docx_hitopsr()` writes a Word paper form, `generate_qualtrics_hitopsr()`
writes a Qualtrics import file, and `generate_redcap_hitopsr()` writes a REDCap
data-dictionary archive.

**Module numbering, as it stands.** A module drawn from scales scattered
through the instrument would print the instrument's own gapped numbers — 7,
42, 213 — which reads as an error to a participant filling in a page. The
maintainer signed that off as a positional-label question rather than an
instrument-content question, and `generate_docx_hitopsr()` gained a `renumber`
argument, default `TRUE`, that prints a module's items as `1..n` down the
page. The two online exports deliberately do *not* renumber: there an item
number names a collected data column, so renumbering would rename variables in
dictionaries already in the field. The reasoning is recorded in `D-036`
(quoted under Constraints below).

`generate_docx_hitopsr()` also has `randomize`, default `FALSE`, which prints
the items in a random order. A randomized *renumbered module* form additionally
prints a crosswalk — each printed number beside the original HiTOP-SR number it
came from — so the form can be scored from the paper alone. That crosswalk is
deliberately *not* printed in three cases, one of which is `renumber = FALSE`
(the printed numbers already are the original ones).

**The browser builder.** `jmgirard/hitop-builder` is a separate repository
holding a single static page (`index.html`) that runs R in the visitor's own
browser via webR, installs `hitop`, and lets a researcher tick scales and
download a Word, Qualtrics, or REDCap file for exactly those scales. It always
passes a module — even when every scale is ticked. It today exposes two Word
options: paper size, and a shuffle checkbox that maps to `randomize`. It does
not expose `renumber`, so every Word download it produces is renumbered `1..n`.

**The milestone under review.** M049 plans to add a *Word item numbering*
control to that page, passing `renumber` to the Word call and to nothing else,
plus a rewrite of the page's shuffle notice (which today claims unconditionally
that every shuffled Word file carries a crosswalk — false once numbering can be
set to original) and a README section. The milestone file is
`cairn/milestones/M049-builder-numbering-toggle.md`.

**Why this needs independent review.** At the implementation gate the
maintainer stated a priority that cuts against the milestone's own stated
reason for existing. Verbatim, from the gate:

> i dont think many people will ever collect data on paper (docx) and then try
> to score it with our functions. so it's more important to make sure the docx
> is self-contained and scoreable from what's included, rather than trying to
> make sure it's easy to map back to the original item numbers in the
> instrument. the main thing to be careful about is, if someone does check all
> boxes for all scales, then we shouldn't renumber. renumbering is only there
> to improve the UX of modules

M049's Goal section reads:

> The browser module builder lets the person building a Word module form keep
> the HiTOP-SR's own item numbers instead of the default `1..n`, so a paper
> form's printed numbers line up with the column names the same module's
> Qualtrics and REDCap exports produce.

That "so" clause — alignment with the online exports' column names — is
precisely the goal the maintainer just ranked below self-containment. A
milestone's Goal cannot be amended in place under this repo's tracking rules;
a wrong goal returns to planning. So the live question is whether the control
is still the right deliverable, and if so on what reasoning.

## Materials

Read these, in this order. Paths are relative to the `hitop` repository root
unless noted.

1. `cairn/milestones/M049-builder-numbering-toggle.md` — the whole file
   (goal, scope, five acceptance criteria, five tasks, work log).
2. `R/generate_docx.R`, lines 73–312 — the roxygen documentation and full body
   of `generate_docx_hitopsr()`. Note in particular:
   - lines 105–124: the `renumber` and `randomize` parameter docs, including
     the "Scoring data collected on a shuffled form" warning;
   - lines 208–221: the single printed-order map (`slot`, `item_order`,
     `printed`, `printed_of`) that drives the items table, the scoring table
     and the crosswalk;
   - lines 279–295: the three conditions gating the crosswalk.
3. `R/module.R` — `hitop_module()` and `apply_module()`, for how a module
   descriptor reduces the item and scale tables.
4. `../hitop-builder/index.html` (sibling repository, checked out at
   `/Users/jmgirard/github/hitop-builder`) — the whole file. Note:
   - lines 113–126: the existing shuffle `fieldset` and its notice text;
   - lines 277–331: `download()`, where `papersize` and `randomize` are bound
     and the R call string is assembled.
5. `cairn/DECISIONS.md`, entries `D-036` and `D-037` (quoted in part under
   Constraints).
6. `cairn/DESIGN.md`, line 94 (IP1).

**Facts already established, on `main`, 2026-08-23.** You may rely on these
without re-running them; re-run any you doubt.

- The 76 HiTOP-SR scales cover all 405 items with no gaps and no overlaps in
  coverage, so `hitop_module("hitopsr", scales = <all 76>)$items` is exactly
  `1:405`. Renumbering an all-scales module to `1..n` is therefore the
  identity: `attr(out, "item_order")` is `1:405` and the printed numbers are
  the instrument's own. **The maintainer's "if all boxes are checked we
  shouldn't renumber" is already satisfied numerically.**
- An all-scales module build nonetheless differs from a whole-instrument build
  in two ways: its Word header reads `HiTOP-SR Module (v1.0)` where a
  whole-instrument call gives `HiTOP-SR (v1.0)` (the `title = NULL` sentinel,
  `R/generate_docx.R:184-186`), and a *shuffled* all-scales module prints a
  405-pair crosswalk paragraph that the package explicitly declines to print
  for a whole-instrument call.
- The Qualtrics `.txt` is byte-identical between an all-scales module call and
  a whole-instrument call, and the REDCap `instrument.csv` inside the archive
  is identical line for line.

To reproduce any of the above:

```r
devtools::load_all(".")
all_sc <- available_scales("hitopsr")$camelCase
m <- hitop_module("hitopsr", scales = all_sc)
identical(as.integer(m$items), 1:405)
f <- tempfile(fileext = ".docx"); generate_docx_hitopsr(file = f, module = m)
source("tests/testthat/helper-generators.R"); docx_header_title(f)
```

**Decided already, and not under review.** At the same gate the maintainer
chose that the *page* will treat an all-scales tick as the full instrument —
passing no module at all when every scale is ticked, so the header reads
`HiTOP-SR` and a shuffled all-scales build prints no 405-pair crosswalk.
Question 4 asks where that logic belongs, not whether it happens.

## Questions

1. **Does a Word item-numbering control belong on this page at all?** Weigh at
   least these three, and name any fourth you prefer: (a) ship the planned
   two-state control, defaulting to renumbered; (b) drop the control, leave
   the page always renumbering module forms, and close M049 by re-cutting it
   around the all-scales behavior instead; (c) drop the control from the page
   but keep `renumber` as an R-caller argument only. Answer against the
   maintainer's stated priority — a paper form being scoreable from what it
   prints, over its numbers mapping back to the instrument — and say
   explicitly whether that priority *kills* the control's rationale or merely
   weakens it. If your answer is (a), supply the reasoning the Goal should
   rest on instead, since the planned one no longer holds.

2. **Is there a real use for original numbering on a browser-built module form
   that survives the maintainer's priority?** Consider at minimum: a
   researcher fielding the same module on paper and online who wants one
   codebook; a researcher hand-entering paper responses into a REDCap project
   built from the same page; longitudinal comparison against data collected on
   a pre-module full-instrument administration; and any use you can identify
   that this brief has not named. Say for each whether it is speculative or
   grounded in what the package's own documentation already promises.

3. **What should happen when original numbering is combined with shuffle?**
   Under `renumber = FALSE, randomize = TRUE` the package prints no crosswalk
   and the items appear with the instrument's own numbers in scrambled order
   (e.g. 312, 45, 200). The scoring key on the same document lists those same
   original numbers, so the form is internally consistent. Is such a form
   "self-contained and scoreable from what's included" by the maintainer's
   standard? If not, should the combination be blocked in the page, warned
   about in the page, warned about in the package, or left alone — and does
   any of those cross into instrument content rather than page behavior?

4. **Where does the all-scales reconciliation belong?** The page will stop
   passing a module when every scale is ticked. Should that live in the page
   (`index.html` compares selected count to total), or package-side — for
   instance `hitop_module()` or `apply_module()` recognizing a module that
   covers the whole instrument, or `generate_docx_hitopsr()` resolving the
   header and crosswalk by item coverage rather than by `is.null(module)`?
   State the failure mode each placement leaves open. Note that a package-side
   change would alter output for existing R callers who pass an all-scales
   module today.

5. **Does anything you recommend need fresh maintainer sign-off under IP1?**
   D-036 and D-037 each closed with "the next artifact-touching change returns
   to the gate". Identify precisely which of your recommendations, if any,
   change participant-facing printed text or the artifacts under
   `inst/extdata/`, and which are page behavior only. A recommendation that
   changes no printed token in any distributed artifact should say so plainly.

6. **Beyond the brief.** Anything in the materials that is wrong, unsafe, or
   about to become wrong — in the package, in the page, or in M049's
   acceptance criteria as written.

## Constraints

Fixed; flag disagreement explicitly rather than working around it.

- **IP1 (inviolable).** "Instrument content is sacrosanct. Keying tables,
  scale memberships, item text, response options, and administration
  instructions — wherever they live (`*_items`/`*_scales`, `R/sysdata.rda`,
  generated DOCX/Qualtrics/REDCap artifacts) — change only with maintainer
  sign-off against the authoritative source… A change to participant-facing
  text is a sourced content change, not a style fix."
- **D-036 stands.** A module Word form's item numbers are positional labels,
  not instrument content, and `renumber = TRUE` is the default. The online
  exports are deliberately excluded from renumbering, because there an item
  number names a collected data column. Do not relitigate either. D-036 also
  records that keeping original numbers behind an opt-in argument was
  *rejected at that gate* — on the ground that the browser builder would then
  need its own change before anyone saw the fix. M049 is that change; whether
  D-036's rejection reasoning has anything left to say about it is fair game
  for question 1.
- **D-037 stands.** A module Word form is headed `HiTOP-SR Module (v1.0)`; a
  full-instrument form keeps `HiTOP-SR (v1.0)`. An explicitly supplied `title`
  is honored unchanged.
- **No shipped artifact under `inst/extdata/` may change.** Every distributed
  form is the full instrument.
- **The page is a static single file** with no build step and no test suite; it
  runs R in the visitor's browser. Recommendations that require app-side
  tooling should say so.
- Your report is **advisory**. Recommend; do not impose acceptance criteria.

## Output format

In `RR03-builder-word-numbering-control.md`: answer each question by number
with your reasoning and evidence; list any additional findings separately under
"Beyond the brief"; end with concrete recommendations, each marked apply /
consider / reject-with-reason. Your report is advisory: emit no
`## Binding criteria` section — this brief does not request one.
