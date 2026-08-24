# M053: A format-first flow for the browser module builder

- **Status:** review
- **Priority:** normal
- **Depends on:** M052
- **Driving RR:** —
- **Principles touched:** —
- **Branch/PR:** `m053-builder-format-first-flow` (tracking) → https://github.com/jmgirard/hitop/pull/59; builder repo `m053-format-first-flow` → https://github.com/jmgirard/hitop-builder/pull/4

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

- [x] AC1: For each of the three formats, a file built from the branch page has
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
- [x] AC2: On the third step, the focusable form controls the page renders
      under each format — enumerated by walking the shown step with real Tab
      presses, once per format — are exactly: for Word, the paper-size radios,
      the numbering radios and the shuffle checkbox; for Qualtrics, the block
      name and question ID prefix boxes; for REDCap, the form name box and the
      required checkbox. No control in any of the three enumerations sets a
      value another format's generator call reads.
- [x] AC3: From a completed download, a second format is built for the same
      scales without re-ticking any scale: driving that path returns a file
      whose scale content matches the first file's, verified by reading both
      files back.
- [x] AC4: Every step boundary the page defines is crossable by keyboard alone
      in both directions where the page offers both, each arrived-at step's
      heading takes focus and paints the focus indicator M051 established, and
      the download control stays unavailable while no scale is ticked, with a
      rendered hint saying why. Querying the rendered DOM for elements carrying
      `aria-live` or a role with an implicit live value returns exactly the
      status line (polite) and the log pane (`off`), whichever step is shown.
- [x] AC5: The status line, the log pane, the two opening paragraphs and the
      back-link to the package documentation render outside the steps, and each
      one's rendered text is unchanged from the deployed M052 page.
- [x] AC6: At viewport widths of 360, 768 and 1280 CSS pixels, in both colour
      schemes, every step renders its own controls unclipped — each control's
      bounding rectangle lies within its container's — and the page's document
      scrolls vertically only (`documentElement.scrollWidth <= clientWidth`).
- [x] AC7: Every control-group title the page renders, read out of the elements
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
- [x] T4: Lay out each step at 360, 768 and 1280 CSS pixels in both schemes;
      run the keyboard walk across every boundary, the focus-indicator check,
      the live-region re-read and the horizontal-overflow check.
- [x] T5: Verify behaviour: rebuild every AC1 matrix cell from the branch page
      and compare against the T1 baseline; run the per-format control
      enumeration; drive the two-format path and read both files back. Show
      each detector able to fail on a planted defect before trusting a clean
      result.
- [x] T6: Rewrite `README.md`'s *What the page shows* section to walk the new
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

- 2026-08-24: T4 — 6 viewport cells (360/768/1280 CSS px × light/dark) × 5 step-and-panel views × 221 controls = 1326 containment checks, 0 clipped, and `documentElement.scrollWidth <= clientWidth` on every view. The containment check was first blind: a `fieldset` grows with an oversized child and document scroll was sampled once at the end, so a planted 3000px text box passed; after adding a page-column comparison and per-view scroll sampling the same planted defect goes red on four controls and on scroll width, and clean on revert. Tab walk: 89 focus events on step one (80 in-step, 9 page-level), 5 on step two, and per format on step three exactly the paper-size radios, numbering radios and shuffle checkbox (Word), block name and question ID prefix (Qualtrics), form name and required checkbox (REDCap); every one of the walk's focus events painted a solid 3px ring, and each arrived-at step's heading took focus with the 3px/4px-offset heading ring. Live-region query returned exactly `#status` (polite) and `#log` (`off`) on all three steps. Download button disabled with no scale ticked and a rendered hint saying why, enabled at one, disabled again at none. AC5's four page-level regions carry identical rendered text on the branch page and the deployed page, all outside the steps. Step three at 360 CSS px measures 778px (Word, shuffle off), 633px (Qualtrics) and 621px (REDCap) against an 800px viewport, so the plan gate's one-screen falsifier did not fire.
- 2026-08-24: the browser harness delivers `keydown` to the page but not the key's default action — a real space press on a focused checkbox left it unchecked — so step boundaries were reached by real Tab presses and then activated through the focused element, which is what Enter's default action does. Noted for review as a limit of the instrument, not of the page.

- 2026-08-24: T5 — all 16 AC1 matrix cells rebuilt from the branch page and compared field by field against the T1 baseline: 112 compared fields, 0 differences, both pages `hitop` 0.2.0. AC3 path driven end to end: two scales ticked once, Word built, "Choose a different format" then REDCap built with the ticks untouched (2 still checked, recap unchanged); the two files read back carry the same 10 items in the same order. Every detector shown able to fail on a planted defect — a Word cell built on A4 instead of US Letter differs from the baseline on `w:pgSz` alone while the control cell matches on all five fields; a third scale ticked between the two AC3 builds takes the item comparison to 10 against 13 and not identical; an extra text box planted in the Qualtrics panel appears in the real-Tab enumeration and goes when it is removed; a planted `role="alert"` appears in the live-region query and goes when it is removed.

- 2026-08-24: T6 — `README.md`'s *What the page shows* rewritten to walk the three steps in page order, naming each format's own settings group; the three stale *Under **Set options*** references in the numbering, naming and shuffling sections re-pointed at the format screens, and the naming bullets renamed to the labels the page now renders. All 7 control-group titles the page renders (5 legends plus the scale-list group label and the filter box label) appear verbatim in the README; the check reports a title absent when given one the README does not carry, and the retired *Word paper size* is gone from both. Builder commit `1b78de9`.

- 2026-08-24: all six tasks done. `cairn_validate` all checks passed (20 pre-existing advisories about legacy D-ids); `devtools::test()` 0 failures, 0 warnings, 1 skip, 13897 passing, with no R source changed on this branch. Status set to review.

- 2026-08-24: review gate — finding 1 fixed on the branch (builder `e2c0600`) and the affected checks re-run clean; findings 2-5 rejected with reasons in the Review section. Merge approved by the maintainer at the chip.
- 2026-08-24: review — all seven criteria verified fresh against a same-session capture from the deployed M052 page; consistency gate clean; three lenses inline, five findings, none a criterion failure. Two harness defects found and corrected mid-review before their results were trusted: a `<w:tr` split that also matched `<w:trPr`/`<w:trHeight` (Word cells re-run on both pages) and a containment check blind to `#scales`'s own scroll box. Acceptance boxes were ticked in one pass as the Review section was written, not one at a time; every tick has its evidence line in the same commit.

## Decisions

## Review

Evidence gathered fresh on 2026-08-24 against the branch page served from the
builder repo's working tree (`index.html` sha256 `65bd63d2…`, identical to
branch HEAD) and the deployed page (`5ebe25f0…`, byte-identical to the builder
repo's `origin/main`). Both pages report `hitop` 0.2.0 and 76 scales, printed on
load in the second opening paragraph.

- AC1 — 16/16 matrix cells identical. The eight Qualtrics and REDCap cells were
  compared on a 17-field canonical digest (Qualtrics: file name, byte length,
  whole-file SHA-256; REDCap: file name, zip entry list with uncompressed sizes,
  CSV name, line count, whole-`instrument.csv` SHA-256, header line, first entry
  line, last entry line) — 0 mismatches, the digest shown deterministic by
  recomputation on both pages. The eight Word cells were re-run on both pages
  after the first parse was found unsound: splitting `word/document.xml` on the
  bare string `<w:tr` also matches `<w:trPr`/`<w:trHeight`, which inflated a
  13-row form to 39 rows of mostly empty text (the same class of defect T1
  recorded for `<w:t>`; the fault was in this review's harness, not in the
  page). Re-run with the tag boundary anchored, all eight cells match on nine
  fields — file name, `w:pgSz`, header text, row count, a SHA-256 over the
  joined printed rows, first/second/last row and an empty-row count of 0 — with
  13 rows for the two-scale module (legend, ten items, crosswalk) and 445 for
  every scale. Detector shown able to fail: a Word cell built on A4 against the
  US Letter baseline differs on `pgSz` alone while the control rebuild differs
  on nothing.
- AC2 — real Tab presses through the shown step, once per format, enumerate
  exactly: Word, the paper-size radio group, the numbering radio group and the
  shuffle checkbox (three tab stops; a radio group is one stop and its second
  radio is reached by arrow key); Qualtrics, the block name and question ID
  prefix boxes; REDCap, the form name box and the required checkbox. No
  enumeration contains a control belonging to another format. The
  "sets no value another format's generator call reads" clause was executed as
  well as read: for each format, a file built with every *other* format's
  controls at their defaults and one built with those same controls moved to
  non-default values (A4, original numbering, shuffle on, `Wave 2 Screening` /
  `W2SCR` / `wave2_screening`, required off) are identical on every compared
  field. Detector shown able to fail: an extra text box planted in the Qualtrics
  panel appears in the Tab enumeration and goes when it is removed.
- AC3 — two scales ticked once, Word built, then "Choose a different format" →
  REDCap built with the ticks untouched: 2 ticked before, between and after,
  recap "2 of 76 scales selected — 10 items." unchanged, and both files read
  back carry the same ten item texts in the same order. Detector shown able to
  fail: a third scale ticked between the two builds takes the comparison to 10
  items against 13 and not identical.
- AC4 — all six step boundaries crossed from a control reached by real Tab
  presses: step 1 → 2 ("Continue to the format"), 2 → 3 (a format card),
  3 → 2 ("Choose a different format"), 2 → 1 ("Back to scales"), 1 → 3 (the
  step bar) and 3 → 1 (the recap's "Change the selection"). Every arrived-at
  step focused its own `h2`, each painting `solid 3px` at `4px` offset in
  `rgb(156, 203, 240)`, and every control on the walk painted the same ring at
  `2px` offset. The download button is disabled with 0 scales ticked, with the
  rendered hint "The button builds the file in this browser and saves it. It
  turns on once at least one scale is ticked."; enabled at 1; disabled again at
  0. The live-region query returns exactly `#status` (polite) and `#log`
  (`off`) on each of the three steps, and a planted `role="alert"` appears in it
  and goes when removed. **Instrument limit, carried from T4 and re-confirmed
  here:** this browser harness delivers `keydown` to the page but performs no
  default action for Enter or Space (a real Enter on the focused "Continue to
  the format" button left the page on step 1), and it ignores the shift
  modifier on Tab. Activation was therefore driven through the Tab-reached
  element, which is what Enter's default action does on a native button. The
  substitute evidence for the untestable half: every boundary control is a
  native `<button type="button">`, the file contains no `role="button"`, no
  `tabindex` other than `-1` on the three headings, and no `keydown`/`keypress`/
  `keyup` handler at all, so activation cannot be mouse-only.
- AC5 — the status line (`aria-live="polite"`), the log pane (`aria-live="off"`),
  the two opening paragraphs and the back-link all render outside every
  `.step`, and their rendered text is character-identical between the branch and
  deployed pages: both opening paragraphs verbatim, the link text
  "← hitop package documentation" with `href` `https://jmgirard.github.io/hitop/`,
  and the status line reading "Ready." at rest on both.
- AC6 — 6 cells (360, 768, 1280 CSS px × light, dark) × 5 views (step 1, step 2
  and step 3 under each of the three formats) × 208 controls = 1248 containment
  checks, 0 clipped, and `documentElement.scrollWidth <= clientWidth` on all 30
  views. The containment check was first blind in a second way: `#scales` is a
  `max-height: 416px; overflow-y: auto` list, so every row below its fold read
  as clipped until the check measured against a scroll container's scroll
  extent rather than its viewport rect. Detector shown able to fail: a 3000px
  text box planted in the Qualtrics panel goes red on five page-column
  violations and on document scroll width (3042 > 360) and clean on revert —
  and the container half alone does not catch it, which is why the page-column
  comparison is there.
- AC7 — all six control-group titles the page renders (the five `legend`
  elements "Paper size", "Item numbering", "Item order", "Block and question
  naming", "Form name and required items", plus the scale list's
  `role="group"` label "HiTOP-SR scales") appear verbatim in `README.md`, as
  does the filter box's `aria-label` "Filter the scale list by name"; the four
  retired M052 titles ("Word paper size", "Word item numbering", "Word item
  order", "Qualtrics and REDCap naming") appear nowhere in it. The README's
  *What the page shows* section walks the steps as Choose scales → Choose a
  format → Options and download, the order of the step bar and of the three
  `section.step` blocks. Detector shown able to fail: titles the README does not
  carry ("Colour scheme", "Font size") are reported absent.

Plan-gate falsifier, not a criterion: step 3 at 360 CSS pixels measures 778px
(Word, shuffle off), 633px (Qualtrics) and 621px (REDCap) against an 800px
viewport, so "a download step that grows past one screen" did not fire.

**Consistency gate.** `cairn_validate.py` exit 0, all checks passed, 20 advisory
warnings — the pre-existing legacy `D-001`…`D-012` id tokens, unchanged by this
milestone; the `release window` advisory did not fire. No `DESIGN.md` principle
changed, so `cairn_impact.py` was skipped. Toolchain checks from the
`r-package` profile's `consistency-gate` slot: `devtools::document()` produced
no diff; `devtools::test()` 0 failures, 0 warnings, 1 skip, 13897 passing;
`devtools::check()` 0 errors, 0 warnings, 0 notes; `pkgdown::check_pkgdown()`
no problems; `README.Rmd`/`README.md` untouched on this branch and in sync;
no `NEWS.md` entry due, the R package having no user-visible change here (the
diff in this repo is tracking files only); no new top-level file, so no
`.Rbuildignore` entry due.

**Independent review.** The declared surface tier is user-facing, so the full
three-lens fan-out applied. All three lenses ran inline in full mode, not in
fresh-context subagents — this session is configured not to spawn agents, as at
M048–M052. The prior-PR-comments lens ran its probe first:
`gh api repos/{owner}/{repo}/pulls/comments?per_page=1` returns an empty list on
both `jmgirard/hitop` and `jmgirard/hitop-builder`, so no GitHub thread walk was
paid for; the archived `## Review` sections of M045 and M048–M052 were the
evidence base. Five findings, ranked; none demonstrates an acceptance criterion
failing and none is a load-bearing defect in what the page does for its users,
so the return floor does not fire.

1. [O] On first load the Word card carries `aria-current="true"` before the
   visitor has chosen anything. `setFormat('docx')` runs at init to make a
   step-bar jump straight to step 3 coherent, and it also marks the card; the
   code comment says the mark tells a returning visitor "which screen the
   visitor just came from", which is not true on a first visit, and a screen
   reader announces one option as current on the screen whose whole purpose is
   to make that choice.
2. [S-prior] The rendered legends are uppercased by CSS ("PAPER SIZE") while the
   README names them in sentence case ("*Paper size*") — the same mismatch M051
   raised and rejected, now recurring on the two new legend names.
3. [S-blame] The Qualtrics and REDCap hints drop M050's sentence "None of them
   reaches the Word form", keeping only "Neither changes the items, their
   wording, or their response options." The reassurance survives in the README
   and the controls no longer sit beside the Word ones, but a sentence a past
   milestone added deliberately is gone from the page.
4. [O] `download()` binds `paper_size` into R's global environment on every
   build, including the two online formats whose generator call never mentions
   it. Nothing reads it — the cross-contamination probe above proves the files
   are identical — but a Word-only control's value is written into the R session
   for a non-Word build.
5. [O] `#downloadHint` renders "It turns on once at least one scale is ticked"
   unconditionally, including when the button is already on. Carried over from
   M052's wording for the three buttons rather than introduced here.

**Triage at the gate (2026-08-24).** Finding 1 fixed on the branch, builder
commit `e2c0600`: the `aria-current` mark is split out of `setFormat()` into
`markFormatChoice()`, called only from a format card's own click handler, so
the page's starting format is set up without being announced as a choice.
Re-verified after the fix: no card marked on load or after a step-bar jump
straight to step 3 (which still lands coherently on Word), the chosen card
marked and the mark persisting on return to the choice screen; AC1 re-run on
one cell per format, all three matching the deployed baseline digests; the
Qualtrics Tab enumeration, the step 2 -> step 3 crossing with its heading focus
and 3px/4px ring, the live-region query, and the 360 CSS px layout cells in both
schemes all unchanged. Findings 2-5 rejected: 2 is the settled M051 call and the
mismatch is CSS casing of names the README gives in sentence case; 3 keeps the
reassurance in the README and the controls are now format-local, so the dropped
sentence has no reader who needs it where it was; 4 has no observable effect,
proved by the cross-contamination probe above; 5 is M052 wording carried over on
an unmodified behaviour.
