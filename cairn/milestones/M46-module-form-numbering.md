# M46: Renumbered and optionally shuffled HiTOP-SR module Word forms

- **Status:** in-progress
- **Priority:** normal
- **Depends on:** M45
- **Driving RR:** —
- **Principles touched:** IP1, GP2, GP3
- **Branch/PR:** `m46-module-form-numbering`

## Goal

A HiTOP-SR module Word form numbers its items 1..n down the page instead of
printing the full instrument's gapped numbers, and can optionally present them
in a shuffled order, without losing the mapping back to the original items.

## Scope

Surface tier: **user-facing** — `generate_docx_hitopsr()` is exported and the
DOCX it writes is handed to research participants.

**In:** a `renumber` argument (default `TRUE`) and a `randomize` argument
(default `FALSE`) on `generate_docx_hitopsr()`; the item table, the scoring
table, and the subscale rows all built from one printed-order map; a printed
new-number → original-number crosswalk on the scoring page whenever items are
shuffled; an `item_order` attribute on the invisible return value; a DOCX
row-extraction test helper; the IP1 layout sign-off recorded as a D-entry; the
documentation sites that today promise unrenumbered generator output.

**Out:** renumbering `generate_qualtrics_hitopsr()` or
`generate_redcap_hitopsr()` — there an item number names a collected data
column and the survey platform does its own randomization; those help pages
gain only a sentence saying the Word form differs → no successor milestone, a
deliberate divergence. A `seed` argument → out; the caller uses `set.seed()`.
Rebuilding any shipped artifact under `inst/extdata/` → out, and AC3 proves it
did not happen. A UI toggle in the browser builder
([jmgirard/hitop-builder](https://github.com/jmgirard/hitop-builder)) → out of
this repo entirely; renumbering by default is what reaches that app with no
change on its side. Detecting or refusing a shuffled form's data inside
`score_hitopsr()` → out, superseded by the printed crosswalk → ROADMAP
candidate row if a mis-scored paper dataset is ever reported.

## Acceptance criteria

For every criterion below, `m` is
`hitop_module("hitopsr", c("Agoraphobia", "Appetite Loss", "Perfectionism",
"Romantic Disinterest", "Social Aloofness"))`, and "printed rows" means the
(number, text) pairs the new test helper extracts from a written DOCX in
document order.

- [ ] AC1 — `generate_docx_hitopsr(module = m)` at the shipped defaults writes
      printed rows whose numbers are `as.character(seq_len(m$nItems))` and
      whose texts equal, element for element, the `Text` values of
      `hitopsr_items[hitopsr_items$Scale %in% m$scales, ]` in ascending `HSR`
      order — the expected set derived by filtering `hitopsr_items$Scale`, not
      through `m$items`.
- [ ] AC2 — in that same file, for each scale in `m$scales`, the scoring page's
      item list — as extracted by `docx_scoring_rows()` — equals that scale's
      items' ranks among the module's items (derived by the same
      `hitopsr_items$Scale` filter), carrying an `(R)` marker on exactly those
      whose `hitopsr_items$Reverse` is `TRUE`; for this `m` that is HSR 310,
      whose printed rank is 20, and the other 22 items carry no marker.
- [ ] AC3 — the printed rows of a fresh default `generate_docx_hitopsr()` call
      with no `module` equal, number for number and text for text, those
      extracted from the committed `inst/extdata/hitopsr_US.docx`; and
      `generate_docx_hitopsr(module = m, renumber = FALSE)` reproduces the
      gapped original numbers in full — `1, 2, 42, …, 389` — so an
      implementation that renumbers unconditionally fails this criterion.
      `tests/testthat/test-artifacts.R` passes unchanged, so no shipped
      artifact or `hitop_artifacts` row moved.
- [ ] AC4 — with `randomize = TRUE` and `module = m`, the printed numbers are
      `as.character(seq_len(m$nItems))` and the printed texts are a permutation
      of AC1's expected texts; across seeds 1 through 5 under `set.seed()` at
      least two distinct printed orders occur; two calls each preceded by
      `set.seed(1)` produce identical printed-text vectors; and `randomize =
      TRUE` with `module = NULL` writes printed numbers `as.character(1:405)`
      whose texts are a permutation of `hitopsr_items$Text` and are not in
      `HSR` order.
- [ ] AC5 — a shuffled form's scoring page carries a crosswalk whose
      (new, original) pairs, as extracted by a new `docx_crosswalk_pairs()`
      helper, are the printed numbers paired with the original `HSR` numbers,
      and the invisible return value carries an `item_order` attribute equal to
      that same original-number vector in printed order; for each scale in
      `m$scales`, `attr(out, "item_order")[<that scale's printed numbers>]`
      equals `hitopsr_scales$itemNumbers` for that scale as a set; and that
      scoring page carries an `(R)` marker on exactly the printed number
      `attr(out, "item_order")` maps to HSR 310, and on no other.
- [ ] AC6 — after the change, no file under `R/`, no file under `vignettes/`,
      not `README.Rmd`, and not `NEWS.md` asserts that a module **Word** form
      keeps its original item numbers, established by
      `grep -rniE 'renumber|original|numbering|item number'` over those paths
      and reading each hit; the sites standing today are `R/module.R:7-8`,
      `R/module.R:35`, `R/generate_docx.R:98`, `R/generate_docx.R:110`,
      `R/generate_qualtrics.R:62`, `R/generate_redcap.R:74`, `NEWS.md:193-195`,
      and `vignettes/articles/modules-hitopsr.Rmd:86-89` and `:95`. The
      `renumber`/`randomize` arguments, the `item_order` return attribute, and
      the Word-vs-online divergence are each documented on
      `?generate_docx_hitopsr` and carry a `NEWS.md` entry, and
      `?generate_qualtrics_hitopsr` and `?generate_redcap_hitopsr` each state
      that the Word form renumbers a module while these do not.
- [ ] AC7 — `devtools::test()` and `devtools::check()` are clean (0 errors,
      0 warnings, and no note absent from the pre-milestone baseline of the
      default branch), per the profile's verify slot.

## Coverage

- AC1 → T2, T3
- AC2 → T2, T3
- AC3 → T1, T3, T7
- AC4 → T4, T5
- AC5 → T4, T5
- AC6 → T6
- AC7 → T7

## Tasks

- [x] T1 — add `docx_item_rows()` and `docx_scoring_rows()` helpers to
      `tests/testthat/helper-generators.R` returning printed (number, text)
      pairs and (scale, items) pairs in document order; pin the first against
      the committed `inst/extdata/hitopsr_US.docx`.
- [ ] T2 — write the failing renumbering tests (AC1, AC2, and AC3's
      `renumber = FALSE` clause).
- [ ] T3 — implement `renumber` in `generate_docx_hitopsr()`: build one
      printed-order map after `apply_module()` and remap the item column
      passed to `make_items_table()` and the `itemdata` frames passed to
      `make_scoring_table()` (including `hitopsr_subscales`) through it.
- [ ] T4 — add a `docx_crosswalk_pairs()` helper and write the failing
      shuffle tests (AC4, AC5).
- [ ] T5 — implement `randomize`, the printed crosswalk on the scoring page,
      and the `item_order` attribute on the invisible return.
- [ ] T6 — update `?generate_docx_hitopsr`, `?hitop_module`, the Qualtrics and
      REDCap help pages (`@param module` plus the divergence sentence),
      `README.Rmd`, `vignettes/articles/modules-hitopsr.Rmd`,
      and `NEWS.md`; `devtools::document()`.
- [ ] T7 — run
      `devtools::test()` and `devtools::check()`.

## Work log

- 2026-08-21: created by /milestone-plan.
- 2026-08-21: plan-gate criteria audit ran in FULL mode (user-facing tier), by a fresh-context [O] reader; returned six per-criterion findings and six cross-cutting ones. Fixed here: the full-form regression clause was unfalsifiable (an unconditional renumber passed it) and is now a comparison against the committed artifact plus a `renumber = FALSE` case; AC2's "ascending" clause was vacuous and its reverse oracle ambiguous, now named as `hitopsr_items$Reverse`; AC1's expected set now derives independently of `m$items` per IP2; AC5's reconstruction is stated as an equation; AC6's evidence-procedure clause was instrument-bound and is replaced by a `grep` domain plus the standing site list; the missing row extractor became T1. Routed to the gate: the IP1 sign-off, the shuffled-form scoring hazard, the cross-generator divergence.
- 2026-08-21: plan gate chose renumbering by default over an opt-in argument because it reaches the browser builder with no change in that repo and makes the printed number equal the column position `score_hitopsr(module =)` expects; falsified by a report of module data collected against an older form being scored against a renumbered key.
- 2026-08-21: plan gate chose a printed crosswalk on the scoring page over an `item_order` attribute alone, and over refusing `randomize` when `include_scoring = FALSE`, because a shuffled form must be scoreable from the paper alone; falsified by a researcher needing a participant-facing shuffled form that must carry no key.
- 2026-08-21: plan gate chose Word-only renumbering over renumbering all three generators because a Qualtrics/REDCap item number names a collected data column; falsified by a user mis-joining Word-collected and online-collected module data.
- 2026-08-22: implement gate settled three open choices: `randomize = TRUE` with `renumber = FALSE` is legal (the two arguments stay independent), the crosswalk prints as compact arrow-joined pairs, and `item_order` is attached on every call rather than only a shuffled one.
- 2026-08-22: substantive amendment — the criteria's module `m` named four scales, none reverse-keyed (only "Romantic Disinterest" is, of all 405 items), so AC2's `(R)` clause was vacuous; `m` gains that scale. A fresh-context [O] reader that authored none of the wording ran the FULL criteria audit (user-facing tier) over the amended text and returned seven findings. Fixed directly: AC2's drafted justification clause was instrument-bound (replaced by the pinned HSR 310 / rank 20 exemplar), AC4's "two calls under one seed" was literally unreachable, AC5 named no crosswalk extractor, AC3's gapped-number clause is now stated in full, AC7's "0 notes" bound the toolchain rather than the deliverable. Routed to the mini gate and adopted at the user's selection: AC5 widened with the shuffled-form `(R)`-marker clause, and AC6 widened to four grep terms over four paths plus a clause on the two online help pages. T1 and T4 gained the two extra extractors; T6 gained `README.Rmd` and the divergence sentence.
- 2026-08-22: T2 — AC1/AC2/AC3 renumbering tests written in `tests/testthat/test-docx-numbering.R`; red by design (7 failures: the 1..n numbers, the rank-based scoring lists, and `renumber` being an unused argument). Box stays unticked until T3 turns them green.
- 2026-08-22: T1 — `docx_item_rows()` added to `helper-generators.R` with an XML-entity unescaper, pinned against the committed `inst/extdata/hitopsr_US.docx` (405 rows, numbers and texts equal to `hitopsr_items` in `HSR` order).

## Decisions

## Review
