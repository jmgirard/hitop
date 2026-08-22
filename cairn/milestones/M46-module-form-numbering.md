# M46: Renumbered and optionally shuffled HiTOP-SR module Word forms

- **Status:** in-progress
- **Priority:** normal
- **Depends on:** M45
- **Driving RR:** —
- **Principles touched:** IP1, GP2, GP3
- **Branch/PR:** `m46-module-form-numbering` / [PR #52](https://github.com/jmgirard/hitop/pull/52)

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

- [x] AC1 — `generate_docx_hitopsr(module = m)` at the shipped defaults writes
      printed rows whose numbers are `as.character(seq_len(m$nItems))` and
      whose texts equal, element for element, the `Text` values of
      `hitopsr_items[hitopsr_items$Scale %in% m$scales, ]` in ascending `HSR`
      order — the expected set derived by filtering `hitopsr_items$Scale`, not
      through `m$items`.
- [x] AC2 — in that same file, for each scale in `m$scales`, the scoring page's
      item list — as extracted by `docx_scoring_rows()` — equals that scale's
      items' ranks among the module's items (derived by the same
      `hitopsr_items$Scale` filter), carrying an `(R)` marker on exactly those
      whose `hitopsr_items$Reverse` is `TRUE`; for this `m` that is HSR 310,
      whose printed rank is 20, and the other 22 items carry no marker.
- [x] AC3 — the printed rows of a fresh default `generate_docx_hitopsr()` call
      with no `module` equal, number for number and text for text, those
      extracted from the committed `inst/extdata/hitopsr_US.docx`; and
      `generate_docx_hitopsr(module = m, renumber = FALSE)` reproduces the
      gapped original numbers in full — `1, 2, 42, …, 389` — so an
      implementation that renumbers unconditionally fails this criterion.
      `tests/testthat/test-artifacts.R` passes unchanged, so no shipped
      artifact or `hitop_artifacts` row moved.
- [x] AC4 — with `randomize = TRUE` and `module = m`, the printed numbers are
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
- [x] AC6 — after the change, no file under `R/`, no file under `vignettes/`,
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
- [x] AC7 — `devtools::test()` and `devtools::check()` are clean (0 errors,
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
- [x] T2 — write the failing renumbering tests (AC1, AC2, and AC3's
      `renumber = FALSE` clause).
- [x] T3 — implement `renumber` in `generate_docx_hitopsr()`: build one
      printed-order map after `apply_module()` and remap the item column
      passed to `make_items_table()` and the `itemdata` frames passed to
      `make_scoring_table()` (including `hitopsr_subscales`) through it.
- [x] T4 — add a `docx_crosswalk_pairs()` helper and write the failing
      shuffle tests (AC4, AC5).
- [x] T5 — implement `randomize`, the printed crosswalk on the scoring page,
      and the `item_order` attribute on the invisible return.
- [x] T6 — update `?generate_docx_hitopsr`, `?hitop_module`, the Qualtrics and
      REDCap help pages (`@param module` plus the divergence sentence),
      `README.Rmd`, `vignettes/articles/modules-hitopsr.Rmd`,
      and `NEWS.md`; `devtools::document()`.
- [x] T7 — run
      `devtools::test()` and `devtools::check()`.

## Work log

- 2026-08-21: created by /milestone-plan.
- 2026-08-21: plan-gate criteria audit ran in FULL mode (user-facing tier), by a fresh-context [O] reader; returned six per-criterion findings and six cross-cutting ones. Fixed here: the full-form regression clause was unfalsifiable (an unconditional renumber passed it) and is now a comparison against the committed artifact plus a `renumber = FALSE` case; AC2's "ascending" clause was vacuous and its reverse oracle ambiguous, now named as `hitopsr_items$Reverse`; AC1's expected set now derives independently of `m$items` per IP2; AC5's reconstruction is stated as an equation; AC6's evidence-procedure clause was instrument-bound and is replaced by a `grep` domain plus the standing site list; the missing row extractor became T1. Routed to the gate: the IP1 sign-off, the shuffled-form scoring hazard, the cross-generator divergence.
- 2026-08-21: plan gate chose renumbering by default over an opt-in argument because it reaches the browser builder with no change in that repo and makes the printed number equal the column position `score_hitopsr(module =)` expects; falsified by a report of module data collected against an older form being scored against a renumbered key.
- 2026-08-21: plan gate chose a printed crosswalk on the scoring page over an `item_order` attribute alone, and over refusing `randomize` when `include_scoring = FALSE`, because a shuffled form must be scoreable from the paper alone; falsified by a researcher needing a participant-facing shuffled form that must carry no key.
- 2026-08-21: plan gate chose Word-only renumbering over renumbering all three generators because a Qualtrics/REDCap item number names a collected data column; falsified by a user mis-joining Word-collected and online-collected module data.
- 2026-08-22: implement gate settled three open choices: `randomize = TRUE` with `renumber = FALSE` is legal (the two arguments stay independent), the crosswalk prints as compact arrow-joined pairs, and `item_order` is attached on every call rather than only a shuffled one.
- 2026-08-22: substantive amendment — the criteria's module `m` named four scales, none reverse-keyed (only "Romantic Disinterest" is, of all 405 items), so AC2's `(R)` clause was vacuous; `m` gains that scale. A fresh-context [O] reader that authored none of the wording ran the FULL criteria audit (user-facing tier) over the amended text and returned seven findings. Fixed directly: AC2's drafted justification clause was instrument-bound (replaced by the pinned HSR 310 / rank 20 exemplar), AC4's "two calls under one seed" was literally unreachable, AC5 named no crosswalk extractor, AC3's gapped-number clause is now stated in full, AC7's "0 notes" bound the toolchain rather than the deliverable. Routed to the mini gate and adopted at the user's selection: AC5 widened with the shuffled-form `(R)`-marker clause, and AC6 widened to four grep terms over four paths plus a clause on the two online help pages. T1 and T4 gained the two extra extractors; T6 gained `README.Rmd` and the divergence sentence.
- 2026-08-22: T3/T4/T5 — `generate_docx_hitopsr()` gains `renumber` and `randomize`, both `validate_flag()`-checked. One printed-order map built after `apply_module()` drives the items table, the scoring table, the subscale rows, the crosswalk, and the return attribute; `remap_itemdata()` re-sorts each scoring row by its printed number. The crosswalk prints as arrow-joined pairs ahead of the scoring table (arrow, not `=`, so it cannot be mistaken for the response legend by the parser that reads it). `docx_crosswalk_pairs()` added; the two M24 original-numbering tests now pass `renumber = FALSE`, which is the opt-out's regression coverage.
- 2026-08-22: T6 — `?generate_docx_hitopsr` documents both arguments, the `item_order` attribute, and the Word-vs-online divergence; `?hitop_module`, `?generate_qualtrics_hitopsr`, `?generate_redcap_hitopsr`, `vignettes/articles/modules-hitopsr.Rmd`, and `NEWS.md` updated, with the 0.2.0 module entry's "not renumbered" claim corrected in place. `apply_module()`'s internal comment now says whose decision the printed numbering is. `devtools::document()` run. AC6's grep re-run over `R/`, `vignettes/`, `README.Rmd`, `NEWS.md`: every remaining hit reads correctly (they concern `m$items`, the online exports, or `rename_hitopsr_items()`'s legacy pool names).
- 2026-08-22: post-T7 gap closed — the `include_subscales` + `randomize` path (reachable only on the full instrument, since subscales cannot combine with a module) remapped `hitopsr_subscales` rows through the printed-order map with nothing exercising it; a test now asserts each subscale row's printed numbers map back through `item_order` to that subscale's original items and are sorted by printed number. `devtools::test()` re-run: FAIL 0 / WARN 0 / SKIP 1 / PASS 13766.
- 2026-08-22: review return (defect return 1) — AC5 unverified and two load-bearing defects. AC5's per-scale reconstruction clause was asserted by an identity (`item_order[match(v, item_order)] == v`) in both the test and this review's first evidence pass; re-run against two arbitrary orders, one deliberately wrong, it passed against both, so it constrains the document not at all. The criterion stands as written — the printed numbers must be read from `docx_scoring_rows()` — so this is a test defect, not a criterion defect, and no amendment is convened. Also returning: `randomize = TRUE` with `include_scoring = FALSE` writes a shuffled form carrying no crosswalk (the map lives inside the scoring block), and a shuffled form's printed order is not the order `score_hitopsr(module = )` addresses columns in, with nothing said about it. Status to in-progress; AC5 unticked; ten findings and their triage recorded in the Review section.
- 2026-08-22: review — branch pushed, draft PR #52 opened, CI running. AC1-AC7 all verified with fresh evidence (recorded in the Review section) and ticked. Consistency gate clean: `cairn_validate` exit 0 with every check PASS (20 pre-existing dangling `D-001`..`D-012` advisories), `document()` no-diff, `pkgdown::check_pkgdown()` no problems, `check()` Status OK. Review fan-out spawned: [S] blame-history returned no findings across all four of its categories; [S] prior-review returned one low-confidence item (a bare `"(R)"` grep echoing the M26 lesson), which I reproduced against the implementation and confirmed cannot match the instruction sentence, since `docx_scoring_rows()` yields only scoring-table cells; [O] diff-bug still running.
- 2026-08-22: T7 — `devtools::check()` reports Status: OK (0 errors, 0 warnings, 0 notes). All tasks done; status to review.
- 2026-08-22: verify slot clean after T6 — `devtools::test()` reports FAIL 0 / WARN 0 / SKIP 1 / PASS 13731, the one skip being the pre-existing SDTD item-38 keying dispute. T1-T6 ticked; T7 adds `devtools::check()`.
- 2026-08-22: T2 — AC1/AC2/AC3 renumbering tests written in `tests/testthat/test-docx-numbering.R`; red by design (7 failures: the 1..n numbers, the rank-based scoring lists, and `renumber` being an unused argument). Box stays unticked until T3 turns them green.
- 2026-08-22: T1 — `docx_item_rows()` added to `helper-generators.R` with an XML-entity unescaper, pinned against the committed `inst/extdata/hitopsr_US.docx` (405 rows, numbers and texts equal to `hitopsr_items` in `HSR` order).

## Decisions

## Review

_Fresh evidence gathered 2026-08-22 on branch `m46-module-form-numbering` at
`0264d7d`, via `devtools::load_all()` + the committed DOCX extractors. `m` is
the five-scale module the criteria preamble fixes (23 items)._

- **AC1 — verified.** `generate_docx_hitopsr(module = m)` at the shipped
  defaults writes 23 printed rows whose numbers are exactly
  `as.character(1:23)` and whose texts are element-for-element identical to the
  `Text` values of `hitopsr_items[hitopsr_items$Scale %in% m$scales, ]` in
  ascending `HSR` order, the expected set built by that filter and never
  through `m$items`.
- **AC2 — verified.** All five scoring rows equal that scale's items' ranks
  among the module's items, derived by the same `hitopsr_items$Scale` filter.
  Exactly one `(R)` marker appears on the page, and the module carries exactly
  one `hitopsr_items$Reverse == TRUE` item: HSR 310 at printed rank 20, and the
  Romantic Disinterest row reads `3, 12, 13, 20(R), 21`.
- **AC3 — verified.** A fresh default `generate_docx_hitopsr()` call with no
  `module` produces printed rows identical, number for number and text for
  text, to those extracted from the committed `inst/extdata/hitopsr_US.docx`.
  `renumber = FALSE` on `m` reproduces the gapped originals in full —
  `1, 2, 42, 45, 55, 66, 84, 86, 109, 118, 144, 152, 187, 195, 202, 216, 260,
  278, 291, 310, 338, 355, 389` — equal to `hitopsr_items$HSR` and unequal to
  `1:23`, so an unconditional renumber fails here. `test-artifacts.R` passes
  unchanged (121 assertions); `git diff main..HEAD` touches no file under
  `inst/extdata/` or `data/`, so no artifact or `hitop_artifacts` row moved.
- **AC4 — verified.** With `randomize = TRUE` and `module = m` the printed
  numbers are `as.character(1:23)` and the texts are a permutation of AC1's
  expected set. Seeds 1 through 5 under `set.seed()` produce 5 distinct printed
  orders (the criterion asks for at least 2). Two calls each preceded by
  `set.seed(1)` produce identical printed-text vectors. With `module = NULL`,
  `randomize = TRUE` writes numbers `as.character(1:405)` whose texts permute
  `hitopsr_items$Text` and are not in `HSR` order.
- **AC5 — NOT verified.** Three of its four clauses hold: under `set.seed(3)`
  the crosswalk has 23 rows whose `new` column is `1:23` and whose `original`
  column equals `attr(out, "item_order")`; reading the module's texts through
  that attribute reproduces the printed page exactly; and the scoring page
  carries exactly one `(R)` marker, on printed number 8, where HSR 310 landed
  in `item_order` — the pre-shuffle rank would have been 20, so that clause
  discriminates. An unshuffled call prints no crosswalk and reports an
  ascending `item_order`. **The per-scale reconstruction clause is not
  verified.** Both the test at `test-docx-numbering.R:216-223` and this
  review's first evidence pass took "that scale's printed numbers" to mean
  `match(want$HSR[want$Scale == scale], item_order)`, which reduces
  `item_order[match(v, item_order)]` to `v` — an identity. Re-run 2026-08-22
  against two arbitrary hand-built orders, one of them deliberately wrong: the
  clause passed against both, so it constrains the generated document not at
  all. The clause is satisfiable non-vacuously — the printed numbers must be
  read from `docx_scoring_rows()`, as the subscale test at `:296-308` already
  does — so the criterion stands as written and the test is what is wrong.
- **AC6 — verified.** `grep -rniE 'renumber|original|numbering|item number'`
  over `R/`, `vignettes/`, `README.Rmd`, and `NEWS.md` returns 94 hits; the 55
  substantive ones (the rest being `rename_hitopsr_items()`'s legacy pool
  names, `\item{}` data-dictionary rows, and `all original data columns`
  boilerplate) were read individually and none asserts that a module **Word**
  form keeps its original item numbers. Every plan-listed site was addressed:
  `R/module.R` now says the descriptor holds original numbers whatever a
  generator prints; the two Qualtrics/REDCap `keeping their original` hits are
  correct for those exports and each carries the divergence sentence beside it.
  `renumber`, `randomize`, and `item_order` are each documented on
  `?generate_docx_hitopsr` and carry a `NEWS.md` entry, and both
  `man/generate_qualtrics_hitopsr.Rd` and `man/generate_redcap_hitopsr.Rd`
  reference `generate_docx_hitopsr()` for the divergence.
- **AC7 — verified.** `devtools::test()`: FAIL 0, WARN 0, SKIP 1, PASS 13766 —
  the one skip is the pre-existing SDTD item-38 keying dispute (SOURCES.md
  OQ-1), unrelated to this milestone. `devtools::check()`: Status OK, 0 errors,
  0 warnings, 0 notes, so the baseline-note clause never bound.

### Consistency gate

- `cairn_validate.py` exit 0 — every check PASS, including `coverage complete`
  and `binding criteria`. 20 advisory warnings, all pre-existing dangling
  `D-001`..`D-012` tokens from the pre-migration numbering range; untouched by
  this milestone.
- No `DESIGN.md` principle changed, so `cairn_impact.py` was not run.
- Profile `r-package` toolchain slot: `devtools::document()` produces no diff;
  `NAMESPACE`, `man/`, `data/*.rda` regenerate cleanly; `README.Rmd`/`README.md`
  untouched by the diff; `pkgdown::check_pkgdown()` reports no problems;
  `NEWS.md` carries the user-visible entry with no milestone number in it; the
  only top-level file in the diff is `NEWS.md`, so no `.Rbuildignore` entry is
  owed; full `devtools::check()` clean.

### Independent review

Three fresh-context lenses (user-facing tier, executable diff).

**[S] blame-history — no findings.** All four categories clean: the default
change is D-036-authorized rather than silent, the M36 one-item-table fix is
untouched, no D-entry is contradicted, and `R/generate_qualtrics.R`'s CRLF was
preserved rather than normalized.

**[S] prior-review — one item, rejected.** It flagged the bare `"(R)"` grep at
`test-docx-numbering.R:85` and `:232` as echoing the M26 lesson that an
unanchored `"(R)"` search matches every form's own instruction sentence.
Reproduced 2026-08-22: `docx_scoring_rows()` yields only the scoring table's
per-scale cells, and no cell contains the instruction sentence, so the search
is not vacuous here. Rejected as out-of-scope-adjacent, though the anchoring
tidy-up rides along with finding 8 below.

**[O] diff-bug — ten findings, all reproduced by execution 2026-08-22.**
Ranked as the reviewer ranked them; every one verified against the
implementation rather than against the reviewer's account.

1. `randomize = TRUE` makes `score_hitopsr(module = )` silently mis-score, and
   nothing says so. `score_hitopsr()` addresses items by position within
   `module$items`, so data keyed in a shuffled form's printed order is scored
   against the wrong keys; no doc this branch adds tells the user to reorder
   through `item_order` first. Reproduced: under `set.seed(3)` printed position
   1 holds original HSR 55, while `score_hitopsr()` expects column 1 to be HSR
   1. Scope put scorer-side *detection* out, superseded by the printed
   crosswalk — but the crosswalk serves hand-scoring only, and the package's
   own scoring path is left unguarded and undocumented.
2. `randomize = TRUE` with `include_scoring = FALSE` writes a shuffled form
   carrying no crosswalk at all. `crosswalk_msg` is built at
   `R/generate_docx.R:259-263` but emitted only inside
   `if (include_scoring && !is.null(table_2))` at `:511-537`. Reproduced:
   `nrow(docx_crosswalk_pairs(f)) == 0` — a paper form unscoreable from the
   paper, exactly what the plan gate said the crosswalk would prevent. No test
   covers the combination.
3. `@param randomize` is false for the legal `renumber = FALSE,
   randomize = TRUE` pairing. It states unconditionally that the numbering runs
   down the page and a crosswalk is printed; with `renumber = FALSE` the page
   shows gapped originals in shuffled order and `:260` suppresses the
   crosswalk. Reproduced: 0 crosswalk rows. Same overreach in the vignette and
   `NEWS.md`.
4. AC5's per-scale reconstruction clause is verified by an algebraic identity,
   not by the document (IP2). See the AC5 line above.
5. Consequently the module scoring rows under `randomize` are covered by
   exactly one item — the HSR-310 `(R)` marker. A remap defect confined to
   another scale's row ships green.
6. `NEWS.md:208` says "see the entry below" but the renumbering entry is above
   it in the same version section.
7. Inserting `renumber`/`randomize` between `module` and `subset` breaks
   positional calls; GP2 permits the break with NEWS, and the NEWS entry
   mentions only the new arguments.
8. The new test file uses bare `set.seed()` rather than `withr::local_seed()`,
   leaving the session RNG at a fixed state for every file that runs after it;
   and `:266` omits the `skip_if_no_docx()` guard its siblings carry.
9. `item_order` is a double vector, since `hitopsr_items$HSR` is double, while
   `docx_crosswalk_pairs()` returns integer. Reproduced:
   `identical(attr(out, "item_order"), 1:405)` is `FALSE`. Cosmetic; no doc
   promises a type.
10. A no-module shuffled form prints all 405 arrow pairs as one 4,237-character
    paragraph (reproduced). No test calls `docx_crosswalk_pairs()` on that
    path, and D-036 describes the crosswalk only for module forms, so this
    participant-facing layout has not been signed off under IP1.

The reviewer independently confirmed AC1, AC2, AC3, AC4, AC6, and AC7 met, and
reported nothing in three further categories: convention violations beyond
finding 7, DOCX extractor defects, and edge cases (a single-item module is
unreachable — the smallest HiTOP-SR scale has 3 items).

### Triage (2026-08-22)

Every finding's disposition, none dropped (IP3):

- **1 — fix now, documentation.** At the gate Jeff chose documenting the
  hazard over adding a runtime warning: `?generate_docx_hitopsr`, the modules
  vignette, and `NEWS.md` will say plainly that responses collected on a
  shuffled form must be reordered through `item_order` before
  `score_hitopsr(module = )`, with a worked line. Scorer-side detection stays
  out of scope per the plan.
- **2 — fix now.** The crosswalk moves out of the `include_scoring` block, so
  a shuffled module form carries its map whether or not the scoring key is
  appended. A crosswalk is a numbering map, not a key: it reveals no scale
  membership and no reverse-keying, so printing it does not defeat
  `include_scoring = FALSE`. A test covers the combination.
- **3 — fix now.** The `@param randomize` text, the vignette, and `NEWS.md`
  narrow to what the code does: the crosswalk is printed for a renumbered
  module form, and `renumber = FALSE` prints the originals, which are their
  own map.
- **4 — fix now.** The AC5 loop reads each scale's printed numbers from
  `docx_scoring_rows()` instead of deriving them by `match()` against
  `item_order`. This is the criterion failure that returned the milestone.
- **5 — fix now, folded into 4.** Once 4 reads the printed page, the module
  scoring rows under `randomize` are covered per scale rather than by the
  single HSR-310 marker.
- **6, 7, 9 — fix now.** The `NEWS.md` cross-reference direction; a NEWS note
  that the two new arguments sit between `module` and `subset`, so positional
  calls past `font_family` must be respelled; `item_order` returned as integer.
- **8 — fix now.** `withr::local_seed()` in place of bare `set.seed()`, and
  the missing `skip_if_no_docx()` guard. The prior-review lens's `(R)`
  anchoring tidy-up rides along here.
- **10 — fix now, per Jeff's gate decision.** The crosswalk prints for module
  forms only. A shuffled full instrument prints none — its 405-pair paragraph
  was never signed off under IP1 and D-036 covers module forms only, so this
  keeps the printed page inside the existing sign-off; the caller reads the
  order from `item_order`. No criterion changes: AC5 is stated over `m`, a
  module, and AC4's no-module clause says nothing about a crosswalk.
- **prior-review lens's `(R)` item — rejected**, reproduced as non-vacuous
  (see above); its anchoring suggestion is absorbed into 8.

CI on PR #52 at `5c450ca`: six of seven jobs pass (macOS, pkgdown, coverage,
Ubuntu devel/oldrel-1/release); Windows was still building when the review
returned. CI will be re-run on the fix commits regardless.
