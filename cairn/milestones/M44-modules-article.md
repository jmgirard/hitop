# M44: A dedicated article for building and scoring HiTOP-SR modules

- **Status:** review
- **Priority:** normal
- **Depends on:** M43
- **Driving RR:** —
- **Principles touched:** GP3
- **Branch/PR:** `m44-modules-article` / https://github.com/jmgirard/hitop/pull/49

## Goal

Give the module workflow its own site article covering selection, generation,
fielding, and scoring end to end, and reduce the scoring tutorial's copy of it
to a link.

## Scope

User-facing tier: the deliverable is a published pkgdown article and three help
pages researchers read.

**In:** a new `vignettes/articles/modules-hitopsr.Rmd` walking the whole
workflow — choosing scales with `available_scales()`, building a module,
generating the DOCX, Qualtrics, and REDCap files, and scoring plus reliability
from the collected columns; the "Scoring a Short Form" section of
`vignettes/hitopsr_scoring.Rmd:121-183` replaced by a cross-reference; a
`_pkgdown.yml` navbar entry under Tutorials; and an ID-column-safe
column-selection idiom taught in the article and in the `@examples` of
`hitop_module()`, `score_hitopsr()`, and `reliability_hitopsr()` — absorbing the
standing candidate row on that idiom (lineage M37, review finding scored 78).

**Out:** the API rename itself → M43, on which this depends so the article is
written once in the settled vocabulary. The browser module builder → M45, which
this article links only after M45 deploys it. Modules for the HiTOP-BR and
PID-5 → the standing generalization candidate row. Scoring module-collected data
with no descriptor in hand → its own candidate row. Any change to scoring
behavior → none is in scope; this milestone is documentation.

## Acceptance criteria

- [x] AC1 `vignettes/articles/modules-hitopsr.Rmd` renders under
      `pkgdown::build_articles()` with no error; every warning the build emits
      is quoted in the Review with its disposition, and the rendered page is
      read once and that reading recorded.
- [x] AC2 The article carries one section per workflow stage, with headings
      named in the file: choosing scales, building the module, generating the
      three instrument formats, and scoring plus reliability. No chunk in the
      article is `eval = FALSE`, so AC1's clean render is evidence every chunk
      ran; a grep of the file for `eval = FALSE` and `eval=FALSE` is recorded.
- [x] AC3 The article selects its item columns by name from a frame carrying
      leading non-item columns — `ku_hitopsr`, whose first two columns are
      `participant` and `biosex` — and a chunk shows the selected frame's width
      equals the module's `nItems`. The article states plainly that indexing a
      data frame by `module$items` is positional and wrong on such a frame.
- [x] AC4 `vignettes/hitopsr_scoring.Rmd` no longer teaches the module
      workflow: a `grep -n` of that one file for `hitop_module`, `hitop_subset`,
      `module`, and `subset` returns only the sentence linking to the new
      article; the command and its full output are recorded in the Review.
- [x] AC5 The `@examples` blocks of `hitop_module()`, `score_hitopsr()`, and
      `reliability_hitopsr()` select item columns by name rather than by
      positional `[m$items]` indexing, and `devtools::check()` runs them clean.
- [x] AC6 `_pkgdown.yml` lists the article under Tutorials,
      `pkgdown::check_pkgdown()` passes, and a full `pkgdown::build_site()`
      writes `docs/articles/modules-hitopsr.html`; `devtools::document()`
      produces no diff and `devtools::test()` is clean.

## Coverage

- AC1 → T1, T5
- AC2 → T1
- AC3 → T1
- AC4 → T2
- AC5 → T3
- AC6 → T4, T5

## Tasks

- [x] T1 Draft `vignettes/articles/modules-hitopsr.Rmd` to AC2's section list,
      lifting and expanding the worked example currently at
      `vignettes/hitopsr_scoring.Rmd:121-183` and adding the generation half,
      which that section only mentions.
- [x] T2 Replace that section with a short cross-reference to the new article.
- [x] T3 Fix the positional-indexing idiom in the three `@examples` blocks; run
      `devtools::document()`.
- [x] T4 Add the `_pkgdown.yml` Tutorials row. Build with
      `pkgdown::build_articles()` (plural) after `devtools::install()` — the M22
      lesson records that the singular `build_article()` cannot find a newly
      added `.Rmd`, and the M21 lesson that articles render against the
      *installed* package.
- [x] T5 Full build and check: `build_site()`, `check_pkgdown()`,
      `devtools::test()`, `devtools::check()`; record AC1's warnings, AC2's and
      AC4's grep output, and the read of the rendered page.

## Work log

- 2026-08-21: created by /milestone-plan.
- 2026-08-21: plan gate chose two milestones with the rename first over one combined milestone, because a combined goal needs an "and" and trips the sizing tripwires, and because writing the article before the rename would mean rewriting it; falsified by M43 slipping far enough that the documentation gap outweighs the rewrite cost.
- 2026-08-21: plan absorbed the standing candidate row on the `@examples` column-selection idiom (lineage M37, scored 78) rather than leaving it queued, because this milestone rewrites the same example blocks and splitting them across two milestones would touch them twice.
- 2026-08-21: T1 — drafted `vignettes/articles/modules-hitopsr.Rmd` with five sections (choosing scales, building the module, generating the instrument, selecting the collected columns, scoring and reliability); `pkgdown::build_articles()` rendered it with no error and no warning against the freshly installed package; every prose figure in it was derived from an executed probe first.
- 2026-08-21: implement gate chose the title "Building HiTOP-SR Modules", the `hitop_module()`-only example fix (the other two blocks already select by name from M43), and `tempdir()` for the article's generated files.
- 2026-08-21: T2 — replaced `vignettes/hitopsr_scoring.Rmd`'s "Scoring a Module" section with a three-line cross-reference under the heading "Scoring Only Some Scales"; the heading avoids the four AC4 grep terms so the linking sentence is the only hit case-insensitively too, and the link is the absolute site URL because that vignette installs with the package while the article does not. The file's one other use of the word, in the `rename_hitopsr_items()` note, was reworded.
- 2026-08-21: T3 — `score_hitopsr()` and `reliability_hitopsr()` already selected by name (M43 wrote them that way), so only `hitop_module()`'s `@examples` needed the idiom; added a by-name selection against `ku_hitopsr`, whose leading `participant`/`biosex` columns make the positional trap concrete. `devtools::document()` rewrote `man/hitop_module.Rd`; the example lines were executed and `devtools::test()` is clean (0 failures, 13674 passing, 1 skip).
- 2026-08-21: T4 — added the Tutorials navbar row directly after "Scoring HiTOP-SR"; `pkgdown::check_pkgdown()` reports no problems, and `pkgdown::build_articles()` after `devtools::install()` re-rendered the edited scoring vignette with no error or warning.
- 2026-08-21: T5 — full local verification clean: `pkgdown::build_site()` wrote `docs/articles/modules-hitopsr.html` with no error or warning, `pkgdown::check_pkgdown()` found no problems, `devtools::document()` produced no diff, and `devtools::check()` returned 0 errors / 0 warnings / 0 notes (tests run inside it). Added the NEWS entry for the new article. The rendered page was read end to end and every printed figure in its prose matches the chunk output above it.
- 2026-08-21: review — all six criteria verified with fresh evidence; consistency gate clean; three fresh-context lenses returned ten findings, six fixed on the branch, three rejected as convention or plan-text, one rejected as pre-existing and captured as a candidate row.
- 2026-08-21: the criteria audit ran in **full** mode over AC1-AC6 and returned two findings, both fixed before the criteria were written. AC1 promised a render with no error *or warning*, which pkgdown routinely emits for reasons unrelated to the article — narrowed to no error, with every warning quoted and disposed. AC2 promised "every R chunk executes", which a clean render evidences only if no chunk is `eval = FALSE` — the promise now rests on a grep that settles exactly that. The audit ran inline in this session rather than in a fresh-context reader, because this session is instructed not to spawn subagents unless asked; the reader-freshness the instrument normally provides was not obtained.


## Decisions

## Review

Reviewed 2026-08-21 on branch `m44-modules-article`, PR #49.

### Acceptance-criteria evidence

- **AC1** — `pkgdown::build_articles()` and a full `pkgdown::build_site()` both
  read `vignettes/articles/modules-hitopsr.Rmd` and wrote
  `docs/articles/modules-hitopsr.html` with **no error and no warning**; the
  warning-disposition list is therefore empty. The rendered page was read end to
  end (text extracted from the HTML), and every printed figure in its prose was
  checked against the chunk output immediately above it.
- **AC2** — five `##` sections, covering AC2's four stages plus a fifth for
  column selection: Choosing Scales, Building the Module, Generating the
  Instrument, Selecting the Collected Columns, Scoring and Reliability.
  `grep -n 'eval = FALSE\|eval=FALSE'` on the article exits 1 with no output, so
  no chunk is disabled and AC1's clean render evidences that every chunk ran.
- **AC3** — the article selects from `ku_hitopsr`, whose first five column names
  print in the rendered page as `participant`, `biosex`, `hsr001`, `hsr002`,
  `hsr003`; `item_cols <- sprintf("hsr%03d", four_scale$items)` selects by name;
  a chunk prints `ncol(collected) == four_scale$nItems` as `TRUE`. A further
  chunk shows `names(ku_hitopsr[four_scale$items])[1:4]` returning
  `hsr040 hsr064 hsr066 hsr107`, and the prose states plainly that positional
  indexing on such a frame silently returns the wrong items.
- **AC4** — `grep -n 'hitop_module\|hitop_subset\|module\|subset'
  vignettes/hitopsr_scoring.Rmd` returns exactly one line: line 125, the
  sentence linking to the new article. The case-insensitive form of the same
  grep returns the same single line.
- **AC5** — all three `@examples` blocks select item columns by name:
  `R/module.R:42` (`ku_hitopsr[sprintf("hsr%03d", m$items)]`, added here),
  `R/score_hitopsr.R:56` and `R/reliability_hitopsr.R:47`
  (`sim_hitopsr[paste0("hsr_", m$items)]`, matching that dataset's own column
  names, written by M43). `devtools::check()` runs all examples clean.
- **AC6** — `_pkgdown.yml:46-47` carries the Tutorials row;
  `pkgdown::check_pkgdown()` reports no problems; `pkgdown::build_site()` wrote
  `docs/articles/modules-hitopsr.html`; `devtools::document()` produces no diff;
  `devtools::test()` is clean (0 failures, 13,674 passing, 1 skip).

### Consistency gate

`cairn_validate` exits 0, all checks PASS (21 pre-existing advisory warnings on
dangling legacy `D-001`..`D-012` tokens, none introduced here). No principle
changed, so `cairn_impact` was skipped. Toolchain slot: `document()` no diff,
no hand-edited generated files, README.Rmd/README.md untouched,
`check_pkgdown()` clean, NEWS.md entry present, no new top-level files,
`devtools::check()` 0 errors / 0 warnings / 0 notes.

### Independent review — three fresh-context lenses

The blame-history lens found no regression: it compared the 61 deleted vignette
lines content-by-content against the new article and confirmed every part is
reproduced or expanded, and confirmed the vocabulary matches D-034. The
prior-review lens found the only relevant prior finding — M43's positional-
indexing finding — carried forward correctly, and the GitHub inline-comment
probe returned empty, so that surface was skipped. The diff-bug lens
re-executed every factual claim in the article and confirmed all of them, then
reported ten findings.

Findings and dispositions (diff-bug lens, its ranking):

1. Machine-specific temp path leaked into the published page (the three cli
   success messages printed `/var/folders/.../Rtmp.../hitopsr_module.docx`).
   **Fixed now** — `#| message: false` on the generation chunk; the rendered
   page no longer contains `var/folders`.
2. The reliability prose blamed the negative α on scale shortness, which a
   negative α is not evidence of. **Fixed now** — rewritten to say what a
   negative α indicates and that shortness alone does not produce one.
3. The three `@examples` blocks use two different item-column name patterns
   (`hsr%03d` vs `hsr_N`) because `ku_hitopsr` and `sim_hitopsr` name their
   columns differently. **Rejected as out of scope** — each block is correct for
   its own dataset and AC5 passes as written; the underlying inconsistency
   between the two shipped datasets (and `rename_hitopsr_items()`'s third
   pattern, `HSR_N`) predates this diff and became a candidate row.
4. The article never said why `module =` is required. **Fixed now** — the
   scoring section now names the failure it prevents, verified by execution to
   be `"Expected 405 items but got 21"`.
5. `library(tibble)` is attached but unused. **Rejected** — matches the sibling
   scoring vignette's setup chunk.
6. `data("ku_hitopsr")` is unnecessary for a lazy-loaded dataset. **Rejected** —
   matches the sibling scoring vignette.
7. `#| warning: false` on the reliability chunk suppresses nothing under
   `omega = FALSE`. **Fixed now** — removed, so a future real warning surfaces.
8. "lists every scale the instrument offers" overstates: the 17 subscales are
   not selectable. **Fixed now** — a caveat paragraph naming `hitopsr_subscales`
   and the rejection, verified by executing `hitop_module("hitopsr", "Anhedonia")`.
9. The Scope section calls the replaced vignette section "Scoring a Short Form"
   when M43 had renamed it "Scoring a Module". **Rejected** — plan-owned text is
   not edited at review, and the stale name misleads no reader of the shipped
   package.
10. "76 scale scores, which is more than ... many participants will sit through"
    — participants sit through items. **Fixed now** — rephrased.

One further correction the session made while checking finding 3: the article's
closing sentence on `rename_hitopsr_items()` implied it standardizes to the
article's own `hsr001` pattern; it produces `HSR_1` by default, and the sentence
now names its `prefix` argument instead.

After the fixes, `build_site()`, `check_pkgdown()`, and `devtools::check()`
(0/0/0) were all re-run clean.

