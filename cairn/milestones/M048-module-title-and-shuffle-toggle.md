# M048: A module-titled Word form and a shuffle toggle in the browser builder

- **Status:** review
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** IP1
- **Branch/PR:** `m048-module-title-and-shuffle-toggle` / https://github.com/jmgirard/hitop/pull/54

## Goal

A Word form built from a HiTOP-SR module says on its face that it is a module,
and the browser builder lets the person building it shuffle the item order.

## Scope

User-facing tier: both deliverables are read by researchers building forms and
by the participants who fill them in.

**In:** a module-aware default for `generate_docx_hitopsr(title = )`; a
Word-only shuffle checkbox in `jmgirard/hitop-builder`'s `index.html`, unticked
on load, with a short on-page note that collected columns must be reordered
before scoring; NEWS, help-page, and app-README text for both.

**Out:** exposing `renumber` in the app (declined at the plan gate; a shuffled
form with original numbers prints no crosswalk) → candidate row if a user asks
for it. A module-aware title for the HiTOP-BR or PID-5 generators → no module
support exists there; it follows the standing modularization candidate. Version
pinning, CI, or a smoke test for the app → the standing M045 candidate row.
Rebuilding any `inst/extdata/` artifact → nothing shipped there is a module.

## Acceptance criteria

- [x] AC1: `generate_docx_hitopsr()` given a `module` and no `title` writes the
      header `"HiTOP-SR Module (v1.0)"`; given no `module` and no `title` it
      writes `"HiTOP-SR (v1.0)"`. Evidence: a test parsing the title paragraph
      back out of each freshly built document.
- [x] AC2: an explicit `title = ` is printed verbatim in both the module and the
      full-instrument case, so the new default overrides nothing a caller sets.
- [x] AC3: rebuilding the full instrument with defaults at both paper sizes and
      comparing each against its committed `inst/extdata/hitopsr_US.docx` /
      `hitopsr_A4.docx` finds the same title and the same item rows — the two
      files that procedure rebuilds are the two the package ships from this
      generator.
- [x] AC4: on the builder page, generating a Word file with the shuffle control
      ticked yields a document carrying a printed-number-to-original-number
      crosswalk whose pairs, read off the crosswalk paragraph, match the
      original numbers read independently off the printed items table; with the
      control unticked the document carries no crosswalk and its items ascend.
- [x] AC5: generating the Qualtrics file twice from the same scale selection,
      once with the control ticked and once unticked, yields byte-identical
      files; likewise the REDCap file compared entry-for-entry.
- [x] AC6: the shuffle control carries an on-page note, shown when it is ticked,
      saying collected columns must be put back into the form's printed order
      before scoring; `?generate_docx_hitopsr`'s `title` entry states the
      module-aware default; NEWS records the title change; the app README
      describes the control.
- [x] AC7: `Rscript -e 'devtools::test()'` clean and `Rscript -e
      'devtools::check()'` clean (0 errors, 0 warnings; NOTEs justified).

## Coverage

- AC1 → T1, T2
- AC2 → T1, T2
- AC3 → T2
- AC4 → T3, T5
- AC5 → T5
- AC6 → T3, T4
- AC7 → T4

## Tasks

- [x] T1: give `generate_docx_hitopsr()` a `title = NULL` sentinel resolving to
      `"HiTOP-SR Module (v1.0)"` when `module` is non-`NULL` and to
      `"HiTOP-SR (v1.0)"` otherwise (`R/generate_docx.R:161-176`), leaving an
      explicit `title` untouched; update the `@param title` text
      (`R/generate_docx.R:87-88`).
- [x] T2: tests parsing the header out of a built DOCX — module default, full
      default, explicit title in each case — plus the full-instrument comparison
      against both committed `inst/extdata/hitopsr_*.docx`.
- [x] T3: in `jmgirard/hitop-builder`, add the Word-only shuffle checkbox beside
      the paper-size fieldset (`index.html:107-117`), pass `randomize` into the
      DOCX call only (`index.html:266-278`), and add the reorder-before-scoring
      note shown when it is ticked.
- [x] T4: NEWS entry; `devtools::document()`; app README row and behavior text;
      run the profile's verify and check commands.
- [x] T5: verify the app against a locally served copy of the page — download a
      Word file with the control ticked and unticked and check the crosswalk and
      item order, and diff the Qualtrics and REDCap downloads across both states.

## Work log

- 2026-08-23: created by /milestone-plan.
- 2026-08-23: criteria audit ran in **full** mode but **inline**, not in a
  fresh-context subagent — this session is configured not to spawn agents; two
  findings, both fixed before writing: AC4 keyed on the deterministic crosswalk
  rather than on "items print non-ascending", which a genuine shuffle can fail
  by chance, and AC3 narrowed from "no shipped artifact changes" to the two
  files its rebuild procedure enumerates.
- 2026-08-23: plan gate chose a module-aware package default for the title over
  the app passing `title` itself, because an R caller building a module would
  otherwise still get a form headed as the full instrument; falsified by a
  report of a caller whose module form must carry the plain instrument title by
  default.
- 2026-08-23: plan gate chose a shuffle-only control over also exposing
  `renumber`, because a shuffled form with original numbers prints no crosswalk
  and cannot be scored from the paper alone; falsified by a user needing
  original numbering on a shuffled module form.
- 2026-08-23: implementation gate — the app change is held unpushed until the
  milestone is approved at review (its `main` publishes straight to the live
  page), and the shuffled Word download keeps the planned filename; naming it
  distinctly was offered and declined as a scope widening, captured as a
  candidate row instead.
- 2026-08-23: T1 — `title` defaults to `NULL` and resolves after
  `resolve_module_arg()`, so a deprecated `subset =` caller gets the module
  header too; an explicit `title` (including one equal to the other default) is
  never replaced. `validate_string(allow_null = TRUE)` guards the new sentinel.
- 2026-08-23: T2 — `tests/testthat/test-docx-title.R` (16 assertions) reads the
  header back out of `word/header1.xml` through the new `docx_header_title()`
  helper; the AC3 case rebuilds the full instrument at both paper sizes and
  compares title and item rows against the committed `inst/extdata` forms. The
  files landed in the T1 commit rather than their own. Suite clean: 13794 pass,
  0 fail, 1 skip.
- 2026-08-23: T3 — `jmgirard/hitop-builder` commit `5e78f98` adds the Word-only
  shuffle checkbox in its own `Word item order` fieldset, the ticked-only
  warning, and the app README section; `randomize` is interpolated into the
  DOCX call alone. Committed locally and deliberately not pushed (that repo's
  `main` publishes to the live page).
- 2026-08-23: review — all seven criteria verified with fresh evidence at
  `f3ea7f0`; consistency gate clean; three lenses run inline rather than in
  fresh-context subagents (session configured not to spawn agents), four
  findings, none an acceptance-criterion failure.

## Decisions

## Review

Fresh evidence, 2026-08-23, at `f3ea7f0` on `m048-module-title-and-shuffle-toggle`
(branch even with `origin/main`; PR #54).

### Acceptance criteria

- AC1: `testthat::test_local(filter = "docx-title")` — the module default, the
  full-instrument default, `module = NULL`, and the deprecated `subset =` path
  each read back their expected header out of `word/header1.xml` of a freshly
  built document. 16 assertions in the file, all pass, no skips.
- AC2: the same run — `title = "Anxiety Screener, Wave 2"` prints verbatim on a
  module form and on a full-instrument form, and the discriminating case
  (`module` supplied with `title = "HiTOP-SR (v1.0)"`, the *other* default)
  prints what was passed, so the resolver keys on `is.null(title)` and not on
  the header text.
- AC3: the same run's last test rebuilds the full instrument at `us` and `a4`
  and compares each against the committed `inst/extdata/hitopsr_US.docx` and
  `hitopsr_A4.docx` — same header (`"HiTOP-SR (v1.0)"` in both the rebuild and
  the committed file) and same item rows, neither skipped. `data-raw/artifacts.R:146`
  registers exactly one `generate_docx_hitopsr` spec built at both paper sizes,
  so those two files are the whole set this generator ships.
- AC4: the builder page served locally from `jmgirard/hitop-builder` at
  `5e78f98`, driven through its own controls (three scales, 16 items). Ticked:
  the file carries a 16-pair crosswalk, and every pair's original number equals
  the number recovered independently by matching the printed item text back to
  `hitopsr_items$Text` — 16/16 agree, 0 mismatches. Unticked: no crosswalk
  paragraph, and the independently recovered originals ascend
  (66, 68, 109, ... 389, 390). Printed numbers run 1..16 in both.
- AC5: same session, same three-scale selection. The two Qualtrics downloads are
  byte-identical (`cmp`, 3621 bytes each). The two REDCap archives differ only in
  zip timestamps; entry-for-entry they carry the same single member
  `instrument.csv` with identical 3309-byte content. The page's own log confirms
  `randomize` reaches the DOCX call alone.
- AC6: on the live page the note is hidden with the box unticked, shown on tick,
  hidden again on untick, and its text names the reorder-before-scoring hazard.
  `man/generate_docx_hitopsr.Rd`'s `title` entry states both resolved defaults.
  `NEWS.md:211-219` records the header change; `NEWS.md:35-38` records the app
  control. The builder README's "Shuffling the Word form" section describes the
  control, its Word-only scope, and the crosswalk.
- AC7: `devtools::test()` — 13794 pass, 0 fail, 0 warn, 1 skip.
  `devtools::check()` — Status OK, 0 errors, 0 warnings, 0 notes (5m 36s,
  hitop 0.2.0).

### Consistency gate

`cairn_validate` exit 0 — every check PASS/OK; 48 advisories, all pre-existing
(wrapped work-log lines, dangling `D-0NN` tokens in DESIGN/SOURCES). No
DESIGN.md principle changed in the diff, so `cairn_impact --changed` did not run.
Toolchain slot: `devtools::document()` leaves no diff; `pkgdown::check_pkgdown()`
finds no problems; README.Rmd/README.md untouched by the branch and still at the
same commit; NEWS carries entries for both user-visible changes; no new top-level
file; `devtools::check()` clean.

### Independent review

Ran **inline, not in fresh-context subagents** — this session is configured not
to spawn agents, the same deviation the plan phase logged. Three lenses applied
by hand over distinct evidence: the diff against the criteria, DESIGN and D-037;
`git log`/`blame` on the touched lines against M046's intent; and the archived
`## Review` records for the touched files (`gh api pulls/comments` returned
nothing, so GitHub threads carry no prior-review evidence here).

Blame-history: no regression. D-036's Word-only rule holds — the app passes
`randomize` to the DOCX call alone — and the crosswalk gate
(`randomize && renumber && !is.null(module)`) is untouched, so every shuffled
build the app can produce carries its crosswalk. Prior-review: M046's review
returned "shuffled-form data mis-scoring through `score_hitopsr()` undocumented";
the new control states that hazard in three places, so the lesson is honored.

Findings, ranked, with disposition:

1. NEWS.md now tells readers the builder page has a *Word item order* box, but
   the builder commit `5e78f98` is deliberately unpushed. Merging publishes that
   claim to the pkgdown changelog while the live page has no such control.
   Disposition: gate action — push the builder commit at merge.
2. `vignettes/articles/modules-hitopsr.Rmd` is the article on module forms and
   lists how they differ from a full-instrument form, but says nothing about the
   new module-aware header. Not required by any criterion. Disposition: at the
   gate.
3. `docx_header_title()` returns `""` for a file with no header part rather than
   signaling. Every call site also pins a literal, so no assertion can pass
   vacuously. Disposition: rejected — no failure mode reaches an assertion.
4. The app's console log omits `randomize = FALSE` and `papersize`, so the logged
   call is not quite the call made. Disposition: rejected — pre-existing pattern
   for `papersize`, and the log is a summary line, not a transcript.

