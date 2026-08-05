# M39: Profile plots in the short- and brief-form vignettes

- **Status:** review
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** GP4
- **Branch/PR:** `m39-plot-vignette-coverage`

## Goal

Give every PID-5 form's scoring vignette a rendered profile plot, so the
plotting surface is demonstrated for the short and brief forms rather than for
the full form alone.

## Scope

**In:** A `## Profile Plots` section in `vignettes/pid5sf_scoring.Rmd` (domain
and facet examples) and `vignettes/pid5bf_scoring.Rmd` (domain example, plus
prose on `total` sitting off the profile line and facet plots being refused for
that form). A rendered `metric = "percentile"` chunk in
`vignettes/pid5_scoring.Rmd`, replacing its prose-only mention. A version-aware
ggplot2 guard on every `plot_pid5()` chunk in all three vignettes, including
the two that shipped at M38. Visual inspection of every figure the three
vignettes render, recorded per figure. The `.Rbuildignore` vignette-figure
pattern, which is a regex matching nothing it intends to.

**Out:** `rank_scales()` vignette coverage → candidate row (it is a ranking
helper, not a plot, and would widen this milestone). Image-snapshot regression
testing → D-030 stands; its own reopening condition is a purely visual
regression that leaves layer data unchanged, and none has occurred. Severity
bands, confidence intervals, and multi-respondent profiles → existing candidate
rows. HiTOP-SR/BR profile plots → no plotting function exists for them.

## Acceptance criteria

- [ ] AC1. `vignettes/pid5sf_scoring.Rmd` and `vignettes/pid5bf_scoring.Rmd`
      each contain a `## Profile Plots` section, and every chunk calling
      `plot_pid5()` in the three PID-5 scoring vignettes carries the guard
      `#| eval: !expr rlang::is_installed("ggplot2", version = "3.4.0")` —
      domain enumerated by grepping `plot_pid5(` across
      `vignettes/pid5*_scoring.Rmd` and reading each matched chunk's header.
- [ ] AC2. With each vignette's `_files/` directory removed first and
      ggplot2 >= 3.4.0 installed,
      `rmarkdown::render(f, output_options = list(self_contained = FALSE))`
      completes with no error on each of the three vignettes, and for each one
      the count of `.png` files written under its `_files/` directory equals
      the count of `plot_pid5(` chunks AC1 enumerates for that vignette.
- [ ] AC3. Every `.png` file AC2's renders write is opened and visually
      inspected, and the Review section records one line per file naming the
      axis metric drawn, the scales on the scale axis, and whether the profile
      line joins the points `plot_pid5()`'s `@details` says it should (the
      brief form's line stops before `total`; facet lines join within a panel).
- [ ] AC4. The brief-form vignette states in prose that `total` is plotted but
      not joined to the profile line, and that `level = "facet"` is refused for
      the brief form. The first is confirmed against the rendered brief-form
      figure inspected under AC3 and against the passing test *the brief form
      plots six scales and stops the line before total*
      (`tests/testthat/test-plot_pid5.R:96`); the second against the passing
      test *the brief form refuses a facet profile*
      (`tests/testthat/test-plot_pid5.R:158`) alone, since a refusal renders no
      figure.
- [ ] AC5. `vignettes/pid5_scoring.Rmd` carries a `metric = "percentile"` chunk
      bearing AC1's guard, and its figure's labelled value-axis breaks are
      confirmed by inspection to run 0 to 100 — the percentile span `pid_norms`
      prints for the plotted scales, not a constant chosen here.
- [ ] AC6. `grepl(<the .Rbuildignore vignette-figure pattern>,
      "vignettes/pid5_scoring_files")` returns `TRUE`.
- [ ] AC7. The `r-package` profile's `verify` slot is clean —
      `devtools::document()` idempotent, `devtools::test()` 0 failures and
      0 errors — and `devtools::check()` reports 0 errors, 0 warnings, 0 notes
      with `pkgdown::check_pkgdown()` reporting no problems.

## Coverage

- AC1 → T1, T2, T3, T4
- AC2 → T1, T2, T3, T6
- AC3 → T6
- AC4 → T2, T6
- AC5 → T3, T6
- AC6 → T5
- AC7 → T7

## Tasks

- [x] T1. Add a `## Profile Plots` section to `vignettes/pid5sf_scoring.Rmd`
      after its norming section (`vignettes/pid5sf_scoring.Rmd:119`), with
      rendered domain and facet chunks mirroring
      `vignettes/pid5_scoring.Rmd:123-158`.
- [x] T2. Add a `## Profile Plots` section to `vignettes/pid5bf_scoring.Rmd`
      after its norming section (`vignettes/pid5bf_scoring.Rmd:80`): a rendered
      domain chunk, plus prose on `total` being plotted off the profile line
      and on `level = "facet"` being refused for this form.
- [x] T3. Replace the prose-only percentile mention at
      `vignettes/pid5_scoring.Rmd:158` with a rendered
      `metric = "percentile"` chunk.
- [x] T4. Switch every `plot_pid5()` chunk in the three vignettes to the
      version-aware guard, including the shipped full-form chunks `x2f`
      (`vignettes/pid5_scoring.Rmd:139`) and `x2g`
      (`vignettes/pid5_scoring.Rmd:152`).
- [x] T5. Fix `.Rbuildignore`'s `^vignettes/*_files$` to `^vignettes/.*_files$`.
- [x] T6. Render the three vignettes non-self-contained into a scratch tree,
      open every figure, and record one inspection line per figure; clean up
      the `_files/` directories afterwards.
- [x] T7. NEWS entry for the new vignette sections; run the `verify` slot,
      `devtools::check()`, and `pkgdown::check_pkgdown()`.

## Work log

- 2026-08-04: created by /milestone-plan.
- 2026-08-04: plan gate chose a version-aware `rlang::is_installed("ggplot2", version = "3.4.0")` guard in all three vignettes over copying M38's `requireNamespace("ggplot2")` guard, because the weaker guard lets a chunk evaluate on ggplot2 < 3.4.0 and then abort inside `plot_pid5()`; falsified by a vignette build failing because `rlang` is unavailable at knit time, or by the guard diverging from the D-031 floor.
- 2026-08-04: plan gate chose one-off visual inspection recorded per figure over committing a maintainer render-and-look script and over reopening D-030 for image snapshots, because D-030's stated reopening condition — a purely visual regression leaving layer data unchanged — has not occurred; falsified by any visual defect reaching a release past green structural assertions, which is the same evidence class D-030 names.
- 2026-08-04: branch `m39-plot-vignette-coverage` cut from main @ 206959a; status in-progress.
- 2026-08-04: plan-gate criteria audit ([O], fresh context) returned six findings: the figure-count criterion was unsatisfiable because `html_vignette` self-containment leaves no figure files on disk (fixed by naming `self_contained = FALSE` and clearing `_files/` first), confirming the facet refusal against a rendered figure was unreachable because a refusal renders no figure (fixed by routing it to the named test), three under-specified terms — "plotting chunk", "grep'd", "the points it should" — now name what they mean, and the ggplot2 guard mismatch was routed to the question gate. It also found `.Rbuildignore`'s `^vignettes/*_files$` matches nothing intended, absorbed as AC6/T5.
- 2026-08-04: T1-T5 done. SF gains domain + facet profile sections after its norming section; BF gains a domain profile section with prose on `total` off the line and facet level refused; FULL gains a rendered `metric = "percentile"` chunk (`x2h`). All six `plot_pid5()` chunks across the three vignettes now guard on `rlang::is_installed("ggplot2", version = "3.4.0")`, the M38 pair included. `.Rbuildignore`'s `^vignettes/*_files$` corrected to `^vignettes/.*_files$`; `grepl()` on the committed line returns TRUE for `vignettes/pid5_scoring_files` where the old pattern returned FALSE.
- 2026-08-04: T6 done. All three vignettes rendered with `self_contained = FALSE` after clearing their `_files/` directories; png counts 3 (FULL) / 2 (SF) / 1 (BF) equal the per-vignette `plot_pid5(` chunk counts. All six figures opened and inspected: FULL `x2f` five domains on a T axis, line joins all five; FULL `x2g` 25 facets over six panels sized 3/3/3/3/3/10, lines join within a panel only; FULL `x2h` the same five domains on a percentile axis with labelled breaks 0/25/50/75/100; SF `x9` five domains on a T axis, line joins all five; SF `x10` the same six-panel facet layout for the short form; BF `profile-plot` six scales on a T axis with the line joining the five domains and stopping before Total, whose point is drawn unconnected. Every figure carries one dashed reference line at the metric's midpoint and no bands or annotations. Rendered artifacts deleted afterwards; tree clean.
- 2026-08-04: T7 done. The existing 0.2.0 `plot_pid5()` NEWS bullet's single vignette pointer now names all three scoring vignettes; no new bullet, since this milestone changes no behavior. `devtools::document()` idempotent (only NEWS.md modified after the run), `devtools::test()` FAIL 0 WARN 0 SKIP 1 PASS 11886, `pkgdown::check_pkgdown()` "No problems found.", `devtools::check()` 0 errors 0 warnings 0 notes with vignette re-building OK. Status review.

## Decisions

## Review
