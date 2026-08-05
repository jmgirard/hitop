# M39: Profile plots in the short- and brief-form vignettes

- **Status:** in-progress
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** GP4
- **Branch/PR:** `m39-plot-vignette-coverage` · https://github.com/jmgirard/hitop/pull/42

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
pattern, which is a regex matching nothing it intends to. The clipped top value
label, in both the facet and unfacetted profiles: the label's offset moves from
the vertical rendering direction into the horizontal one, where the continuous
score axis can be padded to hold it, and the discrete scale returns to
ggplot2's default expansion. A `labels` argument for turning the value labels
off.

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
- [ ] AC8. The value label's offset is horizontal rather than vertical, and the
      discrete scale carries ggplot2's default expansion, so the room a label
      needs no longer comes out of the panel's short dimension. Swept: FULL
      facet, SF facet, FULL domain, and BF domain profiles rendered at 7.5x9,
      6x4.5, and 6x1.8 inches — twelve figures, each inspected, none carrying a
      clipped label. A test asserts the label layer carries `hjust` and no
      `vjust`, that no panel's built y range exceeds its scale count by more
      than the default 0.6, and that a respondent whose value sits at the axis
      maximum yields a built label for every plotted scale with no draw-time
      warning. Mutation-checked.
- [ ] AC9. `plot_pid5()` takes a `labels` argument, `TRUE` by default;
      `labels = FALSE` returns a plot carrying no `GeomLabel` layer, with its
      point, line, and reference-line layers unchanged. Tests cover both values
      for one facet and one unfacetted profile; the argument is documented on
      the help page.

## Coverage

- AC1 → T1, T2, T3, T4
- AC2 → T1, T2, T3, T6
- AC3 → T6
- AC4 → T2, T6
- AC5 → T3, T6
- AC6 → T5
- AC7 → T7
- AC8 → T9, T6
- AC9 → T10

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
- [x] T8. Superseded by T9 at the review return — the upper-expansion fix it
      shipped was device-size dependent.
- [ ] T9. Move the label offset from `vjust` to `hjust` in `plot_pid5()`
      (`R/plot_pid5.R:385-390`), pad the continuous score axis to hold it, and
      drop T8's discrete-scale expansion so both branches return to ggplot2's
      default. Replace T8's test per AC8; fix the three below-threshold review
      findings in the area — the comment blaming short panels (F3), the test's
      over-claiming name (F4), and the missing `info = version` (P1).
- [ ] T10. Add the `labels` argument per AC9: signature, guard, roxygen, tests
      for both values, and a NEWS mention. Re-run T6 and T7 afterwards.

## Work log

- 2026-08-04: created by /milestone-plan.
- 2026-08-04: plan gate chose a version-aware `rlang::is_installed("ggplot2", version = "3.4.0")` guard in all three vignettes over copying M38's `requireNamespace("ggplot2")` guard, because the weaker guard lets a chunk evaluate on ggplot2 < 3.4.0 and then abort inside `plot_pid5()`; falsified by a vignette build failing because `rlang` is unavailable at knit time, or by the guard diverging from the D-031 floor.
- 2026-08-04: plan gate chose one-off visual inspection recorded per figure over committing a maintainer render-and-look script and over reopening D-030 for image snapshots, because D-030's stated reopening condition — a purely visual regression leaving layer data unchanged — has not occurred; falsified by any visual defect reaching a release past green structural assertions, which is the same evidence class D-030 names.
- 2026-08-04: branch `m39-plot-vignette-coverage` cut from main @ 206959a; status in-progress.
- 2026-08-04: plan-gate criteria audit ([O], fresh context) returned six findings: the figure-count criterion was unsatisfiable because `html_vignette` self-containment leaves no figure files on disk (fixed by naming `self_contained = FALSE` and clearing `_files/` first), confirming the facet refusal against a rendered figure was unreachable because a refusal renders no figure (fixed by routing it to the named test), three under-specified terms — "plotting chunk", "grep'd", "the points it should" — now name what they mean, and the ggplot2 guard mismatch was routed to the question gate. It also found `.Rbuildignore`'s `^vignettes/*_files$` matches nothing intended, absorbed as AC6/T5.
- 2026-08-04: T1-T5 done. SF gains domain + facet profile sections after its norming section; BF gains a domain profile section with prose on `total` off the line and facet level refused; FULL gains a rendered `metric = "percentile"` chunk (`x2h`). All six `plot_pid5()` chunks across the three vignettes now guard on `rlang::is_installed("ggplot2", version = "3.4.0")`, the M38 pair included. `.Rbuildignore`'s `^vignettes/*_files$` corrected to `^vignettes/.*_files$`; `grepl()` on the committed line returns TRUE for `vignettes/pid5_scoring_files` where the old pattern returned FALSE.
- 2026-08-04: T6 done. All three vignettes rendered with `self_contained = FALSE` after clearing their `_files/` directories; png counts 3 (FULL) / 2 (SF) / 1 (BF) equal the per-vignette `plot_pid5(` chunk counts. All six figures opened and inspected: FULL `x2f` five domains on a T axis, line joins all five; FULL `x2g` 25 facets over six panels sized 3/3/3/3/3/10, lines join within a panel only; FULL `x2h` the same five domains on a percentile axis with labelled breaks 0/25/50/75/100; SF `x9` five domains on a T axis, line joins all five; SF `x10` the same six-panel facet layout for the short form; BF `profile-plot` six scales on a T axis with the line joining the five domains and stopping before Total, whose point is drawn unconnected. Every figure carries one dashed reference line at the metric's midpoint and no bands or annotations. Rendered artifacts deleted afterwards; tree clean.
- 2026-08-04: T7 done. The existing 0.2.0 `plot_pid5()` NEWS bullet's single vignette pointer now names all three scoring vignettes; no new bullet, since this milestone changes no behavior. `devtools::document()` idempotent (only NEWS.md modified after the run), `devtools::test()` FAIL 0 WARN 0 SKIP 1 PASS 11886, `pkgdown::check_pkgdown()` "No problems found.", `devtools::check()` 0 errors 0 warnings 0 notes with vignette re-building OK. Status review.
- 2026-08-04: amendment at Jeff's report that the facet plots clip their top value label. Scope gains the fix, plus AC8 and T8; T6 and T7 unticked for re-run and status returned to in-progress. The defect is in M38's shipped `plot_pid5()`, not in M39's vignette work — the full-form facet figure clips too — and is invisible to layer data because `vjust` is a rendering property, which is the cost D-030 names. Chosen over routing it to `/hotfix` on its own branch, because the branch is already open on these very figures and shipping M39's facet figures clipped was the alternative; falsified if the fix turns out to need changes beyond the discrete scale's expansion. D-030 is not reopened: the guard asserts built panel bounds, which is what D-030 prescribes.
- 2026-08-04: T8 done. `plot_pid5()`'s facet branch gains `scale_y_discrete(expand = expansion(add = c(0.6, 1.1)))` — expand only, never limits, so per-panel training survives. New test *each facet panel reserves room above its top scale for the label* asserts every panel's built upper y bound exceeds its scale count by more than ggplot2's default 0.6, that no panel lists all 25 facets, and that per-panel counts are still 3/3/3/3/3/10, for FULL and SF. Written first and confirmed red (2 failures at 0.600 <= 0.600); mutation-checked after the fix by restoring `add = c(0.6, 0.6)`, which turns it red again.
- 2026-08-04: T6 and T7 re-run after T8. Package reinstalled so the vignettes knit against the fix (verified: installed `plot_pid5()` builds panel 1 upper bound 4.1). Counts again 3/2/1. All six figures re-inspected — both facet figures now show the top label fully inside its panel with membership unchanged, and the four unfacetted figures are as recorded above. NEWS gains a user-visible bug-fix bullet for the clipped label, backed by the T8 test. `devtools::document()` idempotent, `devtools::test()` FAIL 0 WARN 0 SKIP 1 PASS 11912, `pkgdown::check_pkgdown()` clean, `devtools::check()` 0/0/0. Status review.
- 2026-08-04: review returned M39 to in-progress. What failed: the AC8 fix is device-size dependent — `vjust = -0.55` is an absolute label-height offset while the new headroom is 1.1 data units, so at 6x4.5in every facet panel's top label clips again (reproduced independently at review), and `NEWS.md` claims the fix unconditionally. Two more actioned findings ride along: the unfacetted branch has the same defect and no `expand` at all, and the new test's version loop carries no `info = version`. Defect returns on this milestone: 1.
- 2026-08-04: amendment executing the review return. Scope, AC8, and the tasks are re-cut around a horizontal label offset — the vertical direction is the panel's short one and no data-space reservation in it survives a smaller device, whereas the continuous score axis can be padded. Chosen at Jeff's gate over disabling panel clipping (rejected outright) and over reserving the space in physical units (kept in reserve, fiddlier); falsified by a clipped label at any size in AC8's twelve-figure sweep. AC9 adds a `labels` argument: Jeff asked for labels to drop out automatically when they would overlap, which ggplot2 cannot decide at build time because the device size is unknown until draw time, so the argument is the manual form of it and a draw-time geom is left unbuilt. T8 is superseded rather than reopened.
- 2026-08-04: T9/T10 implemented — label offset moved to `hjust = -0.6`, continuous axis padded `mult = c(0.03, 0.12)`, the facet branch's discrete expansion removed, `labels` argument added with `validate_flag()`, roxygen and NEWS updated, and F3/F4/P1 fixed in the area. Three mutations all red: reverting to `vjust` (10 failures), re-adding the discrete expansion (2), dropping the axis padding (2) — the third only after a further assertion was added, since the first form of the test missed it entirely.
- 2026-08-04: AC8's twelve-figure sweep FAILED at figure 8. SF facet at 6x4.5in clips its `100` label to `10` at the right panel edge. Same shape as the defect this milestone already returned once for: the `hjust` offset and the label's width are absolute, while the padding holding them is a proportion of the data range, so it shrinks with panel width. Measured: the worst case (a three-digit value at the axis maximum) fits down to ~7in wide and clips below. `coord_cartesian(clip = "off")` holds at every width tested by drawing the label into the margin. Second failure of AC8 by a new mechanism of the same shape — taken back to Jeff rather than retuned a third time.

## Decisions

## Review

Reviewed 2026-08-04 on `m39-plot-vignette-coverage` @ c85e19d against `main`
@ 206959a. PR: https://github.com/jmgirard/hitop/pull/42 (draft).
**Outcome: returned to `in-progress`** — see "Return" below. No criterion is
ticked; the re-review re-derives all evidence.

### Acceptance criteria — fresh evidence gathered

- **AC1.** `## Profile Plots` present at `pid5sf_scoring.Rmd:121`,
  `pid5bf_scoring.Rmd:82`, `pid5_scoring.Rmd:121`. All six chunks calling
  `plot_pid5()` — FULL `x2f`/`x2g`/`x2h`, SF `x9`/`x10`, BF `profile-plot` —
  carry `eval: !expr rlang::is_installed("ggplot2", version = "3.4.0")`.
- **AC2.** `_files/` cleared, all three rendered with
  `self_contained = FALSE`, no errors; png counts 3 / 2 / 1 equal the
  per-vignette chunk counts from AC1.
- **AC3.** All six figures opened. FULL `x2f`: T axis ~35-93, five domains in
  `pid_domains` order, line joins all five. FULL `x2g`: T axis 25-100, 25
  facets over six panels 3/3/3/3/3/10, lines join within a panel only. FULL
  `x2h`: percentile axis, labelled breaks 0/25/50/75/100, same five domains
  joined. SF `x9`: T axis ~28-93, five domains joined. SF `x10`: T axis
  25-100, same six-panel layout. BF `profile-plot`: T axis ~35-95, six scales,
  line joins the five domains and stops before Total, whose point is drawn
  unconnected. One dashed midpoint line per figure; no bands or annotations.
- **AC4.** BF prose at `pid5bf_scoring.Rmd:100` and `:104` states both claims.
  Tests *the brief form plots six scales and stops the line before total*
  (`test-plot_pid5.R:96`) and *the brief form refuses a facet profile*
  (`:158`) both pass.
- **AC5.** `x2h` present with the AC1 guard; its figure's labelled breaks run
  0 to 100.
- **AC6.** `grepl("^vignettes/.*_files$", "vignettes/pid5_scoring_files")`
  returns `TRUE`.
- **AC7.** `document()` idempotent (no diff), `test()` FAIL 0 WARN 0 SKIP 1
  PASS 11912, `check_pkgdown()` "No problems found.", `check()` 0/0/0.
- **AC8.** Satisfied as written — headroom 1.1 on every panel against the 0.6
  default, panel sizes 3/3/3/3/3/10 for FULL and SF — but F1 below shows the
  criterion's wording is a weaker promise than the defect requires.

### Consistency gate

- Universal: `cairn_validate` exit 0, all checks passed; 23 advisories (the 22
  standing pre-migration `dangling id tokens` plus a `sizing` advisory for the
  eighth criterion added by the amendment). `cairn_impact` skipped — no
  `DESIGN.md` principle changed.
- Toolchain (`r-package` `consistency-gate`): `document()` no diff · generated
  files not hand-edited · README.md in sync (re-knit produced no change) ·
  `check_pkgdown()` clean · NEWS entry present · no new top-level files ·
  `check()` 0 errors, 0 warnings, 0 notes.

### Review findings

16 candidate findings from three fresh-context lenses, scored by an
independent [S] scorer.

**Actioned (>= 80):**
- **F1 (92) — returns the milestone.** The clipped-label fix is device-size
  dependent. `vjust = -0.55` offsets the label by an absolute label-height,
  while the new headroom is 1.1 *data units*, whose physical size shrinks with
  panel height. Reproduced independently: at 7.5x9in (the vignette size)
  labels fit; at 6x4.5in every panel's top label is clipped again, the
  10-facet panel included. `NEWS.md` claims the clipping is fixed
  unconditionally, which a user drawing into a smaller device falsifies.
- **P1 (85) — fix next pass.** The new test's version loop calls `expect_gt`
  and `expect_lt` with no `info = version`, so a failure does not name FULL or
  SF. Regresses the standing ROADMAP candidate row on exactly this file, and
  is locally inconsistent with the sibling test directly above it.
- **F2 (80) — fix next pass.** The unfacetted branch has the same defect and
  was left untouched: its `scale_y_discrete(limits = rev(labels))` sets no
  `expand`, keeping the 0.6 default. Reproduced: the FULL domain profile
  clips its top label at 6x1.8in.

**Below threshold, logged not actioned (13):** F3 (78) the code comment's
causal claim that only short panels clip is wrong — the 10-facet panel clips
too; F4 (75) the new test asserts "not the ggplot2 default", not "room for the
label", so its name over-claims; F9 (65) AC3's evidence lines were in the work
log rather than the Review section — recorded here now; F7 (55) the assertion
path reads `panel_params` (a ViewScale), unverified at the D-031 floor of
ggplot2 3.4.0; F12 (55) the new vignette prose glosses the reference line as
"the normative sample's mean" where the code says midpoint; F5 (45) the
literal `0.6` hardcodes ggplot2's current default expansion; F6 (45) two
assertions duplicate the preceding test and hardcode `c(3,3,3,3,3,10)`;
F10 (35) the NEWS bullet documents an intra-release regression on a function
that has not shipped; F13 (25), F11 (20), B1 (15), B2 (12) — pre-existing or
inherited patterns this diff did not introduce. F8 (20) claimed the work log's
"2 failures" red-run evidence was inconsistent with a 12-assertion loop; the
scorer reproduced the mutation and got exactly 2 failures, matching the log.

### Return

Returned to `in-progress` under the return floor: F1 scores 92 on a defect in
what `plot_pid5()` does for its users. The fix must hold independent of device
size, and AC8's wording — which the current fix satisfies while the defect
stands — needs a gated amendment in the same pass. F2 and P1 ride along.
Defect returns on this milestone: 1.

