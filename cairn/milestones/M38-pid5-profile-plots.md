# M38: Norm-referenced PID-5 profile plots

- **Status:** in-progress
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** IP2, IP4, GP3, GP4
- **Branch/PR:** `m38-pid5-profile-plots` / https://github.com/jmgirard/hitop/pull/41

## Goal

Give researchers a `plot_pid5()` that renders one respondent's `norm_pid5()`
output as a profile against the norming tables' own metric, presenting scores
without characterizing them.

## Scope

**In:** one exported `plot_pid5(data, version, level, metric, prefix)`
returning a ggplot object; domain profiles for FULL/SF/BF and facet profiles
for FULL/SF; a `metric` argument selecting the T-score or percentile axis;
ggplot2 guarded through Suggests per the `calc_omega()` pattern; structural
tests over `ggplot2::ggplot_build()` layer data; roxygen page, `_pkgdown.yml`
row, vignette section, NEWS entry, README checkbox.

**Out:**
- Interpretive severity bands or elevation thresholds — no cited source
  supplies boundaries (IP2), and shading a region by severity characterizes a
  score (IP4). → D-029; a candidate row carries the alternative.
- Confidence intervals from `calc_se` standard errors — the SEs are in raw
  units and no published rule converts them to the T metric. → candidate row.
- Multi-respondent overlay or faceting. → candidate row.
- Raw-score axes — would surface the unresolved A–6/A–8 raw-ceiling anomaly
  (`references/markon2024.md`, open questions). → stays with that question.
- HiTOP-SR/BR profile plots — those instruments ship no norms yet. → the
  existing HiTOP-SR/BR norms candidate row.
- An `append` argument: this family returns a ggplot, not a tibble, so the
  convention does not apply.

## Acceptance criteria

- [x] AC1. `plot_pid5()` is exported with a roxygen page whose examples are
      guarded `@examplesIf requireNamespace("ggplot2", quietly = TRUE)`
      (precedent `R/reliability.R:123`), and carries a `_pkgdown.yml`
      reference-index row.
- [x] AC2. With `level = "domain"` and `metric = "t"`, the returned plot's
      `GeomPoint` layer built data has exactly 5 rows for FULL and SF, ordered
      by `pid_domains$camelCase`, and exactly 6 rows for BF, ordered by
      `pid_scales[["BF"]]$camelCase` (which ends on `total` and is *not*
      `pid_domains` order); each row's y value equals the corresponding `_t`
      input column. On BF the `GeomLine` layer's built data covers only the
      five domains, so the profile line stops before `total`.
- [ ] AC3. With `level = "facet"` on FULL or SF, the `GeomPoint` layer built
      data has exactly 25 rows distributed over 6 panels: one per domain
      holding that domain's 3 defining facets from `pid_domains$facetStems`
      (15 facets, in `pid_domains` order), and a final panel holding the 10
      facets the APA key assigns to no domain, labelled as such.
      `level = "facet"` with `version = "BF"` aborts via `cli::cli_abort()` in
      a message stating the brief form has no facet scores.
- [ ] AC4. With `metric = "percentile"`, the `GeomPoint` layer's y values equal
      the corresponding `_ptl` input columns multiplied by 100 (`norm_pid5()`
      returns `_ptl` as a proportion, verified 0-1 across `pid_norms`), the
      axis spans 0-100, and the reference line sits at 50. The help page states
      the rescaling.
- [ ] AC5. The plot carries no interpretive furniture: its built layers include
      no `GeomRect`/`GeomTile`, exactly one `GeomHline` (or `GeomVline` after
      `coord_flip()`), and exactly one text/label layer whose `label` values are
      `setequal()` to the plotted score values — so the reference line carries
      no text label and no band or threshold annotation can ship (IP4). Axis
      breaks and limits derive from the plotted scales' own rows in `pid_norms`,
      introducing no boundary constant of this package's invention (IP2).
- [ ] AC6. Each input branch fires in its own test: `nrow(data) != 1` aborts
      naming the row count; a requested `_t`/`_ptl` column absent from `data`
      aborts through the `validate_*()` family as the rest of the package does;
      a present-but-`NA` score is dropped from the series with a
      `cli::cli_warn()` naming every dropped scale while the plot still renders;
      and ggplot2 unavailable aborts informatively, verified by mocking
      `rlang::is_installed()` (the binding `calc_omega()` uses at
      `R/reliability.R:132` — *not* `check_installed()`, which never consults it).
- [ ] AC7. A PID-5 vignette section renders the score → validity → norm → plot
      pipeline under the same `requireNamespace()` guard, NEWS.md carries a
      user-visible entry, and the profile's `verify` slot is clean.

## Coverage

- AC1 → T8
- AC2 → T2, T3, T7
- AC3 → T2, T4, T7
- AC4 → T5, T7
- AC5 → T3, T4, T7
- AC6 → T1, T6, T7
- AC7 → T8

## Tasks

- [x] T1. `R/plot_pid5.R`: signature, `version`/`level`/`metric` via
      `match.arg()` after `toupper()` where the package does, argument checks
      through `R/util.R`'s `validate_*()` family, and the
      `rlang::is_installed("ggplot2")` guard.
- [x] T2. Internal helper mapping `version` + `level` to expected column names
      by **pasting** `prefix` onto the camelCase stems from
      `pid_scales`/`pid_domains` and appending `_t`/`_ptl` — the direction
      `norm_pid5()` names its outputs (D-026 governs stripping, not pasting).
- [x] T3. Domain profile on the T axis: points, connecting line, value labels,
      reference line, breaks and limits derived from `pid_norms`.
- [x] T4. Facet profile: 6 panels keyed on `pid_domains$facetStems` (a
      list-column — unlist into a per-facet domain factor), the sixth holding
      the 10 facets the key assigns to no domain.
- [x] T5. Percentile metric: `_ptl` columns, percentile axis, median reference.
- [x] T6. Input branches: multi-row abort, absent-column abort, `NA`-drop with
      the `cli_warn()` report.
- [x] T7. Tests: structural `ggplot_build()` assertions for AC2–AC5, one test
      per branch for AC6.
- [x] T8. Docs: roxygen + guarded examples, `_pkgdown.yml` row, vignette
      section, NEWS entry, README checkbox, `devtools::document()` +
      `devtools::build_readme()`.

## Work log

- 2026-08-01: created by /milestone-plan.
- 2026-08-01: branch `m38-pid5-profile-plots` cut from main; status planned -> in-progress.
- 2026-08-01: probed installed ggplot2 4.0.3 -- `ggplot_build()` still exposes per-layer `$data` and `$layout$layout`, so D-030's structural-assertion mechanism holds on the installed version.
- 2026-08-01: implement gate amended AC4 -- `norm_pid5()` returns `_ptl` as a proportion (0-1, verified across `pid_norms`), which the plan had left as "whatever units it carries"; the percentile axis rescales to 0-100 for readability (GP3) and the help page states it.
- 2026-08-01: implement gate amended AC2 -- the brief form's `total` is an overall elevation, not a sixth domain, so the profile line stops before it while the point is still plotted.
- 2026-08-01: plan-gate criteria audit ([O], fresh context) returned 17 findings; 13 fixed pre-gate (unworkable `check_installed()` mock, undefined "N plotted positions" over multi-layer build data, missing BF criterion and its differing scale order, wrong abort rationale, self-falsifying AC4 prose, unmechanizable text-layer test, unreachable uncovered-scale branch, D-026 cited for the wrong direction, unguarded `@examples`, AC7 restating the review gate, spurious AC1→T1 coverage, undefined axis extent, undefined missing-column behavior), 4 routed to the gate.
- 2026-08-01: plan gate chose no severity bands over porting the prototype's green/red bands because no source in `references/` or `SOURCES.md` supplies boundaries (IP2) and shading by severity characterizes a score (IP4); falsified by a published, citable set of PID-5 profile elevation thresholds.
- 2026-08-01: plan gate chose structural `ggplot_build()` assertions over `vdiffr` snapshots because they need no new dependency and survive a cosmetic restyle; falsified by a visual regression that ships with every structural assertion still green.
- 2026-08-01: plan gate chose aborting on multi-row input over a row-selector argument because the package validates loudly everywhere else; falsified by a user needing a group profile before the multi-respondent candidate is promoted.
- 2026-08-01: plan chose a fixed axis span derived from the plotted scales' `pid_norms` rows over a data-driven span, autonomously, because a norm-referenced profile whose axis rescales per respondent is not comparable across respondents; falsified by a profile whose scores fall outside the tables' printed span.
- 2026-08-01: T1-T7 done -- `R/plot_pid5.R` plus `tests/testthat/test-plot_pid5.R` (93 assertions); full suite FAIL 0 WARN 0 SKIP 1 PASS 11838.
- 2026-08-01: implement gate amended AC3 -- `pid_domains$facetStems` is the APA key's domain-DEFINING map (3 per domain, 15 of 25), not a full 25->5 assignment, so "25 facets over 5 panels" was unsatisfiable; `pid_items$Domain` is broader but tags 6 facets under more than one domain, so it cannot key panels either. Jeff chose 6 panels: 5 domains plus the 10 facets the key assigns to no domain, labelled "Not domain-defining".
- 2026-08-01: two test-mechanism corrections found by running them -- after `coord_flip()` the value axis is the pre-flip `y` scale (the built panel's `x.range`), and the built range carries ggplot2's default 5% expansion, so axis-bound assertions read `layer_scales(p)$y$get_limits()` rather than a panel range.
- 2026-08-01: T8 docs -- `_pkgdown.yml` gains a Plots section, `pid5_scoring.Rmd` gains a Profile Plots section (renders), NEWS entry added, README's three stale Phase-2 boxes ticked (norms and norming functions had shipped at M25/M27/M33 and were still marked todo) and a Phase-3 HiTOP visualization row added; `build_readme()` re-knitted; `check_pkgdown()` clean.
- 2026-08-01: mutation-checked the four load-bearing guards -- joining BF's total to the profile line, dropping the percentile rescale, adding a red band above T=70, and folding the unassigned facets into a domain panel each turn the intended assertion red (LESSONS M36: a guard not re-run against its named mutation asserts nothing).
- 2026-08-01: a visual check caught what the structural tests cannot (the cost D-030 accepted): `coord_flip()` applies a facet's free scale BEFORE the flip, so every panel of the facet profile drew all 25 scale names on top of one another. Rebuilt with scores on x and scale names on y directly -- no `coord_flip()` -- and `facet_grid(space = "free_y")` so the 10-facet panel is not crushed into the height a 3-facet panel gets.
- 2026-08-01: second defect from the same pass -- a discrete scale trained across layers places a value missing from an earlier layer LAST, so the brief form's `total` (absent from the profile-line layer by design) drew at the top instead of the bottom; `scale_y_discrete(limits = ...)` pins the positions. A test now asserts the drawn order from built coordinates rather than from factor level order, which run opposite ways.
- 2026-08-01: axis labels switched from column stems to the tables' printed names (`pid_scales$Facet`, `pid_domains$Domain`); tests repointed to a `stem` column so they still assert canonical scale names.
- 2026-08-01: AC5 reads "exactly one `GeomHline` (or `GeomVline` after `coord_flip()`)" -- the build uses `GeomVline` with NO `coord_flip()`, since dropping the flip is what fixed the facet layout. The criterion's substance (exactly one reference line, no rectangle layer, one text layer) is met and the test asserts `sum(geoms %in% c("GeomHline", "GeomVline")) == 1`; only the parenthetical's stated reason is now historical.
- 2026-08-01: added a non-numeric-column abort for parity with `norm_pid5()` -- a character column would coerce to `NA` and be reported by the NA-drop branch as "no value", hiding a type mistake behind a missing-data warning.
- 2026-08-01: CHECKPOINT -- T8's content is written (pkgdown row, vignette section, NEWS, README, roxygen) and the plot file's own 109 assertions pass, but T8 stays unchecked until the full `devtools::test()` and `devtools::check()` are confirmed clean after the axis refactor.
- 2026-08-01: T8 done. Final verification on the committed tree: `devtools::test()` failed=0 errors=0 passed=11854; `devtools::check()` 0 errors / 0 warnings / 0 notes; `devtools::document()` idempotent; `pkgdown::check_pkgdown()` clean; `pid5_scoring.Rmd` renders. Status in-progress -> review.
- 2026-08-01: the first `check()` after the axis refactor ran against a stale `plot_pid5.Rd` (roxygen was edited after the preceding `document()`); the Rd was regenerated, committed, and `check()` re-run on the final tree rather than carrying the earlier result forward.
- 2026-08-01: review — all 7 acceptance criteria ticked against fresh evidence; consistency gate green (cairn_validate exit 0, check 0/0/0, document idempotent, README in sync, check_pkgdown clean); draft PR #41 opened, CI running.
- 2026-08-01: review — [S] blame-history lens returned zero findings: the README boxes are documented doc drift (unticked since M12 though norms shipped at M25/M27/M33), NEWS ordering/format and the new pkgdown section follow existing convention with no milestone ids in user-facing text, and D-002/D-029/D-030 are respected.
- 2026-08-01: REVIEW RETURN 1 (review -> in-progress). [O] diff-bug lens found a blocker I reproduced: `scale_y_discrete(limits = rev(labels))`, added late to fix the brief form's `total` ordering, overrides per-panel scale training, so `facet_grid(scales = "free_y", space = "free_y")` stopped working -- all six panels carry all 25 y limits and draw all 25 facet labels overlapping, at equal panel heights. It re-broke the exact defect the earlier `coord_flip()` removal had fixed, because no visual re-check followed the second fix. CI (7 jobs incl. pkgdown) passed green over it, as did all 109 structural assertions.
- 2026-08-01: AC3-AC7 unticked -- their evidence was gathered against the broken build and is void.
- 2026-08-01: fixed the blocker -- the discrete-limits pin now applies only to the unfacetted branch (the facet branch must let `free_y` train per panel); a new test asserts per-panel axis content (3/3/3/3/3/10) and is mutation-confirmed red against the unconditional pin.
- 2026-08-01: my first fix for the hidden-point finding introduced a worse defect -- nudging the label in DATA space pushed a percentile of 98 to 102.5, past the axis limit, and ggplot2 dropped 2 of 6 labels with only a draw-time warning. Replaced with a `vjust` offset (a rendering property, so it cannot move a value off the axis); two guards added, both mutation-confirmed red.
- 2026-08-01: lost the first round of these fixes to a `git checkout --` used to revert a mutation while they were still uncommitted; reapplied and now commit before any mutation run.
- 2026-08-01: [S] scorer results over 23 findings -- 2 at or above the 80 threshold (F1 facet blocker 97, F13 label hides point 88), both fixed. Nine below-threshold findings also fixed while in the area (F2/F3 false comments 76/74, F4 hardcoded percentile breaks 60, F5 axis from survivors 68, F6 unguarded seq/absent-stem 22, F10 weak label assertion 55, F11 non-halting helper expectation 78, F19 duplicated literal 42, F20 vignette warnings 78).

## Decisions

## Review

Reviewed 2026-08-01 on `m38-pid5-profile-plots` @ 028dd01 (+ PR-URL header edit),
against `main` @ 2d37210. PR: https://github.com/jmgirard/hitop/pull/41

### Acceptance criteria — fresh evidence

- **AC1.** `grep '^export(plot_pid5)$' NAMESPACE` matches; `man/plot_pid5.Rd:82`
  carries the `examplesIf` `requireNamespace("ggplot2", ...)` wrapper roxygen
  generated from the guarded block; `_pkgdown.yml:68-70` holds a `Plots` section
  listing `plot_pid5`; `pkgdown::check_pkgdown()` reports "No problems found."
- **AC2.** Tests *domain profiles plot the five domains in pid_domains order*
  (10 assertions), *the brief form plots six scales and stops the line before
  total* (7), *full and short domain profiles join all five points* (4) — all
  pass. Point counts 5/5/6, orders asserted against `pid_domains$camelCase` and
  `pid_scales[["BF"]]$camelCase`, values against the `_t` inputs, and the BF
  `GeomLine` layer at 5 rows. Mutation-checked: joining `total` to the profile
  line turns the BF test red.
- **AC3.** Tests *facet profiles plot 25 facets over six panels* (8) and *each
  panel holds the facets the APA key assigns to it* (12) pass — 25 `GeomPoint`
  rows over 6 panels, each domain panel `setequal()` to its
  `pid_domains$facetStems` triple, the sixth holding exactly the 10 leftover
  facets. *the brief form refuses a facet profile* (1) confirms the abort.
  Mutation-checked: folding the leftovers into a domain panel turns both red.
- **AC4.** Test *the percentile metric plots proportions rescaled to 0-100*
  (6) passes: plotted values equal `_ptl * 100`, `value/100` round-trips to the
  raw proportion, axis limits `c(0, 100)`, reference line `xintercept == 50`.
  Mutation-checked: dropping the `* 100` turns it red.
- **AC5.** Test *the plot carries no bands, thresholds, or extra annotation*
  (20 assertions across four version/level/metric combinations) passes: no
  `GeomRect`/`GeomTile`/`GeomRibbon`/`GeomArea`, exactly one reference line,
  exactly one text layer whose labels are `setequal()` to the plotted values.
  *axis bounds come from pid_norms rather than a chosen constant* (5) passes,
  comparing scale limits against `range(pid_norms$tscore)` per version.
  Mutation-checked: a red band above T = 70 turns the first red on all four
  combinations. NOTE the criterion's parenthetical says "or `GeomVline` after
  `coord_flip()`" — the build uses `GeomVline` with no `coord_flip()` (removing
  the flip is what fixed the facet layout), so the substance is met and the
  test accepts either geom; only the parenthetical's stated reason is
  historical. Recorded, not reinterpreted.
- **AC6.** One test per branch, all passing: *more than one respondent is
  refused* (3, covering 3 rows and 0 rows), *un-normed data is an error, not a
  warning* (2), *a scale with no value is dropped with a warning and the rest
  plot* (4), *ggplot2 being unavailable is reported* (1, mocking
  `rlang::is_installed` — the binding the guard actually consults;
  `check_installed()` appears nowhere in the code path). Two branches beyond
  the criterion also covered: all-values-`NA` aborts (1), non-numeric column
  aborts (2).
- **AC7.** `rmarkdown::render("vignettes/pid5_scoring.Rmd")` completes; NEWS.md
  carries a user-visible `plot_pid5()` entry with no milestone ids; profile
  `verify` slot clean — `devtools::document()` idempotent (no diff on a second
  run) and `devtools::test()` failed=0 errors=0 passed=11854.

### Consistency gate

- Universal: `cairn_validate` exit 0 — all 16 PASS checks green including
  `coverage complete` and `weight caps`; 20 `dangling id tokens` advisories are
  the standing pre-migration references in DESIGN.md/SOURCES.md, unchanged by
  this milestone. `cairn_impact` skipped: no `DESIGN.md` principle changed
  (file not in the diff).
- Toolchain (`r-package` `consistency-gate`): `document()` no diff · generated
  files not hand-edited · README.md in sync with README.Rmd (re-knit produced
  no change) · `check_pkgdown()` clean · NEWS entry present · no new top-level
  files needing `.Rbuildignore` · `devtools::check()` 0 errors, 0 warnings,
  0 notes on the committed tree.
- Thrash count: zero returns to `in-progress`. Two in-gate criterion amendments
  (AC2, AC4) and one mid-implementation amendment (AC3) — amendments, not
  returns.
