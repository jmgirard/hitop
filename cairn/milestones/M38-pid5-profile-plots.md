# M38: Norm-referenced PID-5 profile plots

- **Status:** in-progress
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** IP2, IP4, GP3, GP4
- **Branch/PR:** `m38-pid5-profile-plots`

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

- [ ] AC1. `plot_pid5()` is exported with a roxygen page whose examples are
      guarded `@examplesIf requireNamespace("ggplot2", quietly = TRUE)`
      (precedent `R/reliability.R:123`), and carries a `_pkgdown.yml`
      reference-index row.
- [ ] AC2. With `level = "domain"` and `metric = "t"`, the returned plot's
      `GeomPoint` layer built data has exactly 5 rows for FULL and SF, ordered
      by `pid_domains$camelCase`, and exactly 6 rows for BF, ordered by
      `pid_scales[["BF"]]$camelCase` (which ends on `total` and is *not*
      `pid_domains` order); each row's y value equals the corresponding `_t`
      input column. On BF the `GeomLine` layer's built data covers only the
      five domains, so the profile line stops before `total`.
- [ ] AC3. With `level = "facet"` on FULL or SF, the `GeomPoint` layer built
      data has exactly 25 rows distributed over 5 panels matching
      `pid_domains$facetStems`; `level = "facet"` with `version = "BF"` aborts
      via `cli::cli_abort()` in a message stating the brief form has no facet
      scores.
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

- [ ] T1. `R/plot_pid5.R`: signature, `version`/`level`/`metric` via
      `match.arg()` after `toupper()` where the package does, argument checks
      through `R/util.R`'s `validate_*()` family, and the
      `rlang::is_installed("ggplot2")` guard.
- [ ] T2. Internal helper mapping `version` + `level` to expected column names
      by **pasting** `prefix` onto the camelCase stems from
      `pid_scales`/`pid_domains` and appending `_t`/`_ptl` — the direction
      `norm_pid5()` names its outputs (D-026 governs stripping, not pasting).
- [ ] T3. Domain profile on the T axis: points, connecting line, value labels,
      reference line, breaks and limits derived from `pid_norms`.
- [ ] T4. Facet profile: 5 panels keyed on `pid_domains$facetStems` (a
      list-column — unlist into a per-facet domain factor).
- [ ] T5. Percentile metric: `_ptl` columns, percentile axis, median reference.
- [ ] T6. Input branches: multi-row abort, absent-column abort, `NA`-drop with
      the `cli_warn()` report.
- [ ] T7. Tests: structural `ggplot_build()` assertions for AC2–AC5, one test
      per branch for AC6.
- [ ] T8. Docs: roxygen + guarded examples, `_pkgdown.yml` row, vignette
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

## Decisions

## Review
