# M38: Norm-referenced PID-5 profile plots

**Status:** done (2026-08-01, PR #41 https://github.com/jmgirard/hitop/pull/41)

**Goal:** Give researchers a `plot_pid5()` that renders one respondent's `norm_pid5()` output as a profile against the norming tables' own metric, presenting scores without characterizing them.

**Outcome:** New exported `plot_pid5(data, version, level, metric, prefix)` returning a ggplot object. Domain profiles for FULL/SF/BF, with the brief form's `total` plotted but excluded from the profile line; facet profiles for FULL/SF placing all 25 facets in six panels via `pid_domains$facetStems` (the APA key's domain-defining map covers 15, so the other 10 share a "Not domain-defining" panel), sized by `facet_grid(space = "free_y")`. `metric` selects a T or percentile axis, the latter rescaled from `norm_pid5()`'s proportion to 0-100. Axis limits and stepped breaks derive from the plotted version's `pid_norms` rows over the full scale set. Scales carry printed names and run top-down in table order. Helpers: `plot_scale_stems()`, `plot_scale_labels()`, `plot_facet_domains()`, `plot_pid5_axis()`, `axis_breaks()`, `plot_pid5_build()`. ggplot2 stays in Suggests with a `>= 3.4.0` floor enforced at the `rlang::is_installed()` guard. Docs: roxygen page with `@examplesIf`, `_pkgdown.yml` Plots section, a Profile Plots vignette section, NEWS entry, and three stale README Phase-2 boxes ticked.

**Decisions:** D-029 (no severity bands or elevation thresholds), D-030 (structural `ggplot_build()` assertions, not `vdiffr`), D-031 (ggplot2 `>= 3.4.0` floor, enforced at the call site). AC2/AC3/AC4/AC6 amended through gates.

**Review:** Three lenses, 23 findings, independently scored. Two at or above 80, both fixed: the pinned discrete scale cancelling per-panel faceting (97) and labels hiding their points (88). Nine below-threshold findings fixed in passing; three carried to candidate rows (78/76/68). One review return. The blocker passed all 7 CI jobs and all 109 structural assertions and was visible only in the rendered figure.
