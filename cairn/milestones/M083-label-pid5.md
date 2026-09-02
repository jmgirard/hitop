# M083: `label_pid5()`

- **Status:** in-progress
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** GP2, GP3
- **Resolves:** —
- **Branch/PR:** `m083-label-pid5`

## Goal

Ship `label_pid5()`, so PID-5 item and scale columns carry their questionnaire
text and display names as `label` attributes, as the two HiTOP instruments' data
already does.

## Scope

Surface tier: **user-facing** — a new exported function with documented
arguments, defaults and reports.

**In:**

- `label_pid5(data, target, version, prefix)`, attaching `pid_items$Text` as the
  `label` attribute under `target = "items"` and the scale's display name under
  `target = "scales"`, the latter read from `pid_scales[[version]]` and, for the
  FULL and SF forms, from `pid_domains` as well — two tables where each HiTOP
  instrument needs one. `prefix = NULL` resolves to the stem for the given
  `version` and `target`.
- Reuse of `unpadded_item_cols()` and `warn_unpadded_items()` (`R/util.R:590`)
  so an unpadded item column is reported, not silently skipped.
- Roxygen docs with a runnable example, tests, a NEWS bullet, a `_pkgdown.yml`
  reference entry, and a one-line mention in each PID-5 scoring vignette.

**Out:**

- `rename_pid5_items()` → M082, planned in the same run.
- Labelling the columns `validity_pid5()` writes (`pid_PNA`, `pid_INC`,
  `pid_ORS`, `pid_PRD`, `pid_SDTD`) → candidate row. No shipped table holds
  their display names and D-018 fixes those names to the scale-development
  papers, so sourcing them is IP2 work with its own sign-off.
- Labelling the `_se` columns `score_pid5(calc_se = TRUE)` writes → candidate
  row. `calc_se` is deprecated under D-049 and bound for removal.

## Acceptance criteria

- [ ] AC1: `label_pid5(data, target = "items", version)` is exported and
      attaches each item's `pid_items$Text` as the `label` attribute of that
      item's column, for every item of the named form. Evidence: a test running
      it at the default `prefix` on `sim_pid5`, `sim_pid5sf` and `sim_pid5bf` and
      asserting, for every column of each returned frame, that
      `attr(col, "label")` is `identical()` to the `Text` of the `pid_items` row
      whose number column for that form equals the number parsed from the
      column's name.
- [ ] AC2: `label_pid5(data, target = "scales", version)` attaches a display
      name to every scale column `score_pid5()` writes at its own default
      arguments. Evidence: a test scoring each of the three simulated datasets
      with `append = FALSE` at default `calc_se`, running
      `label_pid5(target = "scales")` on the result, and asserting every returned
      column carries a `label` attribute `identical()` to the `Facet`/`Domain`
      string held against that column's `camelCase` stem in
      `pid_scales[[version]]` or `pid_domains`.
- [ ] AC3: `prefix = NULL` resolves to `"pid5_"`, `"pid5sf_"` and `"pid5bf_"`
      under `target = "items"` with `version = "FULL"`, `"SF"` and `"BF"`, and to
      `"pid_"` under `target = "scales"` for all three. Evidence: a test
      asserting, for all three versions and both targets, that the default call
      labels exactly the columns of a literal expected name vector written in the
      test, not one re-derived by calling the function.
- [ ] AC4: Under `target = "items"`, a column named like an item of the named
      form but not zero-padded to that form's width is not labelled and is named
      in a warning of class `hitop_unpadded_items`, as `label_hitopsr()` does.
      Evidence: a test, per version, renaming columns of the simulated dataset to
      each mis-padded spelling `unpadded_item_cols()` admits — under-padded
      (`pid5_1`, and `pid5_01` for the three-digit forms) and over-padded
      (`pid5_0001`) — applied at the first and last item of the form so the
      probe varies in position as well as form, and asserting the warning's
      class, that the report names each such column, and that each carries no
      `label` attribute.
- [ ] AC5: The function is documented with a runnable roxygen example, carries
      one `NEWS.md` bullet under the development-version heading stating what it
      does, has a `_pkgdown.yml` reference entry, and is named once in each of
      `vignettes/pid5_scoring.Rmd`, `pid5sf_scoring.Rmd` and
      `pid5bf_scoring.Rmd`.
- [ ] AC6: The active profile's verify and review checks are clean:
      `devtools::test()` passes, `devtools::document()` leaves no diff,
      `devtools::check()` reports 0 errors and 0 warnings with any NOTE
      justified, and `pkgdown::check_pkgdown()` passes.

## Coverage

- AC1 → T1, T2
- AC2 → T1, T2
- AC3 → T1, T2
- AC4 → T1, T2
- AC5 → T3
- AC6 → T3, T4

## Tasks

- [x] T1: Write `tests/testthat/test-label_pid5.R` red — AC1's per-item label
      sweep across the three simulated datasets, AC2's scale sweep across
      `pid_scales` and `pid_domains`, AC3's literal-expectation defaults, AC4's
      unpadded spellings.
- [x] T2: Implement `R/label_pid5.R`, modelled on `R/label_hitopsr.R`, resolving
      `prefix = NULL` per `version`/`target`, reading scale display names from
      `pid_scales[[version]]` plus `pid_domains`, and reusing
      `warn_unpadded_items()`; run T1 green.
- [ ] T3: Roxygen docs and example, `document()`, the NEWS bullet, the
      `_pkgdown.yml` entry, and the three vignette mentions.
- [ ] T4: Run the profile's verify and review checks; record the output.

## Work log

- 2026-09-02: created by /milestone-plan, alongside M082.
- 2026-09-02: criteria audit ran in FULL mode (user-facing tier); the findings on this half of the unsplit scope were the scale-label criterion promising a domain including `calc_se` `_se` columns its evidence never enumerated (promise narrowed, `_se` moved to `Out:`), the default-`prefix` criterion being self-comparing and so satisfiable by an implementation ignoring `prefix` (re-anchored on a literal expectation), and the unpadded criterion leaving `target` unstated while probing one spelling of the two a three-digit form admits (both fixed).
- 2026-09-02: re-audited after the gate; AC2 and AC3 returned clean, and AC4 drew one arguable finding — its promise covers any spelling not padded to the form's width while its probes were under-padded only and fixed at one column. Taken rather than posed: the over-padded spelling `pid5_0001` was added and the probes moved to the form's first and last item.
- 2026-09-02: sizing tripwire fired on the unsplit scope (9 acceptance criteria, 7 tasks); split into M082 and M083 rather than compressed, the two functions having disjoint implementations, test files and failure modes.
- 2026-09-02: plan gate chose `prefix = NULL` resolved per `version`/`target` over splitting into `label_pid5_items()`/`label_pid5_scales()` because the split breaks the `label_*(data, target, prefix)` idiom the two HiTOP helpers set; falsified by the NULL default proving hard to document or to predict.
- 2026-09-02: plan gate chose leaving `validity_pid5()`'s columns unlabelled over sourcing their display names because D-018 fixes those names to the scale-development papers and none is on the `references/` shelf (IP2); falsified by a citable source arriving with the five names.
- 2026-09-02: implementation gate chose leaving the no-columns-matched warning unclassed, matching `label_hitopsr()` and `label_hitopbr()`, over giving it a public class; falsified by a user needing to silence it without silencing the rest of the call.
- 2026-09-02: T2 done — `R/label_pid5.R` added; the whole suite passes (0 failures, 17043 passes, 9 pre-existing skips). Check discrimination: dropping `pid_domains` from the scale lookup, hardcoding the item prefix to `pid5_`, removing the `warn_unpadded_items()` call, and reversing the item-text lookup each turn the new file red.
- 2026-09-02: T1 done — `tests/testthat/test-label_pid5.R` written red (6 failures, all `could not find function "label_pid5"`); the suite is red by design until T2, so the profile's verify slot runs at T2.

## Decisions

## Review
