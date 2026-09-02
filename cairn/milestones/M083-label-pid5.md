# M083: `label_pid5()`

- **Status:** review
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** GP2, GP3
- **Resolves:** —
- **Branch/PR:** `m083-label-pid5` / https://github.com/jmgirard/hitop/pull/90

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

- [x] AC1: `label_pid5(data, target = "items", version)` is exported and
      attaches each item's `pid_items$Text` as the `label` attribute of that
      item's column, for every item of the named form. Evidence: a test running
      it at the default `prefix` on `sim_pid5`, `sim_pid5sf` and `sim_pid5bf` and
      asserting, for every column of each returned frame, that
      `attr(col, "label")` is `identical()` to the `Text` of the `pid_items` row
      whose number column for that form equals the number parsed from the
      column's name.
- [x] AC2: `label_pid5(data, target = "scales", version)` attaches a display
      name to every scale column `score_pid5()` writes at its own default
      arguments. Evidence: a test scoring each of the three simulated datasets
      with `append = FALSE` at default `calc_se`, running
      `label_pid5(target = "scales")` on the result, and asserting every returned
      column carries a `label` attribute `identical()` to the `Facet`/`Domain`
      string held against that column's `camelCase` stem in
      `pid_scales[[version]]` or `pid_domains`.
- [x] AC3: `prefix = NULL` resolves to `"pid5_"`, `"pid5sf_"` and `"pid5bf_"`
      under `target = "items"` with `version = "FULL"`, `"SF"` and `"BF"`, and to
      `"pid_"` under `target = "scales"` for all three. Evidence: a test
      asserting, for all three versions and both targets, that the default call
      labels exactly the columns of a literal expected name vector written in the
      test, not one re-derived by calling the function.
- [x] AC4: Under `target = "items"`, a column named like an item of the named
      form but not zero-padded to that form's width is not labelled and is named
      in a warning of class `hitop_unpadded_items`, as `label_hitopsr()` does.
      Evidence: a test, per version, renaming columns of the simulated dataset to
      each mis-padded spelling `unpadded_item_cols()` admits — under-padded
      (`pid5_1`, and `pid5_01` for the three-digit forms) and over-padded
      (`pid5_0001`) — applied at the first and last item of the form so the
      probe varies in position as well as form, and asserting the warning's
      class, that the report names each such column, and that each carries no
      `label` attribute.
- [x] AC5: The function is documented with a runnable roxygen example, carries
      one `NEWS.md` bullet under the development-version heading stating what it
      does, has a `_pkgdown.yml` reference entry, and is named once in each of
      `vignettes/pid5_scoring.Rmd`, `pid5sf_scoring.Rmd` and
      `pid5bf_scoring.Rmd`.
- [x] AC6: The active profile's verify and review checks are clean:
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
- [x] T3: Roxygen docs and example, `document()`, the NEWS bullet, the
      `_pkgdown.yml` entry, and the three vignette mentions.
- [x] T4: Run the profile's verify and review checks; record the output.

## Work log

- 2026-09-02: created by /milestone-plan, alongside M082.
- 2026-09-02: criteria audit ran in FULL mode (user-facing tier); the findings on this half of the unsplit scope were the scale-label criterion promising a domain including `calc_se` `_se` columns its evidence never enumerated (promise narrowed, `_se` moved to `Out:`), the default-`prefix` criterion being self-comparing and so satisfiable by an implementation ignoring `prefix` (re-anchored on a literal expectation), and the unpadded criterion leaving `target` unstated while probing one spelling of the two a three-digit form admits (both fixed).
- 2026-09-02: re-audited after the gate; AC2 and AC3 returned clean, and AC4 drew one arguable finding — its promise covers any spelling not padded to the form's width while its probes were under-padded only and fixed at one column. Taken rather than posed: the over-padded spelling `pid5_0001` was added and the probes moved to the form's first and last item.
- 2026-09-02: sizing tripwire fired on the unsplit scope (9 acceptance criteria, 7 tasks); split into M082 and M083 rather than compressed, the two functions having disjoint implementations, test files and failure modes.
- 2026-09-02: plan gate chose `prefix = NULL` resolved per `version`/`target` over splitting into `label_pid5_items()`/`label_pid5_scales()` because the split breaks the `label_*(data, target, prefix)` idiom the two HiTOP helpers set; falsified by the NULL default proving hard to document or to predict.
- 2026-09-02: plan gate chose leaving `validity_pid5()`'s columns unlabelled over sourcing their display names because D-018 fixes those names to the scale-development papers and none is on the `references/` shelf (IP2); falsified by a citable source arriving with the five names.
- 2026-09-02: implementation gate chose leaving the no-columns-matched warning unclassed, matching `label_hitopsr()` and `label_hitopbr()`, over giving it a public class; falsified by a user needing to silence it without silencing the rest of the call.
- 2026-09-02: T4 done — `devtools::document()` leaves no diff, `devtools::test()` 0 failures / 17043 passes / 9 skips, `devtools::check()` 0 errors 0 warnings 0 notes (4m 14s), `pkgdown::check_pkgdown()` clean, `data-raw/check_line_endings.R` passes.
- 2026-09-02: T3 done — roxygen docs and a runnable example (output verified), the NEWS bullet, the `_pkgdown.yml` reference row (`check_pkgdown()` clean), and a mention in each of the three PID-5 scoring vignettes; `build_vignettes()` runs all three clean.
- 2026-09-02: T2 done — `R/label_pid5.R` added; the whole suite passes (0 failures, 17043 passes, 9 pre-existing skips). Check discrimination: dropping `pid_domains` from the scale lookup, hardcoding the item prefix to `pid5_`, removing the `warn_unpadded_items()` call, and reversing the item-text lookup each turn the new file red.
- 2026-09-02: step-7 approval: PR #90 approved for merge.
- 2026-09-02: T1 done — `tests/testthat/test-label_pid5.R` written red (6 failures, all `could not find function "label_pid5"`); the suite is red by design until T2, so the profile's verify slot runs at T2.

## Decisions

## Review

Verified 2026-09-02 on `m083-label-pid5` at d536290d (PR #90), against `main` at
the same merge base.

- AC1 — PASS. `testthat::test_file("tests/testthat/test-label_pid5.R")`, test
  "label_pid5(target = 'items') attaches each item's text at the default
  prefix": 348 passing expectations, 0 failures. It runs `label_pid5()` at the
  default `prefix` on `sim_pid5`, `sim_pid5sf` and `sim_pid5bf` and asserts, for
  every column of each returned frame, `identical(attr(col, "label"), ...)`
  against the `pid_items$Text` of the row whose form number column equals the
  number parsed from the column name, plus a non-empty-domain guard
  (`ncol(labeled)` equals the form's item count: 220 / 100 / 25).
- AC2 — PASS. Same run, test "label_pid5(target = 'scales') labels every column
  score_pid5() writes": 135 passing expectations, 0 failures. It scores each of
  the three simulated datasets with `append = FALSE` at default `calc_se`, runs
  `label_pid5(target = "scales")`, and asserts every returned column's `label`
  is `identical()` to the `Facet`/`Domain` string held against that column's
  `camelCase` stem in `pid_scales[[version]]` or `pid_domains`, the lookup
  written independently of the function under test.
- AC3 — PASS. Same run, test "the default prefix labels exactly the expected
  columns, per version and target": 6 passing expectations, 0 failures. For all
  three versions and both targets it asserts `identical()` between the labelled
  column names and a literal expected vector written in the test; each frame
  also carries decoys (the other two forms' stems, the pre-rename `pid_` item
  stem, and cross-form scale stems), so an implementation ignoring `prefix`
  fails.
- AC4 — PASS. Same run, test "label_pid5(target = 'items') names mis-padded item
  columns in a warning": 41 passing expectations, 0 failures. Per version it
  renames the form's first and last item to each mis-padding
  `unpadded_item_cols()` admits — under-padded (`pid5_1`; `pid5_01` for the
  three-digit forms; `pid5bf_1` for BF) and over-padded (`pid5_0001`,
  `pid5bf_001`) — and asserts the warning's class is `hitop_unpadded_items`,
  that the report names each such column, and that each carries no `label`
  attribute; a padded neighbour in the same frame is still labelled, and the
  unmodified shipped frame draws `expect_no_warning()`.
- AC5 — PASS. `man/label_pid5.Rd` documents the function; its roxygen example
  runs (`label_pid5(sim_pid5bf, target = "items", version = "BF")` then
  `attr(labeled$pid5bf_01, "label")` returns "People would describe me as
  reckless"), and `R CMD check`'s example run is OK. `NEWS.md:5` carries one
  bullet under the `# hitop (development version)` heading stating what the
  function does. `_pkgdown.yml:101` lists `label_pid5`. The function is named
  once in each of `vignettes/pid5_scoring.Rmd:47`, `pid5sf_scoring.Rmd:30` and
  `pid5bf_scoring.Rmd:30`.
- AC6 — PASS. `devtools::test()`: 0 failures, 0 warnings, 17043 passes, 9
  pre-existing skips. `devtools::check(document = TRUE)`: Status OK, 0 errors /
  0 warnings / 0 notes (4m 20s), and `git status` after it shows no diff in
  `NAMESPACE` or `man/`, so `document()` leaves none. `pkgdown::check_pkgdown()`
  reports no problems. `data-raw/check_line_endings.R` passes. Re-run after the
  F6/F7 documentation fix below: `check()` Status OK, 0/0/0 (3m 59s), with
  `document()` again leaving no unintended diff.

### Consistency gate

- `cairn_validate.py` exits 0, all checks PASS; 24 advisories, all pre-existing
  (23 dangling `D-001`..`D-012` tokens pointing into `cairn/legacy/`, and one
  `references/` page with no extraction status). `release window` OK.
- No `DESIGN.md` principle changed, so `cairn_impact.py` is not run.
- Toolchain checks (`r-package` profile): `document()` no diff; `NAMESPACE`,
  `man/` and `data/*.rda` regenerate clean; `README.Rmd` untouched, so no
  re-knit; `pkgdown::check_pkgdown()` clean; `NEWS.md` carries the user-visible
  entry; no new top-level files, so no `.Rbuildignore` entry is owed;
  `devtools::check()` 0/0/0.

### Independent review

Three fresh-context lenses, none having seen the implementation. The
blame-history lens reports no conflict with any prior milestone's intent or
recorded decision (its one note, a stale `allow_null` comment at `R/util.R:184`,
was already false before this branch). The prior-review lens reports the M077,
M079 and M073 findings on the `label_*()` family are all honoured here and finds
no regression; its GitHub inline-comment probe returned empty, so that surface
was skipped. The diff-bug lens cross-validated the labels independently of the
tests (every item label `identical()` to the text `score_pid5()` carries for
that item number across all three forms; every scale column `score_pid5()`
writes labelled, 30/30 FULL, 30/30 SF, 6/6 BF) and raised ten findings, each
reproduced against the implementation before triage.

- F1 (rank 1) — The unpadded-items report cannot fire on a frame whose item
  columns are *all* mis-padded: the "no columns matched" early return at
  `R/label_pid5.R:83-88` precedes `warn_unpadded_items()`. Reproduced:
  `label_pid5(data.frame(pid5bf_1 = 1, pid5bf_2 = 2), target = "items", version
  = "BF")` raises only an unclassed `rlang_warning`. **Follow-up.** The gap is
  inherited verbatim from `label_hitopsr()` (reproduced there too) and
  `label_hitopbr()`, which M077 shipped; AC4 benchmarks this behavior as
  "`as label_hitopsr()` does", and parity holds. The failing input also lies
  outside AC4's named procedure, which renames columns of the shipped dataset
  and so always leaves padded neighbours — no defect return under the floor.
  Fixing one of three helpers would break their symmetry, so it belongs to a
  family-wide follow-up.
- F2 (rank 2) — A correctly padded but out-of-range item column is reported as
  unpadded, and the hint offered is the offending name itself. Reproduced:
  `label_pid5(data.frame(pid5bf_01 = 1, pid5bf_99 = 2), ...)` warns `1 column is
  named like PID-5-BF items but not zero-padded to 2 digits ... "pid5bf_99"` and
  hints `Item numbers are expected as \`pid5bf_99\``. **Follow-up.** The cause is
  `unpadded_item_cols()` (`R/util.R:590`), which this diff does not modify;
  pre-existing across all three helpers since M077/M079. M083 raises its
  exposure, because the PID-5's three forms number items disjointly.
- F3 (rank 3) — Pluralization in the same warning: `R/util.R:606`'s third cli
  marker does not re-key on the quantity, so one column reads "1 column is named
  ... so they were not labelled". **Follow-up**, with F2 — same unmodified
  helper, cosmetic.
- F4 (rank 4) — `label_pid5()`'s "no columns matched" report is unclassed, where
  `rename_pid5_items()`'s analogous report carries the public class D-057
  names. **Rejected**: a deliberate implementation-gate choice, logged above
  (match the two HiTOP helpers), and the family's unevenness is already on the
  PID-5-helpers candidate row.
- F5 (rank 5) — `prefix = ""` is accepted and labels bare-numeric column names.
  **Rejected**: shared with both siblings, does not error, and no documented
  input reaches it.
- F6 (rank 6) — `@return` did not say that a frame matching nothing comes back
  untouched with a warning. **Fixed now** in `R/label_pid5.R`.
- F7 (rank 7) — `@param prefix` stated the unpadded rule more widely than the
  code implements (F2's actual rule is "not one of the expected names", which
  also catches correctly padded out-of-range numbers). **Fixed now** in
  `R/label_pid5.R`, together with F6; `document()` re-run.
- F8 (rank 8) — `match.arg()` partial-matches `version`, so `"F"` resolves to
  `"FULL"` unannounced. **Rejected**: identical to `score_pid5()` and
  `rename_pid5_items()`; changing it here alone would split the family.
- F9 (rank 9) — The AC4 test always leaves padded neighbours in the frame, so it
  cannot catch F1. **Follow-up**, with F1.
- F10 (rank 10) — Tracking nit: the milestone file was uncommitted and its
  criterion boxes unticked when the lens read it. **No change needed** — that is
  this review writing them; they land in the checkpoint commit.

Actioned list: F1, F2, F3, F6, F7, F9. F6 and F7 fixed on the branch; F1, F2, F3
and F9 route to the PID-5-helpers candidate row at the post-merge pass. No
finding demonstrates an acceptance criterion failing inside its named
procedure's domain, so the return floor is not met.
