# M097: The package ships JSON exports of the three PID-5 forms as checksum-locked artifacts

- **Status:** in-progress
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** IP1, IP2, GP2
- **Resolves:** —
- **Surface tier:** user-facing — shipped download files and site cards
- **Branch/PR:** `m097-pid5-json-export`

## Goal

Ship one format-1.0 JSON file per PID-5 form (FULL, SF, BF) holding its items, response options and administration instructions as the package's tables hold them. Each file is locked to the tables by test and to the manifest by checksum, so hitop-form can render the PID-5.

## Scope

**In:** version subsetting in `data-raw/json_export.R`'s writer and three PID-5 specs. The three files under `inst/extdata/` and their staged copies under `pkgdown/assets/downloads/`. Manifest rows in `hitop_artifacts`. `test-json-export.R` extended to the five stems with a PID-5 plant test. A JSON link on each PID-5 download article. NEWS and the DESIGN.md generator sentence.

**Out:** rendering the PID-5 on hitop-form → M098. Reader fixtures and the `score_pid5()` round-trip → M099. A field telling the export from the module descriptor → stays on the online-form candidate row (Jeff, 2026-09-20 plan gate: format 1.0 unchanged, D-063). The HSUM export and the storage adapters → the online-form candidate row. The six `test-json-export.R` reach gaps M094 deferred → the verification-reach candidate row.

## Acceptance criteria

- [ ] AC1: `inst/extdata/pid5.json`, `inst/extdata/pid5sf.json` and `inst/extdata/pid5bf.json` each hold the D-063 format-1.0 fields. Their `stem` values are `pid5`, `pid5sf` and `pid5bf`. Their `maxItem` values are 220, 100 and 25. Their `instructions` come from `pid_instructions`: `start`, and its four options valued 0 to 3 as `{value, label}`. Their `items` are the rows of `pid_items` whose `FULL`, `SF` or `BF` column is not `NA`, in ascending version number. The item names are `pid5_001` to `pid5_220`, `pid5sf_001` to `pid5sf_100` and `pid5bf_01` to `pid5bf_25`. Each `text` is the row's `pid_items$Text`. The per-stem field comparison in `test-json-export.R` passes for each of the five stems.
- [ ] AC2: The plant test runs against `pid5sf` and `pid5bf` as well as `hitopbr`. On `pid5sf`, four plants are each reported by field name. The plants are an item present only on the full form inserted into the export, an SF item dropped, two SF items swapped, and `maxItem` changed. On `pid5bf`, an item renamed to three-digit padding is reported by field name. The six existing plants stay red on `hitopbr`.
- [ ] AC3: `hitop_artifacts` holds one `json` row per PID-5 stem, with `instrument` `PID-5`, `PID-5-SF` and `PID-5-BF`. `pkgdown/assets/downloads/` holds a byte-identical copy of each new file. `test-artifacts.R` passes, its md5 lock and its download-page link lock included.
- [ ] AC4: Each of the three PID-5 download articles links its JSON file as `download-hitopsr.Rmd` does. The three articles render, and each JSON link resolves to a file the manifest lists.
- [ ] AC5: NEWS names the three PID-5 JSON exports. DESIGN.md's generator sentence names the PID-5 alongside the HiTOP-SR and HiTOP-BR. `devtools::check()` reports 0 errors, 0 warnings and 0 notes.

## Coverage

- AC1 → T1, T2, T3
- AC2 → T2
- AC3 → T3
- AC4 → T4
- AC5 → T5

## Tasks

- [x] T1: Give `write_instrument_json()` version subsetting: keep the rows where the number column is not NA, ordered by that column (`data-raw/json_export.R:57-88`). Add three PID-5 specs to `json_specs` (`:41-55`) with `instrument` `PID-5`, `PID-5-SF`, `PID-5-BF` and `instructions = pid_instructions`. Run the script mode on a scratch copy first (LESSONS M094: load before reading package data).
- [x] T2: Extend the local `json_specs` in `tests/testthat/test-json-export.R:7-20` to the five stems, with the same NA subsetting as T1. `export_report()` reads the number column and `Text` unfiltered at `:47` and `:95`. Add the `pid5sf` and `pid5bf` plant tests of AC2. Show each plant red on the planted file and the loop green on the shipped files.
- [x] T3: Run `data-raw/artifacts.R` with `rebuild_stems` covering the three PID-5 stems and `rebuild_formats = "json"` (`:41`, `:52`). Commit the three files, the manifest rows and the staged copies together.
- [x] T4: Add the JSON `dl_link` to `download-pid5.Rmd`, `download-pid5sf.Rmd` and `download-pid5bf.Rmd` (the HiTOP-SR form is `download-hitopsr.Rmd:45`). Build the site clean with `pkgdown::build_site()` and read the three rendered pages.
- [x] T5: NEWS entry and the DESIGN.md generator sentence (`cairn/DESIGN.md:46`). If the `hitop_artifacts` roxygen enumerates instruments, update it. Run `devtools::document()` and `devtools::check()`.

## Work log

- 2026-09-20: created by /milestone-plan; part one of the online-form candidate row (lineage M093, M094, M095, M096).
- 2026-09-20: criteria audit ran in full mode by a fresh [O] reader; ten findings across the three milestones, all repaired before the gate: harness clauses moved from AC1 to T2, AC4 narrowed from a clean whole-site build to the three articles, AC2 gained the `maxItem` and `pid5bf` plants.
- 2026-09-20: plan gate chose three milestones over one spanning both repositories because one milestone is one pull request; falsified by M098 or M099 proving too small to review on its own.
- 2026-09-20: plan gate chose format 1.0 unchanged over a field telling the export from the module descriptor because no consumer needs it and hitop-form tells them apart by their required fields; falsified by a consumer that must read both files without knowing which it holds.

- 2026-09-20: T1 done. The writer keeps the rows whose number column is not NA, ordered by that column, and three PID-5 specs read `pid_items` with `pid_instructions`. Script mode ran on a scratch copy of the tree. The two HiTOP files rebuild byte-identical, so the subsetting step changes nothing for a one-form table. Suite green, 0 failures.
- 2026-09-20: minor plan amendment. T2, T3 and T4 share one checkpoint commit. The download-page link lock in `test-artifacts.R` requires every manifest file to be linked from a download page, so the suite is red from the moment the manifest gains the three rows until the three articles link them. No criterion, task or scope text changed.
- 2026-09-20: T2 done. The test reads five stems and subsets the expected items by the spec's number column. Before the files existed the three PID-5 stems failed on an absent path. Three checks of the comparison itself: the `pid5` file read against the SF spec reports `stem`, `maxItem` and `items.length`, the `pid5bf` file against its own spec reports nothing, and removing the subsetting from the expected side turns the SF stem red on `maxItem` and `items.length`. Five plants are each reported by field name.
- 2026-09-20: T3 done. `artifacts.R` rebuilt the three PID-5 JSON files, appended one manifest row each and staged the site copies. The other 26 artifacts kept their checksums, the two HiTOP JSON rows included.
- 2026-09-20: T4 done. Each PID-5 download page carries the JSON card. The three articles render after `devtools::install()`, because the page helper reads the installed manifest (LESSONS M094). Each rendered link points at a served file that parses as its own export.
- 2026-09-20: T5 done. NEWS names the three files, their item names and the absence of keying. The DESIGN generator sentence names the PID-5 and states how one table yields three files. The `hitop_artifacts` roxygen enumerates formats, not instruments, so it needed no edit. `document()` produced no diff. `check()` reported 0 errors, 0 warnings and 0 notes.

## Decisions

## Review
