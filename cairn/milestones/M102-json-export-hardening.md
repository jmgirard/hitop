# M102: The JSON export's writer and lock catch the ten gaps M094 and M097 left open

- **Status:** in-progress
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** IP2
- **Resolves:** —
- **Surface tier:** internal — tests, a data-raw script and one unexported writer, with no export, dataset or artifact change
- **Branch/PR:** m102-json-export-hardening

## Goal

The JSON writer and `tests/testthat/test-json-export.R` catch the ten gaps that the M094 and M097 reviews left open. No shipped JSON byte moves.

## Scope

**In:** the ten gaps in the retired candidate row. From M094's review: F7 (the `number.type` note is unreachable), F8 (a shared tempfile), F9 (`auto_unbox` plants), F10 (a duplicated manifest helper), F16 (no direct writer test) and G6 (extra keys ship green). From M097's review: F1 (script mode rewrites all five files), F2 (D-063's "in table order" wording), F5 (the ordering lock copies the writer) and F6 (the wrong-form plant tests the count only). `write_instrument_json()` moves from `data-raw/json_export.R` to an unexported function under `R/` with a `build_date` argument. `json_specs` stays in `data-raw/`, and its script mode goes. D-065 in the plan commit settles F2.

**Out:** the two cross-artifact sweeps that filter to qualtrics and redcap (`test-export-padding-width.R:194`, `test-response-value-no-move.R:44`). AC4's byte lock covers the JSON files they skip, so no edit is planned. Two release-time effects stay in the release candidate row: a version bump reds the suite until `artifacts.R` reruns, and `Sys.Date()` churns the build date. The field that tells the export from the module descriptor stays with D-063 and the online-form candidate row.

## Acceptance criteria

- [ ] AC1: `export_report()` in `tests/testthat/test-json-export.R` compares the key set of each object level that format 1.0 defines against D-063's field list. The levels are the top level, `instructions`, each `options` entry and each `items` entry. It reports an extra key under a name for that level. One plant per level adds an unlisted key, and each plant is reported under that level's name and no other.
- [ ] AC2: `export_report()` reads the file's JSON types without simplification. It reports any of D-063's scalar fields written as an array. The scalar fields are `format`, `package`, `packageVersion`, `buildDate`, `stem`, `maxItem`, `start`, `value` and `label` in each `options` entry, and `number`, `name` and `text` in each `items` entry. It also reports an item `number` written as a non-integer JSON number. A plant that boxes `format` as `["1.0"]` is reported as `format` alone. A plant that writes one item's `number` as `7.0` is reported as `number.type` alone.
- [ ] AC3: `export_report()` checks each export's item count against a count stated in the test (220, 100, 25, 405, 45). It checks each item's text against the table row with that item's number. A plant on the PID-5-SF export replaces one item's text with the text of a FULL-only item and keeps the count. That plant is reported as `text` alone.
- [ ] AC4: `write_instrument_json()` is an unexported function under `R/` that takes a `build_date` argument. `test-json-export.R` tests it directly with no `skip_*` call, so AC6's test and check runs execute it. On a synthetic table whose number column is out of order and holds `NA`, the written file lists the non-`NA` rows in ascending number order. A fresh write of each of the five specs, with `build_date` set to its latest manifest row's date, is byte-identical to the committed `inst/extdata/` file. The bytes include `packageVersion`, so a version bump without a rebuild reds this lock, as `export_report()` already does.
- [ ] AC5: `data-raw/json_export.R` holds no script-mode block (`grep -c 'sys.nframe' data-raw/json_export.R` prints 0). Its header names `data-raw/artifacts.R` with `rebuild_stems` as the way to rebuild one form's file.
- [ ] AC6: No shipped data or artifact byte changes. At review, `git diff --stat main -- inst/extdata pkgdown/assets/downloads data R/sysdata.rda` prints nothing. `devtools::test()` passes, and `devtools::check()` reports 0 errors, 0 warnings and 0 notes.

## Coverage

- AC1 → T3
- AC2 → T3
- AC3 → T4
- AC4 → T1, T5
- AC5 → T2
- AC6 → T1, T2, T6

## Tasks

- [x] T1: Move `write_instrument_json()` from `data-raw/json_export.R:84-121` into a new `R/json_export.R`. Make it unexported, with `build_date = Sys.Date()`, and keep the binary-connection write (LESSONS 2026-07-16). `data-raw/artifacts.R:196-198` then calls it through the loaded package. It passes the date that the manifest row records (`today`, `:223`), so the file and its row cannot disagree. Rerun nothing, because T5's byte lock proves that the move changed no byte.
- [x] T2: Remove the script-mode blocks (`data-raw/json_export.R:40-44` and `:123-127`). Rewrite the header (`:10-15`) to name `data-raw/artifacts.R` with `rebuild_formats = "json"` and `rebuild_stems` as the rebuild path.
- [x] T3: Do the test tidy-ups first, then the type and key checks. Move `latest_manifest()` from `test-artifacts.R:7` to a helper file, and use it in place of `manifest_build_date()` (F10). Give each plant its own `withr::local_tempfile()` (F8). Let a plant serialize with `auto_unbox = FALSE`, or edit raw text where the defect needs it (F9). The `7.0` plant needs a text edit. Parse with `simplifyVector = FALSE`, and check scalars and arrays by JSON type (F7, F9). Then compare key sets per level (G6). Add the AC1 and AC2 plants.
- [x] T4: State the per-form counts in the test. Look up each item's text by its number in place of the sort in `spec_items()` (F5). Add the same-count substitution plant on the PID-5-SF (F6).
- [x] T5: Add the direct writer tests, both through `withr::local_tempfile()`. One uses a synthetic out-of-order table with an `NA` row. The other rebuilds the five specs at each file's manifest date and compares bytes.
- [ ] T6: Run `devtools::document()`, `devtools::test()` and `devtools::check()`. Make sure that the AC6 diff is empty. Add no NEWS entry, because nothing user-visible changes.

## Work log

- 2026-09-21: created by /milestone-plan from the JSON-export candidate row (M094 F7 to F10, F16 and G6, with M097 F1, F2, F5 and F6). The reduced criteria audit ([O] fresh reader) returned four findings, all fixed before the gate. AC2 now names the scalar fields. AC3 now promises counts, a lookup and a substitution plant, not a construction claim. AC6 now says no data or artifact byte, since the writer adds `R/` code. AC4 names its runs.
- 2026-09-21: plan gate chose moving the writer into `R/` over keeping it in `data-raw/` with a skipping test, because `data-raw/` is build-ignored and a direct test there never runs under check (LESSONS 2026-09-02). Falsified by the unexported writer adding check or namespace cost while nothing but `artifacts.R` and the test uses it.
- 2026-09-21: plan gate chose removing script mode over adding a form filter, because `artifacts.R` already filters by stem and one rebuild path needs less upkeep than two. Falsified by a maintainer needing to write a JSON file without touching the manifest.
- 2026-09-21: plan gate chose a byte-for-byte rebuild lock over a synthetic-table test alone, because only the byte lock catches a writer edit never rerun into the committed files. Falsified by the lock going red on changes nobody made to the writer or the tables.
- 2026-09-21: implement started on branch m102-json-export-hardening. No question gate, because the plan left nothing open (jsonlite is already an Import).
- 2026-09-21: T1 done. `write_instrument_json(spec, path, build_date = Sys.Date())` is now in `R/json_export.R` with the format notes. `artifacts.R` sets `today` before the write loops and passes it. A rebuild of the five specs at their manifest dates matched all five committed md5s. Suite 17551 pass, 0 fail.
- 2026-09-21: T2 done. Both `sys.nframe()` blocks are gone (`grep -c` prints 0), and the header names `artifacts.R` with `rebuild_formats` and `rebuild_stems` as the rebuild path. DESIGN's generator line now names `R/json_export.R` as the writer's home.
- 2026-09-21: T3 done. `latest_manifest()` moved to `helper-manifest.R`. `export_report()` parses with `simplifyVector = FALSE`, checks each scalar by JSON type and compares key sets at four levels (`keys.top`, `keys.instructions`, `keys.options`, `keys.items`). Plants unbox scalars and write with `auto_unbox = FALSE` to their own `withr::local_tempfile()`. An unaltered copy of each of the five files reports nothing. The four key plants, the boxed `format` plant and the `7.0` text-edit plant each report their one name. The build date is now looked up by the spec's stem, because tempfile names are random. Suite 17562 pass, 0 fail.
- 2026-09-21: T4 done. Each test spec states its count (220, 100, 25, 405, 45), and the expected numbers are `seq_len(count)`. `table_text()` looks up each item's text by that item's own number, replacing `spec_items()`. The two swapped-pair plants now report `number` and `name` only, because a whole swapped item keeps its text with its number. The PID-5-SF substitution plant reports `text` alone. Suite 17564 pass, 0 fail.
- 2026-09-21: T5 done. Two direct writer tests with no `skip_*` call: a synthetic table (numbers 3, NA, 1, 2) comes out as items 1 to 3, and a write of each of the five specs at its manifest date is byte-identical to the committed file. A control a day later is not identical. Plants in memory: `pretty = FALSE` reds only the byte lock, and dropping the sort reds only the order test. Suite 17575 pass, 0 fail.

## Decisions

## Review
