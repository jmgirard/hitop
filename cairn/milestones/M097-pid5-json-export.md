# M097: The package ships JSON exports of the three PID-5 forms as checksum-locked artifacts

- **Status:** review
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

- [x] AC1: `inst/extdata/pid5.json`, `inst/extdata/pid5sf.json` and `inst/extdata/pid5bf.json` each hold the D-063 format-1.0 fields. Their `stem` values are `pid5`, `pid5sf` and `pid5bf`. Their `maxItem` values are 220, 100 and 25. Their `instructions` come from `pid_instructions`: `start`, and its four options valued 0 to 3 as `{value, label}`. Their `items` are the rows of `pid_items` whose `FULL`, `SF` or `BF` column is not `NA`, in ascending version number. The item names are `pid5_001` to `pid5_220`, `pid5sf_001` to `pid5sf_100` and `pid5bf_01` to `pid5bf_25`. Each `text` is the row's `pid_items$Text`. The per-stem field comparison in `test-json-export.R` passes for each of the five stems.
- [x] AC2: The plant test runs against `pid5sf` and `pid5bf` as well as `hitopbr`. On `pid5sf`, four plants are each reported by field name. The plants are an item present only on the full form inserted into the export, an SF item dropped, two SF items swapped, and `maxItem` changed. On `pid5bf`, an item renamed to three-digit padding is reported by field name. The six existing plants stay red on `hitopbr`.
- [x] AC3: `hitop_artifacts` holds one `json` row per PID-5 stem, with `instrument` `PID-5`, `PID-5-SF` and `PID-5-BF`. `pkgdown/assets/downloads/` holds a byte-identical copy of each new file. `test-artifacts.R` passes, its md5 lock and its download-page link lock included.
- [x] AC4: Each of the three PID-5 download articles links its JSON file as `download-hitopsr.Rmd` does. The three articles render, and each JSON link resolves to a file the manifest lists.
- [x] AC5: NEWS names the three PID-5 JSON exports. DESIGN.md's generator sentence names the PID-5 alongside the HiTOP-SR and HiTOP-BR. `devtools::check()` reports 0 errors, 0 warnings and 0 notes.

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
- 2026-09-20: claim audit: 25 claims read, 3 corrected — tests/testthat/test-json-export.R, data-raw/json_export.R.
- 2026-09-20: status set to review. Suite 0 failures, 17517 passing. `check()` clean, 0 errors, 0 warnings, 0 notes.
- 2026-09-20: review checkpoint, work not finished. AC1 to AC4 verified against fresh evidence and ticked. AC5 waits on a `devtools::check()` run that is still going. Two of the three fresh-context reviewers reported no findings, and the third is still reading.
- 2026-09-20: AC5 met and ticked. `check()` clean, 0 errors, 0 warnings, 0 notes. Consistency gate passed. The third reviewer found no correctness bug and six minor findings.
- 2026-09-20: step-7 approval: m097-pid5-json-export approved for merge. Jeff chose to fix two stale comments first and to send the other four review findings to the backlog.

## Decisions

## Review

Verified 2026-09-20 on `m097-pid5-json-export` at 25f994ed. `origin/main` stood at af728388 and had not moved since the branch was cut, so no merge forward was needed. The tree was clean.

- AC1 met. An independent script read each of the three shipped files with `jsonlite::fromJSON()`. It compared each file to `pid_items` and `pid_instructions` directly, not through the suite's own spec. It ran 19 checks per stem and all passed. The top-level field set is exactly the eight format-1.0 fields, with `format` `"1.0"`, `package` `"hitop"` and `buildDate` in `YYYY-MM-DD` form. The stems are `pid5`, `pid5sf` and `pid5bf`, and `maxItem` is 220, 100 and 25. `instructions$start` is identical to `pid_instructions$start`. The four options carry values `0:3`, labels identical to the table, and the fields `{value, label}`. The items equal the non-`NA` rows of the `FULL`, `SF` and `BF` column, ascending and contiguous from 1 to the maximum. The names run `pid5_001` to `pid5_220`, `pid5sf_001` to `pid5sf_100` and `pid5bf_01` to `pid5bf_25`. Every `text` is identical to its row's `pid_items$Text`, and every item carries the fields `{number, name, text}`. The suite's own per-stem comparison then returned an empty report for all five stems under `testthat::test_local(filter = "json-export")`, 27 passing and 0 failures.
- AC2 met. Each plant ran with its report printed rather than only asserted. The six `hitopbr` plants report `text`, `items.length`, `{number, name, text}`, `options.label`, `start` and `stem`. The four `pid5sf` plants report `items.length` for a full-form item leaked in and `items.length` again for a dropped SF item. They report `{number, name, text}` for two swapped SF items and `maxItem` for a changed maximum. The one `pid5bf` plant repads an item name to three digits and reports `name`. Every report matched the field name the test names. The five shipped files reported nothing.
- AC3 met. `hitop_artifacts` carries five `json` rows. The three new ones are `PID-5` with `pid5.json`, `PID-5-SF` with `pid5sf.json` and `PID-5-BF` with `pid5bf.json`, each with `build_date` 2026-09-20. A raw-byte comparison shows each `inst/extdata/` file identical to its `pkgdown/assets/downloads/` copy, and each md5 matches its manifest row. `testthat::test_local(filter = "artifacts")` passed with 128 checks, the md5 lock and the download-page link lock included.
- AC4 met. The three articles carry the same `dl_card` and `dl_link` shape as `download-hitopsr.Rmd:42-46` and differ only in the file named. `pkgdown::build_article()` rendered all three clean. Each rendered page carries one `href="../downloads/<stem>.json"` with a `download` attribute. That href resolves to `docs/downloads/<stem>.json`, which exists, parses as its own export, appears in `hitop_artifacts`, and matches the manifest md5.
- AC5 met. The NEWS entry names `pid5.json`, `pid5sf.json` and `pid5bf.json`. It also names the three item-name ranges, the absence of keying, and the `hitop_artifacts` rows. Every claim in it was checked against the shipped files under AC1 and AC3. The `cairn/DESIGN.md` generator sentence now names the three PID-5 forms beside the HiTOP-SR and the HiTOP-BR. It also states that a spec's `number_col` turns one table into three files. `devtools::check()` ran 4m 21.5s and reported `Status: OK`, with 0 errors, 0 warnings and 0 notes. Its test stage ran the whole suite in 189s and passed.

Consistency gate. `cairn_validate.py` exited 0 with every check passing. It raised 24 advisories, all of them pre-existing: 23 dangling identifier tokens from the legacy decision numbering, and one references-staleness note on `schmukle2026.md`. The `release window` advisory did not fire. No `DESIGN.md` principle changed in this diff, so `cairn_impact.py` was not owed. The `r-package` profile's toolchain checks all passed. `devtools::document()` produced no diff. No generated file was hand-edited. `README.Rmd` is untouched by this branch and in sync with `README.md`. `pkgdown::check_pkgdown()` reported no problems. NEWS carries the user-visible entry and names no milestone. The branch adds no top-level file, so no `.Rbuildignore` entry is owed. `devtools::check()` is clean.

Independent review. Three fresh-context reviewers read the branch, none having seen the implementation. The blame-history reviewer reported no findings, having read the history behind every touched line against D-010, D-016, D-054, D-063 and the M094 commit. The prior-review reviewer reported no regression against M094's archived Review section. Its one probe found no real inline review comments on the repository, so no per-PR walk was owed. The diff-bug reviewer re-derived all three files from `pid_items` independently, found no correctness bug, and reported six minor findings, ranked here as it ranked them.

- F1. `data-raw/json_export.R:119-123` runs its script mode over all five specs with no stem filter, and the file header at `:13` still advertises that mode. A maintainer who corrects one PID-5 item and runs the script as documented also re-stamps `buildDate` on the two HiTOP files. That reds the md5 lock for instruments nothing changed in. The shape predates M097, which widened it from two files to five. `data-raw/artifacts.R` is the only filtered path.
- F2. D-063's decision text specifies `items` "in table order" where the writer now orders by the spec's number column, which `cairn/DESIGN.md:46` states. This is a no-op today. The `FULL`, `SF`, `BF` and `HSR` columns are each already ascending in row order, so no shipped byte disagrees with the decision text.
- F3. `data-raw/json_export.R:32` still says `items` are "in table order", eight lines below the new paragraph that says the writer orders by the number column. The stale account sits in the format-1.0 field block a reader consults first.
- F4. `tests/testthat/test-artifacts.R:203` says "the 26 staged files are tracked". The directory now holds 29. The assertions read the directory, so nothing fails.
- F5. `spec_items()` at `tests/testthat/test-json-export.R:46-54` reproduces the writer's filter-and-sort expression. Every number column is already ascending, so no fixture or plant tells a sorted export from an unsorted one. The ordering half of the new logic is therefore locked only against itself. The subsetting half is exercised.
- F6. The leaked-item and dropped-item plants both resolve to `items.length`, so the wrong-form plant shows count detection rather than membership detection. A same-count substitution is not planted. AC2 is met as written, because it asks for four plants each reported by field name.

Return floor. No finding demonstrates an acceptance criterion failing, and none is a defect that changes what the package does for its users. F1 is a maintainer footgun in a `data-raw/` script that no user path reaches. F2 is a record that agrees with every shipped byte. F3 and F4 are stale comments. F5 and F6 are verification reach. None returns the milestone.

Triage at the gate, 2026-09-20. Jeff chose to fix the two stale comments before the merge and to send the rest to the backlog.

- F3 fixed now. `data-raw/json_export.R:32` now reads "in ascending number order".
- F4 fixed now. `tests/testthat/test-artifacts.R:203` now reads 29.
- F1 follow-up. A candidate row records the unfiltered script mode.
- F2 follow-up. The same row records that D-063's "in table order" wording needs a superseding entry before any keying table arrives whose number column is not ascending.
- F5 and F6 follow-up. Both extend the verification-reach candidate row.

Both fixes touch comments alone and change no runtime behavior. `test-artifacts.R` and `test-json-export.R` were re-run after them and passed, 155 checks and 0 failures.
