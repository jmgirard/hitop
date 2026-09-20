# M094: The package ships a JSON export of the HiTOP-SR and HiTOP-BR items, response options and instructions as a checksum-locked artifact

- **Status:** in-progress
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** IP1, IP2, GP4
- **Resolves:** —
- **Surface tier:** user-facing — a distributed artifact under D-016 that a page outside the package reads
- **Branch/PR:** m094-json-export

## Goal

Ship one JSON file per HiTOP form holding its items, response options and administration instructions as the package's tables hold them, locked to the tables by test and to the manifest by checksum, so a page outside the package can render the instrument.

## Scope

**In:** `data-raw/json_export.R` writing `inst/extdata/hitopsr.json` and `inst/extdata/hitopbr.json`. Each file's top level: `format` (`"1.0"`), `package`, `packageVersion`, `buildDate`, `stem` (the file stem, also the item-column prefix before the underscore), `maxItem`, `instructions` (`start`, and `options` as an array of `{value, label}`), and `items` as an array of `{number, name, text}` in table order. Manifest rows and staged copies through `data-raw/artifacts.R`, a parse-and-compare test, download-page links, NEWS.

**Out:** the PID-5 forms and the HSUM → the online-form candidate row (Jeff at the 2026-09-20 plan gate: HiTOP-SR and HiTOP-BR first). Keying (reverse flags, scale membership) is not exported: the page never scores and the package rebuilds keying from its own tables (D-039). The page → M095. The package-side reader → M096.

## Acceptance criteria

- [ ] AC1: For each of `hitopsr` and `hitopbr`, `inst/extdata/<stem>.json` parses with `jsonlite::fromJSON(simplifyVector = TRUE, simplifyDataFrame = FALSE, simplifyMatrix = FALSE)`, and its `items` array equals, element by element and in table order, the rows of `hitopsr_items` (numbered by `HSR`) or `hitopbr_items` (numbered by `HBR`) on `number` as an integer, on `name` as `item_names(paste0(stem, "_"), number, maxItem)` builds it, and on `text`; its `instructions.start` equals the matching `*_instructions$start`, and its `instructions.options`, taken as integer `value` and character `label` in array order, equals that object's `options` data frame row for row. A test in `tests/testthat/` asserts this with the expected side read from the tables and `R/sysdata.rda`, never from the file.
- [ ] AC2: Each of the two files has a `hitop_artifacts` row with `format` `"json"` whose `md5` equals the committed file's, and a byte-identical copy under `pkgdown/assets/downloads/`, which a local `pkgdown::build_site()` places at `docs/downloads/<stem>.json`, the path the deployed site serves at `https://jmgirard.github.io/hitop/downloads/<stem>.json` (D-033).
- [ ] AC3: Each file carries top-level `format` `"1.0"`, `package` `"hitop"`, `packageVersion` equal to the current `DESCRIPTION` Version (a mismatch is a rebuild trigger), `buildDate` equal to its current manifest row's `build_date`, `stem` equal to the file stem, and `maxItem` equal to the largest item number in its table; the AC1 test asserts these fields.
- [ ] AC4: Neither committed file holds a carriage-return byte, and `Rscript data-raw/check_line_endings.R` passes.
- [ ] AC5: `vignettes/articles/download-hitopsr.Rmd` and `download-hitopbr.Rmd` each link the form's JSON file with one sentence saying what it holds and that it is what the hitop-form page reads; `NEWS.md` names the two artifacts; `devtools::test()` and `devtools::check()` are clean.

## Coverage

- AC1 → T1, T3
- AC2 → T2, T4
- AC3 → T1, T3
- AC4 → T1, T4
- AC5 → T4

## Tasks

- [x] T1: Write `data-raw/json_export.R`: build each payload from `hitopsr_items`/`hitopbr_items`, the matching `*_instructions` object (`data-raw/sysdata.R:24-41`) and `item_names()` (`R/util.R:697`); serialize with `jsonlite::toJSON(pretty = TRUE)` with scalars `unbox`ed as `R/module_file.R:130-140` does; write through `file(path, open = "wb")` with `useBytes = TRUE` (LESSONS 2026-07-16).
- [x] T2: Register both files in `data-raw/artifacts.R` (`add_row()` at :201, staging at :280) with `format = "json"`; run it; regenerate `hitop_artifacts`.
- [x] T3: `tests/testthat/test-json-export.R`: the AC1 and AC3 assertions with the expected side from the tables; before trusting green, plant in a temporary copy, one at a time, a changed item text, a dropped item, two swapped items, a changed option label, a changed `instructions.start` and a changed `stem`, and see the test red on each.
- [ ] T4: Download articles and NEWS; run `data-raw/check_line_endings.R`; `devtools::document()`; `devtools::test()`; `devtools::check()`; `pkgdown::build_site()` and confirm `docs/downloads/<stem>.json` for both stems.

## Work log

- 2026-09-20: created by /milestone-plan.
- 2026-09-20: criteria audit ran in full mode on a fresh [O] reader over the six-form draft: 33 findings across the four drafted milestones, 30 fixed at the gate (named instruction and items tables, `stem` versus prefix, `packageVersion` against the current DESCRIPTION, instrument clauses removed, plants per field family), three posed as gate questions.
- 2026-09-20: plan gate chose HiTOP-SR and HiTOP-BR only over all six forms (Jeff's call); the PID-5 and HSUM exports live in the candidate row; falsified by nothing, a scope choice.
- 2026-09-20: the audit's second pass over the final wording returned six findings across the three files, all fixed before implementation: here AC2 names the served path and T3 plants `instructions.start`.
- 2026-09-20: plan chose exporting the tables without keying over a self-contained descriptor with reverse flags and scale membership because the page never scores and D-039 rebuilds keying from the package; falsified by a page needing to score in the browser.
- 2026-09-20: implement started on `m094-json-export`; the pre-implementation gate was skipped because the plan left no API, naming or dependency choice open (jsonlite is already an Import).
- 2026-09-20: T1 done: `data-raw/json_export.R` writes both files through a binary connection; `hitopsr.json` 51,477 bytes, `hitopbr.json` 6,255 bytes.
- 2026-09-20: T2 done: `artifacts.R` gains `json_specs`, a `json` format and the two manifest rows (41 rows, the prior 39 unchanged); `test-artifacts.R` admits `json` in its format vocabulary and file pattern.
- 2026-09-20: T3 done: `test-json-export.R` reports every disagreement by field name; six plants (text, dropped item, swapped items, option label, instructions start, stem) each red under their own name; 222 passes across the export, artifact and staged-copy files.

## Decisions

## Review
