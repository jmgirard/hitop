<!-- Section ownership + write-modes: see tracking-rules.md "Milestone-file
     section ownership". A phase skill never rewrites another phase's section. -->
# M108: A module descriptor names its export's item columns

- **Status:** planned
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** IP1, IP2, GP3
- **Resolves:** —
- **Surface tier:** user-facing — adds a field to a public file format and changes what two exported scoring functions accept
- **Branch/PR:** —

## Goal

A researcher scores data from a Qualtrics or REDCap HiTOP-SR export by passing the descriptor the generator wrote, with no item column names typed out.

## Scope

**In:** an optional `columns` field in the module descriptor. The format string stays `"1.0"`. The Qualtrics and REDCap HiTOP-SR generators write the field through their `descriptor` sidecar. `read_module()` returns it on the module's `columns` attribute, and `write_module()` writes that attribute back. `score_hitopsr()` and `reliability_hitopsr()` use a module's columns when `items` is missing or `NULL`. D-067 records the field and its condition class. Help pages, the modules vignette and NEWS describe both.

**Out:** the Word form, which is paper and has no columns, so its descriptor has no field. The hitop-form page, which ignores fields it does not read, so it needs no change (the online-form candidate row). Scoring data that arrived with no descriptor stays in its own candidate row. Modules exist only for the HiTOP-SR (the BR/PID-5 modularization candidate row). If T1 cannot source the Qualtrics claim, Qualtrics goes to a candidate row through an amendment gate.

## Acceptance criteria

- [ ] AC1: `generate_redcap_hitopsr(descriptor = )` and `generate_qualtrics_hitopsr(descriptor = )` write a `columns` field. It holds one string per module item, in ascending item-number order. It equals the names on the export's item rows. For REDCap, these are the dictionary's `Variable / Field Name` values on its `radio` rows. For Qualtrics, these are the `[[ID:...]]` tokens on its multiple-choice questions. The instructions row is not an item row. A test parses each generated export and compares its item names to the descriptor's `columns`. The test also compares `columns` to names built from the module's item numbers with `sprintf()`, not with the package's `item_names()`. It covers the full instrument and a two-scale module, and for Qualtrics the default and one other `id_prefix`. The help pages call these names the names that the generated file assigns. A note under `cairn/references/` cites the REDCap and Qualtrics help pages that make those names the data export's column names. The note also says what breaks that link, for example a question renamed after import.
- [ ] AC2: `generate_docx_hitopsr(descriptor = )` writes a descriptor with no `columns` field. `write_module()` writes no `columns` field for a module with no `columns` attribute. The Word case holds also when the module passed in already carries a `columns` attribute from an earlier descriptor. A Qualtrics or REDCap sidecar replaces a carried attribute with its own export's names. The tests pass a module read from a REDCap descriptor to the Word generator and to the Qualtrics generator with another `id_prefix`. Every descriptor these tests write carries `format` `"1.0"`.
- [ ] AC3: `read_module()` puts a file's `columns` on the module's `columns` attribute as a character vector. A file with no field, or with `"columns": null`, reads back with no such attribute. A bare string reads as a vector of length one. `write_module()` writes the attribute as a JSON array, so a descriptor read and written again has the same `columns` value. Tests cover a file with the field, one without it, one with `null`, and a one-item module with a bare string.
- [ ] AC4: `read_module()` refuses a bad `columns` field under the new condition class `hitop_module_file_bad_columns`. The message names the file. These cases are bad. The value is an object or a number. An element is a number, a boolean, `null`, `""`, an array or an object. The length differs from the module's item count. A name occurs twice. One test per case asserts the class. `write_module()` refuses a `columns` attribute that is not character, holds `NA` or `""`, has the wrong length or repeats a name. It refuses before it opens the path, with a {cli} error that names `columns`, as its other refusals do. One test per case asserts the message and that no file is created.
- [ ] AC5: `score_hitopsr()` and `reliability_hitopsr()` treat a missing or `NULL` `items` as omitted. Take a call with `items` omitted and a module that carries a `columns` attribute. Its result is identical to the same call with those names passed as `items`. A call with `items` omitted aborts with a {cli} error saying to pass `items` in three cases: `module = NULL`, a module with no `columns` attribute, and `layout = "printed"`. A supplied `items` works as it does today, with or without the attribute. One test runs end to end. It generates a REDCap module export and descriptor, and names simulated data by the dictionary it parses. It then checks that scoring from `read_module()` alone is identical to scoring with those parsed names as `items`. (RB tripwire: irreversible-api)
- [ ] AC6: D-067 records the field, the class, and that format 1.0 readers ignore fields they do not know. The Errors list of `?read_module` names the new class. `?read_module`, `?write_module`, the two generators' `descriptor` argument, `?score_hitopsr`, `?reliability_hitopsr`, the modules vignette and `NEWS.md` describe the field and the call with `items` omitted. `devtools::document()` leaves no diff. `devtools::test()` reports 0 failures, and `devtools::check()` reports 0 errors and 0 warnings. Each runs locally, or in the `R-CMD-check.yaml` jobs at the PR head where the local toolchain cannot run it.

## Coverage

- AC1 → T1, T2, T3
- AC2 → T2, T3
- AC3 → T4
- AC4 → T4
- AC5 → T5
- AC6 → T6

## Tasks

- [ ] T1: Sources. Read the REDCap help on the data dictionary and data export. Read the Qualtrics help on the Advanced Format `[[ID:]]` tag and on export column headers. Write a source note under `cairn/references/` for each page the plan relies on, with its `INDEX.md` line. If a Qualtrics page does not confirm the claim, stop at an amendment gate that moves Qualtrics to a candidate row.
- [ ] T2: Writer tests first, in `tests/testthat/test-generator-descriptor.R`. Parse the REDCap dictionary and the Qualtrics text file, and keep only item rows. Build the expected names with `sprintf()` from the module's item numbers. Add the carried-attribute probes from AC2. See each test red before T3.
- [ ] T3: Writer. `write_descriptor_sidecar()` (`R/module_file.R:613`) takes the export's names and sets `columns` every time, clearing it when there are none, as it does `item_order`. The two generators pass the names they build (`R/generate_redcap.R:306`, `R/generate_qualtrics.R:302`). `write_module_impl()` checks the attribute and writes it after `itemOrder`.
- [ ] T4: Reader. Read `columns` from the second parse with `simplifyVector = FALSE` (LESSONS, M054). Add the refusals and the new class. Tests first for AC3 and AC4.
- [ ] T5: Scoring. Tests first for AC5. In both functions, when `items` is missing or `NULL`, take `attr(module, "columns")` before `layout_items()`, or abort. Add no new argument, so no partial match can change (LESSONS, M043).
- [ ] T6: Records and docs. Roxygen for the six help pages, the modules vignette section on scoring, NEWS entries, and `_pkgdown.yml` if an index entry moves. Run `devtools::document()`, `devtools::test()` and `devtools::check()`.

## Work log

- 2026-09-22: created by /milestone-plan. Absorbs the candidate row on the descriptor's missing column names (lineage M054, M107).
- 2026-09-22: criteria audit, full mode, by a fresh [O] reader. It returned 12 findings. 10 were fixed in the draft (instructions row, oracle apart from `item_names()`, doc wording, carried attributes, `null` and bare string, nested probes, writer refusal, `NULL` inputs, end-to-end oracle, Errors list and CI check). 2 were posed at the gate (format version, Qualtrics source).
- 2026-09-22: plan gate chose to keep format `"1.0"` with an optional field over a bump to `"1.1"` because a bump makes older hitop and the hitop-form page refuse every new descriptor; falsified by a 1.0 reader that fails on a field it does not know.
- 2026-09-22: plan gate chose an optional `items` in the two scoring functions over a `module_columns()` helper export because the goal is scoring from the descriptor alone; falsified by a caller scoring the wrong columns because `items` was left out by mistake.
- 2026-09-22: plan gate chose to cover Qualtrics when a vendor page confirms it over REDCap alone because both generators write descriptors; falsified by Qualtrics help that names another source for export column headers.

## Decisions

## Review
