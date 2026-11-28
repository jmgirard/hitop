# M102: The JSON export's writer and lock catch the ten gaps M094 and M097 left open

**Status:** done (2026-09-21, PR #112 https://github.com/jmgirard/hitop/pull/112)

**Goal:** The JSON writer and `tests/testthat/test-json-export.R` catch the ten gaps that the M094 and M097 reviews left open. No shipped JSON byte moves.

**Outcome:** `write_instrument_json(spec, path, build_date = Sys.Date())` moved from `data-raw/json_export.R` into the unexported `R/json_export.R`. `data-raw/artifacts.R` passes it the manifest row's date. The script-mode blocks are gone, and the header names `artifacts.R` with `rebuild_formats` and `rebuild_stems` as the rebuild path. `latest_manifest()` moved to `helper-manifest.R`. `export_report()` parses with `simplifyVector = FALSE`, checks each scalar by JSON type, requires `options` and `items` to be arrays, reads fields with `[[`, checks `buildDate` as `YYYY-MM-DD`, and compares exact key sets at four levels (`keys.top`, `keys.instructions`, `keys.options`, `keys.items`). Each spec states its item count, and `table_text()` looks up text by item number. Each plant writes to its own `withr::local_tempfile()`. Two direct writer tests with no skip: an out-of-order table with an `NA` row, and a byte lock of all five specs against `inst/extdata/`. No shipped byte changed.

**Decisions:** D-065 (items in ascending number order, replacing D-063's "in table order"). The work log records three plan-gate choices: writer in `R/`, script mode removed, byte lock over a synthetic test alone.

**Review:** three-lens fan-out. AC1 to AC6 by fresh runs, `check()` 0/0/0, AC6 diff empty. Blame-history and prior-review lenses: no finding. Diff-bug lens, nine findings. Fixed now: R1 arrays, R2 `buildDate` format, R3 partial `$` matching, R4 truncated `number`, R8 POSIXct date. Rejected: R5 closed key set is the plan's choice, R6 byte lock's falsifier, R7 test specs stated separately on purpose. Noted: R9. After fixes 17583 pass. PR #112 merged at 8/8 CI green after a resumed wait, conversation empty. Nothing graduated or retired.
