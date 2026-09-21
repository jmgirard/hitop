## JSON export of the PID-5, HiTOP-SR and HiTOP-BR instruments (D-016).
##
## One file per form, `inst/extdata/<stem>.json`, holding the items, the
## response options and the administration instructions exactly as the
## package's tables hold them, so a web form outside the package can render
## the instrument without a copy of its content. No keying is
## exported: the page never scores, and the package rebuilds keying from its
## own tables when it reads a module descriptor (D-039).
##
## Sourced unconditionally by data-raw/artifacts.R, whose rebuild filters
## gate only the write loop there and which also records the manifest row
## and stages the site copy; `source()` defines `json_specs` and the writer
## and writes nothing. Run as a script (`Rscript data-raw/json_export.R`) it
## loads the package, writes one file per spec and nothing else; run artifacts.R
## afterwards so the manifest and the staged copies follow.
##
## One instrument table can carry more than one form: `pid_items` numbers the
## FULL, SF and BF forms in three columns, each NA on the rows its form omits.
## A spec's `number_col` selects the form. The writer, its row selection and
## the format it writes are in `R/json_export.R` (`write_instrument_json()`,
## unexported).

## Script mode: `json_specs` below reads the package's tables, so the
## package is loaded before they are built, not in the write block at the end.
if (sys.nframe() == 0L) {
  devtools::load_all()
}

json_specs <- list(
  list(
    stem = "pid5",
    instrument = "PID-5",
    items = pid_items,
    number_col = "FULL",
    instructions = pid_instructions
  ),
  list(
    stem = "pid5sf",
    instrument = "PID-5-SF",
    items = pid_items,
    number_col = "SF",
    instructions = pid_instructions
  ),
  list(
    stem = "pid5bf",
    instrument = "PID-5-BF",
    items = pid_items,
    number_col = "BF",
    instructions = pid_instructions
  ),
  list(
    stem = "hitopsr",
    instrument = "HiTOP-SR",
    items = hitopsr_items,
    number_col = "HSR",
    instructions = hitopsr_instructions
  ),
  list(
    stem = "hitopbr",
    instrument = "HiTOP-BR",
    items = hitopbr_items,
    number_col = "HBR",
    instructions = hitopbr_instructions
  )
)

if (sys.nframe() == 0L) {
  for (spec in json_specs) {
    write_instrument_json(spec, file.path("inst/extdata", paste0(spec$stem, ".json")))
  }
}
