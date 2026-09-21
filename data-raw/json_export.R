## JSON export of the PID-5, HiTOP-SR and HiTOP-BR instruments (D-016).
##
## One file per form, `inst/extdata/<stem>.json`, holding the items, the
## response options and the administration instructions exactly as the
## package's tables hold them, so a web form outside the package can render
## the instrument without a copy of its content. No keying is
## exported: the page never scores, and the package rebuilds keying from its
## own tables when it reads a module descriptor (D-039).
##
## This file defines `json_specs` and writes nothing. data-raw/artifacts.R
## sources it and is the only way to rebuild the files. To rebuild one
## form's file, set `rebuild_formats <- "json"` and `rebuild_stems` to that
## form's stem (for example "pid5sf") in data-raw/artifacts.R, then source
## that script from the package root. It also records the manifest row and
## stages the site copy, so the file, its row and its copy stay in step.
##
## One instrument table can carry more than one form: `pid_items` numbers the
## FULL, SF and BF forms in three columns, each NA on the rows its form omits.
## A spec's `number_col` selects the form. The writer, its row selection and
## the format it writes are in `R/json_export.R` (`write_instrument_json()`,
## unexported).

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
