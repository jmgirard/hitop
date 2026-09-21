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
## loads the package, writes both files and nothing else; run artifacts.R
## afterwards so the manifest and the staged copies follow.
##
## One instrument table can carry more than one form: `pid_items` numbers the
## FULL, SF and BF forms in three columns, each NA on the rows its form omits.
## A spec's `number_col` selects the form, and the writer keeps the rows where
## that column is not NA, in ascending order of it. A one-form table (the two
## HiTOP tables) has no NA in its number column, so the same step is a no-op
## there and the file it writes is unchanged.
##
## Format 1.0, top level:
##   format          "1.0"
##   package         "hitop"
##   packageVersion  the DESCRIPTION Version the file was built under
##   buildDate       YYYY-MM-DD, equal to the manifest row's build_date
##   stem            the file stem; item columns are `<stem>_<number>`
##   maxItem         the largest item number, which sets the zero-padding
##   instructions    { start, options: [ { value, label } ] }
##   items           [ { number, name, text } ] in table order
##
## Every scalar is `unbox()`ed with `auto_unbox = FALSE`, as write_module()
## does, so `options` and `items` stay arrays whatever their length. The file
## is written through a binary connection: a path passed to writeLines() is a
## text connection and emits CRLF on Windows (LESSONS 2026-07-16), which
## would break the md5 lock.

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

write_instrument_json <- function(spec, path) {
  number <- as.integer(spec$items[[spec$number_col]])
  keep <- !is.na(number)
  items <- spec$items[keep, , drop = FALSE]
  number <- number[keep]
  ord <- order(number)
  items <- items[ord, , drop = FALSE]
  number <- number[ord]
  max_n <- max(number)
  payload <- list(
    format = jsonlite::unbox("1.0"),
    package = jsonlite::unbox("hitop"),
    packageVersion =
      jsonlite::unbox(as.character(utils::packageVersion("hitop"))),
    buildDate = jsonlite::unbox(format(Sys.Date())),
    stem = jsonlite::unbox(spec$stem),
    maxItem = jsonlite::unbox(max_n),
    instructions = list(
      start = jsonlite::unbox(spec$instructions$start),
      options = data.frame(
        value = as.integer(spec$instructions$options$value),
        label = as.character(spec$instructions$options$label),
        stringsAsFactors = FALSE
      )
    ),
    items = data.frame(
      number = number,
      name = item_names(paste0(spec$stem, "_"), number, max_n),
      text = as.character(items$Text),
      stringsAsFactors = FALSE
    )
  )
  json <- jsonlite::toJSON(payload, auto_unbox = FALSE, pretty = TRUE)
  con <- file(path, open = "wb")
  on.exit(close(con))
  writeLines(enc2utf8(as.character(json)), con, useBytes = TRUE)
  invisible(path)
}

if (sys.nframe() == 0L) {
  for (spec in json_specs) {
    write_instrument_json(spec, file.path("inst/extdata", paste0(spec$stem, ".json")))
  }
}
