## JSON export of the HiTOP-SR and HiTOP-BR instruments (D-016).
##
## One file per form, `inst/extdata/<stem>.json`, holding the items, the
## response options and the administration instructions exactly as the
## package's tables hold them, so a page outside the package (hitop-form)
## can render the instrument without a copy of its content. No keying is
## exported: the page never scores, and the package rebuilds keying from its
## own tables when it reads a module descriptor (D-039).
##
## Sourced by data-raw/artifacts.R under its rebuild filters, which also
## record the manifest row and stage the site copy. Sourcing this file on
## its own writes both files and nothing else; run artifacts.R afterwards so
## the manifest and the staged copies follow.
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

json_specs <- list(
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

write_instrument_json <- function(spec, file) {
  number <- as.integer(spec$items[[spec$number_col]])
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
      text = as.character(spec$items$Text),
      stringsAsFactors = FALSE
    )
  )
  json <- jsonlite::toJSON(payload, auto_unbox = FALSE, pretty = TRUE)
  con <- file(file, open = "wb")
  on.exit(close(con))
  writeLines(enc2utf8(as.character(json)), con, useBytes = TRUE)
  invisible(file)
}

if (sys.nframe() == 0L) {
  devtools::load_all()
  for (spec in json_specs) {
    write_instrument_json(spec, file.path("inst/extdata", paste0(spec$stem, ".json")))
  }
}
