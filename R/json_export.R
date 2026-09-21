# Writes one instrument's JSON export, `inst/extdata/<stem>.json` (D-016,
# D-063, D-065). Unexported: `data-raw/artifacts.R` calls it through
# `devtools::load_all()` with the specs in `data-raw/json_export.R`, and
# `tests/testthat/test-json-export.R` calls it directly.
#
# `spec` is a list with `stem`, `items` (a keying table with a `Text` column),
# `number_col` (the column of `items` that numbers this form) and
# `instructions` (a `*_instructions` object). One table can carry more than
# one form: `pid_items` numbers the FULL, SF and BF forms in three columns,
# each NA on the rows its form omits. The writer keeps the rows where
# `number_col` is not NA, in ascending order of it.
#
# Format 1.0, top level:
#   format          "1.0"
#   package         "hitop"
#   packageVersion  the DESCRIPTION Version the file was built under
#   buildDate       `build_date` as YYYY-MM-DD; a date-time gives its date
#                   in its own time zone, not in UTC
#   stem            the file stem; item columns are `<stem>_<number>`
#   maxItem         the largest item number, which sets the zero-padding
#   instructions    { start, options: [ { value, label } ] }
#   items           [ { number, name, text } ] in ascending number order
#
# Every scalar is `unbox()`ed with `auto_unbox = FALSE`, as write_module()
# does, so `options` and `items` stay arrays whatever their length. The file
# is written through a binary connection: a path passed to writeLines() is a
# text connection and emits CRLF on Windows (LESSONS 2026-07-16), which
# would break the md5 lock.
write_instrument_json <- function(spec, path, build_date = Sys.Date()) {
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
    buildDate = jsonlite::unbox(
      if (inherits(build_date, "POSIXt")) {
        format(build_date, "%Y-%m-%d")
      } else {
        format(as.Date(build_date))
      }
    ),
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
