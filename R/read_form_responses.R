#' Read hitop-form response files into one data frame
#'
#' Reads the CSV files that the hitop-form web page saves, one file per
#' participant, or the CSV download of a store the page sends to, one row per
#' participant, and binds them into one tibble that the scoring functions take
#' as it is. The page is at <https://jmgirard.github.io/hitop-form/>.
#'
#' @param path A directory that holds the response files, or a character
#'   vector of paths to them. A directory is read as every file in it whose
#'   name ends in `.csv` (in either case). The paths are sorted in the C locale before they are
#'   read, so the rows come back in the same order however the paths were
#'   supplied.
#'
#' @details A file the page saves holds one header row and one response row.
#'   A store's download, such as a Google Sheet's CSV export or a Supabase
#'   table's, holds one header row and one row per participant. Every response
#'   row of every file is a row of the result, the files in path order and the
#'   rows in file order. The first eight columns of the result are `study`,
#'   `participant`, `instrument`, `form_build`, `submitted`, `item_order`,
#'   `prolific_study` and `prolific_session`. The item columns follow from the
#'   ninth, one per item, named by the instrument's file
#'   stem and the item number (`hitopsr_001`, `hitopbr_01`, `pid5_001`,
#'   `pid5sf_001`, `pid5bf_01`). A module form saves only the module's items.
#'   The page keeps the item columns in the order it showed the items, except
#'   under a study link that asks for a random order: the page then draws a
#'   new order for each participant, keeps the item columns in the
#'   instrument's order (a module's items in the order its descriptor lists
#'   them, which `write_module()` writes ascending), and writes `item_order`.
#'
#'   `item_order` is the order the participant saw the items, as item numbers
#'   with no leading zero, joined by single spaces with none at either end
#'   (`hitopbr_01` is 1). The page writes it under a random order and not
#'   otherwise. A file may hold the column
#'   anywhere after `submitted`, as a store's download may append it after the
#'   item columns, and the result places it sixth. A file may also lack it:
#'   its rows then hold `NA` there. Scoring does not read the column, and it is
#'   not an item column, so it does not enter the check that every file holds
#'   the same item columns. A cell that is not blank and does not list the
#'   file's item numbers, each once, is an error naming the file and the
#'   response row.
#'
#'   `prolific_study` and `prolific_session` hold the study and session
#'   identifiers that Prolific adds to a study link, when the file records
#'   them for a study recruited through Prolific. The hitop-form page writes
#'   them when the link builder's "Recruit through Prolific" box is checked,
#'   and then takes the participant identifier from the Prolific ID in the
#'   page's address. A file may hold
#'   either or both anywhere after `submitted`, and the result places
#'   `prolific_study` seventh and `prolific_session` eighth. A row from a file
#'   without a column holds `NA` in it, and so does a blank cell. Scoring does
#'   not read them, and they are not item columns, so neither enters the
#'   check that every file holds the same item columns. The cells are read as
#'   written, with no check on their content.
#'
#'   Every file must carry the same item columns in the same order, because
#'   a set of files that differ cannot be one data frame: a full HiTOP-SR
#'   beside a module, or two modules that shuffled their items differently,
#'   need separate calls. A file that does not look like one the page saved
#'   (first columns other than the five the page writes first, a column that
#'   appears twice, a header with no
#'   response row, an item value that is not a whole number or is outside R's
#'   integer range, a date that does not parse) is an error naming the file.
#'   A `submitted` stamp may carry fractional seconds.
#'
#'   **Errors.** Files whose item columns differ from the first file's in
#'   name, in count or in order stop the read under the condition class
#'   `hitop_form_responses_mismatch`, and the message names each file that
#'   differs and how. A directory holding no `.csv` file stops it under
#'   `hitop_form_responses_none`. Both classes are a public contract a caller
#'   can catch by name.
#'
#' @return A \link[tibble]{tibble} with one row per response row. The first
#'   eight columns are `study`, `participant` and `instrument` as character,
#'   `form_build` as `Date`, `submitted` as `POSIXct` in UTC, and `item_order`,
#'   `prolific_study` and `prolific_session` as character, each `NA` on a row
#'   from a file without that column and on a blank cell. The item columns
#'   follow as integers, in the column order of the first file after sorting.
#'   An item the participant left blank is `NA`.
#'
#' @seealso [score_hitopsr()], [score_hitopbr()], [score_pid5()] and
#'   [read_module()], which score the item columns; the Collecting Responses
#'   Online article walks the Google Sheet route from the study link to the
#'   scores, and the modules article and `vignette("pid5_scoring")` show the
#'   hand-off for a module and for the PID-5.
#'
#' @examples
#' # Two files as the page saves them, here written by hand.
#' dir <- tempfile("responses")
#' dir.create(dir)
#' writeLines(
#'   c("study,participant,instrument,form_build,submitted,hitopbr_01,hitopbr_02",
#'     "demo,p001,hitopbr,2026-09-20,2026-09-20T21:20:36Z,4,1"),
#'   file.path(dir, "demo_p001.csv")
#' )
#' writeLines(
#'   c("study,participant,instrument,form_build,submitted,hitopbr_01,hitopbr_02",
#'     "demo,p002,hitopbr,2026-09-20,2026-09-21T09:02:11Z,2,"),
#'   file.path(dir, "demo_p002.csv")
#' )
#'
#' responses <- read_form_responses(dir)
#' responses
#'
#' unlink(dir, recursive = TRUE)
#'
#' @export
read_form_responses <- function(path) {
  files <- form_response_files(path)
  parts <- lapply(files, read_form_response_file)

  # The item columns follow the eight lead columns of the typed part, so no
  # optional lead column enters the comparison.
  n_lead <- length(form_result_columns)
  item_names <- lapply(parts, function(p) names(p)[-seq_len(n_lead)])
  reference <- item_names[[1L]]
  differs <- !vapply(item_names, identical, logical(1L), reference)
  if (any(differs)) {
    how <- vapply(item_names[differs], function(x) {
      if (length(x) != length(reference)) {
        "count"
      } else if (!setequal(x, reference)) {
        "names"
      } else {
        "order"
      }
    }, character(1L))
    # One line per differing file, each with its own reason.
    lines <- vapply(seq_along(how), function(i) {
      f <- files[differs][[i]]
      h <- how[[i]]
      cli::format_inline("{.file {f}} differs from it in {.field {h}}.")
    }, character(1L))
    cli::cli_abort(
      c(
        "The response files do not all hold the same item columns.",
        "i" = "The first file is {.file {files[[1L]]}}.",
        stats::setNames(lines, rep("x", length(lines))),
        "i" = "Read files from one form together, and other forms in a separate call."
      ),
      class = "hitop_form_responses_mismatch"
    )
  }

  out <- do.call(rbind, parts)
  rownames(out) <- NULL
  tibble::as_tibble(out)
}

# The five lead columns every hitop-form file starts with, in the page's order.
form_lead_columns <- c("study", "participant", "instrument", "form_build",
                       "submitted")

# The optional lead columns. A file may hold any of them anywhere after
# `submitted`, or lack them. The result carries all of them, in this order,
# after the five above, as character with NA where the file has no column or
# a blank cell.
form_optional_columns <- c("item_order", "prolific_study", "prolific_session")

# The lead columns of every result, in order. The item columns follow.
form_result_columns <- c(form_lead_columns, form_optional_columns)

# Blank cells as NA, for a character column read with `na.strings` empty.
blank_to_na <- function(v) {
  v[!nzchar(v)] <- NA_character_
  v
}

# The grammar of a non-blank `item_order` cell: item numbers with no leading
# zero, joined by single spaces, with no space at either end.
item_order_pattern <- "^[1-9][0-9]*( [1-9][0-9]*)*$"

# The grammar of an item column's name: the instrument's file stem, an
# underscore and the item number (`hitopbr_01`, `pid5_001`).
item_column_pattern <- "^[a-z0-9]+_[0-9]+$"

# Resolve `path` to the sorted vector of files to read.
form_response_files <- function(path, call = rlang::caller_env()) {
  cli_assert(
    condition = is.character(path) && length(path) >= 1L && !anyNA(path),
    message = "The {.arg path} argument must be a directory or a character vector of file paths.",
    call = call
  )
  if (length(path) == 1L && dir.exists(path)) {
    files <- list.files(path, pattern = "[.]csv$", full.names = TRUE,
                        ignore.case = TRUE)
    files <- files[!dir.exists(files)]
    if (length(files) == 0L) {
      cli::cli_abort(
        c(
          "No response file found in {.path {path}}.",
          "i" = "The directory holds no file whose name ends in {.code .csv}."
        ),
        class = "hitop_form_responses_none",
        call = call
      )
    }
  } else {
    files <- path
    missing <- files[!file.exists(files) | dir.exists(files)]
    if (length(missing) > 0L) {
      cli::cli_abort(
        c(
          "Every element of {.arg path} must be an existing file.",
          "x" = "Not a file: {.file {missing}}."
        ),
        call = call
      )
    }
  }
  # `method = "radix"` sorts in the C locale on every platform, so the order
  # does not depend on the session's collation.
  sort(files, method = "radix")
}

# The field count of each line of `file`, as `count.fields()` gives it, over
# a connection that strips a byte-order mark. NULL for a file with no line.
count_form_fields <- function(file) {
  con <- file(file, encoding = "UTF-8-BOM")
  on.exit(close(con))
  utils::count.fields(con, sep = ",", quote = "\"", comment.char = "")
}

# Read one file into a data frame with typed columns, one row per response
# row: one for a file the page saved, one per participant for a store's export.
read_form_response_file <- function(file, call = rlang::caller_env()) {
  # The field count of each record, taken before `read.csv()` pads a short
  # row or stops on a long one. `#` is data and a quoted line break is one
  # record, as `read.csv()` reads them: `count.fields()` gives NA on the line
  # a quoted record starts on and the record's count on its last line, so
  # the NA entries fold away. The connection strips a byte-order mark as
  # `read.csv()` does below. A file with nothing to count has no header.
  counts <- count_form_fields(file)
  records <- counts[!is.na(counts)]
  if (length(records) == 0L) {
    cli::cli_abort(
      c(
        "{.file {file}} is not a hitop-form response file.",
        "x" = "It holds no header row."
      ),
      call = call
    )
  }
  n_header <- records[[1L]]
  rows <- which(records[-1L] != n_header)
  if (length(rows) > 0L) {
    lines <- vapply(rows, function(r) {
      n <- records[[r + 1L]]
      cli::format_inline("Response row {r} holds {n} field{?s}, and the header holds {n_header}.")
    }, character(1L))
    cli::cli_abort(
      c(
        "{.file {file}} holds a response row whose field count differs from the header's.",
        stats::setNames(lines, rep("x", length(lines))),
        "i" = "The row is counted from the first row after the header."
      ),
      call = call
    )
  }

  # A file the page saved ends in a row ending; one edited by hand may not,
  # and that is not worth a warning.
  raw <- withCallingHandlers(
    utils::read.csv(
      file,
      colClasses = "character",
      check.names = FALSE,
      na.strings = character(0),
      fileEncoding = "UTF-8-BOM",
      strip.white = FALSE
    ),
    warning = function(w) {
      if (grepl("incomplete final line", conditionMessage(w), fixed = TRUE)) {
        invokeRestart("muffleWarning")
      }
    }
  )

  dup <- unique(names(raw)[duplicated(names(raw))])
  if (length(dup) > 0L) {
    cli::cli_abort(
      c(
        "{.file {file}} is not a hitop-form response file.",
        "x" = "Column{?s} {.field {dup}} appear{?s/} more than once."
      ),
      call = call
    )
  }

  lead <- names(raw)[seq_len(min(5L, ncol(raw)))]
  if (!identical(lead, form_lead_columns)) {
    cli::cli_abort(
      c(
        "{.file {file}} is not a hitop-form response file.",
        "x" = "Its first columns are {.val {lead}}, not {.val {form_lead_columns}}."
      ),
      call = call
    )
  }
  if (nrow(raw) == 0L) {
    cli::cli_abort(
      c(
        "{.file {file}} is not a hitop-form response file.",
        "x" = "It holds a header and no response row."
      ),
      call = call
    )
  }

  # Each item column as character with blanks as NA, then as integer. A file
  # the page saved holds one response row; a store's export holds one per
  # participant, so every check below runs down the column.
  item_cols <- setdiff(names(raw)[-seq_len(5L)], form_optional_columns)

  # An item column is named by the instrument's stem, an underscore and the
  # item number, and one file holds one instrument's columns, so the
  # `item_order` check below reads unambiguous numbers.
  named <- grepl(item_column_pattern, item_cols)
  if (!all(named)) {
    cli::cli_abort(
      c(
        "{.file {file}} holds a column that is not named as an item column.",
        "x" = "Column{?s} {.field {item_cols[!named]}}.",
        "i" = "An item column is named by the instrument's file stem, an underscore and the item number, as {.code hitopbr_01}."
      ),
      call = call
    )
  }
  stems <- unique(sub("_[0-9]+$", "", item_cols))
  if (length(stems) > 1L) {
    cli::cli_abort(
      c(
        "{.file {file}} holds item columns of more than one stem.",
        "x" = "Stems {.val {stems}}.",
        "i" = "A file holds one instrument's items. Read files from one form together, and other forms in a separate call."
      ),
      call = call
    )
  }

  values <- lapply(raw[item_cols], blank_to_na)
  whole <- lapply(values, function(v) is.na(v) | grepl("^-?[0-9]+$", v))
  bad <- item_cols[!vapply(whole, all, logical(1L))]
  if (length(bad) > 0L) {
    rows <- which(!Reduce(`&`, whole, init = rep(TRUE, nrow(raw))))
    cli::cli_abort(
      c(
        "{.file {file}} holds an item value that is not a whole number.",
        "x" = "Column{?s} {.field {bad}}.",
        "x" = "Response {cli::qty(length(rows))}row{?s} {rows}.",
        "i" = "The row is counted from the first row after the header."
      ),
      call = call
    )
  }

  ints <- lapply(values, function(v) suppressWarnings(as.integer(v)))
  fits <- lapply(seq_along(item_cols), function(i) {
    is.na(values[[i]]) | !is.na(ints[[i]])
  })
  wide <- item_cols[!vapply(fits, all, logical(1L))]
  if (length(wide) > 0L) {
    rows <- which(!Reduce(`&`, fits, init = rep(TRUE, nrow(raw))))
    cli::cli_abort(
      c(
        "{.file {file}} holds an item value outside the integer range.",
        "x" = "Column{?s} {.field {wide}}.",
        "x" = "Response {cli::qty(length(rows))}row{?s} {rows}.",
        "i" = "The row is counted from the first row after the header."
      ),
      call = call
    )
  }

  # Each optional lead column as character, blanks as NA, and a column of NA
  # for a file without it.
  optional <- lapply(form_optional_columns, function(col) {
    if (col %in% names(raw)) blank_to_na(raw[[col]]) else rep(NA_character_, nrow(raw))
  })
  names(optional) <- form_optional_columns

  # An `item_order` cell is blank, or the file's item numbers each once in
  # the order shown. The item number of a column is the digits after its last
  # underscore (`hitopbr_01` is 1).
  item_order <- optional$item_order
  numbers <- suppressWarnings(as.integer(sub(".*_", "", item_cols)))
  order_ok <- vapply(item_order, function(cell) {
    if (is.na(cell)) {
      return(TRUE)
    }
    if (!grepl(item_order_pattern, cell)) {
      return(FALSE)
    }
    parts <- strsplit(cell, " ", fixed = TRUE)[[1L]]
    parts <- suppressWarnings(as.integer(parts))
    !anyNA(parts) && !anyNA(numbers) &&
      identical(sort(parts), sort(numbers))
  }, logical(1L), USE.NAMES = FALSE)
  if (!all(order_ok)) {
    rows <- which(!order_ok)
    cli::cli_abort(
      c(
        "{.file {file}} holds an {.field item_order} value that is not the file's item numbers, each once.",
        "x" = "Response {cli::qty(length(rows))}row{?s} {rows}: {.val {item_order[rows]}}.",
        "i" = "The row is counted from the first row after the header."
      ),
      call = call
    )
  }

  # The stamps are matched whole, so a trailing fragment cannot slip past
  # the parser. `submitted` may carry fractional seconds, which
  # `Date.toISOString()` writes and the page trims.
  date_ok <- grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", raw$form_build)
  time_ok <- grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2}([.][0-9]+)?Z$",
                   raw$submitted)
  form_build <- as.Date(ifelse(date_ok, raw$form_build, NA_character_),
                        format = "%Y-%m-%d")
  submitted <- as.POSIXct(ifelse(time_ok, raw$submitted, NA_character_),
                          format = "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC")
  for (field in c("form_build", "submitted")) {
    parsed <- if (field == "form_build") form_build else submitted
    if (anyNA(parsed)) {
      rows <- which(is.na(parsed))
      got <- raw[[field]][rows]
      cli::cli_abort(
        c(
          "{.file {file}} holds a {.field {field}} value that does not parse.",
          "x" = "Response {cli::qty(length(rows))}row{?s} {rows}: {.val {got}}.",
          "i" = "The row is counted from the first row after the header."
        ),
        call = call
      )
    }
  }

  out <- data.frame(
    study = raw$study,
    participant = raw$participant,
    instrument = raw$instrument,
    form_build = form_build,
    submitted = submitted,
    stringsAsFactors = FALSE
  )
  out <- cbind(out, as.data.frame(optional, stringsAsFactors = FALSE))
  if (length(item_cols) > 0L) {
    items <- as.data.frame(ints, check.names = FALSE, stringsAsFactors = FALSE)
    out <- cbind(out, items)
  }
  out
}
