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
#'   rows in file order. The first five columns are `study`, `participant`,
#'   `instrument`, `form_build` and `submitted`; the item columns follow, one
#'   per item, named by the instrument's file stem and the item number
#'   (`hitopsr_001`, `hitopbr_01`, `pid5_001`, `pid5sf_001`, `pid5bf_01`). A
#'   module form saves only the module's items, in the order the form showed
#'   them.
#'
#'   Every file must carry the same item columns in the same order, because
#'   a set of files that differ cannot be one data frame: a full HiTOP-SR
#'   beside a module, or two modules that shuffled their items differently,
#'   need separate calls. A file that does not look like one the page saved
#'   (other lead columns, a column that appears twice, a header with no
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
#' @return A \link[tibble]{tibble} with one row per response row. The first five
#'   columns are `study`, `participant` and `instrument` as character,
#'   `form_build` as `Date` and `submitted` as `POSIXct` in UTC. The item
#'   columns follow as integers, in the column order of the first file after
#'   sorting. An item the participant left blank is `NA`.
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

  item_names <- lapply(parts, function(p) names(p)[-seq_len(5L)])
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

# Read one file into a data frame with typed columns, one row per response
# row: one for a file the page saved, one per participant for a store's export.
read_form_response_file <- function(file, call = rlang::caller_env()) {
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
  item_cols <- names(raw)[-seq_len(5L)]
  values <- lapply(raw[item_cols], function(v) {
    v[!nzchar(v)] <- NA_character_
    v
  })
  whole <- vapply(values, function(v) {
    all(is.na(v) | grepl("^-?[0-9]+$", v))
  }, logical(1L))
  bad <- item_cols[!whole]
  if (length(bad) > 0L) {
    cli::cli_abort(
      c(
        "{.file {file}} holds an item value that is not a whole number.",
        "x" = "Column{?s} {.field {bad}}."
      ),
      call = call
    )
  }

  ints <- lapply(values, function(v) suppressWarnings(as.integer(v)))
  fits <- vapply(seq_along(item_cols), function(i) {
    !any(!is.na(values[[i]]) & is.na(ints[[i]]))
  }, logical(1L))
  wide <- item_cols[!fits]
  if (length(wide) > 0L) {
    cli::cli_abort(
      c(
        "{.file {file}} holds an item value outside the integer range.",
        "x" = "Column{?s} {.field {wide}}."
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
      got <- raw[[field]][is.na(parsed)]
      cli::cli_abort(
        c(
          "{.file {file}} holds a {.field {field}} value that does not parse.",
          "x" = "Got {.val {got}}."
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
  if (length(item_cols) > 0L) {
    items <- as.data.frame(ints, check.names = FALSE, stringsAsFactors = FALSE)
    out <- cbind(out, items)
  }
  out
}
