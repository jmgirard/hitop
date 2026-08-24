# The descriptor format version this release writes.
#
# A public contract (D-039): it changes only through a further decision entry.
# read_module() accepts this version and any earlier one, and refuses anything
# later, because a later file may carry fields this release would silently
# ignore. Tests assert the written value against the literal "1.0" rather than
# against write_module()'s own output, so a change here is visible.
module_format_version <- function() {
  "1.0"
}

#' Save a Module to a File
#'
#' @description Writes a [hitop_module()] descriptor to a JSON file, so that a
#'   researcher can keep it beside the form they field and read it back at
#'   scoring time with [read_module()] instead of retyping every scale name.
#'
#'   The file records the scale names, not the keying: [read_module()] rebuilds
#'   the items and their reverse-keying flags from this package's own tables.
#'   The recorded `items` are there for a human reader and as a cross-check,
#'   and a file that disagrees with what the package derives is an error rather
#'   than a silent preference for either side.
#'
#' @section The descriptor format:
#'
#'   The file is JSON, with these fields:
#'
#'   \describe{
#'     \item{`format`}{The format version, a `"major.minor"` string. This
#'       release writes `"1.0"`.}
#'     \item{`package`, `packageVersion`, `buildDate`}{The package that wrote
#'       the file, its version, and the date it was written. Recorded for the
#'       reader; [read_module()] ignores all three.}
#'     \item{`instrument`}{The instrument the module belongs to.}
#'     \item{`scales`}{The module's scales, as they are printed on the
#'       instrument. **Required**: these are what the module is rebuilt from.}
#'     \item{`items`, `nItems`}{The original instrument item numbers the module
#'       covers, and how many there are. Cross-checked on read.}
#'     \item{`itemOrder`}{Reserved for the printed order of a shuffled form: a
#'       permutation of `items`. [write_module()] never writes it, because a
#'       module object records no printed order; [read_module()] accepts one
#'       and returns it on the `item_order` attribute, the same attribute
#'       [generate_docx_hitopsr()] returns.}
#'   }
#'
#'   `format`, `instrument`, and `scales` are required. The fields and the
#'   version string are a public contract and change only deliberately.
#'
#' @param module A `hitop_module` object, as returned by [hitop_module()].
#' @param file A string giving the path to write to.
#'
#' @return The `file` path, invisibly.
#'
#' @seealso [read_module()] to read the file back; [hitop_module()] to build a
#'   module in the first place.
#'
#' @examples
#' m <- hitop_module("hitopsr", scales = c("Agoraphobia", "Appetite Loss"))
#'
#' f <- tempfile(fileext = ".json")
#' write_module(m, f)
#' cat(readLines(f), sep = "\n")
#'
#' identical(read_module(f), m)
#'
#' file.remove(f)
#'
#' @export
write_module <- function(module, file) {
  cli_assert(
    condition = is_module(module),
    message = c(
      "The {.arg module} argument must be a {.cls hitop_module} object.",
      i = "Build one with {.code hitop_module()}."
    )
  )
  cli_assert(
    condition = is.character(file) && length(file) == 1L && !is.na(file),
    message = "The {.arg file} argument must be a single string."
  )

  # `unbox()` on every scalar, with `auto_unbox = FALSE`, so that `scales` and
  # `items` stay JSON arrays even for a module holding one of either. Under
  # `auto_unbox = TRUE` a length-one vector would collapse to a bare value and
  # the format would not be one shape.
  payload <- list(
    format = jsonlite::unbox(module_format_version()),
    package = jsonlite::unbox("hitop"),
    packageVersion =
      jsonlite::unbox(as.character(utils::packageVersion("hitop"))),
    buildDate = jsonlite::unbox(format(Sys.Date())),
    instrument = jsonlite::unbox(module$instrument),
    scales = as.character(module$scales),
    items = as.integer(module$items),
    nItems = jsonlite::unbox(as.integer(module$nItems))
  )

  json <- jsonlite::toJSON(payload, auto_unbox = FALSE, pretty = TRUE)
  writeLines(as.character(json), con = file)

  invisible(file)
}

#' Read a Module from a File
#'
#' @description Reads a module descriptor written by [write_module()] (or
#'   written by hand to the same format) and returns the [hitop_module()]
#'   object it describes, ready to pass to [score_hitopsr()],
#'   [reliability_hitopsr()], or any of the generators.
#'
#'   The file never supplies keying. The module is rebuilt by passing the
#'   file's `scales` through [hitop_module()], so this package's own tables
#'   remain the only source of which items belong to a scale. The file's
#'   recorded `items` and `nItems`, where present, are checked against that
#'   rebuild, and a disagreement is an error: a descriptor written against
#'   scale tables that have since moved fails loudly rather than scoring
#'   quietly.
#'
#' @inheritSection write_module The descriptor format
#'
#' @param file A string giving the path to read from.
#'
#' @return A `hitop_module` object. If the file carries an `itemOrder`, it is
#'   returned on the object's `item_order` attribute --- the same attribute
#'   [generate_docx_hitopsr()] returns for a shuffled form.
#'
#' @section Errors:
#'
#'   Every failure below aborts with a condition naming the file, so a caller
#'   may catch a particular one by class: `hitop_module_file_missing`,
#'   `hitop_module_file_invalid_json`, `hitop_module_file_missing_field`,
#'   `hitop_module_file_unsupported_format`, `hitop_module_file_unknown_scales`
#'   (which carries [hitop_module()]'s own refusal as its parent),
#'   `hitop_module_file_items_mismatch`, and `hitop_module_file_bad_item_order`.
#'
#' @seealso [write_module()] to write the file; [hitop_module()] to build a
#'   module without one.
#'
#' @examples
#' m <- hitop_module("hitopsr", scales = c("Agoraphobia", "Appetite Loss"))
#'
#' f <- tempfile(fileext = ".json")
#' write_module(m, f)
#'
#' m2 <- read_module(f)
#' m2
#' identical(m2, m)
#'
#' file.remove(f)
#'
#' @export
read_module <- function(file) {
  cli_assert(
    condition = is.character(file) && length(file) == 1L && !is.na(file),
    message = "The {.arg file} argument must be a single string."
  )
  if (!file.exists(file)) {
    cli::cli_abort(
      c(
        "No module descriptor at {.file {file}}.",
        i = "Write one with {.code write_module()}."
      ),
      class = "hitop_module_file_missing"
    )
  }

  parsed <- rlang::try_fetch(
    jsonlite::fromJSON(file, simplifyVector = TRUE),
    error = function(cnd) {
      cli::cli_abort(
        "The module descriptor {.file {file}} is not valid JSON.",
        class = "hitop_module_file_invalid_json",
        parent = cnd
      )
    }
  )
  # A JSON document whose top level is an array or a bare value parses without
  # error but is not a descriptor; it is rejected here rather than reported as
  # a pile of missing fields.
  if (!is.list(parsed) || is.null(names(parsed))) {
    cli::cli_abort(
      c(
        "The module descriptor {.file {file}} is not valid JSON.",
        x = "Its top level is not an object of named fields."
      ),
      class = "hitop_module_file_invalid_json"
    )
  }

  for (field in c("format", "instrument", "scales")) {
    if (is.null(parsed[[field]])) {
      cli::cli_abort(
        c(
          "The module descriptor {.file {file}} is missing a required field.",
          x = "No {.field {field}} field."
        ),
        class = "hitop_module_file_missing_field"
      )
    }
  }

  read_module_check_format(parsed$format, file = file)

  module <- rlang::try_fetch(
    hitop_module(
      instrument = parsed$instrument,
      scales = as.character(parsed$scales)
    ),
    error = function(cnd) {
      cli::cli_abort(
        c(
          "Cannot rebuild the module described by {.file {file}}.",
          i = "See {.code available_scales()} for the names this package knows."
        ),
        class = "hitop_module_file_unknown_scales",
        parent = cnd
      )
    }
  )

  # The rebuild above is the only source of keying (D-039). What the file
  # recorded is compared against it, never substituted for it.
  if (!is.null(parsed$items)) {
    recorded <- suppressWarnings(as.integer(parsed$items))
    if (!identical(recorded, as.integer(module$items))) {
      # Report which numbers differ, not merely how many: a file recording the
      # right number of wrong items is the case a count would describe as
      # "8 items against 8 items".
      absent <- setdiff(module$items, recorded)
      extra <- setdiff(recorded, module$items)
      cli::cli_abort(
        c(
          "The module descriptor {.file {file}} disagrees with this package.",
          x = "Its {.field items} field does not match the items its \\
               {.field scales} cover in this version of the package.",
          x = if (length(absent) > 0L) {
            "Covered but not recorded: {.val {absent}}."
          },
          x = if (length(extra) > 0L) {
            "Recorded but not covered: {.val {extra}}."
          },
          x = if (length(absent) == 0L && length(extra) == 0L) {
            "It records the same {length(recorded)} item{?s} out of ascending \\
             order or with duplicates."
          },
          i = "The scale tables may have changed since the file was written."
        ),
        class = "hitop_module_file_items_mismatch"
      )
    }
  }

  if (!is.null(parsed$nItems)) {
    covered <- if (is.null(parsed$items)) {
      module$nItems
    } else {
      length(parsed$items)
    }
    if (!identical(suppressWarnings(as.integer(parsed$nItems)), as.integer(covered))) {
      cli::cli_abort(
        c(
          "The module descriptor {.file {file}} disagrees with itself.",
          x = "Its {.field nItems} field says {parsed$nItems} but its \\
               {.field items} field carries {covered}."
        ),
        class = "hitop_module_file_items_mismatch"
      )
    }
  }

  if (!is.null(parsed$itemOrder)) {
    order <- suppressWarnings(as.integer(parsed$itemOrder))
    ok <- length(order) == module$nItems &&
      !anyNA(order) &&
      identical(sort(order), as.integer(module$items))
    if (!ok) {
      cli::cli_abort(
        c(
          "The module descriptor {.file {file}} has an unusable \\
           {.field itemOrder}.",
          x = "It must be a permutation of the {module$nItems} item{?s} the \\
               module covers."
        ),
        class = "hitop_module_file_bad_item_order"
      )
    }
    attr(module, "item_order") <- order
  }

  module
}

# Internal Helper: is this file's `format` one this release can read?
#
# Split out of read_module() so the two refusals -- a value that is not a
# version string at all, and a version later than this release writes -- sit
# beside each other. Both carry the same class: a caller catching an
# unreadable format does not care which shape of unreadable it was.
read_module_check_format <- function(format, file, call = rlang::caller_env()) {
  written <- module_format_version()
  looks_like_version <- is.character(format) &&
    length(format) == 1L &&
    !is.na(format) &&
    grepl("^[0-9]+\\.[0-9]+$", format)

  if (!looks_like_version) {
    cli::cli_abort(
      c(
        "The module descriptor {.file {file}} has an unreadable \\
         {.field format}.",
        x = "Expected a version string such as {.val {written}}."
      ),
      class = "hitop_module_file_unsupported_format",
      call = call
    )
  }

  if (numeric_version(format) > numeric_version(written)) {
    cli::cli_abort(
      c(
        "The module descriptor {.file {file}} is in format \\
         {.val {format}}, which this version of {.pkg hitop} cannot read.",
        i = "It writes format {.val {written}}. Update {.pkg hitop}."
      ),
      class = "hitop_module_file_unsupported_format",
      call = call
    )
  }

  invisible(format)
}
