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

# The earliest version of the format that has ever existed. Equal to the
# written version today; it stays behind as the floor once that one moves on.
module_format_first_version <- function() {
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
#'       covers, and how many there are. Cross-checked on read: the order they
#'       are written in carries no meaning --- [read_module()] compares them as
#'       a set --- but a repeated number is an error, and the printed order of
#'       a shuffled form belongs in `itemOrder` instead.}
#'     \item{`itemOrder`}{The printed order of a shuffled form: a permutation
#'       of `items`. Optional --- a form printed in instrument order carries
#'       none. [read_module()] returns it on the module's `item_order`
#'       attribute, the same attribute [generate_docx_hitopsr()] returns, and
#'       [write_module()] writes it back from that attribute, so a descriptor
#'       read and written again keeps the order it recorded. The generators'
#'       `descriptor` argument sets the attribute for you.}
#'   }
#'
#'   `format`, `instrument`, and `scales` are required. The fields and the
#'   version string are a public contract and change only deliberately.
#'
#' @param module A `hitop_module` object, as returned by [hitop_module()]. An
#'   `item_order` attribute, where present, is written as the file's
#'   `itemOrder` and must be a permutation of the module's items.
#' @param file A string giving the path to write to.
#'
#' @return The `file` path, invisibly.
#'
#' @seealso [read_module()] to read the file back; [hitop_module()] to build a
#'   module in the first place; the `descriptor` argument of
#'   [generate_docx_hitopsr()], [generate_qualtrics_hitopsr()], and
#'   [generate_redcap_hitopsr()], which writes one of these files beside the
#'   instrument it builds.
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
  write_module_impl(module, file, call = rlang::current_env())
}

# Internal Helper: the body of write_module(), with the frame to blame
#
# Split out so that the generators' `descriptor` sidecar can write a file
# through the same code and still have a refusal name the exported generator
# the user called, which is the convention test-export-arg-guards.R enforces.
# write_module() keeps its two-argument signature; `call` is not a public
# argument.
write_module_impl <- function(module, file, call = rlang::caller_env()) {
  cli_assert(
    condition = is_module(module),
    message = c(
      "The {.arg module} argument must be a {.cls hitop_module} object.",
      i = "Build one with {.code hitop_module()}."
    ),
    call = call
  )
  cli_assert(
    condition = is.character(file) && length(file) == 1L && !is.na(file),
    message = "The {.arg file} argument must be a single string.",
    call = call
  )
  # An empty path is a string, so the guard above admits it -- and
  # `writeLines(json, con = "")` then opens an anonymous connection whose
  # contents are discarded, leaving no file and raising nothing the caller
  # sees. Refused here so a descriptor is never silently not written.
  cli_assert(
    condition = nzchar(file),
    message = c(
      "The {.arg file} argument must not be an empty string.",
      i = "Give the path the descriptor should be written to."
    ),
    call = call
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

  # A module carrying an `item_order` attribute records the order a shuffled
  # form printed its items in; read_module() returns exactly this attribute, so
  # writing it back is what makes a descriptor round-trip whole. Checked here
  # rather than trusted, because the attribute can be set by hand: an order
  # that is not a permutation of the module's items would write a file
  # read_module() then refuses.
  item_order <- attr(module, "item_order")
  if (!is.null(item_order)) {
    usable <- is.numeric(item_order) &&
      !anyNA(item_order) &&
      identical(sort(as.integer(item_order)), as.integer(module$items))
    cli_assert(
      condition = usable,
      message = c(
        "The {.arg module} argument has an unusable {.field item_order} \\
         attribute.",
        x = "It must be a permutation of the {module$nItems} item{?s} the \\
             module covers."
      ),
      call = call
    )
    payload$itemOrder <- as.integer(item_order)
  }

  json <- jsonlite::toJSON(payload, auto_unbox = FALSE, pretty = TRUE)
  # An unwritable path is reported the way every other failure in this file is
  # -- naming the file -- rather than as the bare "cannot open the connection"
  # that `writeLines()` raises on its own.
  rlang::try_fetch(
    suppressWarnings(writeLines(as.character(json), con = file)),
    error = function(cnd) {
      cli::cli_abort(
        c(
          "Cannot write the module descriptor to {.file {file}}.",
          i = "Check that the directory exists and is writable."
        ),
        parent = cnd,
        call = call
      )
    }
  )

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
#'   The list is exhaustive by design: a descriptor that is malformed rather
#'   than merely wrong --- a top level that is a JSON array instead of an
#'   object, or a number field that is not a flat array of numbers --- is
#'   refused as `hitop_module_file_invalid_json` or as the mismatch condition
#'   for the field it spoils, never as a bare R coercion error.
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

  # `simplifyDataFrame` and `simplifyMatrix` are off so that only the two
  # shapes the format actually uses survive parsing: a named object at the top
  # level, and flat arrays of scalars inside it. Left on, jsonlite turns an
  # array of objects into a data frame -- a *named* list, which walks straight
  # past the shape guard below -- and a nested array into a matrix, which
  # `as.integer()` then flattens into plausible-looking nonsense.
  parsed <- rlang::try_fetch(
    jsonlite::fromJSON(
      file,
      simplifyVector = TRUE,
      simplifyDataFrame = FALSE,
      simplifyMatrix = FALSE
    ),
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
  if (!is.list(parsed) || is.null(names(parsed)) || anyNA(names(parsed)) ||
      any(names(parsed) == "")) {
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
    recorded <- read_module_numbers(
      parsed$items,
      field = "items",
      file = file,
      class = "hitop_module_file_items_mismatch"
    )
    covered <- as.integer(module$items)
    # Compared as a set: the format states no order for `items`, so a
    # hand-written descriptor listing them any way round is a descriptor, not a
    # defect. A repeat is still an error -- it is not a set the module covers.
    absent <- setdiff(covered, recorded)
    extra <- setdiff(recorded, covered)
    duplicated_items <- unique(recorded[duplicated(recorded)])
    if (length(absent) > 0L || length(extra) > 0L ||
        length(duplicated_items) > 0L) {
      # Report which numbers differ, not merely how many: a file recording the
      # right number of wrong items is the case a count would describe as
      # "8 items against 8 items".
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
          x = if (length(duplicated_items) > 0L) {
            "Recorded more than once: {.val {duplicated_items}}."
          },
          i = if (length(absent) > 0L || length(extra) > 0L) {
            "The scale tables may have changed since the file was written."
          }
        ),
        class = "hitop_module_file_items_mismatch"
      )
    }
  }

  if (!is.null(parsed$nItems)) {
    # Checked against the rebuild, never against `parsed$items`: where `items`
    # is present the block above has already proven the two cover the same set,
    # so comparing to it would be a check that cannot fail, reported against a
    # field the file may not even carry.
    recorded_n <- read_module_numbers(
      parsed$nItems,
      field = "nItems",
      file = file,
      class = "hitop_module_file_items_mismatch"
    )
    if (length(recorded_n) != 1L ||
        !identical(recorded_n, as.integer(module$nItems))) {
      cli::cli_abort(
        c(
          "The module descriptor {.file {file}} disagrees with this package.",
          x = "Its {.field nItems} field says {.val {parsed$nItems}} but its \\
               {.field scales} cover {module$nItems} item{?s}."
        ),
        class = "hitop_module_file_items_mismatch"
      )
    }
  }

  if (!is.null(parsed$itemOrder)) {
    order <- read_module_numbers(
      parsed$itemOrder,
      field = "itemOrder",
      file = file,
      class = "hitop_module_file_bad_item_order"
    )
    ok <- length(order) == module$nItems &&
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

# Internal Helper: read one of the format's number fields.
#
# JSON permits shapes R's `as.integer()` either refuses outright -- a ragged
# array parses to a list, and coercing one throws a bare `simpleError` naming
# neither the field nor the file -- or accepts while quietly producing `NA`.
# Both are descriptor problems, so both are raised here as the classed,
# file-naming condition the caller would have raised for a wrong value.
read_module_numbers <- function(x, field, file, class,
                                call = rlang::caller_env()) {
  unusable <- !is.atomic(x) || is.character(x) && anyNA(suppressWarnings(as.integer(x)))
  if (!unusable) {
    x <- suppressWarnings(as.integer(x))
    unusable <- length(x) == 0L || anyNA(x)
  }
  if (unusable) {
    cli::cli_abort(
      c(
        "The module descriptor {.file {file}} has an unreadable \\
         {.field {field}}.",
        x = "It must be a JSON array of item numbers."
      ),
      class = class,
      call = call
    )
  }
  x
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

  # `"1.0"` is the first version of the format, so anything below it names a
  # version that has never existed -- as much a sign of a file this reader
  # should not guess at as one from the future.
  if (numeric_version(format) < numeric_version(module_format_first_version())) {
    cli::cli_abort(
      c(
        "The module descriptor {.file {file}} is in format \\
         {.val {format}}, which is not a version of this format.",
        i = "The earliest is {.val {module_format_first_version()}}; this \\
             release writes {.val {written}}."
      ),
      class = "hitop_module_file_unsupported_format",
      call = call
    )
  }

  invisible(format)
}

# Internal Helper: write a generator's sidecar descriptor
#
# Shared by the three HiTOP-SR generators' `descriptor` argument. `module =
# NULL` is the full instrument, described here as a module over every scale the
# instrument offers, so a full administration gets a descriptor too rather than
# the argument quietly doing nothing. `item_order` is the original item numbers
# in the order a form printed them; NULL leaves the field out, which is what a
# form printed in instrument order deserves.
write_descriptor_sidecar <- function(
  descriptor,
  module,
  instrument,
  item_order = NULL,
  call = rlang::caller_env()
) {
  validate_string(descriptor, "descriptor", call = call)
  # Named for the argument the user actually passed; `write_module_impl()`
  # refuses the same value below, but blames `file`.
  cli_assert(
    condition = nzchar(descriptor),
    message = c(
      "The {.arg descriptor} argument must not be an empty string.",
      i = "Give the path the descriptor should be written to."
    ),
    call = call
  )
  if (is.null(module)) {
    module <- hitop_module(
      instrument = instrument,
      scales = module_scale_tables()[[instrument]]$Scale,
      call = call
    )
  }
  # Set unconditionally, so that a NULL `item_order` CLEARS any attribute the
  # incoming `module` already carried. A module read back from a shuffled Word
  # form's descriptor carries one, and without this an export that never
  # shuffled would inherit that form's printed order.
  attr(module, "item_order") <-
    if (is.null(item_order)) NULL else as.integer(item_order)
  # The writer's own abort is left to speak: it names the path, which is the
  # fact the caller needs, and re-wrapping it here would re-interpolate a
  # message that has already been formatted. `call` is passed through so the
  # refusal blames the generator the user called.
  write_module_impl(module, descriptor, call = call)
}

# Internal Helper: refuse a `descriptor` that would be overwritten by the form
#
# The descriptor is written first and the instrument file second, so pointing
# both at one path leaves the export where the descriptor was, with `built`
# TRUE and no rollback -- a success message and no descriptor. Compared after
# `normalizePath()` so `./m.json` and `m.json` are seen as one path; neither
# file need exist yet, hence `mustWork = FALSE`.
validate_descriptor_target <- function(descriptor, file, call = rlang::caller_env()) {
  if (is.null(descriptor) || !rlang::is_string(file)) {
    return(invisible(NULL))
  }
  # Normalize the DIRECTORY, which exists, rather than the file, which need
  # not: normalizePath() returns a non-existent path unchanged, so comparing
  # the paths whole would miss `dir/./m.json` against `dir/m.json`.
  resolved <- function(path) {
    file.path(
      normalizePath(dirname(path), mustWork = FALSE),
      basename(path)
    )
  }
  same <- resolved(descriptor) == resolved(file)
  cli_assert(
    condition = !same,
    message = c(
      "The {.arg descriptor} and {.arg file} arguments must name different \\
       paths.",
      x = "Both name {.file {file}}.",
      i = "The instrument file would overwrite the descriptor."
    ),
    call = call
  )
  invisible(NULL)
}
