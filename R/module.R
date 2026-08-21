#' Describe a Module of an Instrument's Scales
#'
#' @description Builds a validated description of a **module**: a chosen set of
#'   an instrument's scales, administered and scored on its own. Supplying the
#'   result as the `module` argument of a generator produces an instrument
#'   containing only the items belonging to those scales, **keeping each item's
#'   original number**, so that data collected with the module can still be
#'   scored against the full instrument's key. Supplying it again to
#'   [score_hitopsr()] or [reliability_hitopsr()] scores the collected columns.
#'
#'   Use [available_scales()] to see which scales an instrument offers.
#'
#' @param instrument A string naming the instrument to build a module from.
#'   Currently only `"hitopsr"` is supported. (default = `"hitopsr"`)
#' @param scales A character vector of scale names to keep. Names may be given
#'   either as they are printed on the instrument (`"Antisocial Behavior"`) or
#'   as the camelCase stems used in scored output (`"antisocialBehavior"`), in
#'   any mixture and ignoring case. Duplicates are dropped.
#'
#' @return An object of class `hitop_module`: a list with the resolved
#'   `instrument`, the canonical display `scales` and their `camelCase` stems,
#'   the `items` kept (original instrument numbering, ascending), the parallel
#'   `reverse` keying flags, and `nItems`.
#'
#' @seealso [available_scales()] for the scale names this accepts;
#'   [generate_docx_hitopsr()], [generate_qualtrics_hitopsr()], and
#'   [generate_redcap_hitopsr()], each of which takes a `module` argument;
#'   [score_hitopsr()] and [reliability_hitopsr()] for scoring the result.
#'
#' @examples
#' # Describe a two-scale module of the HiTOP-SR
#' m <- hitop_module("hitopsr", scales = c("Agoraphobia", "Appetite Loss"))
#' m
#'
#' # The item numbers are the original HiTOP-SR numbers, not 1..8
#' m$items
#'
#' @export
hitop_module <- function(instrument = "hitopsr", scales) {
  cli_assert(
    condition = is.character(instrument) && length(instrument) == 1L,
    message = "The {.arg instrument} argument must be a single string."
  )
  instrument <- tolower(instrument)

  supported <- "hitopsr"
  planned <- c("hitopbr", "pid5", "pid5sf", "pid5bf")
  cli_assert(
    condition = instrument %in% c(supported, planned),
    message = c(
      "Unknown {.arg instrument} value {.val {instrument}}.",
      i = "Currently supported: {.val {supported}}."
    )
  )
  if (!instrument %in% supported) {
    cli::cli_abort(c(
      "Scale modules are not yet supported for {.val {instrument}}.",
      i = "Only {.val {supported}} can be built into modules at present."
    ))
  }

  cli_assert(
    condition = length(scales) > 0L,
    message = c(
      "The {.arg scales} argument must name at least one scale.",
      i = "See {.code hitopsr_scales$camelCase} for the available names."
    )
  )
  cli_assert(
    condition = is.character(scales),
    message = "The {.arg scales} argument must be a character vector."
  )
  cli_assert(
    condition = !anyNA(scales),
    message = "The {.arg scales} argument must not contain missing values."
  )

  ref <- hitopsr_scales
  # A scale is matchable by either of its names, compared case-insensitively;
  # the two name columns never collide across different scales.
  lookup <- c(tolower(ref$Scale), tolower(ref$camelCase))
  rows <- rep(seq_len(nrow(ref)), times = 2L)

  idx <- rows[match(tolower(scales), lookup)]
  unknown <- unique(scales[is.na(idx)])
  if (length(unknown) > 0L) {
    cli::cli_abort(c(
      "Unknown scale name{?s}: {.val {unknown}}.",
      i = "See {.code hitopsr_scales$camelCase} for the {nrow(ref)} available names."
    ))
  }

  idx <- sort(unique(idx))

  items <- sort(unique(unlist(ref$itemNumbers[idx], use.names = FALSE)))
  keep <- match(items, hitopsr_items$HSR)

  structure(
    list(
      instrument = instrument,
      scales = ref$Scale[idx],
      camelCase = ref$camelCase[idx],
      items = items,
      reverse = hitopsr_items$Reverse[keep],
      nItems = length(items)
    ),
    class = "hitop_module"
  )
}

#' @export
print.hitop_module <- function(x, ...) {
  # `cat()` rather than {cli}: cli writes to the message connection, and a
  # print method must write to stdout.
  n_scales <- length(x$scales)
  # The label is read off the object rather than hardcoded, so the deprecated
  # `hitop_subset` class prints as itself when it delegates here.
  label <- class(x)[[1L]]
  cat(
    cli::pluralize(
      "<{label}> {x$instrument}: {x$nItems} item{?s} from ",
      "{n_scales} scale{?s}"
    ),
    "\n",
    sep = ""
  )
  cat(paste0("* ", x$scales, "\n"), sep = "")
  invisible(x)
}

# Internal Helper: reduce an items table and a scales table to a module
#
# `module` may be NULL (returns both tables untouched, so callers need no
# branch). Item numbering is never rewritten: the reduced tables carry the
# instrument's original numbers, with gaps where items were dropped.
apply_module <- function(
  items,
  scales,
  module,
  item_col,
  scale_col = "camelCase",
  call = rlang::caller_env()
) {
  if (is.null(module)) {
    return(list(items = items, scales = scales))
  }

  cli_assert(
    condition = is_module(module),
    message = c(
      "The {.arg module} argument must be a {.cls hitop_module} object.",
      i = "Build one with {.code hitop_module()}."
    ),
    call = call
  )

  list(
    items = items[items[[item_col]] %in% module$items, , drop = FALSE],
    scales = if (is.null(scales)) {
      NULL
    } else {
      scales[scales[[scale_col]] %in% module$camelCase, , drop = FALSE]
    }
  )
}

# Internal Helper: remap a module descriptor into the engines' three inputs
#
# `apply_module()` above reduces the instrument's TABLES for the generators,
# which keep the original item numbering. The engines instead address items by
# POSITION within the columns the caller supplied, so scoring module-collected
# data needs the module's original numbers translated into positions within
# `module$items` (which is ascending). Returns `n_items`, `reverse_items`, and
# `items_scales` ready for score_engine()/reliability_engine().
#
# `items` and `scales` are the instrument's own tables, so the reverse key is
# read from the package's canonical source rather than trusted from the
# descriptor's parallel `reverse` flags.
#
# Invariant: every kept scale's items are fully contained in `module$items`,
# because hitop_module() builds `items` as the union of exactly the scales it
# keeps. The match() below therefore never yields NA for a kept scale.
module_engine_inputs <- function(
  module,
  instrument,
  items,
  scales,
  item_col,
  reverse_col = "Reverse",
  scale_col = "camelCase",
  number_col = "itemNumbers",
  call = rlang::caller_env()
) {
  cli_assert(
    condition = is_module(module),
    message = c(
      "The {.arg module} argument must be a {.cls hitop_module} object.",
      i = "Build one with {.code hitop_module()}."
    ),
    call = call
  )
  # Only reachable from a hand-assembled object: hitop_module() refuses to
  # build a module for any instrument but the ones it supports.
  cli_assert(
    condition = identical(module$instrument, instrument),
    message = c(
      "The {.arg module} argument describes the wrong instrument.",
      x = "Expected a {.val {instrument}} module but got {.val {module$instrument}}."
    ),
    call = call
  )

  # `nItems` and `items` are independent fields of a plain list, so a descriptor
  # assembled or edited by hand can disagree with itself. Taking the item count
  # from `nItems` alone would then pass validate_items() against the wrong
  # width and score whichever columns happened to be supplied: an inflated
  # nItems accepts a full 405-column frame and silently scores items 1..n as
  # the module's scales. The count is therefore derived from `items`, which is
  # what the remap below actually indexes into, and the disagreement is an error.
  cli_assert(
    condition = identical(as.integer(module$nItems), length(module$items)),
    message = c(
      "The {.arg module} argument is internally inconsistent.",
      x = "It reports {module$nItems} item{?s} but carries {length(module$items)}.",
      i = "Build one with {.code hitop_module()} rather than by hand."
    ),
    call = call
  )

  kept <- scales[scales[[scale_col]] %in% module$camelCase, , drop = FALSE]
  reverse_numbers <- items[[item_col]][items[[reverse_col]]]
  numbers <- kept[[number_col]]
  names(numbers) <- kept[[scale_col]]

  list(
    n_items = length(module$items),
    reverse_items = which(module$items %in% reverse_numbers),
    items_scales = lapply(numbers, function(x) match(x, module$items))
  )
}

# Internal Helper: the three engine inputs for the HiTOP-SR, full or module
#
# score_hitopsr() and reliability_hitopsr() resolve the same three values the
# same way, so they share this. `module = NULL` is the full instrument, where an
# item's number is already its position among the 405 supplied columns. `call`
# reaches the exported wrapper one frame up, so module_engine_inputs()'s aborts
# blame score_hitopsr()/reliability_hitopsr() rather than this helper.
hitopsr_engine_inputs <- function(module, call = rlang::caller_env()) {
  if (is.null(module)) {
    return(list(
      n_items = 405,
      reverse_items =
        hitopsr_items[hitopsr_items$Reverse == TRUE, "HSR", drop = TRUE],
      items_scales = hitopsr_scales$itemNumbers
    ))
  }

  module_engine_inputs(
    module = module,
    instrument = "hitopsr",
    items = hitopsr_items,
    scales = hitopsr_scales,
    item_col = "HSR",
    call = call
  )
}

# Internal Helper: is this object a module descriptor?
#
# Accepts the deprecated `hitop_subset` class alongside `hitop_module`, so a
# descriptor built before the rename still reaches every consumer. The two
# classes carry identical fields; only the class attribute differs.
is_module <- function(x) {
  inherits(x, c("hitop_module", "hitop_subset"))
}
