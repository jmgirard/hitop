#' Describe a Subset of an Instrument's Scales
#'
#' @description Builds a validated description of a subset of an instrument's
#'   scales, for use with the `generate_*` family. Supplying the result as the
#'   `subset` argument of a generator produces a shortened instrument
#'   containing only the items belonging to the chosen scales, **keeping each
#'   item's original number** so that data collected with the shortened form
#'   can still be scored against the full instrument's key.
#'
#' @param instrument A string naming the instrument to subset. Currently only
#'   `"hitopsr"` is supported. (default = `"hitopsr"`)
#' @param scales A character vector of scale names to keep. Names may be given
#'   either as they are printed on the instrument (`"Antisocial Behavior"`) or
#'   as the camelCase stems used in scored output (`"antisocialBehavior"`), in
#'   any mixture and ignoring case. Duplicates are dropped.
#'
#' @return An object of class `hitop_subset`: a list with the resolved
#'   `instrument`, the canonical display `scales` and their `camelCase` stems,
#'   the `items` kept (original instrument numbering, ascending), the parallel
#'   `reverse` keying flags, and `nItems`.
#'
#' @seealso [generate_docx_hitopsr()], [generate_qualtrics_hitopsr()], and
#'   [generate_redcap_hitopsr()], each of which takes a `subset` argument.
#'
#' @examples
#' # Describe a two-scale subset of the HiTOP-SR
#' s <- hitop_subset("hitopsr", scales = c("Agoraphobia", "Appetite Loss"))
#' s
#'
#' # The item numbers are the original HiTOP-SR numbers, not 1..8
#' s$items
#'
#' @export
hitop_subset <- function(instrument = "hitopsr", scales) {
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
      "Scale subsets are not yet supported for {.val {instrument}}.",
      i = "Only {.val {supported}} can be subset at present."
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
    class = "hitop_subset"
  )
}

#' @export
print.hitop_subset <- function(x, ...) {
  # `cat()` rather than {cli}: cli writes to the message connection, and a
  # print method must write to stdout.
  n_scales <- length(x$scales)
  cat(
    cli::pluralize(
      "<hitop_subset> {x$instrument}: {x$nItems} item{?s} from ",
      "{n_scales} scale{?s}"
    ),
    "\n",
    sep = ""
  )
  cat(paste0("* ", x$scales, "\n"), sep = "")
  invisible(x)
}

# Internal Helper: reduce an items table and a scales table to a subset
#
# `subset` may be NULL (returns both tables untouched, so callers need no
# branch). Item numbering is never rewritten: the reduced tables carry the
# instrument's original numbers, with gaps where items were dropped.
apply_subset <- function(
  items,
  scales,
  subset,
  item_col,
  scale_col = "camelCase",
  call = rlang::caller_env()
) {
  if (is.null(subset)) {
    return(list(items = items, scales = scales))
  }

  cli_assert(
    condition = inherits(subset, "hitop_subset"),
    message = c(
      "The {.arg subset} argument must be a {.cls hitop_subset} object.",
      i = "Build one with {.code hitop_subset()}."
    ),
    call = call
  )

  list(
    items = items[items[[item_col]] %in% subset$items, , drop = FALSE],
    scales = if (is.null(scales)) {
      NULL
    } else {
      scales[scales[[scale_col]] %in% subset$camelCase, , drop = FALSE]
    }
  )
}

# Internal Helper: remap a subset descriptor into the engines' three inputs
#
# `apply_subset()` above reduces the instrument's TABLES for the generators,
# which keep the original item numbering. The engines instead address items by
# POSITION within the columns the caller supplied, so scoring subset-collected
# data needs the subset's original numbers translated into positions within
# `subset$items` (which is ascending). Returns `n_items`, `reverse_items`, and
# `items_scales` ready for score_engine()/reliability_engine().
#
# `items` and `scales` are the instrument's own tables, so the reverse key is
# read from the package's canonical source rather than trusted from the
# descriptor's parallel `reverse` flags.
#
# Invariant: every kept scale's items are fully contained in `subset$items`,
# because hitop_subset() builds `items` as the union of exactly the scales it
# keeps. The match() below therefore never yields NA for a kept scale.
subset_engine_inputs <- function(
  subset,
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
    condition = inherits(subset, "hitop_subset"),
    message = c(
      "The {.arg subset} argument must be a {.cls hitop_subset} object.",
      i = "Build one with {.code hitop_subset()}."
    ),
    call = call
  )
  # Only reachable from a hand-assembled object: hitop_subset() refuses to
  # build a subset for any instrument but the ones it supports.
  cli_assert(
    condition = identical(subset$instrument, instrument),
    message = c(
      "The {.arg subset} argument describes the wrong instrument.",
      x = "Expected a {.val {instrument}} subset but got {.val {subset$instrument}}."
    ),
    call = call
  )

  kept <- scales[scales[[scale_col]] %in% subset$camelCase, , drop = FALSE]
  reverse_numbers <- items[[item_col]][items[[reverse_col]]]
  numbers <- kept[[number_col]]
  names(numbers) <- kept[[scale_col]]

  list(
    n_items = subset$nItems,
    reverse_items = which(subset$items %in% reverse_numbers),
    items_scales = lapply(numbers, function(x) match(x, subset$items))
  )
}
