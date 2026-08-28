#' List the Scales an Instrument Offers
#'
#' @description Returns the scales available for building a module, with the
#'   name to print on a form, the camelCase stem that names the scored output
#'   column, how many items the scale contributes, and the scale's brief
#'   clinician-facing definition. Either name column may be passed to
#'   [hitop_module()].
#'
#'   This is a convenience view of the instrument's own scale table, so a
#'   researcher choosing scales need not know which dataset to open.
#'
#' @param instrument A string naming the instrument. Currently only `"hitopsr"`
#'   is supported. (default = `"hitopsr"`)
#'
#' @return A tibble with one row per scale and four columns: `Scale` (the
#'   display name), `camelCase` (the scored-output stem), `nItems`, and `Brief`
#'   (the clinician-facing definition, as [hitopsr_definitions] carries it).
#'
#' @details A scale whose definition is missing from the instrument's
#'   definitions table is an error, never a blank cell: the abort carries the
#'   condition class `hitop_missing_definition`, which a caller may catch. It is
#'   not reachable from the shipped tables, which are built under a check that
#'   the two tables carry the same stems.
#'
#' @seealso [hitop_module()], which takes these names;
#'   [hitopsr_definitions], which carries the definitions in full.
#'
#' @examples
#' # Every HiTOP-SR scale, with its item count
#' available_scales("hitopsr")
#'
#' # Pick a few and build a module from them
#' hitop_module("hitopsr", scales = c("Agoraphobia", "Appetite Loss"))
#'
#' @export
available_scales <- function(instrument = "hitopsr") {
  instrument <- validate_module_instrument(instrument)

  # Read through the same map hitop_module() uses, never a hardcoded table:
  # otherwise the day another instrument becomes supported, this silently keeps
  # returning HiTOP-SR scales.
  ref <- module_scale_tables()[[instrument]]
  # Resolved before the tibble() call, not inside it: a promise forced in
  # tibble()'s frame leaves scale_definitions() with no calling function to
  # blame, and the abort would carry no call at all.
  brief <- scale_definitions(instrument, ref$camelCase)
  tibble::tibble(
    Scale = ref$Scale,
    camelCase = ref$camelCase,
    nItems = ref$nItems,
    Brief = brief
  )
}

# The definitions are matched on the camelCase stem, never on the printed
# display name: the two tables agree on every HiTOP-SR label today, but a
# relabelling in either table would drop that scale from a name join.
# Abort rather than return a column with a hole in it -- a silently missing
# definition reaches a caller as a blank where a definition should be, and an
# instrument added without a definitions table would return a column of NA.
scale_definitions <- function(instrument, stems, call = rlang::caller_env()) {
  ref <- module_definition_tables()[[instrument]]
  # Only the rows defining a scale; the other rows define subscales, which
  # available_scales() does not list.
  if (!is.null(ref)) ref <- ref[is.na(ref$Subscale), ]
  hit <- if (is.null(ref)) rep(NA_integer_, length(stems)) else match(stems, ref$camelCase)
  if (anyNA(hit)) {
    cli::cli_abort(
      c(
        "Some {.val {instrument}} scales have no definition.",
        "x" = "No definition for {.val {stems[is.na(hit)]}}.",
        "i" = "This is a fault in the installed package, not in what you supplied.",
        "i" = "Please report it at {.url https://github.com/jmgirard/hitop/issues}."
      ),
      call = call,
      class = "hitop_missing_definition"
    )
  }
  ref$Brief[hit]
}

# The sibling of module_scale_tables(): one entry per instrument that has
# definitions, so an instrument gaining a scale table without one is a missing
# key rather than a silent column of NA.
module_definition_tables <- function() {
  list(hitopsr = hitopsr_definitions)
}
