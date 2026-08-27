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
  tibble::tibble(
    Scale = ref$Scale,
    camelCase = ref$camelCase,
    nItems = ref$nItems,
    Brief = scale_definitions(instrument, ref$camelCase)
  )
}

# The definitions are matched on the camelCase stem, never on the printed
# display name: the two tables disagree on one HiTOP-SR label, so a name join
# would drop that scale, and a relabelling in either table would drop another.
# Abort rather than return a column with a hole in it -- a silently missing
# definition reaches a caller as a blank where a definition should be, and an
# instrument added without a definitions table would return a column of NA.
scale_definitions <- function(instrument, stems) {
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
        "i" = "Rebuild the definitions table from {.file data-raw/hitopsr_info.R}."
      ),
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
