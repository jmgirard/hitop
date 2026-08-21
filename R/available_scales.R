#' List the Scales an Instrument Offers
#'
#' @description Returns the scales available for building a module, with the
#'   name to print on a form, the camelCase stem that names the scored output
#'   column, and how many items the scale contributes. Either name column may be
#'   passed to [hitop_module()].
#'
#'   This is a convenience view of the instrument's own scale table, so a
#'   researcher choosing scales need not know which dataset to open.
#'
#' @param instrument A string naming the instrument. Currently only `"hitopsr"`
#'   is supported. (default = `"hitopsr"`)
#'
#' @return A tibble with one row per scale and three columns: `Scale` (the
#'   display name), `camelCase` (the scored-output stem), and `nItems`.
#'
#' @seealso [hitop_module()], which takes these names.
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

  ref <- hitopsr_scales
  tibble::tibble(
    Scale = ref$Scale,
    camelCase = ref$camelCase,
    nItems = ref$nItems
  )
}
