#' Deprecated: Describe a Subset of an Instrument's Scales
#'
#' @description `hitop_subset()` was renamed to [hitop_module()] in hitop 0.2.0,
#'   when a chosen set of an instrument's scales became a *module* throughout
#'   this package. It is kept so existing scripts keep running: it warns, then
#'   returns the same descriptor `hitop_module()` returns, carrying the legacy
#'   `hitop_subset` class. Every function that accepts a module also accepts
#'   that legacy object.
#'
#'   The `subset` argument of [score_hitopsr()], [reliability_hitopsr()],
#'   [generate_docx_hitopsr()], [generate_qualtrics_hitopsr()], and
#'   [generate_redcap_hitopsr()] was renamed to `module` at the same time, and
#'   is deprecated in the same way.
#'
#' @inheritParams hitop_module
#'
#' @return An object of class `hitop_subset`, identical to what
#'   [hitop_module()] returns apart from its class attribute.
#'
#' @seealso [hitop_module()], which replaces this.
#'
#' @examples
#' # Deprecated; use hitop_module() instead
#' m <- suppressWarnings(
#'   hitop_subset("hitopsr", scales = c("Agoraphobia", "Appetite Loss"))
#' )
#' m
#'
#' @keywords internal
#' @export
hitop_subset <- function(instrument = "hitopsr", scales) {
  deprecate_subset("hitop_subset()", "hitop_module()")
  out <- hitop_module(instrument = instrument, scales = scales)
  # The legacy class, not an added one: an object built here differs from a
  # hitop_module only in its class attribute, and is_module() accepts both.
  class(out) <- "hitop_subset"
  out
}

#' @export
print.hitop_subset <- function(x, ...) {
  # No warning here. Printing an object is not a use of the deprecated API —
  # the call that built it already warned, and warning again on every display
  # would punish the user for looking at what they were handed.
  print.hitop_module(x, ...)
}

# Internal Helper: resolve the deprecated `subset` argument against `module`
#
# Every exported function that took `subset` keeps accepting it for now. The
# two arguments name one thing, so supplying both is an error rather than a
# silent precedence rule: there is no reading of `module = a, subset = b` that
# is obviously right, and guessing would score the wrong scales.
resolve_module_arg <- function(module, subset, call = rlang::caller_env()) {
  if (is.null(subset)) {
    return(module)
  }
  # `cli::cli_abort()` directly rather than cli_assert(), which takes no class:
  # this branch is asserted by class in the tests, and widening the shared
  # validator's signature for one call site is not worth it.
  if (!is.null(module)) {
    cli::cli_abort(
      c(
        "Supply either {.arg module} or {.arg subset}, not both.",
        i = "{.arg subset} is the deprecated name for {.arg module}.",
        i = "Drop the {.arg subset} argument and keep {.arg module}."
      ),
      class = "hitop_both_module_args",
      call = call
    )
  }
  deprecate_subset("The `subset` argument", "the `module` argument", call = call)
  subset
}
