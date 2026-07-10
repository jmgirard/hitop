reverse <- function(x, low = NULL, high = NULL) {
  if (is.null(low)) {
    low <- min(x, na.rm = TRUE)
  }

  if (is.null(high)) {
    high <- max(x, na.rm = TRUE)
  }

  low + high - x
}

bind_columns <- function(vector_list) {
  do.call(cbind, vector_list)
}

adiff <- function(data, items, index) {
  abs(data[, items[[index, 2]]] - data[, items[[index, 3]]])
}

# All input-validation helpers take a `call` argument (defaulting to the calling
# frame) so the abort is attributed to the user-facing function that triggered
# it, not to the helper or the internal score_engine(). Direct callers
# (validity_pid5(), rank_scales(), calc_*()) rely on the caller_env() default;
# score_engine() passes its own `call` down so the wrapper is blamed, not the
# engine.
cli_assert <- function(condition, message, call = rlang::caller_env()) {
  if (!condition) {
    # `call` attributes the abort to the user-facing function; `.envir`
    # interpolates the message in the caller's frame (the validate_* helper),
    # where any {glue} locals it references actually live.
    cli::cli_abort(message, call = call, .envir = rlang::caller_env())
  }
}

validate_data <- function(x, call = rlang::caller_env()) {
  cli_assert(
    condition = is.data.frame(x),
    message = "The `data` argument must be a data frame.",
    call = call
  )
}

validate_items <- function(x, n, call = rlang::caller_env()) {
  cli_assert(
    condition = rlang::is_character(x) || rlang::is_integerish(x),
    message = c(
      "The `items` argument must be a character or integer vector.",
      "x" = "You supplied {.cls {class(x)}}."
    ),
    call = call
  )
  cli_assert(
    condition = length(x) == n,
    message = c(
      "The `items` argument has the wrong length.",
      "x" = "Expected {n} item{?s} but got {length(x)}."
    ),
    call = call
  )
}

# Abort when `items` refers to columns not present in `data`, before the base-R
# `data[items]` extraction (whose own subscript error is cryptic). Character
# entries must be names in `data`; integer entries must be valid column
# positions (1..ncol). Assumes validate_items() already checked type/length.
validate_items_present <- function(data, items, call = rlang::caller_env()) {
  if (is.character(items)) {
    missing <- setdiff(items, names(data))
    if (length(missing) > 0) {
      cli::cli_abort(
        c(
          "The `items` names must all be columns in `data`.",
          "x" = "Not found in `data`: {.val {missing}}."
        ),
        call = call
      )
    }
  } else {
    pos <- as.integer(items)
    bad <- pos[pos < 1 | pos > ncol(data)]
    if (length(bad) > 0) {
      cli::cli_abort(
        c(
          "The `items` positions must be valid columns of `data`.",
          "x" = "`data` has {ncol(data)} column{?s}; out of range: {.val {bad}}."
        ),
        call = call
      )
    }
  }
  invisible(NULL)
}

validate_scales <- function(x, call = rlang::caller_env()) {
  cli_assert(
    condition = rlang::is_character(x) || rlang::is_integerish(x),
    message = "The `scales` argument did not have the expected type.",
    call = call
  )
}

validate_item_uniqueness <- function(x, call = rlang::caller_env()) {
  dups <- unique(x[duplicated(x)])
  if (length(dups) > 0) {
    cli::cli_abort(
      c(
        "The `items` argument must map each item to a distinct column.",
        "x" = "Duplicated entries: {.val {dups}}."
      ),
      call = call
    )
  }
  invisible(NULL)
}

# Heuristic guard against a misordered `items` mapping, which silently scores the
# wrong items. Fires only when every entry is a character name sharing one common
# prefix followed by a trailing integer (e.g. "pid_1", "pid_2", ...) and those
# integers are not in ascending order. Integer positions are left alone (an
# out-of-order position vector can be a legitimate remap); mixed prefixes and
# names without trailing digits are ignored (no reliable order to expect).
warn_item_order <- function(x) {
  if (!is.character(x)) {
    return(invisible(NULL))
  }
  trailing <- regmatches(x, regexpr("[0-9]+$", x))
  if (length(trailing) != length(x)) {
    return(invisible(NULL))
  }
  prefixes <- sub("[0-9]+$", "", x)
  if (length(unique(prefixes)) != 1L) {
    return(invisible(NULL))
  }
  if (is.unsorted(as.integer(trailing))) {
    cli::cli_warn(c(
      "!" = "The `items` names are not in ascending numeric order.",
      "i" = "Items must be supplied in instrument order; a misordered mapping scores the wrong items. Sort them (e.g. {.code items[order(as.integer(sub(\"\\\\D+\", \"\", items)))]}) if this is unintended."
    ))
  }
  invisible(NULL)
}

validate_range <- function(x, call = rlang::caller_env()) {
  cli_assert(
    condition = rlang::is_integerish(x, n = 2),
    message = "The `srange` argument must contain two integerish values.",
    call = call
  )
  cli_assert(
    condition = x[[2]] > x[[1]],
    message = "The second `srange` value must be greater than the first.",
    call = call
  )
}

drop_na <- function(x) {
  x[!is.na(x)]
}

calc_sem <- function(x) {
  xc <- x[!is.na(x)]
  stats::sd(xc) / sqrt(length(xc))
}

# Round half away from zero, matching the APA scoring key's "round to the
# nearest whole number" (base round() rounds half to even, e.g. round(2.5) = 2).
round_half_up <- function(x) {
  sign(x) * floor(abs(x) + 0.5)
}

# APA-compliant average score for one scale's item matrix (rows = respondents,
# columns = the items contributing to that scale, already reverse-keyed). Per the
# PID-5 scoring key: if more than 25% of a scale's items are unanswered, the
# score is not used (NA); otherwise the raw score is prorated to the full item
# count and rounded to the nearest whole number before averaging. With no missing
# items this reduces to the plain item mean.
apa_mean <- function(mat) {
  n <- ncol(mat)
  apply(mat, MARGIN = 1, FUN = function(x) {
    a <- sum(!is.na(x))
    if ((n - a) / n > 0.25) {
      return(NA_real_)
    }
    partial <- sum(x, na.rm = TRUE)
    round_half_up(partial * n / a) / n
  })
}
