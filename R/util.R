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

# The three validators below are shared across argument names: score_pid5() and
# friends pass a column mapping called `items`, rank_scales() one called
# `scales`, and norm_pid5() one called `scores`. Each takes an `arg` naming the
# caller's argument so the abort blames the argument the user actually wrote;
# the defaults reproduce the wording these helpers carried before `arg` existed.

# Abort when `items` refers to columns not present in `data`, before the base-R
# `data[items]` extraction (whose own subscript error is cryptic). Character
# entries must be names in `data`; integer entries must be valid column
# positions (1..ncol). Assumes validate_items() already checked type/length.
validate_items_present <- function(
  data,
  items,
  arg = "items",
  call = rlang::caller_env()
) {
  if (is.character(items)) {
    missing <- setdiff(items, names(data))
    if (length(missing) > 0) {
      cli::cli_abort(
        c(
          "The {.arg {arg}} names must all be columns in {.arg data}.",
          "x" = "Not found in {.arg data}: {.val {missing}}."
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
          "The {.arg {arg}} positions must be valid columns of {.arg data}.",
          "x" = "{.arg data} has {ncol(data)} column{?s}; out of range: {.val {bad}}."
        ),
        call = call
      )
    }
  }
  invisible(NULL)
}

validate_scales <- function(x, arg = "scales", call = rlang::caller_env()) {
  cli_assert(
    condition = rlang::is_character(x) || rlang::is_integerish(x),
    message = c(
      "The {.arg {arg}} argument did not have the expected type.",
      "x" = "You supplied {.cls {class(x)}}."
    ),
    call = call
  )
}

validate_item_uniqueness <- function(
  x,
  arg = "items",
  unit = "item",
  call = rlang::caller_env()
) {
  dups <- unique(x[duplicated(x)])
  if (length(dups) > 0) {
    cli::cli_abort(
      c(
        "The {.arg {arg}} argument must map each {unit} to a distinct column.",
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
warn_item_order <- function(x, call = rlang::caller_env()) {
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
    ), call = call)
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

# The three validators below cover the scalar arguments the exported surface
# once checked with a bare base-R predicate assertion -- `prefix`, `name`,
# `append`, `calc_se`, `alpha`, `omega`, `top`. Such an assertion reports the
# predicate that failed (`rlang::is_string(prefix) is not TRUE`), names no
# function, and says nothing about what was actually supplied; these follow the
# `arg`/`call` convention of the validators above instead, so the abort blames
# the exported function the user called and the argument they wrote.

# `allow_null` exists for exactly one caller: rank_scales()'s `prefix`, where
# NULL means "strip nothing" rather than a missing value.
validate_string <- function(
  x,
  arg,
  allow_null = FALSE,
  call = rlang::caller_env()
) {
  if (allow_null && is.null(x)) {
    return(invisible(NULL))
  }
  what <- if (allow_null) "a single string or NULL" else "a single string"
  cli_assert(
    condition = rlang::is_string(x),
    message = c(
      "The {.arg {arg}} argument must be {what}.",
      "x" = "You supplied {.cls {class(x)}} of length {length(x)}."
    ),
    call = call
  )
  invisible(NULL)
}

validate_flag <- function(x, arg, call = rlang::caller_env()) {
  cli_assert(
    condition = rlang::is_bool(x),
    message = c(
      "The {.arg {arg}} argument must be {.code TRUE} or {.code FALSE}.",
      "x" = "You supplied {.cls {class(x)}} of length {length(x)}."
    ),
    call = call
  )
  invisible(NULL)
}

# Both a type and a bounds check, reported separately so the message says which
# one failed: a `top` of 99 is a well-formed number that asks for more scales
# than were supplied, which is a different mistake from passing "3".
#
# `min`, `max` and `allow_null` all default to what rank_scales()'s `top` needed
# when this helper was written, so its calls are unchanged. The generators'
# `breaks` is the reason they exist: it documents 0 and NULL as "no pagination"
# and has no upper limit, a page size larger than the instrument simply never
# reaching a break.
validate_count <- function(
  x,
  arg,
  max = Inf,
  min = 1,
  allow_null = FALSE,
  call = rlang::caller_env()
) {
  if (allow_null && is.null(x)) {
    return(invisible(NULL))
  }
  what <- if (allow_null) {
    "a single whole number or NULL"
  } else {
    "a single whole number"
  }
  cli_assert(
    condition = rlang::is_integerish(x, n = 1) && !is.na(x),
    message = c(
      "The {.arg {arg}} argument must be {what}.",
      "x" = "You supplied {.cls {class(x)}} of length {length(x)}."
    ),
    call = call
  )
  # With no upper limit the "between" phrasing would read "between 0 and Inf",
  # so an unbounded count states its floor instead.
  limit <- if (is.finite(max)) {
    "It must be between {min} and {max}, but you supplied {x}."
  } else {
    "It must be {min} or greater, but you supplied {x}."
  }
  cli_assert(
    condition = x >= min && x <= max,
    message = c(
      "The {.arg {arg}} argument is out of range.",
      "x" = limit
    ),
    call = call
  )
  invisible(NULL)
}

# Refuse any column that is not numeric or logical. Shared by norm_pid5() and
# plot_pid5(), which reject such a column for the same reason: as.numeric()
# turns a factor into its integer codes and a character column into NA -- wrong
# answers rather than errors -- so both stop before any conversion happens. A
# logical column is left alone: as.numeric(TRUE) is 1, which is what a 0/1
# indicator already means.
#
# The detail is shared and the headline is not. Each offending column gets its
# own bullet carrying its own class ({.cls} collapses a vector of classes into a
# single union label, so they cannot share a bullet). But norm_pid5() blames its
# `scores` argument while plot_pid5() names the normed columns it plots, and one
# flattened message would cost norm_pid5() the blame it already gives -- so the
# caller supplies its headline and closing line as functions of how many columns
# offended. That count is what both pluralize on and what neither knows until
# the check has run, which is why they are functions and not strings.
validate_numeric_columns <- function(columns, headline, info,
                                     call = rlang::caller_env()) {
  bad <- !vapply(columns, function(x) is.numeric(x) || is.logical(x), logical(1))
  if (!any(bad)) {
    return(invisible(NULL))
  }
  ## Escaped because these strings are already formatted, and cli_abort() would
  ## interpolate a brace in a column name or a class a second time.
  escape_braces <- function(x) {
    gsub("}", "}}", gsub("{", "{{", x, fixed = TRUE), fixed = TRUE)
  }
  detail <- vapply(which(bad), function(i) {
    nm <- names(columns)[[i]]
    cls <- class(columns[[i]])
    escape_braces(cli::format_inline("{.val {nm}} is {.cls {cls}}."))
  }, character(1))
  names(detail) <- rep("x", length(detail))
  n <- sum(bad)
  cli::cli_abort(
    c(escape_braces(headline(n)), detail, "i" = escape_braces(info(n))),
    call = call
  )
}

# A confidence level is a single probability strictly inside (0, 1). Both ends
# are excluded rather than clamped: a level of 1 asks for an infinitely wide
# interval and a level of 0 for a point, and neither is a request the caller
# meant. Reported as two checks so the message says which one failed -- 95 is a
# well-formed number on the wrong scale, which is a different mistake from "95%".
# The type check accepts an integer, as validate_range() next door does: `1L` is
# a number on the right scale and belongs in the range message, not the type one.
validate_level <- function(x, arg = "level", call = rlang::caller_env()) {
  cli_assert(
    condition = rlang::is_bare_numeric(x, n = 1) && !is.na(x),
    message = c(
      "The {.arg {arg}} argument must be a single number.",
      "x" = "You supplied {.cls {class(x)}} of length {length(x)}."
    ),
    call = call
  )
  cli_assert(
    condition = x > 0 && x < 1,
    message = c(
      "The {.arg {arg}} argument must be a proportion between 0 and 1.",
      "x" = "You supplied {x}.",
      "i" = "A 95% interval is {.code level = 0.95}, not {.code 95}."
    ),
    call = call
  )
  invisible(NULL)
}

# Remove a leading `prefix` from each name by *literal* match (D-026). The
# obvious `sub(paste0("^", prefix), "", x)` compiles the caller's string as a
# regular expression, so a prefix containing `(` aborts with a regex error the
# caller never wrote and one containing `.` strips a prefix that was never
# there. A name that does not start with `prefix` is returned unchanged; an
# empty `prefix` is a no-op.
strip_prefix <- function(x, prefix) {
  hit <- startsWith(x, prefix)
  x[hit] <- substring(x[hit], nchar(prefix) + 1L)
  x
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

# Shared item preparation for score_engine() and reliability_engine(): validate
# the data/items/srange, extract the item columns in instrument order, coerce to
# numeric, and reverse-key the flagged positions. Returns a numeric matrix (rows
# = respondents, columns = items). `call` is forwarded to the validators so
# aborts are attributed to the exported wrapper, not this helper or the engine.
prep_items <- function(
  data,
  items,
  n_items,
  reverse_items,
  srange,
  call = rlang::caller_env()
) {
  validate_data(data, call = call)
  validate_items(items, n = n_items, call = call)
  validate_item_uniqueness(items, call = call)
  validate_items_present(data, items, call = call)
  warn_item_order(items, call = call)
  validate_range(srange, call = call)

  ## Extract item columns and coerce values to numbers
  data_items <- lapply(data[items], as.numeric)

  ## Reverse score the necessary items
  if (length(reverse_items) > 0) {
    data_items[reverse_items] <- lapply(
      reverse_items,
      function(i) {
        reverse(data_items[[i]], low = srange[[1]], high = srange[[2]])
      }
    )
  }

  bind_columns(data_items)
}

# Internal Helper: one deprecation warning carrying a stable condition class
#
# {lifecycle} is deliberately not a dependency (GP4: a single deprecation does
# not earn one), so the warning is hand-rolled. Tests assert on the class, not
# on the message text, so the class is the stable part of this contract.
deprecate_subset <- function(what, instead, call = rlang::caller_env()) {
  cli::cli_warn(
    c(
      "{.code {what}} was renamed to {.code {instead}} in hitop 0.2.0.",
      i = "The old name still works, but please use {.code {instead}}.",
      i = "A chosen set of an instrument's scales is now called a {.emph module}."
    ),
    class = "hitop_deprecated_subset",
    call = call
  )
}

# Internal Helper: validate and normalize an instrument name for the module API
#
# Shared by hitop_module() and available_scales() so the two cannot drift: the
# browser must reject exactly what the constructor rejects, with the same words,
# or a caller is told a scale set exists that no module can be built from.
# Both branches carry a condition class, because tests assert on the class and
# cli_assert()/cli_abort()'s default `rlang_error` does not discriminate.
validate_module_instrument <- function(instrument, call = rlang::caller_env()) {
  cli_assert(
    condition = is.character(instrument) && length(instrument) == 1L,
    message = "The {.arg instrument} argument must be a single string.",
    call = call
  )
  instrument <- tolower(instrument)

  supported <- names(module_scale_tables())
  planned <- c("hitopbr", "pid5", "pid5sf", "pid5bf")
  if (!instrument %in% c(supported, planned)) {
    cli::cli_abort(
      c(
        "Unknown {.arg instrument} value {.val {instrument}}.",
        i = "Currently supported: {.val {supported}}."
      ),
      class = "hitop_unknown_instrument",
      call = call
    )
  }
  if (!instrument %in% supported) {
    cli::cli_abort(
      c(
        "Scale modules are not yet supported for {.val {instrument}}.",
        i = "Only {.val {supported}} can be built into modules at present."
      ),
      class = "hitop_unsupported_instrument",
      call = call
    )
  }
  instrument
}
