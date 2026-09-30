#' Describe a Module of an Instrument's Scales
#'
#' @description Builds a validated description of a **module**: a chosen set of
#'   an instrument's scales, administered and scored on its own. Supplying the
#'   result as the `module` argument of a generator produces an instrument
#'   containing only the items belonging to those scales. The `items` field
#'   below always holds the **original** instrument numbers, whatever a
#'   generator prints: the online exports keep those numbers, while
#'   [generate_docx_hitopsr()] numbers a Word form `1` to `n` down the page
#'   unless asked not to. Supplying the module again to [score_hitopsr()] or
#'   [reliability_hitopsr()] scores the collected columns either way.
#'
#'   A module naming every scale holds exactly the instrument's own items --
#'   all 405 of them for the HiTOP-SR -- but it is still a module, and
#'   [generate_docx_hitopsr()] frames it as one: the form is headed
#'   `"HiTOP-SR Module (v1.0)"`, and with `randomize = TRUE` it also carries a
#'   405-row crosswalk. Supply no `module` at all to get the full instrument's
#'   framing. The Qualtrics and REDCap exports are the same either way.
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
#'   the integer `items` kept (original instrument numbering, ascending), the
#'   parallel `reverse` keying flags, and `nItems`.
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
#' # `$items` holds the original HiTOP-SR numbers, not 1..8 -- this is the
#' # descriptor, not what any particular generator prints
#' m$items
#'
#' # Select the collected item columns by NAME, never by position: `m$items`
#' # holds item numbers, which are column positions only in a frame that is
#' # exactly the 405 items in order. `ku_hitopsr` leads with `participant` and
#' # `biosex`, so `ku_hitopsr[m$items]` would quietly return the wrong columns.
#' collected <- ku_hitopsr[sprintf("hsr_%03d", m$items)]
#' ncol(collected) == m$nItems
#'
#' @param call Internal. The environment blamed by any error this raises. A
#'   default argument is evaluated in this function's own frame, so a direct
#'   call blames `hitop_module()`; the deprecated [hitop_subset()] passes its
#'   own frame instead, so a bad argument there names the function the user
#'   actually wrote. (default = this function's frame)
#'
#' @export
hitop_module <- function(instrument = "hitopsr", scales,
                         call = rlang::current_env()) {
  instrument <- validate_module_instrument(instrument, call = call)

  cli_assert(
    condition = length(scales) > 0L,
    message = c(
      "The {.arg scales} argument must name at least one scale.",
      i = "See {.code hitopsr_scales$camelCase} for the available names."
    ),
    call = call
  )
  cli_assert(
    condition = is.character(scales),
    message = "The {.arg scales} argument must be a character vector.",
    call = call
  )
  cli_assert(
    condition = !anyNA(scales),
    message = "The {.arg scales} argument must not contain missing values.",
    call = call
  )

  ref <- module_scale_tables()[[instrument]]
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
    ), call = call)
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
# branch). This helper itself rewrites no numbering: the reduced tables carry
# the instrument's original numbers, with gaps where items were dropped. What
# a generator PRINTS is its own decision downstream of here --
# generate_docx_hitopsr() renumbers the page 1..n by default, the online
# exports never do.
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
  # Before any generator opens a path, so a refused module writes no file.
  check_module_build(module, call = call)

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
# whose reduced tables keep the original item numbering. The engines instead
# address items by
# POSITION within the columns the caller supplied, so scoring module-collected
# data needs the module's original numbers translated into positions within
# `module$items` (which is ascending). Returns `n_items`, `reverse_items`,
# `items_scales`, `scale_names` and `scale_stems` ready for
# score_engine()/reliability_engine().
#
# `items` and `scales` are the instrument's own tables, so the reverse key is
# read from the package's canonical source rather than trusted from the
# descriptor's parallel `reverse` flags.
#
# Invariant: every kept scale's items are fully contained in `module$items`,
# because check_module_build() refuses any module whose `items` are not the
# union hitop_module() builds from its scales. The match() below therefore
# never yields NA for a kept scale.
module_engine_inputs <- function(
  module,
  instrument,
  items,
  scales,
  item_col,
  reverse_col = "Reverse",
  scale_col = "camelCase",
  display_col = "Scale",
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
  # The fields of a plain list can be edited by hand. A module lacking an item
  # of a kept scale would score that scale from the rest, and an inflated
  # `nItems` would accept a full 405-column frame and score items 1..n as the
  # module's scales. Both stop here, before any remap (D-081). It runs before
  # the instrument check below, so an edited `instrument` that hitop_module()
  # cannot build is refused under the same class as every other edit.
  check_module_build(module, call = call)

  # A module that builds for another supported instrument. check_module_build()
  # has made `instrument` equal the build's, which is "hitopsr" while
  # hitop_module() supports the HiTOP-SR only, so this cannot fail today; it
  # guards the day a second instrument's module could be passed here.
  cli_assert(
    condition = identical(module$instrument, instrument),
    message = c(
      "The {.arg module} argument describes the wrong instrument.",
      x = "Expected a {.val {instrument}} module but got {.val {module$instrument}}."
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
    items_scales = lapply(numbers, function(x) match(x, module$items)),
    # Read from the instrument's own table, not from `module$scales`: the module
    # object carries the same names, but taking them from there would make the
    # reliability call and the module that produced it one source rather than
    # two, and nothing downstream could then tell them apart (M061-D1).
    scale_names = kept[[display_col]],
    scale_stems = kept[[scale_col]]
  )
}

# Internal Helper: the three engine inputs for the HiTOP-SR, full or module
#
# score_hitopsr() and reliability_hitopsr() resolve the same three values the
# same way, so they share this. `module = NULL` is the full instrument, where an
# item's number is already its position among the 405 supplied columns. `call`
# reaches the exported wrapper one frame up, so module_engine_inputs()'s aborts
# blame score_hitopsr()/reliability_hitopsr() rather than this helper.
#
# With `include_subscales = TRUE`, the rows of hitopsr_subscales follow the
# scales in every per-scale element, in that table's row order. Under a module,
# only the subscales whose parent scale the module holds are added.
hitopsr_engine_inputs <- function(module, include_subscales = FALSE,
                                  call = rlang::caller_env()) {
  if (is.null(module)) {
    inputs <- list(
      n_items = 405,
      reverse_items =
        hitopsr_items[hitopsr_items$Reverse == TRUE, "HSR", drop = TRUE],
      items_scales = hitopsr_scales$itemNumbers,
      scale_names = hitopsr_scales$Scale,
      scale_stems = hitopsr_scales$camelCase
    )
  } else {
    inputs <- module_engine_inputs(
      module = module,
      instrument = "hitopsr",
      items = hitopsr_items,
      scales = hitopsr_scales,
      item_col = "HSR",
      call = call
    )
  }

  if (include_subscales) {
    inputs <- add_hitopsr_subscales(inputs, module, call = call)
  }
  inputs
}

# Internal Helper: the rows of the HiTOP-SR subscale table a module holds
#
# All rows when `module` is NULL; under a module, the rows whose parent scale
# the module holds, in table order. Shared by the scoring functions and the
# Word form so the two cannot disagree on which subscales a module carries.
# `subs` is an argument only so that tests can pass a faulty table.
module_subscales <- function(module, subs = hitopsr_subscales,
                             call = rlang::caller_env()) {
  if (is.null(module)) {
    return(subs)
  }
  parent_stem <- hitopsr_scales$camelCase[match(subs$Scale, hitopsr_scales$Scale)]
  # A parent name missing from hitopsr_scales would drop its subscales from
  # every module silently, so it stops here instead.
  if (anyNA(parent_stem)) {
    cli::cli_abort(
      "Internal error: a HiTOP-SR subscale's parent scale name is not in {.code hitopsr_scales}.",
      .internal = TRUE,
      call = call
    )
  }
  subs <- subs[parent_stem %in% module$camelCase, , drop = FALSE]
  # Every subscale item lies in its parent scale, and check_module_build() has
  # already refused a module lacking any item of its scales, so a kept
  # subscale's items are all among the module's. In the package, only a keying
  # change that broke the first fact reaches this abort (tests reach it through
  # a faulty `subs`). Unchecked, scoring would silently score the subscale from
  # its remaining items (or as NA under `missing = "complete"`), and the Word
  # form would print NA in its row, so it stops here instead. Scoring reads
  # `itemNumbers` and the Word form reads `itemdata`, so both are checked.
  sub_items <- c(
    unlist(subs$itemNumbers),
    unlist(lapply(subs$itemdata, function(d) d$HSR))
  )
  if (!all(sub_items %in% module$items)) {
    cli::cli_abort(
      "Internal error: a HiTOP-SR subscale has an item outside its parent scale.",
      .internal = TRUE,
      call = call
    )
  }
  subs
}

# Internal Helper: append the HiTOP-SR subscales to the engine inputs
#
# A subscale's parent is named by its display name in `hitopsr_subscales$Scale`
# and a module holds scale stems, so the parent is matched through
# hitopsr_scales. Under a module, item numbers become positions among the
# module's columns, as module_engine_inputs() does for the scales; every item
# is found there, because module_subscales() stops otherwise. `subs` is an
# argument only so that tests can pass a faulty table.
add_hitopsr_subscales <- function(inputs, module, subs = hitopsr_subscales,
                                  call = rlang::caller_env()) {
  subs <- module_subscales(module, subs, call = call)
  numbers <- subs$itemNumbers
  if (!is.null(module)) {
    numbers <- lapply(subs$itemNumbers, function(x) match(x, module$items))
  }
  names(numbers) <- subs$camelCase

  inputs$items_scales <- c(inputs$items_scales, numbers)
  inputs$scale_names <- c(inputs$scale_names, subs$Subscale)
  inputs$scale_stems <- c(inputs$scale_stems, subs$camelCase)
  inputs
}

# Internal Helper: the caller's `items` put into instrument order for `layout`
#
# Under `layout = "printed"` the supplied columns are in the order a shuffled
# form printed its items: column k holds the answer to printed item k, and
# `items` names the columns in that order. `attr(module, "item_order")` records
# the original HiTOP-SR item numbers in printed order, so instrument item
# `module$items[j]` sits at printed position `match(module$items[j],
# item_order)`. The return value is `items` reindexed so that position j names
# the column holding instrument item j, which is the order the engines expect.
# Under `layout = "instrument"` the vector is returned untouched.
#
# The caller's own vector is validated and passed through warn_item_order()
# here, before the permute: the permuted vector is non-ascending by
# construction, so the engine skips the heuristic on it (`check_order = FALSE`
# in prep_items()). Every refusal blames the exported wrapper via `call`.
layout_items <- function(items, module, layout, call = rlang::caller_env()) {
  if (identical(layout, "instrument")) {
    return(items)
  }
  hint <- c(
    "i" = "A printed order comes from a module descriptor written by {.fn generate_docx_hitopsr} with {.code randomize = TRUE} and read back by {.fn read_module}.",
    "i" = "For columns already in instrument order, use {.code layout = \"instrument\"}."
  )
  cli_assert(
    condition = !is.null(module),
    message = c(
      "The {.arg layout} argument is {.val printed} but no {.arg module} was supplied.",
      hint
    ),
    call = call
  )
  item_order <- attr(module, "item_order")
  cli_assert(
    condition = !is.null(item_order),
    message = c(
      "The {.arg layout} argument is {.val printed} but the {.arg module} has no {.field item_order} attribute.",
      hint
    ),
    call = call
  )
  cli_assert(
    condition = is_item_permutation(item_order, module$items),
    message = c(
      "The {.arg layout} argument is {.val printed} but the {.arg module}'s {.field item_order} is not a permutation of its items.",
      hint
    ),
    call = call
  )

  validate_items(items, n = length(module$items), call = call)
  validate_item_uniqueness(items, call = call)
  item_order <- as.integer(item_order)
  warn_item_order(items, call = call, layout = "printed",
                  item_order = item_order)
  items[match(module$items, item_order)]
}

# Internal Helper: is `item_order` a permutation of a module's items?
#
# The one test layout_items() and write_module() apply to an `item_order`
# attribute: numeric, complete, finite, each entry exactly whole, one entry per
# item, and the same multiset as the items -- which a repeated number fails,
# since sorting then disagrees at some position. The finite and whole tests
# run before any comparison, so a fraction is refused rather than truncated
# and `Inf` never reaches an integer coercion. `items` may be double: a module
# saved to `.rds` before item numbers were integers still carries its order.
is_item_permutation <- function(item_order, items) {
  is.numeric(item_order) &&
    !anyNA(item_order) &&
    all(is.finite(item_order)) &&
    all(item_order == trunc(item_order)) &&
    length(item_order) == length(items) &&
    all(sort(item_order) == sort(items))
}

# Internal Helper: the item columns to score when `items` is missing or NULL
#
# A module read from a descriptor with a `columns` field carries a `columns`
# attribute: the names the export gives the module's items, in instrument
# order. A missing or
# NULL `items` takes those names. A supplied `items` is returned untouched, so
# it always wins. The names are in instrument order, so a call under
# `layout = "printed"` still needs its own `items`. Names taken from the module
# that are not columns of `data` are refused here, so the error blames the
# module's `columns` rather than an `items` the caller never passed.
module_column_items <- function(items_missing, items, module, layout, data,
                                call = rlang::caller_env()) {
  if (!items_missing && !is.null(items)) {
    return(items)
  }
  headline <- if (items_missing) {
    "The {.arg items} argument is missing."
  } else {
    "The {.arg items} argument is {.code NULL}."
  }
  columns <- if (is.null(module)) NULL else attr(module, "columns")
  why <- if (is.null(module)) {
    "No {.arg module} was supplied, so there are no column names to take."
  } else if (is.null(columns)) {
    "The {.arg module} has no {.field columns} attribute to take the names \\
     from."
  } else if (identical(layout, "printed")) {
    "The {.arg module}'s {.field columns} are in instrument order, and \\
     {.code layout = \"printed\"} needs the columns in printed order."
  }
  cli_assert(
    condition = is.null(why),
    message = c(
      headline,
      x = why,
      i = "Pass {.arg items}: the names or positions of the item columns."
    ),
    call = call
  )
  # Data that is not a data frame is left to the engine's own refusal.
  absent <- if (is.data.frame(data)) setdiff(columns, names(data))
  cli_assert(
    condition = length(absent) == 0L,
    message = c(
      "The {.arg module}'s {.field columns} are not all columns in \\
       {.arg data}.",
      x = "Not found in {.arg data}: {.val {absent}}.",
      i = "The descriptor may come from another export. Pass {.arg items}: \\
           the names or positions of the item columns."
    ),
    call = call
  )
  columns
}

# Internal Helper: is this object a module descriptor?
#
# Accepts the deprecated `hitop_subset` class alongside `hitop_module`, so a
# descriptor built before the rename still reaches every consumer. The two
# classes carry identical fields; only the class attribute differs.
is_module <- function(x) {
  inherits(x, c("hitop_module", "hitop_subset"))
}

# Internal Helper: refuse a module that is not the build of its own scales
#
# A module is a plain list, so its fields can be edited by hand. Every function
# that takes one rebuilds it with hitop_module() from its `instrument` and
# `scales` and compares the fields the consumers read: `items` (by value and in
# order), `nItems` and `camelCase` (D-081), and `instrument` itself (M142-D1). By value, so a module saved before
# item numbers were integers, whose `items` are doubles, still passes. Every
# refusal carries the public class `hitop_module_mismatch`. Returns the rebuild,
# which write_module() writes.
check_module_build <- function(module, call = rlang::caller_env()) {
  # An object of the class that is not a list is refused before any field is
  # read: `$` on it is a base error or, on an environment, a read of whatever
  # it holds.
  if (!is.list(module)) {
    cli::cli_abort(
      c(
        "The {.arg module} argument is {.obj_type_friendly {module}} of type \\
         {.cls {typeof(module)}}, not a list of the fields \\
         {.field instrument}, {.field scales}, {.field items}, \\
         {.field nItems} and {.field camelCase}.",
        i = "Build the module with {.code hitop_module()}."
      ),
      class = "hitop_module_mismatch",
      call = call
    )
  }
  rebuilt <- rlang::try_fetch(
    hitop_module(instrument = module$instrument, scales = module$scales),
    error = function(cnd) {
      unknown <- cli::cli_vec(
        module_unknown_scales(module),
        style = list("vec-trunc" = Inf)
      )
      cli::cli_abort(
        c(
          "Cannot rebuild the {.arg module} argument from its \\
           {.field instrument} and {.field scales}.",
          if (length(unknown) > 0L) {
            c(x = "Its scales field names {cli::qty(length(unknown))}{?an/} unknown \\
                   scale{?s}: {.val {unknown}}.")
          },
          i = "Build the module with {.code hitop_module()}."
        ),
        parent = cnd,
        class = "hitop_module_mismatch",
        call = call
      )
    }
  )

  faults <- character()
  items <- module$items
  numeric_items <- is.numeric(items)
  items_ok <- numeric_items &&
    length(items) == length(rebuilt$items) &&
    !anyNA(items) &&
    all(items == rebuilt$items)
  if (!items_ok) {
    faults <- c(faults, x = "Its items field is not the {rebuilt$nItems} item \\
      number{?s} its scales cover, in ascending order.")
    if (!numeric_items) {
      faults <- c(faults, x = "Its items field is not a vector of numbers.")
    } else {
      present <- items[!is.na(items)]
      lacking <- setdiff(rebuilt$items, present)
      extra <- setdiff(present, rebuilt$items)
      repeated <- unique(present[duplicated(present)])
      if (anyNA(items)) {
        faults <- c(faults, x = "Its items field holds a missing value.")
      }
      if (length(lacking) > 0L) {
        lacking_shown <- item_ranges(lacking)
        faults <- c(faults, x = "Its items field lacks \\
          {cli::qty(length(lacking))}item{?s} {lacking_shown}, which its \\
          scales cover.")
      }
      if (length(extra) > 0L) {
        extra_shown <- item_ranges(extra)
        faults <- c(faults, x = "Its items field holds \\
          {cli::qty(length(extra))}item{?s} {extra_shown} outside its \\
          scales.")
      }
      if (length(repeated) > 0L) {
        repeated_shown <- item_ranges(repeated)
        faults <- c(faults, x = "Its items field holds \\
          {cli::qty(length(repeated))}item{?s} {repeated_shown} more than \\
          once.")
      }
    }
  }

  n_items <- module$nItems
  n_ok <- is.numeric(n_items) &&
    length(n_items) == 1L &&
    !is.na(n_items) &&
    n_items == rebuilt$nItems
  if (!n_ok) {
    faults <- c(faults, x = "Its nItems field is not {rebuilt$nItems}, the \\
      number of items its scales cover.")
  }

  # hitop_module() reads `instrument` in any letter case, so a rebuild can
  # succeed for a field that still differs from the one the build writes.
  if (!identical(module$instrument, rebuilt$instrument)) {
    faults <- c(faults, x = "Its instrument field is not \\
      {.val {rebuilt$instrument}}.")
  }

  # Compared exactly: the consumers choose scales by these names.
  if (!identical(module$camelCase, rebuilt$camelCase)) {
    faults <- c(faults, x = "Its camelCase field is not \\
      {.val {rebuilt$camelCase}}, the names of its scales.")
  }

  if (length(faults) > 0L) {
    cli::cli_abort(
      c(
        "The {.arg module} argument does not match its {.field scales}.",
        faults,
        i = "Build the module with {.code hitop_module()}."
      ),
      class = "hitop_module_mismatch",
      call = call
    )
  }
  rebuilt
}

# Internal Helper: item numbers as a cli list that names every one
#
# Sorts the numbers and prints a run of three or more consecutive whole numbers
# as "a-b"; a pair stays two numbers, and a number that is not whole is never
# part of a run. The list is never cut, where cli by default shows only 20
# elements of a longer list and elides the middle, because a refusal must name
# each item it blames.
item_ranges <- function(x) {
  x <- sort(unique(x))
  whole <- x == round(x)
  run <- cumsum(c(TRUE, diff(x) != 1 | !whole[-1L] | !whole[-length(x)]))
  shown <- unlist(lapply(split(x, run), function(r) {
    # 15 significant digits, not the default 7, so 7 + 1e-9 prints as
    # "7.000000001", not "7". An offset past the 15th digit is still lost:
    # 66 + 1e-14 prints as "66".
    r <- format(r, digits = 15, trim = TRUE, scientific = FALSE,
                drop0trailing = TRUE)
    if (length(r) >= 3L) paste0(r[[1L]], "-", r[[length(r)]]) else r
  }), use.names = FALSE)
  cli::cli_vec(shown, style = list("vec-trunc" = Inf))
}

# Internal Helper: the names in a module's `scales` its instrument lacks
#
# Only for the refusal message above; empty when the instrument itself is not
# one the module API supports, since then no scale table can be consulted.
module_unknown_scales <- function(module) {
  tables <- module_scale_tables()
  instrument <- module$instrument
  scales <- module$scales
  if (!is.character(instrument) || length(instrument) != 1L ||
      !is.character(scales)) {
    return(character())
  }
  # hitop_module() reads the instrument name in any letter case.
  instrument <- tolower(instrument)
  if (!instrument %in% names(tables)) {
    return(character())
  }
  ref <- tables[[instrument]]
  known <- c(tolower(ref$Scale), tolower(ref$camelCase))
  unique(scales[!is.na(scales) & !tolower(scales) %in% known])
}

# Internal Helper: the scale table backing each instrument the module API supports
#
# The single source of the supported set. validate_module_instrument() derives
# `supported` from these names and both hitop_module() and available_scales()
# read their table through here, so an instrument cannot be declared supported
# without a table — the failure mode being avoided is a new entry in a
# hand-maintained `supported` vector silently yielding HiTOP-SR scales.
module_scale_tables <- function() {
  list(hitopsr = hitopsr_scales)
}
