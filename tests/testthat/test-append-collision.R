# Every export that appends its output to `data` refuses to append a column
# `data` already holds (AC1).
#
# The domain is swept from getNamespaceExports() and filtered on an `append`
# formal, so an eighth appending export added later is covered without this file
# being edited. The sweep is asserted non-empty and asserted to match the probe
# table exactly: an export the sweep names with no probe is a failure, not a
# silent omission, and a probe for something the sweep does not name means the
# table has gone stale.

append_exports <- function() {
  ns <- asNamespace("hitop")
  nms <- sort(getNamespaceExports("hitop"))
  keep <- vapply(
    nms,
    function(n) {
      obj <- get(n, envir = ns)
      is.function(obj) && "append" %in% names(formals(obj))
    },
    logical(1)
  )
  nms[keep]
}

# The columns named in a collision message, read as whole quoted tokens rather
# than by substring. Every scale column is a prefix of its own `_se` column
# (`hsr_agoraphobia` of `hsr_agoraphobia_se`), so a substring test could not tell
# "names this column" from "names the one built on its name".
named_columns <- function(msg) {
  msg <- cli::ansi_strip(msg)
  hits <- gregexpr('["“]([^"”]+)["”]', msg)
  tokens <- regmatches(msg, hits)[[1]]
  gsub('^["“]|["”]$', "", tokens)
}

# One probe per appending export: the arguments of a call that succeeds today,
# and the `data` it runs against. `extra` supplies a second set of arguments
# exercising a different *form* of output column on the exports that emit one --
# a standard error, and a validity-scale abbreviation -- so the sweep varies the
# collision's form as well as which export it lands on.
collision_probes <- function() {
  list(
    score_pid5 = list(
      data = hitop::sim_pid5,
      args = list(items = 1:220),
      extra = list(items = 1:220, calc_se = TRUE)
    ),
    score_hitopsr = list(
      data = hitop::sim_hitopsr,
      args = list(items = 1:405),
      extra = list(items = 1:405, calc_se = TRUE)
    ),
    score_hitopbr = list(
      data = hitop::sim_hitopbr,
      args = list(items = 1:45),
      extra = list(items = 1:45, calc_se = TRUE)
    ),
    validity_pid5 = list(
      data = hitop::sim_pid5,
      args = list(items = 1:220),
      extra = NULL
    ),
    norm_pid5 = list(
      data = NULL,
      args = list(),
      extra = NULL
    ),
    rank_scales = list(
      data = NULL,
      args = list(),
      extra = NULL
    ),
    interval_hitopsr = list(
      data = NULL,
      args = list(),
      extra = NULL
    )
  )
}

# The three conversion exports read a score frame, so their `data` and their
# selection argument are built here rather than shipped in the table above.
conversion_input <- function(nm) {
  if (nm == "norm_pid5") {
    scored <- suppressWarnings(
      hitop::score_pid5(hitop::sim_pid5, items = 1:220, append = FALSE)
    )
    return(list(data = scored, args = list(scores = names(scored))))
  }
  scored <- suppressWarnings(
    hitop::score_hitopsr(hitop::sim_hitopsr, items = 1:405, append = FALSE)
  )
  if (nm == "rank_scales") {
    return(list(data = scored, args = list(scales = names(scored), prefix = "hsr_")))
  }
  list(data = scored, args = list(scores = names(scored)))
}

# Resolve one probe into (fn, data, args) with the conversion inputs filled in.
resolve_probe <- function(nm, spec, which = "args") {
  args <- spec[[which]]
  if (is.null(args)) {
    return(NULL)
  }
  data <- spec$data
  if (is.null(data)) {
    built <- conversion_input(nm)
    data <- built$data
    args <- c(built$args, args)
  }
  list(fn = get(nm, envir = asNamespace("hitop")), data = data, args = args)
}

# The columns a call produces: the same call with append = FALSE.
produced_columns <- function(p) {
  out <- suppressWarnings(
    do.call(p$fn, c(list(data = p$data), p$args, list(append = FALSE)))
  )
  names(out)
}

test_that("the append-formal sweep matches the probe table exactly", {
  exports <- append_exports()
  # The domain must not be able to empty silently: with no exports enumerated,
  # every expectation in the sweep below would be vacuous and still green.
  expect_true(length(exports) > 0)
  expect_setequal(exports, names(collision_probes()))
})

test_that("every appending export refuses a single output-column collision", {
  probes <- collision_probes()
  exports <- append_exports()
  expect_true(length(exports) > 0)

  for (nm in exports) {
    p <- resolve_probe(nm, probes[[nm]])
    produced <- produced_columns(p)
    expect_true(length(produced) > 0, info = nm)

    # The control: the same call on data that holds none of its output columns
    # still succeeds, so a red result below is the guard firing and not a call
    # that was broken to begin with.
    expect_no_error(
      suppressWarnings(do.call(p$fn, c(list(data = p$data), p$args))),
      message = nm
    )

    collide <- produced[[1]]
    dirty <- p$data
    dirty[[collide]] <- NA_real_
    err <- expect_error(
      suppressWarnings(do.call(p$fn, c(list(data = dirty), p$args))),
      class = "hitop_append_collision"
    )
    named <- named_columns(conditionMessage(err))
    # Set equality against the collision, not against the produced columns: an
    # intersection with `produced` cannot see a name quoted that no call would
    # produce, and "no other" is a promise over every name the message quotes.
    expect_setequal(named, collide)
  }
})

# The exports that append exactly one column by construction. `rank_scales()`
# appends the single column named in its `name` argument, so a several-column
# collision cannot exist for it; every other swept export can exceed one column.
# The exemption is asserted rather than skipped: a `skip_if` inside the loop
# below aborts the whole `test_that`, not that iteration, so the four exports
# sorting after `rank_scales` never received a multi-column probe (M060 review,
# defect return 1). If this export ever appends more than one column, the
# assertion fails instead of the exemption going stale.
SINGLE_COLUMN_EXPORTS <- "rank_scales"

test_that("a collision message names every colliding column and no other", {
  probes <- collision_probes()
  exports <- append_exports()
  expect_true(length(exports) > 0)
  multi <- character(0)

  for (nm in exports) {
    p <- resolve_probe(nm, probes[[nm]])
    produced <- produced_columns(p)

    if (nm %in% SINGLE_COLUMN_EXPORTS) {
      expect_length(produced, 1L)
      next
    }
    expect_true(length(produced) > 1L, info = nm)
    multi <- c(multi, nm)

    # Several at once, taken from both ends of the output so the set is not a
    # contiguous run the guard could happen to report as a range.
    collide <- unique(c(
      produced[[1]],
      produced[[ceiling(length(produced) / 2)]],
      produced[[length(produced)]]
    ))
    expect_true(length(collide) > 1L, info = nm)

    dirty <- p$data
    for (cc in collide) dirty[[cc]] <- NA_real_
    err <- expect_error(
      suppressWarnings(do.call(p$fn, c(list(data = dirty), p$args))),
      class = "hitop_append_collision"
    )
    named <- named_columns(conditionMessage(err))
    expect_setequal(named, collide)
  }

  # Every export that can collide on several columns was probed with several --
  # the loop cannot have exited early or dropped an export silently.
  expect_setequal(multi, setdiff(exports, SINGLE_COLUMN_EXPORTS))
})

test_that("a collision wider than cli's inline vector limit names every column", {
  # cli collapses an inline vector at `cli.vec_trunc` (20 by default) and prints
  # an ellipsis in place of the rest. A caller cannot drop a column the message
  # does not name, so the guard has to defeat that truncation: this probe
  # collides on every column `score_pid5()` produces, which is more than 20.
  produced <- names(suppressWarnings(
    hitop::score_pid5(hitop::sim_pid5, items = 1:220, append = FALSE)
  ))
  expect_true(length(produced) > 20L)

  dirty <- hitop::sim_pid5
  for (cc in produced) dirty[[cc]] <- NA_real_
  err <- expect_error(
    suppressWarnings(hitop::score_pid5(dirty, items = 1:220)),
    class = "hitop_append_collision"
  )
  msg <- cli::ansi_strip(conditionMessage(err))
  expect_setequal(named_columns(msg), produced)
  expect_no_match(msg, "\u2026", fixed = TRUE)
})

test_that("the collision headline agrees in number with the columns it names", {
  # `cli::qty()` sets the number for the *next* pluralization marker only, and
  # an intervening substitution cancels it, so a marker placed after one reads
  # singular however many columns collided (LESSONS: M030, extended M027).
  produced <- names(suppressWarnings(
    hitop::score_hitopbr(hitop::sim_hitopbr, items = 1:45, append = FALSE)
  ))
  expect_true(length(produced) > 1L)

  headline <- function(cols) {
    dirty <- hitop::sim_hitopbr
    for (cc in cols) dirty[[cc]] <- NA_real_
    err <- expect_error(
      suppressWarnings(hitop::score_hitopbr(dirty, items = 1:45)),
      class = "hitop_append_collision"
    )
    cli::ansi_strip(conditionMessage(err))
  }

  one <- headline(produced[[1]])
  expect_match(one, "argument already holds a column", fixed = TRUE)

  many <- headline(produced)
  expect_match(many, "argument already holds columns", fixed = TRUE)
  expect_no_match(many, "argument already holds a column", fixed = TRUE)
})

test_that("a standard-error column collides in its own right", {
  probes <- collision_probes()
  for (nm in names(probes)) {
    p <- resolve_probe(nm, probes[[nm]], which = "extra")
    if (is.null(p)) next
    produced <- produced_columns(p)
    se_cols <- grep("_se$", produced, value = TRUE)
    expect_true(length(se_cols) > 0, info = nm)

    collide <- se_cols[[1]]
    dirty <- p$data
    dirty[[collide]] <- NA_real_
    err <- expect_error(
      suppressWarnings(do.call(p$fn, c(list(data = dirty), p$args))),
      class = "hitop_append_collision"
    )
    named <- named_columns(conditionMessage(err))
    # The scale column the standard error is built on did not collide and must
    # not be reported: `hsr_agoraphobia_se` collided, `hsr_agoraphobia` did not.
    expect_setequal(named, collide)
  }
})

test_that("a validity-scale abbreviation collides in its own right", {
  produced <- names(suppressWarnings(
    hitop::validity_pid5(hitop::sim_pid5, items = 1:220, append = FALSE)
  ))
  expect_true("pid_PNA" %in% produced)

  dirty <- hitop::sim_pid5
  dirty[["pid_PNA"]] <- NA_real_
  err <- expect_error(
    suppressWarnings(hitop::validity_pid5(dirty, items = 1:220)),
    class = "hitop_append_collision"
  )
  named <- named_columns(conditionMessage(err))
  expect_setequal(named, "pid_PNA")
})

test_that("append = FALSE is unaffected by a column of the same name in data", {
  dirty <- hitop::sim_hitopbr
  dirty[["hbr_antagonism"]] <- NA_real_
  expect_no_error(
    suppressWarnings(
      hitop::score_hitopbr(dirty, items = 1:45, append = FALSE)
    )
  )
})

test_that("the collision is reported after the existing argument checks", {
  # A call that is both colliding and otherwise invalid still reports the
  # existing complaint, so no current error message moves (the 2026-08-28 M060
  # implementation gate).
  dirty <- hitop::sim_hitopbr
  dirty[["hbr_antagonism"]] <- NA_real_
  err <- expect_error(
    suppressWarnings(hitop::score_hitopbr(dirty, items = 1:44))
  )
  expect_false(inherits(err, "hitop_append_collision"))
  expect_match(conditionMessage(err), "items", fixed = TRUE)
})

test_that("a collision is found wherever the column sits in data", {
  # Every other probe writes the colliding column onto the end of `data`. A
  # guard reading column positions rather than names would pass all of them, so
  # this probe puts the collision ahead of every column the call reads.
  produced <- names(suppressWarnings(
    hitop::score_hitopbr(hitop::sim_hitopbr, items = 1:45, append = FALSE)
  ))
  collide <- produced[[1]]
  dirty <- hitop::sim_hitopbr
  dirty[[collide]] <- NA_real_
  dirty <- dirty[c(collide, setdiff(names(dirty), collide))]
  expect_identical(names(dirty)[[1]], collide)

  err <- expect_error(
    suppressWarnings(hitop::score_hitopbr(dirty, items = 2:46)),
    class = "hitop_append_collision"
  )
  expect_setequal(named_columns(conditionMessage(err)), collide)
})

test_that("a colliding validity call is not also warned about its coding", {
  # The four other appending sites refuse ahead of their warnings, on the
  # reasoning that a call returning nothing is told about the collision alone.
  # `validity_pid5()` warned first until the M060 repair.
  dirty <- hitop::sim_pid5
  dirty[["pid_PNA"]] <- NA_real_
  expect_no_warning(
    expect_error(
      hitop::validity_pid5(dirty, items = 1:220, srange = c(1, 4)),
      class = "hitop_append_collision"
    )
  )
  # Control: the same call without the collision does warn about the coding, so
  # the silence above is the abort's doing and not a warning that never fires.
  expect_warning(
    suppressMessages(
      hitop::validity_pid5(hitop::sim_pid5, items = 1:220, srange = c(1, 4))
    ),
    "assume items coded 0-3"
  )
})
