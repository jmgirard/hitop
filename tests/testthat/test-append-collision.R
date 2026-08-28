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
    # Names the colliding column, and no other column the call would produce.
    expect_true(collide %in% named, info = nm)
    expect_length(intersect(named, setdiff(produced, collide)), 0)
  }
})

test_that("a collision message names every colliding column and no other", {
  probes <- collision_probes()
  exports <- append_exports()
  expect_true(length(exports) > 0)

  for (nm in exports) {
    p <- resolve_probe(nm, probes[[nm]])
    produced <- produced_columns(p)
    # Several at once, taken from both ends of the output so the set is not a
    # contiguous run the guard could happen to report as a range.
    collide <- unique(c(
      produced[[1]],
      produced[[ceiling(length(produced) / 2)]],
      produced[[length(produced)]]
    ))
    skip_if(length(collide) < 2, paste(nm, "produces too few columns"))

    dirty <- p$data
    for (cc in collide) dirty[[cc]] <- NA_real_
    err <- expect_error(
      suppressWarnings(do.call(p$fn, c(list(data = dirty), p$args))),
      class = "hitop_append_collision"
    )
    named <- named_columns(conditionMessage(err))
    expect_setequal(intersect(named, produced), collide)
    expect_true(all(collide %in% named))
  }
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
    expect_true(collide %in% named, info = nm)
    # The scale column the standard error is built on did not collide and must
    # not be reported: `hsr_agoraphobia_se` collided, `hsr_agoraphobia` did not.
    expect_length(intersect(named, setdiff(produced, collide)), 0)
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
  expect_true("pid_PNA" %in% named)
  expect_length(intersect(named, setdiff(produced, "pid_PNA")), 0)
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
