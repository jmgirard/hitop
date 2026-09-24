# The `layout` argument of score_hitopsr() and reliability_hitopsr().
#
# A module Word form written with `randomize = TRUE` prints its items in a
# shuffled order, recorded on the module's `item_order` attribute (the original
# HiTOP-SR item numbers in printed order). Data entered straight off that form
# has its columns in printed order: column k holds the answer to printed item
# k. `layout = "printed"` scores such columns by permuting the caller's `items`
# through `item_order` before the engine runs; the default `"instrument"` is
# the pre-existing behavior, columns in ascending instrument order.
#
# Every fixture below builds the responses in INSTRUMENT order first, then lays
# the columns out in printed order by `inst[match(item_order, module$items)]`,
# so the two calls compared always see the same answers.

# --- fixtures -------------------------------------------------------------

# A module carrying a printed order. `item_order` must not be its own inverse,
# or a test could pass with the permutation applied the wrong way round: the
# printed-position -> instrument-rank map is `match(item_order, module$items)`,
# its inverse `match(module$items, item_order)`, and an involution makes the
# two identical.
with_order <- function(module, item_order) {
  item_order <- as.integer(item_order)
  stopifnot(setequal(item_order, module$items))
  stopifnot(!identical(
    match(item_order, module$items),
    match(module$items, item_order)
  ))
  attr(module, "item_order") <- item_order
  module
}

# A 3-cycle on the first three instrument positions; the rest stay put.
three_cycle <- function(items) c(items[c(2L, 3L, 1L)], items[-(1:3)])

# Instrument-order responses: one column per instrument item, `hsr_042`-style
# names, one row per respondent. `answers` is a list of named integer vectors
# keyed by item number.
inst_frame <- function(module, answers) {
  df <- as.data.frame(lapply(module$items, function(i) {
    vapply(answers, function(a) a[[as.character(i)]], integer(1))
  }))
  names(df) <- sprintf("hsr_%03d", module$items)
  df
}

# The same responses laid out in printed order, names travelling with them.
printed_frame <- function(inst, module) {
  inst[match(attr(module, "item_order"), module$items)]
}

# Romantic Disinterest (42, 152, 187, 310, 338; HSR 310 reverse-keyed) and
# Appetite Loss (144, 202, 389). Expected scores are hand-computed below.
keyed_pair <- function() {
  m <- hitop_module("hitopsr", c("Romantic Disinterest", "Appetite Loss"))
  expect_identical(m$items, c(42L, 144L, 152L, 187L, 202L, 310L, 338L, 389L))
  with_order(m, three_cycle(m$items))
}

keyed_answers <- list(
  # Romantic Disinterest raw (1, 2, 3, 4, 2); HSR 310 reverses to 5 - 4 = 1,
  # so mean(1, 2, 3, 1, 2) = 9/5 = 1.8. Appetite Loss mean(2, 3, 1) = 2.
  r1 = c("42" = 1L, "144" = 2L, "152" = 2L, "187" = 3L, "202" = 3L,
         "310" = 4L, "338" = 2L, "389" = 1L),
  # Romantic Disinterest raw (4, 1, 1, 1, 3); HSR 310 reverses to 5 - 1 = 4,
  # so mean(4, 1, 1, 4, 3) = 13/5 = 2.6. Appetite Loss mean(4, 2, 3) = 3.
  r2 = c("42" = 4L, "144" = 4L, "152" = 1L, "187" = 1L, "202" = 2L,
         "310" = 1L, "338" = 3L, "389" = 3L)
)
keyed_expected <- data.frame(
  hsr_appetiteLoss = c(2, 3),
  hsr_romanticDisinterest = c(1.8, 2.6)
)

# Agoraphobia (66, 109, 118, 260, 291) and Appetite Loss (144, 202, 389): no
# reverse-keyed item. Shuffled with a seeded sample; with_order() asserts the
# draw is not an involution.
unkeyed_pair <- function() {
  m <- hitop_module("hitopsr", c("Agoraphobia", "Appetite Loss"))
  expect_identical(m$items, c(66L, 109L, 118L, 144L, 202L, 260L, 291L, 389L))
  set.seed(1)
  with_order(m, sample(m$items))
}

unkeyed_answers <- list(
  # Agoraphobia mean(1, 2, 3, 2, 3) = 11/5 = 2.2. Appetite Loss mean(4, 1, 4) = 3.
  r1 = c("66" = 1L, "109" = 2L, "118" = 3L, "144" = 4L, "202" = 1L,
         "260" = 2L, "291" = 3L, "389" = 4L),
  # Agoraphobia mean(4, 4, 3, 1, 2) = 14/5 = 2.8. Appetite Loss mean(1, 2, 3) = 2.
  r2 = c("66" = 4L, "109" = 4L, "118" = 3L, "144" = 1L, "202" = 2L,
         "260" = 1L, "291" = 2L, "389" = 3L)
)
unkeyed_expected <- data.frame(
  hsr_agoraphobia = c(2.2, 2.8),
  hsr_appetiteLoss = c(3, 2)
)

# Four scales, shuffled; responses drawn from sim_hitopsr.
four_scale <- function() {
  m <- hitop_module(
    "hitopsr",
    c("romanticDisinterest", "appetiteLoss", "agoraphobia", "antisocialBehavior")
  )
  set.seed(2)
  with_order(m, sample(m$items))
}

# Every scale, so every one of the 405 items, shuffled.
whole_instrument <- function() {
  m <- hitop_module("hitopsr", hitopsr_scales$Scale)
  expect_identical(m$nItems, 405L)
  set.seed(3)
  with_order(m, sample(m$items))
}

sim_inst <- function(module) sim_hitopsr[sprintf("hsr_%03d", module$items)]

# --- AC2: hand fixtures ---------------------------------------------------

test_that("layout = 'printed' scores a keyed pair from printed-order columns", {
  m <- keyed_pair()
  inst <- inst_frame(m, keyed_answers)
  printed <- printed_frame(inst, m)

  # The instrument-order call pins the hand values first.
  expect_equal(
    as.data.frame(score_hitopsr(inst, items = names(inst), module = m, append = FALSE)),
    keyed_expected
  )
  out <- score_hitopsr(
    printed, items = seq_len(m$nItems), module = m,
    layout = "printed", append = FALSE
  )
  expect_equal(as.data.frame(out), keyed_expected)

  # Discrimination: the same printed columns scored as instrument order give
  # different answers, so the test can tell the two layouts apart.
  wrong <- score_hitopsr(printed, items = seq_len(m$nItems), module = m, append = FALSE)
  expect_false(isTRUE(all.equal(as.data.frame(wrong), keyed_expected)))
})

test_that("layout = 'printed' scores an unkeyed pair from printed-order columns", {
  m <- unkeyed_pair()
  inst <- inst_frame(m, unkeyed_answers)
  printed <- printed_frame(inst, m)

  expect_equal(
    as.data.frame(score_hitopsr(inst, items = names(inst), module = m, append = FALSE)),
    unkeyed_expected
  )
  # Positions, so the name heuristic (AC5, below) stays out of this check.
  out <- score_hitopsr(
    printed, items = seq_len(m$nItems), module = m,
    layout = "printed", append = FALSE
  )
  expect_equal(as.data.frame(out), unkeyed_expected)

  wrong <- score_hitopsr(printed, items = seq_len(m$nItems), module = m, append = FALSE)
  expect_false(isTRUE(all.equal(as.data.frame(wrong), unkeyed_expected)))
})

# --- AC2: consistency with the instrument branch --------------------------

test_that("layout = 'printed' equals the instrument-order call for a four-scale module", {
  m <- four_scale()
  inst <- sim_inst(m)
  printed <- printed_frame(inst, m)

  for (mode in c("available", "complete")) {
    ref <- score_hitopsr(
      inst, items = names(inst), module = m, missing = mode, append = FALSE
    )
    out <- score_hitopsr(
      printed, items = seq_len(m$nItems), module = m,
      layout = "printed", missing = mode, append = FALSE
    )
    expect_equal(out, ref, info = mode)
  }
  # Positions as `items`, and append = TRUE keeping the printed columns.
  out <- score_hitopsr(
    printed, items = seq_len(m$nItems), module = m, layout = "printed"
  )
  expect_identical(names(out)[seq_len(m$nItems)], names(printed))
  expect_equal(
    out[-seq_len(m$nItems)],
    score_hitopsr(inst, items = names(inst), module = m, append = FALSE)
  )
})

test_that("layout = 'printed' equals the instrument-order call for the whole instrument", {
  m <- whole_instrument()
  inst <- sim_inst(m)
  printed <- printed_frame(inst, m)

  ref <- score_hitopsr(inst, items = names(inst), module = m, append = FALSE)
  out <- score_hitopsr(
    printed, items = seq_len(m$nItems), module = m,
    layout = "printed", append = FALSE
  )
  expect_equal(out, ref)
  # And the module-free full run, which the oracle tests pin.
  expect_equal(out, score_hitopsr(sim_hitopsr, items = 1:405, append = FALSE))
})

test_that("layout = 'instrument' is the default and leaves the module path alone", {
  m <- four_scale()
  inst <- sim_inst(m)
  expect_identical(
    score_hitopsr(inst, items = names(inst), module = m, append = FALSE),
    score_hitopsr(inst, items = names(inst), module = m, layout = "instrument", append = FALSE)
  )
  # A module carrying an item_order is scored in instrument order by default:
  # the attribute alone changes nothing (plan gate, explicit over automatic).
  plain <- m
  attr(plain, "item_order") <- NULL
  expect_identical(
    score_hitopsr(inst, items = names(inst), module = m, append = FALSE),
    score_hitopsr(inst, items = names(inst), module = plain, append = FALSE)
  )
})

# --- AC3: reliability -----------------------------------------------------

test_that("reliability_hitopsr(layout = 'printed') returns the instrument-order alphas", {
  m <- four_scale()
  inst <- sim_inst(m)
  printed <- printed_frame(inst, m)

  ref <- reliability_hitopsr(inst, items = names(inst), module = m, omega = FALSE)
  out <- reliability_hitopsr(
    printed, items = seq_len(m$nItems), module = m,
    layout = "printed", omega = FALSE
  )
  expect_equal(out, ref)

  wrong <- reliability_hitopsr(printed, items = seq_len(m$nItems), module = m, omega = FALSE)
  expect_false(isTRUE(all.equal(wrong$alpha, ref$alpha)))
})

test_that("reliability_hitopsr(layout = 'printed') hands calc_omega the instrument-order items", {
  # An internal contract: the per-scale item matrix reaching calc_omega() must
  # be the same one the instrument-order call passes, or omega is estimated on
  # scrambled items. Responses are made per-item distinguishable so a column
  # swap cannot hide behind identical values.
  m <- four_scale()
  inst <- as.data.frame(matrix(
    rep(seq_along(m$items), each = 6L) + rep(c(0L, 1L, 0L, 2L, 1L, 0L), times = m$nItems),
    nrow = 6L
  ))
  names(inst) <- sprintf("hsr_%03d", m$items)
  expect_false(any(duplicated(as.list(inst))))
  printed <- printed_frame(inst, m)

  seen <- list()
  local_mocked_bindings(calc_omega = function(df) {
    seen[[length(seen) + 1L]] <<- df
    0.5
  })

  reliability_hitopsr(inst, items = names(inst), module = m, alpha = FALSE)
  ref <- seen
  seen <- list()
  reliability_hitopsr(
    printed, items = seq_len(m$nItems), module = m, layout = "printed", alpha = FALSE
  )
  expect_length(ref, length(m$scales))
  expect_identical(seen, ref)
})

# --- AC4: refusals --------------------------------------------------------

# The abort blames the exported wrapper and stays unclassed: a caller cannot
# catch it by name, as with the family's other argument validators.
expect_layout_abort <- function(expr, fn, pattern) {
  err <- expect_error(expr, pattern)
  expect_identical(conditionCall(err)[[1]], as.name(fn))
  expect_false(any(startsWith(class(err), "hitop_")))
  invisible(err)
}

test_that("layout = 'printed' refuses a call with no module", {
  m <- four_scale()
  inst <- sim_inst(m)
  err <- expect_layout_abort(
    score_hitopsr(inst, items = names(inst), layout = "printed"),
    "score_hitopsr", "`layout`"
  )
  expect_match(conditionMessage(err), "module")
  expect_match(conditionMessage(err), "randomize = TRUE", fixed = TRUE)
  expect_match(conditionMessage(err), "\"instrument\"", fixed = TRUE)
  expect_layout_abort(
    reliability_hitopsr(inst, items = names(inst), layout = "printed"),
    "reliability_hitopsr", "`layout`"
  )
})

test_that("layout = 'printed' refuses a module with no item_order", {
  m <- four_scale()
  attr(m, "item_order") <- NULL
  inst <- sim_inst(m)
  err <- expect_layout_abort(
    score_hitopsr(inst, items = names(inst), module = m, layout = "printed"),
    "score_hitopsr", "`layout`"
  )
  expect_match(conditionMessage(err), "item_order")
  expect_match(conditionMessage(err), "randomize = TRUE", fixed = TRUE)
  expect_match(conditionMessage(err), "\"instrument\"", fixed = TRUE)
  expect_layout_abort(
    reliability_hitopsr(inst, items = names(inst), module = m, layout = "printed"),
    "reliability_hitopsr", "`layout`"
  )
})

test_that("layout = 'printed' refuses an item_order that is not a permutation of the items", {
  m <- four_scale()
  inst <- sim_inst(m)
  bad <- list(
    short = attr(m, "item_order")[-1L],
    repeated = replace(attr(m, "item_order"), 2L, attr(m, "item_order")[[1L]]),
    foreign = replace(attr(m, "item_order"), 1L, 999L),
    missing = replace(attr(m, "item_order"), 1L, NA_integer_),
    text = as.character(attr(m, "item_order"))
  )
  for (label in names(bad)) {
    broken <- m
    attr(broken, "item_order") <- bad[[label]]
    err <- expect_layout_abort(
      score_hitopsr(inst, items = names(inst), module = broken, layout = "printed"),
      "score_hitopsr", "`layout`"
    )
    expect_match(conditionMessage(err), "permutation", info = label)
    expect_match(conditionMessage(err), "randomize = TRUE", fixed = TRUE, info = label)
    expect_match(conditionMessage(err), "\"instrument\"", fixed = TRUE, info = label)
    expect_layout_abort(
      reliability_hitopsr(inst, items = names(inst), module = broken, layout = "printed"),
      "reliability_hitopsr", "`layout`"
    )
  }
})

test_that("a layout outside the two choices aborts naming the function and both values", {
  m <- four_scale()
  inst <- sim_inst(m)
  # Called by name, not through a wrapper, so the blamed call is the exported
  # function's own.
  errs <- list(
    score_hitopsr = expect_layout_abort(
      score_hitopsr(inst, items = names(inst), module = m, layout = "shuffled"),
      "score_hitopsr", "`layout`"
    ),
    reliability_hitopsr = expect_layout_abort(
      reliability_hitopsr(inst, items = names(inst), module = m, layout = "shuffled"),
      "reliability_hitopsr", "`layout`"
    )
  )
  for (fn in names(errs)) {
    msg <- conditionMessage(errs[[fn]])
    expect_match(msg, "\"instrument\"", fixed = TRUE, info = fn)
    expect_match(msg, "\"printed\"", fixed = TRUE, info = fn)
  }
})

# --- AC5: the ascending-name heuristic reads the caller's items ------------

test_that("layout = 'printed' runs warn_item_order() on the supplied items, not the permuted ones", {
  m <- four_scale()
  inst <- sim_inst(m)
  printed <- printed_frame(inst, m)
  ref <- score_hitopsr(inst, items = names(inst), module = m, append = FALSE)

  # Printed-order columns named q_1 .. q_n: ascending, so no warning -- the
  # permuted vector would be non-ascending and warn if it were the one read.
  q <- printed
  names(q) <- paste0("q_", seq_len(m$nItems))
  warnings <- character(0)
  out <- withCallingHandlers(
    score_hitopsr(q, items = names(q), module = m, layout = "printed", append = FALSE),
    warning = function(w) {
      warnings <<- c(warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  expect_length(warnings, 0L)
  expect_equal(out, ref)

  # Non-ascending names on the caller's side warn exactly once.
  d <- printed
  names(d) <- paste0("q_", rev(seq_len(m$nItems)))
  warnings <- character(0)
  out <- withCallingHandlers(
    score_hitopsr(d, items = names(d), module = m, layout = "printed", append = FALSE),
    warning = function(w) {
      warnings <<- c(warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  expect_length(warnings, 1L)
  expect_match(warnings, "ascending")
  expect_equal(out, ref)

  # The same two counts on reliability_hitopsr().
  for (frame in list(q, d)) {
    warnings <- character(0)
    withCallingHandlers(
      reliability_hitopsr(frame, items = names(frame), module = m, layout = "printed", omega = FALSE),
      warning = function(w) {
        warnings <<- c(warnings, conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    )
    expect_length(warnings, if (identical(names(frame), names(q))) 0L else 1L)
  }
})

# --- the warning's remedy under layout = "printed" ---------------------------
#
# Regression: the misordered-names warning used to tell every caller to sort
# the names. Under layout = "printed" that advice undoes the printed order and
# silently scores the wrong items (the hitop-form fixture scored 2.6 and
# 2.5625 against the correct 3 and 2.4375). Under that layout the remedy is
# positions, never a sort.

test_that("under layout = 'printed' the misordered-names warning says positions, not sort", {
  data <- read_form_responses(example_file("responses-module-shuffled.csv"))
  m <- read_module(example_file("module-shuffled.json"))
  item_cols <- names(data)[-seq_len(8L)]

  cnd <- rlang::catch_cnd(
    score_hitopsr(data, items = item_cols, module = m, layout = "printed",
                  append = FALSE),
    "warning"
  )
  expect_s3_class(cnd, "warning")
  msg <- conditionMessage(cnd)
  expect_match(msg, "ascending", fixed = TRUE)
  expect_match(msg, "position", fixed = TRUE)
  expect_match(msg, "printed", fixed = TRUE)
  expect_false(grepl("Sort them", msg, fixed = TRUE))

  # The instrument-layout wording is unchanged: sorting is still the remedy.
  inst <- rlang::catch_cnd(
    score_hitopsr(data, items = item_cols, module = m, append = FALSE),
    "warning"
  )
  expect_match(conditionMessage(inst), "Sort them", fixed = TRUE)

  # Following the printed-layout remedy gives the correct scores.
  scored <- score_hitopsr(data, items = match(item_cols, names(data)),
                          module = m, layout = "printed", append = FALSE)
  expect_equal(scored$hsr_agoraphobia, 3)
  expect_equal(scored$hsr_distressDysphoria, 2.4375)
})
