# The subset -> module rename. These tests hold the deprecation contract: the
# old names keep working, they say so by signalling a condition whose CLASS is
# stable (the message text is not), and — the point of the whole exercise — no
# number moves.
#
# Oracle note for the equality tests below: the module-scored values are
# compared against a FULL 405-item administration of the same data, restricted
# to the same scale columns. That is an independent recomputation, not the
# code's own output re-asserted: a scale score depends only on its own items,
# so dropping the other scales' columns cannot move it.

module_cols <- function(m, data = sim_hitopsr) {
  # Select by NAME, never by `m$items`: those are original HiTOP-SR numbers,
  # which are column positions only in a frame that is exactly the 405 items
  # in order.
  data[paste0("hsr_", m$items)]
}

# --- AC1: the two constructors differ only in class -------------------------

test_that("hitop_module() and hitop_subset() build identical descriptors", {
  scales <- c("agoraphobia", "appetiteLoss")
  new <- hitop_module("hitopsr", scales)
  old <- suppressWarnings(hitop_subset("hitopsr", scales))

  expect_s3_class(new, "hitop_module")
  expect_s3_class(old, "hitop_subset")

  # Identical apart from the class attribute: strip it from both and nothing
  # else may differ.
  expect_identical(unclass(new), unclass(old))
})

# --- AC2: hitop_subset() warns, by class, and still works -------------------

test_that("hitop_subset() signals a deprecation condition of a stable class", {
  expect_warning(
    hitop_subset("hitopsr", c("agoraphobia", "appetiteLoss")),
    class = "hitop_deprecated_subset"
  )
})

test_that("a descriptor from the deprecated constructor still scores", {
  old <- suppressWarnings(hitop_subset("hitopsr", c("agoraphobia", "appetiteLoss")))
  collected <- module_cols(old)

  scores <- score_hitopsr(
    collected,
    items = names(collected),
    module = old,
    append = FALSE
  )
  expect_equal(ncol(scores), 2L)
  expect_named(scores, c("hsr_agoraphobia", "hsr_appetiteLoss"))
})

# --- AC3: every function that took `subset =` --------------------------------

# One row per exported function that carried the argument, so a function added
# to (or dropped from) the family shows up here as a change to this table
# rather than as a silently untested branch.
arg_consumers <- list(
  list(
    name = "score_hitopsr",
    call = function(m, arg) {
      collected <- module_cols(m)
      args <- list(collected, items = names(collected), append = FALSE)
      do.call(score_hitopsr, c(args, arg))
    }
  ),
  list(
    name = "reliability_hitopsr",
    call = function(m, arg) {
      collected <- module_cols(m)
      args <- list(collected, items = names(collected), omega = FALSE)
      do.call(reliability_hitopsr, c(args, arg))
    }
  ),
  list(
    name = "generate_docx_hitopsr",
    call = function(m, arg) {
      args <- list(file = tempfile(fileext = ".docx"))
      do.call(generate_docx_hitopsr, c(args, arg))
    }
  ),
  list(
    name = "generate_qualtrics_hitopsr",
    call = function(m, arg) {
      args <- list(file = tempfile(fileext = ".txt"))
      do.call(generate_qualtrics_hitopsr, c(args, arg))
    }
  ),
  list(
    name = "generate_redcap_hitopsr",
    call = function(m, arg) {
      args <- list(file = tempfile(fileext = ".zip"))
      do.call(generate_redcap_hitopsr, c(args, arg))
    }
  )
)

test_that("every consumer accepts `module =` without warning", {
  m <- hitop_module("hitopsr", c("agoraphobia", "appetiteLoss"))
  for (f in arg_consumers) {
    expect_no_warning(f$call(m, list(module = m)), message = f$name)
  }
})

test_that("every consumer accepts a legacy hitop_subset object", {
  old <- suppressWarnings(hitop_subset("hitopsr", c("agoraphobia", "appetiteLoss")))
  for (f in arg_consumers) {
    expect_no_warning(f$call(old, list(module = old)), message = f$name)
  }
})

test_that("every consumer warns by class when given `subset =`", {
  m <- hitop_module("hitopsr", c("agoraphobia", "appetiteLoss"))
  for (f in arg_consumers) {
    expect_warning(
      f$call(m, list(subset = m)),
      class = "hitop_deprecated_subset",
      info = f$name
    )
  }
})

test_that("every consumer errors by class when given both arguments", {
  m <- hitop_module("hitopsr", c("agoraphobia", "appetiteLoss"))
  for (f in arg_consumers) {
    expect_error(
      f$call(m, list(module = m, subset = m)),
      class = "hitop_both_module_args",
      info = f$name
    )
  }
})

# --- AC4: the rename moves no number ----------------------------------------

# Three shapes, chosen to vary the axes the values could plausibly turn on:
# width (one scale vs four) and reverse-keying (romanticDisinterest carries a
# reverse-keyed item; agoraphobia and appetiteLoss carry none).
module_shapes <- list(
  single = "agoraphobia",
  reversed = "romanticDisinterest",
  vignette = c(
    "Agoraphobia", "Appetite Loss",
    "Antisocial Behavior", "Romantic Disinterest"
  )
)

test_that("scores and standard errors are unmoved by the rename", {
  full <- score_hitopsr(
    sim_hitopsr,
    items = 1:405,
    calc_se = TRUE,
    append = FALSE
  )

  for (shape in names(module_shapes)) {
    m <- hitop_module("hitopsr", module_shapes[[shape]])
    collected <- module_cols(m)

    via_module <- score_hitopsr(
      collected,
      items = names(collected),
      module = m,
      calc_se = TRUE,
      append = FALSE
    )
    via_subset <- suppressWarnings(score_hitopsr(
      collected,
      items = names(collected),
      subset = m,
      calc_se = TRUE,
      append = FALSE
    ))

    expect_equal(via_module, via_subset, info = shape)
    # The independent recomputation: a full administration restricted to the
    # same columns.
    expect_equal(via_module, full[names(via_module)], info = shape)
  }
})

test_that("Cronbach's alpha is unmoved by the rename", {
  full <- reliability_hitopsr(
    sim_hitopsr,
    items = 1:405,
    omega = FALSE
  )

  for (shape in names(module_shapes)) {
    m <- hitop_module("hitopsr", module_shapes[[shape]])
    collected <- module_cols(m)

    via_module <- reliability_hitopsr(
      collected,
      items = names(collected),
      module = m,
      omega = FALSE
    )
    via_subset <- suppressWarnings(reliability_hitopsr(
      collected,
      items = names(collected),
      subset = m,
      omega = FALSE
    ))

    expect_equal(via_module, via_subset, info = shape)
    expect_equal(
      via_module,
      full[full$scale %in% via_module$scale, ],
      ignore_attr = TRUE,
      info = shape
    )
  }
})
