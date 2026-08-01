# Oracle note: the expected item numbers below are hand-derived from
# `hitopsr_items$Scale` (the item-level source), not copied from
# `hitopsr_scales$itemNumbers` (the table `hitop_subset()` actually reads), so
# a divergence between the two tables would surface here rather than cancel out.

test_that("hitop_subset() resolves items as the sorted union of its scales", {
  # Agoraphobia   = 66, 109, 118, 260, 291
  # Appetite Loss = 144, 202, 389
  s <- hitop_subset("hitopsr", c("agoraphobia", "appetiteLoss"))
  expect_equal(s$items, c(66, 109, 118, 144, 202, 260, 291, 389))
  expect_equal(s$nItems, 8)
  expect_equal(s$scales, c("Agoraphobia", "Appetite Loss"))
  expect_equal(s$camelCase, c("agoraphobia", "appetiteLoss"))
  expect_s3_class(s, "hitop_subset")
})

test_that("hitop_subset() resolves a second, disjoint selection", {
  # Antisocial Behavior  = 68, 156, 167, 185, 239, 268, 274, 390
  # Romantic Disinterest = 42, 152, 187, 310, 338
  s <- hitop_subset("hitopsr", c("antisocialBehavior", "romanticDisinterest"))
  expect_equal(
    s$items,
    c(42, 68, 152, 156, 167, 185, 187, 239, 268, 274, 310, 338, 390)
  )
  expect_equal(s$nItems, 13)
})

test_that("hitop_subset() carries the reverse flags for its items", {
  # HSR 310 is the only reverse-keyed item in the whole HiTOP-SR
  s <- hitop_subset("hitopsr", "romanticDisinterest")
  expect_equal(s$items, c(42, 152, 187, 310, 338))
  expect_equal(s$reverse, c(FALSE, FALSE, FALSE, TRUE, FALSE))

  s2 <- hitop_subset("hitopsr", "agoraphobia")
  expect_false(any(s2$reverse))
})

test_that("hitop_subset() accepts display names and camelCase stems, any case", {
  target <- hitop_subset("hitopsr", c("agoraphobia", "antisocialBehavior"))

  expect_equal(
    hitop_subset("hitopsr", c("Agoraphobia", "Antisocial Behavior"))$items,
    target$items
  )
  expect_equal(
    hitop_subset("hitopsr", c("AGORAPHOBIA", "antisocialbehavior"))$items,
    target$items
  )
  # canonical output is the display name regardless of the input form
  expect_equal(
    hitop_subset("hitopsr", "ANTISOCIALBEHAVIOR")$scales,
    "Antisocial Behavior"
  )
})

test_that("hitop_subset() de-duplicates and canonically orders its scales", {
  s <- hitop_subset(
    "hitopsr",
    c("appetiteLoss", "Agoraphobia", "agoraphobia", "Appetite Loss")
  )
  expect_equal(s$scales, c("Agoraphobia", "Appetite Loss"))
  expect_equal(s$items, c(66, 109, 118, 144, 202, 260, 291, 389))
})

test_that("hitop_subset() errors on unknown scale names, naming each", {
  expect_error(
    hitop_subset("hitopsr", c("agoraphobia", "nosuchscale")),
    "nosuchscale"
  )
  err <- expect_error(
    hitop_subset("hitopsr", c("bogusOne", "bogusTwo")),
    class = "rlang_error"
  )
  expect_match(conditionMessage(err), "bogusOne")
  expect_match(conditionMessage(err), "bogusTwo")
})

test_that("hitop_subset() errors on an empty or non-character selection", {
  expect_error(hitop_subset("hitopsr", character(0)), "at least one")
  expect_error(hitop_subset("hitopsr", NULL), "at least one")
  expect_error(hitop_subset("hitopsr", 1:3), "character")
  expect_error(hitop_subset("hitopsr", NA_character_), "missing")
})

test_that("hitop_subset() errors for instruments that are not yet supported", {
  expect_error(hitop_subset("hitopbr", "agoraphobia"), "not yet supported")
  expect_error(hitop_subset("pid5", "agoraphobia"), "not yet supported")
  expect_error(hitop_subset("nosuchinstrument", "agoraphobia"), "instrument")
})

test_that("hitop_subset() prints a compact summary", {
  expect_snapshot(hitop_subset("hitopsr", c("agoraphobia", "appetiteLoss")))
})

# --- subset_engine_inputs(): descriptor -> engine inputs -------------------
# Oracle note: the expected POSITIONS below are hand-derived from the item
# numbers already hand-derived at the top of this file, never recomputed from
# `hitopsr_scales$itemNumbers` at test time.

test_that("subset_engine_inputs() maps item numbers to subset-column positions", {
  # Agoraphobia   = 66, 109, 118, 260, 291
  # Appetite Loss = 144, 202, 389
  # union, ascending: 66 109 118 144 202 260 291 389
  #        positions:  1   2   3   4   5   6   7   8
  s <- hitop_subset("hitopsr", c("agoraphobia", "appetiteLoss"))
  out <- subset_engine_inputs(
    subset = s,
    instrument = "hitopsr",
    items = hitopsr_items,
    scales = hitopsr_scales,
    item_col = "HSR"
  )
  expect_equal(out$n_items, 8)
  expect_equal(names(out$items_scales), c("agoraphobia", "appetiteLoss"))
  expect_equal(out$items_scales$agoraphobia, c(1L, 2L, 3L, 6L, 7L))
  expect_equal(out$items_scales$appetiteLoss, c(4L, 5L, 8L))
})

test_that("subset_engine_inputs() locates the reverse-keyed item by position", {
  # HSR 310 is the whole instrument's only reverse-keyed item, and it is the
  # 4th of Romantic Disinterest's five items (42, 152, 187, 310, 338).
  s <- hitop_subset("hitopsr", "romanticDisinterest")
  out <- subset_engine_inputs(
    s, "hitopsr", hitopsr_items, hitopsr_scales, item_col = "HSR"
  )
  expect_equal(out$reverse_items, 4L)

  # A subset holding no reverse-keyed item gets an empty vector, which
  # prep_items() skips rather than reverse-keying nothing.
  s2 <- hitop_subset("hitopsr", "agoraphobia")
  out2 <- subset_engine_inputs(
    s2, "hitopsr", hitopsr_items, hitopsr_scales, item_col = "HSR"
  )
  expect_length(out2$reverse_items, 0)
})

test_that("subset_engine_inputs() returns scales in instrument row order", {
  # Selection order must not leak into the output: appetiteLoss is row 3 of
  # hitopsr_scales and agoraphobia row 1, whichever order they are asked for.
  s <- hitop_subset("hitopsr", c("appetiteLoss", "agoraphobia"))
  out <- subset_engine_inputs(
    s, "hitopsr", hitopsr_items, hitopsr_scales, item_col = "HSR"
  )
  expect_equal(names(out$items_scales), c("agoraphobia", "appetiteLoss"))
})

test_that("subset_engine_inputs() covers every subset column exactly once", {
  s <- hitop_subset("hitopsr", c("agoraphobia", "romanticDisinterest", "appetiteLoss"))
  out <- subset_engine_inputs(
    s, "hitopsr", hitopsr_items, hitopsr_scales, item_col = "HSR"
  )
  pos <- sort(unlist(out$items_scales, use.names = FALSE))
  expect_equal(pos, seq_len(s$nItems))
})

test_that("subset_engine_inputs() rejects a non-subset and a wrong instrument", {
  expect_error(
    subset_engine_inputs(
      list(items = 1), "hitopsr", hitopsr_items, hitopsr_scales, item_col = "HSR"
    ),
    "hitop_subset"
  )

  # Only reachable by hand-assembly: hitop_subset() will not build this.
  fake <- hitop_subset("hitopsr", "agoraphobia")
  fake$instrument <- "hitopbr"
  expect_error(
    subset_engine_inputs(
      fake, "hitopsr", hitopsr_items, hitopsr_scales, item_col = "HSR"
    ),
    "wrong instrument"
  )
})
