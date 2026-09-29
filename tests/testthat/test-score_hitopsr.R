# Ground-truth oracle tests for score_hitopsr() (milestone M005).
# Fixtures + hand-computed expectations live in helper-fixtures.R (fx_hitopsr).

test_that("score_hitopsr() matches hand-computed scores (incl. reverse item 310)", {
  out <- score_hitopsr(fx_hitopsr(), items = sprintf("hsr_%03d", 1:405))

  # Romantic Disinterest carries the ONLY reverse-keyed HiTOP-SR item (HSR 310).
  # If reverse-keying were skipped, R1 would be mean(1,1,1,1,1) = 1, not 1.6.
  expect_equal(out$hsr_romanticDisinterest, c(1.6, 3.4, 1.8, 2.8))

  # Two small no-reverse scales; R4 exercises na.rm on the NA'd item 144.
  expect_equal(out$hsr_appetiteLoss, c(1, 4, 2, 3))
  expect_equal(out$hsr_bingeEating, c(1, 4, 2, 3))
})

test_that("score_hitopsr() independently recomputes a scale from hardcoded numbers", {
  # Independent recomputation: Appetite Loss items copied straight from the
  # published HiTOP-SR key (hitopsr_items.csv), NOT read from hitopsr_scales.
  # This is the only check that catches a transcription error in the scales
  # table's itemNumbers.
  appetite_items <- c(144, 202, 389)

  # The scales table's itemNumbers must equal the source Scale grouping.
  i <- which(hitopsr_scales$camelCase == "appetiteLoss")
  expect_equal(sort(hitopsr_scales$itemNumbers[[i]]), appetite_items)
  expect_setequal(hitopsr_items$HSR[hitopsr_items$Scale == "Appetite Loss"],
                  appetite_items)

  # Dumb, explicit recomputation on random data, compared to the package.
  set.seed(405)
  df <- as.data.frame(matrix(
    sample(1:4, 6 * 405, replace = TRUE),
    nrow = 6, ncol = 405
  ))
  names(df) <- sprintf("hsr_%03d", seq_len(405))
  manual <- rowMeans(df[, sprintf("hsr_%03d", appetite_items)])

  out <- score_hitopsr(df, items = sprintf("hsr_%03d", 1:405), append = FALSE)
  expect_equal(out$hsr_appetiteLoss, manual)
})

test_that("score_hitopsr() honors invariants: se, prefix, row count", {
  df <- fx_hitopsr()
  items <- sprintf("hsr_%03d", 1:405)

  # calc_se adds a _se column per scale iff requested.
  base <- score_hitopsr(df, items = items, append = FALSE)
  with_se <- hush_se(score_hitopsr(df, items = items, calc_se = TRUE, append = FALSE))
  expect_false(any(grepl("_se$", names(base))))
  expect_equal(sum(grepl("_se$", names(with_se))), nrow(hitopsr_scales))
  expect_true(all(paste0(names(base), "_se") %in% names(with_se)))

  # A constant row (R1: all items = 1) has zero within-scale SE.
  expect_equal(with_se$hsr_appetiteLoss_se[1], 0)

  # prefix is applied to every scale column.
  pref <- score_hitopsr(df, items = items, prefix = "z_", append = FALSE)
  expect_true(all(paste0("z_", hitopsr_scales$camelCase) %in% names(pref)))

  # Output row count equals input row count; append binds onto the input.
  expect_equal(nrow(base), nrow(df))
  appended <- score_hitopsr(df, items = items, append = TRUE)
  expect_equal(nrow(appended), nrow(df))
  expect_true(all(names(df) %in% names(appended)))
})

# --- module scoring (M037) ---------------------------------------------------
# The module path hands score_engine() the same three inputs as the full path,
# remapped to positions within the supplied columns. These tests fix the output
# contract and pin it to the full-instrument run, which the oracle tests above
# already tie to hand-computed values.

# Four scales, deliberately named out of instrument row order.
sub_four <- function() {
  hitop_module(
    "hitopsr",
    c("romanticDisinterest", "appetiteLoss", "agoraphobia", "antisocialBehavior")
  )
}

# The columns a respondent to that short form would actually supply.
sub_data <- function(s, data = sim_hitopsr) data[s$items]

test_that("score_hitopsr(module=) returns exactly the module's scales, in row order", {
  s <- sub_four()
  out <- score_hitopsr(
    sub_data(s), items = seq_len(s$nItems), module = s, append = FALSE
  )

  # hitopsr_scales row order, NOT the order the scales were asked for.
  expect_equal(
    names(out),
    c("hsr_agoraphobia", "hsr_antisocialBehavior", "hsr_appetiteLoss",
      "hsr_romanticDisinterest")
  )
  expect_equal(nrow(out), nrow(sim_hitopsr))
})

test_that("score_hitopsr(module=) equals the full run, both missing modes, NAs or not", {
  s <- sub_four()

  # Injected NAs must land inside the module's own columns to separate
  # "available" from "complete" on the module path at all. One item per scale,
  # named by HSR number (sim_hitopsr's 405 columns are in instrument order):
  # 66 agoraphobia, 68 antisocial behavior, 144 appetite loss, 310 romantic
  # disinterest — the last also being the only reverse-keyed item.
  set.seed(37)
  holed <- sim_hitopsr
  for (j in c(66, 68, 144, 310)) {
    holed[sample(nrow(holed), 8), j] <- NA_integer_
  }
  # Guard against a vacuous comparison: the two modes must actually differ here.
  expect_false(isTRUE(all.equal(
    score_hitopsr(holed, items = 1:405, missing = "available", append = FALSE),
    score_hitopsr(holed, items = 1:405, missing = "complete", append = FALSE)
  )))

  for (dat in list(sim_hitopsr, holed)) {
    for (mode in c("available", "complete")) {
      full <- score_hitopsr(dat, items = 1:405, missing = mode, append = FALSE)
      part <- score_hitopsr(
        sub_data(s, dat), items = seq_len(s$nItems),
        module = s, missing = mode, append = FALSE
      )
      expect_equal(part, full[names(part)], info = mode)
    }
  }
})

test_that("score_hitopsr(module=) reverse-keys HSR 310 at its module position", {
  # Fixture row 3 answers Romantic Disinterest (42, 152, 187, 310, 338) with
  # (1, 2, 3, 4, 2). HSR 310 is the instrument's only reverse-keyed item and is
  # the 4th of those five, so under srange = c(1, 4) it becomes 5 - 4 = 1, and
  # the scale is mean(1, 2, 3, 1, 2) = 9/5 = 1.8. Unreversed it would be 2.4.
  s <- hitop_module("hitopsr", "romanticDisinterest")
  out <- score_hitopsr(
    sub_data(s, fx_hitopsr()), items = seq_len(s$nItems),
    module = s, append = FALSE
  )
  expect_equal(out$hsr_romanticDisinterest[[3]], 1.8)
})

test_that("score_hitopsr(module=) returns standard errors for just its scales", {
  s <- sub_four()
  full <- hush_se(score_hitopsr(sim_hitopsr, items = 1:405, calc_se = TRUE, append = FALSE))
  part <- hush_se(score_hitopsr(
    sub_data(s), items = seq_len(s$nItems),
    module = s, calc_se = TRUE, append = FALSE
  ))

  expect_equal(
    names(part),
    c(paste0("hsr_", s$camelCase), paste0("hsr_", s$camelCase, "_se"))
  )
  expect_equal(part, full[names(part)])
})

test_that("score_hitopsr(module=) takes column names as well as positions", {
  s <- sub_four()
  dat <- sub_data(s)
  by_pos <- score_hitopsr(dat, items = seq_len(s$nItems), module = s, append = FALSE)
  by_name <- score_hitopsr(dat, items = names(dat), module = s, append = FALSE)
  expect_equal(by_name, by_pos)
})

# --- subscale scoring (M141) -------------------------------------------------
# subscale_key and subscale_parent live in helper-fixtures.R.

hsr_names <- function(n) sprintf("hsr_%03d", n)

# sim_hitopsr with one item of every subscale set to NA in a different row, so
# every subscale has a missing item somewhere and the two `missing` modes
# differ on every subscale column.
holed_subscales <- function() {
  dat <- sim_hitopsr
  for (k in seq_along(subscale_key)) {
    dat[k, subscale_key[[k]][[1]]] <- NA_integer_
  }
  dat
}

test_that("include_subscales = TRUE adds the 17 subscale columns after the scale columns", {
  sub_cols <- paste0("hsr_", names(subscale_key))
  dat <- holed_subscales()
  for (mode in c("available", "complete")) {
    base <- score_hitopsr(dat, items = 1:405, missing = mode, append = FALSE)
    with_sub <- score_hitopsr(dat, items = 1:405, missing = mode,
                              include_subscales = TRUE, append = FALSE)

    # Default: no subscale column under any prefix-plus-stem name.
    expect_false(any(sub_cols %in% names(base)), info = mode)
    # With TRUE: the scale columns unchanged and first, then one column per
    # subscale in hitopsr_subscales row order (alphabetical by Subscale, which
    # subscale_key also follows).
    expect_equal(names(with_sub), c(names(base), sub_cols), info = mode)
    expect_equal(with_sub[names(base)], base, info = mode)

    # Under calc_se, the subscale _se columns follow the scale _se columns.
    base_se <- hush_se(score_hitopsr(dat, items = 1:405, missing = mode,
                                     calc_se = TRUE, append = FALSE))
    sub_se <- hush_se(score_hitopsr(dat, items = 1:405, missing = mode,
                                    calc_se = TRUE, include_subscales = TRUE,
                                    append = FALSE))
    scale_cols <- paste0("hsr_", hitopsr_scales$camelCase)
    expect_equal(
      names(sub_se),
      c(scale_cols, sub_cols, paste0(scale_cols, "_se"), paste0(sub_cols, "_se")),
      info = mode
    )
    expect_equal(sub_se[names(base_se)], base_se, info = mode)
  }
})

test_that("subscale scores match hand-computed values under both missing modes", {
  # Two rows, every item 1 except the ones set here. Cynicism is items
  # 8, 181, 261, 352 and Hallucinations 3, 51, 221, 259, 280, 385.
  dat <- as.data.frame(matrix(1L, nrow = 2, ncol = 405))
  names(dat) <- hsr_names(1:405)
  dat[1, hsr_names(c(8, 181, 261, 352))] <- c(1L, 2L, 3L, 4L)
  dat[2, hsr_names(c(8, 181, 261, 352))] <- c(4L, NA, 2L, 1L)
  dat[1, hsr_names(c(3, 51, 221, 259, 280, 385))] <- c(1L, 2L, 1L, 2L, 1L, 2L)
  dat[2, hsr_names(c(3, 51, 221, 259, 280, 385))] <- c(4L, 4L, 3L, 3L, 2L, 1L)

  avail <- score_hitopsr(dat, items = 1:405, include_subscales = TRUE,
                         append = FALSE)
  compl <- score_hitopsr(dat, items = 1:405, include_subscales = TRUE,
                         missing = "complete", append = FALSE)

  # Cynicism row 1: (1 + 2 + 3 + 4) / 4 = 10 / 4 = 2.5.
  # Row 2, available: (4 + 2 + 1) / 3 = 7 / 3. Complete: NA (item 181 missing).
  expect_equal(avail$hsr_cynicism, c(2.5, 7 / 3))
  expect_equal(compl$hsr_cynicism, c(2.5, NA))
  # Hallucinations row 1: (1 + 2 + 1 + 2 + 1 + 2) / 6 = 9 / 6 = 1.5.
  # Row 2: (4 + 4 + 3 + 3 + 2 + 1) / 6 = 17 / 6, in both modes (no NA).
  expect_equal(avail$hsr_hallucinations, c(1.5, 17 / 6))
  expect_equal(compl$hsr_hallucinations, c(1.5, 17 / 6))
})

test_that("all 17 subscale scores equal a recomputation from hardcoded item numbers", {
  dat <- holed_subscales()
  for (mode in c("available", "complete")) {
    out <- score_hitopsr(dat, items = hsr_names(1:405), missing = mode,
                         include_subscales = TRUE, append = FALSE)
    for (s in names(subscale_key)) {
      manual <- rowMeans(dat[, subscale_key[[s]]],
                         na.rm = identical(mode, "available"))
      expect_equal(out[[paste0("hsr_", s)]], manual, info = paste(mode, s))
    }
  }

  # The hardcoded numbers equal the shipped table's, subscale by subscale.
  expect_equal(hitopsr_subscales$camelCase, names(subscale_key))
  expect_equal(
    lapply(unname(hitopsr_subscales$itemNumbers), as.numeric),
    lapply(unname(subscale_key), sort)
  )
})

test_that("under a module, include_subscales scores exactly the subscales of its scales", {
  dat <- holed_subscales()
  full <- lapply(
    c(available = "available", complete = "complete"),
    function(mode) score_hitopsr(dat, items = 1:405, missing = mode,
                                 include_subscales = TRUE, append = FALSE)
  )

  probes <- c(
    # One module per parent scale.
    lapply(names(subscale_parent), function(p) {
      list(scales = p, subs = subscale_parent[[p]])
    }),
    list(
      # Two parent scales plus a scale with no subscales.
      list(scales = c("mistrust", "dishonesty", "agoraphobia"),
           subs = c(subscale_parent$dishonesty, subscale_parent$mistrust)),
      # No parent scale at all.
      list(scales = c("agoraphobia", "appetiteLoss"), subs = character(0))
    )
  )

  for (p in probes) {
    m <- hitop_module("hitopsr", p$scales)
    for (mode in names(full)) {
      out <- score_hitopsr(dat[m$items], items = seq_len(m$nItems),
                           module = m, missing = mode,
                           include_subscales = TRUE, append = FALSE)
      sub_cols <- setdiff(names(out), paste0("hsr_", hitopsr_scales$camelCase))
      # sprintf(), not paste0(): paste0("hsr_", character(0)) is "hsr_".
      expect_setequal(sub_cols, sprintf("hsr_%s", p$subs))
      expect_equal(out[sub_cols], full[[mode]][sub_cols],
                   info = paste(mode, toString(p$scales)))
    }
  }
})

test_that("include_subscales must be a single TRUE or FALSE", {
  for (bad in list(NA, "yes", c(TRUE, TRUE))) {
    expect_error(
      score_hitopsr(sim_hitopsr, items = 1:405, include_subscales = bad),
      "include_subscales.*must be",
      class = "rlang_error"
    )
  }
})

test_that("include_subscales = TRUE refuses a data column named as a subscale column", {
  for (s in names(subscale_key)) {
    dat <- sim_hitopsr
    dat[[paste0("hsr_", s)]] <- 0
    expect_error(
      score_hitopsr(dat, items = 1:405, include_subscales = TRUE),
      class = "hitop_append_collision"
    )
    dat <- sim_hitopsr
    dat[[paste0("hsr_", s, "_se")]] <- 0
    expect_error(
      score_hitopsr(dat, items = 1:405, include_subscales = TRUE,
                    calc_se = TRUE),
      class = "hitop_append_collision"
    )
  }

  # With the default, a column of that name is not refused and is kept as is.
  dat <- sim_hitopsr
  dat$hsr_cynicism <- 0
  out <- score_hitopsr(dat, items = 1:405)
  expect_identical(out$hsr_cynicism, rep(0, nrow(dat)))
})
