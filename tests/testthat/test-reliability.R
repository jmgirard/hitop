# Ground-truth oracle tests for the reliability functions (milestone M005):
# calc_alpha() (covariance-based Cronbach's alpha) and calc_omega()
# (omega-total via a one-factor lavaan CFA).

test_that("calc_alpha() matches a hand-computed reference value", {
  # Tiny 3-item, 4-observation fixture; alpha worked out by hand below.
  #   X1 = 1,2,3,4   Var = 5/3   (cov() uses the n-1 denominator)
  #   X2 = 2,2,4,4   Var = 4/3
  #   X3 = 1,3,3,5   Var = 8/3
  #   sum of item variances       = 17/3
  #   T = X1+X2+X3 = 4,7,10,13     Var(T) = 45/3 = 15
  #   alpha = k/(k-1) * (1 - sumVar/VarT)
  #         = 3/2 * (1 - (17/3)/15) = 3/2 * (28/45) = 42/45 = 0.9333...
  df <- data.frame(
    X1 = c(1, 2, 3, 4),
    X2 = c(2, 2, 4, 4),
    X3 = c(1, 3, 3, 5)
  )
  expect_equal(calc_alpha(df), 42 / 45)
})

test_that("calc_alpha() handles NA via pairwise deletion", {
  df <- data.frame(
    X1 = c(1, 2, 3, 4),
    X2 = c(2, 2, 4, 4),
    X3 = c(1, 3, 3, 5)
  )
  df_na <- df
  df_na[1, "X1"] <- NA
  # Pairwise-complete covariances still return a finite alpha (not NA).
  expect_true(is.finite(calc_alpha(df_na)))
})

test_that("calc_alpha() rejects degenerate input", {
  expect_error(calc_alpha(data.frame(a = 1:4)), "two items")            # k < 2
  expect_error(calc_alpha(data.frame(a = 1, b = 2)), "two observations") # n < 2
  expect_error(
    calc_alpha(data.frame(a = c(1, 1, 1), b = c(2, 3, 4))),
    "positive variance"
  )
  expect_error(calc_alpha(1:4), "data frame or matrix")
})

test_that("calc_omega() matches a direct lavaan CFA on the same data", {
  skip_if_not_installed("lavaan")

  # Build a clean one-factor dataset (all positive loadings -> converges).
  set.seed(1)
  n <- 300
  f <- stats::rnorm(n)
  df <- data.frame(
    i1 = 0.8 * f + stats::rnorm(n),
    i2 = 0.7 * f + stats::rnorm(n),
    i3 = 0.6 * f + stats::rnorm(n),
    i4 = 0.75 * f + stats::rnorm(n),
    i5 = 0.65 * f + stats::rnorm(n)
  )

  # Independent reference: fit the same model directly and apply the omega
  # formula ourselves, without going through calc_omega()'s code path.
  fit <- lavaan::cfa(
    "f =~ i1 + i2 + i3 + i4 + i5",
    data = df, std.lv = TRUE, estimator = "MLR", missing = "fiml"
  )
  pe <- lavaan::parameterEstimates(fit)
  lambda <- pe$est[pe$op == "=~"]
  theta <- pe$est[pe$op == "~~" & pe$lhs == pe$rhs & pe$lhs != "f"]
  ref <- sum(lambda)^2 / (sum(lambda)^2 + sum(theta))

  expect_equal(calc_omega(df), ref)
  expect_true(ref > 0 && ref < 1)
})

test_that("calc_omega() warns on mixed-sign loadings", {
  skip_if_not_installed("lavaan")

  set.seed(2)
  n <- 300
  f <- stats::rnorm(n)
  df <- data.frame(
    i1 = 0.8 * f + stats::rnorm(n),
    i2 = 0.7 * f + stats::rnorm(n),
    i3 = 0.6 * f + stats::rnorm(n),
    i4 = -0.75 * f + stats::rnorm(n)  # reversed -> mixed-sign loadings
  )
  expect_warning(calc_omega(df), "[Mm]ixed-sign")
})

test_that("calc_omega() rejects degenerate input", {
  skip_if_not_installed("lavaan")
  expect_error(calc_omega(data.frame(a = 1:4)), "two items")
  expect_error(calc_omega("nope"), "data frame or matrix")
})

# ---- reliability_*() family (milestone M015) ---------------------------------
# The returning family that replaced the print-only alpha/omega args on the
# score_*() functions. Each resolves its instrument data and hands the reverse-
# keyed items to reliability_engine(), which calls calc_alpha()/calc_omega() per
# scale. Oracles below independently recompute alpha from hardcoded official item
# numbers (the same strategy as the M013 print oracle it replaces), so they catch
# a transcription error in the scales tables or a broken reverse-key in the prep.

test_that("reliability_hitopbr() matches independently recomputed alphas (M013 oracle)", {
  # ku_hitopbr: columns 1-2 are participant/biosex, then hbr_01..hbr_45.
  d <- ku_hitopbr
  items <- 3:47
  di <- as.data.frame(lapply(d[items], as.numeric))
  stopifnot(!any(hitopbr_items$Reverse))  # guards the no-reverse-keying assumption
  scales <- hitopbr_scales$itemNumbers
  exp_alpha <- vapply(
    scales,
    function(idx) tryCatch(calc_alpha(di[idx]), error = function(e) NA_real_),
    numeric(1)
  )

  rel <- reliability_hitopbr(d, items = items, omega = FALSE)
  expect_equal(rel$alpha, unname(exp_alpha))
  expect_equal(rel$nItems, unname(lengths(scales)))
  # The canonical column of the keying table, not the transformation under test.
  expect_equal(rel$Scale, hitopbr_scales$Scale)

  # Independent, HARDCODED oracles for the two scales whose membership the
  # workbook and this package once disagreed about (they guard the BR scales
  # table itself, not just the plumbing). Both item lists are copied from the
  # `item-to-scale` sheet of `B-HiTOP overview.xlsx`, translated to HBR numbers:
  # Detachment = HiTOP_50, 624, 44, 625, 657 and Internalizing = HiTOP_69, 187,
  # 378, 570, 333, 356, 368, 215. Item 36 belongs to the second list, not the
  # first (SOURCES.md, "HiTOP-BR item-to-scale membership"). BR has no reverse
  # items, so no reverse-keying is applied.
  detach_alpha <- calc_alpha(di[c(7, 12, 30, 31, 37)])
  idet <- which(hitopbr_scales$camelCase == "detachment")
  expect_equal(rel$alpha[idet], detach_alpha)

  int_alpha <- calc_alpha(di[c(8, 9, 18, 22, 23, 36, 42, 44)])
  iint <- which(hitopbr_scales$camelCase == "internalizing")
  expect_equal(rel$alpha[iint], int_alpha)
})

test_that("reliability_hitopsr() reverse-keys before estimating (independent recompute)", {
  # romanticDisinterest = 42,152,187,310,338; item 310 is the lone reverse item
  # (srange c(1,4) -> reverse(x) = 5 - x). Recompute its alpha by hand and check
  # the family reverse-keys 310 before estimating.
  d <- sim_hitopsr
  items <- 1:405
  rd <- c(42, 152, 187, 310, 338)
  di <- as.data.frame(lapply(d[items], as.numeric))
  di[[310]] <- 5 - di[[310]]
  exp_alpha <- calc_alpha(di[rd])

  rel <- reliability_hitopsr(d, items = items, omega = FALSE)
  idx <- which(hitopsr_scales$camelCase == "romanticDisinterest")
  expect_equal(rel$alpha[idx], exp_alpha)
  expect_equal(rel$nItems[idx], 5L)
})

test_that("reliability_pid5(FULL) reverse-keys before estimating (independent recompute)", {
  d <- sim_pid5
  items <- 1:220
  rev_items <- c(7, 30, 35, 58, 87, 90, 96, 97, 98, 131, 142, 155, 164, 177, 210, 215)
  di <- as.data.frame(lapply(d[items], as.numeric))
  for (i in rev_items) di[[i]] <- 3 - di[[i]]        # reverse-key, range c(0, 3)
  anhedonia <- c(1, 23, 26, 30, 124, 155, 157, 189)  # official Anhedonia items (30,155 reversed)
  exp_alpha <- calc_alpha(di[anhedonia])

  rel <- reliability_pid5(d, items = items, version = "FULL", omega = FALSE)
  idx <- which(pid_scales[["FULL"]]$camelCase == "anhedonia")
  expect_equal(rel$alpha[idx], exp_alpha)
})

test_that("reliability_*() return a per-scale tibble with the requested columns", {
  rel <- reliability_hitopbr(sim_hitopbr, items = 1:45, omega = FALSE)
  expect_s3_class(rel, "tbl_df")
  expect_equal(nrow(rel), nrow(hitopbr_scales))
  expect_identical(names(rel), c("Scale", "camelCase", "nItems", "alpha"))  # omega omitted when FALSE

  # PID-5 reliability is facet-level (25), before FULL/SF domain aggregation.
  rel_pid <- reliability_pid5(sim_pid5, items = 1:220, version = "FULL", omega = FALSE)
  expect_equal(nrow(rel_pid), 25L)
  # BF is 5 domains + the 25-item total (M026), so 6 rows.
  rel_bf <- reliability_pid5(sim_pid5bf, items = 1:25, version = "BF", omega = FALSE)
  expect_equal(nrow(rel_bf), 6L)
  expect_true("Total" %in% rel_bf$Scale)
  expect_equal(rel_bf$nItems[rel_bf$Scale == "Total"], 25L)
})

test_that("reliability_pid5(BFPM) fits no 2-item omega and raises no warning", {
  skip_if_not_installed("lavaan")
  # Each domain's 6 items share one factor, so the 6 domain models fit cleanly
  # and any warning would come from a 2-item facet model.
  set.seed(157)
  n <- 300
  domain_of_item <- integer(36)
  for (d in seq_len(nrow(pid_bfpm_domains))) {
    facet_items <- unlist(pid_scales$BFPM$itemNumbers[pid_bfpm_domains$facetStems[[d]]])
    domain_of_item[facet_items] <- d
  }
  latent <- matrix(stats::rnorm(n * 6), n, 6)
  raw <- latent[, domain_of_item] + matrix(stats::rnorm(n * 36, sd = 0.7), n, 36)
  dat <- as.data.frame(matrix(pmin(3, pmax(0, round(1.5 + raw))), n, 36))
  names(dat) <- sprintf("pid5bfpm_%02d", 1:36)

  expect_no_warning(
    rel <- reliability_pid5(dat, items = names(dat), version = "BFPM")
  )
  expect_identical(rel$nItems, c(rep(2L, 18), rep(6L, 6)))
  expect_true(all(is.na(rel$omega[1:18])))
  expect_false(anyNA(rel$omega[19:24]))
  expect_false(anyNA(rel$alpha))
})

test_that("reliability alpha is NA-safe on a zero-variance scale (no abort)", {
  const <- as.data.frame(matrix(2L, nrow = 10, ncol = 45))
  names(const) <- paste0("HBR_", seq_len(45))
  rel <- reliability_hitopbr(const, items = 1:45, omega = FALSE)
  expect_true(all(is.na(rel$alpha)))  # calc_alpha errors on zero variance -> NA
})

test_that("reliability omega is NA-safe on a zero-variance scale (no abort)", {
  skip_if_not_installed("lavaan")
  const <- as.data.frame(matrix(2L, nrow = 10, ncol = 45))
  names(const) <- paste0("HBR_", seq_len(45))
  expect_no_error(
    rel <- suppressWarnings(reliability_hitopbr(const, items = 1:45, alpha = FALSE))
  )
  expect_true(all(is.na(rel$omega)))
})

# --- module reliability (M037) -----------------------------------------------

test_that("reliability_hitopsr(module=) returns one row per module scale", {
  s <- hitop_module(
    "hitopsr",
    c("romanticDisinterest", "appetiteLoss", "agoraphobia", "antisocialBehavior")
  )
  part <- reliability_hitopsr(
    sim_hitopsr[s$items], items = seq_len(s$nItems),
    module = s, omega = FALSE
  )

  # Row order follows hitopsr_scales, not the order the scales were named.
  expect_equal(
    part$Scale,
    c("Agoraphobia", "Antisocial Behavior", "Appetite Loss", "Romantic Disinterest")
  )
  # nItems comes from the remapped positions; it must still match the table.
  expect_equal(
    part$nItems,
    hitopsr_scales$nItems[match(part$Scale, hitopsr_scales$Scale)]
  )
})

test_that("reliability_hitopsr(module=) gives the full run's alpha for its scales", {
  # Alpha is computed within a scale from its own reverse-keyed items, so
  # dropping the other 71 scales' columns cannot move it.
  s <- hitop_module("hitopsr", c("romanticDisinterest", "agoraphobia"))
  full <- reliability_hitopsr(sim_hitopsr, items = 1:405, omega = FALSE)
  part <- reliability_hitopsr(
    sim_hitopsr[s$items], items = seq_len(s$nItems), module = s, omega = FALSE
  )
  expect_equal(part, full[match(part$Scale, full$Scale), ], ignore_attr = "row.names")
})

test_that("a non-module, an unbuildable module and a wrong item count each blame the exported wrapper", {
  s <- hitop_module("hitopsr", "agoraphobia")
  dat <- sim_hitopsr[s$items]

  # Substituting a placeholder keeps each assertion independently reportable:
  # rlang::call_name(NULL) errors rather than failing as an expectation.
  blamed <- function(expr) {
    cnd <- rlang::catch_cnd(expr, classes = "error")
    list(
      fn = rlang::call_name(if (is.null(cnd$call)) quote(no_call()) else cnd$call),
      msg = conditionMessage(cnd)
    )
  }

  # (1) not a hitop_module at all
  bad <- blamed(score_hitopsr(dat, items = seq_len(s$nItems), module = list(items = 1)))
  expect_equal(bad$fn, "score_hitopsr")
  expect_match(bad$msg, "hitop_module")

  # (2) a hand-assembled descriptor naming another instrument; hitop_module()
  #     itself will not build one, so it is refused as a module its scales
  #     cannot rebuild.
  foreign <- s
  foreign$instrument <- "hitopbr"
  bad2 <- blamed(score_hitopsr(dat, items = seq_len(s$nItems), module = foreign))
  expect_equal(bad2$fn, "score_hitopsr")
  expect_match(bad2$msg, "Cannot rebuild")

  # (3) the existing length check, now re-pointed at the module's item count:
  #     the full 405 columns are the wrong input for a 5-item short form.
  bad3 <- blamed(score_hitopsr(sim_hitopsr, items = 1:405, module = s))
  expect_equal(bad3$fn, "score_hitopsr")
  expect_match(bad3$msg, "items")
  expect_match(bad3$msg, "Expected 5 items but got 405")

  # All three reach reliability_hitopsr() through the same helper, and must
  # blame it rather than score_hitopsr() or the internal engine.
  expect_equal(
    blamed(reliability_hitopsr(dat, items = seq_len(s$nItems), module = list(x = 1)))$fn,
    "reliability_hitopsr"
  )
  expect_equal(
    blamed(reliability_hitopsr(dat, items = seq_len(s$nItems), module = foreign))$fn,
    "reliability_hitopsr"
  )
  expect_equal(
    blamed(reliability_hitopsr(sim_hitopsr, items = 1:405, module = s))$fn,
    "reliability_hitopsr"
  )
})

test_that("the descriptor-consistency error also blames the exported wrapper", {
  s <- hitop_module("hitopsr", "agoraphobia")
  bad <- s
  bad$nItems <- 405L

  blamed <- function(expr) {
    cnd <- rlang::catch_cnd(expr, classes = "error")
    rlang::call_name(if (is.null(cnd$call)) quote(no_call()) else cnd$call)
  }

  expect_equal(
    blamed(score_hitopsr(sim_hitopsr, items = 1:405, module = bad)),
    "score_hitopsr"
  )
  expect_equal(
    blamed(reliability_hitopsr(sim_hitopsr, items = 1:405, module = bad)),
    "reliability_hitopsr"
  )
})

# --- subscale reliability (M141) ---------------------------------------------
# subscale_key and subscale_parent live in helper-fixtures.R.

test_that("include_subscales = TRUE adds one row per subscale after the scale rows", {
  base <- reliability_hitopsr(sim_hitopsr, items = 1:405, omega = FALSE)
  rel <- reliability_hitopsr(sim_hitopsr, items = 1:405, omega = FALSE,
                             include_subscales = TRUE)

  # Default: the 76 scale rows only.
  expect_equal(base$Scale, hitopsr_scales$Scale)
  # With TRUE: the same scale rows, then the subscale rows in table order.
  n <- nrow(hitopsr_scales)
  expect_equal(nrow(rel), n + nrow(hitopsr_subscales))
  expect_equal(rel[seq_len(n), ], base)
  subs <- rel[-seq_len(n), ]
  expect_equal(subs$Scale, hitopsr_subscales$Subscale)
  expect_equal(subs$camelCase, hitopsr_subscales$camelCase)
  # expect_identical(), not expect_equal(): nItems must stay an integer.
  expect_identical(subs$nItems, hitopsr_subscales$nItems)
  expect_equal(subs$camelCase, names(subscale_key))

  # Alpha for every subscale equals calc_alpha() on the items the hardcoded
  # numbers select. No subscale holds the one reverse-keyed item (HSR 310).
  stopifnot(!310 %in% unlist(subscale_key))
  di <- as.data.frame(lapply(sim_hitopsr[1:405], as.numeric))
  exp_alpha <- vapply(subscale_key, function(x) calc_alpha(di[x]), numeric(1))
  expect_equal(subs$alpha, unname(exp_alpha))
})

test_that("under a module, include_subscales adds the rows of its scales' subscales", {
  full <- reliability_hitopsr(sim_hitopsr, items = 1:405, omega = FALSE,
                              include_subscales = TRUE)
  probes <- list(
    list(scales = c("mistrust", "dishonesty", "agoraphobia"),
         subs = c(subscale_parent$dishonesty, subscale_parent$mistrust)),
    list(scales = "emotionality", subs = subscale_parent$emotionality),
    list(scales = c("agoraphobia", "appetiteLoss"), subs = character(0))
  )
  for (p in probes) {
    m <- hitop_module("hitopsr", p$scales)
    part <- reliability_hitopsr(sim_hitopsr[m$items], items = seq_len(m$nItems),
                                module = m, omega = FALSE,
                                include_subscales = TRUE)
    n <- length(m$camelCase)
    expect_equal(part$camelCase[seq_len(n)],
                 hitopsr_scales$camelCase[hitopsr_scales$camelCase %in% m$camelCase])
    expect_setequal(part$camelCase[-seq_len(n)], p$subs)
    expect_equal(part, full[match(part$camelCase, full$camelCase), ],
                 ignore_attr = "row.names", info = toString(p$scales))
  }
})

test_that("reliability_hitopsr() refuses an include_subscales that is not TRUE or FALSE", {
  # 1 is the value that once got past an isTRUE() check.
  for (bad in list(NA, "yes", c(TRUE, TRUE), 1)) {
    expect_error(
      reliability_hitopsr(sim_hitopsr, items = 1:405, omega = FALSE,
                          include_subscales = bad),
      "include_subscales.*must be",
      class = "rlang_error"
    )
  }
})
