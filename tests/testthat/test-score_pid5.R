# Ground-truth oracle tests for score_pid5(). Expected values are hand-computed
# in helper-fixtures.R from the published PID-5 keys, never read from the code.
#
# score_pid5() outputs 25 facets for FULL/SF and 5 domains for BF. It does NOT
# output FULL/SF domains -- that feature and its domain->facet oracle live in
# M7 (see tests/testthat/test-keying.R:11). The BF 5-domain structure is
# verified against the APA source in M6 / test-keying.R.

# ---- FULL (220 items) -------------------------------------------------------

test_that("FULL facet scores match hand-computed fixture values", {
  f <- score_pid5(fx_pid5(), items = 1:220, version = "FULL", append = FALSE)

  # R1 (all 0): reverse-keyed items become 3.
  expect_equal(f$pid_anhedonia[1], 0.75)         # (6*0 + 2*3)/8
  expect_equal(f$pid_anxiousness[1], 1 / 3)      # (8*0 + 1*3)/9
  expect_equal(f$pid_intimacyAvoidance[1], 0.5)  # (5*0 + 1*3)/6
  expect_equal(f$pid_separationInsecurity[1], 0) # no reverse items
  expect_equal(f$pid_emotionalLability[1], 0)
  expect_equal(f$pid_withdrawal[1], 0)

  # R2 (all 1): reverse-keyed items become 2.
  expect_equal(f$pid_anhedonia[2], 1.25)         # (6*1 + 2*2)/8
  expect_equal(f$pid_anxiousness[2], 10 / 9)     # (8*1 + 1*2)/9
  expect_equal(f$pid_intimacyAvoidance[2], 7 / 6)# (5*1 + 1*2)/6
})

test_that("FULL independent recomputation from the official key matches", {
  # Deliberately dumb recomputation with item numbers copied from the key,
  # guarding pid_items against transcription errors.
  x <- fx_pid5()
  rev_items <- c(7, 30, 35, 58, 87, 90, 96, 97, 98, 131, 142, 155, 164, 177, 210, 215)
  xr <- x
  for (i in rev_items) xr[[i]] <- 3 - x[[i]]     # reverse-key, range c(0,3)

  anhedonia <- c(1, 23, 26, 30, 124, 155, 157, 189)  # official Anhedonia items
  f_anhedo_hand <- rowMeans(xr[, anhedonia], na.rm = TRUE)

  pkg <- score_pid5(x, items = 1:220, version = "FULL", append = FALSE)
  expect_equal(pkg$pid_anhedonia, f_anhedo_hand)
})

test_that("FULL applies reverse-keying (facet with a reverse item is nonzero on all-0 input)", {
  f <- score_pid5(fx_pid5(), items = 1:220, version = "FULL", append = FALSE)
  expect_gt(f$pid_anhedonia[1], 0)             # contains reverse items 30, 155
  expect_equal(f$pid_separationInsecurity[1], 0) # contains none
})

test_that("FULL scoring tolerates missing items via rowMeans(na.rm = TRUE)", {
  f <- score_pid5(fx_pid5(), items = 1:220, version = "FULL", append = FALSE)
  # R4 drops item 1 from Anhedonia: (5*1 + 2*2)/7 = 9/7
  expect_equal(f$pid_anhedonia[4], 9 / 7)
  expect_false(any(is.na(f$pid_anxiousness))) # Anxiousness has no missing items
})

# ---- SF (100 items) ---------------------------------------------------------

test_that("SF facet scores match hand-computed fixture values", {
  f <- score_pid5(fx_pid5sf(), items = 1:100, version = "SF", append = FALSE)

  # R1 all 0 -> everything 0 (confirms NO reverse-keying on the SF).
  expect_true(all(f[1, ] == 0))
  # R2 all 2 -> everything 2.
  expect_true(all(f[2, ] == 2))
  # R3 targeted facet membership.
  expect_equal(f$pid_anhedonia[3], 1.5)  # items 9,11,43,65 = 0,1,2,3 -> 6/4
  expect_equal(f$pid_grandiosity[3], 1)  # untouched
})

test_that("SF applies no reverse-keying", {
  # Every facet on the all-0 respondent is exactly 0; a reverse-keyed form would
  # push facets with reverse items above 0.
  f <- score_pid5(fx_pid5sf(), items = 1:100, version = "SF", append = FALSE)
  expect_true(all(f[1, ] == 0))
})

test_that("SF independent recomputation from the official key matches", {
  x <- fx_pid5sf()
  anhedo <- c(9, 11, 43, 65)      # official SF Anhedonia items
  withdr <- c(27, 52, 57, 84)     # Withdrawal
  f_anhedo_hand <- rowMeans(x[, anhedo], na.rm = TRUE)
  f_withdr_hand <- rowMeans(x[, withdr], na.rm = TRUE)

  pkg <- score_pid5(x, items = 1:100, version = "SF", append = FALSE)
  expect_equal(pkg$pid_anhedonia, f_anhedo_hand)
  expect_equal(pkg$pid_withdrawal, f_withdr_hand)
})

# ---- BF (25 items, domain scores) -------------------------------------------

test_that("BF domain scores match hand-computed fixture values", {
  d <- score_pid5(fx_pid5bf(), items = 1:25, version = "BF", append = FALSE)

  # R1 all 0 -> every domain 0 (confirms NO reverse-keying on the BF).
  expect_true(all(d[1, ] == 0))
  # R2 all 2 -> every domain 2.
  expect_true(all(d[2, ] == 2))
  # R3 targets Disinhibition (items 1,2,3,5,6 = 0,1,2,3,3 -> 9/5 = 1.8).
  expect_equal(d$pid_disinhibition[3], 1.8)
  expect_equal(d$pid_detachment[3], 1)          # untouched
  expect_equal(d$pid_antagonism[3], 1)          # untouched
})

test_that("BF applies no reverse-keying", {
  d <- score_pid5(fx_pid5bf(), items = 1:25, version = "BF", append = FALSE)
  expect_true(all(d[1, ] == 0))
})

test_that("BF independent recomputation from the APA Domain table matches", {
  x <- fx_pid5bf()
  # BF item numbers copied from the APA PID-5-BF Domain Scoring table.
  disinhib <- c(1, 2, 3, 5, 6)
  detach   <- c(4, 13, 14, 16, 18)
  d_disinhib_hand <- rowMeans(x[, disinhib], na.rm = TRUE)
  d_detach_hand   <- rowMeans(x[, detach], na.rm = TRUE)

  pkg <- score_pid5(x, items = 1:25, version = "BF", append = FALSE)
  expect_equal(pkg$pid_disinhibition, d_disinhib_hand)
  expect_equal(pkg$pid_detachment, d_detach_hand)
})

test_that("BF scoring tolerates missing items via rowMeans(na.rm = TRUE)", {
  d <- score_pid5(fx_pid5bf(), items = 1:25, version = "BF", append = FALSE)
  # R4 sets items 1:5 NA; each domain keeps >= 1 non-missing item at value 1.
  expect_false(anyNA(d[4, ]))
  expect_equal(d$pid_disinhibition[4], 1)  # only item 6 (=1) survives
})
