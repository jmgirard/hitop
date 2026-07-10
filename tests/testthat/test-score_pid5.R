# Ground-truth oracle tests for score_pid5(). Expected values are hand-computed
# in helper-fixtures.R from the published PID-5 keys, never read from the code.
#
# score_pid5() outputs 25 facets + 5 domains for FULL/SF (M7) and 5 domains for
# BF. FULL/SF domains average the 3 primary facets of each domain (APA Step 3);
# the primary-facet map (`pid_domains`) is verified against the APA source in
# test-keying.R. The BF 5-domain structure is verified there too (M6).

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

# ---- FULL/SF domains (M7) ---------------------------------------------------
# Domain = mean of its 3 PRIMARY facet average scores (APA Step 3). The facet
# values used below are the hand-computed fixture facets from helper-fixtures.R.

test_that("FULL domain scores match hand-computed fixture values", {
  f <- score_pid5(fx_pid5(), items = 1:220, version = "FULL", append = FALSE)

  # R1 (all 0; reverse items -> 3):
  #   detachment = mean(withdrawal 0, anhedonia 0.75, intimacyAvoidance 0.5) = 1.25/3
  expect_equal(f$pid_detachment[1], (0 + 0.75 + 0.5) / 3)
  #   negativeAffectivity = mean(emotionalLability 0, anxiousness 1/3, separationInsecurity 0)
  expect_equal(f$pid_negativeAffectivity[1], (0 + 1 / 3 + 0) / 3)

  # R2 (all 1):
  #   detachment = mean(withdrawal 1, anhedonia 1.25, intimacyAvoidance 7/6) = 41/36
  expect_equal(f$pid_detachment[2], (1 + 1.25 + 7 / 6) / 3)
  #   negativeAffectivity = mean(1, anxiousness 10/9, 1) = 28/27
  expect_equal(f$pid_negativeAffectivity[2], (1 + 10 / 9 + 1) / 3)
})

test_that("FULL gains 5 domain columns named like BF, appended after the 25 facets", {
  f <- score_pid5(fx_pid5(), items = 1:220, version = "FULL", append = FALSE)
  expect_equal(ncol(f), 30L)                                  # 25 facets + 5 domains
  expect_equal(tail(names(f), 5), paste0("pid_", pid_domains$camelCase))
  expect_setequal(
    paste0("pid_", pid_domains$camelCase),
    paste0("pid_", pid_scales[["BF"]]$camelCase)
  )
})

test_that("FULL domain = mean of its 3 primary facet columns (independent recompute)", {
  # Facet stems copied from the APA Domain Table, NOT read from pid_domains.
  f <- score_pid5(fx_pid5(), items = 1:220, version = "FULL", append = FALSE)
  psycho <- c("pid_unusualBeliefsExperiences", "pid_eccentricity", "pid_perceptualDysregulation")
  antag  <- c("pid_manipulativeness", "pid_deceitfulness", "pid_grandiosity")
  expect_equal(f$pid_psychoticism, rowMeans(as.matrix(f[, psycho])))
  expect_equal(f$pid_antagonism, rowMeans(as.matrix(f[, antag])))
})

test_that("FULL domains honor na.rm (a fully-missing facet drops from its domain)", {
  x <- fx_pid5()
  anhedonia <- c(1, 23, 26, 30, 124, 155, 157, 189)  # official Anhedonia items
  x[1, anhedonia] <- NA_integer_                      # wipe the whole facet on R1
  f_narm <- score_pid5(x, items = 1:220, version = "FULL", na.rm = TRUE, append = FALSE)
  f_nona <- score_pid5(x, items = 1:220, version = "FULL", na.rm = FALSE, append = FALSE)

  # na.rm = TRUE: anhedonia is NA and drops; detachment averages the other 2 facets.
  expect_true(is.na(f_narm$pid_anhedonia[1]))
  expect_equal(
    f_narm$pid_detachment[1],
    mean(c(f_narm$pid_withdrawal[1], f_narm$pid_intimacyAvoidance[1]))
  )
  # na.rm = FALSE: a contributing facet is NA, so the domain is NA.
  expect_true(is.na(f_nona$pid_detachment[1]))
})

test_that("SF domain scores match hand-computed fixture values", {
  f <- score_pid5(fx_pid5sf(), items = 1:100, version = "SF", append = FALSE)
  domain_cols <- paste0("pid_", pid_domains$camelCase)

  expect_true(all(f[1, domain_cols] == 0))   # R1 all 0 -> every domain 0
  expect_true(all(f[2, domain_cols] == 2))   # R2 all 2 -> every domain 2
  # R3: anhedonia = 1.5; other Detachment facets (withdrawal, intimacyAvoidance) = 1
  expect_equal(f$pid_detachment[3], (1 + 1.5 + 1) / 3)
  expect_equal(f$pid_negativeAffectivity[3], 1)  # untouched in R3
})

test_that("SF gains the same 5 domain columns as FULL", {
  f <- score_pid5(fx_pid5sf(), items = 1:100, version = "SF", append = FALSE)
  expect_equal(ncol(f), 30L)
  expect_true(all(paste0("pid_", pid_domains$camelCase) %in% names(f)))
})

test_that("BF output is unchanged by M7 (still 5 domains, no facet columns)", {
  d <- score_pid5(fx_pid5bf(), items = 1:25, version = "BF", append = FALSE)
  expect_equal(ncol(d), 5L)
  expect_setequal(names(d), paste0("pid_", pid_scales[["BF"]]$camelCase))
})

test_that("domain _se columns appear iff calc_se and derive from the 3 facet scores", {
  f0 <- score_pid5(fx_pid5(), items = 1:220, version = "FULL", append = FALSE)
  expect_false(any(grepl("_se$", names(f0))))

  f <- score_pid5(fx_pid5(), items = 1:220, version = "FULL", calc_se = TRUE, append = FALSE)
  expect_true("pid_detachment_se" %in% names(f))
  # SEM of the 3 detachment facet scores on R2 = sd(facets)/sqrt(3)
  facets_r2 <- c(f$pid_withdrawal[2], f$pid_anhedonia[2], f$pid_intimacyAvoidance[2])
  expect_equal(f$pid_detachment_se[2], stats::sd(facets_r2) / sqrt(3))
})
