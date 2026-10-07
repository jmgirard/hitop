# Ground-truth oracle tests for score_pid5(). Expected values are hand-computed
# in helper-fixtures.R from the published PID-5 keys, never read from the code.
#
# score_pid5() outputs 25 facets + 5 domains for FULL/SF (M007) and IRF (M159)
# and 5 domains for BF. FULL/SF/IRF domains average the 3 primary facets of each
# domain (APA Step 3);
# the primary-facet map (`pid_domains`) is verified against the APA source in
# test-keying.R. The BF 5-domain structure is verified there too (M006).

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
  # guarding pid_items against transcription errors. Uses missing = "available"
  # so the hand rowMeans(na.rm = TRUE) oracle is the right comparison on the
  # missing row R4 too; the APA path is exercised in its own oracle tests below.
  x <- fx_pid5()
  rev_items <- c(7, 30, 35, 58, 87, 90, 96, 97, 98, 131, 142, 155, 164, 177, 210, 215)
  xr <- x
  for (i in rev_items) xr[[i]] <- 3 - x[[i]]     # reverse-key, range c(0,3)

  anhedonia <- c(1, 23, 26, 30, 124, 155, 157, 189)  # official Anhedonia items
  f_anhedo_hand <- rowMeans(xr[, anhedonia], na.rm = TRUE)

  pkg <- score_pid5(x, items = 1:220, version = "FULL", missing = "available", append = FALSE)
  expect_equal(pkg$pid_anhedonia, f_anhedo_hand)
})

test_that("FULL applies reverse-keying (facet with a reverse item is nonzero on all-0 input)", {
  f <- score_pid5(fx_pid5(), items = 1:220, version = "FULL", append = FALSE)
  expect_gt(f$pid_anhedonia[1], 0)             # contains reverse items 30, 155
  expect_equal(f$pid_separationInsecurity[1], 0) # contains none
})

test_that("FULL available-item scoring (missing = 'available') tolerates missing via rowMeans", {
  f <- score_pid5(fx_pid5(), items = 1:220, version = "FULL", missing = "available", append = FALSE)
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

  # missing = "available" keeps rowMeans as the correct oracle on the missing row.
  pkg <- score_pid5(x, items = 1:100, version = "SF", missing = "available", append = FALSE)
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

  # missing = "available" keeps rowMeans as the correct oracle on the missing row.
  pkg <- score_pid5(x, items = 1:25, version = "BF", missing = "available", append = FALSE)
  expect_equal(pkg$pid_disinhibition, d_disinhib_hand)
  expect_equal(pkg$pid_detachment, d_detach_hand)
})

test_that("BF missing-item handling differs by missing level", {
  # R4 sets items 1:5 NA. Disinhibition = items 1,2,3,5,6 -> 4 of 5 missing (80%).
  d_apa  <- score_pid5(fx_pid5bf(), items = 1:25, version = "BF", append = FALSE)
  d_trad <- score_pid5(fx_pid5bf(), items = 1:25, version = "BF", missing = "available", append = FALSE)

  # APA (default): >25% of Disinhibition items missing -> NA (not scored).
  expect_true(is.na(d_apa$pid_disinhibition[4]))
  # Traditional: rowMeans(na.rm = TRUE) averages the single surviving item 6 (=1).
  expect_equal(d_trad$pid_disinhibition[4], 1)
  expect_false(is.na(d_trad$pid_disinhibition[4]))
  # Detachment = items 4,13,14,16,18 -> only item 4 missing (1 of 5 = 20% <= 25%),
  # so APA prorates: round(4*5/4)/5 = 5/5 = 1 (matches traditional here).
  expect_equal(d_apa$pid_detachment[4], 1)
})

# ---- FULL/SF domains (M007) ---------------------------------------------------
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
  # BF's `total` row (M026) is not a domain and is excluded: the claim is that
  # FULL's 5 domain columns are named like BF's 5 DOMAIN columns.
  expect_setequal(
    paste0("pid_", pid_domains$camelCase),
    paste0("pid_", setdiff(pid_scales[["BF"]]$camelCase, "total"))
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

test_that("FULL domains honor missing = available vs complete", {
  x <- fx_pid5()
  anhedonia <- c(1, 23, 26, 30, 124, 155, 157, 189)  # official Anhedonia items
  x[1, anhedonia] <- NA_integer_                      # wipe the whole facet on R1
  f_narm <- score_pid5(x, items = 1:220, version = "FULL", missing = "available", append = FALSE)
  f_nona <- score_pid5(x, items = 1:220, version = "FULL", missing = "complete", append = FALSE)

  # missing = "available": anhedonia is NA and drops; detachment averages the other 2 facets.
  expect_true(is.na(f_narm$pid_anhedonia[1]))
  expect_equal(
    f_narm$pid_detachment[1],
    mean(c(f_narm$pid_withdrawal[1], f_narm$pid_intimacyAvoidance[1]))
  )
  # missing = "complete": a contributing facet is NA, so the domain is NA.
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

test_that("BF output is 5 domains + the total, and no facet columns", {
  d <- score_pid5(fx_pid5bf(), items = 1:25, version = "BF", append = FALSE)
  expect_equal(ncol(d), 6L)
  expect_setequal(names(d), paste0("pid_", pid_scales[["BF"]]$camelCase))
  # The total is appended after the five domains, never interleaved.
  expect_equal(names(d)[[6]], "pid_total")
})

test_that("BF total is the item-level mean over all 25 items (hand-computed oracle)", {
  # Markon et al. (2024, Ch. 3, p. 23): the BF total "can be computed by averaging
  # the overall score by the total number of items in the measure (i.e., 25)".
  # Expected values below are computed BY HAND from fx_pid5bf(), never from
  # score_pid5() (IP2). fx_pid5bf() rows:
  #   1: all 25 items = 0                      -> 0/25   = 0
  #   2: all 25 items = 2                      -> 50/25  = 2
  #   3: all 1, but items 1,2,3,5,6 = 0,1,2,3,3
  #      sum = 25 - (5 x 1) + (0+1+2+3+3) = 29 -> 29/25  = 1.16
  #   4: all 1, items 1:5 missing              -> see the proration case below
  d <- score_pid5(fx_pid5bf(), items = 1:25, version = "BF", append = FALSE)
  expect_equal(d$pid_total[1:3], c(0, 2, 1.16))

  # Row 4 exercises the APA rule at the 25-item level: 5 of 25 unanswered is 20%,
  # within the 25% tolerance, so the total prorates rather than dropping.
  # partial = 20 (20 answered items, each 1); prorated raw = 20 x 25/20 = 25;
  # total = 25/25 = 1.
  expect_equal(d$pid_total[[4]], 1)

  # And it prorates INDEPENDENTLY of the domains (M026 implementation gate): items
  # 1, 2, 3, 5 are all Disinhibition, so 4 of that domain's 5 items are missing
  # (80% > 25%) and the domain drops -- while the total above still computes.
  expect_true(is.na(d$pid_disinhibition[[4]]))

  # The two candidate rules D-017 left open coincide on complete data: with five
  # equal-sized domains, the mean of 25 items IS the mean of the 5 domain means.
  # They are only distinguishable under missingness, where the book's rule governs.
  complete <- d[1:3, ]
  domain_means <- rowMeans(
    complete[, paste0("pid_", setdiff(pid_scales[["BF"]]$camelCase, "total"))]
  )
  expect_equal(unname(domain_means), complete$pid_total)
})

test_that("BF total drops when more than a quarter of the 25 items are unanswered", {
  # apa_mean() drops at (25 - answered)/25 > 0.25, i.e. from 7 unanswered.
  x <- fx_pid5bf()
  x[3, 1:6] <- NA_integer_   # 6 unanswered: 24% -> still prorates
  x[4, 1:7] <- NA_integer_   # 7 unanswered: 28% -> drops
  d <- score_pid5(x, items = 1:25, version = "BF", missing = "apa", append = FALSE)
  expect_false(is.na(d$pid_total[[3]]))
  expect_true(is.na(d$pid_total[[4]]))
})

test_that("domain _se columns appear iff calc_se and derive from the 3 facet scores", {
  f0 <- score_pid5(fx_pid5(), items = 1:220, version = "FULL", append = FALSE)
  expect_false(any(grepl("_se$", names(f0))))

  f <- hush_se(score_pid5(fx_pid5(), items = 1:220, version = "FULL", calc_se = TRUE, append = FALSE))
  expect_true("pid_detachment_se" %in% names(f))
  # SEM of the 3 detachment facet scores on R2 = sd(facets)/sqrt(3)
  facets_r2 <- c(f$pid_withdrawal[2], f$pid_anhedonia[2], f$pid_intimacyAvoidance[2])
  expect_equal(f$pid_detachment_se[2], stats::sd(facets_r2) / sqrt(3))
})

# ---- APA missing-data / proration scoring (M008, default missing = "apa") ----
# APA full-form key (Krueger et al., 2013, p. 8), sourced verbatim in SOURCES.md:
#   > 25% of a facet's items unanswered -> facet NA ("should not be used").
#   <= 25% unanswered -> prorate: round(partial_raw * n_items / n_answered), then
#   average = prorated_raw / n_items. Domain NA if any of its 3 facets is NA.
# The BF key applies the same rule to its 5-item domains (M006). With no missing
# items APA and traditional scoring agree, so the completed-data fixture tests
# above already cover the no-missing case for both modes.

test_that("APA and traditional scoring agree when no items are missing", {
  # R1/R2 of every fixture are complete; proration with n_answered = n_items is
  # round(sum)/n = sum/n = the plain mean, so the two paths must coincide.
  for (v in c("FULL", "SF")) {
    n <- if (v == "FULL") 220 else 100
    fx <- if (v == "FULL") fx_pid5() else fx_pid5sf()
    apa  <- score_pid5(fx[1:2, ], items = 1:n, version = v, append = FALSE)
    trad <- score_pid5(fx[1:2, ], items = 1:n, version = v, missing = "available", append = FALSE)
    expect_equal(apa, trad)
  }
})

test_that("APA facet proration rounds the prorated raw (FULL Anhedonia, item 1 missing)", {
  # fx_pid5() R4: all items 1, items 1:22 NA. Anhedonia = 1,23,26,30R,124,155R,157,189
  # (n = 8); only item 1 missing (1/8 = 12.5% <= 25% -> prorate). Answered raw =
  # five 1s + two reverse(1)=2 = 9 over 7 answered. Prorated raw = round(9*8/7) =
  # round(10.2857) = 10; average = 10/8 = 1.25. Traditional rowMeans = 9/7.
  apa  <- score_pid5(fx_pid5(), items = 1:220, version = "FULL", append = FALSE)
  trad <- score_pid5(fx_pid5(), items = 1:220, version = "FULL", missing = "available", append = FALSE)
  expect_equal(apa$pid_anhedonia[4], 1.25)
  expect_equal(trad$pid_anhedonia[4], 9 / 7)
  expect_false(isTRUE(all.equal(apa$pid_anhedonia[4], trad$pid_anhedonia[4])))
})

test_that("APA drops a facet with more than 25% of items missing (FULL Impulsivity)", {
  # R4: Impulsivity = 4,16,17,22,58,204 (n = 6); items 4,16,17,22 in 1:22 ->
  # 4 of 6 missing (66.7% > 25%) -> NA. Traditional averages the 2 present items.
  apa  <- score_pid5(fx_pid5(), items = 1:220, version = "FULL", append = FALSE)
  trad <- score_pid5(fx_pid5(), items = 1:220, version = "FULL", missing = "available", append = FALSE)
  expect_true(is.na(apa$pid_impulsivity[4]))
  expect_false(is.na(trad$pid_impulsivity[4]))
})

test_that("APA keeps a facet at exactly 25% missing (prorated, not NA)", {
  # Wipe exactly 2 of Anhedonia's 8 items (25%, boundary is inclusive) on R2 (all 1).
  x <- fx_pid5()
  x[2, c(1, 23)] <- NA_integer_   # 2 of 8 Anhedonia items -> 25% missing
  apa <- score_pid5(x, items = 1:220, version = "FULL", append = FALSE)
  # 6 answered: items 26,124,157,189 = 1 (four 1s) and 30,155 reverse to 2 (two
  # 2s) => raw 8 over 6 answered. Prorated raw = round(8*8/6) = round(10.667) = 11;
  # average = 11/8 = 1.375.
  expect_false(is.na(apa$pid_anhedonia[2]))
  expect_equal(apa$pid_anhedonia[2], 11 / 8)
})

test_that("APA half-integer prorated raw rounds up (BF Disinhibition)", {
  # BF Disinhibition = 1,2,3,5,6. Item 1 NA; items 2,3,5,6 = 0,0,1,1 (sum = 2);
  # 1 of 5 missing (20% <= 25%). Prorated raw = round(2*5/4) = round(2.5). APA
  # rounds half UP -> 3, average = 3/5 = 0.6 (base round-half-to-even gives 0.4).
  b <- as.data.frame(matrix(1L, nrow = 1, ncol = 25))
  names(b) <- sprintf("pid5bf_%02d", 1:25)
  b[1, 1] <- NA_integer_
  b[1, c(2, 3, 5, 6)] <- c(0L, 0L, 1L, 1L)
  d <- score_pid5(b, items = 1:25, version = "BF", append = FALSE)
  expect_equal(d$pid_disinhibition, 0.6)
})

test_that("APA domain is NA when any one contributing facet is NA (FULL Disinhibition)", {
  # R4: Impulsivity (a primary Disinhibition facet) is NA (> 25% missing), so the
  # Disinhibition DOMAIN is NA even though Irresponsibility & Distractibility score.
  apa  <- score_pid5(fx_pid5(), items = 1:220, version = "FULL", append = FALSE)
  trad <- score_pid5(fx_pid5(), items = 1:220, version = "FULL", missing = "available", append = FALSE)
  expect_true(is.na(apa$pid_disinhibition[4]))
  expect_false(is.na(apa$pid_irresponsibility[4]))   # a computable sibling facet
  expect_false(is.na(trad$pid_disinhibition[4]))     # traditional averages what it has
})

test_that("APA SE is NA wherever the scale score is NA", {
  se <- hush_se(score_pid5(fx_pid5(), items = 1:220, version = "FULL", calc_se = TRUE, append = FALSE))
  # R4 Impulsivity facet and Disinhibition domain are NA -> their _se must be NA.
  expect_true(is.na(se$pid_impulsivity[4]))
  expect_true(is.na(se$pid_impulsivity_se[4]))
  expect_true(is.na(se$pid_disinhibition[4]))
  expect_true(is.na(se$pid_disinhibition_se[4]))
  # A computable facet keeps a non-NA SE.
  expect_false(is.na(se$pid_anhedonia_se[4]))
})

test_that("the three missing levels agree on complete data", {
  # Rows 1-3 of every fixture are complete; with no missing items apa proration
  # (n_answered = n_items) reduces to the plain mean, so all three levels coincide.
  for (v in c("FULL", "SF", "BF")) {
    n <- switch(v, FULL = 220, SF = 100, BF = 25)
    fx <- switch(v, FULL = fx_pid5(), SF = fx_pid5sf(), BF = fx_pid5bf())[1:3, ]
    apa   <- score_pid5(fx, items = 1:n, version = v, missing = "apa", append = FALSE)
    avail <- score_pid5(fx, items = 1:n, version = v, missing = "available", append = FALSE)
    comp  <- score_pid5(fx, items = 1:n, version = v, missing = "complete", append = FALSE)
    expect_equal(apa, avail)
    expect_equal(avail, comp)
  }
})

test_that("missing = complete returns NA for any scale touching a missing item", {
  # fx_pid5() R4 sets items 1:22 NA. Anhedonia includes item 1, so it is NA under
  # "complete" but computable under "available" (averages the surviving items).
  comp  <- score_pid5(fx_pid5(), items = 1:220, version = "FULL", missing = "complete", append = FALSE)
  avail <- score_pid5(fx_pid5(), items = 1:220, version = "FULL", missing = "available", append = FALSE)
  expect_true(is.na(comp$pid_anhedonia[4]))
  expect_false(is.na(avail$pid_anhedonia[4]))
  # Anxiousness has no item in 1:22, so it is computable under both.
  expect_false(is.na(comp$pid_anxiousness[4]))
})

# ---- Single-row input with calc_se (regression for the M008 follow-up) ---------
# A one-row input drove the facet-SE `apply(data_items[, x], MARGIN = 1, ...)`
# to index a 1-row matrix down to a vector, so `apply(MARGIN = 1)` errored with
# "dim(X) must have a positive length". The fix adds `drop = FALSE` (mirroring
# the domain-SE path and validity_pid5's single-row fix from M003). The SE of a
# scale on one respondent is calc_sem over that scale's reverse-keyed item
# values: sd(items) / sqrt(k). The expected values below hardcode those item
# values from the official key (independent of the package's keying tables).

test_that("FULL single-row calc_se returns per-scale SEs (no drop-to-vector error)", {
  x1 <- as.data.frame(matrix(1L, nrow = 1, ncol = 220))
  names(x1) <- sprintf("pid5_%03d", seq_len(220))
  f <- hush_se(score_pid5(x1, items = 1:220, version = "FULL", calc_se = TRUE, append = FALSE))

  expect_equal(nrow(f), 1L)
  expect_true("pid_anhedonia_se" %in% names(f))
  expect_true("pid_detachment_se" %in% names(f))  # a domain SE column too

  # Anhedonia = items 1,23,26,30,124,155,157,189 (reverse 30,155). With every
  # raw item = 1, reverse(1) = 2, so the 8 keyed values are 1,1,1,2,1,2,1,1.
  anh <- c(1, 1, 1, 2, 1, 2, 1, 1)
  expect_equal(f$pid_anhedonia_se, stats::sd(anh) / sqrt(8))
  # No SE is NA (complete data, k >= 2 per facet).
  se_cols <- f[grepl("_se$", names(f))]
  expect_false(any(is.na(unlist(se_cols))))
})

test_that("SF single-row calc_se matches a hand-computed facet SE", {
  x1 <- as.data.frame(matrix(1L, nrow = 1, ncol = 100))
  names(x1) <- sprintf("pid5sf_%03d", seq_len(100))
  x1[1, c(9, 11, 43, 65)] <- c(0L, 1L, 2L, 3L)  # SF Anhedonia items (no reversal)
  f <- hush_se(score_pid5(x1, items = 1:100, version = "SF", calc_se = TRUE, append = FALSE))

  expect_equal(nrow(f), 1L)
  # SF Anhedonia = items 9,11,43,65 = 0,1,2,3 -> sd(c(0,1,2,3)) / sqrt(4).
  expect_equal(f$pid_anhedonia_se, stats::sd(c(0, 1, 2, 3)) / sqrt(4))
})

test_that("BF single-row calc_se matches a hand-computed domain SE", {
  x1 <- as.data.frame(matrix(1L, nrow = 1, ncol = 25))
  names(x1) <- sprintf("pid5bf_%02d", seq_len(25))
  x1[1, c(1, 2, 3, 5, 6)] <- c(0L, 1L, 2L, 3L, 3L)  # BF Disinhibition items
  d <- hush_se(score_pid5(x1, items = 1:25, version = "BF", calc_se = TRUE, append = FALSE))

  expect_equal(nrow(d), 1L)
  # BF Disinhibition = items 1,2,3,5,6 = 0,1,2,3,3 -> sd / sqrt(5).
  expect_equal(d$pid_disinhibition_se, stats::sd(c(0, 1, 2, 3, 3)) / sqrt(5))
})

test_that("single-row calc_se equals the first row of a multi-row score", {
  # Guards against the fix altering multi-row behavior: scoring one row alone
  # must reproduce that row's SEs from a multi-row call.
  multi <- hush_se(score_pid5(fx_pid5(), items = 1:220, version = "FULL",
                      calc_se = TRUE, missing = "available", append = FALSE))
  one <- hush_se(score_pid5(fx_pid5()[2, ], items = 1:220, version = "FULL",
                    calc_se = TRUE, missing = "available", append = FALSE))
  se_names <- grep("_se$", names(multi), value = TRUE)
  expect_equal(as.numeric(one[1, se_names]), as.numeric(multi[2, se_names]))
})

test_that("SF applies the APA rule the same way as FULL", {
  # fx_pid5sf() R5: items 1:10 NA. Submissiveness = 2 of 4 items missing (> 25%).
  apa  <- score_pid5(fx_pid5sf(), items = 1:100, version = "SF", append = FALSE)
  trad <- score_pid5(fx_pid5sf(), items = 1:100, version = "SF", missing = "available", append = FALSE)
  expect_true(is.na(apa$pid_submissiveness[5]))
  expect_false(is.na(trad$pid_submissiveness[5]))
})

test_that("BF total honors every `missing` mode (hand-computed)", {
  # F9 (M026 review): the total was covered only under missing = "apa". Expected
  # values below are computed BY HAND from fx_pid5bf(), never from score_pid5().
  #   rows 1-3 are complete; row 4 has items 1:5 unanswered (20 answered, all 1)
  #   row 3 = all 1 except items 1,2,3,5,6 = 0,1,2,3,3 -> sum 29
  x <- fx_pid5bf()

  # "available" averages whatever is present: row 4 = 20/20 = 1.
  av <- score_pid5(x, items = 1:25, version = "BF", missing = "available",
                   append = FALSE)
  expect_equal(av$pid_total, c(0, 2, 1.16, 1))

  # "complete" returns NA if any item is missing, so row 4 drops even though
  # only 5 of 25 are unanswered -- where "apa" prorates it to 1.
  cp <- score_pid5(x, items = 1:25, version = "BF", missing = "complete",
                   append = FALSE)
  expect_equal(cp$pid_total[1:3], c(0, 2, 1.16))
  expect_true(is.na(cp$pid_total[[4]]))

  # The three modes agree wherever nothing is missing (rows 1-3).
  ap <- score_pid5(x, items = 1:25, version = "BF", missing = "apa",
                   append = FALSE)
  expect_equal(ap$pid_total[1:3], av$pid_total[1:3])
  expect_equal(ap$pid_total[1:3], cp$pid_total[1:3])
})

test_that("BF total standard error is the SEM over its answered items", {
  # F9 (M026 review): six `_se` columns were counted but none checked by value.
  # Recomputed here from the fixture with base R, independent of calc_sem().
  x <- fx_pid5bf()
  d <- hush_se(score_pid5(x, items = 1:25, version = "BF", missing = "apa",
                  calc_se = TRUE, append = FALSE))

  # Rows 1, 2 and 4 have zero variance among answered items -> SE exactly 0.
  expect_equal(d$pid_total_se[c(1, 2, 4)], c(0, 0, 0))

  # Row 3 varies: sd over all 25 items / sqrt(25).
  row3 <- as.numeric(x[3, ])
  expect_equal(d$pid_total_se[[3]], stats::sd(row3) / sqrt(25))

  # Under APA proration the SE uses only the ANSWERED items (row 4 has 20).
  row4 <- as.numeric(x[4, ])
  answered <- row4[!is.na(row4)]
  expect_equal(d$pid_total_se[[4]],
               stats::sd(answered) / sqrt(length(answered)))

  # And an SE is NA wherever its score is NA (mask_se_na), including the total.
  y <- x
  y[4, 1:7] <- NA_integer_   # 7 unanswered -> total drops
  dy <- hush_se(score_pid5(y, items = 1:25, version = "BF", missing = "apa",
                   calc_se = TRUE, append = FALSE))
  expect_true(is.na(dy$pid_total[[4]]))
  expect_true(is.na(dy$pid_total_se[[4]]))
})

# ---- PID5BF+M (36 items) ----------------------------------------------------
# Expected values are worked out by hand in helper-fixtures.R (fx_pid5bfpm) and
# typed here, rows R1 to R5. Column order is the key sheet's: 18 facets grouped
# by domain, then the 6 domains (D-088(c)).

bfpm_available <- list(
  emotionalLability         = c(0, 3, 0.5, 1,   0.5),
  anxiousness               = c(0, 3, 1.5, 1.5, 1.5),
  separationInsecurity      = c(0, 3, 3,   3,   3),
  withdrawal                = c(0, 3, 0,   0,   0),
  anhedonia                 = c(0, 3, 1.5, 1.5, 1.5),
  intimacyAvoidance         = c(0, 3, 2,   2,   2),
  manipulativeness          = c(0, 3, 1,   1,   1),
  deceitfulness             = c(0, 3, 2.5, 2.5, 2.5),
  grandiosity               = c(0, 3, 0.5, 0.5, 0.5),
  irresponsibility          = c(0, 3, 2.5, 2.5, 2.5),
  impulsivity               = c(0, 3, 1,   1,   1),
  distractibility           = c(0, 3, 1.5, 1.5, 1.5),
  perfectionism             = c(0, 3, 2,   2,   2),
  rigidity                  = c(0, 3, 0.5, 0.5, 0),
  orderliness               = c(0, 3, 1,   1,   1),
  unusualBeliefsExperiences = c(0, 3, 2,   2,   2),
  eccentricity              = c(0, 3, 1.5, 1.5, 0),
  perceptualDysregulation   = c(0, 3, 3,   3,   3),
  negativeAffectivity       = c(0, 3, 5 / 3,  11 / 6, 5 / 3),
  detachment                = c(0, 3, 7 / 6,  7 / 6,  7 / 6),
  antagonism                = c(0, 3, 4 / 3,  4 / 3,  4 / 3),
  disinhibition             = c(0, 3, 5 / 3,  5 / 3,  5 / 3),
  anankastia                = c(0, 3, 7 / 6,  7 / 6,  1),
  psychoticism              = c(0, 3, 13 / 6, 13 / 6, 5 / 3)
)

# Under "apa" and "complete" a facet with 1 of its 2 items missing is NA, and
# so is its domain. Only these cells differ from "available".
bfpm_apa <- bfpm_available
bfpm_apa$emotionalLability   <- c(0, 3, 0.5,    NA,     0.5)
bfpm_apa$negativeAffectivity <- c(0, 3, 5 / 3,  NA,     5 / 3)
bfpm_apa$rigidity            <- c(0, 3, 0.5,    0.5,    NA)
bfpm_apa$eccentricity        <- c(0, 3, 1.5,    1.5,    NA)
bfpm_apa$anankastia          <- c(0, 3, 7 / 6,  7 / 6,  NA)
bfpm_apa$psychoticism        <- c(0, 3, 13 / 6, 13 / 6, NA)

test_that("BFPM output is 18 facets then 6 domains, in the key's order", {
  f <- score_pid5(fx_pid5bfpm(), items = 1:36, version = "BFPM", append = FALSE)
  expect_identical(names(f), paste0("pid_", names(bfpm_available)))
})

test_that("BFPM scores match hand-computed values under each missing mode", {
  x <- fx_pid5bfpm()
  expected <- list(available = bfpm_available, apa = bfpm_apa, complete = bfpm_apa)
  for (mode in names(expected)) {
    f <- score_pid5(x, items = 1:36, version = "BFPM", missing = mode, append = FALSE)
    for (nm in names(expected[[mode]])) {
      expect_equal(
        f[[paste0("pid_", nm)]],
        expected[[mode]][[nm]],
        info = paste(mode, nm)
      )
    }
  }
})

test_that("BFPM version is matched case-insensitively", {
  x <- fx_pid5bfpm()
  expect_identical(
    score_pid5(x, items = 1:36, version = "bfpm", append = FALSE),
    score_pid5(x, items = 1:36, version = "BFPM", append = FALSE)
  )
})

test_that("BFPM independent recomputation from the key sheet's item pairs", {
  # Deliberately dumb recomputation with BF+M item pairs copied from the key
  # sheet, p. 2: facet = mean of its 2 items, domain = mean of its 3 facets.
  x <- fx_pid5bfpm()
  pairs <- list(
    emotionalLability = c(1, 19), anxiousness = c(7, 25),
    separationInsecurity = c(13, 31), withdrawal = c(4, 22),
    anhedonia = c(10, 28), intimacyAvoidance = c(16, 34),
    manipulativeness = c(2, 20), deceitfulness = c(8, 26),
    grandiosity = c(14, 32), irresponsibility = c(3, 21),
    impulsivity = c(9, 27), distractibility = c(15, 33),
    perfectionism = c(6, 18), rigidity = c(12, 24), orderliness = c(30, 36),
    unusualBeliefsExperiences = c(5, 23), eccentricity = c(11, 29),
    perceptualDysregulation = c(17, 35)
  )
  domains <- list(
    negativeAffectivity = c("emotionalLability", "anxiousness", "separationInsecurity"),
    detachment = c("withdrawal", "anhedonia", "intimacyAvoidance"),
    antagonism = c("manipulativeness", "deceitfulness", "grandiosity"),
    disinhibition = c("irresponsibility", "impulsivity", "distractibility"),
    anankastia = c("perfectionism", "rigidity", "orderliness"),
    psychoticism = c("unusualBeliefsExperiences", "eccentricity", "perceptualDysregulation")
  )
  for (mode in c("available", "complete")) {
    narm <- mode == "available"
    facet <- lapply(pairs, function(p) rowMeans(x[, p], na.rm = narm))
    domain <- lapply(domains, function(d) rowMeans(as.data.frame(facet[d]), na.rm = narm))
    pkg <- score_pid5(x, items = 1:36, version = "BFPM", missing = mode, append = FALSE)
    for (nm in names(facet)) {
      expect_equal(pkg[[paste0("pid_", nm)]], facet[[nm]], info = paste(mode, nm))
    }
    for (nm in names(domain)) {
      expect_equal(pkg[[paste0("pid_", nm)]], domain[[nm]], info = paste(mode, nm))
    }
  }
})

test_that("BFPM 'apa' gives the same output as 'complete' on 2-item facets", {
  x <- fx_pid5bfpm()
  expect_identical(
    score_pid5(x, items = 1:36, version = "BFPM", missing = "apa", append = FALSE),
    score_pid5(x, items = 1:36, version = "BFPM", missing = "complete", append = FALSE)
  )
})

test_that("BFPM standard errors follow the facet and domain rules", {
  x <- fx_pid5bfpm()
  d <- hush_se(score_pid5(x, items = 1:36, version = "BFPM",
                          missing = "available", calc_se = TRUE, append = FALSE))
  # R3: Emotional Lability items (1, 19) = (0, 1): sd 0.7071 / sqrt(2) = 0.5.
  expect_equal(d$pid_emotionalLability_se[3], 0.5)
  # R3: negativeAffectivity facets 0.5, 1.5, 3 (mean 5/3): squared deviations
  # 49/36 + 1/36 + 64/36 = 114/36, variance 57/36, so SE = sqrt(57/36) / sqrt(3).
  expect_equal(d$pid_negativeAffectivity_se[3], sqrt(57 / 36) / sqrt(3))
  # R3: anankastia facets 2, 0.5, 1 (mean 7/6): squared deviations
  # 25/36 + 16/36 + 1/36 = 42/36, variance 21/36, so SE = sqrt(21/36) / sqrt(3).
  expect_equal(d$pid_anankastia_se[3], sqrt(21 / 36) / sqrt(3))
  # R4: Emotional Lability from item 19 alone has a score but no SE.
  expect_equal(d$pid_emotionalLability[4], 1)
  expect_true(is.na(d$pid_emotionalLability_se[4]))
})

test_that("version abbreviations 'B' and 'BFP' are refused, and 'bfpm' selects BFPM", {
  x <- fx_pid5bfpm()
  expect_error(score_pid5(x, items = 1:36, version = "B"), class = "hitop_unknown_version")
  expect_error(score_pid5(x, items = 1:36, version = "bfp"), class = "hitop_unknown_version")
  expect_identical(
    score_pid5(x, items = 1:36, version = "bfpm", append = FALSE),
    score_pid5(x, items = 1:36, version = "BFPM", append = FALSE)
  )
})

test_that("BFPM refuses a data frame with the wrong number of items", {
  x <- fx_pid5bfpm()
  expect_error(
    score_pid5(x, items = 1:35, version = "BFPM"),
    "Expected 36 items but got 35",
    fixed = TRUE
  )
})

# ---- PID-5 Informant Form (M159, AC2) ----------------------------------------
#
# Expected values for fx_pid5irf() (helper-fixtures.R), worked out from the APA
# IRF key and D-090: reverse the 14 R items as 3 - x (shown as "xR"); a facet
# with more than 25% of its items missing is NA; otherwise the prorated raw
# (partial sum * n / answered) is rounded to the nearest whole number, halves
# up, and divided by n; a domain is the mean of its 3 primary facets (the key's
# Domain Table), NA if any is NA. Each line lists the item values after
# reversal, in Facet Table order.
# anhedonia (items 1, 23, 26, 30, 123, 154, 156, 187)
#   R1: 1+3+2+1R+3+1R+0+3; 14/8
#   R2: 2+0+1+2R+0+2R+3+0; 10/8
#   R3: NA+NA+NA+1R+3+1R+0+3; 3 of 8 missing -> NA
#   R4: 1+3+2+1R+3+1R+0+3; 14/8
#   R5: 0+0+0+3R+0+3R+0+0; 6/8
# anxiousness (items 79, 93, 95, 108, 109, 129, 140, 173)
#   R1: 3+1+3+0+1+1+0+1; 10/8
#   R2: 0+2+0+3+2+2+3+2; 14/8
#   R3: 3+1+3+0+1+1+0+1; 10/8
#   R4: 3+1+3+0+1+1+0+1; 10/8
#   R5: 0+0+0+0+0+0+0+0; 0/8
# attentionSeeking (items 14, 43, 74, 110, 112, 172, 189, 209)
#   R1: 2+3+2+2+0+0+1+1; 11/8
#   R2: 1+0+1+1+3+3+2+2; 13/8
#   R3: 2+3+2+2+0+0+1+1; 11/8
#   R4: 2+3+2+2+0+0+1+1; 11/8
#   R5: 0+0+0+0+0+0+0+0; 0/8
# callousness (items 11, 13, 19, 54, 72, 73, 90, 152, 165, 181, 196, 198, 205, 206)
#   R1: 3+1+3+2+0+1+1R+0+1+1+0+2+1+2; 18/14
#   R2: 0+2+0+1+3+2+2R+3+2+2+3+1+2+1; 24/14
#   R3: 3+1+3+2+0+1+1R+0+1+1+0+2+1+2; 18/14
#   R4: NA+NA+NA+3+0+1+1R+0+1+1+0+2+1+2; prorated 12*14/11 = 15.27 -> 15; 15/14
#   R5: 0+0+0+0+0+0+3R+0+0+0+0+0+0+0; 3/14
# deceitfulness (items 41, 53, 56, 76, 125, 133, 141, 204, 212, 216)
#   R1: 1+1+0+0+1+1+2R+0+0+0; 6/10
#   R2: 2+2+3+3+2+2+1R+3+3+3; 24/10
#   R3: 1+1+0+0+1+1+2R+0+0+0; 6/10
#   R4: 1+1+0+0+1+1+2R+0+0+0; 6/10
#   R5: 0+0+0+0+0+0+3R+0+0+0; 3/10
# depressivity (items 27, 61, 66, 81, 86, 103, 118, 147, 150, 162, 167, 168, 176, 210)
#   R1: 3+1+2+1+2+3+2+3+2+2+3+0+0+2; 26/14
#   R2: 0+2+1+2+1+0+1+0+1+1+0+3+3+1; 16/14
#   R3: 3+1+2+1+2+3+2+3+2+2+3+0+0+2; 26/14
#   R4: 3+1+2+1+2+3+2+3+2+2+3+0+0+2; 26/14
#   R5: 0+0+0+0+0+0+0+0+0+0+0+0+0+0; 0/14
# distractibility (items 6, 29, 47, 68, 88, 117, 131, 143, 197)
#   R1: 2+1+3+0+0+1+3+3+1; 14/9
#   R2: 1+2+0+3+3+2+0+0+2; 13/9
#   R3: 2+1+3+0+0+1+3+3+1; 14/9
#   R4: NA+1+0+1+0+1+0+1+0; prorated 4*9/8 = 4.5 -> 5; 5/9
#   R5: 0+0+0+0+0+0+0+0+0; 0/9
# eccentricity (items 5, 21, 24, 25, 33, 52, 55, 70, 71, 151, 171, 183, 203)
#   R1: 1+1+0+1+1+0+3+2+3+3+3+3+3; 24/13
#   R2: 2+2+3+2+2+3+0+1+0+0+0+0+0; 15/13
#   R3: 1+1+0+1+1+0+3+2+3+3+3+3+3; 24/13
#   R4: 1+1+0+1+1+0+3+2+3+3+3+3+3; 24/13
#   R5: 0+0+0+0+0+0+0+0+0+0+0+0+0; 0/13
# emotionalLability (items 18, 62, 101, 121, 137, 164, 179)
#   R1: 2+2+1+1+1+0+3; 10/7
#   R2: 1+1+2+2+2+3+0; 11/7
#   R3: 2+2+1+1+1+0+3; 10/7
#   R4: 2+2+1+1+1+0+3; 10/7
#   R5: 0+0+0+0+0+0+0; 0/7
# grandiosity (items 40, 65, 113, 177, 185, 195)
#   R1: 0+1+1+1+1+3; 7/6
#   R2: 3+2+2+2+2+0; 11/6
#   R3: 0+1+1+1+1+3; 7/6
#   R4: 0+1+1+1+1+3; 7/6
#   R5: 0+0+0+0+0+0; 0/6
# hostility (items 28, 32, 38, 85, 92, 115, 157, 169, 186, 214)
#   R1: 0+0+2+1+0+3+1+1+2+2; 12/10
#   R2: 3+3+1+2+3+0+2+2+1+1; 18/10
#   R3: 0+0+2+1+0+3+1+1+2+2; 12/10
#   R4: 0+0+2+1+0+3+1+1+2+2; 12/10
#   R5: 0+0+0+0+0+0+0+0+0+0; 0/10
# impulsivity (items 4, 16, 17, 22, 58, 202)
#   R1: 0+0+1+2+1R+2; 6/6
#   R2: 3+3+2+1+2R+1; 12/6
#   R3: 0+0+1+2+1R+2; 6/6
#   R4: 0+0+1+2+1R+2; 6/6
#   R5: 0+0+0+0+3R+0; 3/6
# intimacyAvoidance (items 89, 96, 107, 119, 144, 201)
#   R1: 1+3R+3+3+0+1; 11/6
#   R2: 2+0R+0+0+3+2; 7/6
#   R3: 1+3R+3+3+0+1; 11/6
#   R4: 1+3R+3+3+0+1; 11/6
#   R5: 0+3R+0+0+0+0; 3/6
# irresponsibility (items 31, 128, 155, 159, 170, 199, 208)
#   R1: 3+0+3+3+2+3+3R; 17/7
#   R2: 0+3+0+0+1+0+0R; 4/7
#   R3: 3+0+3+3+2+3+3R; 17/7
#   R4: 3+0+3+3+2+3+3R; 17/7
#   R5: 0+0+0+0+0+0+3R; 3/7
# manipulativeness (items 106, 124, 161, 178, 217)
#   R1: 2+0+1+2+1; 6/5
#   R2: 1+3+2+1+2; 9/5
#   R3: 2+0+1+2+1; 6/5
#   R4: 2+0+1+2+1; 6/5
#   R5: 0+0+0+0+0; 0/5
# perceptualDysregulation (items 36, 37, 42, 44, 59, 77, 83, 153, 190, 191, 211, 215)
#   R1: 0+1+2+0+3+1+3+1+2+3+3+3; 22/12
#   R2: 3+2+1+3+0+2+0+2+1+0+0+0; 14/12
#   R3: 0+1+2+0+3+1+3+1+2+3+3+3; 22/12
#   R4: 0+1+2+0+3+1+3+1+2+3+3+3; 22/12
#   R5: 0+0+0+0+0+0+0+0+0+0+0+0; 0/12
# perseveration (items 46, 51, 60, 78, 80, 99, 120, 127, 136)
#   R1: 2+3+0+2+0+3+0+3+0; 13/9
#   R2: 1+0+3+1+3+0+3+0+3; 14/9
#   R3: 2+3+0+2+0+3+0+3+0; 13/9
#   R4: 2+3+0+2+0+3+0+3+0; 13/9
#   R5: 0+0+0+0+0+0+0+0+0; 0/9
# restrictedAffectivity (items 8, 45, 84, 91, 100, 166, 182)
#   R1: 0+1+0+3+0+2+2; 8/7
#   R2: 3+2+3+0+3+1+1; 13/7
#   R3: 0+1+0+3+0+2+2; 8/7
#   R4: 0+1+0+3+0+2+2; 8/7
#   R5: 0+0+0+0+0+0+0; 0/7
# rigidPerfectionism (items 34, 49, 104, 114, 122, 134, 139, 175, 194, 218)
#   R1: 2+1+0+2+2+2+3+3+2+2; 19/10
#   R2: 1+2+3+1+1+1+0+0+1+1; 11/10
#   R3: 2+1+0+2+2+2+3+3+2+2; 19/10
#   R4: 2+1+0+2+2+2+3+3+2+2; 19/10
#   R5: 0+0+0+0+0+0+0+0+0+0; 0/10
# riskTaking (items 3, 7, 35, 39, 48, 67, 69, 87, 97, 111, 158, 163, 193, 213)
#   R1: 3+0R+0R+3+0+3+1+0R+2R+3+2+0R+1+2R; 20/14
#   R2: 0+3R+3R+0+3+0+2+3R+1R+0+1+3R+2+1R; 22/14
#   R3: 3+0R+0R+3+0+3+1+0R+2R+3+2+0R+1+2R; 20/14
#   R4: 3+0R+0R+3+0+3+1+0R+2R+3+2+0R+1+2R; 20/14
#   R5: 0+3R+3R+0+0+0+0+3R+3R+0+0+3R+0+3R; 18/14
# separationInsecurity (items 12, 50, 57, 64, 126, 148, 174)
#   R1: 0+2+1+0+2+0+2; 7/7
#   R2: 3+1+2+3+1+3+1; 14/7
#   R3: 0+2+1+0+2+0+2; 7/7
#   R4: 0+2+1+0+2+0+2; 7/7
#   R5: 0+0+0+0+0+0+0; 0/7
# submissiveness (items 9, 15, 63, 200)
#   R1: 1+3+3+0; 7/4
#   R2: 2+0+0+3; 5/4
#   R3: NA+3+3+0; prorated 6*4/3 = 8 -> 8; 8/4
#   R4: 1+3+3+0; 7/4
#   R5: 0+0+0+0; 0/4
# suspiciousness (items 2, 102, 116, 130, 132, 188)
#   R1: 2+2+0+1R+0+0; 5/6
#   R2: 1+1+3+2R+3+3; 13/6
#   R3: 2+2+0+1R+0+0; 5/6
#   R4: 2+2+0+1R+0+0; 5/6
#   R5: 0+0+0+3R+0+0; 3/6
# unusualBeliefsExperiences (items 94, 98, 105, 138, 142, 149, 192, 207)
#   R1: 2+2+1+2+2+1+0+3; 13/8
#   R2: 1+1+2+1+1+2+3+0; 11/8
#   R3: 2+2+1+2+2+1+0+3; 13/8
#   R4: 2+2+1+2+2+1+0+3; 13/8
#   R5: 0+0+0+0+0+0+0+0; 0/8
# withdrawal (items 10, 20, 75, 82, 135, 145, 146, 160, 180, 184)
#   R1: 2+0+3+2+3+1+2+0+0+0; 13/10
#   R2: 1+3+0+1+0+2+1+3+3+3; 17/10
#   R3: 2+0+3+2+3+1+2+0+0+0; 13/10
#   R4: 2+0+3+2+3+1+2+0+0+0; 13/10
#   R5: 0+0+0+0+0+0+0+0+0+0; 0/10
# negativeAffectivity = mean of emotionalLability, anxiousness, separationInsecurity
#   R1: (10/7 + 5/4 + 1)/3
#   R2: (11/7 + 7/4 + 2)/3
#   R3: (10/7 + 5/4 + 1)/3
#   R4: (10/7 + 5/4 + 1)/3
#   R5: (0 + 0 + 0)/3
# detachment = mean of withdrawal, anhedonia, intimacyAvoidance
#   R1: (13/10 + 7/4 + 11/6)/3
#   R2: (17/10 + 5/4 + 7/6)/3
#   R3: NA (a facet is NA)
#   R4: (13/10 + 7/4 + 11/6)/3
#   R5: (0 + 3/4 + 1/2)/3
# antagonism = mean of manipulativeness, deceitfulness, grandiosity
#   R1: (6/5 + 3/5 + 7/6)/3
#   R2: (9/5 + 12/5 + 11/6)/3
#   R3: (6/5 + 3/5 + 7/6)/3
#   R4: (6/5 + 3/5 + 7/6)/3
#   R5: (0 + 3/10 + 0)/3
# disinhibition = mean of irresponsibility, impulsivity, distractibility
#   R1: (17/7 + 1 + 14/9)/3
#   R2: (4/7 + 2 + 13/9)/3
#   R3: (17/7 + 1 + 14/9)/3
#   R4: (17/7 + 1 + 5/9)/3
#   R5: (3/7 + 1/2 + 0)/3
# psychoticism = mean of unusualBeliefsExperiences, eccentricity, perceptualDysregulation
#   R1: (13/8 + 24/13 + 11/6)/3
#   R2: (11/8 + 15/13 + 7/6)/3
#   R3: (13/8 + 24/13 + 11/6)/3
#   R4: (13/8 + 24/13 + 11/6)/3
#   R5: (0 + 0 + 0)/3
irf_expected <- list(
  anhedonia = c(7 / 4, 5 / 4, NA, 7 / 4, 3 / 4),
  anxiousness = c(5 / 4, 7 / 4, 5 / 4, 5 / 4, 0),
  attentionSeeking = c(11 / 8, 13 / 8, 11 / 8, 11 / 8, 0),
  callousness = c(9 / 7, 12 / 7, 9 / 7, 15 / 14, 3 / 14),
  deceitfulness = c(3 / 5, 12 / 5, 3 / 5, 3 / 5, 3 / 10),
  depressivity = c(13 / 7, 8 / 7, 13 / 7, 13 / 7, 0),
  distractibility = c(14 / 9, 13 / 9, 14 / 9, 5 / 9, 0),
  eccentricity = c(24 / 13, 15 / 13, 24 / 13, 24 / 13, 0),
  emotionalLability = c(10 / 7, 11 / 7, 10 / 7, 10 / 7, 0),
  grandiosity = c(7 / 6, 11 / 6, 7 / 6, 7 / 6, 0),
  hostility = c(6 / 5, 9 / 5, 6 / 5, 6 / 5, 0),
  impulsivity = c(1, 2, 1, 1, 1 / 2),
  intimacyAvoidance = c(11 / 6, 7 / 6, 11 / 6, 11 / 6, 1 / 2),
  irresponsibility = c(17 / 7, 4 / 7, 17 / 7, 17 / 7, 3 / 7),
  manipulativeness = c(6 / 5, 9 / 5, 6 / 5, 6 / 5, 0),
  perceptualDysregulation = c(11 / 6, 7 / 6, 11 / 6, 11 / 6, 0),
  perseveration = c(13 / 9, 14 / 9, 13 / 9, 13 / 9, 0),
  restrictedAffectivity = c(8 / 7, 13 / 7, 8 / 7, 8 / 7, 0),
  rigidPerfectionism = c(19 / 10, 11 / 10, 19 / 10, 19 / 10, 0),
  riskTaking = c(10 / 7, 11 / 7, 10 / 7, 10 / 7, 9 / 7),
  separationInsecurity = c(1, 2, 1, 1, 0),
  submissiveness = c(7 / 4, 5 / 4, 2, 7 / 4, 0),
  suspiciousness = c(5 / 6, 13 / 6, 5 / 6, 5 / 6, 1 / 2),
  unusualBeliefsExperiences = c(13 / 8, 11 / 8, 13 / 8, 13 / 8, 0),
  withdrawal = c(13 / 10, 17 / 10, 13 / 10, 13 / 10, 0),
  negativeAffectivity = c(103 / 84, 149 / 84, 103 / 84, 103 / 84, 0),
  detachment = c(293 / 180, 247 / 180, NA, 293 / 180, 5 / 12),
  antagonism = c(89 / 90, 181 / 90, 89 / 90, 89 / 90, 1 / 10),
  disinhibition = c(314 / 189, 253 / 189, 314 / 189, 251 / 189, 13 / 42),
  psychoticism = c(1655 / 936, 1153 / 936, 1655 / 936, 1655 / 936, 0)
)

test_that("IRF output is the FULL form's 30 columns, same names and order", {
  x <- fx_pid5irf()
  f <- score_pid5(x, items = 1:218, version = "IRF", append = FALSE)
  expect_identical(
    names(f),
    names(score_pid5(sim_pid5, items = 1:220, append = FALSE))
  )
  expect_setequal(sub("^pid_", "", names(f)), names(irf_expected))
  g <- score_pid5(x, items = 1:218, version = "IRF", prefix = "x_", append = FALSE)
  expect_identical(
    names(g),
    names(score_pid5(sim_pid5, items = 1:220, prefix = "x_", append = FALSE))
  )
})

test_that("IRF scores match hand-computed values under the APA rule (D-090)", {
  f <- score_pid5(
    fx_pid5irf(), items = 1:218, version = "IRF", missing = "apa",
    srange = c(0, 3), calc_se = FALSE, append = FALSE
  )
  for (nm in names(irf_expected)) {
    expect_equal(f[[paste0("pid_", nm)]], irf_expected[[nm]], info = nm)
  }
  # The probes the fixture header names, stated against the rules they rule out.
  # Callousness R4: a ceiling would give 16 / 14 = 8 / 7.
  expect_false(isTRUE(all.equal(f$pid_callousness[4], 8 / 7)))
  # Distractibility R4: base round(4.5) = 4 would give 4 / 9.
  expect_false(isTRUE(all.equal(f$pid_distractibility[4], 4 / 9)))
  # Step 1's 16-item list would also reverse items 98 and 176: R1 would then
  # give Unusual Beliefs & Experiences (13 - 2 + 1) / 8 = 3 / 2 and
  # Depressivity (26 - 0 + 3) / 14 = 29 / 14.
  expect_false(isTRUE(all.equal(f$pid_unusualBeliefsExperiences[1], 3 / 2)))
  expect_false(isTRUE(all.equal(f$pid_depressivity[1], 29 / 14)))
})

test_that("IRF version is matched case-insensitively, and an abbreviation is refused", {
  x <- fx_pid5irf()
  ref <- score_pid5(x, items = 1:218, version = "IRF", append = FALSE)
  expect_identical(score_pid5(x, items = 1:218, version = "irf", append = FALSE), ref)
  expect_error(score_pid5(x, items = 1:218, version = "I"), class = "hitop_unknown_version")
})

test_that("IRF independent recomputation from the key's typed tables, each missing mode", {
  # Random answers with scattered NAs, so a wrong item in any facet list or a
  # shifted IRF number moves some facet. The facet lists, reverse items and
  # domain triplets are typed from the key (helper-fixtures.R); the facet
  # stems are typed in `irf_expected`, in the same alphabetical facet order.
  set.seed(159)
  n <- 40
  x <- as.data.frame(matrix(sample(0:3, n * 218, replace = TRUE), n, 218))
  x[matrix(stats::runif(n * 218) < 0.04, n, 218)] <- NA
  rev_x <- x
  rev_x[irf_reverse] <- 3 - rev_x[irf_reverse]
  facet_stems <- names(irf_expected)[1:25]
  domain_stems <- names(irf_expected)[26:30]
  apa <- function(m) {
    k <- ncol(m)
    apply(m, 1, function(v) {
      a <- sum(!is.na(v))
      if ((k - a) / k > 0.25) return(NA_real_)
      floor(sum(v, na.rm = TRUE) * k / a + 0.5) / k
    })
  }
  for (mode in c("apa", "available", "complete")) {
    facet <- lapply(irf_facets, function(i) {
      m <- as.matrix(rev_x[, i])
      if (mode == "apa") apa(m) else rowMeans(m, na.rm = mode == "available")
    })
    names(facet) <- facet_stems
    pkg <- score_pid5(x, items = 1:218, version = "IRF", missing = mode, append = FALSE)
    for (nm in facet_stems) {
      expect_equal(pkg[[paste0("pid_", nm)]], facet[[nm]], info = paste(mode, nm))
    }
    for (d in seq_along(irf_domains)) {
      fs <- facet_stems[match(irf_domains[[d]], names(irf_facets))]
      expected <- rowMeans(as.data.frame(facet[fs]), na.rm = mode == "available")
      expect_equal(pkg[[paste0("pid_", domain_stems[d])]], expected, info = paste(mode, d))
    }
  }
})

test_that("IRF standard errors follow the facet and domain rules", {
  d <- hush_se(score_pid5(fx_pid5irf(), items = 1:218, version = "IRF",
                          calc_se = TRUE, append = FALSE))
  # R1 Anhedonia, items after reversal 1, 3, 2, 1, 3, 1, 0, 3 (arithmetic in
  # the block above): SD over 8 items / sqrt(8).
  expect_equal(d$pid_anhedonia_se[1], stats::sd(c(1, 3, 2, 1, 3, 1, 0, 3)) / sqrt(8))
  # R1 Detachment: facets 13/10, 7/4, 11/6, so SD of the 3 / sqrt(3).
  expect_equal(d$pid_detachment_se[1], stats::sd(c(13 / 10, 7 / 4, 11 / 6)) / sqrt(3))
  # R3 Detachment is NA, and so is its standard error.
  expect_true(is.na(d$pid_detachment_se[3]))
})

test_that("IRF refuses a data frame with the wrong number of items", {
  expect_error(
    score_pid5(fx_pid5irf(), items = 1:217, version = "IRF"),
    "Expected 218 items but got 217",
    fixed = TRUE
  )
})
