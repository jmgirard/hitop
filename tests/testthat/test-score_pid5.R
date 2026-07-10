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
  # guarding pid_items against transcription errors. Uses apa_scoring = FALSE so
  # the hand rowMeans(na.rm = TRUE) oracle is the right comparison on the missing
  # row R4 too; the APA path is exercised in its own oracle tests below.
  x <- fx_pid5()
  rev_items <- c(7, 30, 35, 58, 87, 90, 96, 97, 98, 131, 142, 155, 164, 177, 210, 215)
  xr <- x
  for (i in rev_items) xr[[i]] <- 3 - x[[i]]     # reverse-key, range c(0,3)

  anhedonia <- c(1, 23, 26, 30, 124, 155, 157, 189)  # official Anhedonia items
  f_anhedo_hand <- rowMeans(xr[, anhedonia], na.rm = TRUE)

  pkg <- score_pid5(x, items = 1:220, version = "FULL", apa_scoring = FALSE, append = FALSE)
  expect_equal(pkg$pid_anhedonia, f_anhedo_hand)
})

test_that("FULL applies reverse-keying (facet with a reverse item is nonzero on all-0 input)", {
  f <- score_pid5(fx_pid5(), items = 1:220, version = "FULL", append = FALSE)
  expect_gt(f$pid_anhedonia[1], 0)             # contains reverse items 30, 155
  expect_equal(f$pid_separationInsecurity[1], 0) # contains none
})

test_that("FULL traditional scoring (apa_scoring = FALSE) tolerates missing via rowMeans", {
  f <- score_pid5(fx_pid5(), items = 1:220, version = "FULL", apa_scoring = FALSE, append = FALSE)
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

  # apa_scoring = FALSE keeps rowMeans as the correct oracle on the missing row.
  pkg <- score_pid5(x, items = 1:100, version = "SF", apa_scoring = FALSE, append = FALSE)
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

  # apa_scoring = FALSE keeps rowMeans as the correct oracle on the missing row.
  pkg <- score_pid5(x, items = 1:25, version = "BF", apa_scoring = FALSE, append = FALSE)
  expect_equal(pkg$pid_disinhibition, d_disinhib_hand)
  expect_equal(pkg$pid_detachment, d_detach_hand)
})

test_that("BF missing-item handling differs by apa_scoring", {
  # R4 sets items 1:5 NA. Disinhibition = items 1,2,3,5,6 -> 4 of 5 missing (80%).
  d_apa  <- score_pid5(fx_pid5bf(), items = 1:25, version = "BF", append = FALSE)
  d_trad <- score_pid5(fx_pid5bf(), items = 1:25, version = "BF", apa_scoring = FALSE, append = FALSE)

  # APA (default): >25% of Disinhibition items missing -> NA (not scored).
  expect_true(is.na(d_apa$pid_disinhibition[4]))
  # Traditional: rowMeans(na.rm = TRUE) averages the single surviving item 6 (=1).
  expect_equal(d_trad$pid_disinhibition[4], 1)
  expect_false(is.na(d_trad$pid_disinhibition[4]))
  # Detachment = items 4,13,14,16,18 -> only item 4 missing (1 of 5 = 20% <= 25%),
  # so APA prorates: round(4*5/4)/5 = 5/5 = 1 (matches traditional here).
  expect_equal(d_apa$pid_detachment[4], 1)
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

test_that("FULL domains honor na.rm under traditional scoring (apa_scoring = FALSE)", {
  x <- fx_pid5()
  anhedonia <- c(1, 23, 26, 30, 124, 155, 157, 189)  # official Anhedonia items
  x[1, anhedonia] <- NA_integer_                      # wipe the whole facet on R1
  f_narm <- score_pid5(x, items = 1:220, version = "FULL", apa_scoring = FALSE, na.rm = TRUE, append = FALSE)
  f_nona <- score_pid5(x, items = 1:220, version = "FULL", apa_scoring = FALSE, na.rm = FALSE, append = FALSE)

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

# ---- APA missing-data / proration scoring (M8, default apa_scoring = TRUE) ----
# APA full-form key (Krueger et al., 2013, p. 8), sourced verbatim in SOURCES.md:
#   > 25% of a facet's items unanswered -> facet NA ("should not be used").
#   <= 25% unanswered -> prorate: round(partial_raw * n_items / n_answered), then
#   average = prorated_raw / n_items. Domain NA if any of its 3 facets is NA.
# The BF key applies the same rule to its 5-item domains (M6). With no missing
# items APA and traditional scoring agree, so the completed-data fixture tests
# above already cover the no-missing case for both modes.

test_that("APA and traditional scoring agree when no items are missing", {
  # R1/R2 of every fixture are complete; proration with n_answered = n_items is
  # round(sum)/n = sum/n = the plain mean, so the two paths must coincide.
  for (v in c("FULL", "SF")) {
    n <- if (v == "FULL") 220 else 100
    fx <- if (v == "FULL") fx_pid5() else fx_pid5sf()
    apa  <- score_pid5(fx[1:2, ], items = 1:n, version = v, append = FALSE)
    trad <- score_pid5(fx[1:2, ], items = 1:n, version = v, apa_scoring = FALSE, append = FALSE)
    expect_equal(apa, trad)
  }
})

test_that("APA facet proration rounds the prorated raw (FULL Anhedonia, item 1 missing)", {
  # fx_pid5() R4: all items 1, items 1:22 NA. Anhedonia = 1,23,26,30R,124,155R,157,189
  # (n = 8); only item 1 missing (1/8 = 12.5% <= 25% -> prorate). Answered raw =
  # five 1s + two reverse(1)=2 = 9 over 7 answered. Prorated raw = round(9*8/7) =
  # round(10.2857) = 10; average = 10/8 = 1.25. Traditional rowMeans = 9/7.
  apa  <- score_pid5(fx_pid5(), items = 1:220, version = "FULL", append = FALSE)
  trad <- score_pid5(fx_pid5(), items = 1:220, version = "FULL", apa_scoring = FALSE, append = FALSE)
  expect_equal(apa$pid_anhedonia[4], 1.25)
  expect_equal(trad$pid_anhedonia[4], 9 / 7)
  expect_false(isTRUE(all.equal(apa$pid_anhedonia[4], trad$pid_anhedonia[4])))
})

test_that("APA drops a facet with more than 25% of items missing (FULL Impulsivity)", {
  # R4: Impulsivity = 4,16,17,22,58,204 (n = 6); items 4,16,17,22 in 1:22 ->
  # 4 of 6 missing (66.7% > 25%) -> NA. Traditional averages the 2 present items.
  apa  <- score_pid5(fx_pid5(), items = 1:220, version = "FULL", append = FALSE)
  trad <- score_pid5(fx_pid5(), items = 1:220, version = "FULL", apa_scoring = FALSE, append = FALSE)
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
  names(b) <- paste0("pid_", 1:25)
  b[1, 1] <- NA_integer_
  b[1, c(2, 3, 5, 6)] <- c(0L, 0L, 1L, 1L)
  d <- score_pid5(b, items = 1:25, version = "BF", append = FALSE)
  expect_equal(d$pid_disinhibition, 0.6)
})

test_that("APA domain is NA when any one contributing facet is NA (FULL Disinhibition)", {
  # R4: Impulsivity (a primary Disinhibition facet) is NA (> 25% missing), so the
  # Disinhibition DOMAIN is NA even though Irresponsibility & Distractibility score.
  apa  <- score_pid5(fx_pid5(), items = 1:220, version = "FULL", append = FALSE)
  trad <- score_pid5(fx_pid5(), items = 1:220, version = "FULL", apa_scoring = FALSE, append = FALSE)
  expect_true(is.na(apa$pid_disinhibition[4]))
  expect_false(is.na(apa$pid_irresponsibility[4]))   # a computable sibling facet
  expect_false(is.na(trad$pid_disinhibition[4]))     # traditional averages what it has
})

test_that("APA SE is NA wherever the scale score is NA", {
  se <- score_pid5(fx_pid5(), items = 1:220, version = "FULL", calc_se = TRUE, append = FALSE)
  # R4 Impulsivity facet and Disinhibition domain are NA -> their _se must be NA.
  expect_true(is.na(se$pid_impulsivity[4]))
  expect_true(is.na(se$pid_impulsivity_se[4]))
  expect_true(is.na(se$pid_disinhibition[4]))
  expect_true(is.na(se$pid_disinhibition_se[4]))
  # A computable facet keeps a non-NA SE.
  expect_false(is.na(se$pid_anhedonia_se[4]))
})

test_that("na.rm is ignored (with a warning) under apa_scoring = TRUE", {
  expect_warning(
    out <- score_pid5(fx_pid5(), items = 1:220, version = "FULL", na.rm = FALSE, append = FALSE),
    "na.rm"
  )
  # Result equals the default apa call (na.rm truly ignored, not applied).
  default <- score_pid5(fx_pid5(), items = 1:220, version = "FULL", append = FALSE)
  expect_equal(out, default)
})

# ---- Single-row input with calc_se (regression for the M8 follow-up) ---------
# A one-row input drove the facet-SE `apply(data_items[, x], MARGIN = 1, ...)`
# to index a 1-row matrix down to a vector, so `apply(MARGIN = 1)` errored with
# "dim(X) must have a positive length". The fix adds `drop = FALSE` (mirroring
# the domain-SE path and validity_pid5's single-row fix from M3). The SE of a
# scale on one respondent is calc_sem over that scale's reverse-keyed item
# values: sd(items) / sqrt(k). The expected values below hardcode those item
# values from the official key (independent of the package's keying tables).

test_that("FULL single-row calc_se returns per-scale SEs (no drop-to-vector error)", {
  x1 <- as.data.frame(matrix(1L, nrow = 1, ncol = 220))
  names(x1) <- paste0("pid_", seq_len(220))
  f <- score_pid5(x1, items = 1:220, version = "FULL", calc_se = TRUE, append = FALSE)

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
  names(x1) <- paste0("pid_", seq_len(100))
  x1[1, c(9, 11, 43, 65)] <- c(0L, 1L, 2L, 3L)  # SF Anhedonia items (no reversal)
  f <- score_pid5(x1, items = 1:100, version = "SF", calc_se = TRUE, append = FALSE)

  expect_equal(nrow(f), 1L)
  # SF Anhedonia = items 9,11,43,65 = 0,1,2,3 -> sd(c(0,1,2,3)) / sqrt(4).
  expect_equal(f$pid_anhedonia_se, stats::sd(c(0, 1, 2, 3)) / sqrt(4))
})

test_that("BF single-row calc_se matches a hand-computed domain SE", {
  x1 <- as.data.frame(matrix(1L, nrow = 1, ncol = 25))
  names(x1) <- paste0("pid_", seq_len(25))
  x1[1, c(1, 2, 3, 5, 6)] <- c(0L, 1L, 2L, 3L, 3L)  # BF Disinhibition items
  d <- score_pid5(x1, items = 1:25, version = "BF", calc_se = TRUE, append = FALSE)

  expect_equal(nrow(d), 1L)
  # BF Disinhibition = items 1,2,3,5,6 = 0,1,2,3,3 -> sd / sqrt(5).
  expect_equal(d$pid_disinhibition_se, stats::sd(c(0, 1, 2, 3, 3)) / sqrt(5))
})

test_that("single-row calc_se equals the first row of a multi-row score", {
  # Guards against the fix altering multi-row behavior: scoring one row alone
  # must reproduce that row's SEs from a multi-row call.
  multi <- score_pid5(fx_pid5(), items = 1:220, version = "FULL",
                      calc_se = TRUE, apa_scoring = FALSE, append = FALSE)
  one <- score_pid5(fx_pid5()[2, ], items = 1:220, version = "FULL",
                    calc_se = TRUE, apa_scoring = FALSE, append = FALSE)
  se_names <- grep("_se$", names(multi), value = TRUE)
  expect_equal(as.numeric(one[1, se_names]), as.numeric(multi[2, se_names]))
})

test_that("SF applies the APA rule the same way as FULL", {
  # fx_pid5sf() R5: items 1:10 NA. Submissiveness = 2 of 4 items missing (> 25%).
  apa  <- score_pid5(fx_pid5sf(), items = 1:100, version = "SF", append = FALSE)
  trad <- score_pid5(fx_pid5sf(), items = 1:100, version = "SF", apa_scoring = FALSE, append = FALSE)
  expect_true(is.na(apa$pid_submissiveness[5]))
  expect_false(is.na(trad$pid_submissiveness[5]))
})
