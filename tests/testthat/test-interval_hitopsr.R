# interval_hitopsr(): the shipped reference table, the two oracles, and the
# reported branches.
#
# Oracle records for the two numeric oracles below are in cairn/ORACLES.md, the
# location cairn/DESIGN.md's Conventions section declares.

# ---- the two shipped name tables, iterated, never a hand-written list --------

package_names <- function() {
  rbind(
    data.frame(
      Scale = hitopsr_scales$Scale,
      nItems = as.integer(hitopsr_scales$nItems),
      camelCase = hitopsr_scales$camelCase,
      type = "scale",
      stringsAsFactors = FALSE
    ),
    data.frame(
      Scale = hitopsr_subscales$Subscale,
      nItems = as.integer(hitopsr_subscales$nItems),
      camelCase = hitopsr_subscales$camelCase,
      type = "subscale",
      stringsAsFactors = FALSE
    )
  )
}

# ---- AC1: the join is complete, and the checker can say so wrongly -----------

# The two checks the completeness assertion is made of, factored out so the
# probes below can run them over a deliberately broken table. Each returns the
# offending scale *names*, so a failure says which row rather than how many.
join_residue <- function(devstats, tables, declared) {
  setdiff(setdiff(tables$Scale, devstats$Scale), declared)
}
join_missing_exceptions <- function(devstats, tables, declared) {
  intersect(declared, devstats$Scale)
}
join_duplicates <- function(devstats, tables) {
  covered <- devstats$Scale[devstats$Scale %in% tables$Scale]
  unique(covered[duplicated(covered)])
}

# The exception set data-raw/hitopsr_devstats.R declares: package scales and
# subscales that Table 1's primary block gives no row. It is empty, and empty is
# the finding rather than an omission -- the 2026-08-28 run of
# data-raw/verify_hitopsr_names.R reconciles all 93 labels once `Manic Energy`'s
# footnote marker is mapped away. The probes below therefore construct a
# non-empty set rather than borrowing one.
declared_exceptions <- character(0)

test_that("every HiTOP-SR scale and subscale outside the exception set has exactly one reference row", {
  tables <- package_names()
  ## Guard against the domain silently emptying: the sweep is worthless if the
  ## shipped tables ever stop being read.
  expect_gt(nrow(tables), 90L)

  expect_identical(
    join_residue(hitopsr_devstats, tables, declared_exceptions),
    character(0)
  )
  expect_identical(join_duplicates(hitopsr_devstats, tables), character(0))
  ## The residue the checker computes is exactly what the map declares -- in
  ## both directions, so a declared exception that does in fact have a row is a
  ## failure too.
  expect_identical(
    join_missing_exceptions(hitopsr_devstats, tables, declared_exceptions),
    character(0)
  )

  ## The engine joins a score column on `camelCase`, not on `scale`, and
  ## `match()` would silently take the first of two rows sharing a stem. The
  ## printed names differ, so the checks above would not see it.
  expect_identical(
    unique(hitopsr_devstats$camelCase[duplicated(hitopsr_devstats$camelCase)]),
    character(0)
  )

  ## Nothing is covered twice, and the stems and kinds agree with the tables
  ## they were derived from.
  keyed <- merge(tables, as.data.frame(hitopsr_devstats), by = "Scale")
  expect_identical(nrow(keyed), nrow(tables))
  expect_identical(keyed$camelCase.x, keyed$camelCase.y)
  expect_identical(keyed$type.x, keyed$type.y)
})

test_that("the join checker reports a removed, a duplicated, and an undeclared row by name", {
  tables <- package_names()
  victim <- "Agoraphobia"
  expect_true(victim %in% hitopsr_devstats$Scale)

  ## (1) A row removed and not declared: the residue names it.
  removed <- hitopsr_devstats[hitopsr_devstats$Scale != victim, ]
  expect_identical(join_residue(removed, tables, character(0)), victim)

  ## (2) A row duplicated: the duplicate check names it.
  doubled <- rbind(
    hitopsr_devstats,
    hitopsr_devstats[hitopsr_devstats$Scale == victim, ]
  )
  expect_identical(join_duplicates(doubled, tables), victim)

  ## (3) An exception-set member deleted. The shipped set is empty, so the world
  ## in which a member could be deleted is constructed: with the row gone AND
  ## declared, the checker is silent; delete the member from the declaration and
  ## it names the row again. This is the probe that would catch a map going
  ## stale against a table that really does skip a scale.
  expect_identical(join_residue(removed, tables, victim), character(0))
  expect_identical(join_residue(removed, tables, character(0)), victim)

  ## (4) The converse: an exception declared for a scale that does have a row.
  expect_identical(
    join_missing_exceptions(hitopsr_devstats, tables, victim),
    victim
  )
})

# ---- AC2: the CI-runnable half of the source check ---------------------------

# data-raw/verify_hitopsr_devstats.R diffs every shipped cell against Table 1,
# but it needs the gitignored shelf PDF and never runs here. This is the oracle
# that does: Table 1's own `# Items` column, transcribed into `nItems`, against
# the item counts the keying tables derive from `hitopsr_items`. The two come
# from different places -- a printed table and the package's own keying -- so a
# transcription that took a row's cells from the neighbouring row would have to
# have moved an item count too.
test_that("every transcribed item count equals the scale's item count in the keying tables", {
  tables <- package_names()
  keyed <- merge(tables, as.data.frame(hitopsr_devstats), by = "Scale")
  expect_identical(nrow(keyed), 93L)
  expect_identical(keyed$nItems.y, keyed$nItems.x)
})

test_that("the reference statistics are the shape a reference table must be", {
  expect_identical(nrow(hitopsr_devstats), 93L)
  expect_identical(sum(hitopsr_devstats$type == "scale"), 76L)
  expect_identical(sum(hitopsr_devstats$type == "subscale"), 17L)
  expect_false(anyNA(hitopsr_devstats))
  expect_true(all(hitopsr_devstats$reliabilityType == "alpha"))
  ## A reliability outside (0, 1] makes sqrt(rel) or sqrt(1 - rel) undefined.
  expect_true(all(hitopsr_devstats$reliability > 0))
  expect_true(all(hitopsr_devstats$reliability <= 1))
  ## The mean and SD belong to the 1-4 item-mean coding.
  expect_true(all(hitopsr_devstats$mean >= 1 & hitopsr_devstats$mean <= 4))
  expect_true(all(hitopsr_devstats$sd > 0))
})

# ---- the reference coding the function converts on ---------------------------

# `interval_hitopsr()` writes c(1, 4) into its call to the engine, and that
# constant is the one number in this family that no other check traces back to
# the source. Table 1's own Range column is compared against it by
# data-raw/verify_hitopsr_devstats.R; here it is compared against the coding the
# scoring function that produces these columns defaults to, which is the
# package's own independent statement of the same fact.
test_that("the conversion coding is the coding score_hitopsr() scores on", {
  expect_identical(
    eval(formals(interval_hitopsr)$srange),
    eval(formals(score_hitopsr)$srange)
  )
  expect_identical(eval(formals(interval_hitopsr)$srange), c(1, 4))
})

# ---- AC4: closed-form oracle -------------------------------------------------

# Schmukle (2026, Assessment, 33(5), 817-825), Equations (10) to (12), p. 821:
#
#   est  = M + sqrt(r) * (x - M)
#   SEM  = SD * sqrt(1 - r)
#   bounds = est +/- z * SEM
#
# Two scales at the ends of the shipped reliability range, each at a score below,
# at, and above its reference mean, at two confidence levels. Every constant
# below is read off Table 1 as printed, never taken from `hitopsr_devstats`, so a
# row joined wrongly or a column read wrongly fails here. The literal expected
# values are pinned as well as derived: the derivation catches a mistyped
# literal, and the literal catches the function and the derivation drifting
# together.

test_that("the estimate and bounds are Schmukle's Equations (10) to (12)", {
  # Situational Phobias -- the lowest reliability Table 1 prints.
  #   # Items 4, alpha 0.61, M 1.61, SD 0.62   (p. 49 of the shelf PDF)
  #   sqrt(0.61)     = 0.7810249675906654
  #   sqrt(1 - 0.61) = 0.6244997998398398
  #   est(1.20) = 1.61 + 0.7810249675906654 * (1.20 - 1.61) = 1.2897797632878272
  #   est(1.61) = 1.61 + 0.7810249675906654 * (1.61 - 1.61) = 1.61
  #   est(2.50) = 1.61 + 0.7810249675906654 * (2.50 - 1.61) = 2.3051122211556923
  #   z(0.95) = 1.9599639845400534, z(0.80) = 1.2815515655446006
  #   half95 = 1.9599639845400534 * 0.62 * 0.6244997998398398 = 0.7588782119439061
  #   half80 = 1.2815515655446006 * 0.62 * 0.6244997998398398 = 0.4962037916235626
  sp_x <- c(1.20, 1.61, 2.50)
  sp_est <- 1.61 + sqrt(0.61) * (sp_x - 1.61)
  expect_equal(
    sp_est,
    c(1.2897797632878272, 1.61, 2.3051122211556923),
    tolerance = 1e-14
  )
  sp_half95 <- 1.9599639845400534 * 0.62 * sqrt(1 - 0.61)
  sp_half80 <- 1.2815515655446006 * 0.62 * sqrt(1 - 0.61)
  expect_equal(sp_half95, 0.7588782119439061, tolerance = 1e-14)
  expect_equal(sp_half80, 0.4962037916235626, tolerance = 1e-14)

  # Distress-Dysphoria -- the highest reliability Table 1 prints.
  #   # Items 16, alpha 0.96, M 2.19, SD 0.87  (p. 49 of the shelf PDF)
  #   sqrt(0.96)     = 0.9797958971132712
  #   sqrt(1 - 0.96) = 0.2
  #   est(1.50) = 2.19 + 0.9797958971132712 * (1.50 - 2.19) = 1.5139408309918427
  #   est(2.19) = 2.19
  #   est(3.40) = 2.19 + 0.9797958971132712 * (3.40 - 2.19) = 3.3755530355070578
  #   half95 = 1.9599639845400534 * 0.87 * 0.2 = 0.3410337333099694
  #   half80 = 1.2815515655446006 * 0.87 * 0.2 = 0.2229899724047606
  dd_x <- c(1.50, 2.19, 3.40)
  dd_est <- 2.19 + sqrt(0.96) * (dd_x - 2.19)
  expect_equal(
    dd_est,
    c(1.5139408309918427, 2.19, 3.3755530355070578),
    tolerance = 1e-14
  )
  dd_half95 <- 1.9599639845400534 * 0.87 * sqrt(1 - 0.96)
  dd_half80 <- 1.2815515655446006 * 0.87 * sqrt(1 - 0.96)
  expect_equal(dd_half95, 0.3410337333099694, tolerance = 1e-14)
  expect_equal(dd_half80, 0.2229899724047606, tolerance = 1e-14)

  scores <- data.frame(
    hsr_situationalPhobias = sp_x,
    hsr_distressDysphoria = dd_x
  )
  cols <- c("hsr_situationalPhobias", "hsr_distressDysphoria")

  got95 <- interval_hitopsr(scores, scores = cols, append = FALSE)
  expect_equal(got95$hsr_situationalPhobias_est, sp_est, tolerance = 1e-8)
  expect_equal(
    got95$hsr_situationalPhobias_lo, sp_est - sp_half95, tolerance = 1e-8
  )
  expect_equal(
    got95$hsr_situationalPhobias_hi, sp_est + sp_half95, tolerance = 1e-8
  )
  expect_equal(got95$hsr_distressDysphoria_est, dd_est, tolerance = 1e-8)
  expect_equal(
    got95$hsr_distressDysphoria_lo, dd_est - dd_half95, tolerance = 1e-8
  )
  expect_equal(
    got95$hsr_distressDysphoria_hi, dd_est + dd_half95, tolerance = 1e-8
  )

  got80 <- interval_hitopsr(scores, scores = cols, level = 0.80, append = FALSE)
  ## The estimate does not depend on the level; only the width does.
  expect_equal(got80$hsr_situationalPhobias_est, sp_est, tolerance = 1e-8)
  expect_equal(
    got80$hsr_situationalPhobias_lo, sp_est - sp_half80, tolerance = 1e-8
  )
  expect_equal(
    got80$hsr_situationalPhobias_hi, sp_est + sp_half80, tolerance = 1e-8
  )
  expect_equal(got80$hsr_distressDysphoria_est, dd_est, tolerance = 1e-8)
  expect_equal(
    got80$hsr_distressDysphoria_lo, dd_est - dd_half80, tolerance = 1e-8
  )
  expect_equal(
    got80$hsr_distressDysphoria_hi, dd_est + dd_half80, tolerance = 1e-8
  )

  ## The two scales differ in the direction the estimate moves a score and in
  ## how far, which is what makes them opposite ends of the range rather than
  ## two instances of one case: at 0.61 a score 0.89 above the mean is pulled
  ## back to 0.695 above it, at 0.96 a score 1.21 above is pulled back to 1.186.
  expect_lt(sp_est[[3]] - 1.61, 2.50 - 1.61)
  expect_gt(sp_est[[1]] - 1.61, 1.20 - 1.61)
  expect_gt(sp_half95, dd_half95)
})

# The article's own worked examples (Box 1, p. 823), on the metrics it uses. They
# are a published reference value for the same equations, computed nowhere near
# this package's data. The article carries out its substitutions at two decimals
# and prints bounds that follow from the rounded intermediates, so its estimate
# and standard error are compared exactly and its bounds to that rounding.
test_that("the engine reproduces the source's own worked examples", {
  box <- function(M, sd, rel, x, level = 0.95) {
    ref <- data.frame(camelCase = "s", mean = M, sd = sd, reliability = rel)
    interval_engine(
      data = data.frame(s = x),
      scores = "s",
      refstats = ref,
      ref_srange = c(0, 1),
      srange = c(0, 1),
      prefix = "",
      level = level,
      append = FALSE,
      dataset = "box"
    )
  }

  ## Ms. A: observed T of 60, M 50, SD 10, reliability .80.
  ## RETS = 58.94, SEM = 4.47, 95% CI = [50.18, 67.70].
  a <- box(50, 10, 0.80, 60)
  expect_equal(round(a$s_est, 2), 58.94)
  expect_equal(round((a$s_hi - a$s_lo) / 2 / 1.9599639845400534, 2), 4.47)
  expect_equal(round(a$s_lo, 2), 50.18)
  expect_equal(round(a$s_hi, 1), 67.7)

  ## Mr. B: observed IQ of 90, M 100, SD 15, reliability .90.
  ## RETS = 90.51, SEM = 4.74, 95% CI = [81.22, 99.80].
  b <- box(100, 15, 0.90, 90)
  expect_equal(round(b$s_est, 2), 90.51)
  expect_equal(round((b$s_hi - b$s_lo) / 2 / 1.9599639845400534, 2), 4.74)
  expect_equal(round(b$s_lo, 2), 81.22)
  expect_equal(round(b$s_hi, 1), 99.8)
})

# ---- AC5: simulation-coverage oracle -----------------------------------------

# The primary oracle for an interval method is its coverage. The model is the one
# cairn/references/schmukle2026.md records from the source's own simulation
# (p. 822): true scores are drawn on the observed score's metric, and the
# observed score is generated from them so that the two share a standard
# deviation and correlate at sqrt(reliability) --
#
#   t ~ Normal(M, SD)
#   x = M + sqrt(rel) * (t - M) + Normal(0, SD * sqrt(1 - rel))
#
# The metric `t` is drawn on is load-bearing: drawn instead at the classical true
# score's smaller spread, SD * sqrt(rel), Equation (12)'s interval is
# conservative rather than nominal, and this oracle would pass for the wrong
# reason.
#
# What is asserted is *marginal* coverage over that population, which is what the
# method promises. Coverage at a single fixed true score is not asserted, and is
# not nominal: a mean-shrunken estimator covers a true score at the mean far more
# often than one far from it. The check below shows both, so the distinction is
# pinned rather than merely stated.

sim_coverage <- function(rel, level, mean, sd, reps, seed, fixed_true = NULL) {
  withr::with_seed(seed, {
    truth <- if (is.null(fixed_true)) {
      stats::rnorm(reps, mean, sd)
    } else {
      rep(fixed_true, reps)
    }
    observed <- mean + sqrt(rel) * (truth - mean) +
      stats::rnorm(reps, 0, sd * sqrt(1 - rel))
    ref <- data.frame(
      camelCase = "s", mean = mean, sd = sd, reliability = rel
    )
    got <- interval_engine(
      data = data.frame(s = observed),
      scores = "s",
      refstats = ref,
      ref_srange = c(0, 1),
      srange = c(0, 1),
      prefix = "",
      level = level,
      append = FALSE,
      dataset = "sim"
    )
    mean(truth >= got$s_lo & truth <= got$s_hi)
  })
}

test_that("the interval covers the true score at its nominal rate over the reference population", {
  skip_if_not_installed("withr")

  ## The lowest, median and highest reliability the shipped table carries, read
  ## from the table rather than typed, so the sweep follows the data.
  rels <- unname(stats::quantile(
    hitopsr_devstats$reliability,
    probs = c(0, 0.5, 1),
    type = 1
  ))
  expect_equal(rels, c(0.61, 0.84, 0.96))

  ## Fixed here, not drawn: 200,000 replications and one seed per cell. The
  ## Monte-Carlo standard error of a proportion near 0.95 at that count is
  ## sqrt(0.95 * 0.05 / 2e5) = 0.00049, so a four-standard-error band is 0.002.
  ## Near 0.80 it is sqrt(0.8 * 0.2 / 2e5) = 0.00089, and four of those is
  ## 0.0036.
  reps <- 200000L
  seed <- 20260828L
  tol <- c("0.95" = 0.002, "0.8" = 0.0036)

  for (level in c(0.95, 0.80)) {
    for (i in seq_along(rels)) {
      got <- sim_coverage(
        rel = rels[[i]],
        level = level,
        ## The metric is arbitrary under this model, so the sweep uses the
        ## scale the source's own simulation is on.
        mean = 50,
        sd = 10,
        reps = reps,
        seed = seed + i
      )
      expect_equal(
        got,
        level,
        tolerance = tol[[as.character(level)]],
        info = paste("reliability", rels[[i]], "level", level)
      )
    }
  }
})

test_that("coverage at a single fixed true score is not the nominal rate", {
  skip_if_not_installed("withr")
  ## The same model and the same estimator, with the true score held fixed
  ## instead of drawn. At the reference mean the interval over-covers and two
  ## standard deviations out it under-covers, by margins far outside the
  ## Monte-Carlo band above. This is why the oracle above is marginal.
  at_mean <- sim_coverage(0.61, 0.95, 50, 10, 100000L, 4L, fixed_true = 50)
  far_out <- sim_coverage(0.61, 0.95, 50, 10, 100000L, 5L, fixed_true = 70)
  expect_gt(at_mean, 0.97)
  expect_lt(far_out, 0.90)
})

# ---- the bounds are not clamped to the response range ------------------------

# Equation (12) is symmetric and constant-width, so on a low-reliability, low-SD
# scale a bound can fall outside the 1-4 response range. The bounds are returned
# as the equation gives them (M041 Decisions, from RR05): clamping would report
# something other than the cited equation and would hide the discrepancy.
test_that("a bound outside the response range is returned, not clamped", {
  ## Situational Phobias at the response floor: est 1.13 (est = 1.61 +
  ## sqrt(0.61) * (1 - 1.61)), half-width 0.759, so the lower bound is 0.37 --
  ## below the floor of 1 that a 1-4 item mean can reach.
  got <- interval_hitopsr(
    data.frame(hsr_situationalPhobias = 1),
    scores = "hsr_situationalPhobias",
    append = FALSE
  )
  expect_lt(got$hsr_situationalPhobias_lo, 1)
  expect_equal(
    got$hsr_situationalPhobias_lo,
    1.61 + sqrt(0.61) * (1 - 1.61) - 1.9599639845400534 * 0.62 * sqrt(1 - 0.61),
    tolerance = 1e-8
  )

  ## And at the ceiling, above 4.
  hi <- interval_hitopsr(
    data.frame(hsr_situationalPhobias = 4),
    scores = "hsr_situationalPhobias",
    append = FALSE
  )
  expect_gt(hi$hsr_situationalPhobias_hi, 4)
})

# ---- output shape ------------------------------------------------------------

test_that("three columns are returned per requested score, in order, appended or alone", {
  scored <- data.frame(
    id = 1:3,
    hsr_agoraphobia = c(1, 2, 3),
    hsr_wellBeing = c(2, 3, 4)
  )
  cols <- c("hsr_agoraphobia", "hsr_wellBeing")

  alone <- interval_hitopsr(scored, scores = cols, append = FALSE)
  expect_s3_class(alone, "tbl_df")
  expect_identical(
    names(alone),
    c(
      "hsr_agoraphobia_est", "hsr_agoraphobia_lo", "hsr_agoraphobia_hi",
      "hsr_wellBeing_est", "hsr_wellBeing_lo", "hsr_wellBeing_hi"
    )
  )
  expect_identical(nrow(alone), 3L)

  appended <- interval_hitopsr(scored, scores = cols)
  expect_identical(names(appended), c(names(scored), names(alone)))
  expect_identical(appended[names(scored)], tibble::as_tibble(scored))
})

test_that("an NA score returns NA in all three columns and leaves its neighbours alone", {
  got <- interval_hitopsr(
    data.frame(hsr_agoraphobia = c(1.5, NA, 2.5)),
    scores = "hsr_agoraphobia",
    append = FALSE
  )
  expect_identical(is.na(got$hsr_agoraphobia_est), c(FALSE, TRUE, FALSE))
  expect_identical(is.na(got$hsr_agoraphobia_lo), c(FALSE, TRUE, FALSE))
  expect_identical(is.na(got$hsr_agoraphobia_hi), c(FALSE, TRUE, FALSE))
})

test_that("positions and names select the same columns", {
  scored <- data.frame(id = 1:2, hsr_agoraphobia = c(1, 2))
  expect_identical(
    interval_hitopsr(scored, scores = 2, append = FALSE),
    interval_hitopsr(scored, scores = "hsr_agoraphobia", append = FALSE)
  )
})

# ---- the two reported branches, asserted by class ----------------------------

test_that("a score column with no reference row is reported and returned as NA", {
  scored <- data.frame(hsr_agoraphobia = 1.5, hsr_notAScale = 2)
  expect_warning(
    got <- interval_hitopsr(
      scored,
      scores = c("hsr_agoraphobia", "hsr_notAScale"),
      append = FALSE
    ),
    class = "hitop_interval_uncovered"
  )
  expect_true(all(is.na(got$hsr_notAScale_est)))
  expect_true(all(is.na(got$hsr_notAScale_lo)))
  expect_true(all(is.na(got$hsr_notAScale_hi)))
  ## The covered column beside it is converted as usual.
  expect_false(is.na(got$hsr_agoraphobia_est))

  ## A mistyped prefix leaves the whole name unstripped, so it reaches the same
  ## branch rather than silently matching nothing in particular.
  expect_warning(
    interval_hitopsr(
      data.frame(hsr_agoraphobia = 1.5),
      scores = "hsr_agoraphobia",
      prefix = "hsrr_",
      append = FALSE
    ),
    class = "hitop_interval_uncovered"
  )
})

test_that("a response coding other than the reference one is reported and converts nothing", {
  scored <- data.frame(hsr_agoraphobia = 1.5)
  ## Three shapes of mismatch, not one: the coding shifted, stretched, and
  ## narrowed. Each is a different way of not being c(1, 4).
  for (rng in list(c(0, 3), c(1, 5), c(1, 2))) {
    expect_warning(
      got <- interval_hitopsr(
        scored,
        scores = "hsr_agoraphobia",
        srange = rng,
        append = FALSE
      ),
      class = "hitop_interval_coding"
    )
    expect_true(all(is.na(got$hsr_agoraphobia_est)), info = paste(rng, collapse = "-"))
    expect_true(all(is.na(got$hsr_agoraphobia_lo)), info = paste(rng, collapse = "-"))
    expect_true(all(is.na(got$hsr_agoraphobia_hi)), info = paste(rng, collapse = "-"))
  }

  ## The reference coding itself is silent.
  expect_silent(
    interval_hitopsr(scored, scores = "hsr_agoraphobia", append = FALSE)
  )
})

test_that("a call that is both uncovered and on the wrong coding raises both reports", {
  scored <- data.frame(hsr_agoraphobia = 1.5, hsr_notAScale = 2)
  seen <- character(0)
  withCallingHandlers(
    interval_hitopsr(
      scored,
      scores = c("hsr_agoraphobia", "hsr_notAScale"),
      srange = c(0, 3),
      append = FALSE
    ),
    warning = function(w) {
      seen <<- c(seen, class(w)[[1]])
      invokeRestart("muffleWarning")
    }
  )
  expect_setequal(seen, c("hitop_interval_coding", "hitop_interval_uncovered"))

  ## Both are warnings, so one suppressWarnings() silences the function.
  expect_silent(suppressWarnings(
    interval_hitopsr(
      scored,
      scores = c("hsr_agoraphobia", "hsr_notAScale"),
      srange = c(0, 3),
      append = FALSE
    )
  ))
})

# ---- error branches ----------------------------------------------------------

test_that("each argument is rejected with a message naming it", {
  scored <- data.frame(id = 1:2, hsr_agoraphobia = c(1, 2))

  expect_error(
    interval_hitopsr("not a data frame", scores = "hsr_agoraphobia"),
    "must be a data frame"
  )
  expect_error(
    interval_hitopsr(scored, scores = TRUE),
    "did not have the expected type"
  )
  expect_error(
    interval_hitopsr(scored, scores = c("hsr_agoraphobia", "hsr_agoraphobia")),
    "distinct column"
  )
  expect_error(
    interval_hitopsr(scored, scores = "hsr_nope"),
    "must all be columns"
  )
  expect_error(
    interval_hitopsr(scored, scores = 99),
    "valid columns"
  )
  expect_error(
    interval_hitopsr(scored, scores = "hsr_agoraphobia", srange = c(4, 1)),
    "greater than the first"
  )
  expect_error(
    interval_hitopsr(scored, scores = "hsr_agoraphobia", srange = 1),
    "two integerish values"
  )
  expect_error(
    interval_hitopsr(scored, scores = "hsr_agoraphobia", prefix = 1),
    "must be a single string"
  )
  expect_error(
    interval_hitopsr(scored, scores = "hsr_agoraphobia", append = "yes"),
    "must be `TRUE` or `FALSE`"
  )
})

test_that("the confidence level is rejected on its type and on its range, separately", {
  scored <- data.frame(hsr_agoraphobia = 1.5)
  expect_error(
    interval_hitopsr(scored, scores = "hsr_agoraphobia", level = "0.95"),
    "must be a single number"
  )
  expect_error(
    interval_hitopsr(scored, scores = "hsr_agoraphobia", level = c(0.9, 0.95)),
    "must be a single number"
  )
  ## The mistake this message exists for: a percentage rather than a proportion.
  expect_error(
    interval_hitopsr(scored, scores = "hsr_agoraphobia", level = 95),
    "proportion between 0 and 1"
  )
  expect_error(
    interval_hitopsr(scored, scores = "hsr_agoraphobia", level = 0),
    "proportion between 0 and 1"
  )
  expect_error(
    interval_hitopsr(scored, scores = "hsr_agoraphobia", level = 1),
    "proportion between 0 and 1"
  )
  ## An integer is a number on the right scale, so it is out of range rather
  ## than the wrong type -- the two messages must not swap places.
  expect_error(
    interval_hitopsr(scored, scores = "hsr_agoraphobia", level = 1L),
    "proportion between 0 and 1"
  )
})

test_that("a factor or character score column is an error, not a coercion", {
  expect_error(
    interval_hitopsr(
      data.frame(hsr_agoraphobia = factor(c("1", "2"))),
      scores = "hsr_agoraphobia"
    ),
    "must be numeric"
  )
  expect_error(
    interval_hitopsr(
      data.frame(hsr_agoraphobia = c("1", "2")),
      scores = "hsr_agoraphobia"
    ),
    "must be numeric"
  )
  ## A logical column converts as it reads.
  expect_no_error(
    interval_hitopsr(
      data.frame(hsr_agoraphobia = c(TRUE, FALSE)),
      scores = "hsr_agoraphobia",
      append = FALSE
    )
  )
})

test_that("a wider level gives a wider interval and the same estimate", {
  scored <- data.frame(hsr_agoraphobia = 2)
  narrow <- interval_hitopsr(scored, scores = "hsr_agoraphobia",
                             level = 0.50, append = FALSE)
  wide <- interval_hitopsr(scored, scores = "hsr_agoraphobia",
                           level = 0.99, append = FALSE)
  expect_equal(narrow$hsr_agoraphobia_est, wide$hsr_agoraphobia_est)
  expect_gt(
    wide$hsr_agoraphobia_hi - wide$hsr_agoraphobia_lo,
    narrow$hsr_agoraphobia_hi - narrow$hsr_agoraphobia_lo
  )
})
