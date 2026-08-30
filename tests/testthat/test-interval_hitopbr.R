# interval_hitopbr(): the shipped reference table, the closed-form oracle, and
# the reported branches.
#
# Oracle records for the numeric oracles below are in cairn/ORACLES.md, the
# location cairn/DESIGN.md's Conventions section declares.

# ---- the shipped table against the package's own keying ----------------------

# data-raw/verify_hitopbr_devstats.R diffs every shipped cell against Table 1,
# but it needs the gitignored shelf PDF and never runs here. This is the oracle
# that does: Table 1's own `# Items` column, transcribed into `nItems`, against
# the item counts `hitopbr_scales` derives from `hitopbr_items`. The two come
# from different places -- a printed table and the package's own keying -- so a
# transcription that took a row's cells from the neighbouring row would have to
# have moved an item count too.
test_that("every transcribed item count equals the scale's item count in the keying tables", {
  keyed <- lengths(hitopbr_scales$itemNumbers)[
    match(hitopbr_devstats$camelCase, hitopbr_scales$camelCase)
  ]
  ## Guard against the domain silently emptying: an unmatched stem would make
  ## `keyed` all NA, and a comparison against nothing would pass by vacuity.
  expect_false(anyNA(keyed))
  expect_identical(length(keyed), 8L)
  expect_identical(as.integer(keyed), hitopbr_devstats$nItems)
})

test_that("every column score_hitopbr() emits has a reference row", {
  emitted <- names(score_hitopbr(sim_hitopbr, items = 1:45, append = FALSE))
  expect_identical(length(emitted), 8L)
  expect_identical(
    setdiff(sub("^hbr_", "", emitted), hitopbr_devstats$camelCase),
    character(0)
  )
  ## And nothing in the table is unreachable from a scored column, which the
  ## setdiff above cannot see.
  expect_setequal(sub("^hbr_", "", emitted), hitopbr_devstats$camelCase)
})

test_that("the reference statistics are the shape a reference table must be", {
  expect_identical(nrow(hitopbr_devstats), 8L)
  expect_identical(
    names(hitopbr_devstats),
    names(hitopsr_devstats)
  )
  expect_true(all(hitopbr_devstats$type == "scale"))
  expect_false(anyNA(hitopbr_devstats))
  expect_true(all(hitopbr_devstats$reliabilityType == "alpha"))
  ## A reliability outside (0, 1] makes sqrt(rel) or sqrt(1 - rel) undefined.
  expect_true(all(hitopbr_devstats$reliability > 0))
  expect_true(all(hitopbr_devstats$reliability <= 1))
  ## The mean and SD belong to the 1-4 item-mean coding.
  expect_true(all(hitopbr_devstats$mean >= 1 & hitopbr_devstats$mean <= 4))
  expect_true(all(hitopbr_devstats$sd > 0))
  ## The names are the keying table's own, not a second spelling of them.
  expect_setequal(hitopbr_devstats$scale, hitopbr_scales$Scale)
})

# ---- the reference coding the function converts on ---------------------------

# `interval_hitopbr()` writes c(1, 4) into its call to the engine, and that
# constant is the one number in this family that no other check traces back to
# the source. Table 1's own Range column is compared against it by
# data-raw/verify_hitopbr_devstats.R; here it is compared against the coding the
# scoring function that produces these columns defaults to, which is the
# package's own independent statement of the same fact.
test_that("the conversion coding is the coding score_hitopbr() scores on", {
  expect_identical(
    eval(formals(interval_hitopbr)$srange),
    eval(formals(score_hitopbr)$srange)
  )
  expect_identical(eval(formals(interval_hitopbr)$srange), c(1, 4))
  ## And the prefix it strips is the prefix that function applies.
  expect_identical(
    eval(formals(interval_hitopbr)$prefix),
    eval(formals(score_hitopbr)$prefix)
  )
})

# ---- closed-form oracle ------------------------------------------------------

# Schmukle (2026, Assessment, 33(5), 817-825), Equations (10) to (12), p. 821:
#
#   est    = M + sqrt(r) * (x - M)
#   SEM    = SD * sqrt(1 - r)
#   bounds = est +/- z * SEM
#
# All eight scales, each at a score below, at, and above its reference mean, at
# two confidence levels. Every reference constant below is read off Table 1's
# "Superspectra and Spectra Scales" block (p. 51 of the shelf PDF) as printed,
# never taken from `hitopbr_devstats`, so a row joined wrongly or a column read
# wrongly fails here. The probe scores are chosen per scale rather than at a
# fixed offset from the mean, so a bug whose size depends on the distance from
# the mean cannot hide behind one spacing. The expected values are both derived
# by explicit arithmetic and pinned as literals: the derivation catches a
# mistyped literal, and the literal catches the function and the derivation
# drifting together.

br_reference <- list(
  list(scale = "antagonism",      M = 1.42, SD = 0.45, r = 0.82,
       x = c(1.00, 1.42, 3.10),
       est = c(1.039673824198228, 1.42, 2.941304703207086),
       half95 = 0.374194032574413, half80 = 0.2446723266579423),
  list(scale = "detachment",      M = 2.13, SD = 0.88, r = 0.86,
       x = c(1.25, 2.13, 4.00),
       est = c(1.313921572396378, 2.13, 3.864166658657696),
       half95 = 0.6453492074097358, half80 = 0.4219711655941464),
  list(scale = "disinhibition",   M = 1.65, SD = 0.60, r = 0.86,
       x = c(1.11, 1.65, 2.40),
       est = c(1.149224601243232, 1.65, 2.345521387162178),
       half95 = 0.4400108232339108, half80 = 0.2877076129050998),
  list(scale = "externalizing",   M = 1.54, SD = 0.49, r = 0.83,
       x = c(1.30, 1.54, 3.70),
       est = c(1.321349594100537, 1.54, 3.507853653095169),
       half95 = 0.39597578800259, half80 = 0.2589146510013871),
  list(scale = "internalizing",   M = 1.85, SD = 0.77, r = 0.90,
       x = c(1.00, 1.85, 2.75),
       est = c(1.043619196657063, 1.85, 2.703814968245462),
       half95 = 0.4772421748745122, half80 = 0.3120518852278137),
  list(scale = "pFactor",         M = 1.68, SD = 0.55, r = 0.86,
       x = c(1.42, 1.68, 3.25),
       est = c(1.438885919117112, 1.68, 3.135958103792825),
       half95 = 0.4033432546310848, half80 = 0.2637319784963415),
  list(scale = "somatoform",      M = 1.82, SD = 0.71, r = 0.88,
       x = c(1.05, 1.82, 2.60),
       est = c(1.097675972987192, 1.82, 2.551704858532455),
       half95 = 0.482055522716449, half80 = 0.3151991642140612),
  list(scale = "thoughtDisorder", M = 1.26, SD = 0.46, r = 0.85,
       x = c(1.00, 1.26, 3.50),
       est = c(1.020291844110385, 1.26, 3.325177958433607),
       half95 = 0.3491817620793381, half80 = 0.2283176820503729)
)

test_that("the estimate and bounds are Schmukle's Equations (10) to (12)", {
  z95 <- 1.9599639845400534
  z80 <- 1.2815515655446006
  expect_equal(z95, stats::qnorm(0.975), tolerance = 1e-14)
  expect_equal(z80, stats::qnorm(0.900), tolerance = 1e-14)

  ## The sweep must cover the table, not a subset of it that happens to pass.
  expect_setequal(
    vapply(br_reference, function(e) e$scale, character(1)),
    hitopbr_devstats$camelCase
  )

  for (e in br_reference) {
    ## Each scale is probed below, at, and above its own reference mean.
    expect_lt(e$x[[1]], e$M)
    expect_identical(e$x[[2]], e$M)
    expect_gt(e$x[[3]], e$M)

    est <- e$M + sqrt(e$r) * (e$x - e$M)
    half95 <- z95 * e$SD * sqrt(1 - e$r)
    half80 <- z80 * e$SD * sqrt(1 - e$r)
    expect_equal(est, e$est, tolerance = 1e-14, info = e$scale)
    expect_equal(half95, e$half95, tolerance = 1e-14, info = e$scale)
    expect_equal(half80, e$half80, tolerance = 1e-14, info = e$scale)

    col <- paste0("hbr_", e$scale)
    scored <- stats::setNames(data.frame(e$x), col)

    got95 <- interval_hitopbr(scored, scores = col, append = FALSE)
    expect_equal(got95[[paste0(col, "_est")]], est, tolerance = 1e-8, info = e$scale)
    expect_equal(got95[[paste0(col, "_lo")]], est - half95, tolerance = 1e-8, info = e$scale)
    expect_equal(got95[[paste0(col, "_hi")]], est + half95, tolerance = 1e-8, info = e$scale)

    got80 <- interval_hitopbr(scored, scores = col, level = 0.80, append = FALSE)
    ## The estimate does not depend on the level; only the width does.
    expect_equal(got80[[paste0(col, "_est")]], est, tolerance = 1e-8, info = e$scale)
    expect_equal(got80[[paste0(col, "_lo")]], est - half80, tolerance = 1e-8, info = e$scale)
    expect_equal(got80[[paste0(col, "_hi")]], est + half80, tolerance = 1e-8, info = e$scale)

    ## The estimate is the observed score pulled toward the mean, and never past
    ## it -- the property that distinguishes Equation (10) from the observed
    ## score and from an unshrunk transformation of it.
    expect_gt(est[[1]], e$x[[1]])
    expect_lt(est[[1]], e$M)
    expect_lt(est[[3]], e$x[[3]])
    expect_gt(est[[3]], e$M)
  }
})

test_that("a bound outside the response range is returned, not clamped", {
  ## Every HiTOP-BR scale is skewed enough that a score at the response floor
  ## returns a lower bound below it. The floor is asserted for all eight rather
  ## than for one, because the help page states it of all eight.
  for (e in br_reference) {
    col <- paste0("hbr_", e$scale)
    got <- interval_hitopbr(
      stats::setNames(data.frame(1), col),
      scores = col,
      append = FALSE
    )
    expect_lt(got[[paste0(col, "_lo")]], 1, label = e$scale)
    expect_equal(
      got[[paste0(col, "_lo")]],
      e$M + sqrt(e$r) * (1 - e$M) - e$half95,
      tolerance = 1e-8,
      info = e$scale
    )
  }

  ## And at the ceiling, above 4.
  hi <- interval_hitopbr(
    data.frame(hbr_detachment = 4),
    scores = "hbr_detachment",
    append = FALSE
  )
  expect_gt(hi$hbr_detachment_hi, 4)
})

# ---- output shape ------------------------------------------------------------

test_that("three columns are returned per requested score, in order, appended or alone", {
  scored <- data.frame(
    id = 1:3,
    hbr_detachment = c(1, 2, 3),
    hbr_pFactor = c(2, 3, 4)
  )
  cols <- c("hbr_detachment", "hbr_pFactor")

  alone <- interval_hitopbr(scored, scores = cols, append = FALSE)
  expect_s3_class(alone, "tbl_df")
  expect_identical(
    names(alone),
    c(
      "hbr_detachment_est", "hbr_detachment_lo", "hbr_detachment_hi",
      "hbr_pFactor_est", "hbr_pFactor_lo", "hbr_pFactor_hi"
    )
  )
  expect_identical(nrow(alone), 3L)

  appended <- interval_hitopbr(scored, scores = cols)
  expect_identical(names(appended), c(names(scored), names(alone)))
  expect_identical(appended[names(scored)], tibble::as_tibble(scored))
})

test_that("an NA score returns NA in all three columns and leaves its neighbours alone", {
  got <- interval_hitopbr(
    data.frame(hbr_detachment = c(1.5, NA, 2.5)),
    scores = "hbr_detachment",
    append = FALSE
  )
  expect_identical(is.na(got$hbr_detachment_est), c(FALSE, TRUE, FALSE))
  expect_identical(is.na(got$hbr_detachment_lo), c(FALSE, TRUE, FALSE))
  expect_identical(is.na(got$hbr_detachment_hi), c(FALSE, TRUE, FALSE))
})

test_that("positions and names select the same columns", {
  scored <- data.frame(id = 1:2, hbr_detachment = c(1, 2))
  expect_identical(
    interval_hitopbr(scored, scores = 2, append = FALSE),
    interval_hitopbr(scored, scores = "hbr_detachment", append = FALSE)
  )
})

# ---- the two reported branches, asserted by class ----------------------------

test_that("a score column with no reference row is reported and returned as NA", {
  scored <- data.frame(hbr_detachment = 1.5, hbr_notAScale = 2)
  expect_warning(
    got <- interval_hitopbr(
      scored,
      scores = c("hbr_detachment", "hbr_notAScale"),
      append = FALSE
    ),
    class = "hitop_interval_uncovered"
  )
  expect_true(all(is.na(got$hbr_notAScale_est)))
  expect_true(all(is.na(got$hbr_notAScale_lo)))
  expect_true(all(is.na(got$hbr_notAScale_hi)))
  ## The covered column beside it is converted as usual.
  expect_false(is.na(got$hbr_detachment_est))

  ## The report names the HiTOP-BR table, not its HiTOP-SR sibling.
  expect_warning(
    interval_hitopbr(scored, scores = "hbr_notAScale", append = FALSE),
    "hitopbr_devstats"
  )

  ## A mistyped prefix leaves the whole name unstripped, so it reaches the same
  ## branch rather than silently matching nothing in particular.
  expect_warning(
    interval_hitopbr(
      data.frame(hbr_detachment = 1.5),
      scores = "hbr_detachment",
      prefix = "hbrr_",
      append = FALSE
    ),
    class = "hitop_interval_uncovered"
  )

  ## A HiTOP-SR scale stem is not a HiTOP-BR one, however it is prefixed: the
  ## two tables are separate and this function reads only its own.
  expect_warning(
    interval_hitopbr(
      data.frame(hbr_agoraphobia = 1.5),
      scores = "hbr_agoraphobia",
      append = FALSE
    ),
    class = "hitop_interval_uncovered"
  )
})

test_that("a response coding other than the reference one is reported and converts nothing", {
  scored <- data.frame(hbr_detachment = 1.5)
  ## Three shapes of mismatch, not one: the coding shifted, stretched, and
  ## narrowed. Each is a different way of not being c(1, 4).
  for (rng in list(c(0, 3), c(1, 5), c(1, 2))) {
    expect_warning(
      got <- interval_hitopbr(
        scored,
        scores = "hbr_detachment",
        srange = rng,
        append = FALSE
      ),
      class = "hitop_interval_coding"
    )
    expect_true(all(is.na(got$hbr_detachment_est)), info = paste(rng, collapse = "-"))
    expect_true(all(is.na(got$hbr_detachment_lo)), info = paste(rng, collapse = "-"))
    expect_true(all(is.na(got$hbr_detachment_hi)), info = paste(rng, collapse = "-"))
  }

  ## The report names the HiTOP-BR table, not its HiTOP-SR sibling.
  expect_warning(
    interval_hitopbr(scored, scores = "hbr_detachment", srange = c(0, 3),
                     append = FALSE),
    "hitopbr_devstats"
  )

  ## The reference coding itself is silent.
  expect_silent(
    interval_hitopbr(scored, scores = "hbr_detachment", append = FALSE)
  )
})

test_that("a call that is both uncovered and on the wrong coding raises both reports", {
  scored <- data.frame(hbr_detachment = 1.5, hbr_notAScale = 2)
  seen <- character(0)
  withCallingHandlers(
    interval_hitopbr(
      scored,
      scores = c("hbr_detachment", "hbr_notAScale"),
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
    interval_hitopbr(
      scored,
      scores = c("hbr_detachment", "hbr_notAScale"),
      srange = c(0, 3),
      append = FALSE
    )
  ))
})

# ---- error branches ----------------------------------------------------------

test_that("each argument is rejected with a message naming it", {
  scored <- data.frame(id = 1:2, hbr_detachment = c(1, 2))

  expect_error(
    interval_hitopbr("not a data frame", scores = "hbr_detachment"),
    "must be a data frame"
  )
  expect_error(
    interval_hitopbr(scored, scores = TRUE),
    "did not have the expected type"
  )
  expect_error(
    interval_hitopbr(scored, scores = character(0)),
    class = "hitop_empty_selection"
  )
  expect_error(
    interval_hitopbr(scored, scores = c("hbr_detachment", "hbr_detachment")),
    "distinct column"
  )
  expect_error(
    interval_hitopbr(scored, scores = "hbr_nope"),
    "must all be columns"
  )
  expect_error(
    interval_hitopbr(scored, scores = 99),
    "valid columns"
  )
  expect_error(
    interval_hitopbr(scored, scores = "hbr_detachment", srange = c(4, 1)),
    "greater than the first"
  )
  expect_error(
    interval_hitopbr(scored, scores = "hbr_detachment", srange = 1),
    "two integerish values"
  )
  expect_error(
    interval_hitopbr(scored, scores = "hbr_detachment", prefix = 1),
    "must be a single string"
  )
  expect_error(
    interval_hitopbr(scored, scores = "hbr_detachment", append = "yes"),
    "must be `TRUE` or `FALSE`"
  )
  expect_error(
    interval_hitopbr(
      cbind(scored, hbr_detachment_est = 0),
      scores = "hbr_detachment"
    ),
    class = "hitop_append_collision"
  )
})

test_that("the confidence level is rejected on its type and on its range, separately", {
  scored <- data.frame(hbr_detachment = 1.5)
  expect_error(
    interval_hitopbr(scored, scores = "hbr_detachment", level = "0.95"),
    "must be a single number"
  )
  expect_error(
    interval_hitopbr(scored, scores = "hbr_detachment", level = c(0.9, 0.95)),
    "must be a single number"
  )
  ## The mistake this message exists for: a percentage rather than a proportion.
  expect_error(
    interval_hitopbr(scored, scores = "hbr_detachment", level = 95),
    "proportion between 0 and 1"
  )
  expect_error(
    interval_hitopbr(scored, scores = "hbr_detachment", level = 0),
    "proportion between 0 and 1"
  )
  expect_error(
    interval_hitopbr(scored, scores = "hbr_detachment", level = 1),
    "proportion between 0 and 1"
  )
  ## An integer is a number on the right scale, so it is out of range rather
  ## than the wrong type -- the two messages must not swap places.
  expect_error(
    interval_hitopbr(scored, scores = "hbr_detachment", level = 1L),
    "proportion between 0 and 1"
  )
})

test_that("a factor or character score column is an error, not a coercion", {
  expect_error(
    interval_hitopbr(
      data.frame(hbr_detachment = factor(c("1", "2"))),
      scores = "hbr_detachment"
    ),
    "must be numeric"
  )
  expect_error(
    interval_hitopbr(
      data.frame(hbr_detachment = c("1", "2")),
      scores = "hbr_detachment"
    ),
    "must be numeric"
  )
  ## A logical column converts as it reads.
  expect_no_error(
    interval_hitopbr(
      data.frame(hbr_detachment = c(TRUE, FALSE)),
      scores = "hbr_detachment",
      append = FALSE
    )
  )
})

test_that("a wider level gives a wider interval and the same estimate", {
  scored <- data.frame(hbr_detachment = 2)
  narrow <- interval_hitopbr(scored, scores = "hbr_detachment",
                             level = 0.50, append = FALSE)
  wide <- interval_hitopbr(scored, scores = "hbr_detachment",
                           level = 0.99, append = FALSE)
  expect_equal(narrow$hbr_detachment_est, wide$hbr_detachment_est)
  expect_gt(
    wide$hbr_detachment_hi - wide$hbr_detachment_lo,
    narrow$hbr_detachment_hi - narrow$hbr_detachment_lo
  )
})
