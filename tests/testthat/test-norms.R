# pid_norms: the PID-5 / PID-5-SF / PID-5-BF normative tables.
#
# Two kinds of check. The structural invariants hold across all 4,606 rows and
# would catch a systematic corruption of the dataset; the spot values are cells
# read by hand off the rendered pages and would catch a corruption that
# preserves the invariants. Page anchors are the book's own (Markon, Fossati,
# Somma & Krueger, 2024; see cairn/SOURCES.md); every cell of every table is
# additionally machine-verified against the source by
# data-raw/verify_norms_against_book.R.
#
# data-raw/mutate_norms_check.R reddens this file on demand: it applies each
# mutation these tests are meant to catch and reports which tests fail.

# Rows for one (version, scale), in ascending key order.
norm_rows <- function(version, scale) {
  x <- pid_norms[pid_norms$version == version & pid_norms$scale == scale, ]
  key <- if (all(is.na(x$tscore))) x$raw else x$tscore
  x[order(key), ]
}

# Every (version, scale) pair present, one row per pair.
norm_keys <- function(x = pid_norms) unique(x[c("version", "scale")])

domain_names <- c(
  "negativeAffectivity",
  "detachment",
  "antagonism",
  "disinhibition",
  "psychoticism"
)
validity_names <- c("INC", "INCS", "ORS", "PRD")

# The 25 facet stems for one version, from the scale-membership table rather
# than retyped -- pid_scales is what data-raw/norms_pid5.R maps the book's
# captions onto, so a facet renamed there must move the norm rows with it.
facet_names <- function(version) pid_scales[[version]]$camelCase


# ---- structure ---------------------------------------------------------------

test_that("pid_norms has the documented structure", {
  expect_s3_class(pid_norms, "tbl_df")
  expect_named(pid_norms, c("version", "scale", "tscore", "raw", "percentile"))
  expect_type(pid_norms$version, "character")
  expect_type(pid_norms$scale, "character")
  expect_type(pid_norms$tscore, "integer")
  expect_type(pid_norms$raw, "double")
  expect_type(pid_norms$percentile, "double")
  expect_setequal(unique(pid_norms$version), c("FULL", "SF", "BF"))
  expect_false(anyNA(pid_norms$raw))
  expect_false(anyNA(pid_norms$percentile))
  # No (version, scale) is listed twice at the same key.
  expect_equal(anyDuplicated(pid_norms[c("version", "scale", "tscore", "raw")]), 0)
})

test_that("pid_norms covers exactly the scales the nine shipped tables norm", {
  keys <- norm_keys()
  expect_setequal(
    paste(keys$version, keys$scale),
    c(
      paste("FULL", c(domain_names, facet_names("FULL"), "INC", "ORS", "PRD")),
      paste("SF", c(domain_names, facet_names("SF"), "INCS")),
      paste("BF", c("total", domain_names))
    )
  )
  # The domain and facet scale names are score_pid5() output column stems, so a
  # norming lookup can join on them with no crosswalk.
  expect_true(all(domain_names %in% pid_domains$camelCase))
  expect_length(facet_names("FULL"), 25L)
  expect_setequal(facet_names("FULL"), facet_names("SF"))
  # T scores are printed for the domain tables and for no validity table.
  expect_setequal(unique(pid_norms$scale[is.na(pid_norms$tscore)]), validity_names)
  expect_false(anyNA(pid_norms$tscore[!pid_norms$scale %in% validity_names]))
})

test_that("validity scale names are the package's, not the book's captions", {
  # The book captions both inconsistency tables "VRIN" and abbreviates the
  # Williams scale "PID-5-PRD"; the package keeps the scale-development papers'
  # names, which are the validity_pid5() column stems (D-018).
  expect_true(all(validity_names %in% pid_norms$scale))
  expect_false(any(c("VRIN", "VRINS", "INC-S", "PIM-RD") %in% pid_norms$scale))
  # Nor does shipping the norms rename any existing validity surface.
  expect_true(all(
    c("INC", "INCS", "ORS", "ORSS", "PRD", "PRDS", "SDTD", "SDTDS") %in%
      names(pid_items)
  ))
  expect_false(any(c("VRIN", "VRINS", "PIM-RD") %in% names(pid_items)))
})


# ---- structural invariants ---------------------------------------------------

# The rows of one column that lie strictly between its printed floor and its
# printed ceiling: raw above the run of 0.00 the book prints where the line goes
# negative, and, where the column's top raw repeats on consecutive T rows, below
# that run. Both runs are clamps rather than points on the line, so a line is
# fitted to neither.
norm_interior <- function(x) {
  top <- max(x$raw)
  x <- x[x$raw > 0, ]
  run <- x$tscore[x$raw == top]
  if (length(run) > 1) x <- x[x$tscore < min(run), ]
  x
}

# The smallest maximum absolute deviation of any straight line from the points
# (t, y) -- the Chebyshev, or minimax, fit. The optimal line is the midline of
# the narrowest vertical slab containing every point, and that slab always has a
# side flush with an edge of the points' convex hull, so its slope is the slope
# of some pair of points: scanning every pair finds it exactly. `lm` minimizes a
# different thing (squared error) and is not the right instrument for a bound on
# the *largest* deviation.
minimax_line_error <- function(t, y) {
  n <- length(t)
  best <- Inf
  for (i in seq_len(n - 1L)) {
    for (j in seq(i + 1L, n)) {
      if (t[[i]] == t[[j]]) next
      slope <- (y[[j]] - y[[i]]) / (t[[j]] - t[[i]])
      resid <- y - slope * t
      best <- min(best, max(resid) - min(resid))
    }
  }
  best / 2
}

test_that("raw scores are linear in T between the printed floor and ceiling", {
  # A T score is a linear rescaling of the raw metric, clamped at each end once
  # the line leaves the attainable range, so every printed raw between the
  # clamps sits on one line per scale. Values are printed to two decimals, so
  # the best line can be off by at most half a unit in the last place (0.005) on
  # any row -- which is the bound asserted, not a tolerance chosen to pass.
  keys <- norm_keys(pid_norms[!is.na(pid_norms$tscore), ])
  for (i in seq_len(nrow(keys))) {
    x <- norm_interior(norm_rows(keys$version[[i]], keys$scale[[i]]))
    expect_lt(
      minimax_line_error(x$tscore, x$raw),
      0.005,
      label = paste0(
        "largest raw-vs-line deviation for ",
        keys$version[[i]], " ", keys$scale[[i]]
      )
    )
  }
})

test_that("percentiles never decrease as the score rises", {
  keys <- norm_keys()
  for (i in seq_len(nrow(keys))) {
    x <- norm_rows(keys$version[[i]], keys$scale[[i]])
    drops <- which(diff(x$percentile) < 0)
    expect_equal(
      length(drops),
      0,
      label = paste0(
        "percentile decreases in ", keys$version[[i]], " ", keys$scale[[i]]
      )
    )
  }
})

test_that("a repeated top raw ships every printed row and converts to its lowest T", {
  # Nineteen facet columns print their top raw on several consecutive T rows,
  # because the line the book tabulated runs past the 3.00 an item mean can
  # reach and 4.00 is printed instead. Those rows are unattainable but they are
  # printed, so they ship verbatim (M33): the table is the published object, not
  # a filtered view of it. Selection then treats the run like the floor run --
  # the tie rule takes the end nearest the middle of the distribution, which at
  # the ceiling is the run's lowest T.
  keys <- norm_keys(pid_norms[!is.na(pid_norms$tscore), ])
  runs <- 0L
  for (i in seq_len(nrow(keys))) {
    v <- keys$version[[i]]
    s <- keys$scale[[i]]
    x <- norm_rows(v, s)
    where <- paste(v, s)

    # No T row is missing from any column, so no printed row was dropped.
    expect_equal(x$tscore, seq(min(x$tscore), max(x$tscore)), label = where)

    top <- x$tscore[x$raw == max(x$raw)]
    if (length(top) == 1L) next
    runs <- runs + 1L
    # The repeats are consecutive, so the run is one plateau and not a raw
    # value recurring in two places.
    expect_equal(top, seq(min(top), max(top)), label = paste("top run of", where))
    expect_equal(
      norm_convert(max(x$raw), v, s)$t, min(top),
      label = paste("T returned at the top raw of", where)
    )
  }
  expect_equal(runs, 19L)

  # The longest such run, named so a change in its length is visible here
  # rather than only in the count above.
  sf_anx <- norm_rows("SF", "anxiousness")
  expect_equal(sum(sf_anx$raw == 4), 12L)
  expect_equal(norm_convert(4, "SF", "anxiousness")$t, min(sf_anx$tscore[sf_anx$raw == 4]))
})

test_that("percentiles are proportions", {
  outside <- pid_norms$percentile < 0 | pid_norms$percentile > 1
  expect_equal(sum(outside), 0, label = "rows with a percentile outside [0, 1]")
})


# ---- spot values from the printed tables -------------------------------------
#
# The spot values are the only layer that catches a whole column displaced by
# one row -- the defect this dataset actually had before it was corrected. Such
# a shift leaves `raw` perfectly linear in T (the intercept simply moves by one
# step) and leaves a monotone percentile column monotone, so neither structural
# invariant above can see it. Every (version, scale) therefore needs at least
# one anchor, which the coverage test below enforces.

# version, scale, T score, raw, percentile, page
domain_spot <- local({
  spot <- rbind.data.frame(
    # Table A-5, self-report form domain scales (p. 120)
    list("FULL", "negativeAffectivity", 35L, 0.00, 0.00, 120),
    list("FULL", "negativeAffectivity", 50L, 0.81, 0.56, 120),
    list("FULL", "detachment", 65L, 1.57, 0.91, 120),
    list("FULL", "detachment", 72L, 1.94, 0.97, 120),
    list("FULL", "detachment", 76L, 2.15, 0.98, 120),
    list("FULL", "antagonism", 72L, 1.68, 0.97, 120),
    list("FULL", "disinhibition", 90L, 2.51, 1.00, 120),
    list("FULL", "psychoticism", 76L, 1.81, 0.97, 120),
    # Table A-7, 100-item short form domain scales (p. 147)
    list("SF", "negativeAffectivity", 50L, 0.75, 0.58, 147),
    list("SF", "detachment", 60L, 1.21, 0.83, 147),
    list("SF", "antagonism", 48L, 0.40, 0.52, 147),
    list("SF", "disinhibition", 90L, 2.54, 1.00, 147),
    list("SF", "psychoticism", 48L, 0.29, 0.58, 147),
    # Table A-9, brief form total score and domain scales (p. 174)
    list("BF", "total", 50L, 0.56, 0.59, 174),
    list("BF", "total", 95L, 2.60, 1.00, 174),
    list("BF", "negativeAffectivity", 70L, 2.05, 0.95, 174),
    list("BF", "detachment", 70L, 1.93, 0.96, 174),
    list("BF", "antagonism", 50L, 0.40, 0.56, 174),
    list("BF", "disinhibition", 70L, 1.69, 0.95, 174),
    list("BF", "psychoticism", 70L, 1.58, 0.94, 174),
    stringsAsFactors = FALSE
  )
  names(spot) <- c("version", "scale", "tscore", "raw", "percentile", "page")
  spot
})

# The facet anchors, one per new (version, scale) pair, all at T = 65 -- high
# enough to be clear of every column's 0.00 floor and below every ceiling run,
# so a one-row displacement moves the value on all 50. Read by eye off the
# rendered appendix pages, block by block, with each block's facet order taken
# from the banner row visible in the same view; the two tables lay their five
# blocks out in the same order, which is why the two halves below read alike.
# Pages are the tables' own first pages (A-6 begins p. 124, A-8 p. 151); T = 65
# falls a few pages into each.
facet_spot <- local({
  facets <- c(
    "anhedonia", "anxiousness", "attentionSeeking", "callousness",
    "deceitfulness", "depressivity", "distractibility", "eccentricity",
    "emotionalLability", "grandiosity", "hostility", "impulsivity",
    "intimacyAvoidance", "irresponsibility", "manipulativeness",
    "perceptualDysregulation", "perseveration", "restrictedAffectivity",
    "rigidPerfectionism", "riskTaking", "separationInsecurity",
    "submissiveness", "suspiciousness", "unusualBeliefsExperiences",
    "withdrawal"
  )
  # Table A-6, self-report form trait (facet) scales, at T = 65
  full_raw <- c(1.73, 2.01, 1.63, 0.98, 1.18, 1.26, 1.74, 1.78, 1.85, 1.56,
                1.77, 1.57, 1.49, 0.97, 1.67, 0.99, 1.61, 1.75, 2.00, 1.77,
                1.65, 2.13, 1.70, 1.31, 2.01)
  full_ptl <- c(0.89, 0.90, 0.92, 0.92, 0.92, 0.90, 0.91, 0.91, 0.89, 0.91,
                0.91, 0.90, 0.90, 0.90, 0.91, 0.89, 0.91, 0.93, 0.91, 0.92,
                0.90, 0.94, 0.93, 0.91, 0.92)
  # Table A-8, 100-item short form trait (facet) scales, at T = 65
  sf_raw <- c(1.59, 2.10, 1.81, 1.08, 1.10, 1.15, 1.88, 1.80, 1.72, 1.40,
              1.73, 1.64, 1.64, 0.91, 1.53, 0.67, 1.67, 1.92, 1.91, 1.32,
              1.85, 2.13, 1.53, 1.19, 1.91)
  sf_ptl <- c(0.92, 0.93, 0.91, 0.94, 0.94, 0.92, 0.92, 0.93, 0.91, 0.92,
              0.91, 0.91, 0.91, 0.93, 0.94, 0.92, 0.92, 0.90, 0.92, 0.93,
              0.91, 0.93, 0.94, 0.91, 0.94)
  data.frame(
    version = rep(c("FULL", "SF"), each = length(facets)),
    scale = rep(facets, 2),
    tscore = 65L,
    raw = c(full_raw, sf_raw),
    percentile = c(full_ptl, sf_ptl),
    page = rep(c(124, 151), each = length(facets)),
    stringsAsFactors = FALSE
  )
})

# Every anchor whose table prints a T score, domains and facets alike.
tscored_spot <- rbind(domain_spot, facet_spot)

# version, scale, raw score, percentile, page
validity_spot <- local({
  spot <- rbind.data.frame(
    # Table A-1, self-report form inconsistency scale (p. 116)
    list("FULL", "INC", 0, 0.030, 116),
    list("FULL", "INC", 8, 0.550, 116),
    list("FULL", "INC", 17, 0.950, 116),
    list("FULL", "INC", 23, 1.000, 116),
    # Table A-2, 100-item short form inconsistency scale (p. 117)
    list("SF", "INCS", 0, 0.078, 117),
    list("SF", "INCS", 8, 0.945, 117),
    list("SF", "INCS", 15, 1.000, 117),
    # Table A-3, over-reporting scale (p. 117)
    list("FULL", "ORS", 0, 0.889, 117),
    list("FULL", "ORS", 3, 0.996, 117),
    list("FULL", "ORS", 8, 1.000, 117),
    # Table A-4, PID-5-PRD (p. 118)
    list("FULL", "PRD", 0, 0.007, 118),
    list("FULL", "PRD", 10, 0.569, 118),
    list("FULL", "PRD", 21, 0.894, 118),
    list("FULL", "PRD", 55, 1.000, 118),
    stringsAsFactors = FALSE
  )
  names(spot) <- c("version", "scale", "raw", "percentile", "page")
  spot
})

test_that("every scale in pid_norms has at least one spot value", {
  anchored <- unique(rbind(
    tscored_spot[c("version", "scale")],
    validity_spot[c("version", "scale")]
  ))
  keys <- norm_keys()
  expect_setequal(
    paste(keys$version, keys$scale),
    paste(anchored$version, anchored$scale)
  )
})

test_that("domain and facet norms match the values printed in the book", {
  for (i in seq_len(nrow(tscored_spot))) {
    row <- tscored_spot[i, ]
    where <- paste0(row$version, " ", row$scale, " at T = ", row$tscore,
                    " (p. ", row$page, ")")
    got <- pid_norms[
      pid_norms$version == row$version &
        pid_norms$scale == row$scale &
        !is.na(pid_norms$tscore) & pid_norms$tscore == row$tscore,
    ]
    expect_equal(nrow(got), 1, label = paste("rows found for", where))
    expect_equal(got$raw, row$raw, label = paste("raw for", where))
    expect_equal(got$percentile, row$percentile,
                 label = paste("percentile for", where))
  }
})

test_that("validity norms match the values printed in the book", {
  for (i in seq_len(nrow(validity_spot))) {
    row <- validity_spot[i, ]
    where <- paste0(row$version, " ", row$scale, " at a score of ", row$raw,
                    " (p. ", row$page, ")")
    got <- pid_norms[
      pid_norms$version == row$version &
        pid_norms$scale == row$scale &
        pid_norms$raw == row$raw,
    ]
    expect_equal(nrow(got), 1, label = paste("rows found for", where))
    expect_true(is.na(got$tscore), label = paste("no T score printed for", where))
    expect_equal(got$percentile, row$percentile,
                 label = paste("percentile for", where))
  }
})

test_that("every pid_norms scale is produced by score_pid5() or validity_pid5()", {
  # Locks the documented claim in ?pid_norms that `scale` holds score-output
  # column stems with no crosswalk. This has now drifted twice: M25 shipped it
  # with `total` as a stated exception, and M26 removed the exception by adding
  # the BF total scorer. A future normed scale with no scorer fails here rather
  # than silently making the documentation false again.
  stems <- function(x) sub("^pid_", "", names(x))
  produced <- list(
    FULL = c(
      stems(score_pid5(sim_pid5, items = 1:220, version = "FULL", append = FALSE)),
      stems(suppressWarnings(
        validity_pid5(sim_pid5, items = 1:220, version = "FULL", append = FALSE)
      ))
    ),
    SF = c(
      stems(score_pid5(sim_pid5sf, items = 1:100, version = "SF", append = FALSE)),
      stems(suppressWarnings(
        validity_pid5(sim_pid5sf, items = 1:100, version = "SF", append = FALSE)
      ))
    ),
    BF = stems(score_pid5(sim_pid5bf, items = 1:25, version = "BF", append = FALSE))
  )

  for (v in names(produced)) {
    normed <- unique(pid_norms$scale[pid_norms$version == v])
    expect_gt(length(normed), 0)
    expect_setequal(setdiff(normed, produced[[v]]), character(0))
  }

  # The BF total specifically -- the scale M26 added a scorer for.
  expect_true("total" %in% pid_norms$scale[pid_norms$version == "BF"])
  expect_true("pid_total" %in% names(
    score_pid5(sim_pid5bf, items = 1:25, version = "BF", append = FALSE)
  ))
})
