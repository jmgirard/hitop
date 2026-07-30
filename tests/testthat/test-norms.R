# pid_norms: the PID-5 / PID-5-SF / PID-5-BF normative tables.
#
# Two kinds of check. The structural invariants hold across all 1,056 rows and
# would catch a systematic corruption of the dataset; the spot values are cells
# transcribed by hand from the printed tables and would catch a corruption that
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

test_that("pid_norms covers exactly the scales the seven shipped tables norm", {
  keys <- norm_keys()
  expect_setequal(
    paste(keys$version, keys$scale),
    c(
      paste("FULL", c(domain_names, "INC", "ORS", "PRD")),
      paste("SF", c(domain_names, "INCS")),
      paste("BF", c("total", domain_names))
    )
  )
  # The domain scale names are score_pid5() output column stems, so a norming
  # lookup can join on them with no crosswalk.
  expect_true(all(domain_names %in% pid_domains$camelCase))
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

test_that("domain raw scores are linear in T above the scale's zero floor", {
  # A T score is a linear rescaling of the raw metric, floored at zero once the
  # line drops below it, so every printed raw value sits on one line per scale.
  # Values are printed to two decimals, so the fitted line can be off by at most
  # half a unit in the last place (0.005) on any row.
  keys <- norm_keys(pid_norms[!is.na(pid_norms$tscore), ])
  for (i in seq_len(nrow(keys))) {
    x <- norm_rows(keys$version[[i]], keys$scale[[i]])
    above_floor <- x[x$raw > 0, ]
    fit <- stats::lm(raw ~ tscore, data = above_floor)
    predicted <- pmax(
      0,
      stats::coef(fit)[[1]] + stats::coef(fit)[[2]] * x$tscore
    )
    expect_lt(
      max(abs(predicted - x$raw)),
      0.006,
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

test_that("percentiles are proportions", {
  outside <- pid_norms$percentile < 0 | pid_norms$percentile > 1
  expect_equal(sum(outside), 0, label = "rows with a percentile outside [0, 1]")
})


# ---- spot values from the printed tables -------------------------------------

test_that("domain norms match the values printed in the book", {
  # version, scale, T score, raw, percentile, page
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
    list("BF", "psychoticism", 70L, 1.58, 0.94, 174),
    stringsAsFactors = FALSE
  )
  names(spot) <- c("version", "scale", "tscore", "raw", "percentile", "page")

  for (i in seq_len(nrow(spot))) {
    row <- spot[i, ]
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
  # version, scale, raw score, percentile, page
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

  for (i in seq_len(nrow(spot))) {
    row <- spot[i, ]
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
