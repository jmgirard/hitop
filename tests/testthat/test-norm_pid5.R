# Conversion tests for the PID-5 normative lookup.
#
# Ground truth is the shipped `pid_norms` table (itself verified cell-for-cell
# against Markon et al. 2024 by data-raw/verify_norms_against_book.R) plus
# expected values computed by hand from named printed cells. Nothing here
# asserts the function's own output as truth (IP2).

## The 16 version/scale pairs whose rows carry a T score, and the 4 that do not.
t_pairs <- unique(pid_norms[!is.na(pid_norms$tscore), c("version", "scale")])
p_pairs <- unique(pid_norms[c("version", "scale")])

## A tie run is a raw value printed in more than one row of a scale.
tie_raws <- function(rows) {
  tab <- table(rows$raw)
  as.numeric(names(tab)[tab > 1])
}

test_that("T -> raw reproduces every printed row exactly", {
  for (i in seq_len(nrow(t_pairs))) {
    v <- t_pairs$version[[i]]
    s <- t_pairs$scale[[i]]
    rows <- pid_norms[pid_norms$version == v & pid_norms$scale == s, ]
    expect_equal(
      norm_t_to_raw(rows$tscore, v, s),
      rows$raw,
      info = paste(v, s)
    )
  }
})

test_that("raw -> T -> raw is the identity on every printed row", {
  for (i in seq_len(nrow(t_pairs))) {
    v <- t_pairs$version[[i]]
    s <- t_pairs$scale[[i]]
    rows <- pid_norms[pid_norms$version == v & pid_norms$scale == s, ]
    got <- norm_convert(rows$raw, v, s)
    expect_equal(norm_t_to_raw(got$t, v, s), rows$raw, info = paste(v, s))
  }
})

test_that("raw -> percentile reproduces every printed row outside tie runs", {
  for (i in seq_len(nrow(p_pairs))) {
    v <- p_pairs$version[[i]]
    s <- p_pairs$scale[[i]]
    rows <- pid_norms[pid_norms$version == v & pid_norms$scale == s, ]
    keep <- !rows$raw %in% tie_raws(rows)
    expect_equal(
      norm_convert(rows$raw[keep], v, s)$ptl,
      rows$percentile[keep],
      info = paste(v, s)
    )
  }
})

test_that("inside a tie run the shared raw converts to the toward-50 row", {
  for (i in seq_len(nrow(t_pairs))) {
    v <- t_pairs$version[[i]]
    s <- t_pairs$scale[[i]]
    rows <- pid_norms[pid_norms$version == v & pid_norms$scale == s, ]
    for (r in tie_raws(rows)) {
      tied <- rows[rows$raw == r, ]
      want <- tied[which.min(abs(tied$tscore - 50)), ]
      got <- norm_convert(r, v, s)
      expect_equal(got$t, want$tscore, info = paste(v, s, r))
      expect_equal(got$ptl, want$percentile, info = paste(v, s, r))
    }
  }
})

test_that("floor-tie and midpoint fixtures match the printed cells", {
  # SF psychoticism prints raw 0.00 at T=30..42; the run's toward-50 row is
  # T=42, whose printed percentile is 0.31 (T=30..41 all print 0.00).
  expect_equal(norm_convert(0, "SF", "psychoticism"), list(t = 42L, ptl = 0.31))
  # BF psychoticism's run tops out at T=42 with a printed percentile of 0.00.
  expect_equal(norm_convert(0, "BF", "psychoticism"), list(t = 42L, ptl = 0.00))
  # BF total prints raw 0.00 at T=35..37.
  expect_equal(norm_convert(0, "BF", "total"), list(t = 37L, ptl = 0.00))
  # BF detachment: 0.20 (a 5-item mean, second-lowest attainable) is exactly
  # midway between printed raw 0.17 (T=42) and 0.23 (T=43) -> toward 50 = T=43.
  expect_equal(norm_convert(0.2, "BF", "detachment"), list(t = 43L, ptl = 0.35))
  # BF total: 0.76 is midway between 0.74 (T=54) and 0.78 (T=55) -> T=54.
  expect_equal(norm_convert(0.76, "BF", "total"), list(t = 54L, ptl = 0.71))
})

test_that("a maxed 0-3 score lands below the unattainable printed rows", {
  # Five scales print rows above the attainable 3.00 ceiling; a respondent at
  # 3.00 selects the nearest printed row below them, never the top row.
  expect_equal(norm_convert(3, "BF", "negativeAffectivity"), list(t = 84L, ptl = 1))
  expect_equal(norm_convert(3, "BF", "detachment"), list(t = 87L, ptl = 1))
  expect_equal(norm_convert(3, "BF", "disinhibition"), list(t = 93L, ptl = 1))
  expect_equal(norm_convert(3, "FULL", "negativeAffectivity"), list(t = 87L, ptl = 1))
  expect_equal(norm_convert(3, "SF", "negativeAffectivity"), list(t = 85L, ptl = 1))
})

test_that("a generic between-rows value takes the nearer printed row", {
  # FULL negativeAffectivity prints 0.52 at T=45 and 0.58 at T=46; 0.53 is
  # nearer 0.52.
  expect_equal(norm_convert(0.53, "FULL", "negativeAffectivity")$t, 45L)
  # SF detachment prints 0.06 at T=40 and 0.12 at T=41; 0.11 is nearer 0.12.
  expect_equal(norm_convert(0.11, "SF", "detachment")$t, 41L)
  # BF antagonism prints 0.44 at T=51 and 0.49 at T=52; 0.48 is nearer 0.49.
  expect_equal(norm_convert(0.48, "BF", "antagonism")$t, 52L)
})

test_that("out-of-table values cap to the nearest end rather than extrapolate", {
  for (i in seq_len(nrow(p_pairs))) {
    v <- p_pairs$version[[i]]
    s <- p_pairs$scale[[i]]
    rows <- pid_norms[pid_norms$version == v & pid_norms$scale == s, ]
    top <- rows[which.max(rows$raw), ]
    expect_equal(norm_convert(max(rows$raw) + 5, v, s)$ptl, top$percentile)
    # Below the floor, the answer agrees with an observed floor value.
    expect_equal(
      norm_convert(min(rows$raw) - 5, v, s),
      norm_convert(min(rows$raw), v, s),
      info = paste(v, s)
    )
  }
  # PRD is a 22-item sum reaching 66 while its table stops at 55; 60 caps.
  expect_equal(
    norm_convert(60, "FULL", "PRD")$ptl,
    pid_norms$percentile[pid_norms$scale == "PRD" & pid_norms$raw == 55]
  )
  expect_true(is.na(norm_convert(60, "FULL", "PRD")$t))
})

test_that("every integer in a validity table's printed range converts exactly", {
  for (s in c("INC", "ORS", "PRD", "INCS")) {
    rows <- pid_norms[pid_norms$scale == s, ]
    v <- rows$version[[1]]
    got <- norm_convert(rows$raw, v, s)
    expect_equal(got$ptl, rows$percentile, info = s)
    expect_true(all(is.na(got$t)), info = s)
  }
})

test_that("conversion is monotone nondecreasing in the observed score", {
  for (i in seq_len(nrow(p_pairs))) {
    v <- p_pairs$version[[i]]
    s <- p_pairs$scale[[i]]
    rows <- pid_norms[pid_norms$version == v & pid_norms$scale == s, ]
    grid <- seq(min(rows$raw), max(rows$raw), by = 0.01)
    got <- norm_convert(grid, v, s)
    expect_true(all(diff(got$ptl) >= 0), info = paste(v, s))
    if (!all(is.na(got$t))) {
      expect_true(all(diff(got$t) >= 0), info = paste(v, s))
    }
  }
})

test_that("the vectorized lookup agrees with an independent scalar lookup", {
  # Deliberately naive and separately coded: one observation at a time, an
  # explicit loop over the printed rows, no matrix algebra shared with the
  # implementation under test.
  naive <- function(x, rows) {
    best <- NA_integer_
    for (j in seq_len(nrow(rows))) {
      if (is.na(best)) {
        best <- j
        next
      }
      dj <- abs(x - rows$raw[[j]])
      db <- abs(x - rows$raw[[best]])
      middle <- function(k) {
        if (all(is.na(rows$tscore))) {
          abs(rows$percentile[[k]] - 0.5)
        } else {
          abs(rows$tscore[[k]] - 50)
        }
      }
      if (dj < db - norm_tol || (abs(dj - db) <= norm_tol && middle(j) < middle(best))) {
        best <- j
      }
    }
    c(t = rows$tscore[[best]], ptl = rows$percentile[[best]])
  }

  grid_for <- function(v, s, rows) {
    if (v == "BF" && s == "total") seq(0, 3, by = 0.04)
    else if (v == "BF") seq(0, 3, by = 0.2)
    else if (v == "SF" && !s %in% c("INCS")) (0:36) / 12
    else if (v == "FULL" && !s %in% c("INC", "ORS", "PRD")) {
      seq(min(rows$raw), max(rows$raw), by = 0.01)
    } else {
      rows$raw
    }
  }

  for (i in seq_len(nrow(p_pairs))) {
    v <- p_pairs$version[[i]]
    s <- p_pairs$scale[[i]]
    rows <- pid_norms[pid_norms$version == v & pid_norms$scale == s, ]
    grid <- grid_for(v, s, rows)
    got <- norm_convert(grid, v, s)
    want <- vapply(grid, function(x) naive(x, rows), numeric(2))
    expect_equal(as.numeric(got$t), as.numeric(want["t", ]), info = paste(v, s))
    expect_equal(got$ptl, as.numeric(want["ptl", ]), info = paste(v, s))
  }
})

test_that("NA scores convert to NA without affecting their neighbours", {
  got <- norm_convert(c(0.53, NA, 3), "FULL", "negativeAffectivity")
  expect_equal(got$t, c(45L, NA, 87L))
  expect_true(is.na(got$ptl[[2]]))
})

# ---- norm_pid5() ------------------------------------------------------------

scored_bf <- score_pid5(sim_pid5bf, items = 1:25, version = "BF")
bf_scales <- paste0("pid_", c("negativeAffectivity", "detachment", "antagonism",
                             "disinhibition", "psychoticism", "total"))

test_that("norm_pid5() returns a _t and a _ptl column per covered scale", {
  out <- norm_pid5(scored_bf, scores = bf_scales, version = "BF", append = FALSE)
  expect_s3_class(out, "tbl_df")
  expect_equal(names(out), as.vector(rbind(paste0(bf_scales, "_t"),
                                           paste0(bf_scales, "_ptl"))))
  expect_equal(nrow(out), nrow(scored_bf))
  expect_type(out[[1]], "integer")
  expect_type(out[[2]], "double")
})

test_that("norm_pid5() converts each column exactly as the primitive does", {
  out <- norm_pid5(scored_bf, scores = bf_scales, version = "BF", append = FALSE)
  for (s in bf_scales) {
    want <- norm_convert(scored_bf[[s]], "BF", sub("^pid_", "", s))
    expect_equal(out[[paste0(s, "_t")]], as.integer(want$t), info = s)
    expect_equal(out[[paste0(s, "_ptl")]], want$ptl, info = s)
  }
})

test_that("append = TRUE keeps the input columns ahead of the new ones", {
  out <- norm_pid5(scored_bf, scores = bf_scales, version = "BF")
  expect_equal(names(out)[seq_len(ncol(scored_bf))], names(scored_bf))
  expect_equal(ncol(out), ncol(scored_bf) + 2 * length(bf_scales))
})

test_that("validity scales get a percentile column but no T column", {
  scored <- score_pid5(sim_pid5, items = 1:220, version = "FULL")
  scored <- validity_pid5(scored, items = 1:220)
  out <- norm_pid5(scored, scores = c("pid_INC", "pid_ORS", "pid_PRD"),
                   version = "FULL", append = FALSE)
  expect_equal(names(out), c("pid_INC_ptl", "pid_ORS_ptl", "pid_PRD_ptl"))
})

test_that("an uncovered scale yields NA columns and one message naming it", {
  facets <- paste0("pid_", c("anhedonia", "anxiousness"))
  scored <- score_pid5(sim_pid5, items = 1:220, version = "FULL")
  expect_message(
    out <- norm_pid5(scored, scores = facets, version = "FULL", append = FALSE),
    "not covered"
  )
  expect_equal(names(out), c("pid_anhedonia_t", "pid_anhedonia_ptl",
                             "pid_anxiousness_t", "pid_anxiousness_ptl"))
  expect_true(all(vapply(out, function(x) all(is.na(x)), logical(1))))
})

test_that("a non-official srange refuses to convert and says so", {
  expect_message(
    out <- norm_pid5(scored_bf, scores = bf_scales, version = "BF",
                     srange = c(1, 4), append = FALSE),
    "official 0-3 response coding"
  )
  expect_true(all(vapply(out, function(x) all(is.na(x)), logical(1))))
  expect_equal(ncol(out), 2 * length(bf_scales))
})

test_that("capping is reported per end and does not extrapolate", {
  # PRD's table stops at 55; a 22-item sum can reach 66.
  df <- data.frame(pid_PRD = c(0, 55, 60, 66))
  expect_message(
    out <- norm_pid5(df, scores = "pid_PRD", version = "FULL", append = FALSE),
    "above the printed range"
  )
  top <- pid_norms$percentile[pid_norms$scale == "PRD" & pid_norms$raw == 55]
  expect_equal(out$pid_PRD_ptl, c(
    pid_norms$percentile[pid_norms$scale == "PRD" & pid_norms$raw == 0],
    top, top, top
  ))
})

test_that("NA scores pass through as NA in both columns", {
  df <- data.frame(pid_detachment = c(0.2, NA, 1))
  out <- norm_pid5(df, scores = "pid_detachment", version = "BF", append = FALSE)
  expect_equal(out$pid_detachment_t[[1]], 43L)
  expect_true(is.na(out$pid_detachment_t[[2]]))
  expect_true(is.na(out$pid_detachment_ptl[[2]]))
})

test_that("norm_pid5() rejects malformed input", {
  expect_error(norm_pid5(1:5, scores = "x", version = "BF"), "must be a data frame")
  expect_error(
    norm_pid5(scored_bf, scores = "pid_nope", version = "BF"),
    "must all be columns"
  )
  expect_error(
    norm_pid5(scored_bf, scores = bf_scales, version = "BF", srange = c(3, 0)),
    "must be greater"
  )
})

test_that("norm_pid5() handles the R edge cases", {
  # Column positions work as well as names (mirroring `items`).
  by_name <- norm_pid5(scored_bf, scores = bf_scales, version = "BF", append = FALSE)
  pos <- match(bf_scales, names(scored_bf))
  by_pos <- norm_pid5(scored_bf, scores = pos, version = "BF", append = FALSE)
  expect_equal(by_pos, by_name)

  # Zero rows in, zero rows out, with the columns still present.
  empty <- scored_bf[0, ]
  out <- norm_pid5(empty, scores = bf_scales, version = "BF", append = FALSE)
  expect_equal(nrow(out), 0L)
  expect_equal(ncol(out), 2 * length(bf_scales))

  # A single row and a single scale.
  one <- norm_pid5(scored_bf[1, ], scores = bf_scales[[1]], version = "BF",
                   append = FALSE)
  expect_equal(nrow(one), 1L)
  expect_equal(ncol(one), 2L)

  # An empty prefix leaves the column name as the scale name.
  df <- data.frame(detachment = 0.2)
  bare <- norm_pid5(df, scores = "detachment", version = "BF", prefix = "",
                    append = FALSE)
  expect_equal(names(bare), c("detachment_t", "detachment_ptl"))
  expect_equal(bare$detachment_t, 43L)

  # `version` is case-insensitive and rejects anything else.
  expect_equal(
    norm_pid5(df, scores = "detachment", version = "bf", prefix = "",
              append = FALSE),
    bare
  )
  expect_error(
    norm_pid5(df, scores = "detachment", version = "XX", prefix = ""),
    "should be one of"
  )
  expect_error(
    norm_pid5(df, scores = list("detachment"), version = "BF", prefix = ""),
    "did not have the expected type"
  )
})
