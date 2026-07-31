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

  # The full attainable grid of each scale, which for the validity sums runs
  # well past the last printed row (INC 20 pairs x 3, INCS 10 x 3, ORS 10 items,
  # PRD 22 items x 3) and so exercises the capping branch too.
  grid_for <- function(v, s, rows) {
    if (s == "INC") 0:60
    else if (s == "INCS") 0:30
    else if (s == "ORS") 0:10
    else if (s == "PRD") 0:66
    else if (v == "BF" && s == "total") seq(0, 3, by = 0.04)
    else if (v == "BF") seq(0, 3, by = 0.2)
    else if (v == "SF") (0:36) / 12
    else seq(0, 3, by = 0.01)
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

test_that("norm_metric() classifies every scale the shipped tables carry", {
  for (i in seq_len(nrow(p_pairs))) {
    v <- p_pairs$version[[i]]
    s <- p_pairs$scale[[i]]
    expect_true(norm_metric(s, v) %in% c("mean", "sum", "invariant"),
                info = paste(v, s))
  }
  # And each lands in the metric its definition implies.
  expect_equal(
    norm_metric(c("detachment", "total", "PRD", "INC", "INCS", "ORS"), "FULL"),
    c("mean", "mean", "sum", "invariant", "invariant", "invariant")
  )
})

test_that("norm_metric() aborts on a covered scale it cannot classify", {
  # `pid_norms` is lazy data and cannot be rebound by local_mocked_bindings(),
  # so the hypothetical new row is injected through the coverage predicate
  # instead. PRDS and SDTD are real PID-5 validity scales with no normative
  # table today and no reconciliation formula either; were a table to arrive,
  # the old ifelse() would have handed them the item-mean shift in silence.
  local_mocked_bindings(norm_covers = function(version, scale) TRUE)
  expect_error(norm_metric("PRDS", "SF"), "PRDS")
  expect_error(norm_metric("SDTD", "FULL"), "no metric")
  # A scale it *can* classify is unaffected by the coverage answer.
  expect_equal(norm_metric("PRD", "FULL"), "sum")
})

test_that("a scale the tables do not cover classifies without aborting", {
  # The 25 facets are in no version's tables; they are item means, and their
  # metric is never used because an uncovered scale is never converted.
  expect_equal(norm_metric(c("anhedonia", "anxiousness"), "FULL"),
               c("mean", "mean"))
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

# ---- shifted response codings ----------------------------------------------
#
# Expected values below are hand-computed from the shift arithmetic and read off
# named printed cells of `pid_norms`; none is taken from norm_pid5()'s own output
# (IP2). The scales are version-pinned because the tables are: INC, ORS, and PRD
# are printed for the FULL form only and INC-S for the SF only.

## Run `expr`, collecting its warnings and returning them alongside its value.
## Warning text is whitespace-collapsed so assertions are not defeated by the
## line wrapping cli applies at the console width.
capture_warnings <- function(expr) {
  msgs <- character()
  value <- withCallingHandlers(
    expr,
    warning = function(w) {
      msgs <<- c(msgs, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  list(
    value = value,
    n = length(msgs),
    text = gsub("[[:space:]]+", " ", paste(msgs, collapse = " "))
  )
}

test_that("a coding with a different option count converts nothing", {
  got <- capture_warnings(
    norm_pid5(scored_bf, scores = bf_scales, version = "BF",
              srange = c(0, 4), append = FALSE)
  )
  expect_equal(got$n, 1L)
  expect_match(got$text, "implies 5 response options", fixed = TRUE)
  expect_true(all(vapply(got$value, function(x) all(is.na(x)), logical(1))))
  expect_equal(ncol(got$value), 2 * length(bf_scales))

  ## Fewer options is refused on the same footing as more.
  two <- capture_warnings(
    norm_pid5(scored_bf, scores = bf_scales, version = "BF",
              srange = c(0, 1), append = FALSE)
  )
  expect_match(two$text, "implies 2 response options", fixed = TRUE)
  expect_true(all(vapply(two$value, function(x) all(is.na(x)), logical(1))))
})

test_that("a shifted coding reconciles an item mean by `low`", {
  ## Five brief-form detachment items coded 1-4 whose mean is 2.20 are the same
  ## responses as a 0-3 mean of 2.20 - 1 = 1.20. The BF detachment table prints
  ## raw 1.18 at T 58 and raw 1.24 at T 59, and 1.20 is the nearer of the two to
  ## 1.18 (0.02 against 0.04), so the reconciled lookup lands on the T 58 row.
  row <- pid_norms[pid_norms$version == "BF" &
                     pid_norms$scale == "detachment" &
                     pid_norms$tscore == 58L, ]
  expect_equal(row$raw, 1.18)

  got <- capture_warnings(
    norm_pid5(data.frame(pid_detachment = 2.20), scores = "pid_detachment",
              version = "BF", srange = c(1, 4), append = FALSE)
  )
  expect_equal(got$value$pid_detachment_t, 58L)
  expect_equal(got$value$pid_detachment_ptl, row$percentile)

  ## The shift is what produced that row: read unreconciled, 2.20 is a far
  ## higher score than 1.20 and lands elsewhere in the table.
  unshifted <- norm_pid5(data.frame(pid_detachment = 2.20),
                         scores = "pid_detachment", version = "BF",
                         append = FALSE)
  expect_gt(unshifted$pid_detachment_t, 58L)
})

test_that("a shifted coding reconciles PRD by `low` x nItems", {
  ## PRD is a plain sum over its items, so the same responses coded 1-4 rather
  ## than 0-3 sum one point higher per item: a 0-3 sum of 30 is a 1-4 sum of
  ## 30 + 22 = 52.
  expect_equal(sum(!is.na(pid_items$PRD)), 22L)
  want <- pid_norms$percentile[pid_norms$version == "FULL" &
                                 pid_norms$scale == "PRD" &
                                 pid_norms$raw == 30]
  expect_equal(length(want), 1L)

  got <- capture_warnings(
    norm_pid5(data.frame(pid_PRD = 52), scores = "pid_PRD", version = "FULL",
              srange = c(1, 4), append = FALSE)
  )
  expect_equal(got$value$pid_PRD_ptl, want)
})

test_that("a PRD with a missing item stays NA through the shift correction", {
  ## validity_pid5() sums PRD with rowSums() and no `na.rm`
  ## (R/validity_pid5.R:172), so one unanswered item makes the whole sum NA.
  ## The `low x nItems` correction is therefore never applied to a partial sum:
  ## it is NA going in and NA coming out, and both conversion columns are NA.
  prd_cols <- pid_items$FULL[!is.na(pid_items$PRD)]
  holed <- sim_pid5
  holed[1, prd_cols[[1]]] <- NA
  scored <- suppressMessages(
    validity_pid5(score_pid5(holed, items = 1:220), items = 1:220)
  )
  expect_true(is.na(scored$pid_PRD[[1]]))
  expect_false(any(is.na(scored$pid_PRD[-1])))

  out <- suppressMessages(suppressWarnings(
    norm_pid5(scored, scores = "pid_PRD", version = "FULL", srange = c(1, 4),
              append = FALSE)
  ))
  expect_true(is.na(out$pid_PRD_ptl[[1]]))
  expect_false(any(is.na(out$pid_PRD_ptl[-1])))
})

test_that("INC, INC-S, and ORS are unchanged by a shifted coding", {
  ## INC and INC-S sum absolute differences within item pairs, which a constant
  ## added to both members cancels out of; ORS counts items answered at the top
  ## of the response range, which moves with the range. All three keep the score
  ## they were given, so each converts to the cell printed at that raw.
  cases <- list(
    list(version = "FULL", scale = "INC", raw = 12),
    list(version = "SF", scale = "INCS", raw = 5),
    list(version = "FULL", scale = "ORS", raw = 2)
  )
  for (case in cases) {
    col <- paste0("pid_", case$scale)
    want <- pid_norms$percentile[pid_norms$version == case$version &
                                   pid_norms$scale == case$scale &
                                   pid_norms$raw == case$raw]
    expect_equal(length(want), 1L, info = case$scale)
    df <- stats::setNames(data.frame(case$raw), col)
    got <- capture_warnings(
      norm_pid5(df, scores = col, version = case$version, srange = c(1, 4),
                append = FALSE)
    )
    expect_equal(got$value[[paste0(col, "_ptl")]], want, info = case$scale)
  }
})

test_that("the reconciliation is reported once, naming both groups", {
  df <- data.frame(pid_detachment = 2.20, pid_PRD = 52, pid_ORS = 2)
  got <- capture_warnings(
    suppressMessages(
      norm_pid5(df, scores = names(df), version = "FULL", srange = c(1, 4),
                append = FALSE)
    )
  )
  expect_equal(got$n, 1L)
  expect_match(got$text, "reconciled", fixed = TRUE)
  expect_match(got$text, "Adjusted", fixed = TRUE)
  expect_match(got$text, "unchanged", fixed = TRUE)
  for (nm in names(df)) {
    expect_match(got$text, nm, fixed = TRUE)
  }
})

test_that("an all-invariant request is not reported as an adjustment", {
  ## Every requested scale is coding-invariant, so nothing was reconciled and
  ## the report must not claim otherwise.
  got <- capture_warnings(
    norm_pid5(data.frame(pid_INC = 12), scores = "pid_INC", version = "FULL",
              srange = c(1, 4), append = FALSE)
  )
  expect_equal(got$n, 1L)
  expect_match(got$text, "needed no reconciliation", fixed = TRUE)
  expect_false(grepl("Adjusted", got$text, fixed = TRUE))
})

test_that("a request the tables cover nowhere reports coverage, not coding", {
  ## The facets are in no version's tables, so there is nothing to reconcile and
  ## the coverage message is the whole story.
  got <- capture_warnings(
    suppressMessages(
      norm_pid5(data.frame(pid_anhedonia = 2.2), scores = "pid_anhedonia",
                version = "FULL", srange = c(1, 4), append = FALSE)
    )
  )
  expect_equal(got$n, 0L)
  expect_message(
    norm_pid5(data.frame(pid_anhedonia = 2.2), scores = "pid_anhedonia",
              version = "FULL", srange = c(1, 4), append = FALSE),
    "not covered"
  )
})

test_that("the official coding says nothing about response coding", {
  expect_no_warning(
    norm_pid5(data.frame(pid_detachment = 1.20), scores = "pid_detachment",
              version = "BF", srange = c(0, 3), append = FALSE)
  )
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

test_that("norm_pid5() rejects a duplicated `scores` entry", {
  # Without the guard, `data[scores]` silently returns the column twice under
  # base R's de-duplicated names and the output carries two near-identical
  # conversion pairs.
  expect_error(
    norm_pid5(scored_bf, scores = c("pid_detachment", "pid_detachment"),
              version = "BF"),
    "distinct column"
  )
})

test_that("norm_pid5() aborts on a non-numeric score column rather than coercing", {
  # A factor would be coerced to its integer codes and a character column to
  # NA; both are wrong answers rather than errors, so both abort naming the
  # column.
  fct <- data.frame(pid_detachment = factor("0.2"))
  expect_error(
    norm_pid5(fct, scores = "pid_detachment", version = "BF"),
    "must be numeric"
  )
  chr <- data.frame(pid_detachment = "0.2")
  expect_error(
    norm_pid5(chr, scores = "pid_detachment", version = "BF"),
    "must be numeric"
  )
  # Both offenders are named in one abort rather than one per call.
  both <- data.frame(pid_detachment = factor("0.2"), pid_antagonism = "0.5")
  expect_error(
    norm_pid5(both, scores = names(both), version = "BF"),
    "pid_antagonism"
  )

  # A logical column is left alone: as.numeric(TRUE) is 1, which is what a 0/1
  # indicator already means.
  lgl <- data.frame(pid_detachment = c(TRUE, FALSE))
  out <- norm_pid5(lgl, scores = "pid_detachment", version = "BF",
                   append = FALSE)
  expect_equal(
    out$pid_detachment_t,
    as.integer(c(norm_convert(1, "BF", "detachment")$t,
                 norm_convert(0, "BF", "detachment")$t))
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

test_that("`prefix` is stripped by literal match, never as a regex", {
  # A metacharacter-bearing prefix that *is* the literal start of the column
  # name: compiled as a pattern this aborted with "invalid regular expression
  # '^pid(_'", naming a regex the caller never wrote.
  df <- stats::setNames(data.frame(0.2), "pid(_detachment")
  out <- norm_pid5(df, scores = "pid(_detachment", version = "BF",
                   prefix = "pid(_", append = FALSE)
  expect_equal(out[["pid(_detachment_t"]], 43L)
  expect_equal(out[["pid(_detachment_ptl"]], 0.35)

  # A `.` no longer matches an arbitrary character: "pXd_detachment" does not
  # start with the literal "p.d_", so the name is left unstripped, no scale
  # matches it, and both conversion columns come back NA with the column named.
  df2 <- stats::setNames(data.frame(0.2), "pXd_detachment")
  expect_message(
    out2 <- norm_pid5(df2, scores = "pXd_detachment", version = "BF",
                      prefix = "p.d_", append = FALSE),
    "pXd_detachment",
    fixed = TRUE
  )
  expect_true(is.na(out2[["pXd_detachment_t"]]))
  expect_true(is.na(out2[["pXd_detachment_ptl"]]))
})

test_that("every validity scale caps above its printed range, not just PRD", {
  # Each of the four is a sum whose attainable maximum exceeds its last printed
  # row: INC 20 pairs x 3 = 60 against 23, INCS 10 x 3 = 30 against 15, ORS 10
  # items against 8, PRD 22 x 3 = 66 against 55.
  cases <- list(
    list(v = "FULL", s = "INC", above = 24:60),
    list(v = "SF", s = "INCS", above = 16:30),
    list(v = "FULL", s = "ORS", above = 9:10),
    list(v = "FULL", s = "PRD", above = 56:66)
  )
  for (k in cases) {
    rows <- pid_norms[pid_norms$version == k$v & pid_norms$scale == k$s, ]
    top <- rows$percentile[which.max(rows$raw)]
    got <- norm_convert(k$above, k$v, k$s)
    expect_equal(got$ptl, rep(top, length(k$above)), info = k$s)
    expect_true(all(is.na(got$t)), info = k$s)
    # And the wrapper counts them, one message naming both ends.
    df <- data.frame(x = k$above)
    names(df) <- paste0("pid_", k$s)
    expect_message(
      norm_pid5(df, scores = names(df), version = k$v, append = FALSE),
      paste0(length(k$above), " above the printed range")
    )
  }
})

test_that("non-finite and enormous scores cap rather than landing mid-table", {
  # Every printed raw is equally far from an infinite observation, so an
  # unclamped nearest-row search would call them all candidates and return the
  # tie-break winner near T=50 instead of the end row.
  rows <- pid_norms[pid_norms$version == "BF" & pid_norms$scale == "detachment", ]
  top <- rows[which.max(rows$raw), ]
  at_floor <- norm_convert(0, "BF", "detachment")
  got <- norm_convert(c(Inf, -Inf, 1e16, -1e16), "BF", "detachment")
  expect_equal(got$t, c(top$tscore, at_floor$t, top$tscore, at_floor$t))
  expect_equal(got$ptl, c(top$percentile, at_floor$ptl, top$percentile, at_floor$ptl))
})

test_that("capping counts observations, not observation-by-scale pairs", {
  # One respondent, three scales out of range at the same end: one observation.
  df <- data.frame(pid_INC = 30, pid_PRD = 60, pid_ORS = 9)
  expect_message(
    norm_pid5(df, scores = names(df), version = "FULL", append = FALSE),
    "0 observations below and 1 above"
  )
  # Two respondents, one capped on two scales and one on neither: still one.
  df2 <- data.frame(pid_INC = c(30, 1), pid_PRD = c(60, 1))
  expect_message(
    norm_pid5(df2, scores = names(df2), version = "FULL", append = FALSE),
    "0 observations below and 1 above"
  )
})
