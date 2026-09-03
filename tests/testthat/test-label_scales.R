# Tests for the labeling and ranking utilities (milestone M005):
# label_hitopsr(), label_hitopbr(), and rank_scales(). These go beyond the
# incidental coverage in the end-to-end pipeline test.

# ---- label_hitopsr() --------------------------------------------------------

test_that("label_hitopsr() attaches item text and scale names", {
  # Items: a frame named HSR_001 .. HSR_405 is matched under prefix = "HSR_".
  df_items <- as.data.frame(matrix(1, nrow = 2, ncol = 405))
  names(df_items) <- sprintf("HSR_%03d", 1:405)
  labeled <- label_hitopsr(df_items, target = "items", prefix = "HSR_")
  expect_identical(
    attr(labeled$HSR_001, "label"),
    hitopsr_items$Text[hitopsr_items$HSR == 1]
  )
  expect_identical(
    attr(labeled$HSR_002, "label"),
    hitopsr_items$Text[hitopsr_items$HSR == 2]
  )

  # Scales: a scale column gets its clean Scale name as a label.
  df_scales <- data.frame(HSR_agoraphobia = c(1, 2))
  labeled_s <- label_hitopsr(df_scales, target = "scales", prefix = "HSR_")
  expect_identical(
    attr(labeled_s$HSR_agoraphobia, "label"),
    hitopsr_scales$Scale[hitopsr_scales$camelCase == "agoraphobia"]
  )
})

test_that("label_hitopsr() warns and returns data unchanged on no match", {
  df <- data.frame(foo = c(1, 2), bar = c(3, 4))
  expect_warning(
    res <- label_hitopsr(df, target = "items"),
    class = "hitop_no_columns_matched"
  )
  expect_identical(res, df)
})

# ---- label_hitopbr() --------------------------------------------------------

test_that("label_hitopbr() attaches item text and scale names", {
  df_items <- data.frame(HBR_01 = c(1, 2), HBR_02 = c(3, 4))
  labeled <- label_hitopbr(df_items, target = "items", prefix = "HBR_")
  expect_identical(
    attr(labeled$HBR_01, "label"),
    hitopbr_items$Text[hitopbr_items$HBR == 1]
  )

  df_scales <- data.frame(HBR_antagonism = c(1, 2))
  labeled_s <- label_hitopbr(df_scales, target = "scales", prefix = "HBR_")
  expect_identical(
    attr(labeled_s$HBR_antagonism, "label"),
    hitopbr_scales$Scale[hitopbr_scales$camelCase == "antagonism"]
  )
})

test_that("label_hitopbr() warns and returns data unchanged on no match", {
  df <- data.frame(foo = c(1, 2))
  expect_warning(
    res <- label_hitopbr(df, target = "scales"),
    class = "hitop_no_columns_matched"
  )
  expect_identical(res, df)
})

# ---- rank_scales() ----------------------------------------------------------

test_that("rank_scales() ranks high/low and strips the prefix", {
  df <- data.frame(
    hbr_a = c(3, 1),
    hbr_b = c(1, 2),
    hbr_c = c(2, 3)
  )
  scales <- c("hbr_a", "hbr_b", "hbr_c")

  high <- rank_scales(df, scales, prefix = "hbr_", top = 2, dir = "high", append = FALSE)
  #   row1: a=3,b=1,c=2 -> a,c ; row2: a=1,b=2,c=3 -> c,b
  expect_s3_class(high, "tbl_df")
  expect_equal(high$top_scales, c("a,c", "c,b"))

  low <- rank_scales(df, scales, prefix = "hbr_", top = 2, dir = "low", append = FALSE)
  #   row1: -> b,c ; row2: -> a,b
  expect_equal(low$top_scales, c("b,c", "a,b"))
})

test_that("rank_scales() strips `prefix` by literal match, never as a regex", {
  # A metacharacter-bearing prefix that *is* the literal start of the names:
  # compiled as a pattern this aborted with "invalid regular expression".
  df <- stats::setNames(data.frame(x = 3, y = 1), c("hbr(_a", "hbr(_b"))
  out <- rank_scales(df, names(df), prefix = "hbr(_", top = 1, append = FALSE)
  expect_equal(out$top_scales, "a")

  # A `.` no longer matches an arbitrary character: these names do not start
  # with the literal "h.r_", so nothing is stripped.
  df2 <- stats::setNames(data.frame(x = 3, y = 1), c("hXr_a", "hXr_b"))
  out2 <- rank_scales(df2, names(df2), prefix = "h.r_", top = 1, append = FALSE)
  expect_equal(out2$top_scales, "hXr_a")
})

test_that("rank_scales() resolves ties by original column order", {
  df <- data.frame(hbr_a = 2, hbr_b = 2, hbr_c = 1)
  out <- rank_scales(
    df, c("hbr_a", "hbr_b", "hbr_c"), prefix = "hbr_", top = 1,
    dir = "high", append = FALSE
  )
  # a and b tie at 2; order() keeps the earlier column (a).
  expect_equal(out$top_scales, "a")
})

test_that("rank_scales() names the output column via `name` and validates `top`", {
  df <- data.frame(x = c(3, 1), y = c(1, 2))

  # default name
  appended <- rank_scales(df, c("x", "y"), top = 1, append = TRUE)
  expect_s3_class(appended, "tbl_df")
  expect_true("top_scales" %in% names(appended))
  expect_equal(nrow(appended), nrow(df))
  expect_true(all(names(df) %in% names(appended)))

  # custom name, both appended and standalone
  named <- rank_scales(df, c("x", "y"), top = 1, name = "elevated")
  expect_true("elevated" %in% names(named))
  standalone <- rank_scales(df, c("x", "y"), top = 1, name = "elevated", append = FALSE)
  expect_identical(names(standalone), "elevated")

  # top must not exceed the number of ranked scales.
  expect_error(rank_scales(df, c("x", "y"), top = 3), NULL)
})

test_that("rank_scales() reflects reverse-directioned scales before ranking", {
  # wellBeing is keyed opposite (higher = healthier). With reverse + srange it is
  # reflected onto a common 'higher = more elevated' metric before ranking.
  # srange = c(1, 4) -> reflect via 5 - value.
  df <- data.frame(
    hsr_anxiety   = c(2, 2),
    hsr_wellBeing = c(4, 1)  # row1: healthy (high wb) ; row2: low wb (elevated)
  )
  scales <- c("hsr_anxiety", "hsr_wellBeing")

  # Without reflection, dir = "high" surfaces the raw-highest scale: wellBeing in
  # row1 (4 > 2). WITH reflection, wellBeing becomes 5-4 = 1 (row1) and 5-1 = 4
  # (row2), so the top scale is anxiety in row1 and wellBeing in row2.
  ranked <- rank_scales(
    df, scales, prefix = "hsr_", top = 1, dir = "high",
    reverse = "hsr_wellBeing", srange = c(1, 4), append = FALSE
  )
  expect_equal(ranked$top_scales, c("anxiety", "wellBeing"))

  # A high well-being respondent (row1) is NOT flagged as most-elevated.
  expect_false(ranked$top_scales[1] == "wellBeing")

  # `reverse` must be a subset of `scales`, and `srange` is required with it.
  expect_error(
    rank_scales(df, scales, top = 1, reverse = "hsr_missing", srange = c(1, 4)),
    "must all be in"
  )
  expect_error(
    rank_scales(df, scales, top = 1, reverse = "hsr_wellBeing"),
    "srange"
  )
})

# ---- default prefix matches the shipped datasets and scored output ----------

test_that("label_hitopsr() labels ku_hitopsr items 1, 10 and 405 with no prefix given", {
  labeled <- label_hitopsr(ku_hitopsr, target = "items")
  for (n in c(1, 10, 405)) {
    expect_identical(
      attr(labeled[[sprintf("hsr_%03d", n)]], "label"),
      hitopsr_items$Text[hitopsr_items$HSR == n]
    )
  }
  # Non-item columns are left unlabeled.
  expect_null(attr(labeled$participant, "label"))
})

test_that("label_hitopsr(target = 'scales') labels score_hitopsr()'s default output with no prefix given", {
  scored <- score_hitopsr(sim_hitopsr, items = 1:405, append = FALSE)
  labeled <- label_hitopsr(scored, target = "scales")
  expect_identical(
    vapply(labeled, function(col) attr(col, "label"), character(1), USE.NAMES = FALSE),
    hitopsr_scales$Scale[match(sub("^hsr_", "", names(scored)), hitopsr_scales$camelCase)]
  )
})

test_that("label_hitopbr() labels ku_hitopbr items 1, 10 and 45 with no prefix given", {
  labeled <- label_hitopbr(ku_hitopbr, target = "items")
  for (n in c(1, 10, 45)) {
    expect_identical(
      attr(labeled[[sprintf("hbr_%02d", n)]], "label"),
      hitopbr_items$Text[hitopbr_items$HBR == n]
    )
  }
  expect_null(attr(labeled$participant, "label"))
})

# ---- unpadded item numbers under the prefix are reported, not silently skipped ----

test_that("label_hitopsr() warns when prefixed columns carry unpadded item numbers", {
  df <- as.data.frame(matrix(0, nrow = 1, ncol = 405))
  names(df) <- paste0("HSR_", 1:405)
  expect_warning(
    res <- label_hitopsr(df, prefix = "HSR_"),
    class = "hitop_unpadded_items"
  )
  # Items 100 and up match regardless of padding and are labelled; the rest are not.
  expect_identical(attr(res$HSR_405, "label"), hitopsr_items$Text[hitopsr_items$HSR == 405])
  expect_null(attr(res[[names(df)[1]]], "label"))
  # A padded subset (a module's items) is not a padding problem: no warning.
  sub <- ku_hitopsr[, c("participant", "hsr_001", "hsr_002")]
  expect_no_warning(label_hitopsr(sub))
  # Nothing matching at all still reports that, not padding.
  expect_warning(label_hitopsr(data.frame(a = 1)), class = "hitop_no_columns_matched")
})

test_that("label_hitopbr() warns when prefixed columns carry unpadded item numbers", {
  df <- as.data.frame(matrix(0, nrow = 1, ncol = 45))
  names(df) <- paste0("HBR_", 1:45)
  expect_warning(
    res <- label_hitopbr(df, prefix = "HBR_"),
    class = "hitop_unpadded_items"
  )
  expect_identical(attr(res$HBR_45, "label"), hitopbr_items$Text[hitopbr_items$HBR == 45])
  expect_null(attr(res[[names(df)[1]]], "label"))
  sub <- ku_hitopbr[, c("participant", "hbr_01", "hbr_02")]
  expect_no_warning(label_hitopbr(sub))
  expect_warning(label_hitopbr(data.frame(a = 1)), class = "hitop_no_columns_matched")
})

# ---- AC1: the padding report fires even when no column matched --------------

test_that("label_hitopsr() reports mis-padded columns when nothing matched", {
  # A spelling WIDER than the instrument's three digits: `hsr_1`..`hsr_405`
  # would still match items 100 and up, so it never reaches the no-match path.
  cols <- sprintf("hsr_%04d", 1:5)
  df <- frame_of_cols(cols)

  caught <- collect_warnings(label_hitopsr(df, target = "items"))
  expect_length(caught$warnings, 2L)
  if (length(caught$warnings) == 2L) {
    expect_s3_class(caught$warnings[[1]], "hitop_no_columns_matched")
    expect_s3_class(caught$warnings[[2]], "hitop_unpadded_items")
    msg <- warning_text(caught, 2L)
    for (nm in cols) expect_true(grepl(nm, msg, fixed = TRUE), info = nm)
  }
  expect_identical(caught$value, df)
})

test_that("label_hitopbr() reports mis-padded columns when nothing matched", {
  cols <- paste0("hbr_", 1:5)
  df <- frame_of_cols(cols)

  caught <- collect_warnings(label_hitopbr(df, target = "items"))
  expect_length(caught$warnings, 2L)
  if (length(caught$warnings) == 2L) {
    expect_s3_class(caught$warnings[[1]], "hitop_no_columns_matched")
    expect_s3_class(caught$warnings[[2]], "hitop_unpadded_items")
    msg <- warning_text(caught, 2L)
    for (nm in cols) expect_true(grepl(nm, msg, fixed = TRUE), info = nm)
  }
  expect_identical(caught$value, df)
})

# ---- AC2/AC3: two kinds of unlabelled column, two sentences -----------------

test_that("label_hitopsr() sorts mis-padded and out-of-range columns into their own sentences", {
  caught <- collect_warnings(
    label_hitopsr(frame_of_cols(c("hsr_001", "hsr_3", "hsr_406")), target = "items")
  )
  expect_length(caught$warnings, 1L)
  expect_s3_class(caught$warnings[[1]], "hitop_unpadded_items")

  msg <- squashed_warning(caught)
  mis_head <- sentence_pos(msg, "not zero-padded to")
  oor_head <- sentence_pos(msg, "outside the range 1 to")
  expect_true(mis_head > 0)
  expect_true(oor_head > mis_head)
  expect_true(sentence_pos(msg, "hsr_3") > mis_head && sentence_pos(msg, "hsr_3") < oor_head)
  expect_true(sentence_pos(msg, "hsr_406") > oor_head)
  expect_true(grepl("expected as `hsr_003`", msg, fixed = TRUE))
  expect_false(grepl("expected as `hsr_406`", msg, fixed = TRUE))
  expect_true(grepl("outside the range 1 to 405", msg, fixed = TRUE))
  expect_false(is.null(attr(caught$value$hsr_001, "label")))
})

test_that("label_hitopbr() sorts mis-padded and out-of-range columns into their own sentences", {
  caught <- collect_warnings(
    label_hitopbr(frame_of_cols(c("hbr_01", "hbr_3", "hbr_46")), target = "items")
  )
  expect_length(caught$warnings, 1L)
  expect_s3_class(caught$warnings[[1]], "hitop_unpadded_items")

  msg <- squashed_warning(caught)
  mis_head <- sentence_pos(msg, "not zero-padded to")
  oor_head <- sentence_pos(msg, "outside the range 1 to")
  expect_true(mis_head > 0)
  expect_true(oor_head > mis_head)
  expect_true(sentence_pos(msg, "hbr_3") > mis_head && sentence_pos(msg, "hbr_3") < oor_head)
  expect_true(sentence_pos(msg, "hbr_46") > oor_head)
  expect_true(grepl("expected as `hbr_03`", msg, fixed = TRUE))
  expect_false(grepl("expected as `hbr_46`", msg, fixed = TRUE))
  expect_true(grepl("outside the range 1 to 45", msg, fixed = TRUE))
  expect_false(is.null(attr(caught$value$hbr_01, "label")))
})

test_that("the label helpers report an out-of-range column on a frame carrying nothing else wrong", {
  # No mis-padded column at all, so the report is the out-of-range sentence
  # alone -- and carries no "expected as" hint to point anywhere.
  for (case in list(
    list(fn = function(d) label_hitopsr(d, target = "items"),
         cols = c("hsr_001", "hsr_406"), range = "1 to 405"),
    list(fn = function(d) label_hitopbr(d, target = "items"),
         cols = c("hbr_01", "hbr_46"), range = "1 to 45")
  )) {
    caught <- collect_warnings(case$fn(frame_of_cols(case$cols)))
    expect_length(caught$warnings, 1L)
    msg <- squashed_warning(caught)
    expect_true(grepl(paste0("outside the range ", case$range), msg, fixed = TRUE),
      info = case$cols[2])
    expect_false(grepl("not zero-padded", msg, fixed = TRUE), info = case$cols[2])
    expect_false(grepl("expected as", msg, fixed = TRUE), info = case$cols[2])
  }
})

# ---- AC5: frames spelled correctly raise no padding report ------------------

test_that("the label helpers stay silent on correctly padded frames", {
  # `withCallingHandlers` rather than `expect_no_warning(message = )`, which
  # reads `message` as a selector and passes on a warning it did not select
  # (M032). Every warning is caught, and the class is asserted absent.
  no_padding_report <- function(expr, info) {
    caught <- collect_warnings(expr)
    classed <- vapply(
      caught$warnings,
      function(w) inherits(w, "hitop_unpadded_items"),
      logical(1)
    )
    expect_false(any(classed), info = info)
    caught
  }

  # The shipped item frames, whose columns are padded to each instrument's
  # width. Each is also shown to label something, so the silence above is the
  # helper finding nothing to report rather than the frame carrying no item
  # column the helper ever looked at.
  shipped <- list(
    sim_pid5 = function() label_pid5(sim_pid5, target = "items", version = "FULL"),
    sim_pid5sf = function() label_pid5(sim_pid5sf, target = "items", version = "SF"),
    sim_pid5bf = function() label_pid5(sim_pid5bf, target = "items", version = "BF"),
    ku_pid5sf = function() label_pid5(ku_pid5sf, target = "items", version = "SF"),
    sim_hitopsr = function() label_hitopsr(sim_hitopsr, target = "items"),
    sim_hitopbr = function() label_hitopbr(sim_hitopbr, target = "items"),
    ku_hitopsr = function() label_hitopsr(ku_hitopsr, target = "items"),
    ku_hitopbr = function() label_hitopbr(ku_hitopbr, target = "items")
  )
  for (nm in names(shipped)) {
    caught <- no_padding_report(shipped[[nm]](), nm)
    labelled <- sum(!vapply(
      caught$value, function(x) is.null(attr(x, "label")), logical(1)
    ))
    expect_true(labelled > 0, info = nm)
  }

  # A module subset -- fewer items than the instrument, each still padded to the
  # instrument's width -- is not a padding problem.
  no_padding_report(
    label_hitopsr(ku_hitopsr[, c("participant", "hsr_001", "hsr_002")], target = "items"),
    "hitopsr module subset"
  )
  no_padding_report(
    label_hitopbr(ku_hitopbr[, c("participant", "hbr_01", "hbr_02")], target = "items"),
    "hitopbr module subset"
  )

  # A frame carrying no prefixed column at all raises the no-match warning and
  # nothing else -- the padding report has nothing to name.
  for (case in list(
    list(fn = function(d) label_pid5(d, target = "items", version = "FULL"), label = "label_pid5"),
    list(fn = function(d) label_hitopsr(d, target = "items"), label = "label_hitopsr"),
    list(fn = function(d) label_hitopbr(d, target = "items"), label = "label_hitopbr")
  )) {
    caught <- no_padding_report(case$fn(data.frame(a = 1)), case$label)
    expect_length(caught$warnings, 1L)
    if (length(caught$warnings) == 1L) {
      expect_s3_class(caught$warnings[[1]], "hitop_no_columns_matched")
    }
  }
})
