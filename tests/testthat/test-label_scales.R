# Tests for the labeling and ranking utilities (milestone M5):
# label_hitopsr(), label_hitopbr(), and rank_scales(). These go beyond the
# incidental coverage in the end-to-end pipeline test.

# ---- label_hitopsr() --------------------------------------------------------

test_that("label_hitopsr() attaches item text and scale names", {
  # Items: HSR_1 / HSR_2 get their questionnaire prompt text as a label.
  df_items <- data.frame(HSR_1 = c(1, 2), HSR_2 = c(3, 4))
  labeled <- label_hitopsr(df_items, target = "items", prefix = "HSR_")
  expect_identical(
    attr(labeled$HSR_1, "label"),
    hitopsr_items$Text[hitopsr_items$HSR == 1]
  )
  expect_identical(
    attr(labeled$HSR_2, "label"),
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
  expect_warning(res <- label_hitopsr(df, target = "items"), "No columns matched")
  expect_identical(res, df)
})

# ---- label_hitopbr() --------------------------------------------------------

test_that("label_hitopbr() attaches item text and scale names", {
  df_items <- data.frame(HBR_1 = c(1, 2), HBR_2 = c(3, 4))
  labeled <- label_hitopbr(df_items, target = "items", prefix = "HBR_")
  expect_identical(
    attr(labeled$HBR_1, "label"),
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
  expect_warning(res <- label_hitopbr(df, target = "scales"), "No columns matched")
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
