# Ground-truth oracle tests for score_hitopbr() (milestone M5).
# Fixtures + hand-computed expectations live in helper-fixtures.R (fx_hitopbr).

test_that("score_hitopbr() matches hand-computed scores, including overlaps", {
  out <- score_hitopbr(fx_hitopbr(), items = paste0("HBR_", 1:45))

  expect_equal(out$hbr_disinhibition, c(1, 4, 4, 3))
  expect_equal(out$hbr_antagonism, c(1, 4, 2, 3))

  # externalizing / pFactor are overlapping supersets. In R3 only the 9
  # disinhibition items are bumped to 4, so their scores reflect exactly how
  # many disinhibition members each superset contains (5 and 2 respectively).
  expect_equal(out$hbr_externalizing, c(1, 4, 3.0, 3))
  expect_equal(out$hbr_pFactor, c(1, 4, 7 / 3, 3))
})

test_that("score_hitopbr() recomputes overlap scales from the marker columns", {
  # Independent recomputation: the externalizing and pFactor scales are defined
  # by the marker columns in the SOURCE items table. Their itemNumbers must
  # match which() of the markers, and scoring must equal a dumb mean of those
  # columns. This catches a transcription error between the markers and the
  # scales table (the 6 base spectra cannot, since they mirror $Scale directly).
  ext_items <- c(1, 13, 15, 16, 25, 32, 34, 35, 40, 45)
  pf_items <- c(1, 6, 11, 14, 22, 23, 25, 28, 31, 32, 35, 37)

  expect_equal(which(hitopbr_items$Externalizing), ext_items)
  expect_equal(which(hitopbr_items$Pfactor), pf_items)

  ie <- which(hitopbr_scales$camelCase == "externalizing")
  ip <- which(hitopbr_scales$camelCase == "pFactor")
  expect_equal(sort(hitopbr_scales$itemNumbers[[ie]]), ext_items)
  expect_equal(sort(hitopbr_scales$itemNumbers[[ip]]), pf_items)

  set.seed(45)
  df <- as.data.frame(matrix(
    sample(1:4, 8 * 45, replace = TRUE),
    nrow = 8, ncol = 45
  ))
  names(df) <- paste0("HBR_", seq_len(45))
  manual_ext <- rowMeans(df[, paste0("HBR_", ext_items)])
  manual_pf <- rowMeans(df[, paste0("HBR_", pf_items)])

  out <- score_hitopbr(df, items = paste0("HBR_", 1:45), append = FALSE)
  expect_equal(out$hbr_externalizing, manual_ext)
  expect_equal(out$hbr_pFactor, manual_pf)
})

test_that("score_hitopbr() applies no reverse-keying", {
  # The HiTOP-BR has no reverse-keyed items, so an all-minimum response must
  # score at the minimum on every scale (a reversed item would push some to 4).
  df <- as.data.frame(matrix(1L, nrow = 1, ncol = 45))
  names(df) <- paste0("HBR_", seq_len(45))
  out <- score_hitopbr(df, items = paste0("HBR_", 1:45), append = FALSE)
  expect_true(all(out == 1))
  expect_false(any(hitopbr_items$Reverse))
})

test_that("score_hitopbr() honors invariants: se, prefix, row count", {
  df <- fx_hitopbr()
  items <- paste0("HBR_", 1:45)

  base <- score_hitopbr(df, items = items, append = FALSE)
  with_se <- score_hitopbr(df, items = items, calc_se = TRUE, append = FALSE)
  expect_false(any(grepl("_se$", names(base))))
  expect_equal(sum(grepl("_se$", names(with_se))), nrow(hitopbr_scales))
  expect_true(all(paste0(names(base), "_se") %in% names(with_se)))
  expect_equal(with_se$hbr_antagonism_se[1], 0)  # R1 constant row -> SE 0

  pref <- score_hitopbr(df, items = items, prefix = "z_", append = FALSE)
  expect_true(all(paste0("z_", hitopbr_scales$camelCase) %in% names(pref)))

  expect_equal(nrow(base), nrow(df))
  appended <- score_hitopbr(df, items = items, append = TRUE)
  expect_equal(nrow(appended), nrow(df))
  expect_true(all(names(df) %in% names(appended)))
})
