# Ground-truth oracle tests for score_hitopbr() (milestone M005).
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
  with_se <- hush_se(score_hitopbr(df, items = items, calc_se = TRUE, append = FALSE))
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

# Reliability moved out of score_hitopbr() into reliability_hitopbr() (M015); the
# per-scale alpha/omega oracle now lives in test-reliability.R.

test_that("every HiTOP-BR item sits on the scale the development workbook gives it", {
  # The primary source for HiTOP-BR item-to-scale membership is the Society
  # workbook `B-HiTOP overview.xlsx` (SOURCES.md, "HiTOP-BR item-to-scale
  # membership"), which is gitignored and so unreadable from CI. Its
  # `item-to-scale` sheet is transcribed below by the source's own item
  # identifiers -- the `Original` names -- rather than by HBR numbers, so this
  # oracle states the source's grouping and never the package's own numbering.
  # Item 36 (`HiTOP_69`) is the row M068 corrected from Detachment to
  # Internalizing.
  workbook <- list(
    Internalizing = c(
      "HiTOP_69", "HiTOP_187", "HiTOP_378", "HiTOP_570",
      "HiTOP_333", "HiTOP_356", "HiTOP_368", "HiTOP_215"
    ),
    Somatoform = c(
      "HiTOP_479", "HiTOP_449", "HiTOP_451", "HiTOP_490",
      "HiTOP_494", "HiTOP_456", "HiTOP_487", "HiTOP_492"
    ),
    Detachment = c("HiTOP_50", "HiTOP_624", "HiTOP_44", "HiTOP_625", "HiTOP_657"),
    `Thought Disorder` = c(
      "HiTOP_606", "HiTOP_596", "HiTOP_554",
      "HiTOP_558", "HiTOP_583", "HiTOP_557"
    ),
    Disinhibition = c(
      "Ext_102", "Ext_320", "Ext_256", "Ext_361", "Ext_374",
      "Ext_166", "Ext_281", "Ext_97", "Ext_13"
    ),
    Antagonism = c(
      "Ext_262", "Ext_22", "Ext_370", "Ext_175", "HiTOP_577",
      "HiTOP_11", "Ext_367", "HiTOP_21", "Ext_50"
    )
  )

  # The six sheet sections partition the 45 items, so a member moved from one
  # scale to another fails on both, and the count guards against a section this
  # transcription dropped.
  expect_equal(sum(lengths(workbook)), 45L)
  expect_setequal(unlist(workbook, use.names = FALSE), hitopbr_items$Original)

  for (scale in names(workbook)) {
    expect_setequal(
      hitopbr_items$Original[hitopbr_items$Scale == scale],
      workbook[[scale]]
    )
  }

  # The item the correction moved, named on its own so a regression says which
  # item rather than only which scale.
  expect_equal(hitopbr_items$Scale[hitopbr_items$Original == "HiTOP_69"], "Internalizing")
})
