# Ground-truth oracle tests for score_hitopsr() (milestone M5).
# Fixtures + hand-computed expectations live in helper-fixtures.R (fx_hitopsr).

test_that("score_hitopsr() matches hand-computed scores (incl. reverse item 310)", {
  out <- score_hitopsr(fx_hitopsr(), items = paste0("HSR_", 1:405))

  # Romantic Disinterest carries the ONLY reverse-keyed HiTOP-SR item (HSR 310).
  # If reverse-keying were skipped, R1 would be mean(1,1,1,1,1) = 1, not 1.6.
  expect_equal(out$hsr_romanticDisinterest, c(1.6, 3.4, 1.8, 2.8))

  # Two small no-reverse scales; R4 exercises na.rm on the NA'd item 144.
  expect_equal(out$hsr_appetiteLoss, c(1, 4, 2, 3))
  expect_equal(out$hsr_bingeEating, c(1, 4, 2, 3))
})

test_that("score_hitopsr() independently recomputes a scale from hardcoded numbers", {
  # Independent recomputation: Appetite Loss items copied straight from the
  # published HiTOP-SR key (hitopsr_items.csv), NOT read from hitopsr_scales.
  # This is the only check that catches a transcription error in the scales
  # table's itemNumbers.
  appetite_items <- c(144, 202, 389)

  # The scales table's itemNumbers must equal the source Scale grouping.
  i <- which(hitopsr_scales$camelCase == "appetiteLoss")
  expect_equal(sort(hitopsr_scales$itemNumbers[[i]]), appetite_items)
  expect_setequal(hitopsr_items$HSR[hitopsr_items$Scale == "Appetite Loss"],
                  appetite_items)

  # Dumb, explicit recomputation on random data, compared to the package.
  set.seed(405)
  df <- as.data.frame(matrix(
    sample(1:4, 6 * 405, replace = TRUE),
    nrow = 6, ncol = 405
  ))
  names(df) <- paste0("HSR_", seq_len(405))
  manual <- rowMeans(df[, paste0("HSR_", appetite_items)])

  out <- score_hitopsr(df, items = paste0("HSR_", 1:405), append = FALSE)
  expect_equal(out$hsr_appetiteLoss, manual)
})

test_that("score_hitopsr() honors invariants: se, prefix, row count", {
  df <- fx_hitopsr()
  items <- paste0("HSR_", 1:405)

  # calc_se adds a _se column per scale iff requested.
  base <- score_hitopsr(df, items = items, append = FALSE)
  with_se <- score_hitopsr(df, items = items, calc_se = TRUE, append = FALSE)
  expect_false(any(grepl("_se$", names(base))))
  expect_equal(sum(grepl("_se$", names(with_se))), nrow(hitopsr_scales))
  expect_true(all(paste0(names(base), "_se") %in% names(with_se)))

  # A constant row (R1: all items = 1) has zero within-scale SE.
  expect_equal(with_se$hsr_appetiteLoss_se[1], 0)

  # prefix is applied to every scale column.
  pref <- score_hitopsr(df, items = items, prefix = "z_", append = FALSE)
  expect_true(all(paste0("z_", hitopsr_scales$camelCase) %in% names(pref)))

  # Output row count equals input row count; append binds onto the input.
  expect_equal(nrow(base), nrow(df))
  appended <- score_hitopsr(df, items = items, append = TRUE)
  expect_equal(nrow(appended), nrow(df))
  expect_true(all(names(df) %in% names(appended)))
})
