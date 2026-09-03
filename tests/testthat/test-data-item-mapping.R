# The HiTOP-BR is a subset of the HiTOP-SR: every HiTOP-BR item is one of the
# 405 HiTOP-SR items, and `ku_hitopbr` holds the same respondents' answers to
# that subset as `ku_hitopsr`. So for every crosswalk pair the two datasets must
# carry the same column of answers.
#
# This is the invariant a mis-numbered item column breaks. `ku_hitopsr` once
# carried the collection instrument's own question order under the package's
# item numbers, which left every pair below disagreeing; the check is stated
# from the keying tables, not from either dataset's generator.

item_crosswalk <- function() {
  keys <- c("Text", "HSR", "Original")
  merged <- merge(
    hitopsr_items[, keys],
    hitopbr_items[, c(keys, "HBR")],
    by = keys
  )
  merged[order(merged$HBR), c("HSR", "HBR")]
}

test_that("the crosswalk the mapping check runs over covers all 45 HiTOP-BR items", {
  crosswalk <- item_crosswalk()
  expect_identical(sort(crosswalk$HBR), hitopbr_items$HBR)
  expect_identical(nrow(crosswalk), 45L)
})

test_that("ku_hitopsr and ku_hitopbr agree on every item the two forms share", {
  expect_identical(ku_hitopsr$participant, ku_hitopbr$participant)

  crosswalk <- item_crosswalk()
  disagreeing <- character(0)
  for (i in seq_len(nrow(crosswalk))) {
    sr <- sprintf("hsr_%03d", crosswalk$HSR[[i]])
    br <- sprintf("hbr_%02d", crosswalk$HBR[[i]])
    if (!identical(ku_hitopsr[[sr]], ku_hitopbr[[br]])) {
      disagreeing <- c(disagreeing, paste0(sr, "/", br))
    }
  }
  expect_identical(disagreeing, character(0))
})

# The check above compares the two datasets to each other, so a permutation
# applied to both would pass it. `sim_hitopsr` is drawn from the keying table's
# response range and shares no values with the collected data, so it cannot
# anchor the mapping either. What anchors it is the reverse-keyed structure of
# the scales: an item correlates positively with the rest of its own scale once
# reverse-keyed, and a permutation of the item numbers destroys that. The bound
# below is far under the value the shipped data reaches and far above what any
# permutation leaves standing.

test_that("each ku_hitopsr scale holds together, as it cannot under a permuted mapping", {
  alphas <- reliability_hitopsr(
    ku_hitopsr,
    items = 3:407,
    alpha = TRUE,
    omega = FALSE
  )
  expect_identical(nrow(alphas), 76L)
  expect_gt(min(alphas$alpha, na.rm = TRUE), 0.30)
  expect_gt(stats::median(alphas$alpha, na.rm = TRUE), 0.70)
})
