test_that("rename_hitopsr_items works with method = 'original'", {
  df_legacy <- data.frame(
    HiTOP_659 = c(1, 2),
    HiTOP_301 = c(3, 4),
    Unrelated_Col = c("A", "B")
  )

  # Capture the completeness warning safely
  warnings <- capture_warnings({
    res <- rename_hitopsr_items(df_legacy, method = "original", prefix = "HSR_")
  })

  expect_match(warnings, "out of 405 HiTOP-SR items")
  expect_named(res, c("HSR_001", "HSR_002", "Unrelated_Col"))
})

test_that("rename_hitopsr_items works with method = 'text'", {
  df_custom <- data.frame(
    col_a = c(1, 2),
    col_b = c(3, 4),
    Unrelated_Col = c("A", "B")
  )

  user_cols <- c("col_a", "col_b")
  user_texts <- c(
    "I preferred to stay home than to go to a party.",
    "I felt that my work must be flawless."
  )

  warnings <- capture_warnings({
    res <- df_custom |>
      rename_hitopsr_items(
        method = "text",
        item_cols = user_cols,
        item_text = user_texts,
        prefix = "HSR_"
      )
  })

  expect_match(warnings, "out of 405 HiTOP-SR items")
  expect_named(res, c("HSR_001", "HSR_002", "Unrelated_Col"))
})

test_that("rename_hitopsr_items handles unmatched columns and structural errors gracefully", {
  df_empty <- data.frame(Wrong_Name = c(1, 2))

  caught <- collect_warnings(rename_hitopsr_items(df_empty, method = "original"))
  expect_s3_class(caught$warnings[[1]], "hitop_no_columns_matched")
  expect_identical(caught$value, df_empty)

  expect_error(
    rename_hitopsr_items(df_empty, method = "text", item_cols = "Wrong_Name"),
    "Both"
  )

  expect_error(
    rename_hitopsr_items(
      df_empty,
      method = "text",
      item_cols = "Wrong_Name",
      item_text = c("Text 1", "Text 2")
    ),
    "same length"
  )

  expect_error(
    rename_hitopsr_items(
      df_empty,
      method = "text",
      item_cols = "Missing_Col",
      item_text = "Some Text"
    ),
    "not found in the data frame"
  )
})

test_that("rename_hitopsr_items warns about skipped text mismatches and processes the rest", {
  df_custom <- data.frame(
    col_a = c(1, 2),
    col_b = c(3, 4)
  )

  user_cols <- c("col_a", "col_b")
  user_texts <- c(
    "I preferred to stay home than to go to a party.",
    "This text definitely does not exist in the instrument item pool."
  )

  # Keep both warnings, as conditions rather than as text
  caught <- collect_warnings(
    rename_hitopsr_items(
      df_custom,
      method = "text",
      item_cols = user_cols,
      item_text = user_texts
    )
  )
  res <- caught$value

  # The skipped-input report is asserted by the class the package promises;
  # the completeness report keeps its prose check.
  expect_s3_class(caught$warnings[[1]], "hitop_unmatched_items")
  expect_match(warning_text(caught, 2L), "out of 405 HiTOP-SR items")

  # Verify processing continued for the valid item
  expect_named(res, c("hsr_001", "col_b"))
})

# ---- zero-padded item names under the default prefix ------------------------

test_that("rename_hitopsr_items() zero-pads item numbers to three digits (both methods)", {
  probe <- c(1, 10, 405)
  rows <- match(probe, hitopsr_items$HSR)
  expected <- c("hsr_001", "hsr_010", "hsr_405")

  df_legacy <- as.data.frame(matrix(0, nrow = 2, ncol = 3))
  names(df_legacy) <- hitopsr_items$Original[rows]
  res_orig <- suppressWarnings(rename_hitopsr_items(df_legacy, method = "original"))
  expect_named(res_orig, expected)

  df_text <- data.frame(a = 0, b = 0, c = 0)
  res_text <- suppressWarnings(rename_hitopsr_items(
    df_text,
    method = "text",
    item_cols = c("a", "b", "c"),
    item_text = hitopsr_items$Text[rows]
  ))
  expect_named(res_text, expected)

  # A custom prefix is pasted literally; only the number is padded.
  res_custom <- suppressWarnings(
    rename_hitopsr_items(df_legacy, method = "original", prefix = "HSR_")
  )
  expect_named(res_custom, c("HSR_001", "HSR_010", "HSR_405"))
})
