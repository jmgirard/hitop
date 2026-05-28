test_that("end-to-end workflow successfully processes ku_hitopsr from legacy renaming to scale labeling", {
  # Step 1: Mock a legacy dataset by changing ku_hitopsr names to 'Original' pool names.
  # Based on the instrument structure, columns 3:407 correspond exactly to HSR items 1:405.
  test_data <- ku_hitopsr
  colnames(test_data)[3:407] <- hitopsr_items$Original

  # Step 2: Run the complete recommended package pipeline using the native pipe.
  # We expect this to run silently because all 405 items are present (no subset warnings).
  expect_silent({
    processed_data <- test_data |>
      rename_hitopsr_items(method = "original", prefix = "HSR_") |>
      label_hitopsr(target = "items", prefix = "HSR_") |>
      score_hitopsr(
        items = paste0("HSR_", 1:405),
        prefix = "score_",
        append = TRUE
      ) |>
      label_hitopsr(target = "scales", prefix = "score_")
  })

  # Step 3: Assertions on column renaming and structure
  # Check that the items were correctly turned into the HSR_* format
  expect_contains(colnames(processed_data), paste0("HSR_", 1:405))

  # Check that scale columns were properly generated with our custom 'score_' prefix
  expected_scales <- paste0("score_", hitopsr_scales$camelCase)
  expect_contains(colnames(processed_data), expected_scales)

  # Step 4: Assertions on item-level metadata labeling
  # Verify that HSR_1 received its exact question prompt text attribute
  expect_identical(
    attr(processed_data$HSR_1, "label"),
    hitopsr_items$Text[hitopsr_items$HSR == 1]
  )

  # Step 5: Assertions on scale-level metadata labeling
  # Verify that the first scale column received its clean Title Case description attribute
  first_scale_camel <- hitopsr_scales$camelCase[1]
  first_scale_clean <- hitopsr_scales$Scale[1]

  expect_identical(
    attr(processed_data[[paste0("score_", first_scale_camel)]], "label"),
    first_scale_clean
  )
})
