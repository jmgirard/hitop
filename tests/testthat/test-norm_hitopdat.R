# norm_hitopdat(): T scores, alerts and refusals. M and SD come from
# typed_dat_norms (helper-hitopdat-norms.R), never from `hitopdat_norms`.

norm_stems <- typed_dat_norms$camelCase

norm_names <- function(prefix = "dat_") {
  c(
    paste0(prefix, norm_stems, "_t"),
    paste0(prefix, "suicidality_alert"),
    paste0(prefix, "selfHarm_alert")
  )
}

# One row per unrounded T in `t`, every scale at that T.
scores_at <- function(t, prefix = "dat_") {
  cols <- lapply(seq_along(norm_stems), function(s) {
    typed_dat_norms$M[[s]] + typed_dat_norms$SD[[s]] * (t - 50) / 10
  })
  names(cols) <- paste0(prefix, norm_stems)
  as.data.frame(cols)
}

test_that("append = FALSE returns the T columns then the two alerts", {
  out <- norm_hitopdat(scores_at(50), append = FALSE)
  expect_s3_class(out, "tbl_df")
  expect_identical(names(out), norm_names())
})

test_that("each T is rounded half away from zero from the typed M and SD", {
  out <- norm_hitopdat(scores_at(c(50, 60.4, 60.6, NA)), append = FALSE)
  for (s in norm_stems) {
    expect_identical(out[[paste0("dat_", s, "_t")]], c(50, 60, 61, NA), label = s)
  }
})

test_that("a NaN score gives an NA T score, not NaN", {
  data <- scores_at(c(50, 50))
  data$dat_suicidality[2] <- NaN
  out <- norm_hitopdat(data, append = FALSE)
  expect_identical(out$dat_suicidality_t, c(50, NA_real_))
  expect_identical(out$dat_suicidality_alert, c(FALSE, NA))
})

test_that("an alert compares its own scale's rounded T with alert_t", {
  data <- scores_at(c(64.6, 64.4, NA))
  out <- norm_hitopdat(data, append = FALSE)
  expect_identical(out$dat_suicidality_alert, c(TRUE, FALSE, NA))
  expect_identical(out$dat_selfHarm_alert, c(TRUE, FALSE, NA))

  out <- norm_hitopdat(scores_at(c(69.6, 69.4, NA)), alert_t = 70, append = FALSE)
  expect_identical(out$dat_suicidality_alert, c(TRUE, FALSE, NA))
  expect_identical(out$dat_selfHarm_alert, c(TRUE, FALSE, NA))

  # Each alert reads only its own scale.
  data <- scores_at(c(70, 50))
  data$dat_selfHarm <- rev(data$dat_selfHarm)
  out <- norm_hitopdat(data, append = FALSE)
  expect_identical(out$dat_suicidality_alert, c(TRUE, FALSE))
  expect_identical(out$dat_selfHarm_alert, c(FALSE, TRUE))
})

test_that("append = TRUE adds the new columns after the data", {
  data <- cbind(id = 1:2, scores_at(c(50, 60.4)))
  out <- norm_hitopdat(data)
  expect_identical(names(out), c(names(data), norm_names()))
  expect_identical(out$id, 1:2)
})

test_that("prefix names both the score columns read and the columns made", {
  out <- norm_hitopdat(scores_at(60.6, prefix = ""), prefix = "", append = FALSE)
  expect_identical(names(out), norm_names(""))
})

test_that("a zero-row data frame returns a zero-row tibble", {
  out <- norm_hitopdat(scores_at(numeric(0)), append = FALSE)
  expect_s3_class(out, "tbl_df")
  expect_identical(nrow(out), 0L)
  expect_identical(names(out), norm_names())
})

test_that("norm_hitopdat() refuses bad arguments", {
  data <- scores_at(50)
  expect_error(norm_hitopdat(as.list(data)), "must be a data frame")
  expect_error(
    norm_hitopdat(data[names(data) != "dat_selfHarm"]),
    "score column is not found.*dat_selfHarm"
  )
  # A wrong prefix misses all 53, and the message names every one.
  err <- expect_error(norm_hitopdat(data, prefix = "x_"), "53 score columns")
  expect_match(conditionMessage(err), "x_workaholism", fixed = TRUE)
  text <- data
  text$dat_anger <- "high"
  expect_error(norm_hitopdat(text), class = "hitop_nonnumeric_items")
  for (value in list(c(65, 70), "65", Inf)) {
    expect_error(norm_hitopdat(data, alert_t = value), "single finite number")
  }
  expect_error(norm_hitopdat(data, prefix = c("a", "b")), "single string")
  expect_error(norm_hitopdat(data, append = NA), "must be .*TRUE.* or .*FALSE")
  clash <- cbind(data, dat_whodas_t = 1)
  expect_error(norm_hitopdat(clash), class = "hitop_append_collision")
  expect_no_error(norm_hitopdat(clash, append = FALSE))
})
