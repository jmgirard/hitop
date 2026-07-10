# Interface + invariant tests exercised on the shipped datasets.
# This API selects the form with `version` (not a `scales`/`id` argument) and
# requires an explicit `items` argument.

sf_items <- paste0("pid_", 1:100)

test_that("row count is preserved by every exported function/version", {
  expect_equal(nrow(score_pid5(sim_pid5, items = 1:220, version = "FULL")), nrow(sim_pid5))
  expect_equal(nrow(score_pid5(sim_pid5sf, items = 1:100, version = "SF")), nrow(sim_pid5sf))
  expect_equal(nrow(score_pid5(sim_pid5bf, items = 1:25, version = "BF")), nrow(sim_pid5bf))
  expect_equal(
    nrow(suppressMessages(validity_pid5(sim_pid5, items = 1:220, version = "FULL"))),
    nrow(sim_pid5)
  )
  expect_equal(
    nrow(suppressMessages(validity_pid5(sim_pid5sf, items = 1:100, version = "SF"))),
    nrow(sim_pid5sf)
  )
  expect_equal(
    nrow(validity_pid5(sim_pid5bf, items = 1:25, version = "BF")),
    nrow(sim_pid5bf)
  )
})

test_that("version selects the right number of scale columns", {
  full <- score_pid5(sim_pid5, items = 1:220, version = "FULL", append = FALSE)
  sf   <- score_pid5(sim_pid5sf, items = 1:100, version = "SF", append = FALSE)
  bf   <- score_pid5(sim_pid5bf, items = 1:25, version = "BF", append = FALSE)

  expect_equal(ncol(full), 30)   # 25 facets + 5 domains (M7)
  expect_equal(ncol(sf), 30)     # 25 facets + 5 domains (M7)
  expect_equal(ncol(bf), 5)      # 5 domains
  expect_true(all(startsWith(names(full), "pid_")))
  expect_true(all(startsWith(names(bf), "pid_")))
})

test_that("version matching is case-insensitive", {
  lower <- score_pid5(sim_pid5bf, items = 1:25, version = "bf", append = FALSE)
  upper <- score_pid5(sim_pid5bf, items = 1:25, version = "BF", append = FALSE)
  expect_equal(lower, upper)
})

test_that("append = TRUE binds scores onto the input; append = FALSE returns only scores", {
  bound <- score_pid5(sim_pid5bf, items = 1:25, version = "BF", append = TRUE)
  only  <- score_pid5(sim_pid5bf, items = 1:25, version = "BF", append = FALSE)
  expect_equal(ncol(bound), ncol(sim_pid5bf) + ncol(only))
  expect_true(all(names(sim_pid5bf) %in% names(bound)))
})

test_that("prefix controls the scale-column stem", {
  out <- score_pid5(sim_pid5bf, items = 1:25, version = "BF", prefix = "x_", append = FALSE)
  expect_true(all(startsWith(names(out), "x_")))
  expect_true("x_disinhibition" %in% names(out))
})

test_that("calc_se adds one _se column per scale", {
  out <- score_pid5(sim_pid5, items = 1:220, version = "FULL", calc_se = TRUE, append = FALSE)
  se_cols <- grep("_se$", names(out), value = TRUE)
  expect_equal(ncol(out), 60)          # (25 facets + 5 domains) scores + SEs (M7)
  expect_length(se_cols, 30)
  expect_true("pid_anhedonia_se" %in% se_cols)
  expect_true("pid_detachment_se" %in% se_cols)
})

test_that("apa_scoring must be a single logical", {
  expect_error(
    score_pid5(sim_pid5bf, items = 1:25, version = "BF", apa_scoring = "yes"),
    class = "simpleError"
  )
  expect_error(
    score_pid5(sim_pid5bf, items = 1:25, version = "BF", apa_scoring = c(TRUE, FALSE))
  )
})

test_that("apa_scoring changes values but not output shape", {
  apa  <- score_pid5(sim_pid5, items = 1:220, version = "FULL", append = FALSE)
  trad <- score_pid5(sim_pid5, items = 1:220, version = "FULL", apa_scoring = FALSE, append = FALSE)
  expect_equal(dim(apa), dim(trad))
  expect_equal(names(apa), names(trad))
  # calc_se still yields one _se column per scale under APA scoring.
  se <- score_pid5(sim_pid5, items = 1:220, version = "FULL", calc_se = TRUE, append = FALSE)
  expect_equal(ncol(se), 60)
  expect_length(grep("_se$", names(se)), 30)
})

test_that("tibble toggles tibble vs data.frame output", {
  skip_if_not_installed("tibble")
  expect_s3_class(score_pid5(sim_pid5bf, items = 1:25, version = "BF", tibble = TRUE), "tbl_df")
  out_df <- score_pid5(sim_pid5bf, items = 1:25, version = "BF", tibble = FALSE)
  expect_s3_class(out_df, "data.frame")
  expect_false(inherits(out_df, "tbl_df"))
})

test_that("id-style columns pass through when scoring by item name", {
  # ku_pid5sf carries non-item columns alongside pid_1..pid_100.
  out <- score_pid5(ku_pid5sf, items = sf_items, version = "SF", append = TRUE)
  extra <- setdiff(names(ku_pid5sf), sf_items)
  expect_true(all(extra %in% names(out)))
})

test_that("alpha = TRUE prints a reliability summary and leaves scores unchanged", {
  plain <- score_pid5(sim_pid5bf, items = 1:25, version = "BF", append = FALSE)
  expect_output(
    out <- score_pid5(
      sim_pid5bf, items = 1:25, version = "BF", append = FALSE, alpha = TRUE
    ),
    "alpha"
  )
  expect_equal(out, plain)
})

test_that("omega = TRUE runs without error (values verified in M5)", {
  skip_if_not_installed("lavaan")
  expect_no_error(suppressWarnings(utils::capture.output(
    score_pid5(sim_pid5bf, items = 1:25, version = "BF", append = FALSE, omega = TRUE)
  )))
})
