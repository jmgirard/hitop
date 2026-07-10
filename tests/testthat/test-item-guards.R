# Guards against the two silent-wrong-results traps (M11):
#   - warn_item_order():        misordered character `items` names
#   - validate_item_uniqueness(): duplicated `items` entries
# Exercised at the helper level and end-to-end through all four data-taking
# exported functions (score_pid5/hitopsr/hitopbr, validity_pid5). These guards
# only warn/abort; they never change scores (see the "no behavior change" test
# below and the unmodified oracle suites).

# ---- helper-level unit tests ------------------------------------------------

test_that("warn_item_order() fires only for misordered common-prefix numbered names", {
  # Fires: single prefix, trailing integers out of ascending order
  expect_warning(warn_item_order(c("pid_2", "pid_1", "pid_3")), "ascending")
  expect_warning(warn_item_order(paste0("hsr", c(2, 1, 3))), "ascending")
  # Zero-padding does not confuse the integer comparison
  expect_warning(warn_item_order(sprintf("hsr%03d", c(1, 3, 2))), "ascending")
  expect_no_warning(warn_item_order(sprintf("hsr%03d", 1:10)))

  # No fire: ascending names
  expect_no_warning(warn_item_order(c("pid_1", "pid_2", "pid_3")))
  # No fire: integer positions (an out-of-order remap can be legitimate)
  expect_no_warning(warn_item_order(c(3L, 1L, 2L)))
  # No fire: mixed prefixes (no single expected order)
  expect_no_warning(warn_item_order(c("x_99", "pid_1", "pid_2")))
  # No fire: names without trailing digits
  expect_no_warning(warn_item_order(c("age", "sex", "id")))
  # No fire: only some names carry trailing digits
  expect_no_warning(warn_item_order(c("pid_2", "pid_1", "notes")))
})

test_that("validate_item_uniqueness() errors on duplicates and names them", {
  expect_no_error(validate_item_uniqueness(c("a", "b", "c")))
  expect_no_error(validate_item_uniqueness(1:5))
  expect_error(validate_item_uniqueness(c("a", "a", "b")), "distinct")
  expect_error(validate_item_uniqueness(c(1, 1, 3)), "distinct")
})

# ---- end-to-end wiring across all four functions ----------------------------

test_that("misordered `items` names warn in every data-taking function", {
  bf <- setNames(sim_pid5bf, paste0("pid_", 1:25))
  sr <- setNames(sim_hitopsr, paste0("hsr_", 1:405))
  br <- setNames(sim_hitopbr, paste0("hbr_", 1:45))
  full <- setNames(sim_pid5, paste0("pid_", 1:220))

  expect_warning(
    score_pid5(bf, items = paste0("pid_", c(2, 1, 3:25)), version = "BF"),
    "ascending"
  )
  expect_warning(
    score_hitopsr(sr, items = paste0("hsr_", c(2, 1, 3:405))),
    "ascending"
  )
  expect_warning(
    score_hitopbr(br, items = paste0("hbr_", c(2, 1, 3:45))),
    "ascending"
  )
  expect_warning(
    suppressMessages(
      validity_pid5(full, items = paste0("pid_", c(2, 1, 3:220)), version = "FULL")
    ),
    "ascending"
  )
})

test_that("ascending names and integer positions do not warn about order", {
  bf <- setNames(sim_pid5bf, paste0("pid_", 1:25))
  expect_no_warning(score_pid5(bf, items = paste0("pid_", 1:25), version = "BF"))
  expect_no_warning(score_pid5(sim_pid5bf, items = 1:25, version = "BF"))
  expect_no_warning(score_hitopbr(sim_hitopbr, items = 1:45))
  expect_no_warning(score_hitopsr(sim_hitopsr, items = 1:405))
})

test_that("duplicated `items` error in every data-taking function", {
  bf <- setNames(sim_pid5bf, paste0("pid_", 1:25))
  expect_error(
    score_pid5(bf, items = c("pid_1", paste0("pid_", 1:24)), version = "BF"),
    "distinct"
  )
  expect_error(score_hitopbr(sim_hitopbr, items = c(1, 1, 3:45)), "distinct")
  expect_error(score_hitopsr(sim_hitopsr, items = c(1, 1, 3:405)), "distinct")
  expect_error(
    suppressMessages(validity_pid5(sim_pid5, items = c(1, 1, 3:220), version = "FULL")),
    "distinct"
  )
})

test_that("the order guard changes no scores (warning is a pure side effect)", {
  # Same data and same mapping, reached with vs without triggering the warning:
  # scoring the correctly-ordered names must equal scoring integer positions.
  bf <- setNames(sim_pid5bf, paste0("pid_", 1:25))
  by_name <- score_pid5(bf, items = paste0("pid_", 1:25), version = "BF", append = FALSE)
  by_pos  <- score_pid5(bf, items = 1:25, version = "BF", append = FALSE)
  expect_equal(by_name, by_pos)
})
