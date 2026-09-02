# The shipped HiTOP datasets name their item columns by one pattern, stated
# here independently of any generator: lowercase stem, underscore, item number
# zero-padded to the instrument's width.

test_that("the HiTOP-SR datasets name items hsr_001..hsr_405", {
  expected <- sprintf("hsr_%03d", hitopsr_items$HSR)
  expect_identical(names(ku_hitopsr)[-(1:2)], expected)
  expect_identical(names(sim_hitopsr), expected)
})

test_that("the HiTOP-BR datasets name items hbr_01..hbr_45", {
  expected <- sprintf("hbr_%02d", hitopbr_items$HBR)
  expect_identical(names(ku_hitopbr)[-(1:2)], expected)
  expect_identical(names(sim_hitopbr), expected)
})

# The shipped PID-5 datasets follow the same pattern, per form: the form's
# lowercase stem and the item number zero-padded to that form's own width.
# `pid_items` numbers each form in its own column, `NA` where the item is not
# on that form, so sorting the column drops the absentees and puts the rest in
# item order.

test_that("the PID-5 full-form dataset names items pid5_001..pid5_220", {
  expected <- sprintf("pid5_%03d", sort(pid_items$FULL))
  expect_identical(names(sim_pid5), expected)
})

test_that("the PID-5 short-form datasets name items pid5sf_001..pid5sf_100", {
  expected <- sprintf("pid5sf_%03d", sort(pid_items$SF))
  expect_identical(names(sim_pid5sf), expected)
  expect_identical(names(ku_pid5sf)[-1], expected)
})

test_that("the PID-5 brief-form dataset names items pid5bf_01..pid5bf_25", {
  expected <- sprintf("pid5bf_%02d", sort(pid_items$BF))
  expect_identical(names(sim_pid5bf), expected)
})

# `ku_pid5sf` is a readr tibble, so it also records the names its columns were
# read under one attribute down, in `spec$cols`, where a names() check does not
# reach. A regeneration from a stale CSV would leave the columns named right and
# that record naming the old columns, so it is asserted against the columns.

test_that("ku_pid5sf's recorded read spec names its columns as they are named", {
  spec <- attr(ku_pid5sf, "spec")
  expect_false(is.null(spec))
  expect_identical(names(spec$cols), names(ku_pid5sf))
})
