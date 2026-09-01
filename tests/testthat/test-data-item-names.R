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
