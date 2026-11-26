# hitopdat_norms: each row against the values typed in
# helper-hitopdat-norms.R from the committed source notes.

test_that("hitopdat_norms holds the 53 normed scales in hitopdat_scales order", {
  expect_identical(nrow(hitopdat_norms), 53L)
  expect_identical(hitopdat_norms$camelCase, typed_dat_norms$camelCase)
  expect_identical(
    names(hitopdat_norms),
    c("Measure", "Scale", "camelCase", "M", "SD", "Sample", "N", "Source")
  )
  idx <- match(hitopdat_norms$camelCase, hitopdat_scales$camelCase)
  expect_identical(hitopdat_norms$Measure, hitopdat_scales$Measure[idx])
  expect_identical(hitopdat_norms$Scale, hitopdat_scales$Scale[idx])
})

test_that("every M and SD equals its typed source value", {
  expect_equal(hitopdat_norms$M, typed_dat_norms$M, tolerance = 1e-12)
  expect_equal(hitopdat_norms$SD, typed_dat_norms$SD, tolerance = 1e-12)
})

test_that("each row names its reference sample, N and source", {
  measure <- hitopdat_norms$Measure
  expect_identical(
    unique(hitopdat_norms$Source[measure == "WHODAS"]),
    "Andrews et al. (2009), Table 1, Total row, Total column"
  )
  expect_identical(
    unique(hitopdat_norms$Source[measure == "IDAS-II"]),
    "Watson et al. (2012), Table 2, Adults"
  )
  expect_identical(
    unique(hitopdat_norms$Source[measure == "CAPE"]),
    "Galletly et al. (2016), Table 1, CAPE positive, weighted mean of men and women, per item"
  )
  expect_identical(
    unique(hitopdat_norms$Source[measure == "CAT-PD"]),
    "Simms, CAT-PD and IDAS scoring workbook (unpublished), CAT-PD Scoring E3:F35"
  )
  expect_identical(
    hitopdat_norms$N,
    c(8824L, rep(1091L, 18), 305L, rep(NA_integer_, 33))
  )
  expect_identical(
    unique(hitopdat_norms$Sample[measure == "IDAS-II"]),
    "Combined community adult samples (Iowa and South Bend)"
  )
  expect_identical(unique(hitopdat_norms$Sample[measure == "CAT-PD"]), "Community")
  expect_false(anyNA(hitopdat_norms$Sample))
})
