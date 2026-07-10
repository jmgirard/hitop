# Ground-truth oracle tests for validity_pid5(). Expected values hand-computed
# in helper-fixtures.R from the official validity-scale keys.
#
# Unlike the fork, validity_pid5() has no `scales` argument: every scale for the
# version is computed unconditionally (FULL/SF -> PNA + 4 scales; BF -> PNA
# only). So there are no "request one scale" / "column iff requested" tests, and
# the fork's SDTD guard-bug regression tests do not apply here.

# ---- FULL (220 items) -------------------------------------------------------

test_that("FULL PNA, INC, ORS, PRD, SDTD match hand-computed fixture values", {
  v <- suppressMessages(
    validity_pid5(fx_pid5(), items = 1:220, version = "FULL", append = FALSE)
  )
  expect_equal(v$pid_PNA,  c(0, 0, 0, 0.1))    # R4 = 22/220
  expect_equal(v$pid_INC,  c(0, 0, 6, NA))     # R3 pairs 1&2: 3+3
  expect_equal(v$pid_ORS,  c(0, 0, 3, NA))     # R3: items 2,8,39 at max
  expect_equal(v$pid_PRD,  c(0, 22, 24, NA))   # 22x1; R3 item2 1->3 (+2)
  expect_equal(v$pid_SDTD, c(0, 17, 19, NA))   # 17x1; R3 item2 +2
})

test_that("FULL independent recomputation from the official key matches (INC + ORS)", {
  x <- fx_pid5()
  # INC: sum of |a - b| over the 20 official item pairs (FULL numbering).
  inc_pairs <- list(
    c(79, 174), c(109, 110), c(148, 169), c(102, 122), c(138, 181),
    c(38, 92), c(80, 128), c(105, 123), c(50, 127), c(74, 173),
    c(191, 211), c(153, 166), c(125, 180), c(89, 145), c(132, 144),
    c(21, 55), c(24, 25), c(52, 152), c(70, 71), c(172, 185)
  )
  inc_hand <- rowSums(vapply(
    inc_pairs, function(p) abs(x[[p[1]]] - x[[p[2]]]), numeric(nrow(x))
  ))
  ors_items <- c(2, 8, 39, 40, 44, 150, 166, 170, 171, 178)
  ors_hand <- rowSums(x[, ors_items] == 3)

  pkg <- suppressMessages(
    validity_pid5(x, items = 1:220, version = "FULL", append = FALSE)
  )
  expect_equal(pkg$pid_INC, inc_hand)
  expect_equal(pkg$pid_ORS, ors_hand)
})

test_that("FULL missing items propagate to NA in validity scores (no na.rm)", {
  v <- suppressMessages(
    validity_pid5(fx_pid5(), items = 1:220, version = "FULL", append = FALSE)
  )
  expect_true(all(is.na(v[4, c("pid_INC", "pid_ORS", "pid_PRD", "pid_SDTD")])))
})

test_that("FULL validity emits cli warnings past published cutoffs", {
  # Pass the whole fixture (>= 2 rows): R3 -> ORS 3, SDTD 19; R1 -> PRD 0, SDTD 0.
  x <- fx_pid5()
  expect_message(
    validity_pid5(x, items = 1:220, version = "FULL", append = FALSE),
    "overreporting"                    # R3 ORS = 3 (>= 3)
  )
  expect_message(
    validity_pid5(x, items = 1:220, version = "FULL", append = FALSE),
    "defensiveness"                    # R3 SDTD = 19 (>= 19)
  )
  expect_message(
    validity_pid5(x, items = 1:220, version = "FULL", append = FALSE),
    "positive impression management"   # R1 PRD = 0 (<= 10)
  )
  expect_message(
    validity_pid5(x, items = 1:220, version = "FULL", append = FALSE),
    "social desirability"              # R1 SDTD = 0 (<= 11)
  )
})

test_that("FULL validity output is exactly the 5 expected columns", {
  v <- suppressMessages(
    validity_pid5(fx_pid5(), items = 1:220, version = "FULL", append = FALSE)
  )
  expect_named(v, c("pid_PNA", "pid_INC", "pid_ORS", "pid_PRD", "pid_SDTD"))
})

# ---- SF (100 items) ---------------------------------------------------------

test_that("SF PNA, INCS, ORSS, PRDS, SDTDS match hand-computed fixture values", {
  v <- suppressMessages(
    validity_pid5(fx_pid5sf(), items = 1:100, version = "SF", append = FALSE)
  )
  expect_equal(v$pid_PNA,   c(0, 0, 0, 0, 0.1))  # R5 = 10/100
  expect_equal(v$pid_INCS,  c(0, 0, 0, 5, 0))    # R4 pairs 1&2: 3+2; R5 pairs clear
  expect_equal(v$pid_ORSS,  c(0, 0, 0, 3, NA))   # R4: items 1,13,14 at max
  expect_equal(v$pid_PRDS,  c(0, 24, 12, 14, NA))# R2 12x2; R4 item1 +2
  expect_equal(v$pid_SDTDS, c(0, 16, 7, 10, NA)) # R2 8x2; R3 item9 1->0 (-1)
})

test_that("SF independent recomputation from the official key matches (INCS + ORSS)", {
  x <- fx_pid5sf()
  incs_pairs <- list(
    c(24, 78), c(53, 81), c(25, 46), c(33, 42), c(17, 45),
    c(23, 77), c(87, 97), c(62, 72), c(29, 56), c(49, 55)
  )
  incs_hand <- rowSums(vapply(
    incs_pairs, function(p) abs(x[[p[1]]] - x[[p[2]]]), numeric(nrow(x))
  ))
  orss_items <- c(1, 13, 14, 15, 59, 72, 75, 76)  # 8 SF over-reporting items
  orss_hand <- rowSums(x[, orss_items] == 3)

  pkg <- suppressMessages(
    validity_pid5(x, items = 1:100, version = "SF", append = FALSE)
  )
  expect_equal(pkg$pid_INCS, incs_hand)
  expect_equal(pkg$pid_ORSS, orss_hand)
})

test_that("SF missing items propagate to NA (INCS unaffected by items 1:10)", {
  # R5 drops items 1:10; item 1 belongs to ORSS, PRDS, and SDTDS, but no INCS
  # pair uses items 1:10, so INCS stays defined (= 0).
  v <- suppressMessages(
    validity_pid5(fx_pid5sf(), items = 1:100, version = "SF", append = FALSE)
  )
  expect_true(all(is.na(v[5, c("pid_ORSS", "pid_PRDS", "pid_SDTDS")])))
  expect_equal(v$pid_INCS[5], 0)
})

test_that("SF INCS warns past the published cutoff (>= 8)", {
  # Two respondents; the first has INCS >= 8 (maximize three pairs, 3 each = 9).
  x <- as.data.frame(matrix(1L, nrow = 2, ncol = 100))
  names(x) <- paste0("pid_", seq_len(100))
  x[1, c(24, 78)] <- c(3L, 0L)
  x[1, c(53, 81)] <- c(3L, 0L)
  x[1, c(25, 46)] <- c(3L, 0L)   # INCS = 9
  expect_message(
    validity_pid5(x, items = 1:100, version = "SF", append = FALSE),
    "inconsistent responding"
  )
})

# ---- BF (25 items) ----------------------------------------------------------

test_that("BF validity computes only PNA", {
  v <- validity_pid5(fx_pid5bf(), items = 1:25, version = "BF", append = FALSE)
  expect_named(v, "pid_PNA")
  expect_equal(v$pid_PNA, c(0, 0, 0, 0.2))  # R4 = 5/25
})

# ---- Invariant across versions ----------------------------------------------

test_that("row count is preserved by validity_pid5 for every version", {
  expect_equal(
    nrow(suppressMessages(validity_pid5(fx_pid5(), items = 1:220, version = "FULL"))),
    nrow(fx_pid5())
  )
  expect_equal(
    nrow(suppressMessages(validity_pid5(fx_pid5sf(), items = 1:100, version = "SF"))),
    nrow(fx_pid5sf())
  )
  expect_equal(
    nrow(validity_pid5(fx_pid5bf(), items = 1:25, version = "BF")),
    nrow(fx_pid5bf())
  )
})
