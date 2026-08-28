# The display name a reliability_*() call returns is READ from the keying table,
# never rebuilt from the camelCase stem. The expectations below are therefore
# stated against the keying tables and against literal strings -- never by
# re-applying a name transformation to the stem, which would derive the
# expectation from the thing under test (IP2).

# --- AC1: the returned column equals the canonical column, elementwise --------

test_that("all 79 reliability calls return the canonical display name in table order", {
  # Five whole-instrument calls, each against the column of the table its scales
  # came from. Elementwise and in returned row order: a membership test would
  # pass on a permuted or zero-length column.
  expect_identical(
    reliability_pid5(sim_pid5, items = 1:220, version = "FULL", omega = FALSE)$Scale,
    pid_scales[["FULL"]]$Facet
  )
  expect_identical(
    reliability_pid5(sim_pid5sf, items = 1:100, version = "SF", omega = FALSE)$Scale,
    pid_scales[["SF"]]$Facet
  )
  expect_identical(
    reliability_pid5(sim_pid5bf, items = 1:25, version = "BF", omega = FALSE)$Scale,
    pid_scales[["BF"]]$Domain
  )
  expect_identical(
    reliability_hitopsr(sim_hitopsr, items = 1:405, omega = FALSE)$Scale,
    hitopsr_scales$Scale
  )
  expect_identical(
    reliability_hitopbr(sim_hitopbr, items = 1:45, omega = FALSE)$Scale,
    hitopbr_scales$Scale
  )

  # A sliding window of three consecutive rows of hitopsr_scales. Every one of
  # the 76 names goes through the module path at least once, which one exemplar
  # module could not do. The expectation is the module's own $scales, which
  # hitop_module() reads from the table independently of the reliability call.
  n <- nrow(hitopsr_scales)
  windows <- lapply(seq_len(n - 2L), function(i) hitopsr_scales$camelCase[i:(i + 2L)])
  expect_equal(length(windows), 74L)
  expect_setequal(unlist(windows), hitopsr_scales$camelCase)

  for (w in windows) {
    m <- hitop_module("hitopsr", scales = w)
    rel <- reliability_hitopsr(
      sim_hitopsr[m$items],
      items = seq_len(m$nItems),
      module = m,
      omega = FALSE
    )
    expect_identical(rel$Scale, m$scales, info = paste(w, collapse = ", "))
  }
})

# --- AC2: the nine names the old derivation got wrong -------------------------

test_that("the nine names the stem-derived title case got wrong are canonical now", {
  # Literal pairs: the string the old snakecase::to_title_case() derivation
  # returned, and the canonical string the keying table ships. Written out rather
  # than recomputed, so this test does not depend on the removed dependency and
  # cannot drift with it.
  hsr <- reliability_hitopsr(sim_hitopsr, items = 1:405, omega = FALSE)$Scale
  hbr <- reliability_hitopbr(sim_hitopbr, items = 1:45, omega = FALSE)$Scale
  full <- reliability_pid5(sim_pid5, items = 1:220, version = "FULL", omega = FALSE)$Scale
  sf <- reliability_pid5(sim_pid5sf, items = 1:100, version = "SF", omega = FALSE)$Scale
  bf <- reliability_pid5(sim_pid5bf, items = 1:25, version = "BF", omega = FALSE)$Scale

  pairs <- list(
    list(hsr, "Distress Dysphoria", "Distress-Dysphoria"),
    list(hsr, "Non Persistence", "Non-persistence"),
    list(hsr, "Non Planfulness", "Non-planfulness"),
    list(hsr, "Non Suicidal Self Injury", "Non-suicidal Self-injury"),
    list(hsr, "Sex Related Substance Use", "Sex-Related Substance Use"),
    list(hsr, "Well Being", "Well-being"),
    list(hbr, "P Factor", "p-Factor"),
    list(full, "Unusual Beliefs Experiences", "Unusual Beliefs & Experiences"),
    list(sf, "Unusual Beliefs Experiences", "Unusual Beliefs & Experiences"),
    list(bf, "Negative Affectivity", "Negative affectivity")
  )
  for (p in pairs) {
    expect_false(p[[2]] %in% p[[1]], info = p[[2]])
    expect_true(p[[3]] %in% p[[1]], info = p[[3]])
  }
})

# --- AC3: every returned name is a name hitop_module() accepts ----------------

test_that("every HiTOP-SR name reliability returns round-trips through hitop_module()", {
  rel <- reliability_hitopsr(sim_hitopsr, items = 1:405, omega = FALSE)
  avail <- available_scales("hitopsr")
  expect_equal(nrow(rel), 76L)
  expect_equal(nrow(avail), 76L)

  for (i in seq_len(nrow(rel))) {
    nm <- rel$Scale[[i]]
    m <- hitop_module("hitopsr", scales = nm)
    expect_identical(m$scales, nm, info = nm)
    expect_identical(avail$Scale[[i]], nm, info = nm)
  }
})

# --- AC4/AC5: the column's name and the nItems type ---------------------------

test_that("the display-name column is named Scale, not scale", {
  for (rel in list(
    reliability_pid5(sim_pid5bf, items = 1:25, version = "BF", omega = FALSE),
    reliability_hitopsr(sim_hitopsr, items = 1:405, omega = FALSE),
    reliability_hitopbr(sim_hitopbr, items = 1:45, omega = FALSE)
  )) {
    expect_true("Scale" %in% names(rel))
    expect_false("scale" %in% names(rel))
    expect_identical(names(rel)[[1]], "Scale")
  }
})

test_that("every emitted nItems is integer", {
  expect_type(available_scales("hitopsr")$nItems, "integer")
  expect_type(
    hitop_module("hitopsr", scales = c("Agoraphobia", "Appetite Loss"))$nItems,
    "integer"
  )
  expect_type(
    reliability_pid5(sim_pid5bf, items = 1:25, version = "BF", omega = FALSE)$nItems,
    "integer"
  )
  expect_type(
    reliability_hitopsr(sim_hitopsr, items = 1:405, omega = FALSE)$nItems,
    "integer"
  )
  expect_type(
    reliability_hitopbr(sim_hitopbr, items = 1:45, omega = FALSE)$nItems,
    "integer"
  )
})

# --- AC8: the dependency is gone ---------------------------------------------
# Verified by the grep AC8 names, run at review; nothing in R/ can assert the
# absence of a package it no longer references.
