# The per-scale tables the package ships join on one column shape: `nItems` is
# an integer everywhere it appears, and the two devstats tables spell their
# display column `Scale` as the keying tables and available_scales() do.

# ---- AC1: every shipped table with an nItems column stores it as integer -----

# Every shipped dataset, one level down: a data frame with an `nItems` column
# counts, and so does each element of a list of data frames (`pid_scales`). The
# walk is over the installed package's data index, so a dataset added later is
# swept in automatically; the second assertion pins the hit set so the walk is
# shown to cover something rather than passing over an empty domain.
nitems_tables <- function() {
  index <- utils::data(package = "hitop")$results[, "Item"]
  hits <- list()
  for (nm in index) {
    obj <- getExportedValue("hitop", nm)
    if (is.data.frame(obj)) {
      if ("nItems" %in% names(obj)) hits[[nm]] <- obj
    } else if (is.list(obj)) {
      for (el in names(obj)) {
        part <- obj[[el]]
        if (is.data.frame(part) && "nItems" %in% names(part)) {
          hits[[paste0(nm, "$", el)]] <- part
        }
      }
    }
  }
  hits
}

test_that("every shipped table with an nItems column stores it as integer", {
  hits <- nitems_tables()
  expect_setequal(
    names(hits),
    c(
      "hitopsr_scales", "hitopsr_subscales", "hitopbr_scales",
      "hitopsr_devstats", "hitopbr_devstats",
      "pid_scales$FULL", "pid_scales$SF", "pid_scales$BF"
    )
  )
  for (nm in names(hits)) {
    expect_true(is.integer(hits[[nm]]$nItems), info = nm)
  }
})

test_that("the shipped nItems columns are identical to available_scales()'s", {
  # The join the retyping exists for: `identical()`, not `expect_equal()`,
  # which ignores an integer/double difference.
  expect_identical(available_scales("hitopsr")$nItems, hitopsr_scales$nItems)
})

# ---- AC2: only the type or the name changed -----------------------------------

# `old` is the object at the merge base (see helper-merge-base.R); `new` is the
# committed object. After applying to `old` exactly the change this branch
# makes to it, the two must be identical -- every other column, every row, and
# every list-column element. Skips once the merge base already carries the
# change, so the test runs on the branch that made it and never fails a later
# one.

skip_without_double_base <- function(name, sha, element = NULL) {
  old <- merge_base_object(name, sha)
  probe <- if (is.null(element)) old else old[[element]]
  testthat::skip_if(
    is.integer(probe$nItems),
    paste0("the merge base already stores ", name, "$nItems as integer")
  )
  old
}

skip_without_scale_base <- function(name, sha) {
  old <- merge_base_object(name, sha)
  testthat::skip_if(
    identical(names(old)[[1L]], "Scale"),
    paste0("the merge base already names ", name, "'s first column Scale")
  )
  old
}

retype_nitems <- function(x) {
  x$nItems <- as.integer(x$nItems)
  x
}

test_that("retyping nItems moved nothing else in the three flat keying tables", {
  base <- skip_without_merge_base()
  for (nm in c("hitopsr_scales", "hitopsr_subscales", "hitopbr_scales")) {
    old <- skip_without_double_base(nm, base)
    new <- getExportedValue("hitop", nm)
    expect_identical(new, retype_nitems(old), info = nm)
  }
})

test_that("retyping nItems moved nothing else in pid_scales", {
  base <- skip_without_merge_base()
  old <- skip_without_double_base("pid_scales", base, element = "FULL")
  old <- lapply(old, retype_nitems)
  expect_identical(pid_scales, old)
})

test_that("renaming the devstats display column moved nothing else", {
  base <- skip_without_merge_base()
  for (nm in c("hitopsr_devstats", "hitopbr_devstats")) {
    old <- skip_without_scale_base(nm, base)
    names(old)[[1L]] <- "Scale"
    new <- getExportedValue("hitop", nm)
    expect_identical(new, old, info = nm)
  }
})

# ---- AC3: the reliability return carries the keying table's stem -------------

# The six calls the reliability family offers, each paired with the keying
# table its rows come from, that table's display column, and the score_*() call
# that names the columns the stem is meant to join to. The omega estimator is
# mocked: these tests are about the columns the return carries under each flag
# setting, and a real one-factor CFA per scale would cost minutes per call.
reliability_calls <- function() {
  m <- hitop_module("hitopsr", c("agoraphobia", "appetiteLoss", "romanticDisinterest"))
  collected <- sim_hitopsr[sprintf("hsr_%03d", m$items)]
  list(
    FULL = list(
      rel = function(...) reliability_pid5(sim_pid5, items = 1:220, version = "FULL", ...),
      table = pid_scales[["FULL"]], display = "Facet", prefix = "pid_",
      scored = score_pid5(sim_pid5, items = 1:220, version = "FULL")
    ),
    SF = list(
      rel = function(...) reliability_pid5(sim_pid5sf, items = 1:100, version = "SF", ...),
      table = pid_scales[["SF"]], display = "Facet", prefix = "pid_",
      scored = score_pid5(sim_pid5sf, items = 1:100, version = "SF")
    ),
    BF = list(
      rel = function(...) reliability_pid5(sim_pid5bf, items = 1:25, version = "BF", ...),
      table = pid_scales[["BF"]], display = "Domain", prefix = "pid_",
      scored = score_pid5(sim_pid5bf, items = 1:25, version = "BF")
    ),
    hitopsr = list(
      rel = function(...) reliability_hitopsr(sim_hitopsr, items = 1:405, ...),
      table = hitopsr_scales, display = "Scale", prefix = "hsr_",
      scored = score_hitopsr(sim_hitopsr, items = 1:405)
    ),
    hitopsr_module = list(
      rel = function(...) reliability_hitopsr(collected, items = names(collected), module = m, ...),
      table = hitopsr_scales, display = "Scale", prefix = "hsr_",
      scored = score_hitopsr(collected, items = names(collected), module = m)
    ),
    hitopbr = list(
      rel = function(...) reliability_hitopbr(sim_hitopbr, items = 1:45, ...),
      table = hitopbr_scales, display = "Scale", prefix = "hbr_",
      scored = score_hitopbr(sim_hitopbr, items = 1:45)
    )
  )
}

flag_settings <- list(
  default = list(),
  no_omega = list(omega = FALSE),
  no_alpha = list(alpha = FALSE)
)

test_that("reliability_*() return Scale, camelCase, nItems, then the requested coefficients", {
  local_mocked_bindings(calc_omega = function(df) 0.5)
  for (cn in names(reliability_calls())) {
    call <- reliability_calls()[[cn]]
    for (sn in names(flag_settings)) {
      flags <- flag_settings[[sn]]
      rel <- do.call(call$rel, flags)
      want <- c("Scale", "camelCase", "nItems")
      if (!isFALSE(flags$alpha)) want <- c(want, "alpha")
      if (!isFALSE(flags$omega)) want <- c(want, "omega")
      expect_identical(names(rel), want, info = paste(cn, sn))
      expect_true(is.integer(rel$nItems), info = paste(cn, sn))
    }
  }
})

test_that("each returned camelCase is the keying table's stem on the row whose display name is the returned Scale", {
  local_mocked_bindings(calc_omega = function(df) 0.5)
  for (cn in names(reliability_calls())) {
    call <- reliability_calls()[[cn]]
    for (sn in names(flag_settings)) {
      rel <- do.call(call$rel, flag_settings[[sn]])
      # Looked up by the display name, row by row, so a stem column supplied
      # from a misaligned row is caught; a set comparison would not see it.
      row <- match(rel$Scale, call$table[[call$display]])
      expect_false(anyNA(row), info = paste(cn, sn))
      expect_identical(rel$camelCase, call$table$camelCase[row], info = paste(cn, sn))
    }
  }
})

test_that("prefix + camelCase names a column of the matching score_*() output", {
  local_mocked_bindings(calc_omega = function(df) 0.5)
  for (cn in names(reliability_calls())) {
    call <- reliability_calls()[[cn]]
    for (sn in names(flag_settings)) {
      rel <- do.call(call$rel, flag_settings[[sn]])
      expect_gt(nrow(rel), 0L)
      expect_true(
        all(paste0(call$prefix, rel$camelCase) %in% names(call$scored)),
        info = paste(cn, sn)
      )
    }
  }
})
