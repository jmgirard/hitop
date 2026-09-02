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
