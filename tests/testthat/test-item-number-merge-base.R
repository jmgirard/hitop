# Nothing but the type moved. `old` is the object at the merge base (see
# helper-merge-base.R); `new` is the committed object. Applying to `old` exactly
# the change this branch makes -- item-number columns to integer at every
# nesting depth, and the reader's `spec` retyped to match -- must reproduce
# `new` exactly: every other column, every row, every list-column element, every
# nested frame, every attribute.
#
# Skips once the merge base already carries the change, so these run on the
# branch that made it and never fail a later one.

# The item-number columns of each shipped table, by the table they sit in. A
# column named here is coerced wherever the walk below finds it: as a plain
# column, as a column of a nested `itemdata` frame, and (for `itemNumbers`) as
# an element of a list-column.
item_number_columns <- list(
  pid_items = c(
    "FULL", "SF", "BF", "INC", "INCS", "ORS", "ORSS",
    "PRD", "PRDS", "SDTD", "SDTDS"
  ),
  pid_scales = c("FULL", "SF", "BF", "itemNumbers"),
  hitopsr_items = "HSR",
  hitopsr_scales = c("HSR", "itemNumbers"),
  hitopsr_subscales = c("HSR", "itemNumbers"),
  hitopbr_items = c("HBR", "HSR"),
  hitopbr_scales = c("HBR", "HSR", "itemNumbers"),
  hitophsum_items = "Item"
)

retype_item_numbers <- function(x, columns) {
  if (is.data.frame(x)) {
    for (nm in names(x)) {
      column <- x[[nm]]
      if (is.list(column)) {
        x[[nm]] <- lapply(column, function(el) {
          if (is.data.frame(el)) {
            retype_item_numbers(el, columns)
          } else if (nm %in% columns && is.numeric(el)) {
            structure(as.integer(el), names = names(el))
          } else {
            el
          }
        })
      } else if (nm %in% columns && is.numeric(column)) {
        x[[nm]] <- as.integer(column)
      }
    }
  } else if (is.list(x)) {
    x[] <- lapply(x, retype_item_numbers, columns = columns)
  }
  x
}

# The reader's record of the types it used. `readr` stores it as a `spec`
# attribute of collector objects; retyping the columns without retyping this
# would leave the object claiming it read doubles. The retype is done by
# renaming the class of the merge base's own collector, so the test states the
# difference it expects and takes no dependency on {readr}, which this package
# uses only under `data-raw/`.
retype_spec <- function(x, columns) {
  spec <- attr(x, "spec")
  if (is.null(spec)) return(x)
  for (nm in intersect(names(spec$cols), columns)) {
    collector <- spec$cols[[nm]]
    expect_s3_class(collector, "collector_double")
    class(collector) <- sub("^collector_double$", "collector_integer", class(collector))
    spec$cols[[nm]] <- collector
  }
  attr(x, "spec") <- spec
  x
}

skip_without_double_base <- function(name, sha) {
  old <- merge_base_object(name, sha)
  # The guard is the retype itself being a no-op, not a probe on one named
  # column: four of these eight objects hold their item numbers only inside
  # list-columns and nested frames, so a top-level probe reads NULL on them and
  # never skips.
  testthat::skip_if(
    identical(retype_item_numbers(old, item_number_columns[[name]]), old),
    paste0("the merge base already stores ", name, "'s item numbers as integer")
  )
  old
}

test_that("retyping the item numbers moved nothing else", {
  base <- skip_without_merge_base()
  for (name in names(item_number_columns)) {
    columns <- item_number_columns[[name]]
    old <- skip_without_double_base(name, base)
    old <- retype_item_numbers(old, columns)
    if (is.data.frame(old)) {
      old <- retype_spec(old, columns)
    } else {
      old[] <- lapply(old, retype_spec, columns = columns)
    }
    expect_identical(getExportedValue("hitop", name), old, info = name)
  }
})

test_that("the retyping applied above is the change, not an equality that hides it", {
  # `expect_identical()` is what the test above uses precisely because
  # `expect_equal()` passes over an integer/double difference. Pinned here so a
  # later relaxation of that assertion is a visible change, not a silent one.
  base <- skip_without_merge_base()
  old <- skip_without_double_base("hitopsr_items", base)
  expect_equal(old$HSR, hitopsr_items$HSR)
  expect_false(identical(old$HSR, hitopsr_items$HSR))
  expect_type(old$HSR, "double")
  expect_type(hitopsr_items$HSR, "integer")
})

# ---- the same, for the response values -------------------------------------

# The response columns of the four objects whose response values this branch
# retyped. Written out from each instrument's naming rule rather than read off
# the objects, so the retype applied below is not derived from the objects it
# checks.
response_columns <- list(
  ku_hitopsr = sprintf("hsr_%03d", 1:405),
  ku_hitopbr = sprintf("hbr_%02d", 1:45),
  ku_pid5sf = sprintf("pid5sf_%03d", 1:100),
  hitophsum_choices = "Value"
)

retype_responses <- function(x, columns) {
  for (nm in intersect(names(x), columns)) {
    x[[nm]] <- as.integer(x[[nm]])
  }
  x
}

# `testthat::skip_if()` ends the whole `test_that()` block, not the loop
# iteration it is called from, so a per-object skip inside a loop would abandon
# every object after the first one that skipped -- silently, and reported as one
# clean skip. The merge base is therefore read for every object first, and the
# skip decided once over all of them: the loop below either runs whole or does
# not run at all.
merge_base_responses <- function(sha) {
  bases <- lapply(names(response_columns), function(name) {
    old <- merge_base_object(name, sha)
    list(
      old = old,
      moved = !identical(retype_responses(old, response_columns[[name]]), old)
    )
  })
  names(bases) <- names(response_columns)
  testthat::skip_if(
    !any(vapply(bases, `[[`, logical(1), "moved")),
    "the merge base already stores every response value as integer"
  )
  bases
}

test_that("retyping the response columns moved nothing else", {
  base <- skip_without_merge_base()
  bases <- merge_base_responses(base)
  for (name in names(response_columns)) {
    columns <- response_columns[[name]]
    old <- retype_spec(retype_responses(bases[[name]]$old, columns), columns)
    expect_identical(getExportedValue("hitop", name), old, info = name)
  }
  # Each object is compared, and each one really moved -- so no comparison above
  # passed by asserting that nothing changed.
  expect_setequal(
    names(Filter(function(x) x$moved, bases)),
    names(response_columns)
  )
})

test_that("the response retype is the change, not an equality that hides it", {
  base <- skip_without_merge_base()
  old <- merge_base_responses(base)$ku_hitopsr$old
  expect_equal(old$hsr_001, ku_hitopsr$hsr_001)
  expect_false(identical(old$hsr_001, ku_hitopsr$hsr_001))
  expect_type(old$hsr_001, "double")
  expect_type(ku_hitopsr$hsr_001, "integer")
})

# ---- and for the internal instruction objects -------------------------------

# `R/sysdata.rda` holds all four in one file, so it is read through
# `merge_base_sysdata()` rather than `merge_base_object()`.
instruction_objects <- c(
  "pid_instructions", "hitopsr_instructions",
  "hitopbr_instructions", "hitophsum_instructions"
)

test_that("the shipped instruction option values are integers", {
  # `pid_instructions` was already integer before this branch; it is named here
  # so a regression on it fails rather than passing unlooked-at.
  expect_type(pid_instructions$options$value, "integer")
  expect_type(hitopsr_instructions$options$value, "integer")
  expect_type(hitopbr_instructions$options$value, "integer")
  # `$` partial-matches on a list, so the absence is asserted over the names.
  expect_false("options" %in% names(hitophsum_instructions))
})

test_that("retyping the instruction option values moved nothing else", {
  base <- skip_without_merge_base()
  env <- merge_base_sysdata(base)
  moved <- character()
  for (name in instruction_objects) {
    old <- get(name, envir = env)
    if (!is.null(old$options) && !is.integer(old$options$value)) {
      old$options$value <- as.integer(old$options$value)
      moved <- c(moved, name)
    }
    expect_identical(get(name, envir = asNamespace("hitop")), old, info = name)
  }
  # Without this the block above passes on a merge base that already carries the
  # change, asserting only that nothing moved at all. Named per object rather
  # than as a single flag, so one of the two retyped objects failing to move
  # cannot be covered by the other one moving.
  expect_setequal(moved, c("hitopsr_instructions", "hitopbr_instructions"))
})
