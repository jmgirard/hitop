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
# would leave the object claiming it read doubles.
retype_spec <- function(x, columns) {
  spec <- attr(x, "spec")
  if (is.null(spec)) return(x)
  for (nm in intersect(names(spec$cols), columns)) {
    spec$cols[[nm]] <- readr::col_integer()
  }
  attr(x, "spec") <- spec
  x
}

skip_without_double_base <- function(name, sha) {
  old <- merge_base_object(name, sha)
  probe <- if (is.data.frame(old)) old else old[[1L]]
  first <- item_number_columns[[name]][[1L]]
  testthat::skip_if(
    is.integer(probe[[first]]),
    paste0("the merge base already stores ", name, "$", first, " as integer")
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
