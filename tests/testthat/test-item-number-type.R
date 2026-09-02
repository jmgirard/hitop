# Every item number the package ships is an integer. The sweep below is the
# criterion in test form: it walks every shipped dataset and reports each bare
# double column whose values are all whole, so an item number stored as a double
# anywhere -- a plain column, an element of a list-column, a column of a nested
# frame -- shows up as a name in the report.

# A column that carries a class attribute is not a bare double: its class names
# the type, and the value being a double is an implementation detail of that
# class. `hitop_artifacts$build_date` is the case in hand -- a `Date`, stored as
# a whole-valued double -- and without this exclusion the sweep would report it
# as an untyped number.
bare_whole_double <- function(x) {
  if (!is.double(x) || !is.null(attr(x, "class"))) return(FALSE)
  values <- x[!is.na(x)]
  all(is.finite(values)) && all(values == trunc(values))
}

# Four places a column can sit in this package's data: a plain column of a data
# frame, an element of a list-column (`itemNumbers`), a column of a data frame
# nested inside a list-column (`itemdata`), and a data frame held as an element
# of a bare list (`pid_scales$FULL`). Each reported path names the dataset and
# the route taken to the column, so a failure says which column moved.
whole_double_paths <- function(obj, path) {
  found <- character(0)
  if (is.data.frame(obj)) {
    for (nm in names(obj)) {
      column <- obj[[nm]]
      here <- paste0(path, "$", nm)
      if (is.list(column)) {
        for (i in seq_along(column)) {
          found <- c(found, whole_double_paths(column[[i]], paste0(here, "[[", i, "]]")))
        }
      } else if (bare_whole_double(column)) {
        found <- c(found, here)
      }
    }
  } else if (is.list(obj)) {
    for (i in seq_along(obj)) {
      nm <- if (is.null(names(obj))) i else names(obj)[[i]]
      found <- c(found, whole_double_paths(obj[[i]], paste0(path, "$", nm)))
    }
  } else if (is.atomic(obj) && bare_whole_double(obj)) {
    found <- c(found, path)
  }
  found
}

shipped_datasets <- function() utils::data(package = "hitop")$results[, "Item"]

sweep_shipped_datasets <- function() {
  found <- character(0)
  for (nm in shipped_datasets()) {
    found <- c(found, whole_double_paths(getExportedValue("hitop", nm), nm))
  }
  found
}

# The response columns of the three datasets of collected answers. Written out
# from the naming rule rather than read off the datasets, so the expectation
# below is not derived from the objects it checks.
response_columns <- c(
  paste0("ku_hitopsr$", sprintf("hsr_%03d", 1:405)),
  paste0("ku_hitopbr$", sprintf("hbr_%02d", 1:45)),
  paste0("ku_pid5sf$", sprintf("pid5sf_%03d", 1:100))
)

test_that("the sweep runs over the shipped datasets, keying tables included", {
  index <- shipped_datasets()
  expect_gt(length(index), 0L)
  expect_true(
    all(c(
      "pid_items", "pid_scales",
      "hitopsr_items", "hitopsr_scales", "hitopsr_subscales",
      "hitopbr_items", "hitopbr_scales", "hitophsum_items"
    ) %in% index)
  )
})

test_that("no shipped dataset stores an item number as a double", {
  # The two kinds of number that are deliberately doubles: the collected
  # responses, and the response values a HiTOP-HSUM choice set offers. Neither
  # is an item number. Everything else the sweep can see is either an integer,
  # a classed column, or not whole.
  expect_setequal(
    sweep_shipped_datasets(),
    c(response_columns, "hitophsum_choices$Value")
  )
})

# ---- the sweep is shown able to catch each shape of item-number column -------

# Three plants, one per depth the walk descends to. Each pairs the planted copy
# against the shipped object it was made from: the shipped object yields no
# report at that path, the planted one yields exactly that path. Without the
# pairing a plant proves only that the walk reaches somewhere.

test_that("the sweep catches a plain item-number column stored as a double", {
  planted <- hitopsr_items
  planted$HSR <- as.double(planted$HSR)
  expect_false("x$HSR" %in% whole_double_paths(hitopsr_items, "x"))
  expect_true("x$HSR" %in% whole_double_paths(planted, "x"))
})

test_that("the sweep catches an item-number list-column element stored as a double", {
  planted <- pid_scales
  planted$FULL$itemNumbers[[1]] <- as.double(planted$FULL$itemNumbers[[1]])
  path <- "x$FULL$itemNumbers[[1]]"
  expect_false(path %in% whole_double_paths(pid_scales, "x"))
  expect_true(path %in% whole_double_paths(planted, "x"))
})

test_that("the sweep catches a nested-frame item-number column stored as a double", {
  planted <- hitopsr_scales
  planted$itemdata[[1]]$HSR <- as.double(planted$itemdata[[1]]$HSR)
  path <- "x$itemdata[[1]]$HSR"
  expect_false(path %in% whole_double_paths(hitopsr_scales, "x"))
  expect_true(path %in% whole_double_paths(planted, "x"))
})

test_that("the sweep passes over a classed double column", {
  # `hitop_artifacts$build_date` is a whole-valued double carrying a `Date`
  # class. It is the shipped case the exclusion exists for, so the sweep must
  # stay silent on it while still reporting the same numbers unclassed.
  expect_true(is.double(unclass(hitop_artifacts$build_date)))
  expect_false("x$build_date" %in% whole_double_paths(hitop_artifacts, "x"))
  stripped <- hitop_artifacts
  stripped$build_date <- unclass(stripped$build_date)
  expect_true("x$build_date" %in% whole_double_paths(stripped, "x"))
})
