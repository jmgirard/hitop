# Every whole number the package ships is an integer -- item numbers, the
# collected and simulated responses, and the response values a HiTOP-HSUM choice
# set offers. The sweep below is the criterion in test form: it walks every
# shipped dataset and reports each bare double column whose values are all
# whole, so a whole number stored as a double anywhere -- a plain column, an
# element of a list-column, a column of a nested frame -- shows up as a name in
# the report. The report is expected to be empty.

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

test_that("the sweep runs over the shipped datasets, keying and response data included", {
  index <- shipped_datasets()
  expect_gt(length(index), 0L)
  expect_true(
    all(c(
      "pid_items", "pid_scales",
      "hitopsr_items", "hitopsr_scales", "hitopsr_subscales",
      "hitopbr_items", "hitopbr_scales", "hitophsum_items"
    ) %in% index)
  )
  # The datasets that carry response values: the three of collected answers,
  # the five simulated ones, and the choice set whose `Value` is a response
  # value. Named here so an index that stops listing one of them fails, rather
  # than the sweep quietly running over a smaller domain and still reporting
  # nothing.
  expect_true(
    all(c(
      "ku_hitopsr", "ku_hitopbr", "ku_pid5sf",
      "sim_hitopsr", "sim_hitopbr", "sim_pid5", "sim_pid5sf", "sim_pid5bf",
      "hitophsum_choices"
    ) %in% index)
  )
})

test_that("no shipped dataset stores a whole number as a bare double", {
  expect_setequal(sweep_shipped_datasets(), character(0))
})

# ---- the sweep is shown able to catch each shape of whole-number column ------

# Seven plants: three for the depths the walk descends to, and one response
# column in each of the four datasets whose type this promise moved. Each pairs
# the planted copy against the shipped object it was made from: the shipped
# object yields no report at that path, the planted one yields exactly that
# path. Without the pairing a plant proves only that the walk reaches
# somewhere.

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

test_that("the sweep catches a HiTOP-SR response column stored as a double", {
  planted <- ku_hitopsr
  planted$hsr_001 <- as.double(planted$hsr_001)
  expect_false("x$hsr_001" %in% whole_double_paths(ku_hitopsr, "x"))
  expect_identical(whole_double_paths(planted, "x"), "x$hsr_001")
})

test_that("the sweep catches a HiTOP-BR response column stored as a double", {
  planted <- ku_hitopbr
  planted$hbr_01 <- as.double(planted$hbr_01)
  expect_false("x$hbr_01" %in% whole_double_paths(ku_hitopbr, "x"))
  expect_identical(whole_double_paths(planted, "x"), "x$hbr_01")
})

test_that("the sweep catches a PID-5-SF response column stored as a double", {
  planted <- ku_pid5sf
  planted$pid5sf_001 <- as.double(planted$pid5sf_001)
  expect_false("x$pid5sf_001" %in% whole_double_paths(ku_pid5sf, "x"))
  expect_identical(whole_double_paths(planted, "x"), "x$pid5sf_001")
})

test_that("the sweep catches a HiTOP-HSUM response value stored as a double", {
  planted <- hitophsum_choices
  planted$Value <- as.double(planted$Value)
  expect_false("x$Value" %in% whole_double_paths(hitophsum_choices, "x"))
  expect_identical(whole_double_paths(planted, "x"), "x$Value")
})
