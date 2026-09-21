# Locks the committed JSON exports (inst/extdata/pid5.json, pid5sf.json,
# pid5bf.json, hitopsr.json, hitopbr.json) to the keying tables and the
# instruction objects by parse-and-compare (IP2, D-010): the expected side is
# read from the tables and R/sysdata.rda, never from the file. Regenerate via
# data-raw/artifacts.R (rebuild_formats = "json").
#
# `pid_items` numbers three forms in three columns, each NA on the rows its
# form omits, so the expected side here subsets and orders by the spec's
# number column exactly as data-raw/json_export.R's writer does. The two
# HiTOP tables carry one form each and no NA, so that step leaves them whole.

json_specs <- list(
  pid5 = list(
    stem = "pid5",
    items = pid_items,
    number_col = "FULL",
    instructions = hitop:::pid_instructions
  ),
  pid5sf = list(
    stem = "pid5sf",
    items = pid_items,
    number_col = "SF",
    instructions = hitop:::pid_instructions
  ),
  pid5bf = list(
    stem = "pid5bf",
    items = pid_items,
    number_col = "BF",
    instructions = hitop:::pid_instructions
  ),
  hitopsr = list(
    stem = "hitopsr",
    items = hitopsr_items,
    number_col = "HSR",
    instructions = hitop:::hitopsr_instructions
  ),
  hitopbr = list(
    stem = "hitopbr",
    items = hitopbr_items,
    number_col = "HBR",
    instructions = hitop:::hitopbr_instructions
  )
)

# The rows of a spec's table that belong to its form, in its own item-number
# order: the expected items, read from the table and never from the file.
spec_items <- function(spec) {
  number <- as.integer(spec$items[[spec$number_col]])
  keep <- !is.na(number)
  ord <- order(number[keep])
  list(
    number = number[keep][ord],
    text = as.character(spec$items$Text[keep][ord])
  )
}

json_path <- function(stem) {
  system.file("extdata", paste0(stem, ".json"), package = "hitop")
}

manifest_build_date <- function(file) {
  m <- hitop_artifacts[order(hitop_artifacts$file, hitop_artifacts$build_date), ]
  m <- m[!duplicated(m$file, fromLast = TRUE), ]
  m$build_date[m$file == file]
}

# Every way a file can disagree with its tables, each named so a failing
# plant below identifies which field it broke and the shipped file reports
# every disagreement at once rather than the first.
export_report <- function(path, spec) {
  j <- jsonlite::fromJSON(
    path,
    simplifyVector = TRUE,
    simplifyDataFrame = FALSE,
    simplifyMatrix = FALSE
  )
  out <- character(0)
  note <- function(ok, what) if (!isTRUE(ok)) out <<- c(out, what)

  expected <- spec_items(spec)
  number <- expected$number
  max_n <- max(number)
  pull <- function(rows, field, type) {
    vapply(rows, function(r) r[[field]], type)
  }

  note(identical(j$format, "1.0"), "format")
  note(identical(j$package, "hitop"), "package")
  note(
    identical(j$packageVersion, as.character(utils::packageVersion("hitop"))),
    "packageVersion"
  )
  note(
    identical(as.Date(j$buildDate), manifest_build_date(basename(path))),
    "buildDate"
  )
  note(identical(j$stem, spec$stem), "stem")
  note(identical(as.integer(j$maxItem), max_n), "maxItem")
  note(identical(j$instructions$start, spec$instructions$start), "start")

  opts <- j$instructions$options
  note(
    identical(
      pull(opts, "value", integer(1)),
      as.integer(spec$instructions$options$value)
    ),
    "options.value"
  )
  note(
    identical(
      pull(opts, "label", character(1)),
      as.character(spec$instructions$options$label)
    ),
    "options.label"
  )

  note(length(j$items) == length(number), "items.length")
  if (length(j$items) == length(number)) {
    note(
      all(vapply(j$items, function(r) is.integer(r$number), logical(1))),
      "number.type"
    )
    note(identical(pull(j$items, "number", integer(1)), number), "number")
    note(
      identical(
        pull(j$items, "name", character(1)),
        hitop:::item_names(paste0(spec$stem, "_"), number, max_n)
      ),
      "name"
    )
    note(
      identical(pull(j$items, "text", character(1)), expected$text),
      "text"
    )
  }
  out
}

for (stem in names(json_specs)) {
  spec <- json_specs[[stem]]

  test_that(paste(stem, "JSON export matches the tables and instructions"), {
    path <- json_path(stem)
    expect_true(file.exists(path), info = path)
    expect_identical(export_report(path, spec), character(0))
  })

  test_that(paste(stem, "JSON export holds no carriage return"), {
    path <- json_path(stem)
    bytes <- readBin(path, "raw", file.size(path))
    expect_false(any(bytes == as.raw(0x0d)))
  })
}

# The report can fail: each plant alters one field of a temporary copy and
# must be reported under that field's name (check discrimination).
plant <- function(stem, mutate) {
  j <- jsonlite::fromJSON(json_path(stem), simplifyVector = FALSE)
  j <- mutate(j)
  tmp <- file.path(tempdir(), paste0(stem, ".json"))
  writeLines(
    as.character(jsonlite::toJSON(j, auto_unbox = TRUE, pretty = TRUE)),
    tmp
  )
  tmp
}

test_that("the export report discriminates each planted defect", {
  spec <- json_specs$hitopbr

  changed_text <- plant("hitopbr", function(j) {
    j$items[[7]]$text <- paste0(j$items[[7]]$text, " (altered)")
    j
  })
  expect_identical(export_report(changed_text, spec), "text")

  dropped_item <- plant("hitopbr", function(j) {
    j$items[[7]] <- NULL
    j
  })
  expect_identical(export_report(dropped_item, spec), "items.length")

  swapped_items <- plant("hitopbr", function(j) {
    j$items[c(7, 8)] <- j$items[c(8, 7)]
    j
  })
  expect_setequal(export_report(swapped_items, spec), c("number", "name", "text"))

  changed_label <- plant("hitopbr", function(j) {
    j$instructions$options[[2]]$label <- "Slightly"
    j
  })
  expect_identical(export_report(changed_label, spec), "options.label")

  changed_start <- plant("hitopbr", function(j) {
    j$instructions$start <- sub("12 months", "6 months", j$instructions$start)
    j
  })
  expect_identical(export_report(changed_start, spec), "start")

  changed_stem <- plant("hitopbr", function(j) {
    j$stem <- "hbr"
    j
  })
  expect_identical(export_report(changed_stem, spec), "stem")
})

# The three PID-5 forms come out of one table through its three number
# columns, so a form's export can disagree with the table in ways a one-form
# export cannot: it can carry an item another form owns, lose one of its own,
# or number its items to another form's width. The plants below are those
# ways. The first two both report `items.length`, because a count that
# disagrees leaves nothing to compare item by item.
test_that("the export report discriminates a wrong-form defect on the PID-5 SF", {
  spec <- json_specs$pid5sf

  full_only_text <- as.character(pid_items$Text[is.na(pid_items$SF)])[1]
  expect_true(nzchar(full_only_text))

  leaked_item <- plant("pid5sf", function(j) {
    j$items[[length(j$items) + 1L]] <- list(
      number = 101L,
      name = "pid5sf_101",
      text = full_only_text
    )
    j
  })
  expect_identical(export_report(leaked_item, spec), "items.length")

  dropped_item <- plant("pid5sf", function(j) {
    j$items[[42]] <- NULL
    j
  })
  expect_identical(export_report(dropped_item, spec), "items.length")

  swapped_items <- plant("pid5sf", function(j) {
    j$items[c(42, 43)] <- j$items[c(43, 42)]
    j
  })
  expect_setequal(
    export_report(swapped_items, spec),
    c("number", "name", "text")
  )

  changed_max <- plant("pid5sf", function(j) {
    j$maxItem <- 220L
    j
  })
  expect_identical(export_report(changed_max, spec), "maxItem")
})

test_that("the export report discriminates a padding defect on the PID-5 BF", {
  spec <- json_specs$pid5bf

  repadded_name <- plant("pid5bf", function(j) {
    j$items[[7]]$name <- sub("_(\\d+)$", "_0\\1", j$items[[7]]$name)
    j
  })
  expect_identical(export_report(repadded_name, spec), "name")
})
