# Locks the committed JSON exports (inst/extdata/hitopsr.json,
# hitopbr.json) to the keying tables and the instruction objects by
# parse-and-compare (IP2, D-010): the expected side is read from the tables
# and R/sysdata.rda, never from the file. Regenerate via
# data-raw/artifacts.R (rebuild_formats = "json").

json_specs <- list(
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

  number <- as.integer(spec$items[[spec$number_col]])
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
      identical(pull(j$items, "text", character(1)), spec$items$Text),
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
