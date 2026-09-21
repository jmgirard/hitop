# Locks the committed JSON exports (inst/extdata/pid5.json, pid5sf.json,
# pid5bf.json, hitopsr.json, hitopbr.json) to the keying tables and the
# instruction objects by parse-and-compare (IP2, D-010): the expected side is
# read from the tables and R/sysdata.rda, never from the file. Regenerate via
# data-raw/artifacts.R (rebuild_formats = "json").
#
# Each spec states its form's item count. Every form numbers its items 1 to
# `count`, so the expected numbers are `seq_len(count)`, in ascending order
# (D-065). `pid_items` numbers three forms in three columns, each NA on the
# rows its form omits. Each item's expected text is the text of the table row
# its form numbers with that item's number, looked up by number rather than
# rebuilt by the writer's own subset and sort.

json_specs <- list(
  pid5 = list(
    stem = "pid5",
    count = 220L,
    items = pid_items,
    number_col = "FULL",
    instructions = hitop:::pid_instructions
  ),
  pid5sf = list(
    stem = "pid5sf",
    count = 100L,
    items = pid_items,
    number_col = "SF",
    instructions = hitop:::pid_instructions
  ),
  pid5bf = list(
    stem = "pid5bf",
    count = 25L,
    items = pid_items,
    number_col = "BF",
    instructions = hitop:::pid_instructions
  ),
  hitopsr = list(
    stem = "hitopsr",
    count = 405L,
    items = hitopsr_items,
    number_col = "HSR",
    instructions = hitop:::hitopsr_instructions
  ),
  hitopbr = list(
    stem = "hitopbr",
    count = 45L,
    items = hitopbr_items,
    number_col = "HBR",
    instructions = hitop:::hitopbr_instructions
  )
)

# The text of the table row that the spec's form numbers `n`, one per element
# of `n`; NA where no single row carries that number.
table_text <- function(spec, n) {
  col <- as.integer(spec$items[[spec$number_col]])
  vapply(n, function(k) {
    row <- which(col == k)
    if (length(row) == 1L) as.character(spec$items$Text[row]) else NA_character_
  }, character(1))
}

json_path <- function(stem) {
  system.file("extdata", paste0(stem, ".json"), package = "hitop")
}

manifest_build_date <- function(file) {
  m <- latest_manifest()
  m$build_date[m$file == file]
}

# D-063's field list, one key set per object level of format 1.0.
format_keys <- list(
  top = c(
    "format", "package", "packageVersion", "buildDate", "stem", "maxItem",
    "instructions", "items"
  ),
  instructions = c("start", "options"),
  options = c("value", "label"),
  items = c("number", "name", "text")
)

# A JSON scalar as `fromJSON(simplifyVector = FALSE)` reads it: an atomic
# length-one value. A JSON array reads as a list, so a boxed scalar fails
# here. `is_number()` takes 7 and 7.0 alike; the item `number` check below
# separates them by R type (7 reads as integer, 7.0 as double).
is_string <- function(x) is.character(x) && length(x) == 1L && !is.na(x)
is_number <- function(x) is.numeric(x) && length(x) == 1L && !is.na(x)
is_int <- function(x) is.integer(x) && length(x) == 1L && !is.na(x)

# Every way a file can disagree with its tables, each named so a failing
# plant below identifies which field it broke and the shipped file reports
# every disagreement at once rather than the first. The file is read with
# no simplification, so each check sees the JSON type as written.
export_report <- function(path, spec) {
  j <- jsonlite::fromJSON(path, simplifyVector = FALSE)
  out <- character(0)
  note <- function(ok, what) if (!isTRUE(ok)) out <<- c(out, what)

  number <- seq_len(spec$count)
  max_n <- spec$count
  # One field of every entry: each must pass `ok`, and the values must equal
  # `want`. An entry failing `ok` fails the field without reaching `want`.
  field_is <- function(rows, field, ok, want) {
    vals <- lapply(rows, function(r) r[[field]])
    all(vapply(vals, ok, logical(1))) &&
      identical(unlist(vals, use.names = FALSE), want)
  }
  keys_are <- function(x, level) {
    is.list(x) && identical(sort(names(x)), sort(format_keys[[level]]))
  }
  entries_keyed <- function(rows, level) {
    is.list(rows) && all(vapply(rows, keys_are, logical(1), level = level))
  }

  note(keys_are(j, "top"), "keys.top")
  note(keys_are(j$instructions, "instructions"), "keys.instructions")
  note(entries_keyed(j$instructions$options, "options"), "keys.options")
  note(entries_keyed(j$items, "items"), "keys.items")

  note(is_string(j$format) && j$format == "1.0", "format")
  note(is_string(j$package) && j$package == "hitop", "package")
  note(
    is_string(j$packageVersion) &&
      j$packageVersion == as.character(utils::packageVersion("hitop")),
    "packageVersion"
  )
  note(
    is_string(j$buildDate) &&
      identical(
        as.Date(j$buildDate),
        manifest_build_date(paste0(spec$stem, ".json"))
      ),
    "buildDate"
  )
  note(is_string(j$stem) && j$stem == spec$stem, "stem")
  note(is_int(j$maxItem) && j$maxItem == max_n, "maxItem")
  note(
    is_string(j$instructions$start) &&
      j$instructions$start == spec$instructions$start,
    "start"
  )

  opts <- j$instructions$options
  note(
    field_is(
      opts, "value", is_int, as.integer(spec$instructions$options$value)
    ),
    "options.value"
  )
  note(
    field_is(
      opts, "label", is_string, as.character(spec$instructions$options$label)
    ),
    "options.label"
  )

  note(length(j$items) == length(number), "items.length")
  if (length(j$items) == length(number)) {
    item_number <- lapply(j$items, function(r) r$number)
    note(
      all(vapply(item_number, is_number, logical(1))) &&
        identical(as.integer(unlist(item_number)), number),
      "number"
    )
    note(
      all(vapply(
        Filter(is_number, item_number), is.integer, logical(1)
      )),
      "number.type"
    )
    note(
      field_is(
        j$items, "name", is_string,
        hitop:::item_names(paste0(spec$stem, "_"), number, max_n)
      ),
      "name"
    )
    # Each item's text against the row its own number names, so a swapped
    # pair of whole items leaves `text` silent and a substituted text with
    # the right number does not.
    own_number <- vapply(
      item_number,
      function(x) if (is_number(x)) as.integer(x) else NA_integer_,
      integer(1)
    )
    note(
      field_is(j$items, "text", is_string, table_text(spec, own_number)),
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

read_bytes <- function(path) readBin(path, "raw", file.size(path))

# The writer itself (R/json_export.R). A table whose number column is out of
# row order and holds an NA must come out as its non-NA rows in ascending
# number order (D-065), whatever the row order.
test_that("write_instrument_json() writes non-NA rows in ascending number order", {
  spec <- list(
    stem = "demo",
    items = data.frame(
      Num = c(3L, NA, 1L, 2L),
      Text = c("third", "omitted", "first", "second"),
      stringsAsFactors = FALSE
    ),
    number_col = "Num",
    instructions = hitop:::hitopbr_instructions
  )
  path <- withr::local_tempfile(fileext = ".json")
  hitop:::write_instrument_json(spec, path, build_date = as.Date("2026-01-02"))

  j <- jsonlite::fromJSON(path, simplifyVector = FALSE)
  items <- j$items
  expect_identical(vapply(items, function(r) r$number, integer(1)), 1:3)
  expect_identical(
    vapply(items, function(r) r$text, character(1)),
    c("first", "second", "third")
  )
  expect_identical(
    vapply(items, function(r) r$name, character(1)),
    c("demo_1", "demo_2", "demo_3")
  )
  expect_identical(j$maxItem, 3L)
  expect_identical(j$buildDate, "2026-01-02")
})

# A fresh write of each spec at its manifest row's date is the committed file
# byte for byte, so a writer edit that was never rerun into inst/extdata/
# reds here. The bytes carry `packageVersion`, so a version bump without a
# rebuild reds here too. The control shows the comparison can fail: the same
# write a day later differs in `buildDate` alone and is not identical.
test_that("write_instrument_json() rebuilds each committed export byte for byte", {
  for (stem in names(json_specs)) {
    date <- manifest_build_date(paste0(stem, ".json"))
    path <- withr::local_tempfile(fileext = ".json")
    hitop:::write_instrument_json(json_specs[[stem]], path, build_date = date)
    expect_identical(read_bytes(path), read_bytes(json_path(stem)), info = stem)
  }

  later <- withr::local_tempfile(fileext = ".json")
  hitop:::write_instrument_json(
    json_specs$hitopbr,
    later,
    build_date = manifest_build_date("hitopbr.json") + 1
  )
  expect_false(identical(read_bytes(later), read_bytes(json_path("hitopbr"))))
})

# The report can fail: each plant alters one field of a temporary copy and
# must be reported under that field's name (check discrimination).
#
# The copy is parsed with no simplification and written back with every
# atomic value unboxed and `auto_unbox = FALSE`, so each JSON type survives
# the round trip as parsed: a scalar stays a scalar, an array stays an array
# even at length one. `mutate` edits the parsed list. `edit` edits the
# written text, for a defect the list cannot carry (a number written as
# `7.0`), and must change it. Each plant writes to its own tempfile, removed
# when the calling test ends.
unbox_scalars <- function(x) {
  if (is.list(x)) lapply(x, unbox_scalars) else jsonlite::unbox(x)
}

plant <- function(stem, mutate = identity, edit = identity) {
  j <- mutate(jsonlite::fromJSON(json_path(stem), simplifyVector = FALSE))
  txt <- as.character(
    jsonlite::toJSON(unbox_scalars(j), auto_unbox = FALSE, pretty = TRUE)
  )
  edited <- edit(txt)
  if (!identical(edit, identity) && identical(edited, txt)) {
    stop("the plant's text edit changed nothing", call. = FALSE)
  }
  tmp <- withr::local_tempfile(fileext = ".json", .local_envir = parent.frame())
  writeLines(edited, tmp)
  tmp
}

test_that("an unaltered plant copy reports nothing", {
  for (stem in names(json_specs)) {
    expect_identical(
      export_report(plant(stem), json_specs[[stem]]),
      character(0),
      info = stem
    )
  }
})

test_that("the export report names the level of an extra key", {
  spec <- json_specs$hitopbr

  extra_top <- plant("hitopbr", function(j) {
    j$extra <- "x"
    j
  })
  expect_identical(export_report(extra_top, spec), "keys.top")

  extra_instructions <- plant("hitopbr", function(j) {
    j$instructions$extra <- "x"
    j
  })
  expect_identical(
    export_report(extra_instructions, spec),
    "keys.instructions"
  )

  extra_option <- plant("hitopbr", function(j) {
    j$instructions$options[[2]]$extra <- "x"
    j
  })
  expect_identical(export_report(extra_option, spec), "keys.options")

  extra_item <- plant("hitopbr", function(j) {
    j$items[[7]]$extra <- "x"
    j
  })
  expect_identical(export_report(extra_item, spec), "keys.items")
})

test_that("the export report reads JSON types as written", {
  spec <- json_specs$hitopbr

  boxed_format <- plant("hitopbr", function(j) {
    j$format <- list(j$format)
    j
  })
  expect_identical(export_report(boxed_format, spec), "format")

  double_number <- plant("hitopbr", edit = function(txt) {
    sub("\"number\": 7,", "\"number\": 7.0,", txt, fixed = TRUE)
  })
  expect_identical(export_report(double_number, spec), "number.type")
})

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

  # A swapped pair of whole items keeps each text with its own number, so
  # the order checks report it and the text lookup stays silent.
  swapped_items <- plant("hitopbr", function(j) {
    j$items[c(7, 8)] <- j$items[c(8, 7)]
    j
  })
  expect_setequal(export_report(swapped_items, spec), c("number", "name"))

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
# columns, so a form's export can disagree with the table in two ways a
# one-form export cannot: it can carry an item another form owns, and it can
# number its items to another form's width. `leaked_item` and
# `substituted_text` are the first way: an added item changes the count, and
# a substituted text keeps it. `changed_max` and `repadded_name` are the
# second way at its two fields, `maxItem` and the padding of a name. The
# remaining two plants repeat on a multi-form export what the HiTOP-BR block
# above plants on a one-form one, a dropped item and a swapped pair, because
# the expected side here is a subset of its table rather than the whole of
# it. `leaked_item` and `dropped_item` both report `items.length`, because a
# count that disagrees leaves nothing to compare item by item.
test_that("the export report discriminates a wrong-form defect on the PID-5 SF", {
  spec <- json_specs$pid5sf

  full_only_text <- as.character(pid_items$Text[is.na(pid_items$SF)])[1]
  expect_true(nzchar(full_only_text))
  expect_false(full_only_text %in% pid_items$Text[!is.na(pid_items$SF)])

  substituted_text <- plant("pid5sf", function(j) {
    j$items[[42]]$text <- full_only_text
    j
  })
  expect_identical(export_report(substituted_text, spec), "text")

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
  expect_setequal(export_report(swapped_items, spec), c("number", "name"))

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
