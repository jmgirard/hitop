# The `rename_*()` / `label_*()` family's warning contract (D-059).
#
# Every warning these five functions raise carries a condition class, so a
# caller catches or suppresses it by class and these tests assert the class
# rather than the message prose, which the package promises nothing about.
#
# The guard at the foot scans the five source files for a `cli::cli_warn()`
# call passing no `class =`, so a warning added later cannot arrive classless.

# Whether any warning `expr` raises carries `class`. `collect_warnings()`
# (helper-fixtures.R) keeps every warning rather than the first, which matters
# here: several of these paths raise two, and `expect_warning()` would let the
# second escape.
raises_class <- function(expr, class) {
  caught <- collect_warnings(expr)
  any(vapply(caught$warnings, function(w) inherits(w, class), logical(1)))
}

# ---- AC1: the HiTOP sibling's unmatched item-text report --------------------

test_that("rename_hitopsr_items(method = 'text') reports unmatched text by class", {
  df <- data.frame(a = c(1, 2), b = c(3, 4))
  expect_true(
    raises_class(
      rename_hitopsr_items(
        df,
        method = "text",
        item_cols = c("a", "b"),
        item_text = c("not an item of this instrument", "nor is this")
      ),
      "hitop_unmatched_items"
    )
  )
})

# ---- AC2: the eight nothing-matched paths -----------------------------------

test_that("rename_pid5_items(method = 'number') reports nothing matched by class", {
  expect_true(
    raises_class(
      rename_pid5_items(data.frame(age = c(30, 40))),
      "hitop_no_columns_matched"
    )
  )
})

test_that("rename_hitopsr_items(method = 'original') reports nothing matched by class", {
  expect_true(
    raises_class(
      rename_hitopsr_items(data.frame(Wrong_Name = c(1, 2)), method = "original"),
      "hitop_no_columns_matched"
    )
  )
})

test_that("the label_*() helpers report nothing matched by class, on both targets", {
  df <- data.frame(unrelated = c(1, 2))
  calls <- list(
    "label_pid5 items" = function() label_pid5(df, target = "items"),
    "label_pid5 scales" = function() label_pid5(df, target = "scales"),
    "label_hitopsr items" = function() label_hitopsr(df, target = "items"),
    "label_hitopsr scales" = function() label_hitopsr(df, target = "scales"),
    "label_hitopbr items" = function() label_hitopbr(df, target = "items"),
    "label_hitopbr scales" = function() label_hitopbr(df, target = "scales")
  )
  for (nm in names(calls)) {
    expect_true(
      raises_class(calls[[nm]](), "hitop_no_columns_matched"),
      info = nm
    )
  }
})

# ---- AC3: the two completeness reports --------------------------------------

test_that("rename_pid5_items reports an incomplete rename by class", {
  df <- data.frame(pid_1 = c(0, 1), pid_2 = c(2, 3))
  expect_true(
    raises_class(rename_pid5_items(df, version = "FULL"), "hitop_incomplete_rename")
  )
})

test_that("rename_hitopsr_items reports an incomplete rename by class", {
  df <- as.data.frame(matrix(
    0,
    nrow = 2,
    ncol = 3,
    dimnames = list(NULL, head(hitopsr_items$Original, 3))
  ))
  expect_true(
    raises_class(rename_hitopsr_items(df, method = "original"), "hitop_incomplete_rename")
  )
})

# ---- AC4: a column number past R's integer range ----------------------------

test_that("a past-integer-range column raises only hitop_* warnings and is reported", {
  big <- "pid_99999999999"
  df <- frame_of_cols(c("pid_1", big))

  caught <- collect_warnings(rename_pid5_items(df, version = "FULL"))
  expect_gt(length(caught$warnings), 0)
  expect_true(all(vapply(
    caught$warnings,
    function(w) any(startsWith(class(w), "hitop_")),
    logical(1)
  )))

  msg <- NULL
  withCallingHandlers(
    rename_pid5_items(df, version = "FULL"),
    hitop_unmatched_items = function(w) {
      msg <<- conditionMessage(w)
      invokeRestart("muffleWarning")
    },
    warning = function(w) invokeRestart("muffleWarning")
  )
  expect_true(grepl(big, msg, fixed = TRUE))
})

# ---- AC5: <prefix>000 and a past-range number are both out of range ---------

test_that("the label_*() helpers place both out-of-range shapes in one group", {
  cases <- list(
    list(
      fn = function(d) label_pid5(d, target = "items", version = "FULL"),
      prefix = "pid5_",
      expected = item_names("pid5_", pid_items$FULL[!is.na(pid_items$FULL)], max_n = 220),
      max_n = 220
    ),
    list(
      fn = function(d) label_hitopsr(d, target = "items"),
      prefix = "hsr_",
      expected = item_names("hsr_", hitopsr_items$HSR),
      max_n = max(hitopsr_items$HSR)
    ),
    list(
      fn = function(d) label_hitopbr(d, target = "items"),
      prefix = "hbr_",
      expected = item_names("hbr_", hitopbr_items$HBR),
      max_n = max(hitopbr_items$HBR)
    )
  )

  for (case in cases) {
    zero <- paste0(case$prefix, "000")
    past <- paste0(case$prefix, "99999999999")
    df <- frame_of_cols(c(zero, past))

    expect_true(
      raises_class(case$fn(df), "hitop_unpadded_items"),
      info = case$prefix
    )

    groups <- unpadded_item_cols(
      c(zero, past),
      prefix = case$prefix,
      expected = case$expected,
      max_n = case$max_n
    )
    expect_setequal(groups$out_of_range, c(zero, past))
    expect_length(groups$mispadded, 0)
  }
})

# ---- controls: what must stay silent, and what must not be reported --------

test_that("a fully matching call is silent and an unraised class is not reported", {
  df <- frame_of_cols(item_names("hbr_", hitopbr_items$HBR))

  expect_length(collect_warnings(label_hitopbr(df, target = "items"))$warnings, 0)
  expect_false(
    raises_class(label_hitopbr(df, target = "items"), "hitop_no_columns_matched")
  )

  # The nothing-matched path raises one class and not the other, so a passing
  # assertion above is the class it names rather than any warning at all.
  expect_false(
    raises_class(
      rename_pid5_items(data.frame(age = c(30, 40))),
      "hitop_incomplete_rename"
    )
  )
})

# ---- T6 guard: no classless cli_warn() in the family ------------------------

# Read from the source tree: `R/` is not installed in this form, so this skips
# under `R CMD check` and runs under devtools::test(), where the files exist.
family_files <- function() {
  file.path(
    testthat::test_path("..", ".."),
    "R",
    c(
      "rename_pid5_items.R",
      "rename_hitopsr_items.R",
      "label_pid5.R",
      "label_hitopsr.R",
      "label_hitopbr.R"
    )
  )
}

# Every `cli_warn()` call in `path`, as language objects. Parsing rather than
# grepping is what lets a call spanning several lines be read whole, and what
# makes `class =` a named argument rather than a substring.
cli_warn_calls <- function(path) {
  found <- list()
  walk <- function(e) {
    if (!is.call(e)) return(invisible(NULL))
    fn <- e[[1]]
    if (identical(fn, quote(cli::cli_warn)) || identical(fn, quote(cli_warn))) {
      found[[length(found) + 1L]] <<- e
    }
    # An omitted argument (the second in `pid_items[keep, ]`) is the empty
    # symbol, and a local bound to it counts as missing, so it errors the
    # moment it is used. `[` on the call's list form reads it without that,
    # and the empty ones are dropped before anything binds them.
    kids <- as.list(e)
    keep <- vapply(
      seq_along(kids),
      function(i) !identical(kids[i], list(quote(expr = ))),
      logical(1)
    )
    for (kid in kids[keep]) walk(kid)
    invisible(NULL)
  }
  for (e in as.list(parse(path, keep.source = FALSE))) walk(e)
  found
}

classless <- function(paths) {
  out <- character(0)
  for (p in paths) {
    for (call in cli_warn_calls(p)) {
      if (!"class" %in% names(call)) {
        out <- c(out, paste0(basename(p), ": ", deparse(call)[[1]]))
      }
    }
  }
  out
}

test_that("every cli_warn() in the family passes a class", {
  paths <- family_files()
  skip_if(!all(file.exists(paths)), "R/ not available")

  calls <- unlist(lapply(paths, cli_warn_calls), recursive = FALSE)
  # The scan is worthless over an empty domain, so pin the count it found: the
  # eight nothing-matched sites and the two completeness sites. The family's
  # eleventh warning, the unmatched-input report, is raised from
  # `warn_unmatched_items()` in `R/util.R` and so is not one of these calls.
  expect_length(calls, 10)

  expect_identical(classless(paths), character(0))
})

test_that("the classless-cli_warn scan sees both call shapes it must catch", {
  dir <- tempfile("m085-scan-")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)

  single <- file.path(dir, "single.R")
  writeLines(
    c('f <- function() {', '  cli::cli_warn("no class here.")', '}'),
    single
  )

  multi <- file.path(dir, "multi.R")
  writeLines(
    c(
      'g <- function() {',
      '  cli::cli_warn(c(',
      '    "no class here either.",',
      '    "i" = "a bullet."',
      '  ))',
      '}'
    ),
    multi
  )

  classed <- file.path(dir, "classed.R")
  writeLines(
    c('h <- function() {', '  cli::cli_warn("fine.", class = "hitop_x")', '}'),
    classed
  )

  expect_length(classless(single), 1)
  expect_length(classless(multi), 1)
  expect_length(classless(classed), 0)
})
