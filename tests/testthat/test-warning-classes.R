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

# The report is built by `warn_unmatched_items()`, which escapes braces before
# cli interpolates. The inline block this replaced did not, so an item text
# containing `{...}` ERRORED ("Could not evaluate cli `{}` expression") instead
# of warning. Nothing else pins that, and the changelog covers it in one line.
test_that("an item text containing braces warns rather than erroring", {
  df <- data.frame(a = c(1, 2))
  braced <- "How often do you feel {sad}?"

  expect_no_error(
    caught <- collect_warnings(
      rename_hitopsr_items(df, method = "text", item_cols = "a", item_text = braced)
    )
  )
  expect_true(
    any(vapply(
      caught$warnings,
      function(w) inherits(w, "hitop_unmatched_items"),
      logical(1)
    ))
  )
  # The braces survive into the message rather than being interpolated away.
  msg <- paste(vapply(caught$warnings, conditionMessage, character(1)), collapse = " ")
  expect_true(grepl("{sad}", msg, fixed = TRUE))
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

# ---- T6 guard: no unclassed warning in the family ---------------------------

# The guard reads the five functions themselves rather than their source files.
# Reading `R/*.R` off disk skipped under `R CMD check`, where the working
# directory is `<pkg>.Rcheck` and no source tree is unpacked beside it — so the
# guard never ran in CI, which is exactly where a later classless warning would
# slip through. A function object carries its body in both settings.
family_functions <- function() {
  list(
    rename_pid5_items = rename_pid5_items,
    rename_hitopsr_items = rename_hitopsr_items,
    label_pid5 = label_pid5,
    label_hitopsr = label_hitopsr,
    label_hitopbr = label_hitopbr
  )
}

# The three ways this package could raise a warning. `cli::cli_warn()` is the
# convention, but a bare `warning()` is what the base-R coercion leak T4 removed
# looked like, and `rlang::warn()` is one keystroke away from `cli_warn`, so all
# three are in the domain: a guard that watched only `cli_warn` would have been
# blind to the very shape this milestone fixed.
warn_fns <- list(
  quote(cli::cli_warn), quote(cli_warn),
  quote(warning), quote(base::warning),
  quote(rlang::warn), quote(warn)
)

# Every warning-raising call in `fn`'s body, as language objects. Walking the
# body rather than the file text is what lets a call spanning several lines be
# read whole, and what makes `class =` a named argument rather than a substring.
warn_calls <- function(fn) {
  found <- list()
  walk <- function(e) {
    if (!is.call(e)) return(invisible(NULL))
    if (any(vapply(warn_fns, identical, logical(1), y = e[[1]]))) {
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
  walk(body(fn))
  found
}

# A call is guarded only by a class this package promises: a literal string
# starting with `hitop_`. `class = NULL`, and a conditional such as
# `class = if (flag) "hitop_x"`, both name a `class` argument and both leave the
# warning unclassed on at least one path, so presence of the argument is not
# enough.
unclassed <- function(fns) {
  out <- character(0)
  for (nm in names(fns)) {
    for (call in warn_calls(fns[[nm]])) {
      cls <- if ("class" %in% names(call)) call[["class"]] else NULL
      ok <- is.character(cls) && length(cls) >= 1L && all(startsWith(cls, "hitop_"))
      if (!ok) out <- c(out, paste0(nm, ": ", deparse(call)[[1]]))
    }
  }
  out
}

test_that("every warning the family raises passes a hitop_ class", {
  fns <- family_functions()

  # The scan is worthless over an empty domain, so pin the count it found. The
  # floor is the ten inline sites — the eight nothing-matched and the two
  # completeness reports — stated as a floor rather than an equality so that a
  # properly classed warning added later goes red on the class assertion below,
  # which says what is wrong, rather than on an arithmetic mismatch. The
  # family's eleventh warning, the unmatched-input report, is raised from
  # `warn_unmatched_items()` in `R/util.R` and so is not one of these calls.
  calls <- unlist(lapply(fns, warn_calls), recursive = FALSE)
  expect_gte(length(calls), 10L)

  expect_identical(unclassed(fns), character(0))
})

test_that("the scan catches every unclassed shape it must", {
  # Each of these raises a warning no caller can catch by a package class, on
  # at least one path, and each is reported. The single- and multi-line call
  # shapes are both present in the family, so both are planted.
  bad <- list(
    single = function() cli::cli_warn("no class here."),
    multi = function() {
      cli::cli_warn(c(
        "no class here either.",
        "i" = "a bullet."
      ))
    },
    null_class = function() cli::cli_warn("x", class = NULL),
    conditional = function(flag) cli::cli_warn("y", class = if (flag) "hitop_z"),
    foreign_class = function() cli::cli_warn("z", class = "not_ours"),
    base_warning = function() warning("a bare base-R warning"),
    rlang_warn = function() rlang::warn("an rlang warning, unclassed")
  )
  for (nm in names(bad)) {
    expect_length(unclassed(bad[nm]), 1L)
  }

  # The control passes for the reason claimed, not merely because nothing was
  # scanned: the scan sees the call and accepts its class.
  good <- list(ok = function() cli::cli_warn("fine.", class = "hitop_x"))
  expect_length(warn_calls(good$ok), 1L)
  expect_length(unclassed(good), 0L)
})

test_that("the guard reads a function body, so it runs wherever the tests do", {
  # The predecessor read `R/*.R` off disk and skipped under `R CMD check`. A
  # body is present in an installed package too, so nothing here can skip.
  fns <- family_functions()
  expect_true(all(vapply(fns, is.function, logical(1))))
  expect_true(all(vapply(fns, function(f) length(warn_calls(f)) > 0L, logical(1))))
})
