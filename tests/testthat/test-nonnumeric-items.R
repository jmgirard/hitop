# The seven item-scoring functions refuse an item column they cannot read as
# numbers (class `hitop_nonnumeric_items`), and keep scoring the columns they
# accept exactly as before.
#
# Before this refusal, a column of choice text became all `NA` with only base
# R's coercion warning, and a factor became its level codes with no warning at
# all. Accepted columns are compared against the same data held as doubles,
# which is the path every other scoring test already pins.

# ---- cases ------------------------------------------------------------------

# Item numbers keyed in reverse for one version column of a keying table.
reverse_numbers <- function(tbl, col) {
  keep <- tbl$Reverse %in% c(1, TRUE) & !is.na(tbl[[col]])
  sort(as.integer(tbl[[col]][keep]))
}

pid_case <- function(fn, version, data) {
  list(
    fn = fn, version = version,
    data = as.data.frame(utils::head(data, 20)),
    reverse = reverse_numbers(pid_items, version)
  )
}

nonnumeric_cases <- c(
  lapply(c("score_pid5", "reliability_pid5", "validity_pid5"), function(fn) {
    list(
      pid_case(fn, "FULL", sim_pid5),
      pid_case(fn, "SF", sim_pid5sf),
      pid_case(fn, "BF", sim_pid5bf)
    )
  }) |> unlist(recursive = FALSE),
  lapply(c("score_hitopsr", "reliability_hitopsr"), function(fn) {
    list(fn = fn, version = NULL,
         data = as.data.frame(utils::head(sim_hitopsr, 20)),
         reverse = reverse_numbers(hitopsr_items, "HSR"))
  }),
  lapply(c("score_hitopbr", "reliability_hitopbr"), function(fn) {
    list(fn = fn, version = NULL,
         data = as.data.frame(utils::head(sim_hitopbr, 20)),
         reverse = reverse_numbers(hitopbr_items, "HBR"))
  })
)

case_label <- function(case) {
  paste(c(case$fn, case$version), collapse = " ")
}

# Call a case's function on `data`. `...` overrides the defaults: append =
# FALSE for the functions that append, omega = FALSE for reliability (no
# lavaan), and the case's version for the PID-5.
run_case <- function(case, data, items = names(data), ...) {
  defaults <- if (startsWith(case$fn, "reliability_")) {
    list(omega = FALSE)
  } else {
    list(append = FALSE)
  }
  if (!is.null(case$version)) {
    defaults$version <- case$version
  }
  args <- utils::modifyList(defaults, list(...))
  do.call(case$fn, c(list(data = data, items = items), args))
}

catch_error <- function(expr) {
  tryCatch(expr, error = identity)
}

# The warnings a call raised, and its error, recorded together.
record <- function(expr) {
  warnings <- list()
  error <- withCallingHandlers(
    tryCatch({
      expr
      NULL
    }, error = identity),
    warning = function(w) {
      warnings[[length(warnings) + 1L]] <<- w
      invokeRestart("muffleWarning")
    }
  )
  list(error = error, warnings = warnings)
}

choice_text <- function(x) {
  rep_len(c("Not at all", "A little", "Moderately", "A lot"), length(x))
}

bad_columns <- list(
  factor = function(x) factor(x),
  ordered = function(x) factor(x, ordered = TRUE),
  `choice text` = choice_text,
  Date = function(x) as.Date("2026-01-01") + x,
  list = function(x) I(as.list(x))
)

# ---- AC1/AC2: every refused type, named, blamed on the exported call --------

test_that("each function refuses a factor, ordered factor, choice text, Date and list column", {
  for (case in nonnumeric_cases) {
    col <- names(case$data)[[1]]
    for (type in names(bad_columns)) {
      info <- paste(case_label(case), "/", type)
      data <- case$data
      data[[col]] <- bad_columns[[type]](data[[col]])
      e <- catch_error(run_case(case, data))
      expect_true(inherits(e, "hitop_nonnumeric_items"), info = info)
      if (!inherits(e, "hitop_nonnumeric_items")) next
      msg <- cli::ansi_strip(conditionMessage(e))
      expect_true(grepl(col, msg, fixed = TRUE), info = info)
      expect_true(grepl(class(data[[col]])[[1]], msg, fixed = TRUE), info = info)
      expect_identical(rlang::call_name(conditionCall(e)), case$fn, info = info)
      expect_true(grepl("numeric values", msg, fixed = TRUE), info = info)
    }
  }
})

test_that("a choice-text refusal shows the first value that does not parse", {
  for (case in nonnumeric_cases) {
    info <- case_label(case)
    col <- names(case$data)[[1]]
    data <- case$data
    # Digits, a blank and an NA come first: they parse or count as missing.
    data[[col]] <- c("2", "", NA, "Moderately", "A lot", rep("1", 15))
    e <- catch_error(run_case(case, data))
    expect_true(inherits(e, "hitop_nonnumeric_items"), info = info)
    if (!inherits(e, "hitop_nonnumeric_items")) next
    msg <- cli::ansi_strip(conditionMessage(e))
    expect_true(grepl("Moderately", msg, fixed = TRUE), info = info)
    expect_false(grepl("A lot", msg, fixed = TRUE), info = info)
  }
})

test_that("a refused column is found in the last and in a reverse-keyed position", {
  for (case in nonnumeric_cases) {
    items <- names(case$data)
    positions <- c(last = length(items))
    if (length(case$reverse) > 0) {
      positions <- c(positions, reverse = case$reverse[[1]])
    }
    for (where in names(positions)) {
      info <- paste(case_label(case), "/", where)
      col <- items[[positions[[where]]]]
      data <- case$data
      data[[col]] <- choice_text(data[[col]])
      e <- catch_error(run_case(case, data))
      expect_true(inherits(e, "hitop_nonnumeric_items"), info = info)
      if (!inherits(e, "hitop_nonnumeric_items")) next
      expect_true(
        grepl(col, cli::ansi_strip(conditionMessage(e)), fixed = TRUE),
        info = info
      )
    }
  }
})

test_that("two refused columns are both named", {
  for (case in nonnumeric_cases) {
    info <- case_label(case)
    items <- names(case$data)
    cols <- items[c(1, length(items))]
    data <- case$data
    data[[cols[[1]]]] <- factor(data[[cols[[1]]]])
    data[[cols[[2]]]] <- choice_text(data[[cols[[2]]]])
    e <- catch_error(run_case(case, data))
    expect_true(inherits(e, "hitop_nonnumeric_items"), info = info)
    if (!inherits(e, "hitop_nonnumeric_items")) next
    msg <- cli::ansi_strip(conditionMessage(e))
    expect_true(all(vapply(cols, grepl, logical(1), x = msg, fixed = TRUE)),
                info = info)
  }
})

test_that("21 refused columns name the first five in `items` order and count the rest", {
  for (case in nonnumeric_cases) {
    info <- case_label(case)
    items <- names(case$data)
    data <- case$data
    for (col in items[1:21]) {
      data[[col]] <- choice_text(data[[col]])
    }
    # Reversing the data frame's columns must not change which five are named:
    # the order is the caller's `items`, not the order of `data`.
    data <- data[rev(names(data))]
    e <- catch_error(run_case(case, data, items = items))
    expect_true(inherits(e, "hitop_nonnumeric_items"), info = info)
    if (!inherits(e, "hitop_nonnumeric_items")) next
    msg <- cli::ansi_strip(conditionMessage(e))
    at <- vapply(items[1:5], function(nm) regexpr(nm, msg, fixed = TRUE)[[1]],
                 integer(1))
    expect_true(all(at > 0), info = info)
    expect_false(is.unsorted(at), info = info)
    expect_false(grepl(items[[6]], msg, fixed = TRUE), info = info)
    expect_true(grepl("16 more", msg, fixed = TRUE), info = info)
  }
})

test_that("item positions name the refused column by its name in `data`", {
  case <- nonnumeric_cases[[3]] # score_pid5 BF
  data <- case$data
  data[[5]] <- choice_text(data[[5]])
  e <- catch_error(run_case(case, data, items = seq_along(data)))
  expect_s3_class(e, "hitop_nonnumeric_items")
  expect_match(cli::ansi_strip(conditionMessage(e)), names(data)[[5]],
               fixed = TRUE)
})

# ---- AC1/AC2: the module-columns and printed-layout paths -------------------

test_that("a module's `columns` are checked when `items` is omitted", {
  m <- hitop_module("hitopsr", scales = c("Agoraphobia", "Appetite Loss"))
  data <- as.data.frame(sim_hitopsr[sprintf("hsr_%03d", m$items)])
  attr(m, "columns") <- names(data)
  data[[3]] <- choice_text(data[[3]])
  for (fn in c("score_hitopsr", "reliability_hitopsr")) {
    args <- list(data = data, module = m)
    args <- c(args, if (fn == "score_hitopsr") list(append = FALSE) else list(omega = FALSE))
    e <- catch_error(do.call(fn, args))
    expect_true(inherits(e, "hitop_nonnumeric_items"), info = fn)
    if (!inherits(e, "hitop_nonnumeric_items")) next
    expect_true(grepl(names(data)[[3]], cli::ansi_strip(conditionMessage(e)),
                      fixed = TRUE), info = fn)
    expect_identical(rlang::call_name(conditionCall(e)), fn, info = fn)
  }
})

test_that("under layout = \"printed\" the refusal follows the caller's `items` order", {
  m <- hitop_module("hitopsr", scales = c("Agoraphobia", "Appetite Loss"))
  # The printed order reverses the instrument order, so the engine's permuted
  # vector puts the caller's last columns first.
  attr(m, "item_order") <- rev(m$items)
  data <- as.data.frame(sim_hitopsr[sprintf("hsr_%03d", rev(m$items))])
  names(data) <- sprintf("printed_%d", seq_along(data))
  for (i in seq_along(data)) {
    data[[i]] <- choice_text(data[[i]])
  }
  for (fn in c("score_hitopsr", "reliability_hitopsr")) {
    args <- list(data = data, items = names(data), module = m,
                 layout = "printed")
    args <- c(args, if (fn == "score_hitopsr") list(append = FALSE) else list(omega = FALSE))
    e <- catch_error(do.call(fn, args))
    expect_true(inherits(e, "hitop_nonnumeric_items"), info = fn)
    if (!inherits(e, "hitop_nonnumeric_items")) next
    msg <- cli::ansi_strip(conditionMessage(e))
    at <- vapply(names(data)[1:5], function(nm) regexpr(nm, msg, fixed = TRUE)[[1]],
                 integer(1))
    expect_true(all(at > 0), info = fn)
    expect_false(is.unsorted(at), info = fn)
    expect_false(grepl(names(data)[[8]], msg, fixed = TRUE), info = fn)
  }
})

# ---- AC3: where the refusal sits among the other checks ---------------------

test_that("a refused call raises no coercion warning", {
  for (case in nonnumeric_cases) {
    info <- case_label(case)
    col <- names(case$data)[[1]]
    data <- case$data
    data[[col]] <- choice_text(data[[col]])
    got <- record(run_case(case, data))
    expect_true(inherits(got$error, "hitop_nonnumeric_items"), info = info)
    expect_equal(length(got$warnings), 0L, info = info)
  }
})

test_that("argument checks run before the refusal", {
  for (case in nonnumeric_cases) {
    info <- case_label(case)
    col <- names(case$data)[[1]]
    data <- case$data
    data[[col]] <- choice_text(data[[col]])
    bad_args <- list(
      srange = list(srange = c(3, 0)),
      items = list(items = rep(names(data)[[2]], ncol(data)))
    )
    if (startsWith(case$fn, "reliability_")) {
      bad_args$flag <- list(alpha = "yes")
    } else {
      bad_args$prefix <- list(prefix = 1)
      bad_args$flag <- list(append = "yes")
    }
    for (what in names(bad_args)) {
      args <- bad_args[[what]]
      items <- if (is.null(args$items)) names(data) else args$items
      args$items <- NULL
      e <- catch_error(do.call(run_case, c(list(case, data, items), args)))
      expect_true(inherits(e, "error"), info = paste(info, "/", what))
      expect_false(inherits(e, "hitop_nonnumeric_items"),
                   info = paste(info, "/", what))
    }
  }
})

test_that("the refusal comes before the collision check and the srange warning", {
  for (case in nonnumeric_cases) {
    info <- case_label(case)
    col <- names(case$data)[[1]]
    data <- case$data
    args <- list(srange = c(1, 4))
    if (!startsWith(case$fn, "reliability_")) {
      # A column the call would append, so append = TRUE collides.
      produced <- names(run_case(case, case$data))[[1]]
      data[[produced]] <- 0
      args$append <- TRUE
    }
    data[[col]] <- choice_text(data[[col]])
    got <- record(do.call(run_case, c(list(case, data, names(case$data)), args)))
    expect_true(inherits(got$error, "hitop_nonnumeric_items"), info = info)
    expect_equal(length(got$warnings), 0L, info = info)
  }
})

# ---- AC4: accepted columns score as before ----------------------------------

as_double_frame <- function(data) {
  data[] <- lapply(data, as.double)
  data
}

test_that("integer, logical and digit-text columns score as their doubles do", {
  for (case in nonnumeric_cases) {
    base <- as_double_frame(case$data)
    col <- names(base)[[1]]

    integer_data <- base
    integer_data[[col]] <- as.integer(base[[col]])
    expect_identical(run_case(case, integer_data), run_case(case, base),
                     info = paste(case_label(case), "/ integer"))

    logical_base <- base
    logical_base[[col]] <- rep_len(c(0, 1), nrow(base))
    logical_data <- logical_base
    logical_data[[col]] <- rep_len(c(FALSE, TRUE), nrow(base))
    expect_identical(run_case(case, logical_data), run_case(case, logical_base),
                     info = paste(case_label(case), "/ logical"))

    # A blank, an NA and padded digits: the blank and the NA are missing.
    text_base <- base
    text_base[[col]][1:2] <- NA
    text_data <- base
    text_data[[col]] <- as.character(base[[col]])
    text_data[[col]][1] <- ""
    text_data[[col]][2] <- NA_character_
    text_data[[col]][3] <- paste0(" ", text_data[[col]][3], " ")
    expect_identical(run_case(case, text_data), run_case(case, text_base),
                     info = paste(case_label(case), "/ digit text"))
  }
})

test_that("a haven::labelled() double on a reverse-keyed item scores as its double does", {
  skip_if_not_installed("haven")
  for (case in nonnumeric_cases) {
    base <- as_double_frame(case$data)
    pos <- if (length(case$reverse) > 0) case$reverse[[1]] else 1L
    col <- names(base)[[pos]]
    base[[col]][[1]] <- NA
    labelled <- base
    labelled[[col]] <- haven::labelled(base[[col]], c(Low = min(base[[col]], na.rm = TRUE)))
    expect_identical(run_case(case, labelled), run_case(case, base),
                     info = case_label(case))
  }
})

test_that("the text \"NaN\" parses and the text \"NA\" is refused", {
  case <- nonnumeric_cases[[3]] # score_pid5 BF
  base <- as_double_frame(case$data)
  col <- names(base)[[1]]

  nan_base <- base
  nan_base[[col]][[1]] <- NaN
  nan_text <- base
  nan_text[[col]] <- as.character(base[[col]])
  nan_text[[col]][[1]] <- "NaN"
  expect_identical(run_case(case, nan_text), run_case(case, nan_base))

  na_text <- nan_text
  na_text[[col]][[1]] <- "NA"
  e <- catch_error(run_case(case, na_text))
  expect_s3_class(e, "hitop_nonnumeric_items")
  expect_match(cli::ansi_strip(conditionMessage(e)), "\"NA\"", fixed = TRUE)
})
