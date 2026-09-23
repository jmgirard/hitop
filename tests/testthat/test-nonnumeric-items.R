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

as_double_frame <- function(data) {
  data[] <- lapply(data, as.double)
  data
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

test_that("positional items check the column at each position when names repeat", {
  for (case in nonnumeric_cases) {
    info <- case_label(case)
    data <- case$data
    n <- ncol(data)
    # A copy of the first column goes first, under the same name, and the
    # original first column (now at position 2) becomes choice text. Scoring
    # positions 2 to n + 1 reads the choice text, not the numeric copy.
    data <- cbind(data[1], data)
    data[[2]] <- choice_text(data[[2]])
    got <- record(run_case(case, data, items = 2:(n + 1)))
    expect_true(inherits(got$error, "hitop_nonnumeric_items"), info = info)
    expect_equal(length(got$warnings), 0L, info = info)
  }
})

test_that("positional items score data with an empty or NA column name", {
  for (case in nonnumeric_cases) {
    for (blank in list("", NA_character_)) {
      info <- paste(case_label(case), "/", deparse(blank))
      data <- as_double_frame(case$data)
      named <- run_case(case, data, items = seq_along(data))
      names(data)[[2]] <- blank
      expect_identical(run_case(case, data, items = seq_along(data)), named,
                       info = info)
      # A refused column with such a name is named by its position.
      data[[2]] <- choice_text(data[[2]])
      e <- catch_error(run_case(case, data, items = seq_along(data)))
      expect_true(inherits(e, "hitop_nonnumeric_items"), info = info)
      if (!inherits(e, "hitop_nonnumeric_items")) next
      expect_match(cli::ansi_strip(conditionMessage(e)), "Column 2",
                   fixed = TRUE, info = info)
      expect_identical(rlang::call_name(conditionCall(e)), case$fn, info = info)
    }
  }
})

test_that("the factor tip shows only when a refused column is a factor", {
  case <- nonnumeric_cases[[3]] # score_pid5 BF
  tip <- "as.numeric(as.character(x))"
  text_only <- case$data
  text_only[[1]] <- choice_text(text_only[[1]])
  e <- catch_error(run_case(case, text_only))
  expect_s3_class(e, "hitop_nonnumeric_items")
  expect_false(grepl(tip, cli::ansi_strip(conditionMessage(e)), fixed = TRUE))
  with_factor <- text_only
  with_factor[[2]] <- factor(with_factor[[2]])
  e <- catch_error(run_case(case, with_factor))
  expect_s3_class(e, "hitop_nonnumeric_items")
  expect_match(cli::ansi_strip(conditionMessage(e)), tip, fixed = TRUE)
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
    # Each bad argument, and the start of the message its own check raises.
    bad_args <- list(
      srange = list(args = list(srange = c(3, 0)),
                    msg = "The second `srange` value must be greater"),
      items = list(args = list(items = rep(names(data)[[2]], ncol(data))),
                   msg = "The `items` argument must map each item")
    )
    if (startsWith(case$fn, "reliability_")) {
      bad_args$flag <- list(args = list(alpha = "yes"),
                            msg = "The `alpha` argument must be")
      bad_args$omega <- list(args = list(omega = "yes"),
                             msg = "The `omega` argument must be")
    } else {
      if (startsWith(case$fn, "score_")) {
        bad_args$calc_se <- list(args = list(calc_se = "yes"),
                                 msg = "The `calc_se` argument must be")
      }
      bad_args$prefix <- list(args = list(prefix = 1),
                              msg = "The `prefix` argument must be")
      bad_args$flag <- list(args = list(append = "yes"),
                            msg = "The `append` argument must be")
    }
    for (what in names(bad_args)) {
      args <- bad_args[[what]]$args
      items <- if (is.null(args$items)) names(data) else args$items
      args$items <- NULL
      e <- catch_error(do.call(run_case, c(list(case, data, items), args)))
      expect_false(inherits(e, "hitop_nonnumeric_items"),
                   info = paste(info, "/", what))
      expect_true(
        inherits(e, "rlang_error") &&
          startsWith(cli::ansi_strip(conditionMessage(e)), bad_args[[what]]$msg),
        info = paste(info, "/", what)
      )
    }
    # A `data` that is not a data frame has no columns to refuse, so its own
    # check must answer.
    e <- catch_error(run_case(case, as.list(data), names(data)))
    expect_false(inherits(e, "hitop_nonnumeric_items"),
                 info = paste(info, "/ data"))
    expect_true(
      inherits(e, "rlang_error") &&
        startsWith(cli::ansi_strip(conditionMessage(e)),
                   "The `data` argument must be a data frame."),
      info = paste(info, "/ data")
    )
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

test_that("the text \"Inf\" and \"0x1A\" score as Inf and 26 do", {
  for (case in nonnumeric_cases) {
    base <- as_double_frame(case$data)
    col <- names(base)[[1]]
    num <- base
    num[[col]][1:2] <- c(Inf, 26)
    text <- base
    text[[col]] <- as.character(base[[col]])
    text[[col]][1:2] <- c("Inf", "0x1A")
    expect_identical(run_case(case, text), run_case(case, num),
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

# ---- SPSS user-missing codes, 64-bit integers, labelled digit text -----------

# An SPSS column declares some codes missing. haven keeps them as values, so
# as.numeric() would score a 99 as an answer. Each variant returns the column
# and the declared-missing value the message must show, which is the first one
# in row order. Rows 2 and 4 hold codes; row 2 comes first.
spss_variants <- list(
  `double, na_values outside srange` = function(x) {
    x[c(2, 4)] <- c(99, 98)
    list(column = haven::labelled_spss(x, na_values = c(98, 99)),
         shown = "holds 99,")
  },
  `double, na_values inside srange` = function(x) {
    x[] <- 1
    x[c(2, 4)] <- 3
    list(column = haven::labelled_spss(x, na_values = 3), shown = "holds 3,")
  },
  `double, na_range with an infinite bound` = function(x) {
    x[c(2, 4)] <- c(99, 98)
    list(column = haven::labelled_spss(x, na_range = c(90, Inf)),
         shown = "holds 99,")
  },
  `character, na_values` = function(x) {
    x <- as.character(x)
    x[c(2, 4)] <- c("99", "98")
    list(column = haven::labelled_spss(x, na_values = c("98", "99")),
         shown = "holds \"99\",")
  }
)

test_that("an SPSS column holding a declared-missing code is refused", {
  skip_if_not_installed("haven")
  for (case in nonnumeric_cases) {
    base <- as_double_frame(case$data)
    positions <- c(first = 1L)
    if (length(case$reverse) > 0) {
      positions <- c(positions, reverse = case$reverse[[1]])
    }
    for (where in names(positions)) {
      col <- names(base)[[positions[[where]]]]
      for (variant in names(spss_variants)) {
        info <- paste(case_label(case), "/", where, "/", variant)
        built <- spss_variants[[variant]](base[[col]])
        data <- base
        data[[col]] <- built$column
        e <- catch_error(run_case(case, data))
        expect_true(inherits(e, "hitop_nonnumeric_items"), info = info)
        if (!inherits(e, "hitop_nonnumeric_items")) next
        msg <- cli::ansi_strip(conditionMessage(e))
        expect_true(grepl(col, msg, fixed = TRUE), info = info)
        expect_true(grepl(built$shown, msg, fixed = TRUE), info = info)
        expect_true(grepl("haven::zap_missing()", msg, fixed = TRUE),
                    info = info)
      }
    }
  }
})

test_that("an SPSS column declaring codes it does not hold scores as its double does", {
  # A regression guard: this passes before M110 and must keep passing.
  skip_if_not_installed("haven")
  for (case in nonnumeric_cases) {
    base <- as_double_frame(case$data)
    col <- names(base)[[1]]
    declared <- list(
      na_values = haven::labelled_spss(base[[col]], na_values = 99),
      na_range = haven::labelled_spss(base[[col]], na_range = c(90, Inf))
    )
    for (how in names(declared)) {
      data <- base
      data[[col]] <- declared[[how]]
      expect_identical(run_case(case, data), run_case(case, base),
                       info = paste(case_label(case), "/", how))
    }
  }
})

# A stand-in for a 64-bit integer column read without bit64 loaded: a double
# vector classed "integer64". Apart from -0 (bit64's NA pattern), its values
# are not real integer64 bit patterns. The refusal reads only the class, so it
# needs neither real bit patterns nor bit64.
integer64_column <- function(n) {
  structure(c(-0, as.double(seq_len(n - 1L))), class = "integer64")
}

test_that("an integer64 column is refused", {
  for (case in nonnumeric_cases) {
    info <- case_label(case)
    col <- names(case$data)[[1]]
    data <- case$data
    data[[col]] <- integer64_column(nrow(data))
    e <- catch_error(run_case(case, data))
    expect_true(inherits(e, "hitop_nonnumeric_items"), info = info)
    if (!inherits(e, "hitop_nonnumeric_items")) next
    msg <- cli::ansi_strip(conditionMessage(e))
    expect_true(grepl(col, msg, fixed = TRUE), info = info)
    expect_true(grepl("integer64", msg, fixed = TRUE), info = info)
  }
})

test_that("a haven::labelled() digit-text column scores as its plain text does", {
  skip_if_not_installed("haven")
  for (case in nonnumeric_cases) {
    info <- case_label(case)
    base <- as_double_frame(case$data)
    col <- names(base)[[1]]
    text <- base
    text[[col]] <- as.character(base[[col]])
    labelled <- text
    labelled[[col]] <- haven::labelled(text[[col]], c(Low = "0"))
    expect_identical(run_case(case, labelled), run_case(case, text),
                     info = info)
  }
})

test_that("a haven::labelled() choice-text column is refused and shows the value", {
  # With haven loaded, as.numeric() on the labelled values aborts inside
  # haven's cast, so the parse test reads the unclassed values.
  skip_if_not_installed("haven")
  for (case in nonnumeric_cases) {
    info <- case_label(case)
    col <- names(case$data)[[1]]
    data <- case$data
    data[[col]] <- haven::labelled(
      c("2", "Moderately", rep("1", nrow(data) - 2L)),
      c(Some = "Moderately")
    )
    e <- catch_error(run_case(case, data))
    expect_true(inherits(e, "hitop_nonnumeric_items"), info = info)
    if (!inherits(e, "hitop_nonnumeric_items")) next
    expect_true(
      grepl("\"Moderately\"", cli::ansi_strip(conditionMessage(e)), fixed = TRUE),
      info = info
    )
  }
})

test_that("a refused value made only of invisible characters is shown by its code points", {
  # trimws() strips only [ \t\r\n], so these cells are not blank and are
  # refused. Separators, a control and format marks, alone and mixed.
  invisible <- list(
    "\v" = "U+000B",
    " " = "U+00A0",
    " " = "U+2009",
    "　" = "U+3000",
    "﻿" = "U+FEFF",
    "​" = "U+200B",
    "  " = c("U+00A0", "U+2009"),
    " \v" = c("U+00A0", "U+000B")
  )
  # One visible character, before or after the invisible one: shown as text.
  visible <- list(" x" = "x\"", "1 " = "\"1")
  for (case in nonnumeric_cases) {
    col <- names(case$data)[[1]]
    for (value in c(names(invisible), names(visible))) {
      info <- paste(case_label(case), "/", utf8ToInt(value))
      data <- case$data
      data[[col]] <- c(value, rep("1", nrow(data) - 1L))
      e <- catch_error(run_case(case, data))
      expect_true(inherits(e, "hitop_nonnumeric_items"), info = info)
      if (!inherits(e, "hitop_nonnumeric_items")) next
      msg <- cli::ansi_strip(conditionMessage(e))
      if (value %in% names(invisible)) {
        for (point in invisible[[value]]) {
          expect_true(grepl(point, msg, fixed = TRUE), info = info)
        }
      } else {
        expect_false(grepl("U+00A0", msg, fixed = TRUE), info = info)
        expect_true(grepl(visible[[value]], msg, fixed = TRUE), info = info)
      }
    }
  }
})
