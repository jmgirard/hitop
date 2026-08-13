# Structural tests for the PID-5 profile plot.
#
# Plots are asserted over `ggplot2::ggplot_build()` layer data -- layer
# composition, row counts, plotted values, panel assignment -- never over a
# rendered image (D-030). Structural assertions inspect the plot object rather
# than its rendering, which is what makes image snapshots brittle across
# graphics-stack versions.
#
# Ground truth is never the function's own output (IP2): plotted values are
# compared against the `_t`/`_ptl` columns `norm_pid5()` produced, scale order
# against `pid_domains`/`pid_scales`, panel membership against the APA key's
# domain-defining map in `pid_domains$facetStems`, and axis bounds against
# `pid_norms` itself.

skip_if_not_installed("ggplot2")

# The scales a (version, level) pair plots, derived from the same tables
# `plot_pid5()` reads rather than from the plot -- ground truth for what the
# axis spans and which scales it carries (IP2).
profile_stems <- function(version, level) {
  if (identical(level, "facet")) {
    return(pid_scales[[version]]$camelCase)
  }
  if (identical(version, "BF")) {
    return(pid_scales[["BF"]]$camelCase)
  }
  pid_domains$camelCase
}

# One normed respondent for a version, carrying every scale that version's
# `level` plots. Returns a one-row tibble of `_t` and `_ptl` columns.
normed_one <- function(version, level = "domain", row = 1) {
  n_items <- switch(version, FULL = 220, SF = 100, BF = 25)
  dataset <- switch(
    version,
    FULL = sim_pid5,
    SF = sim_pid5sf,
    BF = sim_pid5bf
  )
  stems <- profile_stems(version, level)
  scored <- score_pid5(dataset[row, ], items = seq_len(n_items), version = version)
  # norm_pid5() reports its own capping when a simulated score falls outside
  # the printed range. That report is not this function's, and letting it
  # escape here would make it indistinguishable from a warning plot_pid5()
  # raised.
  suppressWarnings(
    norm_pid5(scored, scores = paste0("pid_", stems), version = version)
  )
}

# The bounds set on the continuous value axis. Read from the scale rather than
# from the built panel range, which ggplot2 pads by a default 5% expansion --
# the claim under test is which limits the function *asked for*, not how far
# the renderer padded them. Scores are on x; the scale names are the discrete
# y axis.
value_limits <- function(p) {
  ggplot2::layer_scales(p)$x$get_limits()
}

# The built data for one geom, by class name -- indexing layers positionally
# would bake in the assembly order the tests are meant to be independent of.
layer_data_for <- function(p, geom) {
  layers <- ggplot2::ggplot_build(p)$data
  hits <- which(vapply(p$layers, function(L) class(L$geom)[[1]], character(1)) == geom)
  # Stop rather than expect: a bare expectation does not halt, and indexing
  # with 0 or 2 hits then throws an opaque subscript error (or, for 2, silently
  # recursive-indexes into a column) instead of naming what went wrong.
  if (length(hits) != 1) {
    testthat::fail(sprintf("expected exactly 1 %s layer, found %d", geom, length(hits)))
  }
  layers[[hits]]
}

geom_classes <- function(p) {
  vapply(p$layers, function(L) class(L$geom)[[1]], character(1))
}

# A printed scale name back to its canonical stem, through the same tables
# `plot_pid5()` maps the other way. Built layer data carries only what is drawn,
# so the printed name is what comes back off a plot; the stem is recovered
# through the package's own tables (IP2), never through the plot object.
stem_for_label <- function(labels, version, level) {
  table <- if (identical(level, "facet")) {
    stats::setNames(pid_scales[[version]]$camelCase, pid_scales[[version]]$Facet)
  } else if (identical(version, "BF")) {
    stats::setNames(pid_scales[["BF"]]$camelCase, pid_scales[["BF"]]$Domain)
  } else {
    stats::setNames(pid_domains$camelCase, pid_domains$Domain)
  }
  unname(table[labels])
}

# What the plot actually drew, recovered from BUILT layer data alone: one row
# per plotted point -- its printed scale name, its canonical stem, its value and
# its panel -- in DRAWN order, panels top to bottom and the topmost scale first
# within each.
#
# The built point layer carries x, y and PANEL and nothing else. `stem` and
# `scale` are columns of the plot object's internal data frame, which these
# tests deliberately never read: that frame is what the assembly was handed
# rather than what it drew, and asserting over it fails under a rename that
# changes no behavior. The printed name is recovered instead by indexing each
# panel's OWN discrete scale with the point's y position, and the panel's name
# from the built layout.
built_profile <- function(p, version, level) {
  built <- ggplot2::ggplot_build(p)
  pts <- layer_data_for(p, "GeomPoint")
  panel_i <- as.integer(pts$PANEL)

  scale_name <- vapply(seq_along(panel_i), function(k) {
    limits <- built$layout$panel_scales_y[[panel_i[[k]]]]$get_limits()
    as.character(limits[[pts$y[[k]]]])
  }, character(1))

  layout <- built$layout$layout
  panel_name <- if ("panel" %in% names(layout)) {
    as.character(layout$panel)[match(pts$PANEL, layout$PANEL)]
  } else {
    rep(NA_character_, length(panel_i))
  }

  out <- data.frame(
    stem = stem_for_label(scale_name, version, level),
    scale = scale_name,
    value = pts$x,
    panel = panel_name,
    stringsAsFactors = FALSE
  )
  # A discrete axis draws its FIRST level at the bottom, so the topmost scale in
  # a panel is its highest y -- the order a reader sees, which is the order
  # these assertions are about.
  out <- out[order(panel_i, -pts$y), , drop = FALSE]
  row.names(out) <- NULL
  out
}


# ---- the shape of this file (self-checks) ---------------------------------

# Every call in this file, each tagged with whether it sits inside a `for` body.
# Parsed rather than grepped: source text cannot tell a call inside a loop from
# one beside it, and shows argument names only by accident of spelling.
test_file_calls <- function() {
  path <- test_path("test-plot_pid5.R")
  if (!file.exists(path)) {
    skip("test source not available")
  }
  found <- list()
  # An absent argument (`x[, drop = FALSE]`) is held as the empty symbol, and
  # pulling one out of a call with `[[` raises a missing-argument error;
  # as.list() hands it back harmlessly, so the walk descends through that.
  parts <- function(e) {
    kids <- as.list(e)
    empty <- vapply(kids, function(k) is.symbol(k) && !nzchar(as.character(k)), logical(1))
    kids[!empty]
  }
  walk <- function(e, in_loop) {
    if (!is.call(e)) {
      return(invisible(NULL))
    }
    found[[length(found) + 1L]] <<- list(call = e, in_loop = in_loop)
    fn <- if (is.name(e[[1]])) as.character(e[[1]]) else ""
    if (identical(fn, "for") && length(e) == 4L) {
      # The sequence is evaluated once; only the body repeats.
      walk(e[[3]], in_loop)
      walk(e[[4]], TRUE)
      return(invisible(NULL))
    }
    for (k in parts(e)) walk(k, in_loop)
    invisible(NULL)
  }
  for (e in parse(path, keep.source = FALSE)) walk(e, FALSE)
  found
}

test_that("no assertion here reads the plot object's internal data frame", {
  calls <- test_file_calls()
  # The walk has to have seen this file at all, or an unreadable path would
  # make every check below pass by finding nothing.
  expect_gt(length(calls), 500)

  # `p$data` is the plot object's internal frame: what the assembly was handed,
  # not what it drew. It carries columns no aesthetic maps, so a rename that
  # changes no behavior reds these tests -- which is a defect in the test.
  # Built layer data is read instead, and the two are told apart by SHAPE: the
  # forbidden read takes `data` off a bare symbol, the permitted one off a call
  # (`ggplot_build(p)$data`). That needs no tracking of which variable holds
  # what. The cost is a deliberate conservatism -- `b <- ggplot_build(p)`
  # followed by `b$data` is legitimate and still rejected -- so built data is
  # taken from the call itself, or through `layer_data_for()`.
  #
  # `[[` counts as well as `$`. Were it not checked, the rule would be a rule
  # about spelling: `b[["data"]]` reads exactly what `b$data` reads, and a
  # guard that waves it through polices nothing.
  extracts_data <- function(e) {
    if (!(identical(e[[1]], as.name("$")) || identical(e[[1]], as.name("[[")))) {
      return(FALSE)
    }
    length(e) == 3L && is.name(e[[2]]) &&
      (is.character(e[[3]]) || is.name(e[[3]])) &&
      identical(as.character(e[[3]]), "data")
  }
  offenders <- Filter(function(hit) extracts_data(hit$call), calls)
  expect_equal(
    vapply(offenders, function(hit) deparse1(hit$call), character(1)),
    character(0)
  )
})

test_that("every expectation inside a loop names its iteration on failure", {
  in_loop <- Filter(
    function(hit) {
      hit$in_loop && is.name(hit$call[[1]]) &&
        startsWith(as.character(hit$call[[1]]), "expect_")
    },
    test_file_calls()
  )
  # There are in-loop expectations to check, so this cannot pass vacuously.
  expect_gt(length(in_loop), 20)

  # A failure inside a loop that does not say which iteration raised it sends
  # the reader back to run the loop by hand. `expect_setequal()` and
  # `expect_length()` accept no `info`, so they cannot say it at all and are
  # recast rather than excused -- as a sorted `expect_equal()` and an
  # `expect_equal()` on the length. `expect_gt()` and `expect_no_warning()` are
  # recast for the same reason.
  undescribed <- Filter(
    function(hit) {
      fn <- as.character(hit$call[[1]])
      if (fn %in% c("expect_setequal", "expect_length")) {
        return(TRUE)
      }
      !("info" %in% names(hit$call))
    },
    in_loop
  )
  expect_equal(
    vapply(undescribed, function(hit) deparse1(hit$call), character(1)),
    character(0)
  )
})


# ---- label-side axis padding ----------------------------------------------

# Every (version, level, metric) combination `plot_pid5()` accepts, enumerated
# from `pid_scales` rather than hand-listed. Which levels a version offers is
# read off its scales table: the forms that score facets name them in a `Facet`
# column, and the brief form -- which has no facet scores to plot -- names
# domains instead. So the two brief-form facet cases `plot_pid5()` aborts are
# excluded by the same fact that makes it abort, not by naming "BF" here.
profile_cases <- function() {
  out <- list()
  for (version in names(pid_scales)) {
    levels <- if ("Facet" %in% names(pid_scales[[version]])) {
      c("domain", "facet")
    } else {
      "domain"
    }
    for (level in levels) {
      for (metric in c("t", "percentile")) {
        out[[length(out) + 1]] <- list(
          version = version, level = level, metric = metric
        )
      }
    }
  }
  out
}

# The span the published tables print for every scale a combination plots --
# the range the continuous scale is trained on, recomputed from `pid_norms`
# here rather than read back off the plot (IP2). Every scale the combination
# plots counts, including any the respondent has no value for, which is what
# makes the axis independent of the respondent.
published_span <- function(version, level, metric) {
  stems <- profile_stems(version, level)
  rows <- pid_norms[
    pid_norms$version == version & pid_norms$scale %in% stems, , drop = FALSE
  ]
  if (identical(metric, "t")) {
    return(range(rows$tscore, na.rm = TRUE))
  }
  range(rows$percentile, na.rm = TRUE) * 100
}

test_that("the label-side padding is taken only when labels are drawn", {
  cases <- profile_cases()
  # 3 versions x 2 levels x 2 metrics, less the two facet cases the brief form
  # has no facet scores for.
  expect_equal(length(cases), 10L)

  for (case in cases) {
    id <- paste(case$version, case$level, case$metric)
    normed <- normed_one(case$version, level = case$level)
    limits <- published_span(case$version, case$level, case$metric)
    span <- diff(limits)

    for (labels in c(TRUE, FALSE)) {
      p <- plot_pid5(
        normed,
        version = case$version, level = case$level,
        metric = case$metric, labels = labels
      )
      # The padding is the label's room. With labels drawn the label side gets
      # the wider 12%; with no label to hold, both ends get the same 3% the
      # left already had. Compared against the tables' own span, never against
      # the range of the plotted values, which lie inside it.
      expected <- limits + c(-0.03, if (labels) 0.12 else 0.03) * span
      ranges <- lapply(
        ggplot2::ggplot_build(p)$layout$panel_params,
        function(pp) pp$x.range
      )
      # Every panel, not only the first: the value axis is fixed across panels,
      # so a per-panel difference would show up here and nowhere else.
      for (i in seq_along(ranges)) {
        expect_equal(
          ranges[[i]], expected,
          info = paste(id, "labels =", labels, "panel", i)
        )
      }
    }
  }
})


# ---- AC2: domain profiles -------------------------------------------------

test_that("domain profiles plot the five domains in pid_domains order", {
  for (version in c("FULL", "SF")) {
    normed <- normed_one(version)
    p <- plot_pid5(normed, version = version)
    prof <- built_profile(p, version, "domain")

    expect_equal(nrow(prof), 5L, info = version)
    # The claim is the table's order, read off the drawn profile top to bottom.
    expect_equal(prof$stem, pid_domains$camelCase, info = version)

    expected <- vapply(
      paste0("pid_", pid_domains$camelCase, "_t"),
      function(nm) as.numeric(normed[[nm]][[1]]),
      numeric(1)
    )
    # Elementwise and in drawn order, so a build that put one domain's score on
    # another domain's row fails -- which a set comparison would pass.
    expect_equal(prof$value, unname(expected), info = version)
  }
})

test_that("the brief form plots six scales and stops the line before total", {
  normed <- normed_one("BF")
  p <- plot_pid5(normed, version = "BF")
  prof <- built_profile(p, "BF", "domain")

  expect_equal(nrow(prof), 6L)
  # BF order ends on `total` and is deliberately NOT pid_domains order.
  expect_equal(prof$stem, pid_scales[["BF"]]$camelCase)
  expect_equal(tail(prof$stem, 1), "total")
  expect_false(identical(prof$stem, pid_domains$camelCase))

  # The total is an overall elevation, not a sixth domain: the profile line
  # covers the five domains only.
  expect_equal(nrow(layer_data_for(p, "GeomLine")), 5)
})

test_that("full and short domain profiles join all five points", {
  for (version in c("FULL", "SF")) {
    p <- plot_pid5(normed_one(version), version = version)
    expect_equal(nrow(layer_data_for(p, "GeomLine")), 5, info = version)
  }
})


# ---- AC3: facet profiles --------------------------------------------------

test_that("facet profiles plot 25 facets over six panels", {
  for (version in c("FULL", "SF")) {
    normed <- normed_one(version, level = "facet")
    p <- plot_pid5(normed, version = version, level = "facet")
    layout <- ggplot2::ggplot_build(p)$layout$layout

    expect_equal(nrow(built_profile(p, version, "facet")), 25L, info = version)
    expect_equal(nrow(layout), 6L, info = version)
    expect_equal(
      as.character(layout$panel),
      c(pid_domains$Domain, PLOT_UNASSIGNED_PANEL),
      info = version
    )
  }
})

test_that("each panel holds the facets the APA key assigns to it", {
  normed <- normed_one("FULL", level = "facet")
  p <- plot_pid5(normed, version = "FULL", level = "facet")
  prof <- built_profile(p, "FULL", "facet")

  for (i in seq_len(nrow(pid_domains))) {
    domain <- pid_domains$Domain[[i]]
    in_panel <- prof$stem[prof$panel == domain]
    # `expect_setequal()` accepts no `info`, so a failure here could not name
    # its domain. Sorting both sides makes the same claim through
    # `expect_equal()`, which can.
    expect_equal(sort(in_panel), sort(pid_domains$facetStems[[i]]), info = domain)
    expect_equal(length(in_panel), 3L, info = domain)
  }

  # The 10 facets the key ties to no domain land in the sixth panel rather
  # than being dropped or attached to a domain the key does not put them in.
  defining <- unlist(pid_domains$facetStems, use.names = FALSE)
  leftover <- setdiff(pid_scales[["FULL"]]$camelCase, defining)
  expect_length(leftover, 10)
  expect_setequal(prof$stem[prof$panel == PLOT_UNASSIGNED_PANEL], leftover)
})

test_that("the brief form refuses a facet profile", {
  expect_error(
    plot_pid5(normed_one("BF"), version = "BF", level = "facet"),
    regexp = "no facet scores"
  )
})


# ---- AC4: percentile metric -----------------------------------------------

test_that("the percentile metric plots proportions rescaled to 0-100", {
  normed <- normed_one("FULL")
  p <- plot_pid5(normed, version = "FULL", metric = "percentile")
  prof <- built_profile(p, "FULL", "domain")

  expected <- vapply(
    paste0("pid_", pid_domains$camelCase, "_ptl"),
    function(nm) as.numeric(normed[[nm]][[1]]) * 100,
    numeric(1)
  )
  expect_equal(prof$value, unname(expected))
  expect_true(all(prof$value >= 0 & prof$value <= 100))

  # norm_pid5() returns a proportion; the rescaling is a factor of exactly 100.
  raw <- vapply(
    paste0("pid_", pid_domains$camelCase, "_ptl"),
    function(nm) as.numeric(normed[[nm]][[1]]),
    numeric(1)
  )
  expect_equal(prof$value / 100, unname(raw))

  expect_equal(value_limits(p), c(0, 100))
  expect_equal(layer_data_for(p, "GeomVline")$xintercept, 50)
})


# ---- AC5: no interpretive furniture ---------------------------------------

test_that("the plot carries no bands, thresholds, or extra annotation", {
  cases <- list(
    list(version = "FULL", level = "domain", metric = "t"),
    list(version = "FULL", level = "facet", metric = "t"),
    list(version = "SF", level = "facet", metric = "percentile"),
    list(version = "BF", level = "domain", metric = "percentile")
  )
  for (case in cases) {
    id <- paste(case$version, case$level, case$metric)
    normed <- normed_one(case$version, level = case$level)
    p <- plot_pid5(
      normed,
      version = case$version,
      level = case$level,
      metric = case$metric
    )
    geoms <- geom_classes(p)

    # A severity band would be a rectangle layer. There is none, on any
    # combination -- the plot presents scores and characterizes nothing (IP4).
    expect_false(
      any(c("GeomRect", "GeomTile", "GeomRibbon", "GeomArea") %in% geoms),
      info = id
    )

    # Exactly one reference line, and it carries no text label of its own:
    # a labelled reference line would be a second text layer.
    expect_equal(sum(geoms %in% c("GeomHline", "GeomVline")), 1L, info = id)
    expect_equal(sum(geoms %in% c("GeomText", "GeomLabel")), 1L, info = id)

    # The one text layer says nothing but the plotted values -- elementwise,
    # so a build that put one scale's label on another scale's point fails.
    lab <- layer_data_for(p, "GeomLabel")
    pts <- layer_data_for(p, "GeomPoint")
    expect_equal(
      as.character(lab$label[order(lab$PANEL, lab$y)]),
      as.character(round(pts$x[order(pts$PANEL, pts$y)])),
      info = id
    )
  }
})

test_that("axis bounds come from pid_norms rather than a chosen constant", {
  for (version in c("FULL", "SF", "BF")) {
    normed <- normed_one(version)
    p <- plot_pid5(normed, version = version)
    stems <- built_profile(p, version, "domain")$stem

    rows <- pid_norms[
      pid_norms$version == version & pid_norms$scale %in% stems,
      ,
      drop = FALSE
    ]
    expect_equal(value_limits(p), range(rows$tscore, na.rm = TRUE), info = version)
  }

  # The T reference line is the metric's own midpoint, not a cut score.
  p <- plot_pid5(normed_one("FULL"), version = "FULL")
  expect_equal(layer_data_for(p, "GeomVline")$xintercept, 50)
})


# ---- AC6: input branches --------------------------------------------------

test_that("more than one respondent is refused", {
  scored <- score_pid5(sim_pid5bf[1:3, ], items = 1:25, version = "BF")
  normed <- norm_pid5(
    scored,
    scores = paste0("pid_", pid_scales[["BF"]]$camelCase),
    version = "BF"
  )
  expect_error(plot_pid5(normed, version = "BF"), regexp = "exactly one row")
  # The message names the count the caller actually supplied.
  expect_error(plot_pid5(normed, version = "BF"), regexp = "3 rows")
  # Zero rows is refused by the same branch, not silently plotted empty.
  expect_error(plot_pid5(normed[0, ], version = "BF"), regexp = "exactly one row")
})

test_that("un-normed data is an error, not a warning", {
  scored <- score_pid5(sim_pid5bf[1, ], items = 1:25, version = "BF")
  expect_error(plot_pid5(scored, version = "BF"), regexp = "missing the normed columns")
  # A frame normed at domain level cannot be plotted at facet level.
  expect_error(
    plot_pid5(normed_one("FULL"), version = "FULL", level = "facet"),
    regexp = "missing the normed columns"
  )
})

test_that("a scale with no value is dropped with a warning and the rest plot", {
  normed <- normed_one("FULL")
  normed$pid_detachment_t[[1]] <- NA_integer_

  expect_warning(
    p <- plot_pid5(normed, version = "FULL"),
    regexp = "detachment"
  )
  expect_equal(nrow(layer_data_for(p, "GeomPoint")), 4)
  expect_false("detachment" %in% built_profile(p, "FULL", "domain")$stem)
})

test_that("a profile with no values at all is an error", {
  normed <- normed_one("FULL")
  for (nm in paste0("pid_", pid_domains$camelCase, "_t")) {
    normed[[nm]][[1]] <- NA_integer_
  }
  expect_error(
    suppressWarnings(plot_pid5(normed, version = "FULL")),
    regexp = "No scale has a value"
  )
})

test_that("ggplot2 being unavailable is reported, not hit as a namespace error", {
  # rlang::is_installed() is the binding calc_omega() guards on and the one
  # this function uses; check_installed() never consults it, so mocking it
  # there would be a test that passes against a guard that does not exist.
  local_mocked_bindings(is_installed = function(...) FALSE, .package = "rlang")
  expect_error(
    plot_pid5(normed_one("BF"), version = "BF"),
    regexp = "ggplot2"
  )
})


# ---- argument handling ----------------------------------------------------

test_that("version is matched case-insensitively and bad arguments abort", {
  normed <- normed_one("BF")
  expect_no_error(plot_pid5(normed, version = "bf"))
  expect_error(plot_pid5(normed, version = "XX"))
  expect_error(plot_pid5(normed, version = "BF", level = "domains"))
  expect_error(plot_pid5(normed, version = "BF", metric = "tscore"))
  expect_error(plot_pid5(as.list(normed), version = "BF"), regexp = "data frame")
  expect_error(plot_pid5(normed, version = "BF", prefix = 1), regexp = "prefix")
})

test_that("a non-default prefix is pasted onto the scale stems", {
  scored <- score_pid5(sim_pid5bf[1, ], items = 1:25, version = "BF", prefix = "p5_")
  normed <- norm_pid5(
    scored,
    scores = paste0("p5_", pid_scales[["BF"]]$camelCase),
    version = "BF",
    prefix = "p5_"
  )
  p <- plot_pid5(normed, version = "BF", prefix = "p5_")
  expect_equal(nrow(layer_data_for(p, "GeomPoint")), 6)
  # The default prefix finds nothing in that frame.
  expect_error(plot_pid5(normed, version = "BF"), regexp = "missing the normed columns")
})


# ---- drawn order ----------------------------------------------------------

test_that("scales are drawn top-to-bottom in their table's order", {
  # `built_profile()` returns the drawn order -- a discrete y scale draws its
  # first level at the bottom, so the drawn order and the factor's level order
  # run opposite ways, and only one of them is what a reader sees.
  expect_equal(
    built_profile(plot_pid5(normed_one("FULL"), version = "FULL"), "FULL", "domain")$stem,
    pid_domains$camelCase
  )
  expect_equal(
    built_profile(plot_pid5(normed_one("BF"), version = "BF"), "BF", "domain")$stem,
    pid_scales[["BF"]]$camelCase
  )

  # Facet panels run top-down in domain order, and within each panel the
  # scales run top-down in the same table order.
  p <- plot_pid5(normed_one("FULL", level = "facet"), version = "FULL", level = "facet")
  prof <- built_profile(p, "FULL", "facet")
  expect_equal(unique(prof$panel), c(pid_domains$Domain, PLOT_UNASSIGNED_PANEL))
  for (panel in unique(prof$panel)) {
    drawn <- prof$stem[prof$panel == panel]
    expect_equal(
      drawn,
      pid_scales[["FULL"]]$camelCase[pid_scales[["FULL"]]$camelCase %in% drawn],
      info = panel
    )
  }
})

test_that("axis labels are the tables' printed names, not column stems", {
  p <- plot_pid5(normed_one("FULL"), version = "FULL")
  # The names as drawn, top to bottom.
  drawn <- built_profile(p, "FULL", "domain")$scale
  expect_equal(drawn, pid_domains$Domain)
  expect_false(any(drawn %in% pid_domains$camelCase))

  p <- plot_pid5(normed_one("FULL", level = "facet"), version = "FULL", level = "facet")
  expect_setequal(built_profile(p, "FULL", "facet")$scale, pid_scales[["FULL"]]$Facet)
})

test_that("a non-numeric normed column is refused, not reported as missing", {
  # A character column coerces to NA, which the NA-drop branch would report as
  # "no value" -- hiding a type mistake behind a missing-data warning.
  normed <- normed_one("BF")
  normed$pid_detachment_t <- as.character(normed$pid_detachment_t)
  expect_error(plot_pid5(normed, version = "BF"), regexp = "must be numeric")

  # A factor's integer codes are not its scores either.
  normed <- normed_one("BF")
  normed$pid_detachment_t <- factor(normed$pid_detachment_t)
  expect_error(plot_pid5(normed, version = "BF"), regexp = "must be numeric")
})


# ---- per-panel axis content (regression guard) -----------------------------

test_that("each facet panel draws only its own scales on the axis", {
  # A pinned `scale_y_discrete(limits =)` overrides per-panel scale training,
  # which silently disables facet_grid's `free_y`/`space = "free_y"` and draws
  # ALL 25 facet names in EVERY panel, at equal panel heights. Every structural
  # assertion above stays green through that, and so does CI including pkgdown
  # -- only the axis content shows it. This is the guard for that regression.
  for (version in c("FULL", "SF")) {
    p <- plot_pid5(
      normed_one(version, level = "facet"),
      version = version,
      level = "facet"
    )
    b <- ggplot2::ggplot_build(p)
    prof <- built_profile(p, version, "facet")
    per_panel <- vapply(
      b$layout$panel_scales_y,
      function(sc) length(sc$get_limits()),
      integer(1)
    )
    # Five domain panels of three defining facets, then the ten the key
    # assigns to no domain.
    expect_equal(per_panel, c(3L, 3L, 3L, 3L, 3L, 10L), info = version)
    expect_equal(sum(per_panel), 25L, info = version)

    # And each panel's axis lists exactly the scales in that panel.
    for (i in seq_along(b$layout$panel_scales_y)) {
      panel_name <- as.character(b$layout$layout$panel[[i]])
      expect_equal(
        sort(b$layout$panel_scales_y[[i]]$get_limits()),
        sort(prof$scale[prof$panel == panel_name]),
        info = paste(version, panel_name)
      )
    }
  }
})

test_that("the value label is offset horizontally, not into the panel height", {
  # A rendering offset is an absolute distance; room reserved for it on the
  # DISCRETE axis is measured in data units whose physical size shrinks with
  # the panel. So a `vjust` offset is clipped by the panel edge on a small
  # enough device however much discrete expansion is added -- which an earlier
  # fix here did, and which layer data cannot see. The offset belongs on the
  # continuous axis, where padding is the room the label needs. This is the
  # guard for that direction.
  for (version in c("FULL", "SF", "BF")) {
    level <- if (identical(version, "BF")) "domain" else "facet"
    p <- plot_pid5(normed_one(version, level = level), version = version, level = level)
    i <- which(vapply(p$layers, function(l) inherits(l$geom, "GeomLabel"), TRUE))
    # `expect_length()` takes no `info`, so a failure inside this loop could not
    # name its version; `expect_equal()` on the length can.
    expect_equal(length(i), 1L, info = version)
    expect_true("hjust" %in% names(p$layers[[i]]$aes_params), info = version)
    expect_false("vjust" %in% names(p$layers[[i]]$aes_params), info = version)

    # And the offset is a RENDERING one, never a nudge of the x value: nudging
    # in data space pushes a score near the end of the axis past the limit and
    # ggplot2 drops that label silently. The layer object cannot show this --
    # `geom_label()` carries a `PositionNudge` with a NULL `$x` whether or not
    # `nudge_x` was given, so asserting over `$position` is vacuous. Built data
    # can: a nudge moves the label's x off its point's.
    expect_equal(
      layer_data_for(p, "GeomLabel")$x,
      layer_data_for(p, "GeomPoint")$x,
      info = version
    )

    # And no room is taken out of the panel height: the discrete axis carries
    # ggplot2's default expansion of 0.6 and nothing wider.
    headroom <- vapply(
      ggplot2::ggplot_build(p)$layout$panel_params,
      function(pp) pp$y.range[[2]] - length(pp$y$get_limits()),
      numeric(1)
    )
    expect_equal(headroom, rep(0.6, length(headroom)), info = version)
  }
})

test_that("a value at the end of the axis still gets its label drawn", {
  # The hazard the horizontal offset has to clear: a score at the top of the
  # published span must not push its label past the axis limit, where ggplot2
  # drops it with only a draw-time warning that ggplot_build() never raises.
  # The continuous axis is padded on the label side for exactly this case.
  for (metric in c("t", "percentile")) {
    normed <- normed_one("FULL", level = "domain")
    suffix <- if (identical(metric, "t")) "_t" else "_ptl"
    cols <- grep(paste0(suffix, "$"), names(normed), value = TRUE)
    # Pin every scale to the largest value the tables print FOR THE SCALES THIS
    # PLOT DRAWS -- that is the axis maximum, recomputed from `pid_norms` here
    # rather than read back off the plot (IP2).
    stems <- sub("^pid_", "", sub(paste0(suffix, "$"), "", cols))
    rows <- pid_norms[
      pid_norms$version == "FULL" & pid_norms$scale %in% stems, , drop = FALSE
    ]
    top <- max(rows[[if (identical(metric, "t")) "tscore" else "percentile"]], na.rm = TRUE)
    normed[cols] <- top
    p <- plot_pid5(normed, version = "FULL", metric = metric)

    lab <- layer_data_for(p, "GeomLabel")
    expect_equal(nrow(lab), length(cols), info = metric)
    # And drawing it emits nothing -- the dropped-label warning fires at draw
    # time, not at build time. `expect_no_warning()` accepts no `info`, so the
    # warnings are captured and compared instead, which both names the metric
    # and shows what was raised.
    expect_equal(
      testthat::capture_warnings(ggplot2::ggplot_gtable(ggplot2::ggplot_build(p))),
      character(0),
      info = metric
    )
    # The facet branch is the one that clipped through two earlier attempts, so
    # the padding is checked there too and not only on a single-panel plot.
    p_facet <- plot_pid5(normed_one("FULL", level = "facet"), version = "FULL",
                         level = "facet", metric = metric)
    facet_params <- ggplot2::ggplot_build(p_facet)$layout$panel_params
    for (j in seq_along(facet_params)) {
      # `expect_gt()` takes a `label` but no `info`; `expect_true()` on the same
      # comparison names both the metric and the panel.
      expect_true(
        facet_params[[j]]$x.range[[2]] - max(value_limits(p_facet)) >
          0.05 * diff(value_limits(p_facet)),
        info = paste(metric, "panel", j)
      )
    }

    # The panel reserves room past the axis limit on the label side. This
    # asserts the room EXISTS, not that a given label fits in it: whether a
    # label fits is a grob measurement against a device size, and no build-time
    # value carries it. AC8's rendered sweep is what checks the fit. Without
    # this the labels at the top of the span render clipped ("90" drawn as "9")
    # and every other assertion here stays green.
    limits <- value_limits(p)
    x_range <- ggplot2::ggplot_build(p)$layout$panel_params[[1]]$x.range
    default_pad <- 0.05 * diff(limits)
    expect_true(x_range[[2]] - limits[[2]] > default_pad, info = metric)
  }
})

test_that("labels = FALSE drops the value labels and leaves the rest alone", {
  for (version in c("FULL", "BF")) {
    level <- if (identical(version, "BF")) "domain" else "facet"
    normed <- normed_one(version, level = level)
    on <- plot_pid5(normed, version = version, level = level)
    off <- plot_pid5(normed, version = version, level = level, labels = FALSE)

    geoms <- function(p) vapply(p$layers, function(l) class(l$geom)[1], character(1))
    expect_true("GeomLabel" %in% geoms(on), info = version)
    expect_false("GeomLabel" %in% geoms(off), info = version)
    # Everything else is untouched: same layers, in the same order.
    expect_equal(unname(setdiff(geoms(on), "GeomLabel")), unname(geoms(off)), info = version)

    # And the points and profile line still carry the same data. Looked up by
    # geom rather than by layer position, which would bake in the assembly
    # order this test is meant to be independent of.
    expect_equal(
      layer_data_for(off, "GeomPoint")$x,
      layer_data_for(on, "GeomPoint")$x,
      info = version
    )
    expect_equal(
      layer_data_for(off, "GeomLine")$x,
      layer_data_for(on, "GeomLine")$x,
      info = version
    )
  }
})

test_that("labels must be TRUE or FALSE", {
  normed <- normed_one("BF")
  expect_error(plot_pid5(normed, version = "BF", labels = "yes"), "must be")
  expect_error(plot_pid5(normed, version = "BF", labels = NA), "must be")
  expect_error(plot_pid5(normed, version = "BF", labels = c(TRUE, TRUE)), "must be")
})

test_that("the unfacetted profile pins its scale order against layer training", {
  # The brief form's profile-line layer omits `total`, and a discrete scale
  # trained across layers puts a value missing from the first layer LAST --
  # which drew `total` at the top. The pin is what prevents that, and it is
  # safe here only because there is a single panel to train.
  p <- plot_pid5(normed_one("BF"), version = "BF")
  expect_equal(
    ggplot2::layer_scales(p)$y$get_limits(),
    rev(pid_scales[["BF"]]$Domain)
  )
})

test_that("the axis does not depend on which scales survived the NA drop", {
  # The documented guarantee is that two profiles on the same version and
  # level share an axis. If the axis were computed from the surviving stems,
  # dropping a scale could move it.
  full <- normed_one("FULL")
  holed <- full
  holed$pid_detachment_t[[1]] <- NA_integer_

  p_full <- plot_pid5(full, version = "FULL")
  p_holed <- suppressWarnings(plot_pid5(holed, version = "FULL"))
  expect_equal(value_limits(p_holed), value_limits(p_full))
})

test_that("axis breaks step across the published span, not fixed positions", {
  p <- plot_pid5(normed_one("FULL"), version = "FULL")
  breaks <- ggplot2::layer_scales(p)$x$get_breaks()
  breaks <- breaks[!is.na(breaks)]
  lim <- value_limits(p)
  expect_true(all(breaks %% 10 == 0))
  expect_true(all(breaks >= lim[[1]] & breaks <= lim[[2]]))

  p <- plot_pid5(normed_one("FULL"), version = "FULL", metric = "percentile")
  breaks <- ggplot2::layer_scales(p)$x$get_breaks()
  breaks <- breaks[!is.na(breaks)]
  expect_true(all(breaks %% 25 == 0))
  expect_true(all(breaks >= 0 & breaks <= 100))
})

test_that("axis_breaks falls back to the span when it holds no multiple", {
  # seq() would run backwards and error here; the guard returns the endpoints.
  expect_equal(axis_breaks(c(52, 58), step = 10), c(52, 58))
  expect_equal(axis_breaks(c(30, 90), step = 10), seq(30, 90, by = 10))
})

test_that("a scale absent from pid_norms is refused by the axis helper", {
  expect_error(
    plot_pid5_axis("nope", "FULL", "t"),
    regexp = "no rows for"
  )
})

test_that("every point keeps a visible marker and an undropped label", {
  # Two failure modes, one test. (a) The label must not sit exactly on the
  # point, or it hides the marker entirely. (b) The offset must not move the
  # label in DATA space: a percentile of 98 nudged outward lands past the
  # axis limit and ggplot2 drops that label silently, with only a warning.
  for (case in list(
    list(v = "BF", m = "percentile"), list(v = "BF", m = "t"),
    list(v = "FULL", m = "t"), list(v = "FULL", m = "percentile")
  )) {
    p <- plot_pid5(normed_one(case$v), version = case$v, metric = case$m)
    lab <- layer_data_for(p, "GeomLabel")
    pts <- layer_data_for(p, "GeomPoint")

    # No label is dropped for falling outside the scale range.
    expect_false(any(is.na(lab$x)), info = paste(case$v, case$m))
    expect_equal(nrow(lab), nrow(pts), info = paste(case$v, case$m))
    expect_true(
      all(lab$x >= min(value_limits(p)) & lab$x <= max(value_limits(p))),
      info = paste(case$v, case$m)
    )

    # The label shares the point's data position; separation is a rendering
    # property (hjust), so it can never push a value off the axis.
    expect_equal(lab$x, pts$x, info = paste(case$v, case$m))
    expect_true(all(lab$hjust != 0.5), info = paste(case$v, case$m))
  }
})

test_that("drawing a plot emits no ggplot2 warnings", {
  # A dropped label or an out-of-range value surfaces only at draw time --
  # ggplot_build() alone does not raise it.
  for (lvl in c("domain", "facet")) {
    for (m in c("t", "percentile")) {
      p <- plot_pid5(
        normed_one("FULL", level = lvl),
        version = "FULL", level = lvl, metric = m
      )
      f <- withr::local_tempfile(fileext = ".png")
      # `expect_no_warning()` accepts no `info`, so the warnings are captured
      # and compared instead -- which names the iteration and shows what was
      # raised rather than only that something was.
      expect_equal(
        testthat::capture_warnings(
          suppressMessages(
            ggplot2::ggsave(f, p, width = 7, height = 7, dpi = 72)
          )
        ),
        character(0),
        info = paste(lvl, m)
      )
    }
  }
})

test_that("the ggplot2 floor in DESCRIPTION matches the one the guard enforces", {
  # D-031 states the floor twice by necessity: DESCRIPTION is what installers
  # and R CMD check read, the guard argument is what actually fires for a user
  # whose ggplot2 is too old. They must move together.
  # R/ is not shipped in an installed package, so this can only run against a
  # source checkout; it skips rather than errors under R CMD check.
  src_path <- test_path("../../R/plot_pid5.R")
  skip_if_not(file.exists(src_path), "source checkout not available")
  declared <- read.dcf(system.file("DESCRIPTION", package = "hitop"), fields = "Suggests")[[1]]
  skip_if(is.na(declared))
  floor_declared <- sub(".*ggplot2 \\(>= ([0-9.]+)\\).*", "\\1", gsub("\n", " ", declared))
  src <- readLines(src_path, warn = FALSE)
  guard <- grep('is_installed\\("ggplot2"', src, value = TRUE)
  expect_length(guard, 1)
  floor_guard <- sub('.*version = "([0-9.]+)".*', "\\1", guard)
  expect_equal(floor_guard, floor_declared)
})
