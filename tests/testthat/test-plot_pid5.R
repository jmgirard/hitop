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
  stems <- if (identical(level, "facet")) {
    pid_scales[[version]]$camelCase
  } else if (identical(version, "BF")) {
    pid_scales[["BF"]]$camelCase
  } else {
    pid_domains$camelCase
  }
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
  b <- ggplot2::ggplot_build(p)
  hits <- which(vapply(p$layers, function(L) class(L$geom)[[1]], character(1)) == geom)
  # Stop rather than expect: a bare expectation does not halt, and indexing
  # with 0 or 2 hits then throws an opaque subscript error (or, for 2, silently
  # recursive-indexes into a column) instead of naming what went wrong.
  if (length(hits) != 1) {
    testthat::fail(sprintf("expected exactly 1 %s layer, found %d", geom, length(hits)))
  }
  b$data[[hits]]
}

geom_classes <- function(p) {
  vapply(p$layers, function(L) class(L$geom)[[1]], character(1))
}


# ---- AC2: domain profiles -------------------------------------------------

test_that("domain profiles plot the five domains in pid_domains order", {
  for (version in c("FULL", "SF")) {
    normed <- normed_one(version)
    p <- plot_pid5(normed, version = version)
    pts <- layer_data_for(p, "GeomPoint")

    expect_equal(nrow(pts), 5)
    # The claim is the table's order, asserted on the canonical stems in the
    # order the plot's data carries them; the drawn top-to-bottom order has
    # its own test below.
    expect_equal(p$data$stem, pid_domains$camelCase)

    expected <- vapply(
      paste0("pid_", pid_domains$camelCase, "_t"),
      function(nm) as.numeric(normed[[nm]][[1]]),
      numeric(1)
    )
    expect_equal(p$data$value, unname(expected))
    expect_setequal(pts$x, unname(expected))
  }
})

test_that("the brief form plots six scales and stops the line before total", {
  normed <- normed_one("BF")
  p <- plot_pid5(normed, version = "BF")

  expect_equal(nrow(layer_data_for(p, "GeomPoint")), 6)
  # BF order ends on `total` and is deliberately NOT pid_domains order.
  expect_equal(p$data$stem, pid_scales[["BF"]]$camelCase)
  expect_equal(tail(p$data$stem, 1), "total")
  expect_false(identical(p$data$stem, pid_domains$camelCase))

  # The total is an overall elevation, not a sixth domain: the profile line
  # covers the five domains only.
  expect_equal(nrow(layer_data_for(p, "GeomLine")), 5)
})

test_that("full and short domain profiles join all five points", {
  for (version in c("FULL", "SF")) {
    p <- plot_pid5(normed_one(version), version = version)
    expect_equal(nrow(layer_data_for(p, "GeomLine")), 5)
  }
})


# ---- AC3: facet profiles --------------------------------------------------

test_that("facet profiles plot 25 facets over six panels", {
  for (version in c("FULL", "SF")) {
    normed <- normed_one(version, level = "facet")
    p <- plot_pid5(normed, version = version, level = "facet")
    b <- ggplot2::ggplot_build(p)

    expect_equal(nrow(layer_data_for(p, "GeomPoint")), 25)
    expect_equal(nrow(b$layout$layout), 6)
    expect_equal(
      as.character(b$layout$layout$panel),
      c(pid_domains$Domain, PLOT_UNASSIGNED_PANEL)
    )
  }
})

test_that("each panel holds the facets the APA key assigns to it", {
  normed <- normed_one("FULL", level = "facet")
  p <- plot_pid5(normed, version = "FULL", level = "facet")

  for (i in seq_len(nrow(pid_domains))) {
    domain <- pid_domains$Domain[[i]]
    in_panel <- as.character(p$data$stem[p$data$panel == domain])
    expect_setequal(in_panel, pid_domains$facetStems[[i]])
    expect_length(in_panel, 3)
  }

  # The 10 facets the key ties to no domain land in the sixth panel rather
  # than being dropped or attached to a domain the key does not put them in.
  defining <- unlist(pid_domains$facetStems, use.names = FALSE)
  leftover <- setdiff(pid_scales[["FULL"]]$camelCase, defining)
  expect_length(leftover, 10)
  expect_setequal(
    as.character(p$data$stem[p$data$panel == PLOT_UNASSIGNED_PANEL]),
    leftover
  )
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

  expected <- vapply(
    paste0("pid_", pid_domains$camelCase, "_ptl"),
    function(nm) as.numeric(normed[[nm]][[1]]) * 100,
    numeric(1)
  )
  expect_equal(p$data$value, unname(expected))
  expect_true(all(p$data$value >= 0 & p$data$value <= 100))

  # norm_pid5() returns a proportion; the rescaling is a factor of exactly 100.
  raw <- vapply(
    paste0("pid_", pid_domains$camelCase, "_ptl"),
    function(nm) as.numeric(normed[[nm]][[1]]),
    numeric(1)
  )
  expect_equal(p$data$value / 100, unname(raw))

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
    expect_false(any(c("GeomRect", "GeomTile", "GeomRibbon", "GeomArea") %in% geoms))

    # Exactly one reference line, and it carries no text label of its own:
    # a labelled reference line would be a second text layer.
    expect_equal(sum(geoms %in% c("GeomHline", "GeomVline")), 1)
    expect_equal(sum(geoms %in% c("GeomText", "GeomLabel")), 1)

    # The one text layer says nothing but the plotted values -- elementwise,
    # so a build that put one scale's label on another scale's point fails.
    lab <- layer_data_for(p, "GeomLabel")
    pts <- layer_data_for(p, "GeomPoint")
    expect_equal(
      as.character(lab$label[order(lab$PANEL, lab$y)]),
      as.character(round(pts$x[order(pts$PANEL, pts$y)]))
    )
  }
})

test_that("axis bounds come from pid_norms rather than a chosen constant", {
  for (version in c("FULL", "SF", "BF")) {
    normed <- normed_one(version)
    p <- plot_pid5(normed, version = version)
    stems <- p$data$stem

    rows <- pid_norms[
      pid_norms$version == version & pid_norms$scale %in% stems,
      ,
      drop = FALSE
    ]
    expect_equal(value_limits(p), range(rows$tscore, na.rm = TRUE))
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
  expect_false("detachment" %in% p$data$stem)
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
  # A discrete y scale draws its first level at the bottom, so this asserts
  # the built y coordinates rather than the factor's level order -- the two
  # run opposite ways and only one of them is what a reader sees.
  drawn_top_down <- function(p) {
    pts <- layer_data_for(p, "GeomPoint")
    as.character(p$data$stem[order(-pts$y)])
  }

  expect_equal(
    drawn_top_down(plot_pid5(normed_one("FULL"), version = "FULL")),
    pid_domains$camelCase
  )
  expect_equal(
    drawn_top_down(plot_pid5(normed_one("BF"), version = "BF")),
    pid_scales[["BF"]]$camelCase
  )

  # Facet panels run top-down in domain order, and within each panel the
  # scales run top-down in the same table order. The sort is done per panel
  # because only within-panel relative order is the claim; built y values are
  # in fact global, so a per-panel sort is the narrower and safer assertion.
  p <- plot_pid5(normed_one("FULL", level = "facet"), version = "FULL", level = "facet")
  pts <- layer_data_for(p, "GeomPoint")
  for (panel in levels(p$data$panel)) {
    keep <- p$data$panel == panel
    drawn <- as.character(p$data$stem[keep][order(-pts$y[keep])])
    expect_equal(drawn, pid_scales[["FULL"]]$camelCase[pid_scales[["FULL"]]$camelCase %in% drawn])
  }
})

test_that("axis labels are the tables' printed names, not column stems", {
  p <- plot_pid5(normed_one("FULL"), version = "FULL")
  expect_equal(levels(p$data$scale), rev(pid_domains$Domain))
  expect_false(any(levels(p$data$scale) %in% pid_domains$camelCase))

  p <- plot_pid5(normed_one("FULL", level = "facet"), version = "FULL", level = "facet")
  expect_setequal(as.character(p$data$scale), pid_scales[["FULL"]]$Facet)
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
      expect_setequal(
        b$layout$panel_scales_y[[i]]$get_limits(),
        as.character(p$data$scale[p$data$panel == panel_name])
      )
    }
  }
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
    expect_true(all(lab$x >= min(value_limits(p)) & lab$x <= max(value_limits(p))))

    # The label shares the point's data position; separation is a rendering
    # property (vjust), so it can never push a value off the axis.
    expect_equal(lab$x, pts$x, info = paste(case$v, case$m))
    expect_true(all(lab$vjust != 0.5))
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
      expect_no_warning(
        suppressMessages(
          ggplot2::ggsave(f, p, width = 7, height = 7, dpi = 72)
        )
      )
    }
  }
})
