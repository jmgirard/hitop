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
# the renderer padded them. The value scale is `y` as declared; coord_flip()
# swaps it only at draw time.
value_limits <- function(p) {
  ggplot2::layer_scales(p)$y$get_limits()
}

# The built data for one geom, by class name -- indexing layers positionally
# would bake in the assembly order the tests are meant to be independent of.
layer_data_for <- function(p, geom) {
  b <- ggplot2::ggplot_build(p)
  hits <- which(vapply(p$layers, function(L) class(L$geom)[[1]], character(1)) == geom)
  expect_length(hits, 1)
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
    # Order is the table's, not the plot's own: coord_flip() reverses the
    # drawn order, so the factor levels carry the claim.
    expect_equal(levels(p$data$scale), pid_domains$camelCase)

    expected <- vapply(
      paste0("pid_", pid_domains$camelCase, "_t"),
      function(nm) as.numeric(normed[[nm]][[1]]),
      numeric(1)
    )
    expect_equal(p$data$value, unname(expected))
    expect_setequal(pts$y, unname(expected))
  }
})

test_that("the brief form plots six scales and stops the line before total", {
  normed <- normed_one("BF")
  p <- plot_pid5(normed, version = "BF")

  expect_equal(nrow(layer_data_for(p, "GeomPoint")), 6)
  # BF order ends on `total` and is deliberately NOT pid_domains order.
  expect_equal(levels(p$data$scale), pid_scales[["BF"]]$camelCase)
  expect_equal(tail(levels(p$data$scale), 1), "total")
  expect_false(identical(levels(p$data$scale), pid_domains$camelCase))

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
      c(pid_domains$Domain, "Not domain-defining")
    )
  }
})

test_that("each panel holds the facets the APA key assigns to it", {
  normed <- normed_one("FULL", level = "facet")
  p <- plot_pid5(normed, version = "FULL", level = "facet")

  for (i in seq_len(nrow(pid_domains))) {
    domain <- pid_domains$Domain[[i]]
    in_panel <- as.character(p$data$scale[p$data$panel == domain])
    expect_setequal(in_panel, pid_domains$facetStems[[i]])
    expect_length(in_panel, 3)
  }

  # The 10 facets the key ties to no domain land in the sixth panel rather
  # than being dropped or attached to a domain the key does not put them in.
  defining <- unlist(pid_domains$facetStems, use.names = FALSE)
  leftover <- setdiff(pid_scales[["FULL"]]$camelCase, defining)
  expect_length(leftover, 10)
  expect_setequal(
    as.character(p$data$scale[p$data$panel == "Not domain-defining"]),
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
  expect_equal(layer_data_for(p, "GeomHline")$yintercept, 50)
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
    expect_equal(sum(geoms == "GeomHline"), 1)
    expect_equal(sum(geoms %in% c("GeomText", "GeomLabel")), 1)

    # The one text layer says nothing but the plotted values.
    labels <- layer_data_for(p, "GeomLabel")$label
    expect_setequal(as.character(labels), as.character(round(p$data$value)))
  }
})

test_that("axis bounds come from pid_norms rather than a chosen constant", {
  for (version in c("FULL", "SF", "BF")) {
    normed <- normed_one(version)
    p <- plot_pid5(normed, version = version)
    stems <- levels(p$data$scale)

    rows <- pid_norms[
      pid_norms$version == version & pid_norms$scale %in% stems,
      ,
      drop = FALSE
    ]
    expect_equal(value_limits(p), range(rows$tscore, na.rm = TRUE))
  }

  # The T reference line is the metric's own midpoint, not a cut score.
  p <- plot_pid5(normed_one("FULL"), version = "FULL")
  expect_equal(layer_data_for(p, "GeomHline")$yintercept, 50)
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
  expect_false("detachment" %in% levels(p$data$scale))
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
