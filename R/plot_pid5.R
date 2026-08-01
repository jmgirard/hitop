#' Plot a norm-referenced PID-5 profile
#'
#' Renders one respondent's normed PID-5 scores as a profile against the
#' published normative tables in [pid_norms]. The plot presents scores against
#' norms and characterizes none of them: it carries no severity bands, no
#' elevation thresholds, and no annotation about what a score means. Judging a
#' profile is the clinician's job, not this package's.
#'
#' @param data A data frame with exactly one row, carrying the `_t` and/or
#'   `_ptl` columns produced by [norm_pid5()]. More than one row is an error --
#'   a profile plot shows one respondent.
#' @param version Which PID-5 version the scores came from: `"FULL"` (220
#'   items), `"SF"` (100 items), or `"BF"` (25 items). Matched case-insensitively.
#' @param level Which scales to plot. `"domain"` plots the five personality
#'   domains, plus the brief form's total. `"facet"` plots the 25 facets grouped
#'   into a panel per domain, and is available for `"FULL"` and `"SF"` only --
#'   the brief form has no facet scores.
#' @param metric Which normed metric to plot: `"t"` for T scores, or
#'   `"percentile"` for percentile ranks. [norm_pid5()] returns percentiles as a
#'   proportion; this function multiplies them by 100 so the axis reads on the
#'   familiar 0-100 percentile scale.
#' @param prefix The column-name prefix used when the scores were computed, as
#'   passed to [score_pid5()] and [norm_pid5()]. Pasted onto each scale's
#'   camelCase name to find its column.
#'
#' @details
#' ## What the plot draws
#'
#' Each plotted scale gets a point at its normed value, labelled with that
#' value, and the points are joined by a profile line. A single reference line
#' marks the normative sample's midpoint -- T = 50, or the 50th percentile.
#' Both are definitional properties of the metrics themselves rather than
#' thresholds this package chose.
#'
#' The score axis spans the range the normative tables actually print for the
#' plotted scales, so the axis does not rescale from respondent to respondent
#' and two profiles on the same version and level are directly comparable.
#'
#' On the brief form the profile line stops before `total`: the total is an
#' overall elevation across the five domains rather than a sixth domain, so
#' joining it to the profile line would imply a comparability it does not have.
#' The point itself is still plotted.
#'
#' ## Scales with no value
#'
#' A scale whose normed value is `NA` -- because the respondent's items were
#' missing, or because the score fell outside what could be converted -- is
#' dropped from the profile with a warning naming it, and the remaining scales
#' are still plotted. A scale whose column is absent from `data` altogether is
#' an error rather than a warning: it means `data` was not normed at the level
#' being plotted.
#'
#' @return A \link[ggplot2]{ggplot} object. Print it to draw the profile, or add
#'   further ggplot2 layers to restyle it.
#'
#' @seealso [score_pid5()] to compute the scores, [norm_pid5()] to convert them
#'   against the normative tables, and [pid_norms] for the tables themselves.
#'
#' @references Markon, K. E., Fossati, A., Somma, A., & Krueger, R. F. (2024).
#'   *Understanding the Personality Inventory for DSM-5 (PID-5).* American
#'   Psychiatric Association Publishing. The normative tables in [pid_norms],
#'   Appendix "Normative Score Distributions" (pp. 113-219), supply every value
#'   and every axis bound this function draws.
#'
#' @examplesIf requireNamespace("ggplot2", quietly = TRUE)
#' # Score, norm, and plot one respondent's brief-form domain profile
#' scored <- score_pid5(sim_pid5bf[1, ], items = 1:25, version = "BF")
#' normed <- norm_pid5(scored, scores = paste0("pid_", pid_scales[["BF"]]$camelCase),
#'                     version = "BF")
#' plot_pid5(normed, version = "BF")
#'
#' @export
plot_pid5 <- function(
  data,
  version = c("FULL", "SF", "BF"),
  level = c("domain", "facet"),
  metric = c("t", "percentile"),
  prefix = "pid_"
) {
  ## ggplot2 lives in Suggests (D-002), so the dependency is checked before it
  ## is touched -- the same rlang::is_installed() guard calc_omega() uses for
  ## lavaan. Note check_installed() would NOT work here: it never consults
  ## is_installed() on its detection path, so it cannot be mocked in tests.
  cli_assert(
    condition = rlang::is_installed("ggplot2"),
    message = c(
      "Package {.pkg ggplot2} is required to plot a PID-5 profile.",
      "i" = "Install it with {.code install.packages(\"ggplot2\")}."
    )
  )

  version <- toupper(version)
  version <- match.arg(version, choices = c("FULL", "SF", "BF"))
  level <- match.arg(level)
  metric <- match.arg(metric)
  validate_data(data)
  validate_string(prefix, arg = "prefix")

  ## A profile is one respondent's. Plotting several would need decisions this
  ## function does not make (legends, colouring, how many series is too many),
  ## so more than one row is refused rather than silently reduced to the first.
  if (nrow(data) != 1) {
    cli::cli_abort(c(
      "{.arg data} must have exactly one row.",
      "x" = "You supplied {nrow(data)} row{?s}.",
      "i" = "A profile plot shows one respondent; subset {.arg data} to the row you want, e.g. {.code data[1, ]}."
    ))
  }

  ## The brief form scores five domains and a total directly from its items; it
  ## has no facet scores at all, so there is nothing to plot rather than merely
  ## nothing to norm.
  if (identical(level, "facet") && identical(version, "BF")) {
    cli::cli_abort(c(
      "The brief form has no facet scores to plot.",
      "x" = "{.arg level} is {.val facet} but {.arg version} is {.val BF}.",
      "i" = "The 25 facets are scored by the full (220-item) and short (100-item) forms only; use {.code level = \"domain\"} for the brief form."
    ))
  }

  stems <- plot_scale_stems(version, level)
  suffix <- if (identical(metric, "t")) "_t" else "_ptl"
  ## Column names are built by PASTING the prefix onto each camelCase stem,
  ## which is the direction norm_pid5() names its own outputs (it appends the
  ## suffix to the full score column name). D-026 governs prefix *stripping*;
  ## nothing here strips.
  cols <- paste0(prefix, stems, suffix)

  ## An absent column means `data` was never normed at this level -- an error,
  ## as everywhere else in the package that a requested column is missing.
  absent <- setdiff(cols, names(data))
  if (length(absent) > 0) {
    cli::cli_abort(c(
      "{.arg data} is missing the normed columns this profile plots.",
      "x" = "Not found in {.arg data}: {.val {absent}}.",
      "i" = "Run {.code norm_pid5()} over the {version} {level} scales first, with the same {.arg prefix}."
    ))
  }

  values <- vapply(cols, function(nm) as.numeric(data[[nm]][[1]]), numeric(1))
  ## Percentiles arrive as a proportion; the axis reads 0-100.
  if (identical(metric, "percentile")) {
    values <- values * 100
  }

  ## A scale with no value is dropped rather than plotted as a gap, and the
  ## drop is reported so it is never silent.
  drop <- is.na(values)
  if (all(drop)) {
    cli::cli_abort(c(
      "No scale has a value to plot.",
      "x" = "Every requested {.code {suffix}} column in {.arg data} is {.code NA}.",
      "i" = "Check that {.code norm_pid5()} converted these scales for version {.val {version}}."
    ))
  }
  if (any(drop)) {
    cli::cli_warn(c(
      "!" = "{cli::qty(sum(drop))}Dropped {sum(drop)} scale{?s} with no value from the profile.",
      "*" = "Dropped: {.val {stems[drop]}}.",
      "i" = "A scale is dropped when its normed value is {.code NA} -- missing item data, or a score outside what the tables convert."
    ))
  }

  plot_pid5_build(
    stems = stems[!drop],
    values = values[!drop],
    version = version,
    level = level,
    metric = metric
  )
}


# Which scales a (version, level) pair plots, in the order they are plotted.
#
# The three sources differ deliberately. The five domains of the full and short
# forms are not in `pid_scales[[version]]` at all (which holds their 25 facets),
# so they come from `pid_domains`. The brief form's scales ARE in `pid_scales`,
# in an order that ends on `total` and is not `pid_domains` order -- so it is
# read from there rather than reconstructed.
plot_scale_stems <- function(version, level) {
  if (identical(level, "facet")) {
    return(pid_scales[[version]]$camelCase)
  }
  if (identical(version, "BF")) {
    return(pid_scales[["BF"]]$camelCase)
  }
  pid_domains$camelCase
}


# The panel each facet belongs to, as a factor whose first five levels run in
# `pid_domains` order so the facet plot's panels match the domain plot's points.
#
# `pid_domains$facetStems` is the APA key's domain-DEFINING map: three facets
# per domain, 15 of the 25. The other 10 are scored and normed but define no
# domain, so they get a sixth panel of their own rather than being dropped
# (which would hide scores) or assigned to a domain the key does not put them
# in. The label states the key's assignment and characterizes nothing.
PLOT_UNASSIGNED_PANEL <- "Not domain-defining"

plot_facet_domains <- function(stems) {
  map <- unlist(pid_domains$facetStems, use.names = FALSE)
  owner <- rep(pid_domains$Domain, lengths(pid_domains$facetStems))
  panel <- owner[match(stems, map)]
  panel[is.na(panel)] <- PLOT_UNASSIGNED_PANEL
  factor(panel, levels = c(pid_domains$Domain, PLOT_UNASSIGNED_PANEL))
}


# The axis bounds and breaks for a (version, level, metric) combination.
#
# Every bound comes from the normative tables' own printed rows for the scales
# being plotted, so this function introduces no boundary constant of its own
# (IP2). The percentile metric is the tables' `percentile` column rescaled to
# 0-100, matching what the caller plots.
plot_pid5_axis <- function(stems, version, metric) {
  rows <- pid_norms[
    pid_norms$version == version & pid_norms$scale %in% stems,
    ,
    drop = FALSE
  ]
  if (identical(metric, "t")) {
    span <- range(rows$tscore, na.rm = TRUE)
    ## Decade gridlines across the printed span: 10 is the T metric's own
    ## standard deviation, not a threshold.
    breaks <- seq(
      ceiling(span[[1]] / 10) * 10,
      floor(span[[2]] / 10) * 10,
      by = 10
    )
    return(list(limits = span, breaks = breaks, midpoint = 50))
  }
  span <- range(rows$percentile, na.rm = TRUE) * 100
  list(limits = span, breaks = seq(0, 100, by = 25), midpoint = 50)
}


# Assemble the profile. Layer order is load-bearing and asserted in the tests:
# reference line, profile line, points, labels.
plot_pid5_build <- function(stems, values, version, level, metric) {
  axis <- plot_pid5_axis(stems, version, metric)

  df <- data.frame(
    scale = factor(stems, levels = stems),
    value = values,
    stringsAsFactors = FALSE
  )

  ## The profile line joins points within a panel; on an unfacetted plot every
  ## point is one series. x is discrete, so the group must be given explicitly
  ## or ggplot2 would draw one segment per x position (i.e. nothing).
  if (identical(level, "facet")) {
    df$panel <- plot_facet_domains(stems)
    df$panel_group <- as.integer(df$panel)
  } else {
    df$panel_group <- 1L
  }

  ## The brief form's total is an overall elevation across the five domains,
  ## not a sixth domain, so the profile line stops before it. The line layer is
  ## given its own filtered data rather than a grouping aesthetic: a one-point
  ## group would still contribute a row to the layer's built data, and the
  ## tests assert the line covers the five domains exactly.
  line_df <- if (identical(version, "BF") && identical(level, "domain")) {
    df[df$scale != "total", , drop = FALSE]
  } else {
    df
  }

  y_label <- if (identical(metric, "t")) "T score" else "Percentile"

  p <- ggplot2::ggplot(
    df,
    ggplot2::aes(x = .data$scale, y = .data$value)
  ) +
    ## The midpoint of the normative sample. It carries no text label: a
    ## labelled reference line is a second text layer, and the tests assert
    ## exactly one.
    ggplot2::geom_hline(
      yintercept = axis$midpoint,
      linetype = "dashed",
      colour = "grey50"
    ) +
    ggplot2::geom_line(
      data = line_df,
      ggplot2::aes(group = .data$panel_group),
      linewidth = 0.7
    ) +
    ggplot2::geom_point(size = 2.5) +
    ggplot2::geom_label(
      ggplot2::aes(label = round(.data$value)),
      size = 3,
      label.padding = ggplot2::unit(0.15, "lines")
    ) +
    ggplot2::scale_y_continuous(
      breaks = axis$breaks,
      limits = axis$limits
    ) +
    ggplot2::labs(
      x = NULL,
      y = y_label,
      title = paste0("PID-5 ", version, " ", level, " profile")
    ) +
    ggplot2::coord_flip() +
    ggplot2::theme_bw(base_size = 12)

  if (identical(level, "facet")) {
    ## `free_x` frees the *scale-name* axis, which is x before coord_flip(),
    ## so each panel lists only its own three facets. The value axis stays
    ## fixed across panels -- freeing it would defeat the point of a common
    ## norm-referenced span.
    p <- p + ggplot2::facet_wrap(~panel, ncol = 1, scales = "free_x")
  }

  p
}
