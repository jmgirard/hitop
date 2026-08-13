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
#'   domains, plus the brief form's total. `"facet"` plots all 25 facets in
#'   panels, and is available for `"FULL"` and `"SF"` only -- the brief form has
#'   no facet scores. The APA key ties three facets to each domain; those get a
#'   panel per domain, and the remaining ten, which define no domain, share a
#'   final panel rather than being dropped.
#' @param metric Which normed metric to plot: `"t"` for T scores, or
#'   `"percentile"` for percentile ranks. [norm_pid5()] returns percentiles as a
#'   proportion; this function multiplies them by 100 so the axis reads on the
#'   familiar 0-100 percentile scale.
#' @param labels Whether to label each point with its rounded value. `TRUE` by
#'   default. The labels need a figure about 7 inches wide or more: below that,
#'   a label on a score at the top of the published span runs into the edge of
#'   the panel and is cut off. Set `labels = FALSE` for a narrower figure and
#'   the points and profile line are drawn without them; the score axis then
#'   drops the extra padding it reserves for a label, so the profile itself
#'   gets that width back. This is a choice you
#'   make, not one the function can make for you -- a plot is assembled before
#'   anything knows what size it will be drawn at.
#' @param prefix The column-name prefix used when the scores were computed, as
#'   passed to [score_pid5()] and [norm_pid5()]. Pasted onto each scale's
#'   camelCase name to find its column.
#'
#' @details
#' ## What the plot draws
#'
#' Each plotted scale gets a point at its normed value, labelled with that
#' value just to its right (set `labels = FALSE` to drop the labels), and the
#' points are joined by a profile line. A single reference line
#' marks the normative sample's midpoint -- T = 50, or the 50th percentile.
#' Both are definitional properties of the metrics themselves rather than
#' thresholds this package chose.
#'
#' The score axis spans the range the normative tables actually print for the
#' plotted scales, so the axis does not rescale from respondent to respondent
#' and two profiles on the same version and level are directly comparable.
#' Scales are listed top to bottom in the order their scoring table gives them,
#' under their printed names rather than their column stems.
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
  labels = TRUE,
  prefix = "pid_"
) {
  ## ggplot2 lives in Suggests (D-002), so the dependency is checked before it
  ## is touched -- the same rlang::is_installed() guard calc_omega() uses for
  ## lavaan. Note check_installed() would NOT work here: it never consults
  ## is_installed() on its detection path, so it cannot be mocked in tests.
  ##
  ## The version floor is enforced HERE and not only declared in DESCRIPTION:
  ## R does not check a Suggests floor when a package is merely loaded, so the
  ## declaration alone would leave an old ggplot2 failing on an unknown
  ## `linewidth` parameter instead of saying which version is wanted (D-031).
  cli_assert(
    condition = rlang::is_installed("ggplot2", version = "3.4.0"),
    message = c(
      "Package {.pkg ggplot2} (>= 3.4.0) is required to plot a PID-5 profile.",
      "i" = "Install or update it with {.code install.packages(\"ggplot2\")}."
    )
  )

  version <- toupper(version)
  version <- match.arg(version, choices = c("FULL", "SF", "BF"))
  level <- match.arg(level)
  metric <- match.arg(metric)
  validate_data(data)
  validate_flag(labels, arg = "labels")
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

  ## Only a numeric column can be plotted against a norm table. A character
  ## column coercing to NA would otherwise be reported below as a dropped
  ## scale, hiding a type mistake behind a warning about missing data -- so
  ## this stays ahead of the NA-drop branch. norm_pid5() refuses the same input
  ## for the same reason, through the same guard.
  validate_numeric_columns(
    data[cols],
    headline = function(n) {
      cli::format_inline(
        "{cli::qty(n)}The normed column{?s} this profile plots must be numeric in {.arg data}."
      )
    },
    info = function(n) {
      cli::format_inline(
        "A factor's integer codes are not its scores, and a character column coerces to {.code NA}, so neither is plotted for you. Convert {cli::qty(n)}{?it/them} before calling {.code plot_pid5()}."
      )
    }
  )

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
    ## The axis is computed from every scale this (version, level) plots, not
    ## from the ones that survived the NA drop -- otherwise the axis would be a
    ## function of which scales happened to be missing for this respondent, and
    ## the documented "does not rescale from respondent to respondent"
    ## guarantee would hold only by coincidence of the shipped tables.
    axis_stems = stems,
    version = version,
    level = level,
    metric = metric,
    show_labels = labels
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


# The printed name for each scale stem, from the same tables the stems came
# from. The axis reads "Negative affectivity", not "negativeAffectivity" --
# the plot is a presentation artifact and camelCase is a column-naming
# convention, not a label.
plot_scale_labels <- function(stems, version, level) {
  table <- if (identical(level, "facet")) {
    stats::setNames(pid_scales[[version]]$Facet, pid_scales[[version]]$camelCase)
  } else if (identical(version, "BF")) {
    stats::setNames(pid_scales[["BF"]]$Domain, pid_scales[["BF"]]$camelCase)
  } else {
    stats::setNames(pid_domains$Domain, pid_domains$camelCase)
  }
  ## A stem the table does not name keeps its stem rather than becoming NA.
  out <- unname(table[stems])
  ifelse(is.na(out), stems, out)
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
  ## A stem with no rows for this version would make range() return c(Inf,-Inf)
  ## after a base-R warning, and hand an incoherent axis to the scale. Not
  ## reachable from the shipped tables, but the helper must not depend on that.
  missing_rows <- setdiff(stems, rows$scale)
  if (length(missing_rows) > 0) {
    cli::cli_abort(c(
      "{.field pid_norms} has no rows for {cli::qty(length(missing_rows))}{?this scale/these scales} on version {.val {version}}.",
      "x" = "Missing: {.val {missing_rows}}.",
      "i" = "The axis is derived from the published tables, so a scale absent from them cannot be placed on it."
    ))
  }
  if (identical(metric, "t")) {
    span <- range(rows$tscore, na.rm = TRUE)
    ## Decade gridlines: 10 is the T metric's own standard deviation, stepped
    ## across the published span. Not a threshold.
    return(list(limits = span, breaks = axis_breaks(span, step = 10), midpoint = 50))
  }
  ## The tables' percentile column, on the 0-100 scale the caller plots.
  ## Quartiles divide a percentile scale as decades divide the T metric; both
  ## step across the published span rather than sitting at fixed positions, so
  ## neither introduces a bound of this package's choosing.
  span <- range(rows$percentile, na.rm = TRUE) * 100
  list(limits = span, breaks = axis_breaks(span, step = 25), midpoint = 50)
}


# Gridline positions at multiples of `step` inside `span`. A span narrower than
# one step contains no multiple, and seq() would then run backwards and error
# ("wrong sign in 'by' argument") -- so that case falls back to the span's own
# endpoints rather than producing an invalid sequence.
axis_breaks <- function(span, step) {
  lo <- ceiling(span[[1]] / step) * step
  hi <- floor(span[[2]] / step) * step
  if (lo > hi) {
    return(span)
  }
  seq(lo, hi, by = step)
}


# Assemble the profile.
#
# The scale names go on y and the scores on x directly, rather than being built
# vertically and flipped: coord_flip() applies a facet's free scale BEFORE the
# flip, so every panel of a facetted profile drew all 25 scale names on top of
# one another. Mapping the axes as they are drawn lets `scales = "free_y"` act
# on the axis it names -- provided nothing pins that scale's limits, which is
# why the pin below is applied only to the unfacetted branch.
plot_pid5_build <- function(stems, values, axis_stems, version, level, metric,
                            show_labels = TRUE) {
  axis <- plot_pid5_axis(axis_stems, version, metric)

  ## `stem` is the canonical scale name and carries the logic (and the tests);
  ## `scale` is the printed label. Level order is set so the table's first
  ## scale sits at the TOP of the axis, which is how a profile is read. A
  ## discrete y scale draws its first level at the BOTTOM, so the levels are
  ## reversed; the test asserts the drawn order rather than the level order.
  labels <- plot_scale_labels(stems, version, level)
  df <- data.frame(
    stem = stems,
    scale = factor(labels, levels = rev(labels)),
    value = values,
    stringsAsFactors = FALSE
  )

  ## The profile line joins points within a panel; on an unfacetted plot every
  ## point is one series. y is discrete, so the group must be given explicitly
  ## or ggplot2 would draw one segment per y position (i.e. nothing).
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
    df[df$stem != "total", , drop = FALSE]
  } else {
    df
  }

  x_label <- if (identical(metric, "t")) "T score" else "Percentile"

  p <- ggplot2::ggplot(
    df,
    ggplot2::aes(x = .data$value, y = .data$scale)
  ) +
    ## The midpoint of the normative sample. It carries no text label: a
    ## labelled reference line is a second text layer, and the tests assert
    ## exactly one.
    ggplot2::geom_vline(
      xintercept = axis$midpoint,
      linetype = "dashed",
      colour = "grey50"
    ) +
    ggplot2::geom_line(
      data = line_df,
      ggplot2::aes(group = .data$panel_group),
      orientation = "y",
      linewidth = 0.7
    ) +
    ggplot2::geom_point(size = 2.5) +
    ggplot2::scale_x_continuous(
      breaks = axis$breaks,
      limits = axis$limits,
      ## Padding on the label side, so a score at the top of the published span
      ## still has somewhere for its label to sit. This is expansion around the
      ## trained range, never a change to `limits` or `breaks` -- the axis
      ## still spans exactly what the tables print.
      ##
      ## The wider side is the label's room and is taken only when a label is
      ## drawn there. Asked for `labels = FALSE`, the caller wants a narrower
      ## figure, so reserving room for a label that is not drawn would spend
      ## the width they asked to save on empty margin.
      expand = ggplot2::expansion(
        mult = if (isTRUE(show_labels)) c(0.03, 0.12) else c(0.03, 0.03)
      )
    ) +
    ggplot2::labs(
      x = x_label,
      y = NULL,
      title = paste0("PID-5 ", version, " ", level, " profile")
    ) +
    ggplot2::theme_bw(base_size = 12)

  if (isTRUE(show_labels)) {
    ## Offset off the point, or the label sits exactly on top of the marker it
    ## labels and hides it entirely. Two constraints decide the direction.
    ##
    ## It is `hjust` and never a nudge of the x value: nudging in data space
    ## pushes a score near the end of the axis PAST the limit, and ggplot2 then
    ## drops that label silently (a percentile of 98 nudged to 102.5 vanished).
    ##
    ## And it is HORIZONTAL and never vertical. A rendering offset is an
    ## absolute distance, while any room reserved for it on the discrete axis
    ## is measured in data units whose physical size shrinks with the panel --
    ## so a `vjust` offset is clipped by the panel edge on a small enough
    ## device, in every panel, however much discrete expansion is added. The
    ## continuous axis is padded on the label side instead, and that padding is
    ## the room the label needs -- which is why the scale above takes it only
    ## on this branch, and pads both ends evenly when no label is drawn. That is
    ## better, not immune: the padding is a proportion of the data range, so it
    ## too shrinks with panel width. It holds at the figure widths `?plot_pid5`
    ## documents and no wider promise is made -- below them the caller passes
    ## `labels = FALSE`.
    p <- p + ggplot2::geom_label(
      ggplot2::aes(label = round(.data$value)),
      hjust = -0.6,
      size = 3,
      label.padding = ggplot2::unit(0.15, "lines")
    )
  }

  if (identical(level, "facet")) {
    ## `free_y` frees the scale-name axis so each panel lists only its own
    ## facets, and `space = "free_y"` sizes each panel to how many it lists --
    ## without it the sixth panel's ten facets are crushed into the height
    ## three get. The value axis stays fixed across panels; freeing it would
    ## defeat the point of a common norm-referenced span.
    ##
    ## Deliberately NO `scale_y_discrete(limits =)` here: pinning the discrete
    ## limits overrides per-panel training, which silently disables `free_y`
    ## and `space = "free_y"` and draws all 25 names in every panel. The
    ## unfacetted branch below needs the pin for a different reason, so the
    ## two cases are handled separately rather than shared.
    p <- p +
      ggplot2::facet_grid(
        rows = ggplot2::vars(.data$panel),
        scales = "free_y",
        space = "free_y",
        switch = "y",
        labeller = ggplot2::label_wrap_gen(width = 14)
      ) +
      ## Deliberately no `scale_y_discrete()` in this branch at all. An earlier
      ## fix widened the discrete expansion here to hold the value label; the
      ## label is offset horizontally now, so the room is taken on the
      ## continuous axis instead and this axis is left to ggplot2's default --
      ## widening it only added dead margin above the top point.
      ##
      ## Horizontal strip text: rotated, a long domain name is clipped to the
      ## height of a panel listing only three facets.
      ggplot2::theme(
        strip.placement = "outside",
        strip.text.y.left = ggplot2::element_text(angle = 0)
      )
  } else {
    ## Unfacetted: pin the discrete positions rather than leave them to layer
    ## training. The profile-line layer deliberately omits the brief form's
    ## `total`, and a scale trained across layers puts a value missing from the
    ## first layer LAST -- which drew `total` at the top instead of the bottom.
    ## Safe here precisely because there is one panel to train.
    p <- p + ggplot2::scale_y_discrete(limits = rev(labels))
  }

  p
}
