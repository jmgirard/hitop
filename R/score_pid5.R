#' Score the Personality Inventory for DSM-5
#'
#' Calculate scale scores on the Personality Inventory for DSM-5: full version
#' (PID-5, 220 items), short form version (PID-5-SF, 100 items), or brief form
#' version (PID-5-BF, 25 items) from item-level data.
#'
#' @param data A data frame containing (at least) all the PID items (numerically
#'   scored and in order).
#' @param items A vector of column names (as strings) or numbers (as integers)
#'   corresponding to the PID items in order. Items must be supplied in
#'   instrument order; a misordered mapping silently scores the wrong items, so a
#'   warning is issued when the names share a common prefix and trailing number
#'   but those numbers are not ascending. Duplicated entries are an error.
#' @param version A string indicating the version of the PID to score: "FULL",
#'   "SF", or "BF". Will be automatically capitalized. (default = `"FULL"`)
#' @param srange An optional numeric vector specifying the minimum and maximum
#'   values of the items, used for reverse-coding. (default = `c(0, 3)`)
#' @param prefix An optional string to add before each scale column name. If no
#'   prefix is desired, set to an empty string `""`. (default = `"pid_"`)
#' @param missing A string selecting how missing item responses are handled when
#'   computing scale scores. `"apa"` (the default) follows the published APA
#'   scoring key: a facet or domain-item scale with more than 25% of its items
#'   unanswered is set to `NA`, and otherwise the raw score is prorated to the
#'   full item count and rounded to the nearest whole number before averaging (a
#'   FULL/SF domain is `NA` if any one of its three contributing facets is `NA`).
#'   `"available"` averages whatever items are present (`rowMeans(na.rm = TRUE)`).
#'   `"complete"` returns `NA` for any scale with a missing item
#'   (`rowMeans(na.rm = FALSE)`). With no missing items the three agree. (default
#'   = `"apa"`)
#' @param calc_se An optional logical indicating whether to calculate the
#'   standard error of each scale score. Standard errors are `NA` wherever their
#'   scale score is `NA`. (default = `FALSE`)
#' @param append An optional logical indicating whether the new columns should
#'   be added to the end of the `data` input. (default = `TRUE`)
#'
#' @details For the FULL and SF versions, the output includes the 25 facet
#'   scores followed by the 5 personality-trait domain scores. Following the APA
#'   scoring key (Step 3), each domain score is the mean of the average scores of
#'   its 3 primary facets (the map is stored in `pid_domains`). The BF version
#'   scores its 5 domains directly from its items. By default (`missing = "apa"`)
#'   all versions apply the APA missing-data and proration rule; use
#'   `missing = "available"` or `missing = "complete"` for the traditional
#'   `rowMeans()` behaviors. For per-scale reliability estimates (Cronbach's
#'   alpha, McDonald's omega), use [reliability_pid5()].
#'
#' @return A \link[tibble]{tibble} containing all scale scores and standard
#'   errors (if requested) and all original `data` columns (if requested)
#'
#' @references Krueger, R. F., Derringer, J., Markon, K. E., Watson, D., &
#'   Skodol, A. E. (2012). Initial construction of a maladaptive personality
#'   trait model and inventory for DSM-5. *Psychological Medicine, 42*,
#'   1879-1890. \doi{10.1017/s0033291711002674}
#' @references Anderson, J. L., Sellbom, M., & Salekin, R. T. (2016). Utility of
#'   the Personality Inventory for DSM-5-Brief Form (PID-5-BF) in the
#'   measurement of maladaptive personality and psychopathology. *Assessment,
#'   25*(5), 596–607. \doi{10.1177/1073191116676889}
#'
#' @references Maples, J. L., Carter, N. T., Few, L. R., Crego, C., Gore, W. L.,
#'   Samuel, D. B., Williamson, R. L., Lynam, D. R., Widiger, T. A., Markon, K.
#'   E., Krueger, R. F., & Miller, J. D. (2015). Testing whether the DSM-5
#'   personality disorder trait model can be measured with a reduced set of
#'   items: An item response theory investigation of the personality inventory
#'   for DSM-5. *Psychological Assessment, 27*(4), 1195–1210.
#'   \doi{10.1037/pas0000120}
#'
#' @examples
#' # Score the full PID-5 (25 facets + 5 domains) from the simulated data
#' score_pid5(sim_pid5, items = 1:220, version = "FULL", append = FALSE)
#'
#' # Short form, using the item column names instead of positions
#' score_pid5(sim_pid5sf, items = sprintf("pid_%d", 1:100), version = "SF",
#'            append = FALSE)
#'
#' # Brief form (5 domains) with standard errors
#' score_pid5(sim_pid5bf, items = 1:25, version = "BF", calc_se = TRUE,
#'            append = FALSE)
#'
#' @export
score_pid5 <- function(
  data,
  items,
  version = c("FULL", "SF", "BF"),
  srange = c(0, 3),
  prefix = "pid_",
  missing = c("apa", "available", "complete"),
  calc_se = FALSE,
  append = TRUE
) {
  ## Resolve the version and its item count (shared arg validation runs in the
  ## engine; version is PID-specific and resolved here)
  version <- toupper(version)
  version <- match.arg(version, choices = c("FULL", "SF", "BF"))
  missing <- match.arg(missing)
  n_items <- switch(
    version,
    "FULL" = 220,
    "SF" = 100,
    "BF" = 25,
    cli::cli_abort("Invalid `version` argument")
  )

  ## Resolve this version's instrument data: which items reverse, the per-scale
  ## item-number lists, and (FULL/SF only) the domain -> primary-facet map.
  reverse_items <- drop_na(
    pid_items[pid_items$Reverse == TRUE, version, drop = TRUE]
  )
  items_scales <- pid_scales[[version]]$itemNumbers
  domain_map <- if (version %in% c("FULL", "SF")) {
    setNames(pid_domains$facetStems, pid_domains$camelCase)
  } else {
    NULL
  }

  score_engine(
    data = data,
    items = items,
    n_items = n_items,
    reverse_items = reverse_items,
    items_scales = items_scales,
    srange = srange,
    prefix = prefix,
    missing = missing,
    calc_se = calc_se,
    append = append,
    domain_map = domain_map,
    mask_se_na = TRUE
  )
}
