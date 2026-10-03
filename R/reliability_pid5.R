#' Estimate PID-5 scale reliability
#'
#' Compute per-scale internal-consistency reliability — Cronbach's alpha and
#' McDonald's omega — for the Personality Inventory for DSM-5: full version
#' (PID-5, 220 items), short form (PID-5-SF, 100 items), brief form (PID-5-BF,
#' 25 items), or modified brief form (PID5BF+M, 36 items). Reliability is
#' estimated on the reverse-keyed item responses, at the facet level for FULL/SF
#' and the domain level for BF (the same scales [score_pid5()] outputs, before
#' FULL/SF domain aggregation). The BF version also returns a `Total` row
#' covering all 25 items; note that this scale spans five heterogeneous domains,
#' so its internal consistency is not comparable to a domain's and is reported
#' without further interpretation.
#'
#' The BFPM version returns both levels: its 18 two-item facets, then its 6
#' domains, each estimated over the 6 items of its 3 facets (the map is in
#' [pid_bfpm_domains]). Omega is `NA` for every facet, because a one-factor
#' model of 2 items is not identified; alpha is reported for all 24 rows.
#'
#' @param data A data frame containing (at least) all the PID items (numerically
#'   scored and in order).
#' @param items A vector of column names (as strings) or numbers (as integers)
#'   corresponding to the PID items in order. Items must be supplied in
#'   instrument order; duplicated entries are an error.
#'   Each column must be numeric or logical, or character holding only
#'   numbers (blank cells and `NA` values count as missing, but the text
#'   `"NA"` is refused). A haven labelled column is read as its plain values.
#'   Any other column, such as the choice text of an online export or a
#'   factor, is an error of class `hitop_nonnumeric_items`. So is a 64-bit
#'   integer (`integer64`) column, and, in a UTF-8 session, a column holding
#'   text that is not valid UTF-8 (text marked Latin-1 is read as its text);
#'   convert such text with
#'   `iconv()`. So is an SPSS column (`haven_labelled_spss`) that holds a
#'   value it declares missing; turn those codes into `NA` with
#'   `haven::zap_missing()` first. A declared value that is blank is not
#'   refused, since blank cells count as missing. The SPSS class of haven
#'   before 2.0 (`labelled_spss`) is refused the same way, but
#'   `haven::zap_missing()` leaves it unchanged, so set the values its
#'   `na_values` or `na_range` attribute declares to `NA` first.
#' @param version A string indicating the version of the PID to score: "FULL",
#'   "SF", "BF", or "BFPM" (the 36-item PID5BF+M). Will be automatically
#'   capitalized. (default = `"FULL"`)
#' @param srange An optional numeric vector specifying the minimum and maximum
#'   values of the items, used for reverse-coding. (default = `c(0, 3)`)
#' @param alpha Optional logical; if `TRUE`, include a column of Cronbach's alpha
#'   per scale. (default = `TRUE`)
#' @param omega Optional logical; if `TRUE`, include a column of McDonald's omega
#'   (total) per scale, estimated via a one-factor CFA (requires the \pkg{lavaan}
#'   package). (default = `TRUE`)
#'
#' @details Alpha is computed by [calc_alpha()] (covariance-based, pairwise
#'   deletion) and omega by [calc_omega()] (one-factor lavaan CFA, FIML). A scale
#'   whose estimate cannot be computed (e.g. too few items or, for omega, a
#'   non-converging CFA or an uninstalled \pkg{lavaan}) is returned as `NA`
#'   rather than aborting the call. Omega needs at least 3 items: for a scale
#'   with fewer, no model is fitted and omega is `NA`.
#'
#' @return A \link[tibble]{tibble} with one row per scale and columns `Scale`
#'   (the scale's canonical display name, as the instrument's keying table spells
#'   it), `camelCase` (the stem that names the scale's column in the matching
#'   `score_*()` output, read from the same keying-table row), `nItems`
#'   (integer), and (when requested) `alpha` and `omega`.
#'
#' @examples
#' # Facet-level reliability for the full PID-5 (alpha only)
#' reliability_pid5(sim_pid5, items = 1:220, version = "FULL", omega = FALSE)
#'
#' @export
reliability_pid5 <- function(
  data,
  items,
  version = c("FULL", "SF", "BF", "BFPM"),
  srange = c(0, 3),
  alpha = TRUE,
  omega = TRUE
) {
  version <- toupper(version)
  version <- match.arg(version, choices = c("FULL", "SF", "BF", "BFPM"))
  n_items <- switch(
    version,
    "FULL" = 220,
    "SF" = 100,
    "BF" = 25,
    "BFPM" = 36,
    cli::cli_abort("Invalid `version` argument")
  )

  reverse_items <- drop_na(
    pid_items[pid_items$Reverse == TRUE, version, drop = TRUE]
  )
  items_scales <- pid_scales[[version]]$itemNumbers
  ## The canonical display names, read from the same table row for row. FULL and
  ## SF are facet-level; BF is domain-level plus its Total row.
  scale_names <- if (version == "BF") {
    pid_scales[["BF"]]$Domain
  } else {
    pid_scales[[version]]$Facet
  }
  scale_stems <- pid_scales[[version]]$camelCase

  ## BFPM adds its 6 domain rows after its 18 facets (D-088(c)). Each domain's
  ## items are the items of its 3 facets, built here from `pid_bfpm_domains`
  ## rather than stored, so `pid_scales$BFPM` holds facets only and
  ## score_pid5() never scores a domain from its items.
  if (version == "BFPM") {
    domain_items <- lapply(
      pid_bfpm_domains$facetStems,
      function(f) unlist(items_scales[f], use.names = FALSE)
    )
    names(domain_items) <- pid_bfpm_domains$camelCase
    items_scales <- c(items_scales, domain_items)
    scale_names <- c(scale_names, pid_bfpm_domains$Domain)
    scale_stems <- c(scale_stems, pid_bfpm_domains$camelCase)
  }

  reliability_engine(
    data = data,
    items = items,
    n_items = n_items,
    reverse_items = reverse_items,
    items_scales = items_scales,
    scale_names = scale_names,
    scale_stems = scale_stems,
    srange = srange,
    alpha = alpha,
    omega = omega
  )
}
