#' Score the Personality Inventory for DSM-5
#'
#' Calculate scale scores on the Personality Inventory for DSM-5: full version
#' (PID-5, 220 items), short form version (PID-5-SF, 100 items), or brief form
#' version (PID-5-BF, 25 items) from item-level data.
#'
#' @param data A data frame containing (at least) all the PID items (numerically
#'   scored and in order).
#' @param items A vector of column names (as strings) or numbers (as integers)
#'   corresponding to the PID items in order.
#' @param version A string indicating the version of the PID to score: "FULL",
#'   "SF", or "BF". Will be automatically capitalized. (default = `"FULL"`)
#' @param srange An optional numeric vector specifying the minimum and maximum
#'   values of the items, used for reverse-coding. (default = `c(0, 3)`)
#' @param prefix An optional string to add before each scale column name. If no
#'   prefix is desired, set to an empty string `""`. (default = `"pid_"`)
#' @param na.rm An optional logical indicating whether missing values should be
#'   ignored when calculating scale scores. (default = `TRUE`)
#' @param calc_se An optional logical indicating whether to calculate the
#'   standard error of each scale score. (default = `FALSE`)
#' @param alpha Optional logical; if `TRUE`, compute and print Cronbach’s alpha
#'   for each scale. (default = `FALSE`)
#' @param omega Optional logical; if `TRUE`, compute and print McDonald’s omega
#'   for each scale using Pearson correlations (i.e., non-ordinal). (default =
#'   `FALSE`)
#' @param append An optional logical indicating whether the new columns should
#'   be added to the end of the `data` input. (default = `TRUE`)
#' @param tibble An optional logical indicating whether the output should be
#'   converted to a \link[tibble]{tibble}. (default = `TRUE`)
#'
#' @details For the FULL and SF versions, the output includes the 25 facet
#'   scores followed by the 5 personality-trait domain scores. Following the APA
#'   scoring key (Step 3), each domain score is the mean of the average scores of
#'   its 3 primary facets (the map is stored in `pid_domains`); domains honor
#'   `na.rm` the same way facets do. The BF version scores its 5 domains directly
#'   from its items and is unaffected. If either `alpha` or `omega` are `TRUE`,
#'   the function prints a per-scale reliability summary (facets only for FULL/SF).
#'   Only reliability columns that contain at least one non-`NA` value are shown
#'   (the `scale` column is always shown).
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
#' @export
score_pid5 <- function(
  data,
  items,
  version = c("FULL", "SF", "BF"),
  srange = c(0, 3),
  prefix = "pid_",
  na.rm = TRUE,
  calc_se = FALSE,
  alpha = FALSE,
  omega = FALSE,
  append = TRUE,
  tibble = TRUE
) {
  ## Assertions
  validate_data(data)
  version <- toupper(version)
  version <- match.arg(version, choices = c("FULL", "SF", "BF"))
  n_items <- switch(
    version,
    "FULL" = 220,
    "SF" = 100,
    "BF" = 25,
    cli::cli_abort("Invalid `version` argument")
  )
  validate_items(items, n = n_items)
  # TODO: validate that the requested items are possible to subset to
  validate_range(srange)
  stopifnot(rlang::is_string(prefix))
  stopifnot(rlang::is_bool(na.rm))
  stopifnot(rlang::is_bool(calc_se))
  stopifnot(rlang::is_bool(alpha))
  stopifnot(rlang::is_bool(omega))
  stopifnot(rlang::is_bool(append))
  stopifnot(rlang::is_bool(tibble))

  ## Extract item columns
  data_items <- data[items]

  ## Coerce values to numbers
  data_items <- lapply(data_items, as.numeric)

  ## Reverse score the necessary items
  items_rev <- drop_na(
    pid_items[pid_items$Reverse == TRUE, version, drop = TRUE]
  )
  if (length(items_rev) > 0) {
    data_items[items_rev] <- lapply(
      items_rev,
      function(i) {
        reverse(
          data_items[[i]],
          low = srange[[1]],
          high = srange[[2]]
        )
      }
    )
  }
  data_items <- bind_columns(data_items)

  ## Find items per scale
  items_scales <- pid_scales[[version]]$itemNumbers

  ## Calculate mean scores per scale (facets for FULL/SF, domains for BF)
  scale_scores <- bind_columns(
    lapply(
      items_scales,
      function(x) rowMeans(data_items[, x], na.rm = na.rm)
    )
  )

  ## For FULL/SF, add the 5 personality-trait domain scores (APA key Step 3):
  ## each domain is the mean of its 3 PRIMARY facet average scores (map stored in
  ## pid_domains). BF already scores domains directly, so it is skipped here.
  domain_facets <- NULL
  if (version %in% c("FULL", "SF")) {
    domain_facets <- setNames(pid_domains$facetStems, pid_domains$camelCase)
    domain_scores <- bind_columns(
      lapply(
        domain_facets,
        function(f) rowMeans(scale_scores[, f, drop = FALSE], na.rm = na.rm)
      )
    )
    out <- cbind(scale_scores, domain_scores)
  } else {
    out <- scale_scores
  }

  ## Apply prefix to scale column names
  colnames(out) <- paste0(prefix, colnames(out))

  ## Add standard errors to output if requested
  if (calc_se) {
    sems_scales <-
      bind_columns(
        lapply(
          items_scales,
          function(x) apply(data_items[, x], MARGIN = 1, FUN = calc_sem)
        )
      )
    ## Domain SEs mirror facet SEs, computed over the 3 primary facet scores
    if (!is.null(domain_facets)) {
      sems_domains <- bind_columns(
        lapply(
          domain_facets,
          function(f) apply(scale_scores[, f, drop = FALSE], MARGIN = 1, FUN = calc_sem)
        )
      )
      sems_scales <- cbind(sems_scales, sems_domains)
    }
    colnames(sems_scales) <- paste0(prefix, colnames(sems_scales), "_se")
    out <- cbind(out, sems_scales)
  }

  ## Preallocate reliability data frame
  reliability_df <- data.frame(
    scale = snakecase::to_title_case(names(items_scales)),
    alpha = NA_real_,
    omega = NA_real_
  )

  # Calculate alpha per scale if requested
  if (isTRUE(alpha)) {
    safe_alpha <- function(idx) {
      df_sub <- as.data.frame(data_items[, idx, drop = FALSE])
      tryCatch(
        calc_alpha(df_sub),
        error = function(e) NA_real_
      )
    }
    reliability_df[, "alpha"] <- vapply(items_scales, safe_alpha, numeric(1))
  }

  # Calculate omega per scale if requested
  if (isTRUE(omega)) {
    safe_omega <- function(idx) {
      df_sub <- as.data.frame(data_items[, idx, drop = FALSE])
      tryCatch(
        calc_omega(df_sub),
        error = function(e) NA_real_
      )
    }
    reliability_df[, "omega"] <- vapply(items_scales, safe_omega, numeric(1))
  }

  # If any reliabilities are requested, print those that were calculated
  if (isTRUE(alpha) || isTRUE(omega)) {
    keep <- c("scale", if (isTRUE(alpha)) "alpha", if (isTRUE(omega)) "omega")
    print(reliability_df[, keep, drop = FALSE], digits = 3)
  }

  ## Append output to input tibble if requested
  if (append == TRUE) {
    out <- cbind(data, out)
  }

  ## Coerce output to tibble if requested
  if (tibble == TRUE) {
    out <- tibble::as_tibble(out)
  }

  ## Return output
  out
}
