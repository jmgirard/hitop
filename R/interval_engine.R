#' Internal interval engine shared by the interval_*() family
#'
#' Computes the regression-based true score with scale correction, and its
#' confidence interval, for already-scored columns. Schmukle (2026, *Assessment*,
#' 33(5), 817-825), Equations (10) to (12) on p. 821:
#'
#'   RETS = M + sqrt(rel) * (x - M)
#'   SEM  = SD * sqrt(1 - rel)
#'   CI   = RETS +/- z * SEM
#'
#' where `M`, `SD` and `rel` are the reference group's mean, standard deviation
#' and reliability for that scale, and `z` is the two-sided normal quantile for
#' the requested level. The bounds are deliberately not clamped to the response
#' range: Equation (12) is symmetric and constant-width, so on a strongly skewed
#' scale a bound can fall outside it, and clamping would compute something other
#' than the cited equation (M041 Decisions, from RR05).
#'
#' `interval_hitopsr()` and `interval_hitopbr()` call this. It takes the
#' reference statistics as an argument rather than reading a dataset, so a
#' further wrapper -- a PID-5 one -- can hand it a different table without the
#' engine learning about instruments.
#'
#' @param data,scores,srange,prefix,level,append As in the wrappers.
#' @param refstats A data frame of reference statistics with columns `camelCase`
#'   (the stem a score column carries after `prefix` is stripped), `mean`, `sd`
#'   and `reliability`. One row per scale; a stem it does not carry is uncovered.
#' @param ref_srange The response coding `refstats`'s `mean` and `sd` are printed
#'   on, as `c(low, high)`. A call whose `srange` is anything else is on a
#'   different metric and is not converted.
#' @param dataset The name of the reference dataset, used in the reports so a
#'   user is told where to look rather than only what failed.
#' @param call The calling environment, so input-validation aborts blame the
#'   exported wrapper rather than this internal engine.
#' @noRd
interval_engine <- function(
  data,
  scores,
  refstats,
  ref_srange,
  srange,
  prefix,
  level,
  append,
  dataset,
  call = rlang::caller_env()
) {
  validate_data(data, call = call)
  ## Every complaint about this argument says `scores`, the name the caller
  ## actually wrote, rather than the `items` the scoring family passes these
  ## validators.
  validate_scales(scores, arg = "scores", call = call)
  ## A selection that names no column aborts here, after its own type check and
  ## ahead of `srange`, `prefix`, `level` and `append` (D-045(b)). `data` is
  ## checked above and is exempt.
  validate_nonempty_selection(scores, arg = "scores", call = call)
  validate_item_uniqueness(scores, arg = "scores", unit = "score", call = call)
  validate_items_present(data, scores, arg = "scores", call = call)
  validate_range(srange, call = call)
  validate_string(prefix, arg = "prefix", call = call)
  validate_level(level, call = call)
  validate_flag(append, arg = "append", call = call)

  score_cols <- data[scores]
  col_names <- names(score_cols)
  stems <- strip_prefix(col_names, prefix)

  validate_numeric_columns(
    score_cols,
    headline = function(n) {
      cli::format_inline(
        "{cli::qty(n)}The {.arg scores} column{?s} must be numeric."
      )
    },
    info = function(n) {
      cli::format_inline(
        "A factor's integer codes are not its scores, and a character column coerces to {.code NA}, so neither is converted for you. Convert {cli::qty(n)}{?it/them} before computing an interval."
      )
    },
    call = call
  )

  ## Refuse an append that would collide with a column `data` already holds
  ## (D-045(a)). Every requested column gets all three suffixes whatever its
  ## coverage, so the output names follow from `col_names` alone. Placed before
  ## the two reports below so a colliding call is told about the collision
  ## alone, not about an unusable coding or an uncovered scale on the way to a
  ## result it will not return.
  if (append) {
    validate_no_output_collision(
      paste0(rep(col_names, each = 3L), c("_est", "_lo", "_hi")),
      data,
      call = call
    )
  }

  ## The reference mean and SD are printed on one response coding. A score
  ## computed on any other coding is a different quantity -- a shift moves the
  ## mean, a stretch moves both -- and no mapping onto these statistics is
  ## published, so nothing is converted.
  usable <- isTRUE(all.equal(as.numeric(srange), as.numeric(ref_srange)))
  if (!usable) {
    cli::cli_warn(
      c(
        "!" = "{.arg srange} {.code c({srange[[1]]}, {srange[[2]]})} is not the response coding {.code {dataset}} is printed on, {.code c({ref_srange[[1]]}, {ref_srange[[2]]})}.",
        "i" = "The reference mean and standard deviation belong to that coding, and no mapping from another one is published, so every {.code _est}, {.code _lo} and {.code _hi} column is returned as {.code NA}.",
        "i" = "Recode the items to {.code c({ref_srange[[1]]}, {ref_srange[[2]]})} and rescore before computing an interval."
      ),
      class = "hitop_interval_coding",
      call = call
    )
  }

  hit <- match(stems, refstats$camelCase)
  covered <- !is.na(hit)

  ## z for a two-sided interval at the requested level. Computed rather than
  ## tabulated so a non-default `level` is exact.
  z <- stats::qnorm(1 - (1 - level) / 2)

  out <- list()
  n <- nrow(data)
  for (i in seq_along(col_names)) {
    if (usable && covered[[i]]) {
      ref <- refstats[hit[[i]], ]
      x <- as.numeric(score_cols[[i]])
      est <- ref$mean + sqrt(ref$reliability) * (x - ref$mean)
      half <- z * ref$sd * sqrt(1 - ref$reliability)
      got <- list(est = est, lo = est - half, hi = est + half)
    } else {
      got <- list(
        est = rep(NA_real_, n),
        lo = rep(NA_real_, n),
        hi = rep(NA_real_, n)
      )
    }
    out[[paste0(col_names[[i]], "_est")]] <- got$est
    out[[paste0(col_names[[i]], "_lo")]] <- got$lo
    out[[paste0(col_names[[i]], "_hi")]] <- got$hi
  }

  ## An uncovered column still gets all three columns, filled with NA, so a
  ## missing interval is visible in the output rather than absent from it. Both
  ## reports are warning conditions, so one suppressWarnings() silences the
  ## function and either can be caught on its own (D-025's posture, D-044's
  ## classes). This one is raised whether or not the coding matched: the two say
  ## different things -- that this column has no reference row at all, and that
  ## no column is convertible on this coding -- so a caller hitting both is told
  ## both rather than one standing in for the other.
  if (any(!covered)) {
    uncovered <- col_names[!covered]
    cli::cli_warn(
      c(
        "!" = "{length(uncovered)} score column{?s} {?has/have} no row in {.code {dataset}}: {.val {uncovered}}.",
        "i" = "{cli::qty(length(uncovered))}{?Its/Their} {.code _est}, {.code _lo} and {.code _hi} columns are returned as {.code NA}. A column is matched by stripping {.arg prefix} and looking the rest up in {.code {dataset}$camelCase}."
      ),
      class = "hitop_interval_uncovered",
      call = call
    )
  }

  out <- as.data.frame(out, check.names = FALSE)
  if (append == TRUE) {
    out <- cbind(data, out)
  }
  tibble::as_tibble(out)
}
