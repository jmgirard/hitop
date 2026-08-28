#' Confidence intervals for HiTOP-SR scale scores
#'
#' Converts scored HiTOP-SR columns into a regression-based true-score estimate
#' and a confidence interval around it, using the development-sample mean,
#' standard deviation and reliability shipped as [hitopsr_devstats]. Scores are
#' produced by [score_hitopsr()]; this function converts them and never rescores.
#'
#' @param data A data frame containing scored HiTOP-SR columns.
#' @param scores The score columns to convert, as column names or column
#'   positions (mirroring the `items` argument of [score_hitopsr()]). Each column
#'   must be numeric (or logical) and each may be named only once.
#' @param srange The response range the items were scored on, as
#'   `c(low, high)`. The reference statistics are printed on the official
#'   four-option `c(1, 4)` coding; any other coding is a different metric. See
#'   Details.
#' @param prefix The prefix [score_hitopsr()] applied to its output columns, used
#'   to match a score column back to its scale. Matched literally, not as a
#'   regular expression: a column name that does not begin with exactly this
#'   string keeps its whole name and is reported as uncovered. Pass `""` when the
#'   columns are named for the scales themselves, with no prefix to strip.
#' @param level The confidence level, as a proportion between 0 and 1. Defaults
#'   to `0.95`.
#' @param append Whether to return the input `data` with the interval columns
#'   appended (`TRUE`, the default) or the interval columns alone.
#'
#' @details **What is computed.** For a score \eqn{x} on a scale whose reference
#'   group has mean \eqn{M}, standard deviation \eqn{SD} and reliability
#'   \eqn{r}, the estimate and bounds are Schmukle's (2026) Equations (10) to
#'   (12), p. 821:
#'
#'   \deqn{est = M + \sqrt{r}\,(x - M)}
#'   \deqn{SEM = SD \sqrt{1 - r}}
#'   \deqn{lo,\ hi = est \pm z\,SEM}
#'
#'   where \eqn{z} is the two-sided standard normal quantile for `level`
#'   (1.959964 at the default 0.95). The estimate is the observed score pulled
#'   toward the reference mean, because with imperfect measurement a true score
#'   tends to lie nearer the mean than the observed score does; the
#'   \eqn{\sqrt{r}} factor is Schmukle's scale correction, which returns the
#'   estimate to the metric the observed score is on so the two can be read
#'   against each other.
#'
#'   **The reference group is a development sample.** [hitopsr_devstats] carries
#'   the statistics printed for the HiTOP-SR introduction paper's Development
#'   Sample 2: N = 780 Prolific Academic participants stratified by sex and age
#'   to approximate a community-representative United States population. It is
#'   not a community norm -- no census weighting was applied and no raw-score to
#'   T-score table is published -- so an interval from this function says where a
#'   score sits relative to the sample the instrument was developed on, and not
#'   what percentile it occupies in any population.
#'
#'   **Two limitations worth stating.** Equation (12)'s interval is symmetric
#'   about the estimate and the same width for every respondent on a given scale,
#'   which is what classical test theory implies and what the source computes. On
#'   a strongly skewed scale that width can put a bound outside the response
#'   range -- the floor of `hsr_conversionSymptoms` is one such case -- and the
#'   bounds are **not** clamped, because clamping would report something other
#'   than the equation this function cites. And the coverage Schmukle
#'   demonstrates is *marginal*: over a population of respondents drawn from the
#'   reference distribution, about `level` of the intervals contain the true
#'   score. It is established under a linear measurement model with
#'   approximately normal item responses, so it is not a guarantee for any one
#'   respondent, nor a demonstration on scales as skewed as the rarer HiTOP-SR
#'   ones.
#'
#'   **Reporting and silence.** Both things this function reports are warning
#'   conditions, so a single `suppressWarnings()` call silences it and either
#'   report can still be caught and tested for on its own.
#'
#'   * A score column with no [hitopsr_devstats] row returns `NA` in all three
#'     columns, with a warning of class `hitop_interval_uncovered` naming the
#'     columns. A column is matched by stripping `prefix` and looking the rest up
#'     in `hitopsr_devstats$camelCase`, so this fires for a mistyped prefix as
#'     well as for a scale the table does not carry.
#'   * A call whose `srange` is not the `c(1, 4)` coding the reference mean and
#'     standard deviation are printed on returns `NA` in every interval column,
#'     with a warning of class `hitop_interval_coding`. Nothing is reconciled: a
#'     shift or a stretch of the response range moves a scale score, and no
#'     mapping from another coding onto these statistics is published.
#'
#'   **Partly scored scales are not detected.** A score computed from fewer than
#'   a scale's full items -- from a module form, or from data with items missing
#'   under `missing = "available"` -- is not on the same footing as the reference
#'   statistics, which come from complete scales. This function receives scores
#'   and not items, so it cannot tell such a column from a fully scored one and
#'   does not try: it converts what it is given. Treat an interval on a partly
#'   scored scale as not comparable to the reference group.
#'
#'   **Errors.** `scores` is checked before anything is converted. Naming the
#'   same score column twice is an error rather than a duplicated set of output
#'   columns, and a factor or character score column is an error rather than a
#'   silent coercion -- a factor's integer codes are not its scores, and a
#'   character column would coerce to `NA`. Logical columns are accepted.
#'
#' @return A \link[tibble]{tibble} with an `_est`, `_lo` and `_hi` column for
#'   every requested score column, alongside all original `data` columns if
#'   requested. An `NA` score returns `NA` in all three.
#'
#' @references Schmukle, S. C. (2026). Unbiased confidence intervals for
#'   psychological testing: The regression-based true score approach with scale
#'   correction. *Assessment, 33*(5), 817-825. Equations (10) to (12), p. 821,
#'   are what this function computes; Box 1, p. 823, works two examples.
#'
#' @examples
#' # Score the HiTOP-SR, then put an interval around two scales
#' scored <- score_hitopsr(sim_hitopsr, items = 1:405)
#' interval_hitopsr(
#'   scored,
#'   scores = c("hsr_agoraphobia", "hsr_wellBeing"),
#'   append = FALSE
#' )
#'
#' # A 90% interval is narrower
#' interval_hitopsr(
#'   scored,
#'   scores = "hsr_agoraphobia",
#'   level = 0.90,
#'   append = FALSE
#' )
#'
#' @export
interval_hitopsr <- function(
  data,
  scores,
  srange = c(1, 4),
  prefix = "hsr_",
  level = 0.95,
  append = TRUE
) {
  interval_engine(
    data = data,
    scores = scores,
    refstats = hitopsr_devstats,
    ## The coding Table 1's Range column prints, and the coding
    ## score_hitopsr() defaults to.
    ref_srange = c(1, 4),
    srange = srange,
    prefix = prefix,
    level = level,
    append = append,
    dataset = "hitopsr_devstats",
    call = rlang::current_env()
  )
}
