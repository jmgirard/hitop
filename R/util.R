reverse <- function(x, low = NULL, high = NULL) {
  if (is.null(low)) {
    low <- min(x, na.rm = TRUE)
  }

  if (is.null(high)) {
    high <- max(x, na.rm = TRUE)
  }

  low + high - x
}

bind_columns <- function(vector_list) {
  do.call(cbind, vector_list)
}

adiff <- function(data, items, index) {
  abs(data[, items[[index, 2]]] - data[, items[[index, 3]]])
}

cli_assert <- function(condition, message) {
  if (!condition) {
    cli::cli_abort(message)
  }
}

validate_data <- function(x) {
  cli_assert(
    condition = is.data.frame(x),
    message = "The `data` argument must be a data frame."
  )
}

validate_items <- function(x, n) {
  cli_assert(
    condition = rlang::is_character(x, n = n) || rlang::is_integerish(x, n = n),
    message = "The `items` argument did not have the expected length."
  )
}

validate_scales <- function(x) {
  cli_assert(
    condition = rlang::is_character(x) || rlang::is_integerish(x),
    message = "The `scales` argument did not have the expected type."
  )
}

validate_range <- function(x) {
  cli_assert(
    condition = rlang::is_integerish(x, n = 2),
    message = "The `srange` argument must contain two integerish values."
  )
  cli_assert(
    condition = x[[2]] > x[[1]],
    message = "The second `srange` value must be greater than the first."
  )
}

drop_na <- function(x) {
  x[!is.na(x)]
}

calc_sem <- function(x) {
  xc <- x[!is.na(x)]
  stats::sd(xc) / sqrt(length(xc))
}

# Round half away from zero, matching the APA scoring key's "round to the
# nearest whole number" (base round() rounds half to even, e.g. round(2.5) = 2).
round_half_up <- function(x) {
  sign(x) * floor(abs(x) + 0.5)
}

# APA-compliant average score for one scale's item matrix (rows = respondents,
# columns = the items contributing to that scale, already reverse-keyed). Per the
# PID-5 scoring key: if more than 25% of a scale's items are unanswered, the
# score is not used (NA); otherwise the raw score is prorated to the full item
# count and rounded to the nearest whole number before averaging. With no missing
# items this reduces to the plain item mean.
apa_mean <- function(mat) {
  n <- ncol(mat)
  apply(mat, MARGIN = 1, FUN = function(x) {
    a <- sum(!is.na(x))
    if ((n - a) / n > 0.25) {
      return(NA_real_)
    }
    partial <- sum(x, na.rm = TRUE)
    round_half_up(partial * n / a) / n
  })
}
