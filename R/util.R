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
