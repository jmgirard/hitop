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

validate_data <- function(x) {
  stopifnot(is.data.frame(x))
}

validate_items <- function(x, n) {
  stopifnot(
    is.null(x) ||
      rlang::is_character(x, n = n) ||
      rlang::is_integerish(x, n = n)
  )
}

validate_id <- function(x) {
  stopifnot(
    is.null(x) ||
      rlang::is_character(x) ||
      rlang::is_integerish(x)
  )
}

validate_range <- function(x) {
  stopifnot(rlang::is_integerish(x, n = 2))
  stopifnot(x[[2]] > x[[1]])
}

drop_na <- function(x) {
  x[!is.na(x)]
}

calc_sem <- function(x) {
  xc <- x[!is.na(x)]
  sd(xc) / sqrt(length(xc))
}
