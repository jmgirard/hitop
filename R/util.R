reverse <- function(x, min = NULL, max = NULL) {

  if (is.null(min)) {
    min <- min(x, na.rm = TRUE)
  }

  if (is.null(max)) {
    max <- max(x, na.rm = TRUE)
  }

  min + max - x
}

bind_columns <- function(vector_list) {
  do.call(cbind, vector_list)
}

adiff <- function(df, x, y) {
  abs(df[, x] - df[, y])
}

highest <- function(df, x, range = c(0, 3)) {
  as.integer(df[, x] == range[[2]])
}


validate_data <- function(x) {
  stopifnot(is.data.frame(x))
}

validate_items <- function(x) {
  stopifnot(
    is.null(x) ||
      rlang::is_character(x, n = 220) ||
      rlang::is_integerish(x, n = 220)
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
