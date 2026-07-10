#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @import rlang
#' @importFrom stats setNames
#' @importFrom utils write.csv
## usethis namespace: end
NULL

# Package datasets referenced by name inside functions (lazy-loaded); declared
# here so R CMD check does not flag them as undefined global variables.
utils::globalVariables(c(
  "pid_items",
  "hitopsr_items",
  "hitopsr_scales",
  "hitopsr_subscales",
  "hitopbr_items",
  "hitopbr_scales",
  "hitophsum_items",
  "hitophsum_choices"
))
