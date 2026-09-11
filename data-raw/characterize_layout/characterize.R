# Characterization of the `layout` argument's default: record every value
# score_hitopsr() and reliability_hitopsr() return while the merge-base test
# files run, at one package source tree. Run once per tree, then compare the
# two .rds files with compare.R (every value identical()).
#
# Preparing the two trees, from the repo root, into a scratch directory <S>:
#   BASE=$(git merge-base origin/main HEAD)
#   mkdir -p <S>/base <S>/head
#   git archive $BASE | tar -x -C <S>/base
#   rsync -a --exclude .git --exclude tests --exclude cairn ./ <S>/head/
#   cp -R <S>/base/tests <S>/head/tests    # both runs use the base test files
# Enumerating the call sites the runs exercise:
#   grep -rn 'score_hitopsr(\|reliability_hitopsr(' <S>/base/tests/testthat/
#
# Usage: Rscript characterize.R <pkg-dir> <out.rds> [<test filter>]
args <- commandArgs(trailingOnly = TRUE)
pkg <- args[[1]]
out <- args[[2]]

devtools::load_all(pkg, quiet = TRUE)
ns <- asNamespace("hitop")
log <- new.env()
log$records <- list()

wrap <- function(name) {
  original <- get(name, envir = ns)
  wrapped <- function(...) {
    cond <- NULL
    value <- tryCatch(original(...), error = function(e) { cond <<- e; NULL })
    record <- if (is.null(cond)) {
      list(fn = name, value = value)
    } else {
      list(fn = name, error = conditionMessage(cond), class = class(cond))
    }
    log$records[[length(log$records) + 1L]] <- record
    if (!is.null(cond)) stop(cond) # re-signal the original condition intact
    value
  }
  for (env in list(ns, as.environment("package:hitop"))) {
    if (exists(name, envir = env, inherits = FALSE)) {
      if (bindingIsLocked(name, env)) unlockBinding(name, env)
      assign(name, wrapped, envir = env)
      lockBinding(name, env)
    }
  }
}
wrap("score_hitopsr")
wrap("reliability_hitopsr")

# test_dir() with load_package = "none": devtools::test() would load_all() again
# and replace the wrapped bindings with the originals. The summary reporter
# keeps the console short; failures are not the point here and never stop the
# run.
testthat::set_max_fails(Inf)
suppressWarnings(testthat::test_dir(
  file.path(pkg, "tests", "testthat"),
  package = "hitop",
  load_package = "none",
  reporter = "summary",
  stop_on_failure = FALSE,
  filter = if (length(args) >= 3L) args[[3]] else NULL # smoke runs only
))

saveRDS(log$records, out)
cat("recorded", length(log$records), "calls to", out, "\n")
