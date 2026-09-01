# Maintainer-run characterization harness for the `calc_se` deprecation (M069).
#
# Captures every `calc_se = TRUE` return value the milestone's AC2 matrix names,
# from whichever checkout is passed as the first argument, and writes them to
# the RDS named by the second. Run once against the commit the milestone branch
# was cut from and once against the branch; the two RDS files are then compared
# with identical(). This is the D-011 characterization-harness pattern: a
# deprecation adds a warning and must move no number.
#
# Usage, from any directory:
#   git worktree add /tmp/hitop-base <baseline-commit>
#   Rscript data-raw/characterize_calc_se.R /tmp/hitop-base /tmp/before.rds
#   Rscript data-raw/characterize_calc_se.R . /tmp/after.rds
#   Rscript -e 'a <- readRDS("/tmp/before.rds"); b <- readRDS("/tmp/after.rds");
#               stopifnot(identical(names(a), names(b)));
#               print(sum(mapply(identical, a, b[names(a)])))'
#
# Each element of either RDS is one call's returned value, so that comparison is
# a comparison of values and of nothing else. It prints the number of matching
# cells; the run is clean when that number is length(a).
#
# The datasets come from the loaded checkout, not from a fixture, so each run
# scores that version's own shipped data. Each function is paired only with the
# datasets it can score; `missing` and `append` are crossed over those pairings,
# and `score_hitopsr()` is run both on a full administration and through a
# module, which resolves a different item count, reverse-item set and scale map.
#
# Each entry is the value the call returned, and nothing else. It once also
# recorded the classes of every condition the call signalled bar the `calc_se`
# deprecation; that channel was deleted at M072 because it recorded nothing:
# on the 2026-08-30 run of this script over the whole matrix, all 48 cells
# captured an empty condition set, so a comparison of the channel could report
# an added condition but never a removed one, while the header claimed it
# covered both. What a run compares is the returned values.

`%||%` <- function(x, y) if (is.null(x)) y else x

args <- commandArgs(trailingOnly = TRUE)
pkg_dir <- args[[1]]
out_rds <- args[[2]]

suppressMessages(pkgload::load_all(pkg_dir, quiet = TRUE, export_all = FALSE))

# One entry per (function, dataset, version) pair AC2 admits, carrying the item
# selector that dataset needs. `missing` and `append` are crossed over all of
# these below.
cases <- list(
  list(fn = "score_pid5", data = "sim_pid5",
       args = list(items = paste0("pid_", 1:220), version = "FULL"),
       missing = c("apa", "available", "complete")),
  list(fn = "score_pid5", data = "sim_pid5sf",
       args = list(items = paste0("pid_", 1:100), version = "SF"),
       missing = c("apa", "available", "complete")),
  list(fn = "score_pid5", data = "ku_pid5sf",
       args = list(items = sprintf("pid_%d", 1:100), version = "SF"),
       missing = c("apa", "available", "complete")),
  list(fn = "score_pid5", data = "sim_pid5bf",
       args = list(items = paste0("pid_", 1:25), version = "BF"),
       missing = c("apa", "available", "complete")),
  list(fn = "score_hitopsr", data = "sim_hitopsr",
       args = list(items = paste0("hsr_", 1:405)),
       missing = c("available", "complete")),
  list(fn = "score_hitopsr", data = "ku_hitopsr",
       args = list(items = sprintf("hsr_%03d", 1:405)),
       missing = c("available", "complete")),
  list(fn = "score_hitopsr", data = "sim_hitopsr", module = TRUE,
       args = list(items = paste0("hsr_", 1:405)),
       missing = c("available", "complete")),
  list(fn = "score_hitopsr", data = "ku_hitopsr", module = TRUE,
       args = list(items = sprintf("hsr_%03d", 1:405)),
       missing = c("available", "complete")),
  list(fn = "score_hitopbr", data = "sim_hitopbr",
       args = list(items = paste0("hitopbr_", 1:45)),
       missing = c("available", "complete")),
  list(fn = "score_hitopbr", data = "ku_hitopbr",
       args = list(items = sprintf("hbr_%02d", 1:45)),
       missing = c("available", "complete"))
)

# The four scales used for the module runs: one carrying a reverse-keyed item
# and three that do not, the shape test-deprecated.R already exercises.
module_scales <- c(
  "Agoraphobia", "Appetite Loss", "Antisocial Behavior", "Romantic Disinterest"
)

# Run one call and return what it produced. The `calc_se` deprecation the
# milestone adds is muffled rather than recorded: every call here passes
# `calc_se = TRUE`, so it fires on all of them and says nothing about the
# values being compared.
capture_call <- function(fun, call_args) {
  withCallingHandlers(
    do.call(fun, call_args),
    hitop_deprecated_calc_se = function(cnd) {
      if (inherits(cnd, "warning")) invokeRestart("muffleWarning")
    }
  )
}

results <- list()
for (case in cases) {
  fun <- get(case$fn, envir = asNamespace("hitop"))
  dat <- get(case$data, envir = asNamespace("hitop"))
  extra <- list()
  if (isTRUE(case$module)) {
    build <- get("hitop_module", envir = asNamespace("hitop"))
    m <- build("hitopsr", module_scales)
    extra <- list(module = m)
    # A module is scored from the columns it names, in module order.
    dat <- dat[case$args$items[m$items]]
    case$args$items <- names(dat)
  }
  for (miss in case$missing) {
    for (app in c(TRUE, FALSE)) {
      key <- paste(case$fn, case$data, if (isTRUE(case$module)) "module" else "full",
                   case$args$version %||% "-", miss, app, sep = "/")
      call_args <- c(
        list(data = dat), case$args, extra,
        list(missing = miss, append = app, calc_se = TRUE)
      )
      results[[key]] <- capture_call(fun, call_args)
    }
  }
}

cat("configs:", length(results), "\n")
saveRDS(results, out_rds)
