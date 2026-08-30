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
# The datasets come from the loaded checkout, not from a fixture, so each run
# scores that version's own shipped data. Each function is paired only with the
# datasets it can score; `missing` and `append` are crossed over those pairings,
# and `score_hitopsr()` is run both on a full administration and through a
# module, which resolves a different item count, reverse-item set and scale map.
#
# Each entry records the returned value AND the classes of every condition the
# call signalled, less the `calc_se` deprecation the milestone adds -- the
# deprecation is meant to be the only new one, and a comparison that discarded
# the condition channel could not tell.

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
       args = list(items = sprintf("hsr%03d", 1:405)),
       missing = c("available", "complete")),
  list(fn = "score_hitopsr", data = "sim_hitopsr", module = TRUE,
       args = list(items = paste0("hsr_", 1:405)),
       missing = c("available", "complete")),
  list(fn = "score_hitopsr", data = "ku_hitopsr", module = TRUE,
       args = list(items = sprintf("hsr%03d", 1:405)),
       missing = c("available", "complete")),
  list(fn = "score_hitopbr", data = "sim_hitopbr",
       args = list(items = paste0("hitopbr_", 1:45)),
       missing = c("available", "complete")),
  list(fn = "score_hitopbr", data = "ku_hitopbr",
       args = list(items = sprintf("hbr%02d", 1:45)),
       missing = c("available", "complete"))
)

# The four scales used for the module runs: one carrying a reverse-keyed item
# and three that do not, the shape test-deprecated.R already exercises.
module_scales <- c(
  "Agoraphobia", "Appetite Loss", "Antisocial Behavior", "Romantic Disinterest"
)

# Run one call, returning both what it produced and the classes of every
# condition it signalled bar the deprecation itself.
capture_call <- function(fun, call_args) {
  seen <- character(0)
  value <- withCallingHandlers(
    do.call(fun, call_args),
    condition = function(cnd) {
      if (!inherits(cnd, "hitop_deprecated_calc_se")) {
        seen <<- c(seen, paste(class(cnd), collapse = "+"))
      } else if (inherits(cnd, "warning")) {
        invokeRestart("muffleWarning")
      }
    }
  )
  list(value = value, conditions = seen)
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
