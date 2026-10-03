# Maintainer-run characterization harness for the PID5BF+M version (M157).
#
# Adding `version = "BFPM"` must not move any output of the three existing PID-5
# forms. This script captures every call of the milestone's AC3 matrix from
# whichever checkout is passed as the first argument, and writes them to the RDS
# named by the second. Run it once against the commit the milestone branch was
# cut from and once against the branch, then compare the two files with
# identical(). This is the D-011 characterization-harness pattern of
# characterize_calc_se.R.
#
# Usage, from the package directory:
#   git worktree add /tmp/hitop-base <baseline-commit>
#   Rscript data-raw/characterize_bfpm.R /tmp/hitop-base /tmp/before.rds
#   Rscript data-raw/characterize_bfpm.R . /tmp/after.rds
#   Rscript -e 'a <- readRDS("/tmp/before.rds"); b <- readRDS("/tmp/after.rds");
#               stopifnot(identical(names(a), names(b)));
#               print(sum(mapply(identical, a, b[names(a)])))'
#
# The run is clean when the printed number equals length(a).
#
# Each entry holds the value the call returned and the class and message of
# every condition it signalled. `rename_pid5_items()` and `label_pid5()` report
# through warnings, so a changed report is a changed entry. `score_pid5()` with
# `calc_se = TRUE` signals its deprecation warning on every such call, so that
# warning is recorded like any other.
#
# The datasets come from the loaded checkout, so each run scores that
# checkout's own shipped data.

args <- commandArgs(trailingOnly = TRUE)
pkg_dir <- args[[1]]
out_rds <- args[[2]]

suppressMessages(pkgload::load_all(pkg_dir, quiet = TRUE, export_all = FALSE))
ns <- asNamespace("hitop")
pid_items <- get("pid_items", envir = ns)

# Run one call. Return its value, or its error, and the conditions it signalled.
capture_call <- function(fun, call_args) {
  conds <- list()
  value <- tryCatch(
    withCallingHandlers(
      do.call(fun, call_args),
      warning = function(cnd) {
        conds[[length(conds) + 1]] <<- list(class(cnd), conditionMessage(cnd))
        invokeRestart("muffleWarning")
      },
      message = function(cnd) {
        conds[[length(conds) + 1]] <<- list(class(cnd), conditionMessage(cnd))
        invokeRestart("muffleMessage")
      }
    ),
    error = function(cnd) list(error = class(cnd), message = conditionMessage(cnd))
  )
  list(value = value, conditions = conds)
}

# The four dataset pairings, each with the version it is scored as and its item
# column names.
forms <- list(
  list(data = "sim_pid5", version = "FULL", items = sprintf("pid5_%03d", 1:220)),
  list(data = "sim_pid5sf", version = "SF", items = sprintf("pid5sf_%03d", 1:100)),
  list(data = "ku_pid5sf", version = "SF", items = sprintf("pid5sf_%03d", 1:100)),
  list(data = "sim_pid5bf", version = "BF", items = sprintf("pid5bf_%02d", 1:25))
)

score_pid5 <- get("score_pid5", envir = ns)
reliability_pid5 <- get("reliability_pid5", envir = ns)
rename_pid5_items <- get("rename_pid5_items", envir = ns)
label_pid5 <- get("label_pid5", envir = ns)

results <- list()
for (f in forms) {
  dat <- get(f$data, envir = ns)

  # score_pid5(): missing x calc_se x append
  for (miss in c("apa", "available", "complete")) {
    for (se in c(TRUE, FALSE)) {
      for (app in c(TRUE, FALSE)) {
        key <- paste("score_pid5", f$data, f$version, miss, se, app, sep = "/")
        results[[key]] <- capture_call(score_pid5, list(
          data = dat, items = f$items, version = f$version,
          missing = miss, calc_se = se, append = app
        ))
      }
    }
  }

  # reliability_pid5(): alpha x omega
  for (a in c(TRUE, FALSE)) {
    for (o in c(TRUE, FALSE)) {
      key <- paste("reliability_pid5", f$data, f$version, a, o, sep = "/")
      results[[key]] <- capture_call(reliability_pid5, list(
        data = dat, items = f$items, version = f$version, alpha = a, omega = o
      ))
    }
  }

  # rename_pid5_items(): both matching methods. "number" reads columns spelled
  # with the default `from_prefix`, so the item columns are renamed to it
  # first; "text" matches each column to its item's text in `pid_items`.
  numbers <- seq_along(f$items)
  by_number <- dat[f$items]
  names(by_number) <- paste0("pid_", numbers)
  results[[paste("rename_pid5_items", f$data, f$version, "number", sep = "/")]] <-
    capture_call(rename_pid5_items, list(
      data = by_number, version = f$version, method = "number"
    ))
  form <- pid_items[!is.na(pid_items[[f$version]]), ]
  by_text <- dat[f$items]
  names(by_text) <- paste0("col_", numbers)
  results[[paste("rename_pid5_items", f$data, f$version, "text", sep = "/")]] <-
    capture_call(rename_pid5_items, list(
      data = by_text, version = f$version, method = "text",
      item_cols = names(by_text),
      item_text = form$Text[match(numbers, form[[f$version]])]
    ))

  # label_pid5(): both targets. Scale columns come from a default-prefix score.
  results[[paste("label_pid5", f$data, f$version, "items", sep = "/")]] <-
    capture_call(label_pid5, list(data = dat, target = "items", version = f$version))
  scored <- score_pid5(dat, items = f$items, version = f$version)
  results[[paste("label_pid5", f$data, f$version, "scales", sep = "/")]] <-
    capture_call(label_pid5, list(data = scored, target = "scales", version = f$version))
}

cat("configs:", length(results), "\n")
saveRDS(results, out_rds)
