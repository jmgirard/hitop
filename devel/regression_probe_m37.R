# AC7 differential regression probe: with `subset = NULL`, does the
# full-instrument path of score_hitopsr()/reliability_hitopsr() return exactly
# what it returned before this milestone, and abort with exactly the same
# conditions?
#
# Unlike M031's acceptance probe — which compared accept-vs-reject only, because
# that milestone meant to change the messages — this one compares the returned
# VALUES and the full condition objects (class, message, and the function the
# abort is attributed to), because M037 means to change neither.
#
#   Rscript devel/regression_probe_m37.R            # driver: base vs. working tree
#   Rscript devel/regression_probe_m37.R <pkg> <out.rds>   # worker, one tree
#
# The driver exports the branch's merge-base with the default branch to a temp
# dir via `git archive` (never a checkout, which would move HEAD under anything
# else reading the tree — M034) and runs the worker in a separate R subprocess
# per tree, so the two package versions never share a namespace.

args <- commandArgs(trailingOnly = TRUE)

# ---- worker ---------------------------------------------------------------

probe_tree <- function(pkg, out) {
  suppressMessages(devtools::load_all(pkg, quiet = TRUE))

  # Record a call's full outcome: its value, or the condition it raised. A
  # condition is reduced to the three things a caller can observe — class,
  # message, and the function blamed — because the condition object also
  # carries environments that differ between two loads of the same source.
  outcome <- function(expr) {
    cnd <- NULL
    val <- withCallingHandlers(
      tryCatch(force(expr), error = function(e) {
        cnd <<- e
        NULL
      }),
      warning = function(w) {
        cnd <<- w
        invokeRestart("muffleWarning")
      },
      message = function(m) invokeRestart("muffleMessage")
    )
    if (is.null(cnd)) {
      return(list(kind = "value", value = val))
    }
    list(
      kind = "condition",
      class = class(cnd),
      message = conditionMessage(cnd),
      call = if (is.null(conditionCall(cnd))) {
        NA_character_
      } else {
        rlang::call_name(conditionCall(cnd))
      }
    )
  }

  res <- list()
  rec <- function(key, v) res[[key]] <<- v

  dat <- sim_hitopsr
  nms <- names(dat)

  ## --- score_hitopsr(): missing x calc_se x append x items-as-{names,positions}
  for (miss in c("available", "complete")) {
    for (se in c(TRUE, FALSE)) {
      for (app in c(TRUE, FALSE)) {
        for (form in c("positions", "names")) {
          it <- if (form == "positions") 1:405 else nms
          rec(
            sprintf("score/%s/se=%s/append=%s/%s", miss, se, app, form),
            outcome(score_hitopsr(
              dat, items = it, missing = miss, calc_se = se, append = app
            ))
          )
        }
      }
    }
  }

  ## --- score_hitopsr(): a non-default srange and prefix
  rec("score/srange_0_3", outcome(
    score_hitopsr(dat, items = 1:405, srange = c(0, 3), append = FALSE)
  ))
  rec("score/prefix_empty", outcome(
    score_hitopsr(dat, items = 1:405, prefix = "", append = FALSE)
  ))

  ## --- reliability_hitopsr(): alpha x omega
  for (a in c(TRUE, FALSE)) {
    for (o in c(TRUE, FALSE)) {
      rec(
        sprintf("reliability/alpha=%s/omega=%s", a, o),
        outcome(reliability_hitopsr(dat, items = 1:405, alpha = a, omega = o))
      )
    }
  }

  ## --- error and warning conditions on both wrappers
  bad <- list(
    data_not_df = quote(score_hitopsr(1:405, items = 1:405)),
    items_short = quote(score_hitopsr(dat, items = 1:404)),
    items_long = quote(score_hitopsr(dat, items = 1:406)),
    items_dup = quote(score_hitopsr(dat, items = c(1, 1, 3:405))),
    items_absent = quote(score_hitopsr(dat, items = paste0("nope_", 1:405))),
    items_oob = quote(score_hitopsr(dat, items = 2:406)),
    items_type = quote(score_hitopsr(dat, items = as.list(1:405))),
    items_misordered = quote(score_hitopsr(dat, items = nms[c(2, 1, 3:405)])),
    srange_one = quote(score_hitopsr(dat, items = 1:405, srange = 1)),
    srange_rev = quote(score_hitopsr(dat, items = 1:405, srange = c(4, 1))),
    prefix_num = quote(score_hitopsr(dat, items = 1:405, prefix = 1)),
    prefix_len2 = quote(score_hitopsr(dat, items = 1:405, prefix = c("a", "b"))),
    calc_se_na = quote(score_hitopsr(dat, items = 1:405, calc_se = NA)),
    calc_se_num = quote(score_hitopsr(dat, items = 1:405, calc_se = 1)),
    append_str = quote(score_hitopsr(dat, items = 1:405, append = "yes")),
    missing_bad = quote(score_hitopsr(dat, items = 1:405, missing = "apa")),
    rel_data = quote(reliability_hitopsr(1:405, items = 1:405)),
    rel_items_short = quote(reliability_hitopsr(dat, items = 1:404)),
    rel_alpha_na = quote(reliability_hitopsr(dat, items = 1:405, alpha = NA)),
    rel_omega_num = quote(reliability_hitopsr(dat, items = 1:405, omega = 2)),
    rel_srange = quote(reliability_hitopsr(dat, items = 1:405, srange = "a"))
  )
  for (nm in names(bad)) {
    rec(paste0("cnd/", nm), outcome(eval(bad[[nm]])))
  }

  saveRDS(res, out)
  cat("probed", length(res), "cells\n")
}

if (length(args) == 2L) {
  probe_tree(args[[1]], args[[2]])
  quit(save = "no")
}

# ---- driver ---------------------------------------------------------------

sh <- function(...) {
  out <- system2(..., stdout = TRUE, stderr = TRUE)
  if (!is.null(attr(out, "status")) && attr(out, "status") != 0L) {
    stop(paste(out, collapse = "\n"))
  }
  # Invisible so the top-level calls below don't auto-print a command's output
  # (or an empty character(0)) into the middle of the report.
  invisible(out)
}

default_branch <- sub(
  "^origin/", "",
  sh("git", c("symbolic-ref", "--short", "refs/remotes/origin/HEAD"))
)
base <- sh("git", c("merge-base", default_branch, "HEAD"))
cat("base ref:", base, "(merge-base with", paste0(default_branch, ")\n"))

tmp <- tempfile("m37base")
dir.create(tmp)
on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
tar <- file.path(tmp, "base.tar")
sh("git", c("archive", "-o", shQuote(tar), base))
utils::untar(tar, exdir = tmp)

this <- file.path(R.home("bin"), "Rscript")
# This file's own path, so the worker subprocesses run THIS script (the
# branch's), never the base tree's copy of it — the probe grid must be the
# same on both sides or the comparison is meaningless.
script <- normalizePath(sub(
  "^--file=", "",
  grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)[[1]]
))
old_rds <- file.path(tmp, "old.rds")
new_rds <- file.path(tmp, "new.rds")

cat("--- probing base ---\n")
cat(sh(this, c(shQuote(script), shQuote(tmp), shQuote(old_rds))), sep = "\n")
cat("--- probing working tree ---\n")
cat(sh(this, c(shQuote(script), shQuote(normalizePath(".")), shQuote(new_rds))), sep = "\n")

old <- readRDS(old_rds)
new <- readRDS(new_rds)

stopifnot(identical(names(old), names(new)))
same <- vapply(names(old), function(k) identical(old[[k]], new[[k]]), logical(1))

cat("\ncells compared:", length(same), "\n")
cat("identical:     ", sum(same), "\n")
if (all(same)) {
  cat("RESULT: the full-instrument path is unchanged.\n")
} else {
  cat("RESULT: DIFFERENCES in", sum(!same), "cell(s):\n")
  for (k in names(same)[!same]) {
    cat("  *", k, "\n")
    print(all.equal(old[[k]], new[[k]]))
  }
  quit(save = "no", status = 1L)
}
