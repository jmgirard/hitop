# Characterization harness for M060 (AC3)
#
# M060 adds two refusals to the seven exports carrying an `append` formal and
# changes no arithmetic. This script is the evidence for that claim: it runs one
# call per export against the branch and against the branch's merge base, and
# diffs the two recordings.
#
# Each call is recorded as its *value and its condition together*. A value-only
# probe is blind in exactly the dimension this milestone moves -- a call that
# used to be accepted and is now refused returns no value at all -- so the record
# carries the returned object, every condition signalled, and whether the call
# aborted (the M031/M037 lesson).
#
# The two sides run in separate R subprocesses against two package trees: the
# working tree as it stands, and a `git archive` of the merge base unpacked into
# a temp dir. Neither process loads the other's namespace. The comparison is a
# diff of two RDS recordings, so the branch side never derives its expectation
# from the branch.
#
# Domain: `getNamespaceExports("hitop")` filtered on an `append` formal, the same
# sweep AC1's test runs. The script aborts if that sweep comes back empty or if
# any enumerated export has no probe -- a silently-emptying domain would let this
# script pass over nothing.
#
# Proven able to fail, 2026-08-28, one planted defect at a time against that
# day's working tree, with a clean control before and after: a changed default
# (`rank_scales()`'s `top` 5 -> 4) was reported as a changed value; a planted
# `stop()` in `score_hitopbr()` was reported on all three dimensions at once
# (verdict, value, conditions); a planted `warning()` in `score_hitopsr()`,
# which leaves the returned value identical, was reported on the conditions
# dimension alone -- so the condition half is not carried by the value half.
# A deleted probe was reported as "No probe for: rank_scales" rather than
# quietly comparing six calls. Each defect exited 1; both controls exited 0.
#
# What this script cannot do: it needs a git checkout, so it never runs under
# `R CMD check`, which builds from a source tarball with no repository. It is
# maintainer-run, per the 2026-08-28 M060 implementation gate.
#
# Run from the package root:  Rscript data-raw/verify_m060_characterization.R

## ---- the probe definition, shared by both sides -----------------------------

## Written to a temp file and sourced by each subprocess after it has loaded its
## own copy of the package, so both sides run byte-identical probe code.
probe_source <- '
enumerate_append_exports <- function() {
  ns <- asNamespace("hitop")
  nms <- sort(getNamespaceExports("hitop"))
  keep <- vapply(
    nms,
    function(n) {
      obj <- get(n, envir = ns)
      is.function(obj) && "append" %in% names(formals(obj))
    },
    logical(1)
  )
  nms[keep]
}

## Record a call as value + conditions + aborted-flag. Warnings are collected
## rather than caught, so a call that both warns and returns records both.
record_call <- function(expr) {
  conds <- list()
  aborted <- FALSE
  value <- NULL
  withCallingHandlers(
    tryCatch(
      value <- force(expr),
      error = function(e) {
        aborted <<- TRUE
        conds[[length(conds) + 1L]] <<- list(
          type = "error",
          class = class(e),
          message = conditionMessage(e)
        )
      }
    ),
    warning = function(w) {
      conds[[length(conds) + 1L]] <<- list(
        type = "warning",
        class = class(w),
        message = conditionMessage(w)
      )
      invokeRestart("muffleWarning")
    },
    message = function(m) {
      conds[[length(conds) + 1L]] <<- list(
        type = "message",
        class = class(m),
        message = conditionMessage(m)
      )
      invokeRestart("muffleMessage")
    }
  )
  list(aborted = aborted, value = value, conditions = conds)
}

## The seven calls AC3 enumerates: the four exports that read item columns, on
## their instrument\'s sim_* dataset; the three conversion exports, on that
## dataset\'s score_*() output.
build_probes <- function() {
  pid <- hitop::sim_pid5
  sr <- hitop::sim_hitopsr
  br <- hitop::sim_hitopbr

  ## The conversion inputs are built with warnings muffled: they are setup, not
  ## part of any recording.
  scored_pid <- suppressWarnings(
    hitop::score_pid5(pid, items = 1:220, append = FALSE)
  )
  scored_sr <- suppressWarnings(
    hitop::score_hitopsr(sr, items = 1:405, append = FALSE)
  )

  list(
    score_pid5 = function() {
      hitop::score_pid5(pid, items = 1:220)
    },
    score_hitopsr = function() {
      hitop::score_hitopsr(sr, items = 1:405)
    },
    score_hitopbr = function() {
      hitop::score_hitopbr(br, items = 1:45)
    },
    validity_pid5 = function() {
      hitop::validity_pid5(pid, items = 1:220)
    },
    norm_pid5 = function() {
      hitop::norm_pid5(scored_pid, scores = names(scored_pid))
    },
    rank_scales = function() {
      hitop::rank_scales(scored_sr, scales = names(scored_sr), prefix = "hsr_")
    },
    interval_hitopsr = function() {
      hitop::interval_hitopsr(scored_sr, scores = names(scored_sr))
    }
  )
}

run_probes <- function() {
  exports <- enumerate_append_exports()
  if (length(exports) == 0L) {
    stop("The append-formal sweep enumerated no exports.", call. = FALSE)
  }
  probes <- build_probes()
  missing <- setdiff(exports, names(probes))
  if (length(missing) > 0L) {
    stop(
      "No probe for: ", paste(missing, collapse = ", "),
      call. = FALSE
    )
  }
  extra <- setdiff(names(probes), exports)
  if (length(extra) > 0L) {
    stop(
      "Probe for a non-enumerated export: ", paste(extra, collapse = ", "),
      call. = FALSE
    )
  }
  out <- lapply(exports, function(n) record_call(probes[[n]]()))
  names(out) <- exports
  out
}
'

## ---- run one side in its own subprocess -------------------------------------

probe_file <- tempfile(fileext = ".R")
writeLines(probe_source, probe_file)

run_side <- function(pkg_dir, out_rds, label) {
  script <- tempfile(fileext = ".R")
  writeLines(
    c(
      sprintf('suppressMessages(pkgload::load_all(%s, quiet = TRUE))', shQuote(pkg_dir)),
      sprintf('source(%s)', shQuote(probe_file)),
      'rec <- run_probes()',
      sprintf('saveRDS(rec, %s)', shQuote(out_rds)),
      'cat("probes recorded:", length(rec), "\\n")'
    ),
    script
  )
  cat("--- ", label, " (", pkg_dir, ")\n", sep = "")
  status <- system2("Rscript", script, stdout = "", stderr = "")
  if (status != 0L) {
    stop("The ", label, " side exited ", status, ".", call. = FALSE)
  }
  readRDS(out_rds)
}

## ---- the two package trees --------------------------------------------------

branch <- system2("git", c("rev-parse", "--abbrev-ref", "HEAD"), stdout = TRUE)
default_ref <- "origin/HEAD"
base_sha <- system2(
  "git",
  c("merge-base", "HEAD", default_ref),
  stdout = TRUE
)
cat("Branch:     ", branch, "\n", sep = "")
cat("Merge base: ", base_sha, "\n\n", sep = "")

base_dir <- file.path(tempdir(), paste0("m060-base-", substr(base_sha, 1, 8)))
dir.create(base_dir, showWarnings = FALSE)
tarball <- file.path(tempdir(), "m060-base.tar")
system2("git", c("archive", "--format=tar", "-o", tarball, base_sha))
utils::untar(tarball, exdir = base_dir)

base <- run_side(base_dir, tempfile(fileext = ".rds"), "merge base")
head <- run_side(".", tempfile(fileext = ".rds"), "branch")

## ---- compare ----------------------------------------------------------------

cat("\n")
if (!identical(names(base), names(head))) {
  cat("The enumerated export set differs between the two sides.\n")
  cat("  merge base: ", paste(names(base), collapse = ", "), "\n", sep = "")
  cat("  branch:     ", paste(names(head), collapse = ", "), "\n", sep = "")
  quit(status = 1L)
}

cond_signature <- function(rec) {
  if (length(rec$conditions) == 0L) {
    return(character(0))
  }
  vapply(
    rec$conditions,
    function(c) paste0(c$type, ": ", c$message),
    character(1)
  )
}

differ <- character(0)
for (nm in names(base)) {
  b <- base[[nm]]
  h <- head[[nm]]
  problems <- character(0)
  if (!identical(b$aborted, h$aborted)) {
    problems <- c(
      problems,
      sprintf(
        "accept/reject verdict changed (merge base aborted = %s, branch = %s)",
        b$aborted, h$aborted
      )
    )
  }
  if (!identical(b$value, h$value)) {
    problems <- c(problems, "returned value changed")
  }
  bs <- cond_signature(b)
  hs <- cond_signature(h)
  if (!identical(bs, hs)) {
    problems <- c(
      problems,
      sprintf(
        "conditions changed (%d -> %d):\n      merge base: %s\n      branch:     %s",
        length(bs), length(hs),
        paste(bs, collapse = " | "), paste(hs, collapse = " | ")
      )
    )
  }
  if (length(problems) > 0L) {
    differ <- c(differ, nm)
    cat("DIFF  ", nm, "\n", sep = "")
    for (p in problems) cat("    ", p, "\n", sep = "")
  } else {
    cat(
      "same  ", nm, " (",
      if (b$aborted) "aborted" else sprintf("%d columns", ncol(b$value)),
      ", ", length(cond_signature(b)), " condition",
      if (length(cond_signature(b)) == 1L) "" else "s", ")\n",
      sep = ""
    )
  }
}

cat("\n", length(names(base)), " call", if (length(names(base)) == 1L) "" else "s",
    " compared; ", length(differ), " differ", if (length(differ) == 1L) "s" else "",
    ".\n", sep = "")
if (length(differ) > 0L) {
  quit(status = 1L)
}
cat("No returned value and no accept/reject verdict changed (AC3).\n")
