# Characterization harness for M061 (AC6)
#
# M061 changes where the reliability_*() family gets each scale's printed name:
# it reads the canonical name from the keying table instead of rebuilding one
# from the camelCase stem. It also renames the column that carries that name.
# Nothing else about the family's output is meant to move. This script is the
# evidence for that claim: it runs the whole argument matrix against the branch
# and against the branch's merge base, and diffs the two recordings with the
# display-name column set aside.
#
# Each call is recorded as its *value and its conditions together*. A value-only
# probe is blind to a call that gains or loses a warning while returning the same
# numbers, which is one of the ways this change could go wrong (the M031/M037
# lesson, carried forward from data-raw/verify_m060_characterization.R).
#
# The two sides run in separate R subprocesses against two package trees: the
# working tree as it stands, and a `git archive` of the merge base unpacked into
# a temp dir. Neither process loads the other's namespace, and neither side's
# recording is reused from a previous run -- both are recomputed every time, so
# there is no cached recording that could go stale (2026-08-28 implementation
# gate, over reusing the merge-base side).
#
# The display-name column is the one thing that is SUPPOSED to change, so it is
# lifted out of every recorded value before the diff and reported separately.
# The column is named `scale` at the merge base and `Scale` on the branch, so it
# is located by either name rather than by position.
#
# Domain: `getNamespaceExports("hitop")` filtered to the reliability family, the
# same sweep AC1's test runs. The script aborts if that sweep comes back empty,
# if any enumerated export has no probe, if a probe names a non-enumerated
# export, or if the built matrix is not the size its own dimensions imply -- a
# silently-emptying domain would let this script pass over nothing.
#
# The argument matrix (AC6): six instrument paths (PID-5 FULL, SF and BF;
# HiTOP-SR full instrument; HiTOP-SR module; HiTOP-BR) x `alpha` on/off x `omega`
# on/off x two `srange` values = 48 cells per side.
#
# Proven able to fail, 2026-08-28, one planted defect at a time against that day's
# working tree, each with a clean control run before and after (four controls, all
# 48 cells same, all exiting 0). The three defects differ in FORM, not only in
# location, and each was reported on its own dimension alone:
#   1. A changed returned number -- `out$alpha` shifted by 1e-6 in
#      reliability_engine() -- was reported as "returned value changed" on exactly
#      the 24 `alpha = TRUE` cells and on the conditions dimension nowhere.
#   2. An added condition that leaves every returned value identical -- a
#      `warning()` before the engine's return -- was reported as "conditions
#      changed" on all 48 cells and as a value change on none, so the condition
#      half of the recording is not carried by the value half.
#   3. A deleted probe -- the HiTOP-BR path removed from build_paths() -- was
#      reported as "No probe for: reliability_hitopbr" rather than quietly
#      comparing the 40 remaining cells.
# Each defect run exited 1.
#
# What this script cannot do: it needs a git checkout, so it never runs under
# `R CMD check`, which builds from a source tarball with no repository. It is
# maintainer-run. It also needs {lavaan} installed, since half the matrix asks
# for omega; without it every omega cell would be NA on both sides and the diff
# would be blind to omega entirely, so the script refuses to run without it.
#
# Run from the package root:  Rscript data-raw/verify_m061_characterization.R

## ---- the probe definition, shared by both sides -----------------------------

## Written to a temp file and sourced by each subprocess after it has loaded its
## own copy of the package, so both sides run byte-identical probe code.
probe_source <- '
## The reliability family, read off the namespace rather than typed out.
enumerate_reliability_exports <- function() {
  ns <- asNamespace("hitop")
  nms <- sort(getNamespaceExports("hitop"))
  keep <- vapply(
    nms,
    function(n) {
      obj <- get(n, envir = ns)
      is.function(obj) &&
        grepl("^reliability_", n) &&
        all(c("alpha", "omega", "srange") %in% names(formals(obj)))
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

## The six instrument paths. Each entry knows which export it exercises, what its
## default srange is, and how to make the call for a given srange and flags.
## The module path uses a fixed three-scale module and the columns that module
## collects, selected by NAME (its `items` are instrument numbers, which are
## column positions only in a frame that is exactly the 405 items in order).
build_paths <- function() {
  pid <- hitop::sim_pid5
  sr <- hitop::sim_hitopsr
  br <- hitop::sim_hitopbr

  m <- hitop::hitop_module(
    "hitopsr",
    scales = c("Agoraphobia", "Appetite Loss", "Binge Eating")
  )
  collected <- sr[paste0("hsr_", m$items)]

  list(
    list(
      key = "pid5_FULL", export = "reliability_pid5", sranges = list(c(0, 3), c(0, 4)),
      call = function(srange, alpha, omega) {
        hitop::reliability_pid5(
          pid, items = 1:220, version = "FULL",
          srange = srange, alpha = alpha, omega = omega
        )
      }
    ),
    list(
      key = "pid5_SF", export = "reliability_pid5", sranges = list(c(0, 3), c(0, 4)),
      call = function(srange, alpha, omega) {
        hitop::reliability_pid5(
          pid, items = 1:100, version = "SF",
          srange = srange, alpha = alpha, omega = omega
        )
      }
    ),
    list(
      key = "pid5_BF", export = "reliability_pid5", sranges = list(c(0, 3), c(0, 4)),
      call = function(srange, alpha, omega) {
        hitop::reliability_pid5(
          pid, items = 1:25, version = "BF",
          srange = srange, alpha = alpha, omega = omega
        )
      }
    ),
    list(
      key = "hitopsr_full", export = "reliability_hitopsr", sranges = list(c(1, 4), c(1, 5)),
      call = function(srange, alpha, omega) {
        hitop::reliability_hitopsr(
          sr, items = 1:405,
          srange = srange, alpha = alpha, omega = omega
        )
      }
    ),
    list(
      key = "hitopsr_module", export = "reliability_hitopsr", sranges = list(c(1, 4), c(1, 5)),
      call = function(srange, alpha, omega) {
        hitop::reliability_hitopsr(
          collected, items = names(collected), module = m,
          srange = srange, alpha = alpha, omega = omega
        )
      }
    ),
    list(
      key = "hitopbr", export = "reliability_hitopbr", sranges = list(c(1, 4), c(1, 5)),
      call = function(srange, alpha, omega) {
        hitop::reliability_hitopbr(
          br, items = 1:45,
          srange = srange, alpha = alpha, omega = omega
        )
      }
    )
  )
}

## Cross the paths with the flag and srange dimensions into one flat cell list.
FLAGS <- list(
  c(alpha = TRUE,  omega = TRUE),
  c(alpha = TRUE,  omega = FALSE),
  c(alpha = FALSE, omega = TRUE),
  c(alpha = FALSE, omega = FALSE)
)

build_cells <- function() {
  paths <- build_paths()
  cells <- list()
  for (p in paths) {
    for (si in seq_along(p$sranges)) {
      for (fl in FLAGS) {
        key <- sprintf(
          "%s | srange%d=[%s] | alpha=%s omega=%s",
          p$key, si, paste(p$sranges[[si]], collapse = ","),
          fl[["alpha"]], fl[["omega"]]
        )
        local({
          pp <- p; ss <- p$sranges[[si]]; ff <- fl
          cells[[key]] <<- list(
            export = pp$export,
            call = function() pp$call(ss, ff[["alpha"]], ff[["omega"]])
          )
        })
      }
    }
  }
  list(cells = cells, n_paths = length(paths))
}

run_probes <- function() {
  if (!requireNamespace("lavaan", quietly = TRUE)) {
    stop(
      "{lavaan} is not installed; every omega cell would be NA on both sides.",
      call. = FALSE
    )
  }
  exports <- enumerate_reliability_exports()
  if (length(exports) == 0L) {
    stop("The reliability-family sweep enumerated no exports.", call. = FALSE)
  }
  built <- build_cells()
  cells <- built$cells
  if (length(cells) == 0L) {
    stop("The argument matrix built no cells.", call. = FALSE)
  }
  ## The matrix is a full crossing, so its size is known from its dimensions.
  ## A cell dropped by an editing slip would otherwise go unnoticed.
  expected <- built$n_paths * 2L * length(FLAGS)
  if (length(cells) != expected) {
    stop(
      "The argument matrix holds ", length(cells), " cells but its dimensions ",
      "imply ", expected, ".",
      call. = FALSE
    )
  }
  probed <- unique(vapply(cells, function(c) c$export, character(1)))
  missing <- setdiff(exports, probed)
  if (length(missing) > 0L) {
    stop("No probe for: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  extra <- setdiff(probed, exports)
  if (length(extra) > 0L) {
    stop(
      "Probe for a non-enumerated export: ", paste(extra, collapse = ", "),
      call. = FALSE
    )
  }
  out <- lapply(names(cells), function(k) record_call(cells[[k]]$call()))
  names(out) <- names(cells)
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
      'cat("cells recorded:", length(rec), "\\n")'
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
base_sha <- system2("git", c("merge-base", "HEAD", default_ref), stdout = TRUE)
cat("Branch:     ", branch, "\n", sep = "")
cat("Merge base: ", base_sha, "\n\n", sep = "")

base_dir <- file.path(tempdir(), paste0("m061-base-", substr(base_sha, 1, 8)))
dir.create(base_dir, showWarnings = FALSE)
tarball <- file.path(tempdir(), "m061-base.tar")
system2("git", c("archive", "--format=tar", "-o", tarball, base_sha))
utils::untar(tarball, exdir = base_dir)

base <- run_side(base_dir, tempfile(fileext = ".rds"), "merge base")
head <- run_side(".", tempfile(fileext = ".rds"), "branch")

## ---- compare ----------------------------------------------------------------

cat("\n")
if (!identical(names(base), names(head))) {
  cat("The enumerated cell set differs between the two sides.\n")
  cat("  merge base: ", length(names(base)), " cells\n", sep = "")
  cat("  branch:     ", length(names(head)), " cells\n", sep = "")
  cat("  only at the merge base: ",
      paste(setdiff(names(base), names(head)), collapse = " ; "), "\n", sep = "")
  cat("  only on the branch:     ",
      paste(setdiff(names(head), names(base)), collapse = " ; "), "\n", sep = "")
  quit(status = 1L)
}

## The display-name column is the intended change. Locate it by either of its two
## names -- `scale` at the merge base, `Scale` on the branch -- never by
## position, so a column reordering could not smuggle a different column out of
## the diff.
NAME_COLS <- c("scale", "Scale")

name_col_of <- function(value) {
  if (!is.data.frame(value)) return(NULL)
  hit <- intersect(NAME_COLS, names(value))
  if (length(hit) == 0L) NULL else hit
}

## Everything except the display-name column, as a plain list so the two sides'
## tibble classes and column ORDER are both compared.
without_names <- function(value) {
  if (!is.data.frame(value)) return(value)
  drop <- name_col_of(value)
  if (is.null(drop)) return(value)
  value[, setdiff(names(value), drop), drop = FALSE]
}

names_of <- function(value) {
  col <- name_col_of(value)
  if (is.null(col) || length(col) != 1L) return(NULL)
  value[[col]]
}

cond_signature <- function(rec) {
  if (length(rec$conditions) == 0L) return(character(0))
  vapply(rec$conditions, function(c) paste0(c$type, ": ", c$message), character(1))
}

differ <- character(0)
renamed_cells <- character(0)
for (nm in names(base)) {
  b <- base[[nm]]
  h <- head[[nm]]
  problems <- character(0)
  if (!identical(b$aborted, h$aborted)) {
    problems <- c(problems, sprintf(
      "accept/reject verdict changed (merge base aborted = %s, branch = %s)",
      b$aborted, h$aborted
    ))
  }
  if (!identical(without_names(b$value), without_names(h$value))) {
    problems <- c(problems, "returned value changed outside the display-name column")
  }
  bs <- cond_signature(b)
  hs <- cond_signature(h)
  if (!identical(bs, hs)) {
    problems <- c(problems, sprintf(
      "conditions changed (%d -> %d):\n      merge base: %s\n      branch:     %s",
      length(bs), length(hs),
      paste(bs, collapse = " | "), paste(hs, collapse = " | ")
    ))
  }
  ## The display-name column, reported but not counted as a difference.
  bn <- names_of(b$value)
  hn <- names_of(h$value)
  if (!identical(bn, hn)) {
    renamed_cells <- c(renamed_cells, nm)
    moved <- if (is.null(bn) || is.null(hn) || length(bn) != length(hn)) {
      "(the column itself changed shape)"
    } else {
      d <- which(bn != hn)
      if (length(d) == 0L) "(no value moved)" else
        paste(sprintf("%s -> %s", bn[d], hn[d]), collapse = " ; ")
    }
    cat("name  ", nm, "\n      ", moved, "\n", sep = "")
  }
  if (length(problems) > 0L) {
    differ <- c(differ, nm)
    cat("DIFF  ", nm, "\n", sep = "")
    for (p in problems) cat("    ", p, "\n", sep = "")
  }
}

cat("\n", length(names(base)), " cell", if (length(names(base)) == 1L) "" else "s",
    " compared; ", length(differ), " differ", if (length(differ) == 1L) "s" else "",
    " outside the display-name column; ", length(renamed_cells),
    " carr", if (length(renamed_cells) == 1L) "ies" else "y",
    " a display-name change.\n", sep = "")
if (length(differ) > 0L) {
  quit(status = 1L)
}
cat("No number moved and no accept/reject verdict changed (AC6).\n")
