# Line-ending policy check (M047).
#
# Walks every tracked path and reports four facts per path:
#   * the `text` attribute git resolves for it (from .gitattributes)
#   * whether its INDEX BLOB contains a CR byte
#   * whether that blob contains a NUL byte in its first 8000 bytes
#   * whether the path sits under a byte-locked artifact directory
#
# The blob is what matters, not the working-tree file: `eol=lf` normalizes on
# the way in, so a working tree can hold CRLF on Windows while the index stays
# clean, and it is the index every other clone checks out from.
#
# The NUL fact is git's own binary criterion. A file it holds for is binary and
# must resolve to something other than `text=auto`, or its stored bytes depend
# on a content heuristic rather than a declaration. That heuristic already
# failed here: a Windows `actions/checkout` CRLF-converted the .txt/.qsf
# artifacts and tripped the md5 lock during M020's review.
#
# Four violations, any of which exits non-zero:
#   1. a path no .gitattributes rule covers (`unspecified`)
#   2. a convertible path whose blob carries a CR byte
#   3. a binary path (by the NUL criterion) not declared `-text`/`binary`
#   4. OVER-declaration: a path declared `-text`/`binary` that is plainly text
#      (no NUL, no CR) and is not byte-locked. Declaring a text file binary
#      freezes it out of normalization silently, which is why the milestone
#      barred `*.svg binary` -- `pkgdown/favicon/favicon.svg` is text. The
#      byte-locked directories are exempt by RULE, not by a hand-list: their
#      contents are md5-locked under D-016/D-033 and are declared for that
#      reason regardless of what bytes they happen to hold.
#
# Run from anywhere inside the repo: Rscript data-raw/check_line_endings.R

# Byte-locked artifact trees (D-016, D-033). Declared regardless of content.
LOCKED <- c("inst/extdata/", "pkgdown/assets/downloads/")

# Every git call goes through here. `system2()` returns the exit status as an
# attribute and R does NOT raise on a non-zero one, so a failing git call would
# otherwise return character(0) and every downstream count would read zero --
# the guard reporting a clean pass on a repo it never managed to inspect.
git_lines <- function(args, input = NULL) {
  out <- system2("git", args, input = input, stdout = TRUE, stderr = FALSE)
  status <- attr(out, "status")
  if (!is.null(status) && status != 0L) {
    stop(
      "git ", paste(args, collapse = " "), " failed with status ", status,
      call. = FALSE
    )
  }
  out
}

# Anchor to the repo root. `git ls-files` is relative to the working directory,
# so running from data-raw/ would list 36 cwd-relative paths that no `:path`
# lookup can resolve -- and, before the exit-status check above, would have
# reported all of them clean.
repo_root <- function() {
  root <- git_lines(c("rev-parse", "--show-toplevel"))
  if (length(root) != 1L || !nzchar(root)) {
    stop("could not locate the repository root", call. = FALSE)
  }
  root
}

tracked_paths <- function() {
  paths <- git_lines("ls-files")
  # `git ls-files` quotes any path holding a control or non-ASCII byte, which
  # would make the lookup below disagree with the real path. Refuse rather
  # than check the wrong file.
  quoted <- grepl('^"', paths)
  if (any(quoted)) {
    stop(
      "git ls-files returned quoted path(s), which this check cannot resolve: ",
      paste(paths[quoted], collapse = ", "),
      call. = FALSE
    )
  }
  paths
}

# One `git check-attr` call for every path, not one per path. The output reads
# `<path>: text: <value>`; the attribute name is fixed, so the split is
# unambiguous unless a path itself contains ": text: ". The round-trip
# assertion turns that case into a named error instead of a wrong answer.
text_attrs <- function(paths) {
  out <- git_lines(c("check-attr", "--stdin", "text"), input = paths)
  if (length(out) != length(paths)) {
    stop(
      "git check-attr returned ", length(out), " lines for ", length(paths),
      " paths", call. = FALSE
    )
  }
  got_path <- sub(": text: .*$", "", out)
  value <- sub("^.*: text: ", "", out)
  mismatch <- which(got_path != paths)
  if (length(mismatch) > 0L) {
    stop(
      "could not parse git check-attr output for: ",
      paste(paths[mismatch], collapse = ", "),
      " -- a path containing ': text: ' would do this",
      call. = FALSE
    )
  }
  value
}

# Read every blob in ONE `git cat-file --batch` call, with the paths arriving
# on stdin and the output captured to an R-controlled temp file.
#
# No path ever reaches a command line. The earlier version interpolated the
# path into a `pipe()` command string, so a path holding an apostrophe broke
# the quoting, the read returned zero bytes, and the guard scored the file as
# carrying no CR and no NUL -- a silent pass on the exact fault this script
# exists to catch. `system2(stdout = <file>)` does not fix that on its own:
# it builds a shell redirect, so the path is still shell-parsed. Passing the
# paths on stdin is what makes any byte in a filename safe. Batching also
# turns 334 git processes into one.
#
# `--batch` emits, per object: "<oid> <type> <size>\n", then <size> raw bytes,
# then one "\n". A name it cannot resolve emits "<name> missing\n" instead.
blob_facts <- function(paths) {
  out <- tempfile("blobs")
  on.exit(unlink(out), add = TRUE)
  status <- system2(
    "git", c("cat-file", "--batch"),
    input = paste0(":", paths), stdout = out, stderr = FALSE
  )
  if (!identical(status, 0L)) {
    stop("git cat-file --batch failed with status ", status, call. = FALSE)
  }

  con <- file(out, open = "rb")
  on.exit(close(con), add = TRUE)
  size <- file.size(out)
  raw_all <- readBin(con, what = "raw", n = size)
  if (length(raw_all) != size) {
    stop("short read on the cat-file stream", call. = FALSE)
  }

  nl <- as.raw(10L)
  pos <- 1L
  facts <- vector("list", length(paths))
  for (i in seq_along(paths)) {
    eol <- which(raw_all[pos:length(raw_all)] == nl)[1L]
    if (is.na(eol)) {
      stop("cat-file stream ended early at: ", paths[[i]], call. = FALSE)
    }
    header <- rawToChar(raw_all[seq.int(pos, pos + eol - 2L)])
    if (grepl(" missing$", header)) {
      stop("git could not read the blob for: ", paths[[i]], call. = FALSE)
    }
    n <- as.integer(sub("^.* ([0-9]+)$", "\\1", header))
    if (is.na(n)) {
      stop("unparseable cat-file header for ", paths[[i]], ": ", header,
           call. = FALSE)
    }
    body_start <- pos + eol
    b <- if (n > 0L) raw_all[seq.int(body_start, body_start + n - 1L)] else raw()
    facts[[i]] <- list(
      cr = any(b == as.raw(13L)),
      nul = any(utils::head(b, 8000L) == as.raw(0L))
    )
    pos <- body_start + n + 1L  # skip the body and its trailing newline
  }
  facts
}

setwd(repo_root())
paths <- tracked_paths()
attrs <- text_attrs(paths)

facts <- blob_facts(paths)

report <- data.frame(
  path = paths,
  text = attrs,
  cr = vapply(facts, `[[`, logical(1), "cr"),
  nul = vapply(facts, `[[`, logical(1), "nul"),
  stringsAsFactors = FALSE
)
report$locked <- Reduce(
  `|`,
  lapply(LOCKED, function(d) startsWith(report$path, d))
)

# `git check-attr` reports four states, and only ONE of them declares that the
# file is not to be converted:
#   unset       -- `-text` or `binary`; git leaves the bytes alone.
#   auto / set  -- normalized in the index; the blob must carry no CR.
#   unspecified -- NOTHING declares it, so behavior falls back to the local
#                  `core.autocrlf`, which differs per machine and per CI image.
# Treating `unspecified` as safe is the bug this script was first written with:
# before the policy landed every path was `unspecified`, so comparing against
# "auto" found zero convertible files and the check passed vacuously on a repo
# carrying 84 CR-bearing files.
declared <- report$text == "unset"
convertible <- report$text %in% c("auto", "set")
undeclared <- report$text == "unspecified"

v_undeclared <- report$path[undeclared]
v_cr <- report$path[convertible & report$cr]
v_binary <- report$path[!declared & report$nul]
v_over <- report$path[declared & !report$nul & !report$cr & !report$locked]

cat(sprintf("tracked paths:             %d\n", nrow(report)))
cat(sprintf("resolved text=auto/set:    %d\n", sum(convertible)))
cat(sprintf("declared -text/binary:     %d\n", sum(declared)))
cat(sprintf("undeclared (unspecified):  %d\n", sum(undeclared)))
cat(sprintf("carrying a CR byte:        %d\n", sum(report$cr)))
cat(sprintf("binary by git's criterion: %d\n", sum(report$nul)))

report_violation <- function(paths, headline) {
  if (length(paths) == 0L) {
    return(invisible(FALSE))
  }
  cat("\nVIOLATION - ", headline, "\n", sep = "")
  cat(paste0("  ", utils::head(paths, 20L), "\n"), sep = "")
  if (length(paths) > 20L) {
    cat(sprintf("  ... and %d more\n", length(paths) - 20L))
  }
  invisible(TRUE)
}

bad <- c(
  report_violation(
    v_undeclared,
    "paths no .gitattributes rule covers, so their handling depends on the local core.autocrlf:"
  ),
  report_violation(v_cr, "convertible paths carrying a CR byte:"),
  report_violation(v_binary, "binary paths not declared -text/binary:"),
  report_violation(
    v_over,
    "paths declared -text/binary that are plainly text (no NUL, no CR) and are not byte-locked:"
  )
)

if (any(bad)) {
  cat("\nLine-ending policy check FAILED.\n")
  quit(status = 1L)
}
cat("\nLine-ending policy check passed.\n")
