# Line-ending policy check (M47).
#
# Walks every tracked path and reports three facts per path:
#   * the `text` attribute git resolves for it (from .gitattributes)
#   * whether its INDEX BLOB contains a CR byte
#   * whether that blob contains a NUL byte in its first 8000 bytes
#
# The blob is what matters, not the working-tree file: `eol=lf` normalizes on
# the way in, so a working tree can hold CRLF on Windows while the index is
# clean, and it is the index that every other clone checks out from.
#
# The last fact is git's own binary criterion. A file it holds for is binary,
# and must resolve to something other than `text=auto` — otherwise its stored
# bytes depend on a content heuristic rather than a declaration. That heuristic
# already failed in this repo: a Windows `actions/checkout` CRLF-converted the
# .txt/.qsf artifacts and tripped the md5 lock during M20's review.
#
# Two violations, either of which exits non-zero:
#   1. a `text=auto` path whose blob carries a CR byte
#   2. a NUL-carrying path left at `text=auto`
#
# Run: Rscript data-raw/check_line_endings.R

tracked_paths <- function() {
  paths <- system2("git", "ls-files", stdout = TRUE)
  # `git ls-files` quotes any path holding a control or non-ASCII byte, which
  # would make the string below disagree with the real path. Refuse rather
  # than check the wrong file.
  quoted <- grepl('^"', paths)
  if (any(quoted)) {
    stop("quoted path(s) from git ls-files: ", paste(paths[quoted], collapse = ", "))
  }
  paths
}

# One `git check-attr` call for every path, not one per path. The output reads
# `<path>: text: <value>`; the attribute name is fixed, so the split is
# unambiguous unless a path itself contains ": text: ". The round-trip
# assertion below turns that case into an error instead of a wrong answer.
text_attrs <- function(paths) {
  out <- system2(
    "git",
    c("check-attr", "--stdin", "text"),
    input = paths,
    stdout = TRUE
  )
  stopifnot(length(out) == length(paths))
  got_path <- sub(": text: .*$", "", out)
  value <- sub("^.*: text: ", "", out)
  stopifnot(identical(got_path, paths))
  value
}

blob_bytes <- function(path) {
  con <- pipe(paste0("git cat-file blob ':", path, "'"), open = "rb")
  on.exit(close(con), add = TRUE)
  readBin(con, what = "raw", n = 50e6)
}

paths <- tracked_paths()
attrs <- text_attrs(paths)

has_cr <- logical(length(paths))
has_nul <- logical(length(paths))
for (i in seq_along(paths)) {
  b <- blob_bytes(paths[[i]])
  has_cr[[i]] <- any(b == as.raw(13L))
  has_nul[[i]] <- any(utils::head(b, 8000L) == as.raw(0L))
}

report <- data.frame(
  path = paths,
  text = unname(attrs),
  cr = has_cr,
  nul = has_nul,
  stringsAsFactors = FALSE
)

# `git check-attr` reports four states, and only ONE of them is a declaration
# that the file is not to be converted:
#   unset       -- `-text` or `binary`; git leaves the bytes alone. Safe.
#   auto        -- normalized to LF in the index. Its blob must carry no CR.
#   set         -- forced text; same requirement.
#   unspecified -- NOTHING declares it, so behavior falls back to the local
#                  `core.autocrlf`, which differs per machine and per CI image.
# Treating `unspecified` as safe is the bug this check was written with: before
# the policy landed, every path was `unspecified`, so a comparison against
# "auto" found zero conversion-eligible files and the check passed vacuously on
# a repo carrying 84 CR-bearing files.
convertible <- report$text %in% c("auto", "set")
undeclared <- report$text == "unspecified"

cr_in_text <- report$path[convertible & report$cr]
nul_in_text <- report$path[!(report$text == "unset") & report$nul]

cat(sprintf("tracked paths:             %d\n", nrow(report)))
cat(sprintf("resolved text=auto/set:    %d\n", sum(convertible)))
cat(sprintf("declared -text/binary:     %d\n", sum(report$text == "unset")))
cat(sprintf("undeclared (unspecified):  %d\n", sum(undeclared)))
cat(sprintf("carrying a CR byte:        %d\n", sum(report$cr)))
cat(sprintf("binary by git's criterion: %d\n", sum(report$nul)))

if (any(undeclared)) {
  cat("\nVIOLATION - paths no .gitattributes rule covers (behavior depends on\n")
  cat("the local core.autocrlf, so it differs per machine and per CI image):\n")
  cat(paste0("  ", utils::head(report$path[undeclared], 20L), collapse = "\n"), "\n")
  if (sum(undeclared) > 20L) {
    cat(sprintf("  ... and %d more\n", sum(undeclared) - 20L))
  }
}
if (length(cr_in_text) > 0L) {
  cat("\nVIOLATION - convertible paths carrying a CR byte:\n")
  cat(paste0("  ", cr_in_text, collapse = "\n"), "\n")
}
if (length(nul_in_text) > 0L) {
  cat("\nVIOLATION - binary paths not declared -text/binary:\n")
  cat(paste0("  ", nul_in_text, collapse = "\n"), "\n")
}

if (any(undeclared) || length(cr_in_text) > 0L || length(nul_in_text) > 0L) {
  cat("\nLine-ending policy check FAILED.\n")
  quit(status = 1L)
}
cat("\nLine-ending policy check passed.\n")
