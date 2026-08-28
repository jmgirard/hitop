# Access to the branch's merge-base copies of the package's data files.
#
# M059's AC2 and AC5 compare the renamed keying tables and the rebuilt
# artifacts against the state before the rename, which lives only in git. The
# suite runs from a built tarball under R CMD check, where there is no
# repository at all, and after this branch merges the merge base becomes HEAD
# and there is nothing to compare. Both cases skip: the tests below run on the
# branch, under devtools::test(), and nowhere else.

repo_root <- function() testthat::test_path("..", "..")

git_ok <- function() {
  nzchar(Sys.which("git")) && dir.exists(file.path(repo_root(), ".git"))
}

git_run <- function(...) {
  out <- suppressWarnings(system2(
    "git",
    c("-C", shQuote(normalizePath(repo_root())), ...),
    stdout = TRUE,
    stderr = FALSE
  ))
  if (!is.null(attr(out, "status")) && attr(out, "status") != 0L) NULL else out
}

# The commit this branch was cut from. Never assumes the default branch is
# called `main`: the name is read from the remote's own HEAD.
merge_base_sha <- function() {
  if (!git_ok()) return(NULL)
  head <- git_run("rev-parse", "HEAD")
  ref <- git_run("symbolic-ref", "--short", "refs/remotes/origin/HEAD")
  if (is.null(ref) || !length(ref)) return(NULL)
  base <- git_run("merge-base", ref[[1]], "HEAD")
  if (is.null(base) || !length(base)) return(NULL)
  if (identical(base[[1]], head[[1]])) return(NULL) # nothing to compare against
  base[[1]]
}

skip_without_merge_base <- function() {
  base <- merge_base_sha()
  testthat::skip_if(
    is.null(base),
    "no distinct merge base (no repository, no remote HEAD, or already merged)"
  )
  base
}

# Load one `data/<name>.rda` as it stood at `sha`, returning the object.
merge_base_object <- function(name, sha) {
  tmp <- tempfile(fileext = ".rda")
  on.exit(unlink(tmp), add = TRUE)
  status <- suppressWarnings(system2(
    "git",
    c("-C", shQuote(normalizePath(repo_root())),
      "show", paste0(sha, ":data/", name, ".rda")),
    stdout = tmp,
    stderr = FALSE
  ))
  if (!identical(status, 0L) || file.size(tmp) == 0L) {
    testthat::skip(paste0("could not read data/", name, ".rda at ", sha))
  }
  env <- new.env()
  load(tmp, envir = env)
  get(name, envir = env)
}

# The merge base for M059's rename-diff tests specifically.
#
# Those tests compare the branch against the state before the Body Focus ->
# Appearance Focus rename, which exists only on the branch that performed it.
# Once the rename is on the default branch, a later branch's merge base already
# carries the new name: `skip_without_merge_base()` still returns a distinct
# commit, and the "only the name moved" assertions then report that branch's own
# work as an unexpected change. There is no rename diff to check there, so the
# tests skip rather than fail. Detected by asking the merge base what it called
# the scale, never by naming a branch or a commit.
skip_without_rename_base <- function(old_name = "Body Focus") {
  base <- skip_without_merge_base()
  old_scales <- merge_base_object("hitopsr_scales", base)
  testthat::skip_if_not(
    old_name %in% old_scales$Scale,
    paste0("the merge base already carries the rename; no ", old_name,
           " diff to compare")
  )
  base
}
