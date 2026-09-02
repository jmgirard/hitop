## Rename the item columns of the four shipped PID-5 datasets to the pattern
## this package's own REDCap export writes: lowercase per-form stem,
## underscore, item number zero-padded to that form's width
## (`pid5_001`..`pid5_220`, `pid5sf_001`..`pid5sf_100`, `pid5bf_01`..`pid5bf_25`).
## `data-raw/sim_pid.R` draws under a fixed seed and `data-raw/ku_data.R` reads
## a network drive, so neither is re-run here: each object is loaded from
## `data/`, its item columns renamed in place from the trailing integer of each
## old name, checked against the object committed at d3ac6695 (the last commit
## before this rename), and saved back. Values never move.
##
## Run from the package root. Compares loaded objects, never `.rda` bytes.

devtools::load_all(quiet = TRUE)

before_commit <- "d3ac6695"

object_at <- function(name, commit) {
  f <- tempfile(fileext = ".rda")
  system2("git", c("show", sprintf("%s:data/%s.rda", commit, name)), stdout = f)
  e <- new.env()
  load(f, envir = e)
  e[[name]]
}

# The number after the final underscore, so one extractor reads both the old
# unpadded names and the new padded ones (whose stems themselves carry a digit).
trailing_integer <- function(x) as.integer(sub("^.*_", "", x))

rename_items <- function(name, id_cols, prefix, max_n) {
  old <- object_at(name, before_commit)
  is_item <- !(names(old) %in% id_cols)
  n <- trailing_integer(names(old)[is_item])
  stopifnot(!anyNA(n), identical(n, seq_len(max_n)))

  new <- old
  renamed <- item_names(prefix, n, max_n = max_n)
  names(new)[is_item] <- renamed

  # `ku_pid5sf` is a readr tibble, so it carries a `spec` attribute recording
  # the column names the CSV was read under. Left alone it would keep the old
  # names inside the shipped object, where a names() scan cannot see them and
  # where they would not match what re-reading the renamed CSV produces.
  spec <- attr(new, "spec")
  if (!is.null(spec)) {
    # The rename below writes by position, so the recorded names must be the
    # object's own names, in the same order: otherwise a new name would land on
    # a slot describing a different column and every check below would still
    # pass.
    stopifnot(identical(names(spec$cols), names(old)))
    names(spec$cols)[is_item] <- renamed
    attr(new, "spec") <- spec
    stopifnot(identical(names(spec$cols), names(new)))
  }

  # Only the names changed, and each column kept its item. The comparison is
  # over the columns alone: `as.list()` on a data frame keeps the object's
  # remaining attributes, `spec` among them, so it cannot tell a moved value
  # from the rename this script makes to the `spec` above.
  stopifnot(
    identical(unname(lapply(new, identity)), unname(lapply(old, identity))),
    identical(trailing_integer(names(new)[is_item]), n)
  )

  # Nothing but the names and that `spec` moved.
  strip <- function(x) {
    a <- attributes(x)
    a[setdiff(names(a), c("names", "spec"))]
  }
  stopifnot(identical(strip(new), strip(old)))
  new
}

sim_pid5 <- rename_items(
  "sim_pid5", character(0), "pid5_", max(pid_items$FULL, na.rm = TRUE)
)
sim_pid5sf <- rename_items(
  "sim_pid5sf", character(0), "pid5sf_", max(pid_items$SF, na.rm = TRUE)
)
ku_pid5sf <- rename_items(
  "ku_pid5sf", "response_id", "pid5sf_", max(pid_items$SF, na.rm = TRUE)
)
sim_pid5bf <- rename_items(
  "sim_pid5bf", character(0), "pid5bf_", max(pid_items$BF, na.rm = TRUE)
)

usethis::use_data(sim_pid5, sim_pid5sf, ku_pid5sf, sim_pid5bf, overwrite = TRUE)
