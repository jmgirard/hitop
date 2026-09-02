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
  names(new)[is_item] <- item_names(prefix, n, max_n = max_n)

  # Only the names changed, and each column kept its item.
  stopifnot(
    identical(unname(as.list(new)), unname(as.list(old))),
    identical(trailing_integer(names(new)[is_item]), n)
  )
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
