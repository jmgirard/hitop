## Rename the item columns of the four shipped HiTOP datasets to the pattern
## the REDCap export writes: lowercase stem, underscore, item number
## zero-padded to the instrument's width (`hsr_001`..`hsr_405`,
## `hbr_01`..`hbr_45`). The sim scripts draw with no seed and `ku_data.R`
## reads a network drive, so neither is re-run here: each object is loaded
## from `data/`, its item columns renamed in place from the trailing integer
## of each old name, checked against the object committed at 7e06f40e (the
## last commit before this rename), and saved back. Values never move.
##
## Run from the package root. Compares loaded objects, never `.rda` bytes.

devtools::load_all(quiet = TRUE)

before_commit <- "7e06f40e"

object_at <- function(name, commit) {
  f <- tempfile(fileext = ".rda")
  system2("git", c("show", sprintf("%s:data/%s.rda", commit, name)), stdout = f)
  e <- new.env()
  load(f, envir = e)
  e[[name]]
}

trailing_integer <- function(x) as.integer(sub("^\\D+", "", x))

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

ku_hitopsr <- rename_items(
  "ku_hitopsr", c("participant", "biosex"), "hsr_", max(hitopsr_items$HSR)
)
sim_hitopsr <- rename_items(
  "sim_hitopsr", character(0), "hsr_", max(hitopsr_items$HSR)
)
ku_hitopbr <- rename_items(
  "ku_hitopbr", c("participant", "biosex"), "hbr_", max(hitopbr_items$HBR)
)
sim_hitopbr <- rename_items(
  "sim_hitopbr", character(0), "hbr_", max(hitopbr_items$HBR)
)

usethis::use_data(ku_hitopsr, sim_hitopsr, ku_hitopbr, sim_hitopbr, overwrite = TRUE)
