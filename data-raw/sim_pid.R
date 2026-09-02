## Simulation Function
##
## Item columns are named the way this package's REDCap export writes them:
## the form's lowercase stem, an underscore, and the item number zero-padded to
## the width of that form's last item.
sim_pid <- function(n_subjects, n_items, prefix) {
  out <- matrix(
    sample(0:3, size = n_subjects*n_items, replace = TRUE),
    nrow = n_subjects,
    ncol = n_items
  )
  colnames(out) <- sprintf(
    "%s%0*d", prefix, nchar(as.character(n_items)), 1:n_items
  )
  tibble::as_tibble(out)
}

# Fully set seed

set.seed(
  seed = 2025,
  kind = "Mersenne-Twister",
  normal.kind = "Inversion",
  sample.kind = "Rejection"
)

# ------------------------------------------------------------------------------

## PID-5
sim_pid5 <- sim_pid(n_subjects = 100, n_items = 220, prefix = "pid5_")
usethis::use_data(sim_pid5, overwrite = TRUE)

# ------------------------------------------------------------------------------

## PID-5-SF
sim_pid5sf <- sim_pid(n_subjects = 100, n_items = 100, prefix = "pid5sf_")
usethis::use_data(sim_pid5sf, overwrite = TRUE)

# ------------------------------------------------------------------------------

## PID-5-BF
sim_pid5bf <- sim_pid(n_subjects = 100, n_items = 25, prefix = "pid5bf_")
usethis::use_data(sim_pid5bf, overwrite = TRUE)
