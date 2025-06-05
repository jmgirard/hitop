## Simulation Function
sim_pid <- function(n_subjects, n_items) {
  out <- matrix(
    sample(0:3, size = n_subjects*n_items, replace = TRUE),
    nrow = n_subjects,
    ncol = n_items
  )
  colnames(out) <- paste0("pid_", 1:n_items)
  tibble::as_tibble(out)
}

# ------------------------------------------------------------------------------

## PID-5
sim_pid5 <- sim_pid(n_subjects = 100, n_items = 220)
usethis::use_data(sim_pid5, overwrite = TRUE)

# ------------------------------------------------------------------------------

## PID-5-FSF
sim_pid5fsf <- sim_pid(n_subjects = 100, n_items = 100)
usethis::use_data(sim_pid5fsf, overwrite = TRUE)

# ------------------------------------------------------------------------------

## PID-5-BF
sim_pid5bf <- sim_pid(n_subjects = 100, n_items = 25)
usethis::use_data(sim_pid5bf, overwrite = TRUE)
