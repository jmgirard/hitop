## Simulation Function
sim_bhitop <- function(n_subjects) {
  out <- matrix(
    sample(1:4, size = n_subjects*45, replace = TRUE),
    nrow = n_subjects,
    ncol = 45
  )
  colnames(out) <- paste0("bhitop_", 1:45)
  tibble::as_tibble(out)
}

# ------------------------------------------------------------------------------

## B-HiTOP
sim_bhitop <- sim_bhitop(n_subjects = 100)
usethis::use_data(sim_bhitop, overwrite = TRUE)
