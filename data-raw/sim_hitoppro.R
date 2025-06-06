## Simulation Function
sim_pro <- function(n_subjects) {
  out <- matrix(
    sample(1:4, size = n_subjects*405, replace = TRUE),
    nrow = n_subjects,
    ncol = 405
  )
  colnames(out) <- paste0("pro_", 1:405)
  tibble::as_tibble(out)
}

# ------------------------------------------------------------------------------

## HiTOP-PRO
sim_hitoppro <- sim_pro(n_subjects = 100)
usethis::use_data(sim_hitoppro, overwrite = TRUE)
