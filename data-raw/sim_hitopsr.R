## Simulation Function
sim_hsr <- function(n_subjects) {
  out <- matrix(
    sample(1:4, size = n_subjects * 405, replace = TRUE),
    nrow = n_subjects,
    ncol = 405
  )
  colnames(out) <- paste0("hsr_", 1:405)
  tibble::as_tibble(out)
}

# ------------------------------------------------------------------------------

## HiTOP-SR
sim_hitopsr <- sim_hsr(n_subjects = 100)
usethis::use_data(sim_hitopsr, overwrite = TRUE)
