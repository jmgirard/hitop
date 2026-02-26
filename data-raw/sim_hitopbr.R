## Simulation Function
sim_hitopbr <- function(n_subjects) {
  out <- matrix(
    sample(1:4, size = n_subjects * 45, replace = TRUE),
    nrow = n_subjects,
    ncol = 45
  )
  colnames(out) <- paste0("hitopbr_", 1:45)
  tibble::as_tibble(out)
}

# ------------------------------------------------------------------------------

## HiTOP-BR
sim_hitopbr <- sim_hitopbr(n_subjects = 100)
usethis::use_data(sim_hitopbr, overwrite = TRUE)
