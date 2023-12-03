pidmat <- matrix(
  sample(0:4, size = 100*220, replace = TRUE),
  nrow = 100,
  ncol = 220
)

colnames(pidmat) <- paste0("pid_", 1:220)

sim_pid5 <- as.data.frame(pidmat)

usethis::use_data(sim_pid5, overwrite = TRUE)
