pidmat <- matrix(
  sample(0:3, size = 100*25, replace = TRUE),
  nrow = 100,
  ncol = 25
)

colnames(pidmat) <- paste0("pid_", 1:25)

sim_pid5bf <- as.data.frame(pidmat)

usethis::use_data(sim_pid5bf, overwrite = TRUE)
