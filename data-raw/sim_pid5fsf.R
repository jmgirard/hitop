pidmat <- matrix(
  sample(0:3, size = 100*100, replace = TRUE),
  nrow = 100,
  ncol = 100
)

colnames(pidmat) <- paste0("pid_", 1:100)

sim_pid5fsf <- as.data.frame(pidmat)

usethis::use_data(sim_pid5fsf, overwrite = TRUE)
