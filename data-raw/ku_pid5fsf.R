ku_pid5fsf <- read.csv("data-raw/ku_pid5fsf.csv")
colnames(ku_pid5fsf) <- c("response_id", paste0("pid_", 1:100))

usethis::use_data(ku_pid5fsf, overwrite = TRUE)
