## HiTOP-PRO
ku_hitoppro <-
  readr::read_csv("data-raw/study1.csv", show_col_types = FALSE) |>
  dplyr::filter(
    complete == 1, # complete the study
    consent == 12, # consent to participate
    data_share == 1, # allow us to share data
    use_data == 1, # recommend that we use their data
    att_hitop == 2 | att_hitop2 == 4 # get at least one attention check right
  ) |>
  dplyr::mutate(
    participant = response_id,
    biosex = factor(demo_biosex, levels = 0:1, labels = c("male", "female"))
  ) |>
  dplyr::select(participant, biosex, starts_with("hitop"))
usethis::use_data(ku_hitoppro, overwrite = TRUE)

# ------------------------------------------------------------------------------

## B-HiTOP
item_conversion <-
  dplyr::full_join(hitoppro_items, bhitop_items, by = "Text") |>
  dplyr::select(PRO, BHITOP) |>
  tidyr::drop_na()

ku_bhitop <-
  ku_hitoppro |>
  dplyr::select(
    participant,
    biosex,
    sprintf("hitop%03d", item_conversion$PRO)
  ) |>
  setNames(c(
    "participant",
    "biosex",
    sprintf("bhitop%02d", item_conversion$BHITOP)
  ))
usethis::use_data(ku_bhitop, overwrite = TRUE)

# ------------------------------------------------------------------------------

## PID-5-FSF
ku_pid5fsf <- readr::read_csv("data-raw/study2.csv", show_col_types = FALSE)
colnames(ku_pid5fsf) <- c("response_id", paste0("pid_", 1:100))
usethis::use_data(ku_pid5fsf, overwrite = TRUE)
