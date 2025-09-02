## HiTOP-SR
ku_hitopsr <-
  readr::read_csv(
    "Z:/VIDAS/Study1/qualtrics_2025-09-02.csv",
    show_col_types = FALSE
  ) |>
  dplyr::filter(
    Finished == 1, # complete the study
    consent == 12, # consent to participate
    data_share == 1, # allow us to share data
    use_data == 1, # recommend that we use their data
    att_hitop == 2 | att_hitop2 == 4 # get at least one attention check right
  ) |>
  dplyr::mutate(
    participant = sprintf("P%03d", as.integer(factor(ResponseId))),
    biosex = factor(demo_biosex, levels = 0:1, labels = c("male", "female"))
  ) |>
  dplyr::select(participant, biosex, starts_with("hitop")) |>
  dplyr::arrange(participant)
usethis::use_data(ku_hitopsr, overwrite = TRUE)

# ------------------------------------------------------------------------------

## B-HiTOP
item_conversion <-
  dplyr::full_join(hitopsr_items, bhitop_items, by = "Text") |>
  dplyr::select(HSR, BHITOP) |>
  tidyr::drop_na()

ku_bhitop <-
  ku_hitopsr |>
  dplyr::select(
    participant,
    biosex,
    sprintf("hitop%03d", item_conversion$HSR)
  ) |>
  setNames(c(
    "participant",
    "biosex",
    sprintf("bhitop%02d", item_conversion$BHITOP)
  ))
usethis::use_data(ku_bhitop, overwrite = TRUE)

# ------------------------------------------------------------------------------

## PID-5-SF
ku_pid5sf <- readr::read_csv("data-raw/ku_pid5sf.csv", show_col_types = FALSE)
usethis::use_data(ku_pid5sf, overwrite = TRUE)
