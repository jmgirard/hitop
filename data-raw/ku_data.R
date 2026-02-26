## HiTOP-SR
ku_hitopsr <-
  readr::read_csv(
    "Y:/VIDAS/Study1/qualtrics_2026-02-26.csv",
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
  dplyr::rename_all(\(x) stringr::str_replace(x, "hitop", "hsr")) |>
  dplyr::arrange(participant)
usethis::use_data(ku_hitopsr, overwrite = TRUE)

# ------------------------------------------------------------------------------

## HiTOP-BR
item_conversion <-
  dplyr::full_join(hitopsr_items, hitopbr_items, by = "Text") |>
  dplyr::select(HSR, HBR) |>
  tidyr::drop_na()

ku_hitopbr <-
  ku_hitopsr |>
  dplyr::select(
    participant,
    biosex,
    sprintf("hsr%03d", item_conversion$HSR)
  ) |>
  setNames(c(
    "participant",
    "biosex",
    sprintf("hbr%02d", item_conversion$HBR)
  ))
usethis::use_data(ku_hitopbr, overwrite = TRUE)

# ------------------------------------------------------------------------------

## PID-5-SF
ku_pid5sf <- readr::read_csv("data-raw/ku_pid5sf.csv", show_col_types = FALSE)
usethis::use_data(ku_pid5sf, overwrite = TRUE)
