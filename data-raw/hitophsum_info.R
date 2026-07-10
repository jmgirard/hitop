## HiTOP-HSUM Items
hitophsum_items <- readr::read_csv("data-raw/hitophsum_items.csv")
usethis::use_data(hitophsum_items, overwrite = TRUE)

## HiTOP-HSUM Choices
hitophsum_choices <- readr::read_csv("data-raw/hitophsum_choices.csv")
usethis::use_data(hitophsum_choices, overwrite = TRUE)

## hitophsum_instructions (administration text) is internal data — see data-raw/sysdata.R
