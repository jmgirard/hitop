## HiTOP-HSUM Items
## Item numbers are read as integers, never guessed and coerced afterwards, so
## the `spec` attribute the reader stores stays a truthful record of the types.
hitophsum_items <- readr::read_csv(
  "data-raw/hitophsum_items.csv",
  col_types = readr::cols(Item = readr::col_integer())
)
usethis::use_data(hitophsum_items, overwrite = TRUE)

## HiTOP-HSUM Choices
hitophsum_choices <- readr::read_csv("data-raw/hitophsum_choices.csv")
usethis::use_data(hitophsum_choices, overwrite = TRUE)

## hitophsum_instructions (administration text) is internal data — see data-raw/sysdata.R
