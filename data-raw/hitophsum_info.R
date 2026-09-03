## HiTOP-HSUM Items
## Item numbers are read as integers, never guessed and coerced afterwards, so
## the `spec` attribute the reader stores stays a truthful record of the types.
hitophsum_items <- readr::read_csv(
  "data-raw/hitophsum_items.csv",
  col_types = readr::cols(Item = readr::col_integer())
)
usethis::use_data(hitophsum_items, overwrite = TRUE)

## HiTOP-HSUM Choices
## `Value` is a response value, read as an integer for the same reason `Item`
## is above: the type is set at the read rather than guessed and coerced after.
hitophsum_choices <- readr::read_csv(
  "data-raw/hitophsum_choices.csv",
  col_types = readr::cols(Value = readr::col_integer())
)
usethis::use_data(hitophsum_choices, overwrite = TRUE)

## hitophsum_instructions (administration text) is internal data — see data-raw/sysdata.R
