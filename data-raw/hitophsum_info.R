## HiTOP-HSUM Items
hitophsum_items <- readr::read_csv("data-raw/hitophsum_items.csv")
usethis::use_data(hitophsum_items, overwrite = TRUE)

## HiTOP-HSUM Choices
hitophsum_choices <- readr::read_csv("data-raw/hitophsum_choices.csv")
usethis::use_data(hitophsum_choices, overwrite = TRUE)

## HiTOP-HSUM Instructions
hitophsum_instructions <- list(
  start = "Please select which of the following substance(s) you have used in the past 12 months. Please consider only substances that were NOT prescribed to you by a medical professional or that you used in a manner that was NOT prescribed."
)
usethis::use_data(hitophsum_instructions, overwrite = TRUE)
