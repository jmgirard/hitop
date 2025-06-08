## B-HiTOP Items
bhitop_items <- readr::read_csv("data-raw/bhitop_items.csv")
usethis::use_data(bhitop_items, overwrite = TRUE)

# ------------------------------------------------------------------------------

## B-HiTOP Scales
bhitop_scales <-
  bhitop_items |>
  tidyr::nest(
    itemdata = c(BHITOP, Reverse, Text),
    .by = Scale
  ) |>
  dplyr::add_row(
    Scale = "p-Factor",
    itemdata = list(
      bhitop_items |>
        dplyr::filter(Pfactor) |>
        dplyr::select(-Scale, -Pfactor)
    )
  ) |>
  dplyr::mutate(
    nItems = purrr::map_dbl(itemdata, nrow),
    itemNumbers = purrr::map(itemdata, "BHITOP"),
    camelCase = snakecase::to_any_case(Scale, case = "lower_camel")
  )
names(bhitop_scales$itemNumbers) <- bhitop_scales$camelCase
usethis::use_data(bhitop_scales, overwrite = TRUE)
