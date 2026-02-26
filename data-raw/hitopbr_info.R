## HiTOP-BR Items
hitopbr_items <- readr::read_csv("data-raw/hitopbr_items.csv")
usethis::use_data(hitopbr_items, overwrite = TRUE)

# ------------------------------------------------------------------------------

## HiTOP-BR Scales
hitopbr_scales <-
  hitopbr_items |>
  tidyr::nest(
    itemdata = c(HBR, Reverse, Text),
    .by = Scale
  ) |>
  dplyr::add_row(
    Scale = "p-Factor",
    itemdata = list(
      hitopbr_items |>
        dplyr::filter(Pfactor) |>
        dplyr::select(-Scale, -Pfactor)
    )
  ) |>
  dplyr::mutate(
    nItems = purrr::map_dbl(itemdata, nrow),
    itemNumbers = purrr::map(itemdata, "HBR"),
    camelCase = snakecase::to_any_case(Scale, case = "lower_camel")
  )
names(hitopbr_scales$itemNumbers) <- hitopbr_scales$camelCase
usethis::use_data(hitopbr_scales, overwrite = TRUE)
