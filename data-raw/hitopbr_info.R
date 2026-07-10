## HiTOP-BR Items
hitopbr_items <- readr::read_csv("data-raw/hitopbr_items.csv")
usethis::use_data(hitopbr_items, overwrite = TRUE)

# ------------------------------------------------------------------------------

## HiTOP-BR Scales
hitopbr_scales <-
  hitopbr_items |>
  tidyr::nest(
    itemdata = c(HBR, Reverse, Text, HSR, Original),
    .by = Scale
  ) |>
  dplyr::arrange(Scale) |>
  dplyr::add_row(
    Scale = "Externalizing",
    itemdata = list(
      hitopbr_items |>
        dplyr::filter(Externalizing) |>
        dplyr::select(-Scale, -Externalizing, -Pfactor)
    )
  ) |>
  dplyr::add_row(
    Scale = "p-Factor",
    itemdata = list(
      hitopbr_items |>
        dplyr::filter(Pfactor) |>
        dplyr::select(-Scale, -Externalizing, -Pfactor)
    )
  ) |>
  dplyr::mutate(
    nItems = purrr::map_dbl(itemdata, nrow),
    itemNumbers = purrr::map(itemdata, "HBR"),
    camelCase = snakecase::to_any_case(Scale, case = "lower_camel")
  )
names(hitopbr_scales$itemNumbers) <- hitopbr_scales$camelCase
usethis::use_data(hitopbr_scales, overwrite = TRUE)

## hitopbr_instructions (administration text) is internal data — see data-raw/sysdata.R
