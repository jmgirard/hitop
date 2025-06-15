## HiTOP-PRO Items
hitoppro_items <- readr::read_csv("data-raw/hitop-pro_items.csv")
usethis::use_data(hitoppro_items, overwrite = TRUE)

# ------------------------------------------------------------------------------

## HiTOP-PRO Scales
hitoppro_scales <-
  hitoppro_items |>
  tidyr::nest(
    itemdata = c(PRO, Reverse, Text, Subscale),
    .by = c(Scale, Subfactor, Spectrum)
  ) |>
  dplyr::mutate(
    nItems = purrr::map_dbl(itemdata, nrow),
    itemNumbers = purrr::map(itemdata, "PRO"),
    camelCase = snakecase::to_any_case(Scale, case = "lower_camel")
  )
names(hitoppro_scales$itemNumbers) <- hitoppro_scales$camelCase
usethis::use_data(hitoppro_scales, overwrite = TRUE)

# ------------------------------------------------------------------------------

## HiTOP-PRO Subscales
hitoppro_subscales <-
  hitoppro_items |>
  dplyr::filter(Subscale != "") |>
  tidyr::nest(
    itemdata = c(PRO, Reverse, Text, Subscale),
    .by = c(Subscale, Scale, Subfactor, Spectrum)
  ) |>
  dplyr::mutate(
    nItems = purrr::map_dbl(itemdata, nrow),
    itemNumbers = purrr::map(itemdata, "PRO"),
    camelCase = snakecase::to_any_case(Subscale, case = "lower_camel")
  )
names(hitoppro_subscales$itemNumbers) <- hitoppro_subscales$camelCase
usethis::use_data(hitoppro_subscales, overwrite = TRUE)
