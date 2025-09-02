## HiTOP-SR Items
hitopsr_items <- readr::read_csv("data-raw/hitopsr_items.csv")
usethis::use_data(hitopsr_items, overwrite = TRUE)

# ------------------------------------------------------------------------------

## HiTOP-SR Scales
hitopsr_scales <-
  hitopsr_items |>
  tidyr::nest(
    itemdata = c(HSR, Reverse, Text, Subscale),
    .by = c(Scale, Subfactor, Spectrum)
  ) |>
  dplyr::mutate(
    nItems = purrr::map_dbl(itemdata, nrow),
    itemNumbers = purrr::map(itemdata, "HSR"),
    camelCase = snakecase::to_any_case(Scale, case = "lower_camel")
  )
names(hitopsr_scales$itemNumbers) <- hitopsr_scales$camelCase
usethis::use_data(hitopsr_scales, overwrite = TRUE)

# ------------------------------------------------------------------------------

## HiTOP-SR Subscales
hitopsr_subscales <-
  hitopsr_items |>
  dplyr::filter(Subscale != "") |>
  tidyr::nest(
    itemdata = c(HSR, Reverse, Text, Subscale),
    .by = c(Subscale, Scale, Subfactor, Spectrum)
  ) |>
  dplyr::mutate(
    nItems = purrr::map_dbl(itemdata, nrow),
    itemNumbers = purrr::map(itemdata, "HSR"),
    camelCase = snakecase::to_any_case(Subscale, case = "lower_camel")
  )
names(hitopsr_subscales$itemNumbers) <- hitopsr_subscales$camelCase
usethis::use_data(hitopsr_subscales, overwrite = TRUE)
