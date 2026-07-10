## HiTOP-SR Items
hitopsr_items <- readr::read_csv("data-raw/hitopsr_items.csv") |>
  dplyr::select(-Subfactor, -Spectrum)
usethis::use_data(hitopsr_items, overwrite = TRUE)

# ------------------------------------------------------------------------------

## HiTOP-SR Scales
hitopsr_scales <-
  hitopsr_items |>
  tidyr::nest(
    itemdata = c(HSR, Reverse, Text, Subscale),
    .by = Scale
  ) |>
  dplyr::mutate(
    nItems = purrr::map_dbl(itemdata, nrow),
    itemNumbers = purrr::map(itemdata, "HSR"),
    camelCase = snakecase::to_any_case(Scale, case = "lower_camel")
  ) |>
  dplyr::arrange(Scale)
names(hitopsr_scales$itemNumbers) <- hitopsr_scales$camelCase
usethis::use_data(hitopsr_scales, overwrite = TRUE)

# ------------------------------------------------------------------------------

## HiTOP-SR Subscales
hitopsr_subscales <-
  hitopsr_items |>
  dplyr::filter(Subscale != "") |>
  tidyr::nest(
    itemdata = c(HSR, Reverse, Text, Subscale),
    .by = c(Subscale, Scale)
  ) |>
  dplyr::mutate(
    nItems = purrr::map_dbl(itemdata, nrow),
    itemNumbers = purrr::map(itemdata, "HSR"),
    camelCase = snakecase::to_any_case(Subscale, case = "lower_camel")
  ) |>
  dplyr::arrange(Subscale)
names(hitopsr_subscales$itemNumbers) <- hitopsr_subscales$camelCase
usethis::use_data(hitopsr_subscales, overwrite = TRUE)

## hitopsr_instructions (administration text) is internal data — see data-raw/sysdata.R

## HiTOP-SR Definitions
hitopsr_definitions <-
  readr::read_csv("data-raw/hitopsr_definitions.csv", show_col_types = FALSE)
usethis::use_data(hitopsr_definitions, overwrite = TRUE)
