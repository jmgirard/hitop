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

## HiTOP-BR Instructions
hitopbr_instructions <-
  list(
    start = "Please consider whether there have been significant times during the last 12 months during which the following statements applied to you. Then please select the option that best describes how well each statement described you during that period.",
    options = data.frame(
      value = c(1, 2, 3, 4),
      label = c("Not at all", "A little", "Moderately", "A lot")
    )
  )
usethis::use_data(hitopbr_instructions, overwrite = TRUE)
