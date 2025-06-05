## PID Items
pid_items <- readr::read_csv("data-raw/pid_items.csv", show_col_types = FALSE)
usethis::use_data(pid_items, overwrite = TRUE)

# ------------------------------------------------------------------------------

## PID Scales
pid5_scales <-
  pid_items |>
  dplyr::select(-Domain) |>
  tidyr::nest(
    itemdata = c(FULL, Reverse, Text),
    .by = Facet
  ) |>
  dplyr::mutate(
    nItems = purrr::map_dbl(itemdata, nrow),
    itemNumbers = purrr::map(itemdata, "FULL"),
    camelCase = snakecase::to_any_case(Facet, case = "lower_camel")
  )
names(pid5_scales$itemNumbers) <- pid5_scales$camelCase

pid5fsf_scales <-
  pid_items |>
  dplyr::select(-Domain) |>
  tidyr::drop_na(FSF) |>
  tidyr::nest(
    itemdata = c(FSF, Reverse, Text),
    .by = Facet
  ) |>
  dplyr::mutate(
    nItems = purrr::map_dbl(itemdata, nrow),
    itemNumbers = purrr::map(itemdata, "FSF"),
    camelCase = snakecase::to_any_case(Facet, case = "lower_camel")
  )
names(pid5fsf_scales$itemNumbers) <- pid5fsf_scales$camelCase

pid5bf_scales <-
  pid_items |>
  dplyr::select(-Facet) |>
  tidyr::drop_na(BF) |>
  tidyr::nest(
    itemdata = c(BF, Reverse, Text),
    .by = Domain
  ) |>
  dplyr::mutate(
    nItems = purrr::map_dbl(itemdata, nrow),
    itemNumbers = purrr::map(itemdata, "BF"),
    camelCase = snakecase::to_any_case(Domain, case = "lower_camel")
  )
names(pid5bf_scales$itemNumbers) <- pid5bf_scales$camelCase

pid_scales <- list(
  FULL = pid5_scales,
  FSF = pid5fsf_scales,
  BF = pid5bf_scales
)
usethis::use_data(pid_scales, overwrite = TRUE)
