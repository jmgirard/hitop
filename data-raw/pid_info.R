## PID-5 Items
pid_items <- readr::read_csv("data-raw/pid_items.csv", show_col_types = FALSE)
usethis::use_data(pid_items, overwrite = TRUE)

# ------------------------------------------------------------------------------

## PID-5 Scales
pid5_scales <-
  pid_items |>
  select(-Domain) |>
  tidyr::nest(
    itemdata = c(PID5, Reverse, Text),
    .by = Facet
  ) |>
  dplyr::mutate(
    nItems = purrr::map_dbl(itemdata, nrow),
    itemNumbers = purrr::map(itemdata, "PID5"),
    camelCase = snakecase::to_any_case(Facet, case = "lower_camel")
  )
names(pid5_scales$itemNumbers) <- pid5_scales$camelCase
usethis::use_data(pid5_scales, overwrite = TRUE)

# ------------------------------------------------------------------------------

## PID-5-FSF Scales
pid5fsf_scales <-
  pid_items |>
  select(-Domain) |>
  tidyr::drop_na(PID5FSF) |>
  tidyr::nest(
    itemdata = c(PID5FSF, Reverse, Text),
    .by = Facet
  ) |>
  dplyr::mutate(
    nItems = purrr::map_dbl(itemdata, nrow),
    itemNumbers = purrr::map(itemdata, "PID5FSF"),
    camelCase = snakecase::to_any_case(Facet, case = "lower_camel")
  )
names(pid5fsf_scales$itemNumbers) <- pid5fsf_scales$camelCase
usethis::use_data(pid5fsf_scales, overwrite = TRUE)

# ------------------------------------------------------------------------------

## PID-5-BF Scales
pid5bf_scales <-
  pid_items |>
  select(-Facet) |>
  tidyr::drop_na(PID5BF) |>
  tidyr::nest(
    itemdata = c(PID5BF, Reverse, Text),
    .by = Domain
  ) |>
  dplyr::mutate(
    nItems = purrr::map_dbl(itemdata, nrow),
    itemNumbers = purrr::map(itemdata, "PID5BF"),
    camelCase = snakecase::to_any_case(Domain, case = "lower_camel")
  )
names(pid5bf_scales$itemNumbers) <- pid5bf_scales$camelCase
usethis::use_data(pid5bf_scales, overwrite = TRUE)
