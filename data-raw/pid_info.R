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

pid5sf_scales <-
  pid_items |>
  dplyr::select(-Domain) |>
  tidyr::drop_na(SF) |>
  tidyr::nest(
    itemdata = c(SF, Reverse, Text),
    .by = Facet
  ) |>
  dplyr::mutate(
    nItems = purrr::map_dbl(itemdata, nrow),
    itemNumbers = purrr::map(itemdata, "SF"),
    camelCase = snakecase::to_any_case(Facet, case = "lower_camel")
  )
names(pid5sf_scales$itemNumbers) <- pid5sf_scales$camelCase

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
  SF = pid5sf_scales,
  BF = pid5bf_scales
)
usethis::use_data(pid_scales, overwrite = TRUE)

# Standard instructions and response choices for the PID-5
pid_instructions <- list(
  start = "This is a list of things different people might say about themselves. We are interested in how you would describe yourself. There are no right or wrong answers. So you can describe yourself as honestly as possible, we will keep your responses confidential. We'd like you to take your time and read each statement carefully, selecting the response that best describes you.",
  options = data.frame(
    value = 0:3,
    label = c(
      "Very False or Often False",
      "Sometimes or Somewhat False",
      "Sometimes or Somewhat True",
      "Very True or Often True"
    ),
    stringsAsFactors = FALSE
  )
)
usethis::use_data(pid_instructions, overwrite = TRUE)
