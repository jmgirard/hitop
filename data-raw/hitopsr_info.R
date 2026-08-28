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

# Every scale label now converts identically in both tables, so the stem is left
# to the case conversion throughout. A `definition_scale_labels` map lived here
# until M058, pairing this file's spelling of one scale against the abbreviated
# form the keying table then used; the M058 rename made it inert and it was
# removed. The `stopifnot` below is what catches a genuine drift, and is why no
# such map is needed to notice one.

hitopsr_definitions <-
  readr::read_csv("data-raw/hitopsr_definitions.csv", show_col_types = FALSE) |>
  dplyr::mutate(
    # A row's stem names whatever that row defines: the subscale where there is
    # one, otherwise the scale. So the column is a key over all 93 rows, and it
    # matches `hitopsr_scales$camelCase` on the 76 scale rows and
    # `hitopsr_subscales$camelCase` on the 17 subscale rows.
    camelCase = snakecase::to_any_case(
      dplyr::coalesce(
        dplyr::na_if(Subscale, ""),
        Scale
      ),
      case = "lower_camel"
    )
  )

# Stop rather than ship a definitions table whose stems no longer pair with the
# tables built above: a relabelled scale or subscale would otherwise write an
# `.rda` in which some scale has no definition and some stem matches nothing,
# which `available_scales()` would then serve.
stopifnot(
  "definition stems do not match hitopsr_scales" = setequal(
    hitopsr_definitions$camelCase[is.na(hitopsr_definitions$Subscale)],
    hitopsr_scales$camelCase
  ),
  "definition stems do not match hitopsr_subscales" = setequal(
    hitopsr_definitions$camelCase[!is.na(hitopsr_definitions$Subscale)],
    hitopsr_subscales$camelCase
  ),
  "definition stems are not unique" = !anyDuplicated(hitopsr_definitions$camelCase)
)

usethis::use_data(hitopsr_definitions, overwrite = TRUE)
