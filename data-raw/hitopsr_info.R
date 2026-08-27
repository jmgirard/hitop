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

# One display label differs between the two sources: the definitions CSV writes
# the scale out as "Non-suicidal Self-injury" where `hitopsr_items` abbreviates
# it to "NSSI". Deriving the stem from the printed label alone would give this
# one row `nonSuicidalSelfInjury` against the scale table's `nssi`, so the pair
# is stated here rather than left to the case conversion. Every other label
# converts identically in both places. (Renaming the scale itself would make
# this pairing inert but harmless -- the check below is what would then catch a
# genuine drift.)
definition_scale_labels <- c("Non-suicidal Self-injury" = "NSSI")

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
        dplyr::coalesce(unname(definition_scale_labels[Scale]), Scale)
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
