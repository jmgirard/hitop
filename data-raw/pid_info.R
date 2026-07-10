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

# ------------------------------------------------------------------------------

## PID Domains (FULL/SF Step 3 domain scoring)
# APA full-form scoring key (Krueger et al., 2013, p. 8, Domain Table): each of
# the 5 personality-trait domains is the average of the 3 facets contributing
# PRIMARILY to it. This 15-facet primary subset is NOT the broader
# `pid_items$Domain` grouping (which tags 21 facets to domains); it drives
# score_pid5(version = "FULL"/"SF") domain output and is verified against the APA
# Domain Table in tests/testthat/test-keying.R. `primaryFacets` holds the facet
# labels as printed (matching `pid_items$Facet` / `pid_scales$Facet`); `camelCase`
# and `facetStems` are the score-output column stems, derived so the labels stay
# the single source of truth. The 5 `camelCase` domain names deliberately match
# the BF domain output names (`pid_scales[["BF"]]$camelCase`).
pid_domains <- tibble::tibble(
  Domain = c(
    "Negative affectivity",
    "Detachment",
    "Antagonism",
    "Disinhibition",
    "Psychoticism"
  ),
  primaryFacets = list(
    c("Emotional Lability", "Anxiousness", "Separation Insecurity"),
    c("Withdrawal", "Anhedonia", "Intimacy Avoidance"),
    c("Manipulativeness", "Deceitfulness", "Grandiosity"),
    c("Irresponsibility", "Impulsivity", "Distractibility"),
    c("Unusual Beliefs & Experiences", "Eccentricity", "Perceptual Dysregulation")
  )
)
pid_domains$camelCase <- snakecase::to_any_case(
  pid_domains$Domain,
  case = "lower_camel"
)
pid_domains$facetStems <- lapply(
  pid_domains$primaryFacets,
  function(f) snakecase::to_any_case(f, case = "lower_camel")
)
pid_domains <- pid_domains[, c("Domain", "camelCase", "primaryFacets", "facetStems")]
usethis::use_data(pid_domains, overwrite = TRUE)

# pid_instructions (administration text) is internal data — see data-raw/sysdata.R
