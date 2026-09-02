## PID Items
## Needs {snakecase} installed. It is not a package dependency -- no code under
## R/ uses it -- so it is declared in DESCRIPTION under Config/Needs/data-raw
## rather than Imports, and a contributor regenerating this data must install it
## themselves: install.packages("snakecase").
## Item numbers are read as integers, never guessed and coerced afterwards, so
## the `spec` attribute the reader stores stays a truthful record of the types.
pid_items <- readr::read_csv(
  "data-raw/pid_items.csv",
  col_types = readr::cols(
    FULL = readr::col_integer(),
    SF = readr::col_integer(),
    BF = readr::col_integer(),
    INC = readr::col_integer(),
    INCS = readr::col_integer(),
    ORS = readr::col_integer(),
    ORSS = readr::col_integer(),
    PRD = readr::col_integer(),
    PRDS = readr::col_integer(),
    SDTD = readr::col_integer(),
    SDTDS = readr::col_integer()
  )
)
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
    nItems = purrr::map_int(itemdata, nrow),
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
    nItems = purrr::map_int(itemdata, nrow),
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
    nItems = purrr::map_int(itemdata, nrow),
    itemNumbers = purrr::map(itemdata, "BF"),
    camelCase = snakecase::to_any_case(Domain, case = "lower_camel")
  )
## The PID-5-BF total scale. Unlike the five domain rows above, this is not a
## grouping of `pid_items` -- it is the whole 25-item form scored as one scale.
## Markon et al. (2024, Ch. 3, p. 23): the BF total "can be computed by averaging
## the overall score by the total number of items in the measure (i.e., 25)", so
## it is the item-level mean over all 25 items, NOT the mean of the five domain
## means (the two coincide on complete data and diverge only under missingness).
## It lives here rather than as a score_pid5() special case so that every
## pid_scales consumer -- scoring, reliability, and the DOCX scoring table --
## reads one item list. Provenance: cairn/SOURCES.md, "Note on the BF total
## score"; the decision to carry the ripple is D-019.
pid5bf_total_itemdata <-
  pid_items |>
  tidyr::drop_na(BF) |>
  dplyr::arrange(BF) |>
  dplyr::select(BF, Reverse, Text)

pid5bf_scales <- dplyr::bind_rows(
  pid5bf_scales,
  tibble::tibble(
    Domain = "Total",
    itemdata = list(pid5bf_total_itemdata),
    nItems = as.integer(nrow(pid5bf_total_itemdata)),
    itemNumbers = list(pid5bf_total_itemdata$BF),
    camelCase = "total"
  )
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
