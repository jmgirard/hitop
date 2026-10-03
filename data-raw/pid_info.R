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

## PID5BF+M key (Bach et al., 2020; D-088). One row per BF+M item, in the key
## sheet's domain-grouped order: its BF+M number, the PID-5 item it reuses, and
## the facet and domain the key sheet puts it under. Facet and domain names are
## the package's printed names (D-088(b)): the 15 facets shared with the PID-5
## keep their `pid_items$Facet` spelling, and the six anankastia items, which
## `pid_items$Facet` files under Rigid Perfectionism, take the key's three
## facets. Source: the FU Berlin key sheet, p. 2
## (cairn/references/fuberlin2020pid5bfpm.md).
pid_bfpm_key <- readr::read_csv(
  "data-raw/pid_bfpm_key.csv",
  col_types = readr::cols(
    BFPM = readr::col_integer(),
    FULL = readr::col_integer(),
    Facet = readr::col_character(),
    Domain = readr::col_character()
  )
)
stopifnot(
  setequal(pid_bfpm_key$BFPM, 1:36),
  !anyDuplicated(pid_bfpm_key$BFPM),
  !anyDuplicated(pid_bfpm_key$FULL),
  all(pid_bfpm_key$FULL %in% pid_items$FULL)
)

## The BFPM column sits after BF, with the other form numbers. add_column()
## keeps readr's class and `spec`; the spec stays the record of what was read
## from pid_items.csv, so it does not list BFPM, which comes from the key CSV.
pid_items <- tibble::add_column(
  pid_items,
  BFPM = pid_bfpm_key$BFPM[match(pid_items$FULL, pid_bfpm_key$FULL)],
  .after = "BF"
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

## The PID5BF+M facets, built from the key CSV rather than from
## `pid_items$Facet` (D-088(c)): its anankastia facets regroup six Rigid
## Perfectionism items. Rows keep the key sheet's domain-grouped order, which is
## also the score_pid5() column order and the reliability_pid5() row order.
## Domain rows are not stored here; `pid_bfpm_domains` below maps them.
pid5bfpm_key_items <- dplyr::left_join(
  pid_bfpm_key,
  dplyr::select(pid_items, FULL, Reverse, Text),
  by = "FULL"
)
pid5bfpm_scales <-
  pid5bfpm_key_items |>
  dplyr::select(Facet, BFPM, Reverse, Text) |>
  tidyr::nest(
    itemdata = c(BFPM, Reverse, Text),
    .by = Facet
  ) |>
  dplyr::mutate(
    nItems = purrr::map_int(itemdata, nrow),
    itemNumbers = purrr::map(itemdata, "BFPM"),
    camelCase = snakecase::to_any_case(Facet, case = "lower_camel")
  )
names(pid5bfpm_scales$itemNumbers) <- pid5bfpm_scales$camelCase

pid_scales <- list(
  FULL = pid5_scales,
  SF = pid5sf_scales,
  BF = pid5bf_scales,
  BFPM = pid5bfpm_scales
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

# ------------------------------------------------------------------------------

## PID5BF+M Domains (D-088(c))
# Bach et al. (2020, p. 181) and the key sheet: each of the 6 domains is the
# average of its 3 facets. Same four columns as `pid_domains`, read from the key
# CSV in its domain order, so Anankastia sits fifth, before Psychoticism.
bfpm_facets <- unique(pid_bfpm_key[, c("Facet", "Domain")])
pid_bfpm_domains <- tibble::tibble(Domain = unique(bfpm_facets$Domain))
pid_bfpm_domains$camelCase <- snakecase::to_any_case(
  pid_bfpm_domains$Domain,
  case = "lower_camel"
)
pid_bfpm_domains$primaryFacets <- lapply(
  pid_bfpm_domains$Domain,
  function(d) bfpm_facets$Facet[bfpm_facets$Domain == d]
)
pid_bfpm_domains$facetStems <- lapply(
  pid_bfpm_domains$primaryFacets,
  function(f) snakecase::to_any_case(f, case = "lower_camel")
)
usethis::use_data(pid_bfpm_domains, overwrite = TRUE)

# pid_instructions (administration text) is internal data — see data-raw/sysdata.R
