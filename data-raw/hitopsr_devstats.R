## HiTOP-SR development-sample descriptive statistics (hitopsr_devstats)
##
## Builds data/hitopsr_devstats.rda from data-raw/hitopsr_devstats.csv, the
## transcription of Table 1 of the HiTOP-SR introduction manuscript
## (cairn/references/simms2026.md; D-042 admits the document, D-032 admits its
## descriptive table as a published source under IP3).
##
## The CSV is a transcription, not an extraction: its 93 rows were read from the
## three table pages rendered at 200 dpi. The machine extraction lives in
## data-raw/hitopsr_table1.R and is run against this CSV, and against the object
## this script builds, by data-raw/verify_hitopsr_devstats.R. Keeping the two
## routes apart is what makes that diff a check rather than a tautology (IP2).
##
## Requires the package's own tables, so run after data-raw/hitopsr_info.R.

devtools::load_all(".", quiet = TRUE)

table1 <- readr::read_csv(
  "data-raw/hitopsr_devstats.csv",
  col_types = readr::cols(
    label = readr::col_character(),
    nItems = readr::col_integer(),
    alpha = readr::col_double(),
    mean = readr::col_double(),
    sd = readr::col_double()
  )
)

# ------------------------------------------------------------------------------
## The name map
##
## Table 1 prints 101 data rows in 13 sections. The last section, "Superspectra
## and Spectra Scales", is the eight HiTOP-BR scales; it is out of this
## milestone's scope (Table 1 and `hitopbr_scales` disagree on the item counts of
## Detachment and Internalizing) and its rows are not transcribed at all, so it
## is excluded wholesale rather than itemized here. That leaves Table 1's 93
## primary-scale and subscale rows, which is the count the paper's own prose
## states three times (pp. 5, 17, 24 of the shelf PDF).
##
## Source for the correspondence: the run of `data-raw/verify_hitopsr_names.R`
## on 2026-08-28 against the pinned shelf PDF, which reports every one of those
## 93 labels reconciling with `hitopsr_scales$Scale` and
## `hitopsr_subscales$Subscale` except the single pair below. This script cites
## that run rather than re-deriving the correspondence, so the two files cannot
## disagree about it (IP1: a discrepancy stays visible in one place).

## Table 1 labels that do not match a package name character for character, and
## the package name each resolves to.
label_map <- tibble::tribble(
  ~table1,          ~package,        ~why,
  "Manic Energy†", "Manic Energy",
  paste(
    "The dagger is Table 1's own footnote marker -- 'Manic Energy is defined in",
    "the model to split between the Thought Disorder and Internalizing spectra'",
    "(p. 51 of the shelf PDF) -- and is a note about where the scale sits in the",
    "HiTOP model, not part of its name. It is Table 1's only marked label."
  )
)

## The exception set: package scales and subscales that get no row from Table 1's
## primary-scale and subscale block. It is **empty**, and empty is the finding,
## not an omission -- the 2026-08-28 verifier run above reconciles all 93 labels
## once the one marker above is resolved, so every scale and subscale the package
## ships is covered. A member added here would be named with why it does not
## join; there is nothing to name. The `stopifnot()` at the foot of this script
## re-derives the residue at every rebuild and refuses to write an `.rda` whose
## residue is not this set, so the declaration cannot go stale silently.
package_exceptions <- character(0)

# ------------------------------------------------------------------------------
## Build

table1$name <- table1$label
hit <- match(table1$name, label_map$table1)
table1$name[!is.na(hit)] <- label_map$package[stats::na.omit(hit)]

## The stem and the scale/subscale kind come from the package's own tables, never
## from Table 1: the table marks a subscale only by indenting its label, and the
## camelCase stem is what a scored column carries, which Table 1 knows nothing
## about. `verify_hitopsr_devstats.R` reads that indentation back out of the
## source and checks `type` against it, so the package-side derivation is
## cross-checked rather than assumed.
package_names <- rbind(
  data.frame(
    scale = hitopsr_scales$Scale,
    camelCase = hitopsr_scales$camelCase,
    type = "scale",
    stringsAsFactors = FALSE
  ),
  data.frame(
    scale = hitopsr_subscales$Subscale,
    camelCase = hitopsr_subscales$camelCase,
    type = "subscale",
    stringsAsFactors = FALSE
  )
)

joined <- merge(
  package_names,
  table1[c("name", "nItems", "alpha", "mean", "sd")],
  by.x = "scale",
  by.y = "name",
  all.x = TRUE,
  sort = FALSE
)

hitopsr_devstats <- tibble::tibble(
  scale = joined$scale,
  camelCase = joined$camelCase,
  type = joined$type,
  nItems = as.integer(joined$nItems),
  ## The coefficient and its identity travel together (D-043): `reliability`
  ## carries Table 1's printed value and `reliabilityType` names what it is, so a
  ## later source supplying a different coefficient cannot be mistaken for this
  ## one. `reliabilityType` is a package-supplied label -- Table 1 prints no such
  ## cell; it prints a column headed `alpha`, which the paper's text (p. 24)
  ## calls a Cronbach's alpha.
  reliability = joined$alpha,
  reliabilityType = "alpha",
  mean = joined$mean,
  sd = joined$sd
)
hitopsr_devstats <-
  hitopsr_devstats[order(hitopsr_devstats$type, hitopsr_devstats$scale), ]

# ------------------------------------------------------------------------------
## Refuse to write a stale table

residue <- hitopsr_devstats$scale[is.na(hitopsr_devstats$reliability)]
unmapped <- setdiff(table1$name, package_names$scale)

stopifnot(
  "Table 1 transcription is not 93 rows" = nrow(table1) == 93L,
  "a Table 1 label appears twice" = !anyDuplicated(table1$name),
  "a package scale appears twice" = !anyDuplicated(package_names$scale),
  "the package-side residue is not the declared exception set" =
    setequal(residue, package_exceptions),
  "a Table 1 label resolves to no package scale" = length(unmapped) == 0L,
  "an item count disagrees with the shipped tables" = all(
    hitopsr_devstats$nItems ==
      c(hitopsr_scales$nItems, hitopsr_subscales$nItems)[
        match(
          hitopsr_devstats$scale,
          c(hitopsr_scales$Scale, hitopsr_subscales$Subscale)
        )
      ]
  ),
  "a reliability is outside (0, 1]" =
    all(hitopsr_devstats$reliability > 0 & hitopsr_devstats$reliability <= 1),
  "a mean or SD is outside the 1-4 response coding" = all(
    hitopsr_devstats$mean >= 1,
    hitopsr_devstats$mean <= 4,
    hitopsr_devstats$sd > 0,
    hitopsr_devstats$sd <= 3
  )
)

usethis::use_data(hitopsr_devstats, overwrite = TRUE)
