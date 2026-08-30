## HiTOP-BR development-sample descriptive statistics (hitopbr_devstats)
##
## Builds data/hitopbr_devstats.rda from data-raw/hitopbr_table1.R, the
## transcription of the "Superspectra and Spectra Scales" block of Table 1 of
## the HiTOP-SR introduction manuscript (cairn/references/simms2026.md; D-042
## admits the document, D-032 admits its descriptive table as a published source
## under IP3).
##
## The transcription is a hand reading of the rendered pages, not an extraction.
## The machine extraction lives in data-raw/hitopsr_table1.R and is run against
## this script's transcription, and against the object it builds, by
## data-raw/verify_hitopbr_devstats.R. Keeping the two routes apart is what makes
## that diff a check rather than a tautology (IP2).
##
## Requires the package's own tables, so run after data-raw/hitopbr_info.R.

devtools::load_all(".", quiet = TRUE)

source("data-raw/hitopbr_table1.R")

# ------------------------------------------------------------------------------
## The name map
##
## Table 1's block prints eight labels, one per HiTOP-BR scale. Seven match
## `hitopbr_scales$Scale` character for character; the eighth differs in one
## letter's case. The map below is the whole of the correspondence, so a label
## the table prints that is neither matched nor mapped stops the build.

label_map <- tibble::tribble(
  ~table1,    ~package,   ~why,
  "p-factor", "p-Factor",
  paste(
    "The table prints a lowercase f. So does the development workbook",
    "`B-HiTOP overview.xlsx`. `hitopbr_scales$Scale` prints `p-Factor`, and",
    "whether that capitalization should follow the sources is an open",
    "ROADMAP candidate of its own -- it changes a printed scale name on the",
    "HiTOP-BR Word scoring page and in `available_scales()`, which is IP1",
    "content needing its own sign-off. This map resolves the pair for the",
    "join only; it adopts nothing."
  )
)

## The exception set: package scales that get no row from this block. It is
## **empty**, and empty is the finding, not an omission -- Table 1's last section
## prints one row for each of the eight HiTOP-BR scales. A member added here
## would be named with why it does not join; there is nothing to name. The
## `stopifnot()` at the foot re-derives the residue at every rebuild and refuses
## to write an `.rda` whose residue is not this set, so the declaration cannot go
## stale silently.
package_exceptions <- character(0)

# ------------------------------------------------------------------------------
## Build

table1 <- hitopbr_table1
table1$name <- table1$label
hit <- match(table1$name, label_map$table1)
table1$name[!is.na(hit)] <- label_map$package[stats::na.omit(hit)]

## The stem comes from the package's own table, never from Table 1: the camelCase
## stem is what a scored column carries, which Table 1 knows nothing about.
##
## `type` is `"scale"` for every row. Table 1 prints all eight under one heading,
## "Superspectra and Spectra Scales", and separates Externalizing and p-factor
## from the other six with a blank row and no printed label, so a
## superspectrum/spectrum column would be read out of a typographic gap rather
## than out of the source (IP2; M068 question gate).
package_names <- data.frame(
  scale = hitopbr_scales$Scale,
  camelCase = hitopbr_scales$camelCase,
  type = "scale",
  stringsAsFactors = FALSE
)

joined <- merge(
  package_names,
  table1[c("name", "nItems", "alpha", "mean", "sd")],
  by.x = "scale",
  by.y = "name",
  all.x = TRUE,
  sort = FALSE
)

hitopbr_devstats <- tibble::tibble(
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
hitopbr_devstats <-
  hitopbr_devstats[order(hitopbr_devstats$type, hitopbr_devstats$scale), ]

# ------------------------------------------------------------------------------
## Refuse to write a stale table

residue <- hitopbr_devstats$scale[is.na(hitopbr_devstats$reliability)]
unmapped <- setdiff(table1$name, package_names$scale)

stopifnot(
  "the Table 1 transcription is not 8 rows" = nrow(table1) == 8L,
  "a Table 1 label appears twice" = !anyDuplicated(table1$name),
  "a package scale appears twice" = !anyDuplicated(package_names$scale),
  "the package-side residue is not the declared exception set" =
    setequal(residue, package_exceptions),
  "a Table 1 label resolves to no package scale" = length(unmapped) == 0L,
  ## The counts printed in Table 1 against the counts the keying tables derive
  ## from `hitopbr_items`. The two agree only since M068 moved item 36 to
  ## Internalizing (cairn/SOURCES.md, "HiTOP-BR item-to-scale membership"); the
  ## CI-runnable form of this check is in tests/testthat/test-interval_hitopbr.R.
  "an item count disagrees with the shipped tables" = all(
    hitopbr_devstats$nItems ==
      lengths(hitopbr_scales$itemNumbers)[
        match(hitopbr_devstats$camelCase, hitopbr_scales$camelCase)
      ]
  ),
  "a reliability is outside (0, 1]" =
    all(hitopbr_devstats$reliability > 0 & hitopbr_devstats$reliability <= 1),
  "a mean or SD is outside the 1-4 response coding" = all(
    hitopbr_devstats$mean >= 1,
    hitopbr_devstats$mean <= 4,
    hitopbr_devstats$sd > 0,
    hitopbr_devstats$sd <= 3
  )
)

usethis::use_data(hitopbr_devstats, overwrite = TRUE)
