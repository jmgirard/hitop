## HiTOP-DAT norms (hitopdat_norms)
##
## Builds data/hitopdat_norms.rda from data-raw/hitopdat_norms.csv: one row per
## scale that norm_hitopdat() converts to a T score, with the mean and SD the
## battery's authors name for it (D-098; cairn/SOURCES.md "The authors' second
## answers" and "The authors' joint answers").
##
## The CSV is a transcription. Its sources and anchors:
##   - whodas: Andrews et al. (2009), Table 1, Total row, Total column
##     (cairn/references/andrews2009.md).
##   - the 18 IDAS-II scales: Watson et al. (2012), Table 2, "Adults" column
##     (cairn/references/watson2012.md).
##   - the 33 CAT-PD facets: Len Simms's unpublished CAT-PD and IDAS scoring
##     workbook, sheet "CAT-PD Scoring", cells E3:F35, at their stored
##     precision (cairn/references/simms2026catpdworkbook.md). The workbook is
##     not read here: it is gitignored on the source shelf, and reading it would
##     need {readxl}, which is not a dependency.
## positiveSymptoms carries no M or SD in the CSV. They are computed below from
## the four cells Galletly et al. (2016) print in Table 1
## (cairn/references/galletly2016.md), because a weighted mean is a repeating
## decimal that a CSV cell would truncate.
##
## General Depression is left out (the authors dropped it), and so are the
## AUDIT, DUDIT and PHQ-15, whose T scores wait on the authors' follow-up.
##
## Requires the package's own tables, so run after data-raw/hitopdat_info.R.

devtools::load_all(".", quiet = TRUE)

norms <- utils::read.csv(
  "data-raw/hitopdat_norms.csv",
  colClasses = c(
    camelCase = "character",
    M = "numeric",
    SD = "numeric",
    Sample = "character",
    N = "integer",
    Source = "character"
  ),
  encoding = "UTF-8"
)

## CAPE positive symptoms: the n-weighted mean of the men's and women's means
## and their common printed SD (both 3.52), each divided by the 20 positive
## items, because score_hitopdat() scores the CAPE as an item mean. Jonas: "I
## believe we took the weighted mean of the male and female values here"
## (jonas_ringwald2026email.txt, answer 5).
cape <- list(n_men = 125, m_men = 26.26, n_women = 180, m_women = 26.58,
             sd = 3.52, n_items = 20)
stopifnot(
  hitopdat_scales$nItems[hitopdat_scales$camelCase == "positiveSymptoms"] ==
    cape$n_items
)
is_cape <- norms$camelCase == "positiveSymptoms"
stopifnot(sum(is_cape) == 1, is.na(norms$M[is_cape]), is.na(norms$SD[is_cape]))
norms$M[is_cape] <- (cape$n_men * cape$m_men + cape$n_women * cape$m_women) /
  (cape$n_men + cape$n_women) / cape$n_items
norms$SD[is_cape] <- cape$sd / cape$n_items

## The rows follow hitopdat_scales, less the four scales without a norm here.
unnormed <- c("generalDepression", "alcoholUse", "drugUse", "physicalSymptoms")
expected <- setdiff(hitopdat_scales$camelCase, unnormed)
stopifnot(
  identical(norms$camelCase, expected),
  !anyNA(norms$M),
  all(norms$SD > 0)
)

idx <- match(norms$camelCase, hitopdat_scales$camelCase)
hitopdat_norms <- tibble::tibble(
  Measure = hitopdat_scales$Measure[idx],
  Scale = hitopdat_scales$Scale[idx],
  camelCase = norms$camelCase,
  M = norms$M,
  SD = norms$SD,
  Sample = norms$Sample,
  N = norms$N,
  Source = norms$Source
)

usethis::use_data(hitopdat_norms, overwrite = TRUE)
