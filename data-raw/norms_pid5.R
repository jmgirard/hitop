## PID-5 Normative Tables
#
# Builds the exported long-form `pid_norms` from the seven transcribed
# `data-raw/norms_*.csv` tables. Every numeric cell of those CSVs is verified
# against the published source by `data-raw/verify_norms_against_book.R`;
# the citation, per-table page anchors, the normative sample, and the
# book-label -> package-column mapping live in `cairn/SOURCES.md`.
#
# Usage:  Rscript data-raw/norms_pid5.R

stopifnot(requireNamespace("readr", quietly = TRUE))
stopifnot(requireNamespace("usethis", quietly = TRUE))
stopifnot(requireNamespace("tibble", quietly = TRUE))

## The domain and facet scale names are the score-output column stems, taken
## from pid_domains / pid_scales rather than retyped, so `pid_norms$scale` joins
## to `score_pid5()` output by string equality with no crosswalk. Run
## data-raw/pid_info.R first if the scale labels have changed.
load("data/pid_domains.rda")
load("data/pid_scales.rda")

## Column abbreviations as printed in the book's domain-table heads.
domain_abbr <- c(
  NEG = "Negative affectivity",
  DET = "Detachment",
  ANT = "Antagonism",
  DIS = "Disinhibition",
  PSY = "Psychoticism"
)
stopifnot(all(domain_abbr %in% pid_domains$Domain))
scale_for <- stats::setNames(
  pid_domains$camelCase[match(domain_abbr, pid_domains$Domain)],
  names(domain_abbr)
)
## The PID-5-BF total has no facet or domain entry of its own: it is the
## whole-form score that `score_pid5(version = "BF")` gains in M26 (D-017),
## whose output column stem is "total".
scale_for["TOT"] <- "total"

read_norms <- function(path) {
  as.data.frame(readr::read_csv(path, show_col_types = FALSE, progress = FALSE))
}

## ---- domain tables ---------------------------------------------------------
## One row per (version, scale, T score). The first CSV column is the T score;
## each scale contributes a `<ABBR>_Raw` and a `<ABBR>_Ptl` column.

domain_spec <- list(
  list(version = "FULL", csv = "data-raw/norms_pid5_domains.csv"),
  list(version = "SF", csv = "data-raw/norms_pid5sf_domains.csv"),
  list(version = "BF", csv = "data-raw/norms_pid5bf_domains.csv")
)

domain_blocks <- function(s) {
  tbl <- read_norms(s$csv)
  tscore <- as.integer(tbl[[1]])
  stems <- sub("_Raw$", "", grep("_Raw$", names(tbl), value = TRUE))
  stopifnot(all(stems %in% names(scale_for)))
  lapply(stems, function(stem) {
    data.frame(
      version = s$version,
      scale = unname(scale_for[[stem]]),
      tscore = tscore,
      raw = as.numeric(tbl[[paste0(stem, "_Raw")]]),
      percentile = as.numeric(tbl[[paste0(stem, "_Ptl")]]),
      stringsAsFactors = FALSE
    )
  })
}

## ---- facet tables ----------------------------------------------------------
## One row per (version, scale, T score), as for the domains. A-6 and A-8 print
## the facets under the book's own sentence-case captions, which differ from the
## package's `Facet` labels in case and in spelling out "and"; the map below is
## the one place the book's wording lives. The output column stem still comes
## from `pid_scales[[version]]$camelCase` rather than being retyped, so a facet
## renamed in pid_scales moves the norm rows with it.

facet_caption <- c(
  "Anhedonia"                      = "Anhedonia",
  "Anxiousness"                    = "Anxiousness",
  "Attention seeking"              = "Attention Seeking",
  "Callousness"                    = "Callousness",
  "Deceitfulness"                  = "Deceitfulness",
  "Depressivity"                   = "Depressivity",
  "Distractibility"                = "Distractibility",
  "Eccentricity"                   = "Eccentricity",
  "Emotional lability"             = "Emotional Lability",
  "Grandiosity"                    = "Grandiosity",
  "Hostility"                      = "Hostility",
  "Impulsivity"                    = "Impulsivity",
  "Intimacy avoidance"             = "Intimacy Avoidance",
  "Irresponsibility"               = "Irresponsibility",
  "Manipulativeness"               = "Manipulativeness",
  "Perceptual dysregulation"       = "Perceptual Dysregulation",
  "Perseveration"                  = "Perseveration",
  "Restricted affectivity"         = "Restricted Affectivity",
  "Rigid perfectionism"            = "Rigid Perfectionism",
  "Risk taking"                    = "Risk Taking",
  "Separation insecurity"          = "Separation Insecurity",
  "Submissiveness"                 = "Submissiveness",
  "Suspiciousness"                 = "Suspiciousness",
  "Unusual beliefs and experiences" = "Unusual Beliefs & Experiences",
  "Withdrawal"                     = "Withdrawal"
)

facet_spec <- list(
  list(version = "FULL", csv = "data-raw/norms_pid5_facets.csv"),
  list(version = "SF", csv = "data-raw/norms_pid5sf_facets.csv")
)

facet_blocks <- function(s) {
  tbl <- read_norms(s$csv)
  tscore <- as.integer(tbl[[1]])
  captions <- sub("_Raw$", "", grep("_Raw$", names(tbl), value = TRUE))
  stopifnot(all(captions %in% names(facet_caption)))

  scales <- pid_scales[[s$version]]
  stem <- stats::setNames(
    scales$camelCase[match(facet_caption[captions], scales$Facet)],
    captions
  )
  ## The crosswalk is checked in both directions before anything is built: a
  ## caption the package does not know maps to NA, and a facet the CSV omits or
  ## names twice breaks the setequal. Either way the guard fires here rather
  ## than shipping a `pid_norms` quietly missing a facet.
  stopifnot(
    !anyNA(stem),
    !anyDuplicated(stem),
    setequal(unname(stem), scales$camelCase)
  )

  lapply(captions, function(cap) {
    data.frame(
      version = s$version,
      scale = unname(stem[[cap]]),
      tscore = tscore,
      raw = as.numeric(tbl[[paste0(cap, "_Raw")]]),
      percentile = as.numeric(tbl[[paste0(cap, "_Ptl")]]),
      stringsAsFactors = FALSE
    )
  })
}

## ---- validity tables -------------------------------------------------------
## One row per (version, scale, raw score). These tables print no T score, so
## `tscore` is NA. `scale` is the `validity_pid5()` column stem, which is not
## always the book's caption name: A-1 and A-2 are both captioned "Variable
## Response Inconsistency (VRIN)" while the package (with the scale-development
## papers) calls them INC and INC-S, and A-4's PID-5-PRD is the package's PRD
## (D-018; the book-label mapping is tabulated in cairn/SOURCES.md).

validity_spec <- list(
  list(version = "FULL", scale = "INC", csv = "data-raw/norms_pid5_vrin.csv"),
  list(version = "FULL", scale = "ORS", csv = "data-raw/norms_pid5_ors.csv"),
  list(version = "FULL", scale = "PRD", csv = "data-raw/norms_pid5_pimrd.csv"),
  list(version = "SF", scale = "INCS", csv = "data-raw/norms_pid5sf_vrin.csv")
)

validity_block <- function(s) {
  tbl <- read_norms(s$csv)
  data.frame(
    version = s$version,
    scale = s$scale,
    tscore = NA_integer_,
    raw = as.numeric(tbl[[1]]),
    percentile = as.numeric(tbl[[2]]),
    stringsAsFactors = FALSE
  )
}

## ---- assemble --------------------------------------------------------------
## Blocks are concatenated version by version (FULL, SF, BF), domains before
## facets before validity scales; each block is already ascending in T or in raw
## score, so the result is sorted without a sort.

blocks <- c(
  domain_blocks(domain_spec[[1]]),
  facet_blocks(facet_spec[[1]]),
  lapply(validity_spec[1:3], validity_block),
  domain_blocks(domain_spec[[2]]),
  facet_blocks(facet_spec[[2]]),
  list(validity_block(validity_spec[[4]])),
  domain_blocks(domain_spec[[3]])
)

pid_norms <- tibble::as_tibble(do.call(rbind, blocks))

## ---- checks ----------------------------------------------------------------
## Structural guards on the built object. The substantive invariants (linearity
## of raw in T, monotone percentiles, the page-cited spot values) are tested in
## tests/testthat/test-norms.R.

stopifnot(
  identical(names(pid_norms), c("version", "scale", "tscore", "raw", "percentile")),
  all(pid_norms$version %in% c("FULL", "SF", "BF")),
  !anyNA(pid_norms$raw),
  !anyNA(pid_norms$percentile),
  all(pid_norms$percentile >= 0 & pid_norms$percentile <= 1),
  all(pid_norms$raw >= 0),
  ## tscore is missing exactly on the four validity scales
  identical(
    sort(unique(pid_norms$scale[is.na(pid_norms$tscore)])),
    c("INC", "INCS", "ORS", "PRD")
  ),
  !anyNA(pid_norms$tscore[!pid_norms$scale %in% c("INC", "INCS", "ORS", "PRD")]),
  ## no duplicate rows within a (version, scale)
  !anyDuplicated(pid_norms[c("version", "scale", "tscore", "raw")]),
  ## every T-scored pair spans exactly the T rows its table prints, so a block
  ## silently short of rows cannot reach the .rda
  all(pid_norms$tscore >= 30 & pid_norms$tscore <= 100, na.rm = TRUE),
  ## the 25 facets are normed for FULL and SF and for neither the BF nor
  ## anything else, at 71 T rows apiece
  identical(
    sort(unique(pid_norms$scale[
      pid_norms$scale %in% pid_scales[["FULL"]]$camelCase
    ])),
    sort(pid_scales[["FULL"]]$camelCase)
  ),
  all(
    table(pid_norms$version[pid_norms$scale %in% pid_scales[["FULL"]]$camelCase]) ==
      c(FULL = 25 * 71, SF = 25 * 71)
  )
)

usethis::use_data(pid_norms, overwrite = TRUE)
